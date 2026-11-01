(* Run a program, capture its output, and kill it if it runs past a deadline. *)

type result = {
  exit_code : int;
  stdout : string;
  stderr : string;
  timed_out : bool;
}

let exit_code_of_status = function
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> 128

let rec wait_for_exit_status pid =
  try snd (Unix.waitpid [] pid)
  with Unix.Unix_error (Unix.EINTR, _, _) -> wait_for_exit_status pid

let close_ignoring_errors fd = try Unix.close fd with Unix.Unix_error _ -> ()

(* Returns false once the pipe reaches end of file. *)
let read_chunk_into_buffer fd buffer =
  let chunk = Bytes.create 65536 in
  match Unix.read fd chunk 0 (Bytes.length chunk) with
  | 0 -> false
  | count ->
      Buffer.add_subbytes buffer chunk 0 count;
      true
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> true

let write_all_ignoring_closed_pipe fd text =
  let rec write_from offset =
    if offset < String.length text then
      match Unix.write_substring fd text offset (String.length text - offset) with
      | written -> write_from (offset + written)
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> write_from offset
      | exception Unix.Unix_error (Unix.EPIPE, _, _) -> ()
  in
  write_from 0

(* Read stdout and stderr together so neither pipe fills up and stalls the child.
   Returns true if the deadline passed before both pipes closed. *)
let drain_pipes_until_deadline ~deadline pipes_with_buffers =
  let rec drain open_pipes =
    if open_pipes = [] then false
    else
      let remaining = deadline -. Unix.gettimeofday () in
      if remaining <= 0. then true
      else
        match Unix.select (List.map fst open_pipes) [] [] remaining with
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> drain open_pipes
        | ready, _, _ ->
            drain
              (List.filter
                 (fun (fd, buffer) ->
                   (not (List.mem fd ready)) || read_chunk_into_buffer fd buffer)
                 open_pipes)
  in
  drain pipes_with_buffers

let run ?(timeout_seconds = 10.) ?(stdin_text = "") program arguments =
  let stdin_read, stdin_write = Unix.pipe ~cloexec:true () in
  let stdout_read, stdout_write = Unix.pipe ~cloexec:true () in
  let stderr_read, stderr_write = Unix.pipe ~cloexec:true () in
  let spawned =
    try
      Ok
        (Unix.create_process program
           (Array.of_list (program :: arguments))
           stdin_read stdout_write stderr_write)
    with Unix.Unix_error (error, _, _) -> Error (Unix.error_message error)
  in
  List.iter close_ignoring_errors [ stdin_read; stdout_write; stderr_write ];
  match spawned with
  | Error message ->
      List.iter close_ignoring_errors [ stdin_write; stdout_read; stderr_read ];
      { exit_code = 127; stdout = ""; stderr = program ^ ": " ^ message; timed_out = false }
  | Ok pid ->
      write_all_ignoring_closed_pipe stdin_write stdin_text;
      close_ignoring_errors stdin_write;
      let stdout_buffer = Buffer.create 65536 in
      let stderr_buffer = Buffer.create 1024 in
      let timed_out =
        drain_pipes_until_deadline
          ~deadline:(Unix.gettimeofday () +. timeout_seconds)
          [ (stdout_read, stdout_buffer); (stderr_read, stderr_buffer) ]
      in
      if timed_out then (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
      List.iter close_ignoring_errors [ stdout_read; stderr_read ];
      let status = wait_for_exit_status pid in
      {
        exit_code = (if timed_out then 124 else exit_code_of_status status);
        stdout = Buffer.contents stdout_buffer;
        stderr = Buffer.contents stderr_buffer;
        timed_out;
      }

let succeeded result = result.exit_code = 0
