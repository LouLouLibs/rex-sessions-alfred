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

(* A started child whose stdout and stderr are still being read. *)
type running = {
  pid : int;
  deadline : float;
  stdout_buffer : Buffer.t;
  stderr_buffer : Buffer.t;
  mutable open_pipes : (Unix.file_descr * Buffer.t) list;
}

let spawn_failure program message =
  { exit_code = 127; stdout = ""; stderr = program ^ ": " ^ message; timed_out = false }

let spawn ~timeout_seconds ~stdin_text program arguments =
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
      Error (spawn_failure program message)
  | Ok pid ->
      write_all_ignoring_closed_pipe stdin_write stdin_text;
      close_ignoring_errors stdin_write;
      let stdout_buffer = Buffer.create 65536 and stderr_buffer = Buffer.create 1024 in
      Ok
        {
          pid;
          deadline = Unix.gettimeofday () +. timeout_seconds;
          stdout_buffer;
          stderr_buffer;
          open_pipes = [ (stdout_read, stdout_buffer); (stderr_read, stderr_buffer) ];
        }

let finish ~timed_out running =
  if timed_out then (try Unix.kill running.pid Sys.sigkill with Unix.Unix_error _ -> ());
  List.iter (fun (fd, _) -> close_ignoring_errors fd) running.open_pipes;
  running.open_pipes <- [];
  let status = wait_for_exit_status running.pid in
  {
    exit_code = (if timed_out then 124 else exit_code_of_status status);
    stdout = Buffer.contents running.stdout_buffer;
    stderr = Buffer.contents running.stderr_buffer;
    timed_out;
  }

(* Run several programs at once from this one thread, reading every pipe in one select
   loop so no child stalls on a full pipe. on_finished gets each result as soon as that
   child is done, so a fast job never waits behind a slow one. *)
let run_all ?(timeout_seconds = 10.) ?(stdin_text = "") jobs ~on_finished =
  let started =
    List.filter_map
      (fun (key, program, arguments) ->
        match spawn ~timeout_seconds ~stdin_text program arguments with
        | Ok running -> Some (key, running)
        | Error result ->
            on_finished key result;
            None)
      jobs
  in
  let rec loop active =
    if active <> [] then begin
      let now = Unix.gettimeofday () in
      let finished, still_running =
        List.partition
          (fun (_, running) -> running.open_pipes = [] || running.deadline <= now)
          active
      in
      List.iter
        (fun (key, running) ->
          on_finished key (finish ~timed_out:(running.open_pipes <> []) running))
        finished;
      if still_running <> [] then begin
        let earliest_deadline =
          List.fold_left (fun earliest (_, running) -> Float.min earliest running.deadline)
            infinity still_running
        in
        let open_fds =
          List.concat_map (fun (_, running) -> List.map fst running.open_pipes) still_running
        in
        (match Unix.select open_fds [] [] (Float.max 0. (earliest_deadline -. now)) with
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> ()
        | ready, _, _ ->
            List.iter
              (fun (_, running) ->
                running.open_pipes <-
                  List.filter
                    (fun (fd, buffer) ->
                      (not (List.mem fd ready))
                      || read_chunk_into_buffer fd buffer
                      || (close_ignoring_errors fd; false))
                    running.open_pipes)
              still_running);
        loop still_running
      end
    end
  in
  loop started

let run ?timeout_seconds ?stdin_text program arguments =
  let outcome = ref None in
  run_all ?timeout_seconds ?stdin_text [ ((), program, arguments) ] ~on_finished:(fun () result ->
      outcome := Some result);
  Option.get !outcome

let succeeded result = result.exit_code = 0
