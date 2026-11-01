(* Append-only log in the cache folder, for seeing what background refreshes did. *)

let maximum_log_bytes = 1_000_000

let timestamp_of_time time =
  let tm = Unix.localtime time in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (tm.tm_year + 1900) (tm.tm_mon + 1)
    tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let start_over_if_too_large path =
  match Unix.stat path with
  | stats when stats.st_size > maximum_log_bytes -> Sys.remove path
  | _ -> ()
  | exception Unix.Unix_error _ -> ()

let log_line format =
  Printf.ksprintf
    (fun message ->
      let path = Settings.log_path () in
      try
        Settings.make_directory_with_parents (Filename.dirname path);
        start_over_if_too_large path;
        Out_channel.with_open_gen [ Open_append; Open_creat; Open_text ] 0o644 path
          (fun channel ->
            Printf.fprintf channel "%s [%d] %s\n"
              (timestamp_of_time (Unix.gettimeofday ()))
              (Unix.getpid ()) message)
      with Sys_error _ | Unix.Unix_error _ -> ())
    format
