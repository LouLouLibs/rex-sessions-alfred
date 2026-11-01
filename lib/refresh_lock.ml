(* At most one background refresh at a time. The lock is a file; its age tells
   whether the refresh holding it is still alive. *)

let refresh_is_running () =
  match Unix.stat (Settings.refresh_lock_path ()) with
  | stats ->
      Unix.gettimeofday () -. stats.st_mtime < Settings.refresh_lock_expires_after_seconds
  | exception Unix.Unix_error _ -> false

let try_take_lock () =
  let path = Settings.refresh_lock_path () in
  Settings.make_directory_with_parents (Filename.dirname path);
  match Unix.openfile path [ Unix.O_CREAT; Unix.O_EXCL; Unix.O_WRONLY ] 0o644 with
  | fd ->
      Unix.close fd;
      true
  | exception Unix.Unix_error (Unix.EEXIST, _, _) ->
      if refresh_is_running () then false
      else begin
        (* A dead refresh left it behind: take it over. *)
        Unix.utimes path 0. 0.;
        true
      end

let release_lock () = try Sys.remove (Settings.refresh_lock_path ()) with Sys_error _ -> ()

let redirect_standard_streams_to_dev_null () =
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
  List.iter (fun stream -> Unix.dup2 dev_null stream) [ Unix.stdin; Unix.stdout; Unix.stderr ];
  Unix.close dev_null

(* Start `refresh` in the background, fully detached: Alfred waits for every process
   holding its stdout, so the refresh must not inherit it. Double fork so it's
   reparented to launchd and never becomes a zombie of the picker. *)
let spawn_detached_refresh () =
  if try_take_lock () then begin
    flush_all ();
    match Unix.fork () with
    | 0 -> (
        ignore (Unix.setsid ());
        match Unix.fork () with
        | 0 -> (
            redirect_standard_streams_to_dev_null ();
            try Unix.execv Sys.executable_name [| Sys.executable_name; "refresh" |]
            with Unix.Unix_error _ -> Unix._exit 1)
        | _ -> Unix._exit 0)
    | child -> ignore (Process.wait_for_exit_status child)
  end
