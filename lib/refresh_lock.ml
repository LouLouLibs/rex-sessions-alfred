(* At most one background refresh at a time. The lock file holds the PID of the refresh
   that owns it: the picker creates it empty, and the detached refresh writes its own
   PID on start. A lock whose owner is alive is never taken over, however slow the
   refresh; only an empty lock (a spawn that never started) or a dead owner's lock is. *)

let read_lock () =
  match Unix.stat (Settings.refresh_lock_path ()) with
  | exception Unix.Unix_error _ -> None
  | stats ->
      let contents =
        try In_channel.with_open_bin (Settings.refresh_lock_path ()) In_channel.input_all
        with Sys_error _ -> ""
      in
      Some (int_of_string_opt (String.trim contents), Unix.gettimeofday () -. stats.st_mtime)

let process_is_alive pid =
  match Unix.kill pid 0 with
  | () -> true
  | exception Unix.Unix_error (Unix.EPERM, _, _) -> true
  | exception Unix.Unix_error _ -> false

let refresh_is_running () =
  match read_lock () with
  | None -> false
  | Some (Some pid, age) -> process_is_alive pid && age < Settings.refresh_lock_owner_trusted_seconds
  | Some (None, age) -> age < Settings.refresh_lock_expires_after_seconds

let create_lock_exclusively () =
  match
    Unix.openfile (Settings.refresh_lock_path ()) [ Unix.O_CREAT; Unix.O_EXCL; Unix.O_WRONLY ] 0o644
  with
  | fd ->
      Unix.close fd;
      true
  | exception Unix.Unix_error (Unix.EEXIST, _, _) -> false

(* For the picker: take the lock (empty) before spawning a refresh. *)
let try_take_lock () =
  Settings.make_directory_with_parents (Settings.cache_directory ());
  create_lock_exclusively ()
  || (not (refresh_is_running ()))
     && begin
       (* Its owner is gone: remove it and race any other picker for a fresh one. *)
       (try Sys.remove (Settings.refresh_lock_path ()) with Sys_error _ -> ());
       create_lock_exclusively ()
     end

let lock_is_ours () =
  match read_lock () with
  | Some (Some pid, _) -> pid = Unix.getpid ()
  | _ -> false

(* For the refresh itself: put our PID in the lock, unless a live refresh owns it. *)
let claim_lock_for_this_process () =
  Settings.make_directory_with_parents (Settings.cache_directory ());
  let free_to_claim =
    match read_lock () with
    | None -> create_lock_exclusively ()
    | Some (Some pid, _) when pid <> Unix.getpid () -> not (refresh_is_running ())
    | Some _ -> true
  in
  free_to_claim
  && begin
    Cache_index.write_file_atomically (Settings.refresh_lock_path ())
      (string_of_int (Unix.getpid ()));
    true
  end

(* Touch the lock as the refresh makes progress, so its age reflects a live refresh. *)
let keep_lock_fresh () =
  if lock_is_ours () then
    try Unix.utimes (Settings.refresh_lock_path ()) 0. 0. with Unix.Unix_error _ -> ()

let release_lock () =
  if lock_is_ours () then try Sys.remove (Settings.refresh_lock_path ()) with Sys_error _ -> ()

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
