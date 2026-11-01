(* Paths and tunables. Alfred passes workflow variables as environment variables. *)

let non_empty_environment_value name =
  match Sys.getenv_opt name with
  | Some value when String.trim value <> "" -> Some (String.trim value)
  | _ -> None

let home_directory () = Option.value (Sys.getenv_opt "HOME") ~default:"/tmp"

let int_setting name ~default =
  Option.bind (non_empty_environment_value name) int_of_string_opt
  |> Option.value ~default

let float_setting name ~default =
  Option.bind (non_empty_environment_value name) float_of_string_opt
  |> Option.value ~default

let cache_directory () =
  match non_empty_environment_value "alfred_workflow_cache" with
  | Some directory -> directory
  | None -> Filename.concat (home_directory ()) "Library/Caches/rex-sessions-alfred"

let index_path () = Filename.concat (cache_directory ()) "index.json"
let refresh_lock_path () = Filename.concat (cache_directory ()) "refresh.lock"
let previews_directory () = Filename.concat (cache_directory ()) "previews"
let log_path () = Filename.concat (cache_directory ()) "rex-sessions.log"

(* Lines of scrollback kept in each Quick Look preview. *)
let preview_line_count () = int_setting "preview_lines" ~default:70

(* An index older than this triggers a background refresh. *)
let stale_after_seconds () = float_setting "refresh_seconds" ~default:5.

(* A refresh lock older than this belongs to a refresh that died. *)
let refresh_lock_expires_after_seconds = 60.

(* Remote hosts send whole scrollbacks, so give them time. *)
let host_snapshot_timeout_seconds = 30.

(* Alfred runs the binary from the workflow folder, next to snapshot.lua. *)
let workflow_directory () = Filename.dirname Sys.executable_name

let snapshot_script_path () =
  match non_empty_environment_value "snapshot_script" with
  | Some path -> path
  | None -> Filename.concat (workflow_directory ()) "snapshot.lua"

let rec make_directory_with_parents path =
  if not (Sys.file_exists path) then begin
    make_directory_with_parents (Filename.dirname path);
    try Sys.mkdir path 0o755 with Sys_error _ when Sys.file_exists path -> ()
  end
