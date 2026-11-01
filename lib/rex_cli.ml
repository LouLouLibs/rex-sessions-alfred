(* Calls to the `rex` command-line tool. *)

(* Alfred doesn't see the shell's PATH, so default to the copy inside Rex's app bundle. *)
let bundled_rex_path () =
  Filename.concat (Settings.home_directory ())
    "Library/Application Support/com.superlogical.rex/RexServer/Versions/Current/Rex \
     Server.app/Contents/MacOS/rex"

let rex_executable_path () =
  match Settings.non_empty_environment_value "rex_path" with
  | Some path -> path
  | None ->
      let bundled = bundled_rex_path () in
      if Sys.file_exists bundled then bundled else "rex"

let rex_arguments host arguments = Host.server_flags_of_host host @ arguments

let run_rex ?timeout_seconds host arguments =
  Process.run ?timeout_seconds (rex_executable_path ()) (rex_arguments host arguments)

let error_message_of_result (result : Process.result) =
  if result.timed_out then "rex timed out"
  else
    match String.trim result.stderr with
    | "" -> Printf.sprintf "rex exited with status %d" result.exit_code
    | message -> message
