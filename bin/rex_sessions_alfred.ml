(* Alfred workflow binary: fuzzy-pick Rex tabs on every host.

     rex_sessions_alfred filter [QUERY]   print Alfred Script Filter JSON
     rex_sessions_alfred refresh          snapshot every host into the cache
     rex_sessions_alfred run ACTION       perform a picked action (go/close/copy/new) *)

open Rex_sessions

let usage =
  "usage: rex_sessions_alfred (filter [QUERY] | refresh | run ACTION)\n"

let () =
  (* A child closing its stdin early must not kill us. *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  match Array.to_list Sys.argv |> List.tl with
  | [ "filter" ] -> print_endline (Yojson.Safe.to_string (Picker.alfred_response_of_query ""))
  | [ "filter"; query ] ->
      print_endline (Yojson.Safe.to_string (Picker.alfred_response_of_query query))
  | [ "refresh" ] -> Refresh.refresh_all_hosts ()
  | [ "run"; argument ] -> (
      match Tab_action.action_of_argument argument with
      | Some action -> Tab_action.perform action
      | None ->
          Activity_log.log_line "unknown action: %s" argument;
          prerr_string usage;
          exit 2)
  | _ ->
      prerr_string usage;
      exit 2
