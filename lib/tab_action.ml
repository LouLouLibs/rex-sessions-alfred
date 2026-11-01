(* What pressing return (with or without a modifier) on a result does. Alfred hands
   the chosen action back to us as a string, so actions round-trip through one. *)

type tab_target = {
  host : Host.t;
  session_id : string;
  window_id : string;
}

type t =
  | Go_to_tab of tab_target
  | Close_tab of tab_target
  | Copy_tab_ids of tab_target
  | Create_local_session of string

let tab_target_of_tab (tab : Tab.t) =
  { host = tab.host; session_id = tab.session_id; window_id = tab.window_id }

let string_of_tab_target target =
  String.concat "|" [ Host.key_of_host target.host; target.session_id; target.window_id ]

let tab_target_of_string text =
  match String.split_on_char '|' text with
  | [ host_key; session_id; window_id ] ->
      Some { host = Host.host_of_key host_key; session_id; window_id }
  | _ -> None

let argument_of_action = function
  | Go_to_tab target -> "go:" ^ string_of_tab_target target
  | Close_tab target -> "close:" ^ string_of_tab_target target
  | Copy_tab_ids target -> "copy:" ^ string_of_tab_target target
  | Create_local_session name -> "new:" ^ name

(* `rex new` would read a leading "-" as a flag. *)
let is_valid_session_name name = name <> "" && name.[0] <> '-'

let action_of_argument argument =
  match String.index_opt argument ':' with
  | None -> None
  | Some colon -> (
      let verb = String.sub argument 0 colon in
      let rest = String.sub argument (colon + 1) (String.length argument - colon - 1) in
      let with_target make = Option.map make (tab_target_of_string rest) in
      match verb with
      | "go" -> with_target (fun target -> Go_to_tab target)
      | "close" -> with_target (fun target -> Close_tab target)
      | "copy" -> with_target (fun target -> Copy_tab_ids target)
      | "new" when is_valid_session_name rest -> Some (Create_local_session rest)
      | _ -> None)

(* The Rex app is a client of every host it shows, so ask the tab's own server to
   switch it; with one app connected, rex picks that client without -C. *)
let go_to_tab target =
  let result =
    Rex_cli.run_rex target.host
      [
        "do";
        "session.select";
        "session_id=" ^ target.session_id;
        "window_id=" ^ target.window_id;
      ]
  in
  if not (Process.succeeded result) then begin
    Activity_log.log_line "go to tab failed: %s" (Rex_cli.error_message_of_result result);
    Macos.show_notification (Rex_cli.error_message_of_result result)
  end;
  Macos.bring_rex_to_front ()

let description_of_tab_target target =
  let cached_tab =
    Option.bind (Cache_index.load_index ()) (fun index ->
        List.find_opt
          (fun (tab : Tab.t) -> tab.window_id = target.window_id)
          (Cache_index.tabs_of_host index target.host))
  in
  match cached_tab with
  | Some tab -> Printf.sprintf "%s › %s" tab.session_label tab.tab_name
  | None -> target.window_id

let close_tab target =
  let description = description_of_tab_target target in
  let where = match target.host with Host.Local -> "" | Host.Remote label -> " on " ^ label in
  if
    Macos.confirm_with_dialog
      ~message:
        (Printf.sprintf "Close the Rex tab “%s”%s and everything running in it?" description
           where)
      ~confirm_button:"Close Tab"
  then begin
    let result =
      Rex_cli.run_rex target.host
        [
          "api";
          "call";
          "-s";
          target.session_id;
          "session.close_window";
          Yojson.Safe.to_string (`Assoc [ ("window_id", `String target.window_id) ]);
        ]
    in
    Macos.show_notification
      (if Process.succeeded result then "Closed " ^ description
       else Rex_cli.error_message_of_result result);
    Cache_index.mark_index_stale ()
  end

let session_id_of_new_session_output output =
  match Yojson.Safe.from_string output with
  | json -> (
      match Tab.string_field "session_id" json with
      | "" -> None
      | session_id -> Some session_id)
  | exception Yojson.Json_error _ -> None

let create_local_session name =
  let result = Rex_cli.run_rex Host.Local [ "new"; name; "--json" ] in
  if not (Process.succeeded result) then
    Macos.show_notification (Rex_cli.error_message_of_result result)
  else begin
    (match session_id_of_new_session_output result.stdout with
    | Some session_id ->
        ignore (Rex_cli.run_rex Host.Local [ "do"; "session.select"; "session_id=" ^ session_id ])
    | None -> Activity_log.log_line "rex new printed no session_id: %s" result.stdout);
    Macos.bring_rex_to_front ();
    Cache_index.mark_index_stale ()
  end

let perform = function
  | Go_to_tab target -> go_to_tab target
  | Close_tab target -> close_tab target
  | Copy_tab_ids target ->
      Macos.copy_to_clipboard (target.session_id ^ " " ^ target.window_id)
  | Create_local_session name -> create_local_session name
