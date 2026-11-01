(* Snapshot every host's sessions and tabs into the cache index and preview files.
   Hosts are snapshotted in parallel: a remote host takes seconds because Rex sends
   whole scrollbacks, and no host should wait for a slower one. *)

let remote_hosts_of_hosts_json json =
  List.filter_map
    (fun entry ->
      match Yojson.Safe.Util.(entry |> member "host" |> member "label") with
      | `String label when label <> "" -> Some (Host.Remote label)
      | _ -> None
      | exception Yojson.Safe.Util.Type_error _ -> None)
    (Tab.list_field "hosts" json)

let configured_hosts () =
  let result = Rex_cli.run_rex Host.Local [ "hosts"; "--json" ] in
  let remote_hosts =
    if Process.succeeded result then
      match Yojson.Safe.from_string result.stdout with
      | json -> remote_hosts_of_hosts_json json
      | exception Yojson.Json_error _ -> []
    else begin
      Activity_log.log_line "rex hosts failed: %s" (Rex_cli.error_message_of_result result);
      []
    end
  in
  Host.Local :: remote_hosts

let snapshot_script_arguments host =
  Rex_cli.rex_arguments host
    [
      "do";
      Settings.snapshot_script_path ();
      "tail=" ^ string_of_int (Settings.preview_line_count ());
    ]

(* snapshot.lua returns a JSON string, which `rex do` prints JSON-encoded. *)
let snapshot_json_of_script_output output =
  match Yojson.Safe.from_string output with
  | `String inner -> Yojson.Safe.from_string inner
  | json -> json

let file_name_safe_of_id id =
  String.map (fun character -> if character = ':' || character = '/' then '_' else character) id

let preview_path_of_window host window_id =
  Filename.concat (Settings.previews_directory ())
    (Printf.sprintf "%s_%s.html"
       (file_name_safe_of_id (Host.label_of_host host))
       (file_name_safe_of_id window_id))

let preview_page_template =
  {|<!DOCTYPE html><html><head><meta charset="utf-8"><style>
html,body{margin:0;background:#16181d;color:#d6d9de}
body{padding:14px 16px;font:11px/1.35 "SF Mono",Menlo,monospace}
h1{font:600 12px -apple-system,sans-serif;color:#c3c9d3;margin:0 0 2px}
h2{font:11px -apple-system,sans-serif;color:#7d8696;margin:0 0 10px}
.screen{white-space:pre;overflow:hidden}
</style></head><body><h1>|}

let preview_page_of_tab ~heading ~subheading ~screen_html =
  String.concat ""
    [
      preview_page_template;
      Display_text.html_escape heading;
      "</h1><h2>";
      Display_text.html_escape subheading;
      {|</h2><div class="screen">|};
      screen_html;
      "</div></body></html>";
    ]

(* Rewriting an unchanged preview would make an open Quick Look panel flicker. A preview
   that can't be written (disk full, say) is logged and skipped, not fatal. *)
let write_file_if_changed path contents =
  let unchanged =
    match In_channel.with_open_bin path In_channel.input_all with
    | existing -> existing = contents
    | exception Sys_error _ -> false
  in
  if not unchanged then
    try Cache_index.write_file_atomically path contents
    with Sys_error message -> Activity_log.log_line "preview not written: %s" message

let foreground_process_of_json json =
  {
    Program_icon.argv0 = Tab.string_field "argv0" json;
    process_name = Tab.string_field "name" json;
    invoked_path = Tab.string_field "invoked_path" json;
  }

let is_focused_pane json =
  match Yojson.Safe.Util.member "focused" json with
  | `Bool focused -> focused
  | _ -> false

(* Every pane's program, the focused pane's first: that one gives the tab its icon. *)
let programs_of_window_json window_json =
  let focused, others = List.partition is_focused_pane (Tab.list_field "programs" window_json) in
  List.map
    (fun json -> Program_icon.program_name_of_process (foreground_process_of_json json))
    (focused @ others)
  |> List.filter (fun program -> program <> "")

let tab_of_window_json ~host ~session_id ~session_label window_json =
  let window_id = Tab.string_field "window_id" window_json in
  let terminal_title = Tab.string_field "title" window_json in
  let display_path =
    Display_text.display_path_of_reported_directory (Tab.string_field "pwd" window_json)
  in
  let tab_index = Tab.int_field "index" window_json in
  let tab_name =
    Display_text.tab_name_of_window
      ~window_label:(Tab.string_field "label" window_json)
      ~terminal_title ~display_path ~tab_index
  in
  let preview_path = preview_path_of_window host window_id in
  let programs = programs_of_window_json window_json in
  let icon_path =
    Program_icon.icon_path_of_program_name (match programs with first :: _ -> first | [] -> "")
  in
  write_file_if_changed preview_path
    (preview_page_of_tab
       ~heading:(Printf.sprintf "%s › %s" session_label tab_name)
       ~subheading:
         (String.concat "  ·  "
            (List.filter
               (fun part -> part <> "")
               [ Host.label_of_host host; display_path; terminal_title ]))
       ~screen_html:(Display_text.screen_html_of_capture (Tab.string_field "html" window_json)));
  {
    Tab.host;
    session_id;
    session_label;
    window_id;
    tab_index;
    tab_name;
    terminal_title;
    display_path;
    pane_count = Tab.int_field "panes" window_json;
    pane_labels = Tab.string_list_field "labels" window_json;
    programs;
    icon_path;
    preview_path;
  }

let tabs_of_snapshot_json host snapshot_json =
  List.concat_map
    (fun session_json ->
      let session_id = Tab.string_field "session_id" session_json in
      let session_label =
        match Tab.string_field "label" session_json with
        | "" -> session_id
        | label -> label
      in
      List.map
        (tab_of_window_json ~host ~session_id ~session_label)
        (Tab.list_field "windows" session_json))
    (Tab.list_field "sessions" snapshot_json)

(* An unreachable host keeps its last known tabs, marked unreachable. *)
let host_snapshot_of_script_result ~previous host (result : Process.result) =
  let unreachable error_message =
    Activity_log.log_line "%s: %s" (Host.label_of_host host) error_message;
    { previous with Cache_index.reachable = false; error_message }
  in
  if not (Process.succeeded result) then unreachable (Rex_cli.error_message_of_result result)
  else
    match tabs_of_snapshot_json host (snapshot_json_of_script_output result.stdout) with
    | tabs ->
        {
          Cache_index.reachable = true;
          error_message = "";
          tabs;
          taken_at = Unix.gettimeofday ();
        }
    | exception (Yojson.Json_error message | Yojson.Safe.Util.Type_error (message, _)) ->
        unreachable ("unreadable snapshot: " ^ message)

let refresh_hosts hosts =
  Settings.make_directory_with_parents (Settings.previews_directory ());
  let previous_index =
    Option.value (Cache_index.load_index ()) ~default:{ Cache_index.hosts = []; written_at = 0. }
  in
  let previous_snapshot_of_host host =
    Option.value (List.assoc_opt host previous_index.hosts) ~default:Cache_index.empty_host_snapshot
  in
  (* Until a host reports in, show what we had for it. *)
  let snapshots = ref (List.map (fun host -> (host, previous_snapshot_of_host host)) hosts) in
  let publish () =
    Cache_index.save_index { Cache_index.hosts = !snapshots; written_at = Unix.gettimeofday () };
    Refresh_lock.keep_lock_fresh ()
  in
  let started_at = Unix.gettimeofday () in
  (* Each host is published as soon as its snapshot arrives, so the fast ones show first. *)
  Process.run_all ~timeout_seconds:Settings.host_snapshot_timeout_seconds
    (List.map
       (fun host -> (host, Rex_cli.rex_executable_path (), snapshot_script_arguments host))
       hosts)
    ~on_finished:(fun host result ->
      let snapshot =
        host_snapshot_of_script_result ~previous:(previous_snapshot_of_host host) host result
      in
      Activity_log.log_line "%s: %d tabs in %.2fs" (Host.label_of_host host)
        (List.length snapshot.tabs)
        (Unix.gettimeofday () -. started_at);
      snapshots :=
        List.map
          (fun (listed_host, listed_snapshot) ->
            if listed_host = host then (host, snapshot) else (listed_host, listed_snapshot))
          !snapshots;
      publish ());
  !snapshots

(* Previews of tabs that are gone, or of hosts no longer configured, would pile up. *)
let delete_previews_not_in snapshots =
  let kept = Hashtbl.create 64 in
  List.iter
    (fun (_, snapshot) ->
      List.iter
        (fun (tab : Tab.t) -> Hashtbl.replace kept (Filename.basename tab.preview_path) ())
        snapshot.Cache_index.tabs)
    snapshots;
  let directory = Settings.previews_directory () in
  Array.iter
    (fun name ->
      if Filename.check_suffix name ".html" && not (Hashtbl.mem kept name) then
        try Sys.remove (Filename.concat directory name) with Sys_error _ -> ())
    (try Sys.readdir directory with Sys_error _ -> [||])

let refresh_all_hosts () =
  if Refresh_lock.claim_lock_for_this_process () then
    Fun.protect ~finally:Refresh_lock.release_lock (fun () ->
        Program_icon.ensure_icons_extracted ();
        delete_previews_not_in (refresh_hosts (configured_hosts ())))
  else Activity_log.log_line "refresh skipped: another refresh is running"
