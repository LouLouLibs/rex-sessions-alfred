(* The Alfred Script Filter: rank cached tabs against the query and print Alfred's JSON.
   It never waits on remote hosts; a stale cache only starts a background refresh. *)

(* Two sessions with one name (say, a duplicate set left running) get an ID suffix. *)
let sessions_sharing_a_label tabs =
  let first_session_of_label = Hashtbl.create 16 and shared = Hashtbl.create 4 in
  List.iter
    (fun (tab : Tab.t) ->
      let key = (tab.host, tab.session_label) in
      match Hashtbl.find_opt first_session_of_label key with
      | None -> Hashtbl.add first_session_of_label key tab.session_id
      | Some session_id when session_id <> tab.session_id -> Hashtbl.replace shared key ()
      | Some _ -> ())
    tabs;
  fun (tab : Tab.t) -> Hashtbl.mem shared (tab.host, tab.session_label)

let last_characters count text =
  let length = String.length text in
  if length <= count then text else String.sub text (length - count) count

let session_display_name ~label_is_shared (tab : Tab.t) =
  if label_is_shared tab then tab.session_label ^ " #" ^ last_characters 6 tab.session_id
  else tab.session_label

let unique_in_order items =
  List.rev
    (List.fold_left (fun kept item -> if List.mem item kept then kept else item :: kept) [] items)

(* "3 panes: claude, zsh" *)
let panes_summary_of_tab (tab : Tab.t) =
  let count = Display_text.plural_count tab.pane_count "pane" in
  match unique_in_order tab.programs with
  | [] -> count
  | programs -> count ^ ": " ^ String.concat ", " programs

let subtitle_of_tab (tab : Tab.t) =
  let title_adds_something =
    tab.terminal_title <> ""
    && tab.terminal_title <> tab.tab_name
    && not (Display_text.is_shell_prompt_title tab.terminal_title)
  in
  String.concat "  ·  "
    (List.filter
       (fun part -> part <> "")
       [
         (match tab.host with Host.Local -> "" | Host.Remote label -> label);
         tab.display_path;
         (if title_adds_something then tab.terminal_title else "");
         (if tab.pane_count > 1 then panes_summary_of_tab tab else "");
       ])

(* Without an icon field Alfred shows the workflow's own icon. *)
let icon_fields_of_tab (tab : Tab.t) =
  if tab.icon_path = "" then [] else [ ("icon", `Assoc [ ("path", `String tab.icon_path) ]) ]

let alfred_item_of_tab ~label_is_shared (tab : Tab.t) : Yojson.Safe.t =
  let target = Tab_action.tab_target_of_tab tab in
  let title = Printf.sprintf "%s › %s" (session_display_name ~label_is_shared tab) tab.tab_name in
  `Assoc
    (icon_fields_of_tab tab
    @ [
      ("uid", `String (Host.label_of_host tab.host ^ "|" ^ tab.window_id));
      ("title", `String title);
      ("subtitle", `String (subtitle_of_tab tab));
      ("arg", `String (Tab_action.argument_of_action (Go_to_tab target)));
      ("autocomplete", `String tab.session_label);
      ("quicklookurl", `String tab.preview_path);
      ("text", `Assoc [ ("copy", `String tab.session_id); ("largetype", `String title) ]);
      ( "mods",
        `Assoc
          [
            ( "cmd",
              `Assoc
                [
                  ("arg", `String (Tab_action.argument_of_action (Close_tab target)));
                  ( "subtitle",
                    `String
                      (Printf.sprintf "Close tab “%s” and everything running in it" tab.tab_name)
                  );
                ] );
            ( "alt",
              `Assoc
                [
                  ("arg", `String (Tab_action.argument_of_action (Copy_tab_ids target)));
                  ( "subtitle",
                    `String (Printf.sprintf "Copy IDs  %s  %s" tab.session_id tab.window_id) );
                ] );
          ] );
    ])

let searchable_text_of_tab (tab : Tab.t) =
  String.concat " "
    ([
       tab.session_label;
       tab.tab_name;
       tab.terminal_title;
       tab.display_path;
       Host.label_of_host tab.host;
     ]
    @ tab.pane_labels @ tab.programs)

(* Matches in the session or tab name beat ones buried in a path or title, and
   typing a name exactly ("orbit") puts that session above near-misses ("orbit-q"). *)
let ranking_score_of_tab ~query (tab : Tab.t) =
  if Fuzzy_match.query_is_blank query then Some 0
  else
    Option.map
      (fun score_anywhere ->
        let name_score =
          Option.value ~default:0
            (Fuzzy_match.score_of_query ~query (tab.session_label ^ " " ^ tab.tab_name))
        in
        let normalized_query = String.lowercase_ascii (String.trim query) in
        let exact_name_score =
          if
            List.mem normalized_query
              [ String.lowercase_ascii tab.session_label; String.lowercase_ascii tab.tab_name ]
          then Fuzzy_match.exact_name_bonus
          else 0
        in
        score_anywhere + name_score + exact_name_score)
      (Fuzzy_match.score_of_query ~query (searchable_text_of_tab tab))

(* Best score first; ties keep host order (local first), then Rex's own order. *)
let ranked_tabs ~query tabs =
  List.filter_map
    (fun (position, tab) ->
      Option.map (fun score -> (score, position, tab)) (ranking_score_of_tab ~query tab))
    (List.mapi (fun position tab -> (position, tab)) tabs)
  |> List.stable_sort (fun (score_a, position_a, _) (score_b, position_b, _) ->
         match compare score_b score_a with
         | 0 -> compare position_a position_b
         | order -> order)
  |> List.map (fun (_, _, tab) -> tab)

let unselectable_item ~title ~subtitle : Yojson.Safe.t =
  `Assoc [ ("title", `String title); ("subtitle", `String subtitle); ("valid", `Bool false) ]

let new_session_item query : Yojson.Safe.t =
  let name = String.trim query in
  `Assoc
    [
      ("title", `String (Printf.sprintf "New session “%s”" name));
      ( "subtitle",
        `String (Printf.sprintf "Create a local Rex session named %s and switch to it" name) );
      ("arg", `String (Tab_action.argument_of_action (Create_local_session name)));
    ]

(* First run: local takes a tenth of a second, so show it now and fetch the rest behind. *)
let current_index () =
  match Cache_index.load_index () with
  | None ->
      Refresh.refresh_hosts [ Host.Local ];
      Refresh_lock.spawn_detached_refresh ();
      Option.value (Cache_index.load_index ()) ~default:{ Cache_index.hosts = []; written_at = 0. }
  | Some index ->
      if Cache_index.age_of_index_seconds index > Settings.stale_after_seconds () then
        Refresh_lock.spawn_detached_refresh ();
      index

let alfred_response_of_query query : Yojson.Safe.t =
  let index = current_index () in
  let tabs = List.concat_map (fun (_, snapshot) -> snapshot.Cache_index.tabs) index.hosts in
  let unreachable_hosts =
    List.filter_map
      (fun (host, snapshot) ->
        if snapshot.Cache_index.reachable then None else Some (Host.label_of_host host))
      index.hosts
  in
  let label_is_shared = sessions_sharing_a_label tabs in
  let tab_items = List.map (alfred_item_of_tab ~label_is_shared) (ranked_tabs ~query tabs) in
  let query_is_blank = Fuzzy_match.query_is_blank query in
  let trailing_items =
    if not query_is_blank then [ new_session_item query ]
    else if tab_items = [] then
      [ unselectable_item ~title:"No Rex tabs found" ~subtitle:"Type a name to create a session" ]
    else []
  in
  let unreachable_items =
    if unreachable_hosts <> [] && query_is_blank then
      [
        unselectable_item
          ~title:("Unreachable: " ^ String.concat ", " unreachable_hosts)
          ~subtitle:"Showing their last known tabs, if any";
      ]
    else []
  in
  (* While a refresh runs, have Alfred ask again to pick up hosts as they report in. *)
  let rerun = if Refresh_lock.refresh_is_running () then [ ("rerun", `Float 0.5) ] else [] in
  `Assoc
    ([
       ("items", `List (tab_items @ trailing_items @ unreachable_items));
       (* With a query, our ranking wins; without one, Alfred floats recent picks up. *)
       ("skipknowledge", `Bool (not query_is_blank));
     ]
    @ rerun)
