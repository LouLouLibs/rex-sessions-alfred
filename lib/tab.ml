(* One Rex tab (a "window" in Rex's API) as the picker lists it. *)

type t = {
  host : Host.t;
  session_id : string;
  session_label : string;
  window_id : string;
  tab_index : int;
  tab_name : string;
  terminal_title : string;
  display_path : string;
  pane_count : int;
  pane_labels : string list;
  programs : string list;  (* each pane's foreground program, focused pane first *)
  icon_path : string;  (* Rex's glyph for the focused pane's program; "" if none *)
  preview_path : string;
}

let json_of_tab tab : Yojson.Safe.t =
  `Assoc
    [
      ("host", `String (Host.key_of_host tab.host));
      ("session_id", `String tab.session_id);
      ("session_label", `String tab.session_label);
      ("window_id", `String tab.window_id);
      ("tab_index", `Int tab.tab_index);
      ("tab_name", `String tab.tab_name);
      ("terminal_title", `String tab.terminal_title);
      ("display_path", `String tab.display_path);
      ("pane_count", `Int tab.pane_count);
      ("pane_labels", `List (List.map (fun label -> `String label) tab.pane_labels));
      ("programs", `List (List.map (fun program -> `String program) tab.programs));
      ("icon_path", `String tab.icon_path);
      ("preview_path", `String tab.preview_path);
    ]

(* Lenient readers: a missing or odd field becomes a default instead of an error. *)
let string_field name (json : Yojson.Safe.t) =
  match Yojson.Safe.Util.member name json with
  | `String value -> value
  | _ -> ""

let int_field name (json : Yojson.Safe.t) =
  match Yojson.Safe.Util.member name json with
  | `Int value -> value
  | `Float value -> int_of_float value
  | _ -> 0

(* rex.json encodes an empty Lua table as {}, so an object can stand for an empty list. *)
let list_field name (json : Yojson.Safe.t) =
  match Yojson.Safe.Util.member name json with
  | `List items -> items
  | _ -> []

let string_list_field name json =
  List.filter_map
    (function
      | `String value -> Some value
      | _ -> None)
    (list_field name json)

let tab_of_json json =
  {
    host = Host.host_of_key (string_field "host" json);
    session_id = string_field "session_id" json;
    session_label = string_field "session_label" json;
    window_id = string_field "window_id" json;
    tab_index = int_field "tab_index" json;
    tab_name = string_field "tab_name" json;
    terminal_title = string_field "terminal_title" json;
    display_path = string_field "display_path" json;
    pane_count = int_field "pane_count" json;
    pane_labels = string_list_field "pane_labels" json;
    programs = string_list_field "programs" json;
    icon_path = string_field "icon_path" json;
    preview_path = string_field "preview_path" json;
  }
