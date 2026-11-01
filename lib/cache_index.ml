(* The cached list of tabs on every host, which the picker reads instead of asking
   slow remote hosts on every keystroke. *)

type host_snapshot = {
  reachable : bool;
  error_message : string;
  tabs : Tab.t list;
  taken_at : float;
}

type t = {
  hosts : (Host.t * host_snapshot) list;  (* in display order *)
  written_at : float;
}

let empty_host_snapshot = { reachable = true; error_message = ""; tabs = []; taken_at = 0. }

let json_of_host_snapshot host snapshot : Yojson.Safe.t =
  `Assoc
    [
      ("host", `String (Host.key_of_host host));
      ("reachable", `Bool snapshot.reachable);
      ("error_message", `String snapshot.error_message);
      ("taken_at", `Float snapshot.taken_at);
      ("tabs", `List (List.map Tab.json_of_tab snapshot.tabs));
    ]

let host_snapshot_of_json json =
  let reachable =
    match Yojson.Safe.Util.member "reachable" json with
    | `Bool value -> value
    | _ -> true
  in
  let taken_at =
    match Yojson.Safe.Util.member "taken_at" json with
    | `Float value -> value
    | `Int value -> float_of_int value
    | _ -> 0.
  in
  ( Host.host_of_key (Tab.string_field "host" json),
    {
      reachable;
      error_message = Tab.string_field "error_message" json;
      tabs = List.map Tab.tab_of_json (Tab.list_field "tabs" json);
      taken_at;
    } )

let json_of_index index : Yojson.Safe.t =
  `Assoc
    [
      ("written_at", `Float index.written_at);
      ( "hosts",
        `List (List.map (fun (host, snapshot) -> json_of_host_snapshot host snapshot) index.hosts)
      );
    ]

let index_of_json json =
  let written_at =
    match Yojson.Safe.Util.member "written_at" json with
    | `Float value -> value
    | `Int value -> float_of_int value
    | _ -> 0.
  in
  { hosts = List.map host_snapshot_of_json (Tab.list_field "hosts" json); written_at }

let load_index () =
  match Yojson.Safe.from_file (Settings.index_path ()) with
  | json -> Some (index_of_json json)
  | exception (Sys_error _ | Yojson.Json_error _) -> None

(* Write to a temporary file and rename, so a reader never sees half an index. *)
let write_file_atomically path contents =
  let temporary_path = path ^ ".tmp" ^ string_of_int (Unix.getpid ()) in
  Out_channel.with_open_bin temporary_path (fun channel ->
      Out_channel.output_string channel contents);
  Sys.rename temporary_path path

let save_index index =
  Settings.make_directory_with_parents (Settings.cache_directory ());
  write_file_atomically (Settings.index_path ()) (Yojson.Safe.to_string (json_of_index index))

let age_of_index_seconds index = Unix.gettimeofday () -. index.written_at

(* After closing or creating something, make the next picker open refresh at once. *)
let mark_index_stale () =
  match load_index () with
  | Some index -> save_index { index with written_at = 0. }
  | None -> ()

let tabs_of_host index host =
  match List.assoc_opt host index.hosts with
  | Some snapshot -> snapshot.tabs
  | None -> []
