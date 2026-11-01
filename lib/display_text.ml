(* Turning what Rex reports into the text the picker shows. *)

let html_escape text =
  let buffer = Buffer.create (String.length text) in
  String.iter
    (function
      | '&' -> Buffer.add_string buffer "&amp;"
      | '<' -> Buffer.add_string buffer "&lt;"
      | '>' -> Buffer.add_string buffer "&gt;"
      | '"' -> Buffer.add_string buffer "&quot;"
      | character -> Buffer.add_char buffer character)
    text;
  Buffer.contents buffer

let remove_prefix ~prefix text =
  if String.starts_with ~prefix text then
    Some (String.sub text (String.length prefix) (String.length text - String.length prefix))
  else None

let remove_suffix ~suffix text =
  if String.ends_with ~suffix text then
    Some (String.sub text 0 (String.length text - String.length suffix))
  else None

(* /Users/name/rest and /home/name/rest become ~/rest. *)
let abbreviate_home_directory path =
  match String.split_on_char '/' path with
  | "" :: ("Users" | "home") :: _user :: rest -> String.concat "/" ("~" :: rest)
  | _ -> path

(* Terminals report their directory as file://host/path (OSC 7) or as a plain path. *)
let display_path_of_reported_directory reported =
  if reported = "" then ""
  else
    let path =
      match remove_prefix ~prefix:"file://" reported with
      | None -> reported
      | Some host_and_path -> (
          match String.index_opt host_and_path '/' with
          | Some slash -> String.sub host_and_path slash (String.length host_and_path - slash)
          | None -> "/")
    in
    abbreviate_home_directory path

(* Rex names unlabelled tabs "Window 1a2b3c4d". *)
let generic_window_label_regexp = Str.regexp "^Window [0-9a-f]+$"

let is_generic_window_label label = Str.string_match generic_window_label_regexp label 0

(* Shells set titles like user@host:~/path, which say less than the directory does. *)
let shell_prompt_title_regexp = Str.regexp "^[^@ ]+@[^: ]+:"

let is_shell_prompt_title title = Str.string_match shell_prompt_title_regexp title 0

let last_directory_name_of_path path =
  match Filename.basename path with
  | "" | "." | "/" -> None
  | name -> Some name

(* The most telling name for a tab: its own label, else what the program titled it
   (Claude Code titles the terminal with its current task), else its directory. *)
let tab_name_of_window ~window_label ~terminal_title ~display_path ~tab_index =
  if window_label <> "" && not (is_generic_window_label window_label) then window_label
  else if terminal_title <> "" && not (is_shell_prompt_title terminal_title) then terminal_title
  else
    match last_directory_name_of_path display_path with
    | Some name -> name
    | None when display_path <> "" -> display_path
    | None -> Printf.sprintf "tab %d" tab_index

let strip_newlines_at_both_ends text =
  let is_newline character = character = '\n' || character = '\r' in
  let length = String.length text in
  let first = ref 0 and last = ref (length - 1) in
  while !first < length && is_newline text.[!first] do incr first done;
  while !last >= !first && is_newline text.[!last] do decr last done;
  String.sub text !first (!last - !first + 1)

let rex_capture_wrapper_opening = {|<div style="font-family: monospace; white-space: pre;">|}

(* Rex wraps a capture in one monospace div; drop it, the preview page has its own.
   Leading spaces are only stripped before the wrapper: after a cut they're indentation. *)
let screen_html_of_capture capture =
  let without_opening =
    let left_trimmed =
      let rec first_non_space index =
        if index < String.length capture && String.contains " \t\r\n" capture.[index] then
          first_non_space (index + 1)
        else index
      in
      let start = first_non_space 0 in
      String.sub capture start (String.length capture - start)
    in
    match remove_prefix ~prefix:rex_capture_wrapper_opening left_trimmed with
    | Some rest -> rest
    | None -> capture
  in
  let right_trimmed =
    let rec last_non_space index =
      if index >= 0 && String.contains " \t\r\n" without_opening.[index] then
        last_non_space (index - 1)
      else index
    in
    String.sub without_opening 0 (last_non_space (String.length without_opening - 1) + 1)
  in
  let without_closing =
    Option.value (remove_suffix ~suffix:"</div>" right_trimmed) ~default:right_trimmed
  in
  strip_newlines_at_both_ends without_closing

let plural_count count noun = Printf.sprintf "%d %s%s" count noun (if count = 1 then "" else "s")
