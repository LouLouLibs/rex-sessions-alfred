(* Tests for the pure parts: matching, naming, paths, capture cleanup, action strings. *)

open Rex_sessions

let failures = ref 0

let check name condition =
  if not condition then begin
    incr failures;
    Printf.printf "FAIL %s\n" name
  end

let check_string name ~expected actual =
  if expected <> actual then begin
    incr failures;
    Printf.printf "FAIL %s\n  expected %S\n  got      %S\n" name expected actual
  end

let matches query text = Option.is_some (Fuzzy_match.score_of_query ~query text)

let score query text = Option.value (Fuzzy_match.score_of_query ~query text) ~default:(-1)

let test_fuzzy_match () =
  check "blank query matches anything" (matches "" "anything");
  check "subsequence across word starts" (matches "lmk" "library-maker-app");
  check "every word must match" (not (matches "orbit zzz" "orbit budget"));
  check "words match in any order" (matches "bud orbit" "orbit budget");
  check "scattered letters are rejected" (not (matches "tamarind" "NOTES-WRITING welfare decomposition Demand-based asset pricing introduction"));
  check "case-insensitive" (matches "NOTES" "notes drafts");
  check "whole word beats substring" (score "orbit" "orbit" > score "orbi" "orbit");
  check "substring beats subsequence" (score "lib" "library" > score "lby" "library")

let make_tab ?(host = Host.Local) ?(terminal_title = "") session_label tab_name : Tab.t =
  {
    host;
    session_id = "session:" ^ session_label;
    session_label;
    window_id = "window:" ^ tab_name;
    tab_index = 1;
    tab_name;
    terminal_title;
    display_path = "";
    pane_count = 1;
    pane_labels = [];
    programs = [];
    icon_path = "";
    preview_path = "";
  }

let test_ranking () =
  let tabs =
    [
      make_tab "orbit-q" "writing";
      make_tab ~host:(Host.Remote "devbox") "orbit" "budget";
      make_tab ~host:(Host.Remote "devbox") "misc-devbox" "Replication (Tamarind)";
    ]
  in
  let first_session query =
    match Picker.ranked_tabs ~query tabs with
    | first :: _ -> first.session_label
    | [] -> "<none>"
  in
  check_string "exact session name ranks first" ~expected:"orbit" (first_session "orbit");
  check_string "host name narrows the search" ~expected:"misc-devbox" (first_session "devbox tamarind");
  check_string "blank query keeps Rex's order" ~expected:"orbit-q" (first_session "")

let test_display_paths () =
  check_string "OSC 7 URL on a Mac" ~expected:"~/Dropbox/notes"
    (Display_text.display_path_of_reported_directory "file://laptop/Users/alice/Dropbox/notes");
  check_string "OSC 7 URL on Linux" ~expected:"~/WORK"
    (Display_text.display_path_of_reported_directory "file://devbox/home/alice/WORK");
  check_string "home itself" ~expected:"~"
    (Display_text.display_path_of_reported_directory "file://devbox/home/alice");
  check_string "path outside home" ~expected:"/etc/nixos"
    (Display_text.display_path_of_reported_directory "/etc/nixos");
  check_string "nothing reported" ~expected:"" (Display_text.display_path_of_reported_directory "")

let test_tab_names () =
  let name ?(window_label = "") ?(terminal_title = "") ?(display_path = "") () =
    Display_text.tab_name_of_window ~window_label ~terminal_title ~display_path ~tab_index:2
  in
  check_string "own label wins" ~expected:"budget"
    (name ~window_label:"budget" ~terminal_title:"ILL requests" ());
  check_string "generic label falls back to title" ~expected:"ILL requests"
    (name ~window_label:"Window 810d4052" ~terminal_title:"ILL requests" ());
  check_string "shell prompt title falls back to directory" ~expected:"writing"
    (name ~window_label:"Window 36ff7ca2" ~terminal_title:"alice@laptop:~/x/writing"
       ~display_path:"~/x/writing" ());
  check_string "home directory" ~expected:"~" (name ~display_path:"~" ());
  check_string "nothing known" ~expected:"tab 2" (name ())

let test_capture_cleanup () =
  check_string "wrapper removed" ~expected:"a\n  b"
    (Display_text.screen_html_of_capture
       "<div style=\"font-family: monospace; white-space: pre;\">\na\n  b\n</div>\n");
  check_string "indentation kept after a cut" ~expected:"    indented\nnext"
    (Display_text.screen_html_of_capture "    indented\nnext\n</div>");
  check_string "html escaped" ~expected:"a &lt;b&gt; &amp; &quot;c&quot;"
    (Display_text.html_escape "a <b> & \"c\"")

let test_action_round_trip () =
  let target =
    { Tab_action.host = Host.Remote "devbox"; session_id = "session:1"; window_id = "window:2" }
  in
  List.iter
    (fun action ->
      check "action survives a round trip"
        (Tab_action.action_of_argument (Tab_action.argument_of_action action) = Some action))
    [
      Tab_action.Go_to_tab target;
      Close_tab { target with host = Host.Local };
      Copy_tab_ids target;
      Create_local_session "notes: draft";
    ];
  check "garbage is rejected" (Tab_action.action_of_argument "go:only-one-part" = None);
  check "empty session name is rejected" (Tab_action.action_of_argument "new:" = None)

let test_index_round_trip () =
  let tab = make_tab ~host:(Host.Remote "devbox") ~terminal_title:"Claude Code" "notes" "drafts" in
  let index =
    {
      Cache_index.hosts =
        [
          (Host.Local, Cache_index.empty_host_snapshot);
          ( Host.Remote "devbox",
            { reachable = false; error_message = "timeout"; tabs = [ tab ]; taken_at = 12.5 } );
        ];
      written_at = 99.;
    }
  in
  check "index survives JSON"
    (Cache_index.index_of_json (Cache_index.json_of_index index) = index)

let test_program_icons () =
  let program ?(argv0 = "") ?(process_name = "") ?(invoked_path = "") () =
    Program_icon.program_name_of_process { argv0; process_name; invoked_path }
  in
  check_string "argv0 beats a version-number name" ~expected:"claude"
    (program ~argv0:"claude" ~process_name:"2.1.309" ());
  check_string "login shell" ~expected:"zsh" (program ~argv0:"-zsh" ~process_name:"zsh" ());
  check_string "nix wrapper" ~expected:"claude"
    (program ~argv0:"/nix/store/abc-claude-code-2.1.300/bin/claude"
       ~process_name:".claude-unwrapped" ());
  check_string "only a wrapped name" ~expected:"vm-launcher"
    (program ~process_name:".vm-launcher-wrapped" ());
  let glyphs = [ "neovim"; "python"; "claude"; "zsh"; "terminal"; "k9s" ] in
  let glyph ?(aliases = Program_icon.glyph_of_program_alias) name =
    Program_icon.glyph_of_program_name ~glyph_exists:(fun g -> List.mem g glyphs) ~aliases name
  in
  check_string "alias" ~expected:"neovim" (glyph "nvim");
  check_string "version suffix" ~expected:"python" (glyph "python3.13");
  check_string "digits inside a name stay" ~expected:"k9s" (glyph "k9s");
  check_string "unknown program" ~expected:"terminal" (glyph "julia");
  check_string "user alias" ~expected:"claude"
    (glyph ~aliases:(Program_icon.user_aliases_of_setting " vm-launcher = claude, x=" @ Program_icon.glyph_of_program_alias) "vm-launcher")

let () =
  test_fuzzy_match ();
  test_program_icons ();
  test_ranking ();
  test_display_paths ();
  test_tab_names ();
  test_capture_cleanup ();
  test_action_round_trip ();
  test_index_round_trip ();
  if !failures > 0 then begin
    Printf.printf "%d check(s) failed\n" !failures;
    exit 1
  end
  else print_endline "all checks passed"
