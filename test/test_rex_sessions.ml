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
  check "empty session name is rejected" (Tab_action.action_of_argument "new:" = None);
  check "flag-like session name is rejected" (Tab_action.action_of_argument "new:-x foo" = None)

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

let test_process_runner () =
  let finished = ref [] in
  Process.run_all ~timeout_seconds:1.
    [
      ("slow", "/bin/sh", [ "-c"; "sleep 0.4; echo slow" ]);
      ("fast", "/bin/sh", [ "-c"; "echo fast" ]);
      ("hung", "/bin/sh", [ "-c"; "sleep 30" ]);
      ("missing", "/nonexistent/program", []);
      ("big", "/bin/sh", [ "-c"; "head -c 300000 /dev/zero; echo oops >&2" ]);
    ]
    ~on_finished:(fun key result -> finished := (key, result) :: !finished);
  let order = List.rev_map fst !finished in
  let result key = List.assoc key !finished in
  check_string "results arrive as children finish" ~expected:"missing fast big slow hung"
    (String.concat " " order);
  check_string "output captured" ~expected:"fast\n" (result "fast").stdout;
  check "hung child is killed at the deadline"
    ((result "hung").timed_out && (result "hung").exit_code = 124);
  check "missing program is 127" ((result "missing").exit_code = 127);
  check "large output doesn't stall"
    (String.length (result "big").stdout = 300000 && (result "big").stderr = "oops\n")

let test_closed_pipe_timeout () =
  let started = Unix.gettimeofday () in
  let finished = ref [] in
  Process.run_all ~timeout_seconds:0.4
    [
      ("closed", "/bin/sh", [ "-c"; "echo $$; exec 1>&- 2>&-; exec sleep 2" ]);
      ("fast", "/bin/sh", [ "-c"; "sleep 0.1; echo fast" ]);
    ]
    ~on_finished:(fun key result -> finished := !finished @ [ (key, result) ]);
  let closed = List.assoc "closed" !finished in
  check "closed pipes still time out" (closed.timed_out && closed.exit_code = 124);
  check "closed pipes do not block other jobs" (List.map fst !finished = [ "fast"; "closed" ]);
  check "timeout remains bounded after EOF" (Unix.gettimeofday () -. started < 1.5);
  let pid = int_of_string (String.trim closed.stdout) in
  check "timed out child is reaped"
    (try ignore (Unix.waitpid [ Unix.WNOHANG ] pid); false
     with Unix.Unix_error (Unix.ECHILD, _, _) -> true);
  let normal = Process.run ~timeout_seconds:1. "/bin/sh"
      [ "-c"; "exec 1>&- 2>&-; sleep 0.1; exit 7" ] in
  check "exit after EOF keeps real status" (not normal.timed_out && normal.exit_code = 7)

let test_refresh_failures () =
  let directory = Filename.temp_file "rex-refresh-test" "" in
  Sys.remove directory;
  Sys.mkdir directory 0o700;
  let variables = [ "alfred_workflow_cache"; "rex_path"; "test_hosts_mode" ] in
  let previous_environment = List.map (fun name -> (name, Sys.getenv_opt name)) variables in
  let rec remove_tree path =
    if Sys.is_directory path then begin
      Array.iter (fun name -> remove_tree (Filename.concat path name)) (Sys.readdir path);
      Unix.rmdir path
    end else Sys.remove path
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun (name, value) -> Unix.putenv name (Option.value value ~default:""))
        previous_environment;
      remove_tree directory)
    (fun () ->
      Unix.putenv "alfred_workflow_cache" directory;
      let mock = Filename.concat directory "rex" in
      Cache_index.write_file_atomically mock
        {|#!/bin/sh
if [ "$1" = hosts ]; then
  case "$test_hosts_mode" in
    command) echo unavailable >&2; exit 1 ;;
    json) echo '{'; exit 0 ;;
    schema) echo '{}'; exit 0 ;;
    entry) echo '{"hosts":[{}]}'; exit 0 ;;
    empty) echo '{"hosts":[]}'; exit 0 ;;
  esac
fi
if [ "$1" = -S ]; then echo offline >&2; exit 1; fi
echo '{"sessions":[]}'
|};
      Unix.chmod mock 0o700;
      Unix.putenv "rex_path" mock;
      Settings.make_directory_with_parents (Settings.previews_directory ());
      let preview = Filename.concat (Settings.previews_directory ()) "remote.html" in
      let tab = { (make_tab ~host:(Host.Remote "devbox") "notes" "draft") with
        preview_path = preview } in
      let previous : Cache_index.host_snapshot =
        { reachable = true; error_message = ""; tabs = [ tab ]; taken_at = 12. } in
      List.iter
        (fun mode ->
          Cache_index.save_index
            { hosts = [ (Host.Remote "devbox", previous) ]; written_at = 0. };
          Cache_index.write_file_atomically preview "old preview";
          Unix.putenv "test_hosts_mode" mode;
          let hosts = Refresh.hosts_to_refresh () in
          check (mode ^ ": discovery retains cached host")
            (hosts = [ Host.Local; Host.Remote "devbox" ]);
          let snapshots = Refresh.refresh_hosts hosts in
          Refresh.delete_previews_not_in snapshots;
          let cached = Option.get (Cache_index.load_index ()) in
          let remote = List.assoc (Host.Remote "devbox") cached.hosts in
          check (mode ^ ": failed refresh retains tabs and timestamp")
            (remote.tabs = [ tab ] && remote.taken_at = 12. && not remote.reachable);
          check (mode ^ ": failed refresh retains preview") (Sys.file_exists preview))
        [ "command"; "json"; "schema"; "entry" ];
      let failed : Process.result =
        { exit_code = 1; stdout = ""; stderr = "session.view: failed"; timed_out = false } in
      let unreachable = Refresh.host_snapshot_of_script_result ~previous Host.Local failed in
      check "essential snapshot failure preserves previous tabs"
        (not unreachable.reachable && unreachable.tabs = previous.tabs);
      let success = { failed with exit_code = 0; stdout = {|{"sessions":[]}|}; stderr = "" } in
      let recovered = Refresh.host_snapshot_of_script_result ~previous:unreachable Host.Local success in
      check "successful empty snapshot clears unreachable status and stale tabs"
        (recovered.reachable && recovered.error_message = "" && recovered.tabs = []
         && recovered.taken_at > previous.taken_at);
      Unix.putenv "test_hosts_mode" "empty";
      let hosts = Refresh.hosts_to_refresh () in
      check "successful empty discovery removes remote hosts" (hosts = [ Host.Local ]);
      Refresh.delete_previews_not_in (Refresh.refresh_hosts hosts);
      check "confirmed removed host loses its preview" (not (Sys.file_exists preview)))

let () =
  test_process_runner ();
  test_closed_pipe_timeout ();
  test_refresh_failures ();
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
