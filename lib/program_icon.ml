(* Which of Rex's program glyphs to show for a pane. Rex doesn't report the icon it
   picked, so match the pane's foreground program against the glyph names
   (block-glyph-<name> in Rex's app bundle, extracted to <cache>/icons/<name>.png). *)

type foreground_process = {
  argv0 : string;
  process_name : string;
  invoked_path : string;
}

let fallback_glyph = "terminal"

(* Programs whose glyph has another name. Glyph names that equal the program
   (claude, codex, zsh, git, docker, htop, ...) need no entry. *)
let glyph_of_program_alias =
  [
    ("nvim", "neovim"); ("vi", "vim"); ("view", "vim"); ("helix", "hx");
    ("emacsclient", "emacs"); ("python3", "python"); ("ipython", "python");
    ("pip", "python"); ("pip3", "python"); ("npm", "node"); ("npx", "node");
    ("bunx", "bun"); ("pnpx", "pnpm"); ("uvx", "uv"); ("cargo", "rust"); ("rustc", "rust");
    ("rustup", "rust"); ("kubectl", "kube"); ("gh", "github"); ("glab", "gitlab");
    ("lazygit", "git"); ("tig", "git"); ("gitui", "git"); ("docker-compose", "docker");
    ("swiftc", "swift"); ("xcodebuild", "xcode"); ("irb", "ruby"); ("bundle", "ruby");
    ("psql", "postgres"); ("pgcli", "postgres"); ("sqlite3", "sqlite"); ("litecli", "sqlite");
    ("tofu", "terraform"); ("nu", "nushell"); ("sh", "bash"); ("dash", "bash");
    ("nix-shell", "nix"); ("nix-build", "nix"); ("iex", "elixir"); ("mix", "elixir");
    ("llama-server", "llama-cpp"); ("llama-cli", "llama-cpp"); ("jupyter-lab", "jupyter");
    ("cursor-agent", "cursor"); ("gemini-cli", "gemini");
  ]

(* Extra aliases from the icon_aliases workflow variable: "vm-launcher=claude, julia=terminal". *)
let user_aliases_of_setting setting =
  String.split_on_char ',' setting
  |> List.filter_map (fun pair ->
         match String.split_on_char '=' pair with
         | [ program; glyph ] when String.trim program <> "" && String.trim glyph <> "" ->
             Some (String.lowercase_ascii (String.trim program), String.lowercase_ascii (String.trim glyph))
         | _ -> None)

let user_aliases () =
  match Settings.non_empty_environment_value "icon_aliases" with
  | Some setting -> user_aliases_of_setting setting
  | None -> []

let remove_suffix_if_present ~suffix text =
  Option.value (Display_text.remove_suffix ~suffix text) ~default:text

let is_version_number text =
  text <> "" && String.for_all (fun character -> character = '.' || ('0' <= character && character <= '9')) text

(* "/nix/store/...-claude-code/bin/.claude-unwrapped" -> "claude"; "-zsh" (a login
   shell) -> "zsh". *)
let program_name_of_command text =
  let base = String.lowercase_ascii (Filename.basename text) in
  let rec strip_leading_marks name =
    if name <> "" && (name.[0] = '-' || name.[0] = '.') then
      strip_leading_marks (String.sub name 1 (String.length name - 1))
    else name
  in
  strip_leading_marks base
  |> remove_suffix_if_present ~suffix:"-unwrapped"
  |> remove_suffix_if_present ~suffix:"-wrapped"

(* argv0 is what the person typed; the process name can be a version ("2.1.309"). *)
let program_name_of_process process =
  List.map program_name_of_command [ process.argv0; process.invoked_path; process.process_name ]
  |> List.find_opt (fun name -> name <> "" && name <> "." && not (is_version_number name))
  |> Option.value ~default:""

(* python3.13 -> python3 -> python *)
let without_trailing_version name =
  let rec last_kept index =
    if index > 0 && (name.[index - 1] = '.' || ('0' <= name.[index - 1] && name.[index - 1] <= '9'))
    then last_kept (index - 1)
    else index
  in
  let kept = last_kept (String.length name) in
  if kept = 0 then name else String.sub name 0 kept

let icons_directory () = Filename.concat (Settings.cache_directory ()) "icons"

let icon_path_of_glyph glyph = Filename.concat (icons_directory ()) (glyph ^ ".png")

let glyph_of_program_name ~glyph_exists ~aliases program_name =
  let candidates =
    List.concat_map
      (fun name -> [ List.assoc_opt name aliases; Some name ])
      [ program_name; without_trailing_version program_name ]
    |> List.filter_map Fun.id
  in
  match List.find_opt glyph_exists candidates with
  | Some glyph -> glyph
  | None -> fallback_glyph

let icon_path_of_program_name program_name =
  let glyph =
    glyph_of_program_name
      ~glyph_exists:(fun glyph -> Sys.file_exists (icon_path_of_glyph glyph))
      ~aliases:(user_aliases () @ glyph_of_program_alias)
      program_name
  in
  let path = icon_path_of_glyph glyph in
  if Sys.file_exists path then path else ""

(* The workflow ships no Rex artwork: the bundled extract_rex_icons helper renders the
   glyphs from the installed Rex app, again whenever Rex's asset catalog changes. *)
let rex_app_candidates () =
  [
    "/Applications/Rex.app";
    "/Applications/Rex Beta.app";
    Filename.concat (Settings.home_directory ()) "Applications/Rex.app";
    Filename.concat (Settings.home_directory ()) "Applications/Rex Beta.app";
  ]

let asset_catalog_of_app app =
  Filename.concat app
    "Contents/Resources/SuperBlockIcons_SuperBlockIcons.bundle/Contents/Resources/Assets.car"

let installed_rex_app () =
  match Settings.non_empty_environment_value "rex_app" with
  | Some app -> Some app
  | None -> List.find_opt (fun app -> Sys.file_exists (asset_catalog_of_app app)) (rex_app_candidates ())

let extraction_stamp_path () = Filename.concat (icons_directory ()) ".extracted-from"

let read_file_or_empty path =
  try In_channel.with_open_bin path In_channel.input_all with Sys_error _ -> ""

let ensure_icons_extracted () =
  match installed_rex_app () with
  | None -> Activity_log.log_line "icons: no Rex app found; set rex_app"
  | Some app -> (
      match Unix.stat (asset_catalog_of_app app) with
      | exception Unix.Unix_error _ -> Activity_log.log_line "icons: no glyph catalog in %s" app
      | catalog ->
          let stamp = Printf.sprintf "%s %.0f %d" app catalog.st_mtime catalog.st_size in
          if read_file_or_empty (extraction_stamp_path ()) <> stamp then begin
            let helper = Filename.concat (Settings.workflow_directory ()) "extract_rex_icons" in
            let result = Process.run ~timeout_seconds:60. helper [ app; icons_directory () ] in
            if Process.succeeded result then begin
              Out_channel.with_open_bin (extraction_stamp_path ()) (fun channel ->
                  output_string channel stamp);
              Activity_log.log_line "icons: %s" (String.trim result.stdout)
            end
            else Activity_log.log_line "icons: extraction failed: %s" (String.trim result.stderr)
          end)
