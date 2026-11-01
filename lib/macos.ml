(* Small pieces of the Mac: notifications, dialogs, the clipboard, bringing Rex forward. *)

let rex_bundle_identifier = "com.superlogical.rex"

let applescript_string_literal text =
  let buffer = Buffer.create (String.length text + 2) in
  Buffer.add_char buffer '"';
  String.iter
    (function
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | character -> Buffer.add_char buffer character)
    text;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let run_applescript script = Process.run ~timeout_seconds:600. "/usr/bin/osascript" [ "-e"; script ]

let show_notification message =
  ignore
    (run_applescript
       (Printf.sprintf "display notification %s with title \"Rex\""
          (applescript_string_literal message)))

(* True if the person pressed the confirm button. *)
let confirm_with_dialog ~message ~confirm_button =
  let button = applescript_string_literal confirm_button in
  Process.succeeded
    (run_applescript
       (Printf.sprintf
          "display dialog %s buttons {\"Cancel\", %s} default button %s cancel button \
           \"Cancel\" with icon caution"
          (applescript_string_literal message) button button))

let copy_to_clipboard text = ignore (Process.run ~stdin_text:text "/usr/bin/pbcopy" [])

let bring_rex_to_front () =
  ignore (Process.run "/usr/bin/open" [ "-b"; rex_bundle_identifier ])
