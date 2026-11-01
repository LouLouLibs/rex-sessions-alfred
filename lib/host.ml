(* A Rex server: this Mac's own, or one that `rex hosts` lists by label. *)

type t =
  | Local
  | Remote of string

let label_of_host = function
  | Local -> "local"
  | Remote label -> label

(* The cache index stores the local host as "" so no host label can collide with it. *)
let key_of_host = function
  | Local -> ""
  | Remote label -> label

let host_of_key = function
  | "" -> Local
  | label -> Remote label

let server_flags_of_host = function
  | Local -> []
  | Remote label -> [ "-S"; label ]
