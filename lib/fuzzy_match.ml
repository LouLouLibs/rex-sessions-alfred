(* Score how well a query matches a line of text. Every query word must match;
   a higher score is a better match. *)

let whole_word_score = 300
let substring_score = 100
let substring_at_word_start_bonus = 50
let exact_name_bonus = 1000

let is_alphanumeric = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false

let starts_a_word text index = index = 0 || not (is_alphanumeric text.[index - 1])

let words_of_text text =
  String.split_on_char ' '
    (String.map (fun character -> if is_alphanumeric character then character else ' ') text)
  |> List.filter (fun word -> word <> "")

let index_of_substring ~needle haystack =
  let needle_length = String.length needle and haystack_length = String.length haystack in
  let rec search_from index =
    if index + needle_length > haystack_length then None
    else if String.sub haystack index needle_length = needle then Some index
    else search_from (index + 1)
  in
  search_from 0

(* Letters in order, rewarding runs and word starts. A match whose letters are
   scattered across unrelated words scores under the threshold and is dropped. *)
let subsequence_score ~word text =
  let rec score_from ~word_index ~text_index ~previous_match ~score =
    if word_index = String.length word then Some score
    else
      match String.index_from_opt text text_index word.[word_index] with
      | None -> None
      | Some found ->
          let run_score = if found = previous_match + 1 then 5 else 1 in
          let word_start_score = if starts_a_word text found then 4 else 0 in
          score_from ~word_index:(word_index + 1) ~text_index:(found + 1)
            ~previous_match:found ~score:(score + run_score + word_start_score)
  in
  match score_from ~word_index:0 ~text_index:0 ~previous_match:(-2) ~score:0 with
  | Some score when score >= 3 * String.length word -> Some score
  | _ -> None

let query_word_score ~text ~words_in_text word =
  if List.mem word words_in_text then Some whole_word_score
  else
    match index_of_substring ~needle:word text with
    | Some index ->
        Some
          (substring_score
          + if starts_a_word text index then substring_at_word_start_bonus else 0)
    | None -> subsequence_score ~word text

let query_words_of_query query =
  String.split_on_char ' ' (String.lowercase_ascii query) |> List.filter (fun word -> word <> "")

let score_of_query ~query text =
  let text = String.lowercase_ascii text in
  let words_in_text = words_of_text text in
  List.fold_left
    (fun total word ->
      Option.bind total (fun total ->
          Option.map (( + ) total) (query_word_score ~text ~words_in_text word)))
    (Some 0) (query_words_of_query query)

let query_is_blank query = query_words_of_query query = []
