(* util functions *)

let is_lower_case c =
  'a' <= c && c <= 'z'

let is_upper_case c =
  'A' <= c && c <= 'Z'

let is_alpha c =
  is_lower_case c || is_upper_case c

let is_digit c =
  '0' <= c && c <= '9'

let is_alphanum c =
  is_lower_case c ||
  is_upper_case c ||
  is_digit c

let is_blank c =
  String.contains " \012\n\r\t" c

let explode s =
  List.of_seq (String.to_seq s)

let implode ls =
  String.of_seq (List.to_seq ls)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ (loop ())
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

(* end of util functions *)

(* parser combinators *)

type 'a parser = char list -> ('a * char list) option

let parse (p : 'a parser) (s : string) : ('a * char list) option =
  p (explode s)

let pure (x : 'a) : 'a parser =
  fun ls -> Some (x, ls)

let fail : 'a parser = fun ls -> None

let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> q a ls
  | None -> None

let (>>=) = bind
let (let*) = bind

let read : char parser =
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls)
  | _ -> None

let satisfy (f : char -> bool) : char parser =
  fun ls ->
  match ls with
  | x :: ls ->
    if f x then Some (x, ls)
    else None
  | _ -> None

let char (c : char) : char parser =
  satisfy (fun x -> x = c)

let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None

let (>>) = seq

let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) ->
    (match p2 ls with
     | Some (_, ls) -> Some (x, ls)
     | None -> None)
  | None -> None

let (<<) = seq'

let alt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls)  -> Some (x, ls)
  | None -> p2 ls

let (<|>) = alt

let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> Some (f a, ls)
  | None -> None

let (>|=) = map

let (>|) = fun p c -> map p (fun _ -> c)

let rec many (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

let rec many1 (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None

let rec many' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

let rec many1' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None

let whitespace : unit parser =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c
    then Some ((), ls)
    else None
  | _ -> None

let ws : unit parser =
  (many whitespace) >| ()

let ws1 : unit parser =
  (many1 whitespace) >| ()

let digit : char parser =
  satisfy is_digit

let literal (s : string) : unit parser =
  fun ls ->
  let cs = explode s in
  let rec loop cs ls =
    match cs, ls with
    | [], _ -> Some ((), ls)
    | c :: cs, x :: xs ->
      if x = c
      then loop cs xs
      else None
    | _ -> None
  in loop cs ls

let keyword (s : string) : unit parser =
  (literal s) >> ws >| ()

(* end of parser combinators *)

(* Interprets a program written in the Part1 Stack Language.
 * Required by the autograder, do not change its type. *)

(* Section one: define atomic data type *)
type constant = Num of int
                | Bool of bool
                | Nothing

type command = Push of constant
             | Pop of constant
             | Trace of constant
             | Add of constant
             | Sub of constant
             | Mul of constant
             | Div of constant

type program = command list

type memory = constant list

(* Section two: constant parser *)
let natural : constant parser =
  fun ls ->
  match many1 digit ls with
  | Some (xs, ls) ->
    Some (Num(int_of_string (implode xs)), ls)
  | _ -> None

let num_parser : constant parser =
  let* ele = natural in
  pure (ele)

let nothing_parser : constant parser =
  let* _ = char '(' in
  let* _ = char ')' in
  pure (Nothing)

let true_parser : constant parser =
  let* _ = keyword "True" in
  pure (Bool (true))

let false_parser : constant parser =
  let* _ = keyword "False" in
  pure (Bool(false))

let rec constant_parser () = 
  num_parser
  <|>
  nothing_parser
  <|>
  true_parser
  <|>
  false_parser

(* command parser *)
let push_parser () : command parser =
  let* _ = ws in
  let* _ = keyword "Push" in
  let* ele = constant_parser () in
  pure (Push ele)

let pop_parser () : command parser =
  let* _ = ws in
  let* _ = keyword "Pop" in
  let* ele = constant_parser () in
  pure (Pop ele)

let trace_parser () : command parser = 
  let* _ = ws in
  let* _ = keyword "Trace" in
  let* ele = constant_parser () in
  pure (Trace ele)

let add_parser () : command parser =
  let* _ = ws in
  let* _ = keyword "Add" in
  let* ele = constant_parser () in
  pure (Add ele)

let sub_parser () : command parser = 
  let* _ = ws in
  let* _ = keyword "Sub" in
  let* ele = constant_parser () in
  pure (Sub ele)

let mul_parser () : command parser =
  let* _ = ws in
  let* _ = keyword "Mul" in
  let* ele = constant_parser () in
  pure (Mul ele)

let div_parser () : command parser = 
  let* _ = ws in
  let* _ = keyword "Div" in
  let* ele = constant_parser () in
  pure (Div ele)

let rec command_parser () =
  push_parser ()
  <|>
  pop_parser ()
  <|>
  trace_parser ()
  <|>
  add_parser ()
  <|>
  sub_parser ()
  <|>
  mul_parser ()
  <|>
  div_parser ()


let program_parser () =
  many (command_parser ())

let parse_code = parse(ws >> program_parser())

(* evaluation helper function *)

let rec count_length = fun to_count ->
  match to_count with
  | [] -> 0
  | h::t -> 1 + count_length t

let rec pop_element = fun to_pop number ->
  if number == 0 then to_pop else
  match to_pop with
  | [] -> []
  | h::t -> pop_element t (number - 1)

let rec trace_element = fun trace_from trace_to number ->
  if number == 0 then (trace_from, trace_to) else
  match trace_from with
  | [] -> ([], [])
  | h::t -> match h with
            | Num ele -> trace_element t (string_of_int ele :: trace_to) (number - 1)
            | Bool ele -> trace_element t (string_of_bool ele :: trace_to) (number - 1)
            | Nothing -> trace_element t ("()" :: trace_to) (number - 1)

let rec fetch_for_operation = fun memory_stack result_stack number ->
    if number == 0 then (memory_stack, result_stack) else
    match memory_stack with
    | h::t -> fetch_for_operation t (h::result_stack) (number - 1)
    | _ -> ([], [])

let rec only_number = fun to_check ->
  match to_check with
  | [] -> true
  | h::t -> match h with
            | Num _ -> only_number t
            | _ -> false

let rec add_numbers = fun to_add ->
  match to_add with
  | [] -> 0
  | h :: t -> match h with
              | Num ele -> ele + (add_numbers t)
              | _ -> 0

(* evaluation function *)

let rec evaluation = fun (call_stack: program) (mem : memory) (log : string list) ->
  match call_stack with
  | [] -> (mem, log)
  | current_command :: rest_of_command -> match current_command with
                                          | Push content -> evaluation rest_of_command (content :: mem) log
                                          | Pop content -> (let memory_length = count_length mem in
                                                            match content with
                                                            | Num ele -> if memory_length < ele then ([], ["Error"]) 
                                                                          else evaluation rest_of_command (pop_element mem ele) log
                                                            | _ -> ([], ["Error"])
                                                            )
                                          | Trace content -> (let memory_length = count_length mem in
                                                              match content with
                                                              | Num ele -> (if memory_length < ele then ([], ["Error"])
                                                                            else match trace_element mem log ele with
                                                                            | mem_after, log_after -> evaluation rest_of_command mem_after log_after
                                                                            )
                                                              | _ -> ([], ["Error"])
                                                              )
                                          | Add content -> (let memory_length = count_length mem in
                                                            match content with
                                                            | Num 0 -> evaluation rest_of_command (Num(0) :: mem) log
                                                            | Num ele -> (if memory_length < ele then ([], ["Error"])
                                                                        else match fetch_for_operation mem [] ele with 
                                                                        | result_memory, operation_stack -> (if only_number operation_stack then 
                                                                                                              let add_result = add_numbers operation_stack in
                                                                                                              evaluation rest_of_command (Num(add_result)::result_memory) log
                                                                                                            else ([], ["Error"]))
                                                                        | _ -> ([], ["Error"])
                                                                        )
                                                            | _ -> ([], ["Error"])
                                                            )
                                          | _ -> ([], [])

let execute_program = fun src ->
  let parse_result = parse_code src in
  match parse_result with
  | None -> ([], ["Error"])
  | Some(command_list, errors) -> match errors with
                                  | [] -> evaluation command_list [] []
                                  | _ -> ([], ["Error"])