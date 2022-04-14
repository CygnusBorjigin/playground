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
              | Name of string 
              | Nothing
              | Bool of bool
              | Closure of constant * constant * program * env
              
and command = Push of constant 
             | Add of constant 
             | Trace of constant
             | Sub of constant
             | Mul of constant
             | Div of constant
             | Pop of constant
             | And
             | Or
             | Not
             | Equal
             | Lte
             | Local
             | Global
             | Condition of (command list) * (command list)
             | Lookup
             | Beginend of command list 

and program = command list

and stack = constant list

and env = (constant * constant) list 


(* Section two: set up the atomic parsers *)

let is_initial i =
  is_alphanum i || i = '_' || i = '\''

let initial : char parser = satisfy is_initial

let is_name : constant parser =
  fun ls -> 
  match many1 initial ls with
  | Some (xs, ls) ->
    Some (Name(implode xs), ls)
  | _ -> None

let natural : constant parser =
  fun ls ->
  match many1 digit ls with
  | Some (xs, ls) ->
    Some (Num(int_of_string (implode xs)), ls)
  | _ -> None

(* Section three: set up parser *)

let int_parser : constant parser = 
  let* ele = natural in
  pure (ele)

let nothing_parser : constant parser =
  let* _ = char '(' in
  let* _ = char ')' in
  pure (Nothing)

let name_parser : constant parser =
  let* ele = is_name in
  pure (ele)

let true_parser : constant parser =
  let* _ = keyword "True" in
  pure (Bool true)

let false_parser : constant parser =
  let* _ = keyword "False" in
  pure (Bool false)

let rec constant_parser () =
  int_parser
  <|>
  true_parser
  <|>
  false_parser
  <|>
  name_parser
  <|>
  nothing_parser

(* Command parsers *)

and push_parser () : command parser = 
  let* _ = ws in 
  let* _ = keyword "Push" in
  let* ele = constant_parser () in
  pure (Push ele)

and pop_parser () : command parser = 
  let* _ = ws in 
  let* _ = keyword "Pop" in
  let* ele = constant_parser () in
  pure (Pop ele)

and add_parser () : command parser =
  let* _ = ws in 
  let* _ = keyword "Add" in
  let* _ = ws in
  let* ele = constant_parser () in
  pure (Add ele)

and sub_parser () : command parser = 
  let* _ = ws in 
  let* _ = keyword "Sub" in
  let* _ = ws in
  let* ele = constant_parser () in
  pure (Sub ele)

and mul_parser () : command parser = 
  let* _ = ws in 
  let* _ = keyword "Mul" in
  let* _ = ws in
  let* ele = constant_parser () in
  pure (Mul ele)

and div_parser () : command parser =
  let* _ = ws in 
  let* _ = keyword "Div" in
  let* _ = ws in
  let* ele = constant_parser () in
  pure (Div ele)

and and_parser () : command parser =
  let* _ = ws in
  let* _ = keyword "And" in
  pure And

and or_parser () : command parser =
  let* _ = ws in
  let* _ = keyword "Or" in
  pure Or

and not_parser () : command parser =
  let* _ = ws in
  let* _ = keyword "Not" in
  pure Not

and equal_parser () : command parser =
  let* _ = ws in
  let* _ = keyword "Equal" in
  pure Equal

and lte_parser () : command parser =
  let* _ = ws in
  let* _  = keyword "Lte" in
  pure Lte

and global_parser () : command parser =
  let* _ = ws in
  let* _ = keyword "Global" in
  pure Global

and local_parser () : command parser =
  let* _ = ws in
  let* _ = keyword "Local" in
  pure Local

and trace_parser () : command parser = 
  let* _ = ws in 
  let* _ = keyword "Trace" in
  let* _ = ws in
  let* ele = constant_parser () in
  pure (Trace ele)

and lookup_parser () : command parser =
  let* _ = ws in
  let* _ = keyword "Lookup" in
  pure (Lookup)

and conditional_parser () : command parser = 
  let* _ = ws in 
  let* _ = keyword "If" in
  let* _ = ws in
  let* ele1 = program_parser () in
  let* _ = ws in 
  let* _ = keyword "Else" in
  let* _ = ws in
  let* ele2 = program_parser () in
  let* _ = ws in
  let* _ = keyword "End" in
  pure (Condition(ele1, ele2))

and beginend_parser () : command parser =
  let* _ = ws in
  let* _ = keyword "Begin" in
  let* _ = ws in
  let* ele = program_parser () in
  let* _ = ws in
  let* _ = keyword "End" in
  pure (Beginend ele)

and command_parser () = 
  push_parser ()
  <|>
  pop_parser ()
  <|>
  add_parser ()
  <|>
  sub_parser ()
  <|>
  mul_parser ()
  <|>
  div_parser ()
  <|>
  and_parser ()
  <|>
  or_parser ()
  <|>
  not_parser ()
  <|>
  equal_parser ()
  <|>
  lte_parser ()
  <|>
  local_parser ()
  <|>
  global_parser ()
  <|>
  trace_parser ()
  <|>
  lookup_parser ()
  <|>
  conditional_parser ()
  <|>
  beginend_parser ()

and program_parser () =
  many (command_parser ())

let parse_code = parse(ws >> program_parser ())

let rec pop_helper = fun stack value acc ->
  if value == 0 then Some((stack, acc)) else 
  match stack with 
  | [] -> None
  | hd::tl -> (match hd with
              | Num _ -> pop_helper tl (value - 1) (acc @ [hd])
              | _ -> None
              )

let rec add_helper = fun stack ->
  match stack with
  | [] -> 0
  | hd :: tl -> match hd with
              | Num i -> i + add_helper tl
              | _ -> 0

let rec sub_helper = fun stack ->
  match stack with
  | [] -> 0
  | hd :: tl -> match hd with
                | Num i -> i - (add_helper tl)
                | _ -> 0

let rec mul_helper = fun stack ->
  match stack with
  | [] -> 1
  | hd :: tl -> match hd with
                | Num i -> mul_helper tl * i
                | _ -> 1

let rec div_checker = fun stack ->
  match stack with
  | [] -> true
  | hd::tl -> match hd with
              | Num 0 -> false
              | _ -> div_checker tl

let div_helper = fun stack ->
  match stack with
  | [] -> 1
  | hd :: tl -> match hd with
    | Num i -> i / (mul_helper tl)
    | _ -> 1

let rec trace_pop_helper = fun stack value acc ->
    if value == 0 then Some(stack, acc) else
    match stack with
    | [] -> None
    | hd::tl -> trace_pop_helper tl (value - 1) (acc @ [hd])

let rec trace_helper = fun stack ->
  match stack with
  | [] -> []
  | hd::tl -> match hd with
              | Num value -> trace_helper tl @ [(string_of_int value)]
              | Bool value -> (match value with
                          | true -> (trace_helper tl) @ ["True"]
                          | false -> (trace_helper tl) @ ["False"])
              | Nothing ->  (trace_helper tl) @ ["()"]
              | Name value -> (trace_helper tl) @ [value]  

let rec lookup_helper = fun lookfor env ->
  match env with
  | [] -> None
  | h::t -> match h with
            | (Name v_name, v_value) -> if v_name = lookfor then Some(v_value) else lookup_helper lookfor t

(* evaluation function *)

let rec eval = fun (progs: program) (stack : stack) (log : string list) (local_disk: env) (global_disk: env) ->
  match progs with
  | [] -> (stack, log, local_disk, global_disk)
  | hd::tl -> match hd with
    | Push value -> eval tl (value :: stack) log local_disk global_disk
    | Pop value -> (match value with
                                  | Num i -> (let res = trace_pop_helper stack i [] in
                                            match res with
                                            | None -> ([], ["Pop_Error_1"], [], [])
                                            | Some(stack_res, pop_res) -> eval tl stack_res log local_disk global_disk
                                          )
                                  | _ -> ([], ["Pop_Error_2"], [], [])
                    )
    | Add value -> (match value with
                    | Num 0 -> eval tl (Num 0 :: stack) log local_disk global_disk
                    | Num i -> (let res = pop_helper stack i [] in
                              match res with
                              | None -> ([], ["Add_Error_1"], [], [])
                              | Some(stack_res, pop_res) -> (let add_res = add_helper pop_res in
                                                              eval tl (Num add_res :: stack_res) log local_disk global_disk
                                                              )
                    )
                    | _ -> ([], ["Add_Error_2"], [], [])
                    )
    | Sub value -> (match value with
        | Num 0 -> eval tl (Num 0 :: stack) log local_disk global_disk
        | Num i -> (let res = pop_helper stack i [] in
                  match res with
                  | None -> ([], ["Sub_Error_1"], [], [])
                  | Some(stack_res, pop_res) -> (let sub_res = sub_helper pop_res in
                                                 eval tl (Num sub_res :: stack_res) log local_disk global_disk
                                                )
                 )
        | _ -> ([], ["Sub_Error_1"], [], [])
      )
    | Mul value -> (match value with
        | Num 0 -> eval tl (Num 1 :: stack) log local_disk global_disk
        | Num i -> (let res = pop_helper stack i [] in
                  match res with
                  | None -> ([], ["Mul_Error_1"], [], [])
                  | Some(stack_res, pop_res) -> (let mul_res = mul_helper pop_res in
                                                 eval tl (Num mul_res :: stack_res) log local_disk global_disk
                                                )
                 )
        | _ -> ([], ["Mul_Error_2"], [], [])
      )
    | Div value -> (match value with
        | Num 0 -> eval tl (Num 1 :: stack) log local_disk global_disk
        | Num i -> (let res = pop_helper stack i [] in
                  match res with
                  | None -> ([], ["Div_Error_1"], [], [])
                  | Some(stack_res, pop_res) -> ( match pop_res with
                      | [] -> eval tl (Num 0 :: stack_res) log local_disk global_disk
                      | h::t -> if div_checker t == false then ([], ["Div_Error_2"], [], []) else
                                let div_res = div_helper pop_res in 
                                eval tl (Num div_res :: stack_res) log local_disk global_disk
                                )
                 )
        | _ -> ([], ["Div_Error_3"], [], [])
      )
    
    | Trace value -> (match value with
                      | Num i -> ( let res = trace_pop_helper stack i [] in
                                match res with
                                | None -> ([], ["Trace_Error_1"], [], [])
                                | Some(stack_res, pop_res) -> eval tl stack_res ((trace_helper pop_res) @ log) local_disk global_disk
                                )
                      | _ -> ([], ["Trace_Error_2"], [], [])
                      )
    | And -> (match stack with
              | Bool ele1 :: Bool ele2 :: rest_stack -> eval tl (Bool (ele1 && ele2) :: rest_stack) log local_disk global_disk
              | _ -> ([], ["And_Error"], [], [])
              )
    | Or -> (match stack with
              | Bool ele1 :: Bool ele2 :: rest_stack -> eval tl (Bool (ele1 || ele2) :: rest_stack) log local_disk global_disk
              | _ -> ([], ["Or_Error"], [], [])
              )
    | Equal -> (match stack with
              | Num ele1 :: Num ele2 :: rest_stack -> eval tl (Bool (ele1 == ele2) :: rest_stack) log local_disk global_disk
              | _ -> ([], ["Equal_Error"], [], [])
              )
    | Lte -> (match stack with
              | Num ele1 :: Num ele2 :: rest_stack -> eval tl (Bool (ele1 <= ele2) :: rest_stack) log local_disk global_disk
              | _ -> ([], ["Lte_Error"], [], [])
              )
    | Local -> (match stack with
              | Name ele1 :: ele2 :: rest_stack -> eval tl (Nothing :: rest_stack) log ((Name ele1, ele2)::local_disk) global_disk
              | _ -> ([], ["Local_Error"], [], []))
    | Global -> (match stack with
              | Name ele1 :: ele2 :: rest_stack -> eval tl (Nothing :: rest_stack) log local_disk ((Name ele1, ele2) :: global_disk)
              | _ -> ([], ["Error"], [], []))
    | Lookup -> (match stack with
              | Name ele :: rest_stack -> (let value = lookup_helper ele local_disk in
                                            match value with
                                            | Some(found) -> eval tl (found :: rest_stack) log local_disk global_disk
                                            | None -> ( let value_2 = lookup_helper ele global_disk in
                                                        match value_2 with
                                                        | Some(found) -> eval tl (found :: rest_stack) log local_disk global_disk
                                                        | None -> (rest_stack, ["Loopup_Error_1"], local_disk, global_disk)
                                                      )
                                            )
              | _ -> ([], ["Lookup_Error_2"], [], []))
    | Beginend ele -> ( let result = eval ele [] [] local_disk global_disk in
                        match result with
                        | inner_stack_h::inner_stack_t, _, _, inner_global_disk -> eval tl (inner_stack_h :: stack) log local_disk (inner_global_disk @ global_disk)
                        | _, _, _, _ -> ([], ["Beginend_Error"], [], [])

                      )
    (* | Condition (if_true, if_false) -> (match stack with
                                        | Bool true -> ()
                                        | Bool false -> ()
                                        | _ ->                                         
                                        ) *)
    | Not -> (match stack with
              | Bool ele :: rest_stack -> eval tl (Bool (not ele) :: rest_stack) log local_disk global_disk
              | _ -> ([], ["Not_Error"], [], [])
    )
    | _ -> ([], ["Incomplete"], [], [])

let compute = fun x ->
  let result = parse_code x in
  match result with
  | None -> ([], ["Error"], [], [])
  | Some(x, y) -> eval x [] [] [] []


(* TODO *)
let rec interp (src : string) : string list = 
  let result = compute src in
  match result with
  | (stack_final, log_final, _, _) -> log_final

(* Calling (main "test.txt") will read the file test.txt and run interp on it.
   This is only used for debugging and will not be used by the gradescope autograder. *)
let main fname =
  let src = readlines fname in
  interp src 
