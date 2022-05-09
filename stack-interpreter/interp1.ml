let is_lower_case c = 'a' <= c && c <= 'z'

let is_upper_case c = 'A' <= c && c <= 'Z'

let is_alpha c = is_lower_case c || is_upper_case c

let is_digit c = '0' <= c && c <= '9'

let is_alphanum c = is_lower_case c || is_upper_case c || is_digit c

let is_blank c = String.contains " \012\n\r\t" c

let explode s = List.of_seq (String.to_seq s)

let implode ls = String.of_seq (List.to_seq ls)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ loop ()
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

(* end of util functions *)

(* parser combinators *)

type 'a parser = char list -> ('a * char list) option

let parse (p : 'a parser) (s : string) : ('a * char list) option = p (explode s)

let pure (x : 'a) : 'a parser = fun ls -> Some (x, ls)

let fail : 'a parser = fun ls -> None

let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> q a ls
  | None -> None

let ( >>= ) = bind

let ( let* ) = bind

let read : char parser =
  (*return first char of input list*)
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls)
  | _ -> None

let satisfy (f : char -> bool) : char parser =
  (*if function f when applied to x is true, then parsing is succesful and return char x*)
  fun ls ->
  match ls with
  | x :: ls ->
    if f x then
      Some (x, ls)
    else
      None
  | _ -> None

let char (c : char) : char parser = satisfy (fun x -> x = c)
(*check if first element of list is equal to char c*)

let abcParser = 
  let* x = char 'a' in 
  let* y = char 'b' in
  let* z = char 'c' in 
  pure (implode [x;y;x])

let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None

let ( >> ) = seq

let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) -> (
      match p2 ls with
      | Some (_, ls) -> Some (x, ls)
      | None -> None)
  | None -> None

let ( << ) = seq'

let alt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) -> Some (x, ls)
  | None -> p2 ls

let ( <|> ) = alt

let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> Some (f a, ls)
  | None -> None

let ( >|= ) = map

let ( >| ) p c = map p (fun _ -> c)

let rec many (p : 'a parser) : 'a list parser =
  fun ls ->
  match p ls with
  | Some (x, ls) -> (
      match many p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> Some ([], ls)

let rec many1 (p : 'a parser) : 'a list parser =
  fun ls ->
  match p ls with
  | Some (x, ls) -> (
      match many p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> None

let rec many' (p : unit -> 'a parser) : 'a list parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) -> (
      match many' p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> Some ([], ls)

let rec many1' (p : unit -> 'a parser) : 'a list parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) -> (
      match many' p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> None

let whitespace : unit parser =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c then
      Some ((), ls)
    else
      None
  | _ -> None

let ws : unit parser = many whitespace >| ()

let ws1 : unit parser = many1 whitespace >| ()

let digit : char parser = satisfy is_digit

let natural : int parser =
  fun ls ->
  match many1 digit ls with
  | Some (xs, ls) -> Some (int_of_string (implode xs), ls)
  | _ -> None

let literal (s : string) : unit parser =
  fun ls ->
  let cs = explode s in
  let rec loop cs ls =
    match (cs, ls) with
    | [], _ -> Some ((), ls)
    | c :: cs, x :: xs ->
      if x = c then
        loop cs xs
      else
        None
    | _ -> None
  in
  loop cs ls

let keyword (s : string) : unit parser = literal s >> ws >| ()

type constant = 
  | Num of int
  | Bool of bool
  | U
  | Name of string
  | Fun of constant * program * env

and command = 
  | Push of constant
  | Pop of constant
  | Trace of constant
  | Add of constant
  | Sub of constant
  | Mul of constant
  | Div of constant
  | And 
  | Or
  | Not 
  | Equal
  | Lte 
  | Local 
  | Global 
  | Lookup 
  | Bgend of command list
  | Condition of (command list) * (command list)
  | Func of constant * constant * program
  | Call
  | TryE of program
  | Case of constant * program
  | Switch of command list

and program = command list
and memory = constant list
and env = (constant * constant) list

(* Section two: constant parser *)

let is_initial i =
  is_alphanum i || i = '_' || i = '\''

let initial : char parser = satisfy is_initial

let is_name : constant parser = 
  fun ls -> 
  match many1 initial ls with 
  |Some (xs, ls) -> Some (Name (implode xs), ls)
  |_ -> None 

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
  pure (U)

let true_parser : constant parser =
  let* _ = keyword "True" in
  pure (Bool (true))

let false_parser : constant parser =
  let* _ = keyword "False" in
  pure (Bool(false))

let name_parser : constant parser = 
  let* ele = is_name in 
  pure (ele)

let rec constant_parser () = 
  num_parser
  <|>
  nothing_parser
  <|>
  true_parser
  <|>
  false_parser
  <|>
  name_parser

(* command parser *)
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
  let* ele = constant_parser () in
  pure (Add ele)

and sub_parser () : command parser = 
  let* _ = ws in
  let* _ = keyword "Sub" in
  let* ele = constant_parser () in
  pure (Sub ele)

and mul_parser () : command parser =
  let* _ = ws in
  let* _ = keyword "Mul" in
  let* ele = constant_parser () in
  pure (Mul ele)

and div_parser () : command parser = 
  let* _ = ws in
  let* _ = keyword "Div" in
  let* ele = constant_parser () in
  pure (Div ele)

and and_parser () : command parser = 
  let*_ = ws in 
  let*_ = keyword "And" in 
  pure (And)

and or_parser () : command parser = 
  let*_ = ws in 
  let*_ = keyword "Or" in 
  pure (Or)

and not_parser () : command parser = 
  let*_ = ws in 
  let*_ = keyword "Not" in 
  pure (Not)

and equal_parser () : command parser = 
  let*_ = ws in 
  let*_ = keyword "Equal" in 
  pure (Equal)

and lte_parser () : command parser = 
  let*_ = ws in 
  let*_ = keyword "Lte" in 
  pure (Lte)

and global_parser () : command parser = 
  let*_ = ws in 
  let*_ = keyword "Global" in 
  pure (Global)

and local_parser () : command parser = 
  let*_ = ws in 
  let*_ = keyword "Local" in 
  pure (Local)

and call_parser () : command parser =
  let* _ = ws in
  let* _ = keyword "Call" in
  pure (Call)

and trace_parser () : command parser = 
  let*_ = ws in 
  let*_ = keyword "Trace" in 
  let*_ = ws in
  let* ele = constant_parser () in
  pure (Trace ele)

and lookup_parser () : command parser = 
  let*_ = ws in 
  let*_ = keyword "Lookup" in 
  pure (Lookup)

and conditional_parser () : command parser = 
  let*_ = ws in 
  let*_ = keyword "If" in 
  let*_ = ws in 
  let* ele1 = program_parser () in 
  let* _ = ws in 
  let*_ = keyword "Else" in 
  let*_ = ws in 
  let* ele2 = program_parser () in 
  let*_ = ws in 
  let*_ = keyword "End" in 
  pure (Condition(ele1,ele2))

and bgend_parser () : command parser = 
  let*_ = ws in 
  let*_ = keyword "Begin" in 
  let*_ = ws in 
  let* ele = program_parser () in 
  let*_ = ws in 
  let*_ = keyword "End" in 
  pure (Bgend ele)

and function_parser () : command parser = 
  let* _ = ws in
  let* _ = keyword "Fun" in
  let* _ = ws in
  let* fName = name_parser in
  let* _ = ws in
  let* fArg = name_parser in
  let* _ = ws in
  let* fBody = program_parser () in
  let* _ = ws in
  let* _ = keyword "End" in
  pure (Func (fName, fArg, fBody))

and try_parser () : command parser =
  let* _ = ws in
  let* _ = keyword "Try" in
  let* _ = ws in
  let* ele = program_parser () in
  let* _ = ws in
  let* _ = keyword "End" in
  pure (TryE (ele))

and case_parser () : command parser =
  let* _ = ws in
  let* _ = keyword "Case" in
  let* _ = ws in
  let* case_number = constant_parser () in
  let* _ = ws in
  let* case_command = program_parser () in
  pure (Case (case_number, case_command))

and switch_parser () : command parser =
  let* _ = ws in
  let* _ = keyword "Switch" in
  let* _ = ws in
  let* cases = many(case_parser ()) in
  let* _ = ws in
  let* _ = keyword "End" in
  pure (Switch cases)

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
  bgend_parser ()
  <|>
  function_parser ()
  <|>
  call_parser ()
  <|>
  try_parser ()
  <|>
  switch_parser ()

and program_parser () = 
  many (command_parser ())

let parse_code = (parse (ws >> program_parser()))

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

let rec pop_helper = fun stack value acc -> 
  if value == 0 then Some ((stack,acc)) else 
    match stack with 
    |[] -> None 
    |h::t -> (match h with 
        |Num _ -> pop_helper t (value - 1) (acc @ [h])
        |_ -> None )

let rec trace_element = fun trace_from trace_to number ->
  if number == 0 then (trace_from, trace_to) else
    match trace_from with
    | [] -> ([], [])
    | h::t -> match h with
      | Num ele -> trace_element t ([string_of_int ele] @ trace_to) (number - 1)
      | Bool ele -> (match ele with
          | true -> trace_element t ("True" :: trace_to) (number - 1)
          | false -> trace_element t ("False" :: trace_to) (number - 1)
        )
      | U -> trace_element t ("()" :: trace_to) (number - 1)
      | Name ele -> trace_element t (ele :: trace_to) (number - 1)

let rec fetch_for_operation = fun memory_stack result_stack number ->
  if number == 0 then (memory_stack, result_stack) else
    match memory_stack with
    | h::t -> fetch_for_operation t (result_stack @ [h]) (number - 1)
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

let rec mul_numbers = fun to_mul ->
  match to_mul with
  | [] -> 1
  | h :: t -> match h with
    | Num ele -> ele * (mul_numbers t)
    | _ -> 0

let sub_number = fun to_sub ->
  match to_sub with
  | [] -> 0
  | h::t -> match h with
    | Num ele -> ele - (add_numbers t)
    | _ -> 0 

let div_number = fun to_div ->
  match to_div with
  | [] -> 1
  | h::t -> match h with
    | Num ele -> if mul_numbers t == 0 then -1 else ele / (mul_numbers t)
    | _ -> -1

let rec pop_helper = fun stack value acc -> 
  if value = 0 then Some(stack, acc) else 
    match stack with 
    |[] -> None 
    |hd::tl -> (match hd with 
        |Num _ -> pop_helper tl (value - 1) (acc @ [hd])
        |_ -> None)

let rec add_helper = fun stack -> 
  match stack with 
  |[] -> 0
  |hd :: tl -> match hd with 
    |Num i -> i + add_helper tl
    | _ -> 0

let rec sub_helper = fun stack -> 
  match stack with 
  |[] -> 0
  |hd :: tl -> match hd with 
    |Num i -> i - (add_helper tl)
    |_ -> 0

let rec mul_helper = fun stack -> 
  match stack with 
  |[] -> 1
  |hd::tl -> match hd with 
    |Num i -> mul_helper tl * i
    |_ -> 1

let rec trace_pop_helper = fun stack value acc -> 
  if value == 0 then Some (stack, acc) else 
    match stack with 
    | [] -> None 
    |h::t -> trace_pop_helper t (value - 1) (acc @ [h])

let rec trace_helper = fun stack ->
  match stack with
  | [] -> []
  | hd::tl -> match hd with
    | Num value -> trace_helper tl @ [(string_of_int value)]
    | Bool value -> (match value with
        | true -> (trace_helper tl) @ ["True"]
        | false -> (trace_helper tl) @ ["False"]
      )
    | U ->  (trace_helper tl) @ ["()"]
    | Name value -> (trace_helper tl) @ [value]


let rec lookup_helper = fun lookfor env -> 
  match env with 
  |[] -> None 
  |h::t -> match h with 
    | (Name v_name, v_value) -> if v_name = lookfor then Some(v_value) else lookup_helper lookfor t
    |_ -> None


let rec div_checker = fun stack -> 
  match stack with 
  |[] -> true
  |hd::tl -> match hd with 
    |Num 0 -> false
    |_ -> div_checker tl

let rec interp_helper = fun ls ->
    match ls with
    | [] -> true
    | h::t -> match h with
      | "Error" -> false
      | _ -> interp_helper t
  
let rec switch_lookup = fun look_for look_from ->
  match look_from with
  | [] -> None
  | h :: t -> (match h with
              | Case ( Num (case_number), case_command) -> if case_number = look_for then Some(case_command) else switch_lookup look_for t)
  

(* evaluation function *)

let rec evaluation = fun (call_stack: program) (local_disk : env) (global_disk : env) (mem : memory) (log : string list) ->
  match call_stack with
  | [] -> (call_stack, local_disk, global_disk, mem, log)
  | current_command :: rest_of_command -> match current_command with
    | Push content -> evaluation rest_of_command local_disk global_disk  (content :: mem) log
    | Pop content -> (let memory_length = count_length mem in
                      match content with
                      | Num ele -> if memory_length < ele then ([], local_disk, global_disk, mem, ["Error"]) 
                        else evaluation rest_of_command local_disk global_disk (pop_element mem ele) log
                      | _ -> (rest_of_command, local_disk, global_disk, mem, ["Error"])
                     )
    | Trace content -> (let memory_length = count_length mem in
                        match content with
                        | Num ele -> (if memory_length < ele then ([], local_disk, global_disk, mem, ["Error"])
                                      else match trace_element mem log ele with
                                        | mem_after, log_after -> evaluation rest_of_command local_disk global_disk mem_after log_after
                                     )
                        | _ -> (rest_of_command, local_disk, global_disk, mem, ["Error"])
                       )
    | Add content -> (let memory_length = count_length mem in
                      match content with
                      | Num 0 -> evaluation rest_of_command local_disk global_disk (Num(0) :: mem) log
                      | Num ele -> (if memory_length < ele then ([], local_disk, global_disk, mem, ["Error"])
                                    else match fetch_for_operation mem [] ele with 
                                      | result_memory, operation_stack -> (if only_number operation_stack then 
                                                                             let add_result = add_numbers operation_stack in
                                                                             evaluation rest_of_command local_disk global_disk (Num(add_result)::result_memory) log
                                                                           else ([], local_disk, global_disk, mem, ["Error"]))
                                   )
                      | _ -> (rest_of_command, local_disk, global_disk, mem, ["Error"])
                     )
    | Sub content -> (let memory_length = count_length mem in
                      match content with
                      | Num 0 -> evaluation rest_of_command local_disk global_disk (Num(0) :: mem) log
                      | Num ele -> (if memory_length < ele then ([], local_disk, global_disk, mem, ["Error"])
                                    else match fetch_for_operation mem [] ele with
                                      | result_memory, operation_stack -> (if only_number operation_stack then
                                                                             let sub_result = sub_number operation_stack in
                                                                             evaluation rest_of_command local_disk global_disk (Num(sub_result)::result_memory) log
                                                                           else ([], local_disk, global_disk, mem, ["Error"]))
                                   )
                      | _ -> (rest_of_command, local_disk, global_disk, mem, ["Error"]))
    | Mul content -> (let memory_length = count_length mem in
                      match content with
                      | Num 0 -> evaluation rest_of_command local_disk global_disk (Num(1) :: mem) log
                      | Num ele -> (if memory_length < ele then ([], local_disk, global_disk, mem, ["Error"])
                                    else match fetch_for_operation mem [] ele with 
                                      | result_memory, operation_stack -> (if only_number operation_stack then 
                                                                             let mul_result = mul_numbers operation_stack in
                                                                             evaluation rest_of_command local_disk global_disk (Num(mul_result)::result_memory) log
                                                                           else (rest_of_command, local_disk, global_disk, mem, ["Error"]))
                                   )
                      | _ -> (rest_of_command, local_disk, global_disk, mem, ["Error"]))
    | Div content -> (let memory_length = count_length mem in
                      match content with
                      | Num 0 -> evaluation rest_of_command local_disk global_disk (Num(1) :: mem) log
                      | Num ele -> (if memory_length < ele then (rest_of_command, local_disk, global_disk, mem, ["Error"])
                                    else match fetch_for_operation mem [] ele with 
                                      | result_memory, operation_stack -> (if only_number operation_stack then 
                                                                             let div_result = div_number operation_stack in
                                                                             if div_result == -1 then ([], local_disk, global_disk, mem, ["Error"]) else 
                                                                               evaluation rest_of_command local_disk global_disk (Num(div_result)::result_memory) log
                                                                           else (rest_of_command, local_disk, global_disk, mem, ["Error"]))
                                   )
                      | _ -> (rest_of_command, local_disk, global_disk, mem, ["Error"]))
    | And -> (match mem with
        | Bool ele1 :: Bool ele2 :: rest_stack -> evaluation rest_of_command local_disk global_disk (Bool (ele1 && ele2) :: rest_stack) log 
        | _ -> (rest_of_command, local_disk, global_disk, mem, ["Error"])
      )
    | Or -> (match mem with
        | Bool ele1 :: Bool ele2 :: rest_stack -> evaluation rest_of_command local_disk global_disk (Bool (ele1 || ele2) :: rest_stack) log 
        | _ -> (rest_of_command, local_disk, global_disk, mem, ["Error"])
      )
    | Equal -> (match mem with
        | Num ele1 :: Num ele2 :: rest_stack -> evaluation rest_of_command local_disk global_disk (Bool (ele1 == ele2) :: rest_stack) log
        | _ -> (rest_of_command, local_disk, global_disk, mem, ["Error"])
      )
    | Lte -> (match mem with
        | Num ele1 :: Num ele2 :: rest_stack -> evaluation rest_of_command local_disk global_disk (Bool (ele1 <= ele2) :: rest_stack) log 
        | _ -> (rest_of_command, local_disk, global_disk, mem, ["Error"])
      )
    | Local -> (match mem with
        | Name ele1 :: ele2 :: rest_stack -> evaluation rest_of_command ((Name ele1, ele2)::local_disk) global_disk (U :: rest_stack) log 
        | _ -> (rest_of_command, local_disk, global_disk, mem, ["Error"]))
    | Global -> (match mem with
        | Name ele1 :: ele2 :: rest_stack -> evaluation rest_of_command local_disk ((Name ele1, ele2) :: global_disk) (U :: rest_stack) log 
        | _ -> (rest_of_command, local_disk, global_disk, mem, ["Error"]))
    | Lookup -> (match mem with
        | Name ele :: rest_stack -> (let value = lookup_helper ele local_disk in
                                     match value with
                                     | Some(found) -> evaluation rest_of_command local_disk global_disk (found :: rest_stack) log 
                                     | None -> ( let value_2 = lookup_helper ele global_disk in
                                                 match value_2 with
                                                 | Some(found) -> evaluation rest_of_command local_disk global_disk (found :: rest_stack) log 
                                                 | None -> (rest_of_command, local_disk, global_disk, mem, ["Error"])
                                               )
                                    )
        | _ -> (rest_of_command, local_disk, global_disk, mem, ["Error"]))
    | Bgend ele -> ( let result = evaluation ele local_disk global_disk [] log in
                     match result with
                     | _, _, _, _, ["Error"] -> (rest_of_command, local_disk, global_disk, mem, ["Error"])
                     | _, _, inner_global_disk, inner_stack_h::inner_stack_t, inner_log -> evaluation rest_of_command local_disk (inner_global_disk @ global_disk) (inner_stack_h :: mem) inner_log
                     | _, _, inner_global_disk, [], inner_log -> (rest_of_command, local_disk, global_disk, mem, ["Error"])

                   )
    | Condition (if_true, if_false) -> (match mem with
        | Bool true :: rest_stack -> (let result = evaluation if_true local_disk global_disk rest_stack log  in
                                      match result with
                                      | if_stack, if_local_disk, if_global_disk, if_mem, if_log -> evaluation rest_of_command (if_local_disk @ local_disk) (if_global_disk @ global_disk) if_mem if_log )
        | Bool false :: rest_stack -> (let result = evaluation if_false local_disk global_disk rest_stack log  in
                                       match result with
                                       | if_stack, if_local_disk, if_global_disk, if_mem, if_log -> evaluation rest_of_command (if_local_disk @ local_disk) (if_global_disk @ global_disk) if_mem if_log )
        | _ ->  (rest_of_command, local_disk, global_disk, mem, ["Error"])
      ) 
    | Not -> (match mem with
        | Bool ele :: rest_stack -> evaluation rest_of_command local_disk global_disk (Bool (not ele) :: rest_stack) log 
        | _ -> (rest_of_command, local_disk, global_disk, mem, ["Error"])
      )
    | Func (fName, fArg, fBody) -> evaluation rest_of_command ((fName, Fun(fArg, fBody, local_disk ))::local_disk) global_disk mem log
    | Call -> (match mem with
        | Fun (fArgName, fCommand, fEnv) :: fArgValue :: rest_stack -> (let fun_result = evaluation fCommand ((fArgName, fArgValue) :: local_disk @ fEnv) global_disk [] log in
                                                                        match fun_result with
                                                                        |  res_call_stack, res_local_disk, res_global_disk, res_mem, res_log -> (match res_mem with
                                                                                                                                                | [] -> (rest_of_command, local_disk, global_disk, mem, ["Error"])
                                                                                                                                                | res_mem_h :: res_mem_t -> (evaluation rest_of_command local_disk res_global_disk (res_mem_h :: rest_stack) res_log))
                                                                        | _ -> (rest_of_command, local_disk, global_disk, mem, ["Error"])
                                                                        )
        | fArgValue :: Fun (fArgName, fCommand, fEnv) :: rest_stack -> (let fun_result = evaluation fCommand ((fArgName, fArgValue) :: fEnv) global_disk [] log in
                                                                        match fun_result with
                                                                        |  res_call_stack, res_local_disk, res_global_disk, res_mem, res_log -> (match res_mem with
                                                                                                                                                | [] -> (rest_of_command, local_disk, global_disk, mem, ["Error"])
                                                                                                                                                | res_mem_h :: res_mem_t -> evaluation rest_of_command local_disk res_global_disk (res_mem_h :: rest_stack) res_log)
                                                                        | _ -> (rest_of_command, local_disk, global_disk, mem, ["Error"])
                                                                        )
        | _ -> (rest_of_command, local_disk, global_disk, mem, ["Error"]))
    | TryE ele -> (let tryEResult = evaluation ele local_disk global_disk [] log in
                  match tryEResult with
                  | res_stack, res_local, res_global, res_mem, res_log -> (if interp_helper res_log then (match res_mem with
                                                                                                          |res_mem_h :: res_mem_t -> evaluation rest_of_command local_disk res_global (res_mem_h :: mem) res_log
                                                                                                          | _ -> ([], [], [], [], ["Error"]))
                                                                          else evaluation rest_of_command local_disk res_global mem log
                                                                          )
                  )
    | Switch ele -> (match mem with
                    | Num match_for :: rest_mem -> (let match_command = switch_lookup match_for ele in
                                        match match_command with
                                        | None -> (rest_of_command, local_disk, global_disk, mem, ["Error"])
                                        | Some(matched_command) -> (let eval_res = evaluation matched_command local_disk global_disk rest_mem log in
                                                                    match eval_res with
                                                                    | res_call_stack, res_local, res_global, res_mem, res_log -> evaluation rest_of_command res_local res_global res_mem res_log)
                                        )
                    | _ -> (rest_of_command, local_disk, global_disk, mem, ["Error"])
                    )

let execute_program = fun src ->
  let parse_result = parse_code src in
  match parse_result with
  | None -> ([], [], [], [], ["Error"])
  | Some(x,y) -> evaluation x [] [] [] []


let debug = fun src -> 
match execute_program src with
| (res_call_stack, res_local_disk, res_global_disk, res_mem, res_log) -> res_global_disk
  


(* TODO *)
let rec interp (src : string) : string list = 
  let interp_result = execute_program src in
  match interp_result with
  | (x,_,_,_,y) -> (let no_error = interp_helper y in
                    if no_error then y else ["Error"])

(* Calling (main "test.txt") will read the file test.txt and run interp on it.
   This is only used for debugging and will not be used by the gradescope autograder. *)
let main fname =
  let src = readlines fname in
  interp src


