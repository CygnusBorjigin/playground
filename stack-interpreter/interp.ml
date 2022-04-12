(* parsing util functions *)

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
  fun ls ->
    match ls with
    | x :: ls -> Some (x, ls)
    | _ -> None

let satisfy (f : char -> bool) : char parser =
  fun ls ->
    match ls with
    | x :: ls ->
        if f x then
          Some (x, ls)
        else
          None
    | _ -> None

let char (c : char) : char parser = satisfy (fun x -> x = c)

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

(* end of parser combinators *)

type const = 
  | B of bool
  | I of int
  | Unit

type comm = Push of const | Pop of const | Trace of const
          | Add of const | Sub of const | Mul of const | Div of const

type commands = comm list

type stack = const list
      
let parse_false : const parser = 
let* _ = ws in let* _ = keyword "False" in pure (B(false)) 

let parse_true : const parser = 
let* _ = ws in let* _ = keyword "True" in pure (B(true))

let parse_int : const parser =
let* x = natural in pure (I x)

let parse_unit : const parser =
let* _ = char '(' in let* _ = char ')' in pure(Unit)
 
let rec parse_const () = 
  parse_false <|> parse_true <|> parse_int <|> parse_unit

(* command parser *)
let parse_push () : comm parser = 
let* _ = ws in let* _ = keyword "Push" in let* x =  parse_const () in pure (Push x)

let parse_pop () : comm parser =
let* _ = ws in let* _ = keyword "Pop" in let* x = parse_const () in pure (Pop x)

let parse_trace () : comm parser = 
let* _ = ws in let* _ = keyword "Trace" in let* x = parse_const () in pure (Trace x)

let parse_add () : comm parser =
let* _ = ws in let* _ = keyword "Add" in let* x = parse_const () in pure (Add x)

let parse_sub () : comm parser = 
let* _ = ws in let* _ = keyword "Sub" in let* x = parse_const () in pure (Sub x)

let parse_mul () : comm parser =
let* _ = ws in let* _ = keyword "Mul" in let* x = parse_const () in pure (Mul x)

let parse_div () : comm parser = 
let* _ = ws in let* _ = keyword "Div" in let* x = parse_const () in pure (Div x)

let rec parse_comm () =
  parse_push() <|> parse_pop() <|> parse_trace() <|> 
  parse_add() <|> parse_sub() <|> parse_mul() <|> parse_div() 

let parse_commands () =
  many (parse_comm ())

let parse_total = parse(ws >> parse_commands()) 
  

let rec pop_helper = fun stack value acc ->
  if value == 0 then Some((stack, acc)) else 
  match stack with 
  | [] -> None
  | hd::tl -> (match hd with
              | I _ -> pop_helper tl (value - 1) (acc @ [hd])
              | _ -> None
              )

let rec add_helper = fun stack ->
  match stack with
  | [] -> 0
  | hd :: tl -> match hd with
              | I i -> i + add_helper tl
              | _ -> 0

let rec sub_helper = fun stack ->
  match stack with
  | [] -> 0
  | hd :: tl -> match hd with
                | I i -> i - (add_helper tl)
                | _ -> 0

let rec mul_helper = fun stack ->
  match stack with
  | [] -> 1
  | hd :: tl -> match hd with
                | I i -> mul_helper tl * i
                | _ -> 1

let rec div_checker = fun stack ->
  match stack with
  | [] -> true
  | hd::tl -> match hd with
              | I 0 -> false
              | _ -> div_checker tl

let div_helper = fun stack ->
  match stack with
  | [] -> 1
  | hd :: tl -> match hd with
    | I i -> i / (mul_helper tl)
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
              | I value -> trace_helper tl @ [(string_of_int value)]
              | B value -> (match value with
                          | true -> (trace_helper tl) @ ["True"]
                          | false -> (trace_helper tl) @ ["False"])
              | Unit ->  (trace_helper tl) @ ["()"]
(* evaluation function *)

let rec eval = fun (progs: commands) (stack : stack) (log : string list) ->
  match progs with
  | [] -> (stack, log)
  | hd::tl -> match hd with
    | Push value -> eval tl (value :: stack) log
    | Pop value -> (match value with
                                  | I i -> (let res = trace_pop_helper stack i [] in
                                            match res with
                                            | None -> ([], ["Error"])
                                            | Some(stack_res, pop_res) -> eval tl stack_res log
                                          )
                                  | _ -> ([], ["Error"])
                    )
    | Add value -> (match value with
                    | I 0 -> eval tl (I 0 :: stack) log
                    | I i -> (let res = pop_helper stack i [] in
                              match res with
                              | None -> ([], ["Error"])
                              | Some(stack_res, pop_res) -> (let add_res = add_helper pop_res in
                                                              eval tl (I add_res :: stack_res) log
                                                              )
                    )
                    | _ -> ([], ["Error"])
                    )
    | Sub value -> (match value with
        | I 0 -> eval tl (I 0 :: stack) log
        | I i -> (let res = pop_helper stack i [] in
                  match res with
                  | None -> ([], ["Error"])
                  | Some(stack_res, pop_res) -> (let sub_res = sub_helper pop_res in
                                                 eval tl (I sub_res :: stack_res) log
                                                )
                 )
        | _ -> ([], ["Error"])
      )
    | Mul value -> (match value with
        | I 0 -> eval tl (I 1 :: stack) log
        | I i -> (let res = pop_helper stack i [] in
                  match res with
                  | None -> ([], ["Error"])
                  | Some(stack_res, pop_res) -> (let mul_res = mul_helper pop_res in
                                                 eval tl (I mul_res :: stack_res) log
                                                )
                 )
        | _ -> ([], ["Error"])
      )
    | Div value -> (match value with
        | I 0 -> eval tl (I 1 :: stack) log
        | I i -> (let res = pop_helper stack i [] in
                  match res with
                  | None -> ([], ["Error"])
                  | Some(stack_res, pop_res) -> ( match pop_res with
                      | [] -> eval tl (I 0 :: stack_res) log
                      | h::t -> if div_checker t == false then ([], ["Error"]) else
                                let div_res = div_helper pop_res in 
                                eval tl (I div_res :: stack_res) log
                                )
                 )
        | _ -> ([], ["Error"])
      )
    
    | Trace value -> (match value with
                      | I i -> ( let res = trace_pop_helper stack i [] in
                                match res with
                                | None -> ([], ["Error"])
                                | Some(stack_res, pop_res) -> eval tl stack_res ((trace_helper pop_res) @ log)
                                )
                      | _ -> ([], ["Error"])
                      )

let compute = fun x ->
  let result = parse_total x in
  match result with
  | None -> ([], ["Error"])
  | Some(x, y) -> eval x [] []


(* TODO *)
let rec interp (src : string) : string list = 
  let result = compute src in
  match result with
  | (stack_final, log_final) -> log_final

(* Calling (main "test.txt") will read the file test.txt and run interp on it.
   This is only used for debugging and will not be used by the gradescope autograder. *)
let main fname =
  let src = readlines fname in
  interp src
