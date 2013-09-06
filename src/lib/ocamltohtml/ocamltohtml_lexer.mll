(* OCaml to HTML ************************************************** *
 * (c) 2007, Philippe Wang **************************************** *
 * Licence: any version of GNU GPL or CeCILL-B, choose one that fits
 * best your needs and legislation. ******************************* *)

{
  (* header *)

exception Eof

let debug = false

let oc = ref stdout
let ic = ref stdin

let char_count = ref 0

let line_break = "\n"

let print_newline () =
  char_count := 0;
  output_char !oc '\n'

let print_char c =
  output_char !oc c

let print s =
  char_count := !char_count + String.length s;
  output_string !oc s

let print_string = print

let print_endline s =
  print s ;
  print_newline()

let comments = ref 0

type env = | NIL | KWD1 | KWD2 | KWD | OP | STR | MODULE | COMMENTS

let toclose = Stack.create ()
let toopen = Stack.create ()

let open_env e =
  match !comments with 
    | 0 ->
        print_string
          begin
            Stack.push e toclose;
            match e with
              | NIL -> "<span class='nil'>"
              | KWD1 -> "<span class='k'>"
              | KWD2 -> "<span class='o'>"
              | KWD -> "<span class='k'>"
              | STR -> "<span class='s'>"
              | MODULE -> "<span class='m'>"
              | OP -> "<span class='o'>"
              | COMMENTS -> "<span class='c'>"
          end
    | 1 ->
        if e = COMMENTS then 
          (Stack.push e toclose ; print_string "<span class='com2'>")
        else 
          (Stack.push e toclose ; print_string "<span class='ic'>" )
    | _ ->
        Stack.push e toclose ; print_string "<span class='other'>"

and close_env e =
  match !comments with 
    | 0 ->
        begin
          (try ignore(Stack.pop toclose)
            with _ -> if debug then prerr_endline "**************");
          match e with
            | COMMENTS -> print_string "</span><!-- end comment -->"
            | _ -> print_string "</span>" 
        end
    | _ -> 
        (try ignore(Stack.pop toclose)
          with _ -> if debug then prerr_endline "**************");
        match e with
          | COMMENTS ->  print_string "</span>"
          | _ -> print_string "</span>"


let open_envs () = 
  assert(Stack.is_empty toclose);
  Stack.iter (if debug then prerr_endline "---OPEN---"; fun e -> open_env e)  toopen ;
  Stack.clear toopen

let close_envs () = 
  assert(Stack.is_empty toopen);
  Stack.iter
    (if debug then prerr_endline "---CLOSE---";
     fun e -> 
       Stack.push e toopen; close_env e)
    toclose;
  Stack.clear toclose



let newline =
  let x = ref 1 in
    fun () ->
      incr x ;
      close_envs ();
      Printf.fprintf !oc "\n%!";
      open_envs ()

      

let html_escape s =
  let l = String.length s in
  let rec esc i =
    if i = l then ()
    else
      match s.[i] with
        | '&' | '<' | '>' | '\'' | '"' as c ->
            Printf.fprintf !oc "&#%d;" (int_of_char c);
            esc (succ i)
        | c -> 
            print_char c; esc (succ i)
  in esc 0

(* /header *)}

let lowercase = ['a'-'z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9''?']*

let uppercase = ['A'-'Z']['a'-'z' 'A'-'Z' '_' '0'-'9''?']*

rule token = parse
  | ("(" "*"+ ")") as lxm
      { incr comments ; 
        open_env COMMENTS;
        print_string  lxm;
        decr comments;
        close_env COMMENTS
      }
  | ("(" "*"+ ) as lxm
      { incr comments ; 
        open_env COMMENTS;
        print_string  lxm }
  | ("*"+ ")") as lxm
      { decr comments ;
        print_string lxm;
        close_env COMMENTS }
  | " " | "\t" as c { html_escape (String.make 1 c)}
  | '\n'  {newline() }  
  | ('-'? (['0'-'9']+ )
    | ('0' ('X'|'x') ['0'-'9']+) 
    | ('0' ('o'|'O') ['0'-'7']+)
    | ('0' ('B'|'b') ['0'-'1']+))
  | (('-' ? (['0'-'9']+ '.' ['0'-'9']* ((('e'|'E')['0'-'9']+)?) )
    | (['0'-'9']+((('e'|'E')['0'-'9']+)?) )) 
    | ('-' ? (['0'-'9']+ '.'))) as lxm
      {  print_string lxm }

  | (("let"|"and") ( ([' ' '\t' '\n']* ) "rec")? 
        as lxm)
      (lowercase as name)
        { 
          html_escape lxm;
          open_env KWD2; (* plop *)
          html_escape name;
          close_env KWD2;
        }

  | ((("let"|"and"|"module") ( ([' ' '\t' '\n']* ) "rec")?  )
    | "in" | "include" | "open" | "rec"  | "exception" | "external"
    | "method"  | "sig" | "struct"
    | "type" | "val" | "virtual"| "private"
    | "mutable" | "new" | "open" | "end" | "begin"
    | "class" | "do" | "done" | "downto" | "else" | "fun" | "function"
    | "functor" | "if" | "inherit" | "initializer"
    | "try"
    | "when" | "while" | "with"
    ) as lxm
      {open_env KWD1 ;
       html_escape lxm ;
       close_env KWD1}

  | ("true"|"false") as lxm 
      {open_env KWD2 ; 
       html_escape lxm ; 
       close_env KWD2}

  | ( "as" | "assert" | "asr" | "constraint" 
     | "false"
     | "for" | "lazy" | "match"    | "then" |  "of" | "to" 
     | "land" | "lor" | "lsl" | "lsr" | "lxor"
     | "mod"| "or" 
     | "true" | "=" | "->" )
      as lxm { open_env KWD ;
               print_string lxm;
               close_env KWD }

  (* chars *)
  | (( '\'' (_ | "\\b" |"\\n" |"\\r" |"\\t" | "\\'" | "\\\"") '\'' )
    |( '\'' (("\\" ['0'-'2']['0'-'9']['0'-'9']) | ("\\" ['X''x']['0'-'9''a'-'f''A'-'F']['0'-'9''a'-'f''A'-'F'])) '\'' )) as c 
      { if debug then prerr_endline "$$$$$$$$$$$$$ --------- CHAR";
        open_env STR ;
        html_escape c;
        close_env STR ;
      }

  (* strings *)
  | ('"' (( [^'"'] | "\\\"" | "\\b" |"\\n" |"\\r" |"\\t" )* ) '"') as lxm
      { open_env STR ;
        html_escape lxm;
        close_env STR}

  (* operators *)
  | (['#' ';' '!' '?' ',' ]+ | ['[' ']' '}' '{' '<' '>'] )
      as lxm
      {open_env OP;
       (* print_string "$\\mathtt{";  *)
       html_escape lxm;
       (* print_string "}$"; *)
       close_env OP }

  (* operators -- no parenthesis *)
  | ['=' '<' '>' '@' '^' ';' '&' '+' '-' '*'
	'%' '!' '?' '~' '$' '%' '_'
	'*' '+' '-' '.' '/' ':' '?' '^' '|' 
	'\\' ]+ as lxm
      {open_env OP;
       (* print_string "$\\mathtt{"; *)
       html_escape lxm;
       (* print_string "}$"; *)
       close_env OP }

  (* operators -- start with parenthesis *)
  | '(' ['=' '<' '>' '@' '^' ';' '&' '+' '-' '*'
	'%' '!' '?' '~' '$' '%' '_'
	'*' '+' '-' '.' '/' ':' '?' '^' '|' 
	'\\' ]+ as lxm
      {open_env OP;
       (* print_string "$\\mathtt{"; *)
       html_escape lxm;
       (* print_string "}$"; *)
       close_env OP }

  (* operators -- end with parenthesis *)
  | ['=' '<' '>' '@' '^' ';' '&' '+' '-' '*'
	'%' '!' '?' '~' '$' '%' '_'
	'*' '+' '-' '.' '/' ':' '?' '^' '|' 
	'\\' ]+ ')' as lxm
      {open_env OP;
       (* print_string "$\\mathtt{"; *)
       html_escape lxm;
       (* print_string "}$"; *)
       close_env OP }

	
  (* modules prefixes *)
  | (['A'-'Z']['a'-'z' 'A'-'Z' '_' '0'-'9''?']* as lxm)
      ([' ' '\t' '\n']* as spaces) '.'
      {open_env MODULE;
       html_escape lxm;
       close_env MODULE;
       html_escape spaces;
       open_env OP;
       html_escape ".";
       close_env OP }

  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9''?']* as lxm 
      {print_string lxm}

  | (lowercase|uppercase) as lxm 
      { html_escape lxm }


  | eof { close_envs () ; raise Eof }
  |  _ as c
      { 
	print_char c;
	if debug then prerr_endline ("unrecognized character <" ^ Char.escaped c ^ ">") }

