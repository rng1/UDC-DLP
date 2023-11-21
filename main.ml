open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

let read_cmd () =
  let rec in_loop acc =
    try
      let line = read_line () in
      if String.ends_with ~suffix:";;" line then 
        String.concat " " (List.rev (String.sub line 0 (String.length line - 2) :: acc))
      else 
        in_loop (line :: acc)
    with End_of_file ->
      String.concat " " (List.rev acc)
    in in_loop []

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    print_string ">> ";
    flush stdout;
    try
      let tm = s token (from_string (read_cmd ())) in
      let tyTm = typeof ctx tm in
      print_endline (string_of_term (eval tm) ^ " : " ^ string_of_ty tyTm);
      loop ctx
    with
       Lexical_error ->
         print_endline "lexical error";
         loop ctx
     | Parse_error ->
         print_endline "syntax error";
         loop ctx
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop ctx
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop emptyctx
  ;;

top_level_loop ()
;;
