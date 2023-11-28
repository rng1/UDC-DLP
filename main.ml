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
  let rec loop (vctx, tctx) =
    print_string ">> ";
    flush stdout;
    try
      let tm = s token (from_string (read_cmd ())) in
      loop (execute (vctx, tctx) tm)
    with
       Lexical_error ->
         print_endline "lexical error";
         loop (vctx, tctx)
     | Parse_error ->
         print_endline "syntax error";
         loop (vctx, tctx)
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop (vctx, tctx)
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop (emptyctx, emptyctx)
  ;;

top_level_loop ()
;;
