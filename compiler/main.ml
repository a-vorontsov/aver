open Printf
open Instruction
open Bytecode
open Typing

let emit b = String.concat "\n" (List.map as_string b)

let parse_file f =
  let input = open_in f in
  let filebuf = Lexing.from_channel input in
  Printexc.record_backtrace true;
  try
    Ok
      (emit
         (gen_bytecode
            (type_program
               (Parse.parse filebuf
                  (Parser.Incremental.prog filebuf.lex_curr_p)))))
  with
  | Lexer.Error msg -> Error (sprintf "%s!" msg)
  | Parser.Error ->
      Error
        (sprintf "Syntax error at offset %d:\n%!" (Lexing.lexeme_start filebuf))
  | Util.Syntax_error (location, msg) -> (
      match location with
      | Some (line, pos) ->
          Error (sprintf "Syntax error at %d:%d\n%s" line pos msg)
      | None -> Error (sprintf "%s" msg) )
  | e ->
      Printexc.print_backtrace stdout;
      Error (Printexc.to_string e)

let write_file s out =
  let oc = open_out_bin (out ^ ".avb") in
  let b = Bytes.of_string s in
  output_bytes oc b;
  close_out oc

let () =
  if Array.length Sys.argv == 2 then (
    let file = Sys.argv.(1) in
    ignore print_newline;
    let bc = parse_file file in
    match bc with
    | Ok x -> write_file x (Filename.remove_extension file)
    | Error e ->
        eprintf "%s" e;
        exit (-1) )
