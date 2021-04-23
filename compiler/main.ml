open Printf
open Instruction
open Bytecode
open Typing

let emit b =
  "# Begin Aver Bytecode\n" ^ String.concat "\n" (List.map as_string b)

let filename = ref ""

let output = ref ""

let version () =
  Printf.printf "averc 0.1\n";
  exit 0

let usage =
  Printf.sprintf "usage: %s [--version] [--output file] filename" Sys.argv.(0)

let speclist =
  [
    ("--output", Arg.Set_string output, ": write output in file <file>");
    ("--version", Arg.Unit version, ": display version and exit");
  ]

let parse_cmdline =
  Arg.parse speclist (fun x -> filename := x) usage;
  try
    if !filename = "" then
      raise
        (Arg.Bad
           "Missing argument: no input file name given. Please supply a valid \
            aver file (.av)")
  with Arg.Bad msg ->
    Printf.eprintf "%s: %s\n" Sys.argv.(0) msg;
    Printf.eprintf "%s\n" usage;
    exit 1

let parse_file f =
  let input = open_in f in
  let filebuf = Lexing.from_channel input in
  Printexc.record_backtrace true;
  try Ok (emit (gen_bytecode (type_program (Parse.parse filebuf))))
  with e ->
    Printexc.print_backtrace stdout;
    Error (Printexc.to_string e)

let write_file s out =
  let oc = open_out_bin out in
  let b = Bytes.of_string s in
  output_bytes oc b;
  close_out oc

let () =
  parse_cmdline;
  if Filename.extension !filename = ".av" then (
    let bc = parse_file !filename in
    match bc with
    | Ok x ->
        write_file x
          ( if !output = "" then Filename.remove_extension !filename ^ ".avb"
          else !output )
    | Error e ->
        eprintf "%s\n" e;
        exit (-1) )
  else eprintf "Unable to open non aver file\n"
