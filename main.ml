open Lexing

let ifile = ref ""
let ofile = ref ""
let show_tokens = ref false
let show_tokens_with_loc = ref false
let show_ast = ref false
let show_ast_with_loc = ref false
let stop_at_typechecking = ref false
let stop_at_parsing = ref false
let tgc_path = ref "tgc"
let cc = ref "cc"

let set_file f s = f := s

let usage = "usage: mini-java file.java"
let options =
  ["--show-tokens", Arg.Set show_tokens,
     " print all tokens and stop";
   "--show-tokens-with-loc", Arg.Set show_tokens_with_loc,
     " print all tokens with location informations and stop";
   "--show-ast", Arg.Set show_ast,
     " show abstract syntax tree and stop";
   "--show-ast-with-loc", Arg.Set show_ast_with_loc,
     " show abstract syntax tree with location informations and stop";
   "--stop-at-parsing", Arg.Set stop_at_parsing,
     " parse the program and stop";
   "--stop-at-typechecking", Arg.Set stop_at_typechecking,
     " typecheck the program and stop";
   "--tgc-path", Arg.Set_string tgc_path,
     " path to tgc; default is ./tgc";
   "--c-compiler", Arg.Set_string cc,
     " c compiler; default is cc"
  ]

let () =
  Arg.parse options (set_file ifile) usage;

  if !ifile = "" then
    begin
      Printf.fprintf stderr "no input file\n";
      Arg.usage options usage;
      exit 1
    end;

  if not (Filename.check_suffix !ifile ".java") then
    begin
      Printf.fprintf stderr "filename must have .java suffix.\n";
      Arg.usage options usage;
      exit 1
    end;

  ofile := (Filename.chop_extension !ifile) ^ ".c";

  let input = open_in !ifile in

  let lexbuf = Lexing.from_channel input in

  (** [lexbuf.Lexing.lex_curr_p] describes the current position in the source file.
      [pos_fname] is the file name.
      [pos_lnum] is the line number.
      [pos_bol] is the offset of the beginning of the line (number of characters between the beginning of the file and the beginning of the line).
      [pos_cnum] is the offset of the position (number of characters between the beginning of the file and the position).
      [pos_cnum - pos_bol + 1] is the character number within the line. *)
  lexbuf.Lexing.lex_curr_p <-
    {
      Lexing.pos_fname = !ifile;
      Lexing.pos_lnum  = 1; (** The first line number is 1 and not 0. *)
      Lexing.pos_bol   = 0;
      Lexing.pos_cnum  = 0
    };
  try
    if !show_tokens || !show_tokens_with_loc then
      begin
        Print_tokens.print stdout lexbuf !show_tokens_with_loc;
        close_in input;
        exit 0;
      end;

    (** [program] is the abstract syntax tree with position informations. *)
    let program = Parser.program Lexer.get_token lexbuf in
    close_in input;

    if !show_ast || !show_ast_with_loc then
      begin
        Print_ast.print stdout program !show_ast_with_loc;
        exit 0;
      end;
    (** [!stop_at_parsing] is used by our testing script [test.sh] to only test
        the lexical and syntactical phases. *)
    if !stop_at_parsing then exit 0;
    Typechecking.typecheck_program program;
    (** [!stop_at_typechecking] is used by our testing script [test.sh] to only test
        the typechecker. *)
    if !stop_at_typechecking then exit 0;
    (** [translate_program] is used to get rid of the position informations.
        We don't need them after the typechecking phase. *)
    let mj = Lmj2mj.translate_program program in
    let output = open_out !ofile in
    Printf.fprintf output "/*\n";
    PrintMJ.print_program output mj;
    Printf.fprintf output "*/\n";
    Mj2c.program2c output mj;
    close_out output;
    match
      Unix.system(Printf.sprintf "%s %s -o %s -I%s %s/tgc.o"
                    !cc !ofile (Filename.chop_extension !ifile) !tgc_path !tgc_path)
    with
    | Unix.WEXITED code -> exit code
    | _ -> exit 1
  with
    | Lexer.Error msg ->
        Printf.fprintf stderr "Lexical error %s:\n%s.\n" (Error.position (Lexing.lexeme_start_p lexbuf)) msg;
        exit 1
    | Parser.Error ->
        Printf.fprintf stderr "Syntax error %s.\n" (Error.position (Lexing.lexeme_start_p lexbuf));
        exit 1
    | Typechecking.Error msg ->
        Printf.fprintf stderr "Type checking error %s.\n" msg;
        exit 1
