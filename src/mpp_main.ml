(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)

open Mpp_charstream
open Mpp_init

let debug =
  try ignore(Sys.getenv "DEBUG") ; true with _ -> false

let () = Mpp_charstream.debug := debug
let () = Mpp_actions.debug := debug

let ignore_orphan_closing_tokens = ref false
let save_newlines = ref false
let ignore_trailing_spaces = ref false

module Out = Mpp_out

let rec preprocess : charstream -> Out.t -> unit = fun (charstream:charstream) out ->
  let default_buffer = Buffer.create 4242 in
    
  (* entry point *)
  let rec loop (last_cond:bool option ref) : unit =
    begin
      if match_token !open_token charstream then
        open_token_action last_cond ~nesting:false

      else if match_token !open_nesting_token charstream then
        open_token_action last_cond ~nesting:true

      else if match_token !endline_comments_token charstream then
        endline_comments_token_action()

      else if match_token !open_comments_token charstream then
        open_comments_token_action()

      else if match_token !open_foreign_token charstream then
        open_foreign_token_action()

      else if match_token !close_foreign_token charstream then
        close_foreign_token_action()

      else if match_token !close_token charstream then
        close_token_action()

      else if match_token !close_comments_token charstream then
        close_comments_token_action()

      else
        default last_cond (charstream.take())
    end;
    match charstream.take() with
      | None -> ()
      | Some c -> charstream.push c;  loop last_cond

  and flush_default() =
    let r = !Mpp_init.foreign.print (Buffer.contents default_buffer) in
      Out.output_string out r;
      Buffer.clear default_buffer;
      Out.flush out

  (* default action *)
  and default last_cond = function
    | None -> ()
    | Some c ->
        Buffer.add_string default_buffer
          (!Mpp_init.foreign.string_escape (String.make 1 c));
        loop last_cond

  and open_foreign_token_action last_cond =
    flush_default();
    let block_name = (* block_name: syntactic "tool" *)
      match charstream.take() with
        | None ->
            parse_error
              ~msg:"No characters left to read right after an opening! (F2)" 
              (charstream.where());
            exit 1
        | Some (' ' | '\t') ->
            ""
        | Some c ->
            charstream.push c;
            let () =
              if debug then Printf.eprintf "peek<%s>\n%!"
                (String.escaped (charstream_peek ~n:20 charstream))
            in
              (read_until ~failsafe:true ' ' charstream)
    in
    let x = read_until_word charstream (block_name ^ !close_foreign_token) in
    if !save_newlines then
      begin match charstream.take() with
        | Some '\n' -> ()
        | Some c -> charstream.push c
        | None -> ()
      end;
    Out.output_string out x

  (* new block *)
  and open_token_action last_cond ~nesting =
    flush_default();
    let () =
      if debug then 
        Printf.eprintf "peek<%s>\n%!"
          (String.escaped (charstream_peek ~n:20 charstream))
    in
    let block_name = (* block_name: syntactic "tool" *)
      match charstream.take() with
        | None ->
            parse_error
              ~msg:"No characters left to read right after an opening! (2)" 
              (charstream.where());
            exit 1
        | Some (' ' | '\t') ->
            None
        | Some c ->
            charstream.push c;
            let () =
              if debug then Printf.eprintf "peek<%s>\n%!"
                (String.escaped (charstream_peek ~n:20 charstream))
            in
              Some (read_until ~failsafe:true ' ' charstream)
    in
    let () = eat space_chars charstream in
    let block_start_location = charstream.where() in
    let block_contents =
      (* the contents of the block *)
      match block_name with
        | Some name ->
            if debug then Printf.eprintf "name=<%s>%!" name;
            read_until_word charstream (name ^ (if nesting then !close_nesting_token else !close_token))
        | None ->
            read_until_word charstream (if nesting then !close_nesting_token else !close_token)
    in
    let () = 
      if !save_newlines then 
        begin match charstream.take() with
          | Some '\n' -> ()
          | Some c -> charstream.push c
          | None -> ()
        end
    in
    let block_contents =
      if !ignore_trailing_spaces then
        delete_trailing_spaces block_contents
      else
        block_contents
    in
    let charstream = () in let _ = charstream in (* ~> to prevent its use afterwards *)
    let blockcharstream =
      (* the contents of the block is converted into a charstream *)
      charstream_of_string ~location:(block_start_location) block_contents
    in
    let action_name : string = (* name of the action *)
      eat space_chars blockcharstream;
      read_until_one_of
        ~failsafe:true
        space_chars
        (* ~exclude:newline_chars *)
        ~expect:"Zero or more spaces, and then an action name."
        blockcharstream
    in
    let action_arguments : charstream = (* action arguments *)
      match blockcharstream.take() with
        | Some c ->
            blockcharstream.push c;
            let tmp = read_until_one_of ~failsafe:true ~push_back:true newline_chars blockcharstream in
              charstream_of_string 
                (if !ignore_trailing_spaces then
                  delete_trailing_spaces tmp
                  else
                    tmp)
        | None ->
            charstream_of_string ""
    in
      Mpp_actions.exec nesting last_cond action_name action_arguments blockcharstream out

  (* Closing a block that hasn't been opened is wrong. *)
  and close_token_action() =
    if not (!ignore_orphan_closing_tokens) then
      begin
        parse_error ~msg:"Closing unopened action block." (charstream.where());
        exit 1
      end

  and close_foreign_token_action() = 
    if not (!ignore_orphan_closing_tokens) then
      begin
        parse_error ~msg:"Closing unopened foreign block." (charstream.where());
        exit 1
      end

  (* Just ignore what has to be ignored. *)
  and endline_comments_token_action() =
    let _l = read_until_one_of newline_chars charstream in
      if debug then Printf.eprintf  "comments: <%s>\n%!" _l

  (* New comment block. *)
  and open_comments_token_action() =
    flush_default();
    let _c = read_until_word charstream (!close_comments_token) in
      if debug then Printf.eprintf  "comments: <%s>\n%!" _c

  (* Closing a comment block that hasn't been opened is wrong. *)
  and close_comments_token_action() =
    if not (!ignore_orphan_closing_tokens) then
      begin
        parse_error ~msg:"Closing unopened comments block." (charstream.where());
        exit 1
      end
  in 
    loop (ref None);
    flush_default()


let init() =
  Mpp_actions.main_process := preprocess;
  begin
    (* This is here because the input builtin needs to access the
       preprocess function.  *)
    let builtin__input =
      (fun __last_cond _nesting arg cs out ->
        let arg = string_of_charstream arg in
        let x = open_in arg in
          cs.insert (charstream_of_inchannel arg x);
          preprocess cs out;
          close_in x
      )
    in
      Mpp_actions.register "input" builtin__input "Input and process a file.";
  end;
  begin
    List.iter
      (fun (name, action, documentation) -> Mpp_actions.register name action documentation)
      [
        "so", (fun _lc _n x _cs _out -> open_token := string_of_charstream x), "Sets the opening token. Related: sc.";
        "sc", (fun _lc _n x _cs _out -> close_token := string_of_charstream x), "Sets the closing token. Related: so.";
        "sso", (fun _lc _n x _cs _out -> open_foreign_token := string_of_charstream x), "Sets the foreign opening token. Related: ssc.";
        "ssc", (fun _lc _n x _cs _out -> close_foreign_token := string_of_charstream x), "Sets the foreign closing token. Related: sso.";
        "sec", (fun _lc _n x _cs _out -> endline_comments_token := string_of_charstream x), "Sets the endline comments token.";
        "soc", (fun _lc _n x _cs _out -> open_comments_token := string_of_charstream x), "Sets the opening comments token. Related: scc.";
        "scc", (fun _lc _n x _cs _out -> close_comments_token := string_of_charstream x), "Sets the endline comments token. Related: soc.";
      ]
  end


(**************************************************)
(*****************INITIALISATION*******************)
(**************************************************)
let _ =
  let () = init() in
  let l = Array.length Sys.argv in
  let overwrite = ref false in
  let continue = ref false in
  let common_output = ref None in
  let common_output_filename = ref "" in
  let at_least_one_file_processed = ref false in
  let files_to_process = ref [] in
  let process_one_file filename =
    if not(Sys.file_exists filename) then
      begin
        if not !continue then
          (Printf.eprintf "Error: input file <%s> does not exist, I'm stopping right here.\n%!" filename;
           exit 4)
      end
    else (* filename does exist, so it's fine. *)
    if !common_output_filename <> ""
    then
      (begin
        match !common_output with
        | None ->
          let out =
            Out.Out_channel
              (open_out_gen
                 [Open_wronly;Open_creat;Open_trunc;Open_binary]
                 0o640
                 !common_output_filename)
          in
          common_output := Some out;
          preprocess (charstream_of_inchannel filename (open_in filename)) out;
          at_least_one_file_processed := true
        | Some out ->
          preprocess (charstream_of_inchannel filename (open_in filename)) out;
          at_least_one_file_processed := true
      end)
    else
      begin
        preprocess (charstream_of_inchannel filename (open_in filename)) (Out.Out_channel stdout);
        at_least_one_file_processed := true
      end
  in
  let register_file f = files_to_process := f :: !files_to_process in
  try
    if l > 1 then
      begin
        let aligned =
          Arg.align [
            "-o", Arg.Set_string(common_output_filename), "f Output to the file f instead of standard option.";
            "-w", Arg.Set(overwrite), " @deprecated [Overwrite existing destination files]. Now MPP always overwrites.";
            "-c", Arg.Set(continue), " Continue even if an input file doesn't exist.";
            "-ine", Arg.Set(Mpp_actions.ignore_non_existing_commands), " Ignore non existing commands instead of stopping.";
            "-iee", Arg.Set(Mpp_actions.ignore_exec_error), " Ignore errors that occur when executing external commands.";
            "-ioc", Arg.Set(ignore_orphan_closing_tokens), " Ignore orphan closing tokens.";
            "-its", Arg.Set(ignore_trailing_spaces), " Ignore trailing spaces (i.e. spaces at end of block and end of command line).";
            "-b", Arg.Unit(fun () -> Mpp_actions.list_builtins (Out.Out_channel stdout); exit 0), " List builtins.";
            "-so", Arg.Set_string(open_token), Printf.sprintf "token Set open token. Default is %s." !open_token;
            "-sc", Arg.Set_string(close_token), Printf.sprintf "token Set close token. Default is %s." !close_token;
            "-son", Arg.Set_string(open_nesting_token), Printf.sprintf "token Set open token for blocks which allow nesting. Default is %s." !open_nesting_token;
            "-scn", Arg.Set_string(close_nesting_token), Printf.sprintf "token Set close token for blocks which allow nesting. Default is %s." !close_nesting_token;
            "-sos", Arg.Set_string(open_foreign_token), Printf.sprintf "token Set open foreign token. Default is %s." !open_foreign_token;
            "-scs", Arg.Set_string(close_foreign_token), Printf.sprintf "token Set close foreign token. Default is %s." !close_foreign_token;
            "-soc", Arg.Set_string(open_comments_token), Printf.sprintf "token Set open comments token. Default is %s." !open_comments_token;
            "-scc", Arg.Set_string(close_comments_token), Printf.sprintf "token Set close comments token. Default is %s." !close_comments_token;
            "-sec", Arg.Set_string(endline_comments_token), Printf.sprintf "token Set endline comments token. Default is %s."  !endline_comments_token;
            "-set", Arg.String(fun s ->
                let cs = charstream_of_string s in 
                let vn = read_until_one_of ~failsafe:true (Mpp_charset.of_list ['='; ' ';'\t']) cs in
                Mpp_actions.Variable.set (charstream_of_string (vn ^ " " ^ string_of_charstream cs)) (charstream_of_string "") stdout),
            "x=s Sets variable x to s (if you know how, you can use a space instead of =).";
            "-l", Arg.String(Mpp_init.set_foreign), "lang Set MPP to convert the file into a lang file. (Does not work yet.)";
            "-ll", Arg.Unit(Mpp_init.list_foreign), " List available foreign languages. Advanced use: to add one, cf. the file mpp_init.ml";
            "-snl", Arg.Set(save_newlines), " Don't print newlines that follow closing blocks.";
            "--", Arg.Rest(register_file), " If you use this parameter, all remaining arguments are considered as file names.";
          ]
        in
        Arg.parse
          aligned
          register_file
          ("Usage: " ^ Sys.argv.(0) ^ " [-options] [filename1.ext.mpp ... filenameN.ext.mpp]
~ By default, MPP outputs on stdout. You can specify a file to output to, using the -o option (if you use it multiple times, only the last one is considered).
~ MPP reads /dev/stdin unless you give it one or several files to read.
~ If a token becomes empty, it removes the associated feature (remember to empty closing tokens if you empty opening ones).
~ MPP works on ASCII and strict supersets of ASCII such as UTF8, it performs no conversion.
~ When an option exists to enable a feature, it means that the feature is disabled by default.
~ MPP reads all its command-line arguments and interprets them sequentially before processing any file.
~ Please feel free to email pw374@cl.cam.ac.uk if you meet any bug.

List of options:");
        List.iter process_one_file (List.rev !files_to_process);
      end;
    if not !at_least_one_file_processed then
      begin
        (* preprocess (charstream_of_inchannel "/dev/stdin" stdin) (Out.Out_channel stdout) *)
        process_one_file "/dev/stdin"
      end;
  with e ->
    let bt = Printexc.get_backtrace () in
    if debug then Printf.eprintf "%s\n%!" bt;
    if debug then Printf.eprintf "Exception raised: <%s>\n%!" (Printexc.to_string e);
    Pervasives.exit 1

