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
let () = Mpp_variables.debug := debug
let () = Mpp_actions.debug := debug

let ignore_orphan_closing_tokens = ref false

module Out = Mpp_out

let rec preprocess : charstream -> Out.t -> unit = fun (charstream:charstream) out ->
  let default_buffer = Buffer.create 4242 in
    
  (* entry point *)
  let rec loop (): unit =
    begin
      if match_token !open_token charstream then
        open_token_action ~nesting:false

      else if match_token !open_nesting_token charstream then
        open_token_action ~nesting:true

      else if match_token !endline_comments_token charstream then
        endline_comments_token_action()

      else if match_token !open_comments_token charstream then
        open_comments_token_action()

      else if match_token !open_special_token charstream then
        open_special_token_action()

      else if match_token !close_special_token charstream then
        close_special_token_action()

      else if match_token !close_token charstream then
        close_token_action()

      else if match_token !close_comments_token charstream then
        close_comments_token_action()

      else
        default(charstream.take())
    end

  and flush_default() =
    Out.output_buffer out default_buffer;
    Buffer.clear default_buffer;
    Out.flush out

  (* default action *)
  and default = function
    | None -> ()
    | Some c ->
        Buffer.add_string default_buffer (!Mpp_init.special.char_escape c);
        loop()

  and open_special_token_action() =
    flush_default();
    let x = read_until_word charstream (!close_special_token) in
      Out.output_string out x;
      loop()

  (* new block *)
  and open_token_action ~nesting =
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
    let charstream = () in let _ = charstream in (* ~> to prevent its use afterwards *)
    let blockcharstream =
      (* the contents of the block is converted into a charstream *)
      let l_bc = charstream_of_string ~location:(block_start_location) block_contents in
        if nesting then
          begin
            let buff = Buffer.create 42 in
              preprocess l_bc (Out.Buffer buff);
              charstream_of_string ~location:(block_start_location) (Buffer.contents buff)
          end
        else
          l_bc
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
    let action_arguments : string = (* action arguments *)
      match blockcharstream.take() with
        | Some c ->
            blockcharstream.push c;
            read_until_one_of ~failsafe:true newline_chars blockcharstream
        | None ->
            ""
    in
      Mpp_actions.exec action_name action_arguments blockcharstream out;
      loop ()

  (* Closing a block that hasn't been opened is wrong. *)
  and close_token_action() =
    if not (!ignore_orphan_closing_tokens) then
      begin
        parse_error ~msg:"Closing unopened action block." (charstream.where());
        exit 1
      end

  and close_special_token_action() = 
    if not (!ignore_orphan_closing_tokens) then
      begin
        parse_error ~msg:"Closing unopened special block." (charstream.where());
        exit 1
      end

  (* Just ignore what has to be ignored. *)
  and endline_comments_token_action() =
    let _l = read_until_one_of newline_chars charstream in
      if debug then Printf.eprintf  "comments: <%s>\n%!" _l;
      loop()

  (* New comment block. *)
  and open_comments_token_action() =
    flush_default();
    let _c = read_until_word charstream (!close_comments_token) in
      if debug then Printf.eprintf  "comments: <%s>\n%!" _c;
      loop()

  (* Closing a comment block that hasn't been opened is wrong. *)
  and close_comments_token_action() =
    if not (!ignore_orphan_closing_tokens) then
      begin
        parse_error ~msg:"Closing unopened comments block." (charstream.where());
        exit 1
      end
  in 
    loop();
    flush_default()


let init() =
  begin
    (* This is here because the input builtin needs to access the
       preprocess function.  *)
    let builtin__input =
      (fun arg cs out ->
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
        "setopen", (fun x _cs _out -> open_token := x), "Sets the opening token. Related: setclose.";
        "setclose", (fun x _cs _out -> close_token := x), "Sets the closing token. Related: setopen.";
(*         "setopen", (fun x _cs _out -> open_token := x), "Sets the opening token. Related: setclose."; *)
(*         "setclose", (fun x _cs _out -> close_token := x), "Sets the closing token. Related: setopen."; *)
        "setendlinecomments", (fun x _cs _out -> endline_comments_token := x), "Sets the endline comments token.";
        "setopencomments", (fun x _cs _out -> open_comments_token := x), "Sets the opening comments token. Related: setclosecomments.";
        "setclosecomments", (fun x _cs _out -> close_comments_token := x), "Sets the endline comments token. Related: setopencomments.";
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
  let defaultoutput = ref "" in
  let at_least_one_file_processed = ref false in
  let process_one_file filename =
    if not(Sys.file_exists filename) then
      begin
        if !continue then
          ()
        else
          Printf.eprintf "Error: file <%s> does not exist, I will stop. You might want to use -continue.\n%!"
            filename
      end
    else
      if
        try Filename.chop_extension filename ^ ".mpp" = filename
        with Invalid_argument _ -> false
      then
        begin
          let outputfilename =
            if !defaultoutput = "" then
              Filename.chop_extension filename 
            else
              !defaultoutput
          in
            if outputfilename <> "/dev/stdout" && Sys.file_exists outputfilename && not !overwrite then
              begin
                Printf.eprintf "Warning: file <%s> already exists, I won't overwrite it. You might want to use -overwrite.\n%!"
                  outputfilename
              end
            else
              begin
                let out = Out.Out_channel(open_out_gen [Open_wronly;Open_creat;Open_trunc;Open_binary] 0o640 outputfilename) in
                  preprocess (charstream_of_inchannel filename (open_in filename)) out;
                  at_least_one_file_processed := true
              end
        end
      else
        begin
          Printf.eprintf "Warning: filename <%s> does not have .mpp extension. So I'll output on stdout.\n%!" filename;
          preprocess (charstream_of_inchannel filename (open_in filename)) (Out.Out_channel stdout);
          at_least_one_file_processed := true
        end
  in
    try
      if l > 1 then
        begin
          let aligned =
            Arg.align [
              "-o", Arg.Set_string(defaultoutput), "filename Output to filename instead of standard option.";
(*               "-overwrite", Arg.Set(overwrite), " Overwrite existing destination files."; *)
              "-w", Arg.Set(overwrite), " Overwrite existing destination files.";
(*               "-continue", Arg.Set(continue), " Continue even if an input file doesn't exist."; *)
              "-c", Arg.Set(continue), " Continue even if an input file doesn't exist.";
              "-ine", Arg.Set(Mpp_actions.ignore_non_existing_commands), " Ignore non existing commands instead of stopping.";
              "-iee", Arg.Set(Mpp_actions.ignore_exec_error), " Ignore errors that occur when executing external commands.";
              "-ioc", Arg.Set(ignore_orphan_closing_tokens), " Ignore orphan closing tokens.";
              "-b", Arg.Unit(Mpp_actions.list_builtins), " List builtins.";
              "-so", Arg.Set_string(open_token), Printf.sprintf "token Set open token. Default is %s." !open_token;
              "-sc", Arg.Set_string(close_token), Printf.sprintf "token Set close token. Default is %s." !close_token;
              "-son", Arg.Set_string(open_nesting_token), Printf.sprintf "token Set open token for blocks which allow nesting. Default is %s." !open_nesting_token;
              "-scn", Arg.Set_string(close_nesting_token), Printf.sprintf "token Set close token for blocks which allow nesting. Default is %s." !close_nesting_token;
              "-sos", Arg.Set_string(open_special_token), Printf.sprintf "token Set open special token. Default is %s." !open_special_token;
              "-scs", Arg.Set_string(close_special_token), Printf.sprintf "token Set close special token. Default is %s." !close_special_token;
              "-soc", Arg.Set_string(open_comments_token), Printf.sprintf "token Set open comments token. Default is %s." !open_comments_token;
              "-scc", Arg.Set_string(close_comments_token), Printf.sprintf "token Set close comments token. Default is %s." !close_comments_token;
              "-sec", Arg.Set_string(endline_comments_token), Printf.sprintf "token Set endline comments token. Default is %s."  !endline_comments_token;
              "-set", Arg.String(fun s ->
                                   let cs = charstream_of_string s in 
                                   let vn = read_until_one_of (Mpp_charset.of_list ['='; ' ';'\t']) cs in
                                   let _ = cs.take() in
                                     Mpp_variables.Variable.set (vn ^ " " ^ string_of_charstream cs) (charstream_of_string "") stdout),
              "x=s Sets variable x to s (if you know how, you can use a space instead of =).";
              "-lang", Arg.String(Mpp_init.set_special), "lang Set MPP to convert the file into a lang file.";
              "-ll", Arg.Unit(Mpp_init.list_specials), " List available special languages. Advanced use: to add one, cf. the file mpp_init.ml";
              "--", Arg.Rest(process_one_file), " If you use this parameter, all remaining arguments are considered as file names.";
            ]
          in
            Arg.parse
              aligned
              process_one_file
              ("Usage: " ^ Sys.argv.(0) ^ " [-options] [filename1.ext.mpp ... filenameN.ext.mpp]
~ If a file name doesn't have the .mpp extension, it will output on stdout.
~ If you don't give any file name, it will use standard input (/dev/stdin).
~ If a token becomes empty, it removes the associated feature (remember to empty closing tokens if you empty opening ones).
~ This software does not care about characters encoding, hence it performs no conversion at all.
~ When an options exists to enable a feature, it means that it is disabled by default.
~ Please feel free to email pw374@cl.cam.ac.uk if you find any bug.

List of options:")
        end;
      if not !at_least_one_file_processed then
        begin
          preprocess (charstream_of_inchannel "/dev/stdin" stdin) (Out.Out_channel stdout)
        end;
    with e ->
      let bt = Printexc.get_backtrace () in
      if debug then Printf.eprintf "%s\n%!" bt;
      if debug then Printf.eprintf "Exception raised: <%s>\n%!" (Printexc.to_string e);
      Pervasives.exit 1







