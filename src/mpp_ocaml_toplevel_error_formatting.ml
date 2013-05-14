(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)

let open_underline  = "\x1b\x5b\x34\x6d" 
let close_underline = "\x1b\x5b\x32\x34\x6d"

include Mpp_charstream

let _x() = print_endline (open_underline ^  "plop"  ^ close_underline)

let convert_underline charstream : string =
  let b = Buffer.create 100 in
  let underlining = Buffer.create 100 in
  let uf = ref false in
  let rec loop() =
    match charstream.take() with
      | None ->
          if !uf then
            begin
              Buffer.add_char b '\n';
              Buffer.add_buffer b underlining;
            end;
          Buffer.contents b
      | Some ('\x1b' as c) ->
          charstream.push c;
          if charstream_peek ~n:3 charstream = "\x1b\x5b\x41" then
            begin
              ignore(charstream_take_n 3 charstream);
              loop()
            end
          else
            begin
              try_underlined()
            end
      | Some '\n' ->
          Buffer.add_char b '\n';
          if !uf then
            begin
              uf := false;
              Buffer.add_buffer b underlining;
              Buffer.add_char b '\n';
            end;
          if Buffer.length underlining > 250 then
            Buffer.reset underlining
          else
            Buffer.clear underlining;
          loop()
      | Some c ->
          Buffer.add_char b c;
          Buffer.add_char underlining ' ';
          loop()

  and try_underlined() =
    let success = ref false in
    let si = charstream_peek ~n:(String.length open_underline) charstream in
      if si = open_underline
      then
        begin
          if "" <> read_until_word ~failsafe:true charstream open_underline
          then assert false;
          match
            read_until_word
              ~failsafe:true ~success:success
              charstream close_underline
          with
            | "" -> ()
            | s ->
                begin
                  Buffer.add_string b s;
                  for i = 1 to String.length s do
                    Buffer.add_char underlining '^'
                  done;
                  uf := true
                end
        end
      else
        begin
          (*           Printf.eprintf "Ignoring<%s>\n%!" (String.escaped si); *)
          Buffer.add_string b si;
          for i = 1 to String.length si do ignore(charstream.take()) done
        end;
      loop()

  in
    ignore(charstream_take_n 31 charstream);
    eat Mpp_init.space_chars charstream;
    loop()

let _ =
  print_endline (
    convert_underline (charstream_of_inchannel "stdin" stdin)
  )
