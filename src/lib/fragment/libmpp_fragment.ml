(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)

exception Next

let fragment fl fc fr tl tc tr cl cc filename (out:Mpp_out.t) =
  let open Mpp_charstream in
    (*   let open Mpp_out in *)
  let byte_count = ref 0 in
  let module Input = struct
    let cs = match filename with
      | None -> charstream_of_inchannel "/dev/stdin" stdin
      | Some f -> charstream_of_inchannel f (open_in f)
    let limit_on = ref false
    let activate_counting =
      if cc <> None || cl <> None then
        (fun () -> limit_on := true)
      else
        fun () -> ()
    let take_byte =
      match cc, cl with
        | Some cc, None -> 
            begin
              let climit = ref cc in
                fun () ->
                  match cs.take() with
                    | Some _ as c ->
                        incr byte_count;
                        if !limit_on then (decr climit; if !climit = 0 then raise Next);
                        c
                    | None -> None
            end
        | None, Some cl ->
            begin
              let llimit = ref cl in
                fun () ->
                  match cs.take() with
                    | Some x as c ->
                        incr byte_count;
                        if !limit_on then (if x = '\n' then (decr llimit; if !llimit = 0 then raise Next));
                        c
                    | None -> None
            end
        | Some cc, Some cl ->
            begin
              let climit = ref cc in
              let llimit = ref cl in
                fun () ->
                  match cs.take() with
                    | Some x as c ->
                        incr byte_count;
                        if !limit_on then
                          begin
                            decr climit; if !climit = 0 then raise Next; 
                            if x = '\n' then (decr llimit; if !llimit = 0 then raise Next)
                          end;
                        c
                    | None -> None
            end
        | None, None ->
            begin
              fun () ->
                match cs.take() with
                  | Some _ as c ->
                      incr byte_count; c
                  | None -> None
            end
    let take_line () =
      let b = Buffer.create 100 in
      let rec loop () =
        match take_byte() with
          | Some '\n' ->
              Some (Buffer.contents b)
          | Some c ->
              Buffer.add_char b c;
              loop()
          | None ->
              if Buffer.length b = 0 then
                None
              else
                Some (Buffer.contents b)
      in loop()

    let where_byte () = !byte_count
    let where_line () = match cs.where() with _f, l, _c -> l
  end (* of module *)
  in
    begin try
      begin match fl with
        | None -> ()
        | Some fl ->
            while Input.where_line() <= fl do
              ignore (Input.take_line())
            done;
            raise Next
      end;
      begin match fc with
        | None -> ()
        | Some fc ->
            while Input.where_byte() <= fc do
              ignore (Input.take_byte())
            done;
            raise Next
      end;
      begin match fr with
        | None -> ()
        | Some r -> 
            let r = Str.regexp r in
              while true do
                match Input.take_line() with
                  | None -> raise Next
                  | Some l ->
                      match Str.string_match r l 0 with
                        | true -> raise Next
                        | false -> ()
              done;
              raise Next
      end;
    with Next -> ()
    end;
    Input.activate_counting();
    begin try
      begin match tl with
        | None -> ()
        | Some tl ->
            while Input.where_line() < tl do
              match Input.take_line() with
                | None -> raise Next
                | Some l ->
                    Mpp_out.output_string out l;
                    Mpp_out.output_char out '\n'
            done;
            raise Next
      end;
      begin match tc with
        | None -> ()
        | Some tc ->
            while Input.where_byte() < tc do
              match Input.take_byte() with
                | None -> raise Next
                | Some c -> Mpp_out.output_char out c
            done;
            raise Next
      end;
      begin match tr with
        | None -> ()
        | Some r ->
            let r = Str.regexp r in
              while true do
                match Input.take_line() with
                  | None -> raise Next
                  | Some l ->
                      match Str.string_match r l 0 with
                        | true -> raise Next
                        | false ->
                            Mpp_out.output_string out l;
                            Mpp_out.output_char out '\n'
              done;
              raise Next
      end;
      begin (* no options *)
        while true do
          match Input.take_line() with
            | None -> raise Next
            | Some l ->
                Mpp_out.output_string out l;
                Mpp_out.output_char out '\n'
        done
      end
    with Next -> Mpp_out.flush out
    end






