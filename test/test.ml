(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

let str = Format.sprintf 
let log f = Format.printf (f ^^ "@?") 
let fail fmt =
  let fail _ = failwith (Format.flush_str_formatter ()) in
  Format.kfprintf fail Format.str_formatter fmt

let encoder_invalid () = 
  log "Invalid encodes.\n";
  let rec encode_seq e = function 
  | v :: vs -> ignore (Jsonm.Uncut.encode e v); encode_seq e vs
  | [] -> ()
  in
  let seq ~invalid s =
    let test ~minify = 
      let e = Jsonm.encoder ~minify (`Buffer (Buffer.create 256)) in
      try encode_seq e s; assert (not invalid) with
      | Invalid_argument _ as e -> if invalid then () else raise e
    in
    test ~minify:true; test ~minify:false
  in
  seq ~invalid:true [ `Lexeme `Oe];
  seq ~invalid:true [ `Lexeme `Null];
  seq ~invalid:true [ `Lexeme (`Name "b")];
  seq ~invalid:true [ `White "    "; `Lexeme `Oe];
  seq ~invalid:true [ `Lexeme `Os; `Lexeme `Ae];
  seq ~invalid:true [ `Comment (`S, "bla"); `Lexeme `Os; `Lexeme `Ae];
  seq ~invalid:true [ `Lexeme `Os; `Lexeme `Null];
  seq ~invalid:true [ `Lexeme `Os; `Lexeme (`Name "b"); `Lexeme (`Name "b")];
  seq ~invalid:true [ `Lexeme `Os; `Lexeme (`Name "b"); `Lexeme `Ae];
  seq ~invalid:true [ `Lexeme `Os; `Lexeme (`Name "b"); `Lexeme `Oe];
  seq ~invalid:true [ `Lexeme `Os; `Lexeme (`Name "b"); `Lexeme `Ae];
  seq ~invalid:true [ `Lexeme `Os; `Lexeme (`Name "b"); `Lexeme `Null; 
                      `Lexeme `Null; ];
  seq ~invalid:true [ `Lexeme `Os; `Lexeme (`Name "b"); `Lexeme `Null; 
                      `Lexeme (`Name "c"); `Lexeme `Ae;];
  seq ~invalid:true [ `Lexeme `As; `Lexeme (`Oe) ];
  seq ~invalid:true [ `Lexeme `As; `Lexeme (`Name "b") ];
  seq ~invalid:true [ `Lexeme `As; `Lexeme `Null; `Lexeme (`Name "b"); ];
  seq ~invalid:true [ `Lexeme `As; `Lexeme `Ae; `Lexeme `As];
  seq ~invalid:true [ `Lexeme `As; `Lexeme `Ae; `Lexeme `Null];
  seq ~invalid:true [ `Lexeme `As; `Lexeme `Ae; `End; `Lexeme `As];
  seq ~invalid:true [ `Lexeme `Os; `Lexeme `Oe; `Lexeme `Os];
  seq ~invalid:true [ `Lexeme `Os; `Lexeme `Oe; `Lexeme `Null];
  seq ~invalid:true [ `Lexeme `Os; `Lexeme `Oe; `End; `Lexeme `Os];
  ()

let encoder_escapes () = 
  log "Encoder escapes.\n";
  let encode ascii sascii =
    let b = Buffer.create 10 in 
    let e = Jsonm.encoder (`Buffer b) in 
    let enc v = ignore (Jsonm.encode e (`Lexeme v)) in
    List.iter enc [ `As; `String (Printf.sprintf "%c" (Char.chr ascii)); `Ae ]; 
    ignore (Jsonm.encode e `End);
    let json = Buffer.contents b in
    let exp = str "[\"%s\"]" sascii in
    if json <> exp then fail "found: %s exp: %s" json exp
  in
  encode 0x22 "\\\""; 
  encode 0x5C "\\\\";
  for i = 0x00 to 0x1F do 
    if i = 0x0A then encode i "\\n" else
    encode i (str "\\u00%02X" i)
  done;
  ()

let decoder_encoding_guess () = 
  log "Decoder encoding guesses.\n";
  let test enc s = 
    let d = Jsonm.decoder (`String s) in
    let enc' = (ignore (Jsonm.decode d); Jsonm.decoder_encoding d) in
    if  enc' <> enc then 
    fail "found: %s exp: %s"
      (Uutf.encoding_to_string enc') (Uutf.encoding_to_string enc)
  in
  test `UTF_8 "[]"; 
  test `UTF_8 "{}"; 
  test `UTF_16BE "\x00\x5B\x00\x5D"; 
  test `UTF_16BE "\x00\x7B\x00\x7D";
  test `UTF_16LE "\x5B\x00\x5D\x00"; 
  test `UTF_16LE "\x7B\x00\x7D\x00";
  ()

let test_decode fnd exp = 
  if fnd <> exp then fail "found: %a expected: %a" 
  Jsonm.Uncut.pp_decode fnd Jsonm.Uncut.pp_decode exp

let test_seq decode src seq = 
  let d = Jsonm.decoder (`String src) in 
  let rec loop d = function  [] -> ()
  | v :: vs -> test_decode (decode d) v; loop d vs
  in
  loop d seq 

let arr seq = [`Lexeme `As] @ seq @ [`Lexeme `Ae; `End; `End; `End ]

let decoder_comments () = 
  log "Decoder comments.\n";
  let test (s,c) src = test_seq Jsonm.Uncut.decode src (arr [`Comment (s,c)]) in
  let test_eoi v src = test_seq Jsonm.Uncut.decode  src
    [`Lexeme `As; `Lexeme `Ae; v; `End]; 
  in  
  test (`M, "bla") "[/*bla*/]";
  test (`M, "b*") "[/*b**/]";
  test (`M, "b** /") "[/*b** /*/]";
  test (`M, "b** /") "[/*b** /*/]";
  test (`M, "b***\n/") "[/*b***\n/*/]";
  test (`S, "abcd") "[//abcd\n]";
  test_eoi (`Comment (`S, "abcd")) "[]//abcd";
  test_eoi (`Comment (`S, "abcd///* ")) "[]//abcd///* ";
  test_eoi (`Comment (`M, " abcd ")) "[]/* abcd */";
  test_eoi (`Comment (`M, " abcd ")) "[]/* abcd */";
  test_eoi (`Error (`Unclosed `Comment)) "[]/* abcd ";
  test_eoi (`Error (`Expected `Comment)) "[]/";
  ()

let decoder_escapes () =
  log "Decoder escapes.\n";
  let test str src = test_seq Jsonm.decode src (arr [`Lexeme (`String str)]) in
  let test_ill ill str src = test_seq Jsonm.decode src
    (arr [`Error (`Illegal_escape ill); `Lexeme (`String str)])
  in
  let s s = Printf.sprintf "[\"%s\"]" s in
  test "<\">" (s "<\\\">");
  test "<\\>" (s "<\\\\>");
  test "</>" (s "<\\/>");
  test "<\b>" (s "<\\b>");
  test "<\x0C>" (s "<\\f>");
  test "<\n>" (s "<\\n>");
  test "<\r>" (s "<\\r>");
  test "<\t>" (s "<\\t>");
  test "<\xF0\x9D\x84\x9E><\xE6\xB0\xB4>" (s "<\\uD834\\uDd1E><\\u6C34>");
  test_ill (`Not_esc_uchar 0x61) "<\xEF\xBF\xBD>" (s "<\\a>");
  test_ill (`Not_esc_uchar 0x31) "<\xEF\xBF\xBD>" (s "<\\1>");
  test_ill (`Not_esc_uchar 0xFFFD) "<\xEF\xBF\xBD>" (s "<\\\xEF\xBF\xBD>");
  test_ill (`Not_hex_uchar 0x47) "<\xEF\xBF\xBDAF1>" (s "<\\uGAF1>");
  test_ill (`Not_hex_uchar 0x47) "<\xEF\xBF\xBDF1>" (s "<\\uAGF1>");
  test_ill (`Not_hex_uchar 0x67) "<\xEF\xBF\xBD1>" (s "<\\uAFg1>");
  test_ill (`Not_hex_uchar 0x67) "<\xEF\xBF\xBD>" (s "<\\uAF1g>");
  test_ill (`Not_lo_surrogate 0x6C34) "<\xEF\xBF\xBD>" (s "<\\uD834\\u6C34>");
  test_ill (`Lone_hi_surrogate 0xD834) "<\xEF\xBF\xBDbla>" (s "<\\uD834bla>");
  test_ill (`Lone_lo_surrogate 0xDD1E) "<\xEF\xBF\xBDbla>" (s "<\\uDd1Ebla>");
  test_ill (`Lone_hi_surrogate 0xD834) "<\xEF\xBF\xBD\nf>" (s "<\\uD834\\nf>");
  ()

let decoder_strings () = 
  log "Decoder strings.\n";
  test_seq Jsonm.decode "[\"blibla\"]" (arr [ `Lexeme (`String "blibla") ]); 
  test_seq Jsonm.decode "[\"bli\nbla\"]" 
    (arr [`Error (`Illegal_string_uchar 0x0A); 
          `Lexeme (`String "bli\xEF\xBF\xBDbla"); ]);
  test_seq Jsonm.decode "[\"blabla" 
    [`Lexeme `As; `Error (`Unclosed `Comment); `End; `End]; 
  ()

let decoder_literals () =
  log "Decoder literals.\n"; 
  test_seq Jsonm.decode "[null]" (arr [ `Lexeme `Null]); 
  test_seq Jsonm.decode "[true]" (arr [ `Lexeme (`Bool true)]); 
  test_seq Jsonm.decode "[false]" (arr [ `Lexeme (`Bool false)]); 
  test_seq Jsonm.decode "[truee]" (arr [ `Error (`Illegal_literal "truee")]); 
  test_seq Jsonm.decode "[tru" 
    [ `Lexeme `As; `Error (`Illegal_literal "tru"); `Error (`Unclosed `As); 
      `End; `End; `End ];
  test_seq Jsonm.decode "{\"\" : tru" 
    [ `Lexeme `Os; `Lexeme (`Name ""); `Error (`Illegal_literal "tru"); 
      `Error (`Unclosed `Os); `End; `End; `End ];
  ()

let decoder_numbers () = 
  log "Decoder numbers.\n"; 
  test_seq Jsonm.decode "[1.0]" (arr [ `Lexeme (`Float 1.0)]);
  test_seq Jsonm.decode "[1e12]" (arr [ `Lexeme (`Float 1e12)]);
  test_seq Jsonm.decode "[-1e12]" (arr [ `Lexeme (`Float ~-.1e12)]);
  test_seq Jsonm.decode "[-1eee2]" (arr [ `Error (`Illegal_number "-1eee2")]);
  test_seq Jsonm.decode "[-1ee2" 
    [ `Lexeme `As; `Error (`Illegal_number "-1ee2"); `Error (`Unclosed `As); 
      `End; `End; `End ];
  test_seq Jsonm.decode "{\"\" : -1ee2" 
    [ `Lexeme `Os; `Lexeme (`Name ""); `Error (`Illegal_number "-1ee2"); 
      `Error (`Unclosed `Os); `End; `End; `End ];
  ()

let decoder_arrays () = 
  log "Decoder arrays.\n";
  test_seq Jsonm.decode "[]" (arr []);
  test_seq Jsonm.decode "[" 
    [`Lexeme `As; `Error (`Unclosed `As); `End; `End; `End];
  test_seq Jsonm.decode "[null" 
    [`Lexeme `As; `Lexeme `Null; `Error (`Unclosed `As); 
     `End; `End; `End];
  test_seq Jsonm.decode "[null," 
    [`Lexeme `As; `Lexeme `Null; `Error (`Expected (`Value)); 
     `Error (`Unclosed `As); `End; `End; `End];
  test_seq Jsonm.decode "[null { null]" 
    [`Lexeme `As; `Lexeme `Null; `Error (`Expected (`Aval false)); 
     `Lexeme `Ae; `End; `End; `End];
  test_seq Jsonm.decode "[null { null,null]" 
    [`Lexeme `As; `Lexeme `Null; `Error (`Expected (`Aval false)); 
     `Lexeme `Null; `Lexeme `Ae; `End; `End; `End];
  test_seq Jsonm.decode "[; null]" 
    [`Lexeme `As; `Error (`Expected (`Aval true));
     `Lexeme `Ae; `End; `End; `End];
  test_seq Jsonm.decode "[; , null]" 
    [`Lexeme `As; `Error (`Expected (`Aval true)); `Lexeme `Null;
     `Lexeme `Ae; `End; `End; `End];
  ()

let decoder_objects () =
  log "Decoder objects.\n";
  test_seq Jsonm.decode "{" 
    [`Lexeme `Os; `Error (`Unclosed `Os); `End; `End; `End];
  test_seq Jsonm.decode "{null" 
    [`Lexeme `Os; `Error (`Expected (`Omem true)); `Error (`Unclosed `Os); 
     `End; `End; `End];
  test_seq Jsonm.decode "{ \"b\" " 
    [`Lexeme `Os; `Lexeme (`Name "b"); `Error (`Expected (`Name_sep)); 
     `Error (`Unclosed `Os); `End; `End; `End];
  test_seq Jsonm.decode "{ \"b\" : ] null]" 
    [`Lexeme `Os; `Lexeme (`Name "b"); `Error (`Expected (`Value)); 
     `Error (`Unclosed `Os); `End; `End; `End];
  test_seq Jsonm.decode "{ \"b\" : null"
    [`Lexeme `Os; `Lexeme (`Name "b"); `Lexeme `Null; 
     `Error (`Unclosed `Os); `End; `End; `End];
  test_seq Jsonm.decode "{; null}" 
    [`Lexeme `Os; `Error (`Expected (`Omem true)); 
     `Lexeme `Oe; `End; `End; `End];
  test_seq Jsonm.decode "{ ill : null, \"bli\" : null}" 
    [`Lexeme `Os; `Error (`Expected (`Omem true)); `Lexeme (`Name "bli"); 
     `Lexeme `Null; `Lexeme `Oe; `End; `End; `End];
  test_seq Jsonm.decode "{ \"bli\" : null ill : null }" 
    [`Lexeme `Os; `Lexeme (`Name "bli"); 
     `Lexeme `Null; `Error (`Expected (`Omem false)); 
     `Lexeme `Oe; `End; `End; `End];
  test_seq Jsonm.decode "{ \"bli\" : null, ill : null }" 
    [`Lexeme `Os; `Lexeme (`Name "bli"); 
     `Lexeme `Null; `Error (`Expected `Name); 
     `Lexeme `Oe; `End; `End; `End];
  ()

let decoder_json_text () = 
  log "Decoder JSON text.\n";
  test_seq Jsonm.decode "a : null {}" 
    [ `Error (`Expected `Json); `Lexeme `Os; `Lexeme `Oe];
  test_seq Jsonm.decode "a : null []" 
    [ `Error (`Expected `Json); `Lexeme `As; `Lexeme `Ae];
  ()

let decoder_bom () = 
  log "Decoder BOM.\n";
  let seq = [`Error `Illegal_BOM; `Lexeme `Os; `Lexeme `Oe] in
  test_seq Jsonm.decode "\xEF\xBB\xBF  {}" seq;
  test_seq Jsonm.decode "\xFE\xFF\x00\x7B\x00\x7D" seq; 
  test_seq Jsonm.decode "\xFE\xFF\x00\x7B\x00\x7D\x00" seq
  
let decoder_eoi () =
  log "Decoder end of input.\n";
  test_seq Jsonm.decode "" [`Error (`Expected `Json) ];
  test_seq Jsonm.decode "{} a : null" 
    [ `Lexeme `Os; `Lexeme `Oe; `Error (`Expected `Eoi); ];
  test_seq Jsonm.decode "[] a : null " 
    [ `Lexeme `As; `Lexeme `Ae; `Error (`Expected `Eoi); ];
  ()
    
let trip () = 
  log "Codec round-trips.\n";
  let trip s = 
    let b = Buffer.create (String.length s) in
    let d = Jsonm.decoder (`String s) in 
    let e = Jsonm.encoder (`Buffer b) in
    let rec loop d e = match Jsonm.decode d with 
    | `Lexeme _ as v -> ignore (Jsonm.encode e v); loop d e
    | `End as v -> ignore (Jsonm.encode e v)
    | `Error e -> fail "err: %a" Jsonm.pp_error e
    | `Await -> assert false
    in
    loop d e;
    let trips = Buffer.contents b in
    if trips <> s then 
    fail "fnd: %s@\nexp: %s@\n" trips s
  in
  trip "[null,null,0.1,true,false,[true,false]]";
  trip "{\"a\":[1,2,4,5,[true,false]],\"b\":{}}";
  trip "{\"a\":[1,2,4,5,[true,{\"c\":[null]}]],\"b\":{}}";
  trip "{\"a\":[1,2,4,5,[true,{\"c\":[\"\\nbli\",5,6]}]],\"b\":{}}";
  (* Verify that integers that can be represented exactly by an OCaml float
     value [-2^53;2^53] do trip. *)
  trip "[9007199254740992,-9007199254740992]";
  ()
    
let test () =
  Printexc.record_backtrace true;
  encoder_invalid ();
  encoder_escapes ();
  decoder_encoding_guess ();
  decoder_escapes ();
  decoder_comments ();
  decoder_literals ();
  decoder_numbers ();
  decoder_arrays (); 
  decoder_objects (); 
  decoder_json_text ();
  decoder_bom ();
  decoder_eoi ();
  trip ();
  log "All tests succeeded.\n"

let () = if not (!Sys.interactive) then test ()

(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
     
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
