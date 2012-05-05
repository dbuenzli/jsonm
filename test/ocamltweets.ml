(* This code is in the public domain. *)

let str = Format.sprintf
let pp = Format.fprintf
let pp_text ppf s =    (* hints whitespace, this should really be in Format. *)
  let left = ref 0 and right = ref 0 and len = String.length s in 
  let flush () = 
    Format.pp_print_string ppf (String.sub s !left (!right - !left)); 
    incr right; left := !right;                      
  in
  while (!right <> len) do 
    if s.[!right] = '\n' then (flush (); Format.pp_force_newline ppf ()) else
    if s.[!right] = ' ' then (flush (); Format.pp_print_space ppf ())
    else incr right;
  done;
  if !left <> len then flush ()

let http_get uri = Unix.open_process_in (str "curl -L -f -s \"%s\"" uri)

(* Tweets *)

type tweet = 
  { id : string; 
    date : string;
    userid : string; 
    username : string; 
    text : string; }

let tweet_link t = str "http://twitter.com/%s/status/%s" t.userid t.id
let tweet_search q max =                     (* [q] should be URI encoded... *)
  str "http://search.twitter.com/search.json?q=%s&rpp=%d" q max 

let pp_tweet ppf t = pp ppf "@\n* @[%s - %s@]@\n  @[%s@]@\n@\n  @[%a@]@\n@." 
  t.username (tweet_link t) t.date pp_text t.text

(* N.B. The parsing code assumes members are given in order. Theoretically
   there's no order in JSON members. *)

let fold_tweets f acc src =
  let ret a = `Ok a in
  let bind v f = match v with `Error -> `Error | `End -> `End | `Ok v -> f v in
  let rec mem n d = match Jsonm.decode d with 
  | `Lexeme (`Name n') when n = n' -> 
      begin match Jsonm.decode d with 
      | `Lexeme (`String s) -> ret s | _ -> `Error  
      end
  | `End -> `End
  | _ -> mem n d
  in
  let parse_tweet d = 
    bind (mem "created_at" d) (fun date -> 
    bind (mem "from_user_id_str" d) (fun userid -> 
    bind (mem "from_user_name" d) (fun username -> 
    bind (mem "id_str" d) (fun id ->
    bind (mem "text" d) (fun text -> 
    ret { id; date; userid; username; text })))))
  in
  let rec loop f acc d = match parse_tweet d with 
  | `Ok t -> loop f (f acc t) d 
  | `End -> acc
  | `Error -> loop f acc d
  in
  loop f acc (Jsonm.decoder src)

let ocaml_tweets max = 
  let pp_tweet () t = pp_tweet Format.std_formatter t in
  let ic = http_get (tweet_search "OCaml" max) in
  fold_tweets pp_tweet () (`Channel ic); 
  close_in ic

let main () = 
  let exec = Filename.basename Sys.executable_name in
  let usage = str
    "Usage: %s [OPTION]...\n\
     Print the latest tweets about OCaml on stdout.\n\
     Options:" exec
  in
  let max = ref 50 in 
  let options = [ "-max", Arg.Set_int max, "maximal number of tweets."; ] in
  Arg.parse options (fun _ -> raise (Arg.Bad "illegal argument")) usage;
  ocaml_tweets !max

let () = main ()
