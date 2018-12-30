(* Module StringStream
   allows the user to "open" a string and read in its characters one by one as if they were read from stdin.
   
   It provides four major functions:
   
   sopen inputstring : "opens" string inputstring;
   getchar descriptor: gets the next char from a sopen'ed string;
   seechar descriptor: views the next char that will be read by getchar; the descriptor is _not_ updated;
   sclose  descriptor: "closes" the stringstream
   
   Note: in this trivial implementation, only _one_string can be operated upon at any given time...
 *)  
 
type ltable = (string * string list) list
type distribution =
  { total : int ;
    amounts : (string * int) list }

module StringStream =
struct
  type strfile = { mutable pos : int ; str : string ; last : int }
  let empty = []
  let sopen inputstring =
    { pos = -1 ; str = inputstring; last =  String.length inputstring }

  let sclose des =
      des.pos = -1

  let isopen des = (des.pos >= -1)

     (* getchar... gets the next char from the des-sopen'ed string *)
  let getchar des =
    if isopen des then
      begin
        des.pos <- des.pos + 1;
        if des.pos < des.last then Some (String.get des.str des.pos) else None
      end
    else None

     (* seechar views the next char that will be read by getchar: pos is not updated! *)
  let seechar des =
    let nextpos = des.pos + 1 in
    if isopen des then
      if nextpos < des.last then Some (String.get des.str nextpos) else None
    else None

  let sreset des = { pos = -1 ; str = des.str ; last = des.last }
  let sreopen inputstring = sopen inputstring
end;;

(* Here we test StringStream by creating a tokenizer: a function that returns a list of
   strings that appeared in a whitespace separated input string
 *)
 
open StringStream;;

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

type classes = EoS | WhiteSpace | Valid;;

let charclass c =
  match c with
  | None    ->   EoS
  | Some(c) ->   if is_alpha(c) || is_digit(c) then Valid else WhiteSpace
;;

let tokenize des =
    (* we assume that the stringstream is open *)

    (* let nextchar be the next char in the input stream, if any *)
  let nextchar = seechar des in

    (* let classchar be the class of nextchar *)
  let classchar = charclass nextchar in

    (* advances pos to either None or a Valid (non WhiteSpace) character *)
  let valid_or_none =
    match classchar with
    | EoS        ->  None
    | WhiteSpace -> (* purge all whitespaces *)
        let rec purge des' =
          begin
            let ch = seechar des' in
            match charclass ch with
            | WhiteSpace -> begin
                let _ = getchar des' in
                purge des'
              end
            | EoS        -> None
            | Valid      -> ch
          end
        in
        purge des
    | Valid     -> nextchar
  in

    (* "None" means we bumped onto the EoS; otherwise, we collect a valid word *)
  match valid_or_none with
  | None    -> "" (* when EoS, return an empty string *)
  | Some(c) ->    (* collect all non-whitespaces *)
      begin
        let rec collect des' accu =
          let ch = seechar des' in
          match charclass ch with
          | Valid ->
              begin
                let ch = getchar des' in
                match ch with
                | None    -> accu
                | Some(cha) -> collect des' ( accu ^ ( String.make 1 cha ) )
              end
          (* as soon as a whitespace is encountered, or the stream is over, we return accu *)
          | WhiteSpace -> accu
          | EoS        -> accu
        in
        collect des ""
      end
;;

let words str =
   (* 1. First I open the string stream *)
  let des = StringStream.sopen str in
  let rec accumulate desc accu =
    let s = tokenize desc in
    if String.length s = 0 then accu else accumulate desc ( accu @ [s] )
  in
  accumulate des []
;;


let test_words = words "This is a simple test of the tokenization procedure";;

(*
    $ ocaml
            OCaml version 4.05.0

    # #use "stringstream.ml";;
    type ltable = (string * string list) list
    type distribution = { total : int; amounts : (string * int) list; }
    module StringStream :
      sig
        type strfile = { mutable pos : int; str : string; last : int; }
        val empty : 'a list
        val sopen : string -> strfile
        val sclose : strfile -> bool
        val isopen : strfile -> bool
        val getchar : strfile -> char option
        val seechar : strfile -> char option
        val sreset : strfile -> strfile
        val sreopen : string -> strfile
      end
    val is_alpha : char -> bool = <fun>
    val is_digit : char -> bool = <fun>
    type classes = EoS | WhiteSpace | Valid
    val charclass : char option -> classes = <fun>
    val tokenize : StringStream.strfile -> string = <fun>
    val words : string -> string list = <fun>
    val test_words : string list =
      ["This"; "is"; "a"; "simple"; "test"; "of"; "the"; "tokenization";
       "procedure"]
 *)
