(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)
type _ tag =
| VAR    : string tag
| INT    : unit tag
| BOOL   : unit tag
| UNIT   : unit tag
| FLOAT  : unit tag
| STRING : unit tag
| REF    : unit tag
| LIST   : unit tag
| ARRAY  : unit tag
| RARROW : unit tag
| TIMES  : unit tag
| LPAREN : unit tag
| RPAREN : unit tag

module Tag =
struct
  type 'a t = 'a tag
  let print : type a. Format.formatter -> a t -> unit =
    fun fmt -> function
    | VAR    -> Format.fprintf fmt "VAR"
    | INT    -> Format.fprintf fmt "INT"
    | BOOL   -> Format.fprintf fmt "BOOL"
    | UNIT   -> Format.fprintf fmt "UNIT"
    | FLOAT  -> Format.fprintf fmt "FLOAT"
    | STRING -> Format.fprintf fmt "STRING"
    | REF    -> Format.fprintf fmt "REF"
    | LIST   -> Format.fprintf fmt "LIST"
    | ARRAY  -> Format.fprintf fmt "ARRAY"
    | RARROW -> Format.fprintf fmt "RARROW"
    | TIMES  -> Format.fprintf fmt "TIMES"
    | LPAREN -> Format.fprintf fmt "LPAREN"
    | RPAREN -> Format.fprintf fmt "RPAREN"

  let compare : type a b.a t -> b t -> (a, b) Asp.Types.cmp =
    fun x y ->
    match x, y with
    | VAR    , VAR    -> Eql
    | INT    , INT    -> Eql
    | BOOL   , BOOL   -> Eql
    | UNIT   , UNIT   -> Eql
    | FLOAT  , FLOAT  -> Eql
    | STRING , STRING -> Eql
    | REF    , REF    -> Eql
    | LIST   , LIST   -> Eql
    | ARRAY  , ARRAY  -> Eql
    | RARROW , RARROW -> Eql
    | TIMES  , TIMES  -> Eql
    | LPAREN , LPAREN -> Eql
    | RPAREN , RPAREN -> Eql
    | x      , y   -> if Obj.repr x < Obj.repr y then Leq else Geq
end

type utag = U : _ tag -> utag [@@unboxed]

type t = T : 'a tag * 'a -> t

let to_int : utag -> int = Obj.magic

let tag : t -> utag = function (T (tag,_)) -> U tag

module Ord =
struct
  type t = utag
  let compare l r = Pervasives.compare (to_int l) (to_int r)
end

module TagSet = Set.Make(Ord)
let all = TagSet.of_list
            [U VAR   ;
             U INT   ;
             U BOOL  ;
             U UNIT  ;
             U FLOAT ;
             U STRING;
             U REF   ;
             U LIST  ;
             U ARRAY ;
             U RARROW;
             U TIMES ;
             U LPAREN;
             U RPAREN]

let inj t = U t

let match_ : type a b. t -> a tag -> (a -> b) -> (unit -> b) -> b =
  fun (T (tag, v)) tag' yes no ->
  if U tag = U tag' then yes (Obj.magic v) else no ()

let matchlist_ : type a b. t -> a tag list -> (a -> b) -> (unit -> b) -> b =
  fun (T (tag, v)) tags yes no ->
  if List.mem (Obj.magic tag) tags then yes (Obj.magic v) else no ()
