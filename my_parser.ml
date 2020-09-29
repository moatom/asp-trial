(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)
(* 
#require "asp";;
#load "my_parser.cma";;
My_parser.Parser.staged_complete "int -> (bool -> bool) ref ref -> int*(bool -> int) ref list";;
My_parser.Parser.staged_complete "int list * (bool -> int ref) ref list";;
*)
module P1 = Asp.Staged.Parse(Asp.Utilities.Unstaged.Char_element)
module P2 = Asp.Staged.Parse(Tokens)

module Lexer = struct
  open P1
  open Asp.Utilities.Staged.Charparsing

  let mk_token tk = .< Some (Tokens.T (tk, ())) >.

  let str s = 
    let bd = String.length s - 1 in
    let z = ref (chr s.[0] $ (fun p -> .< [.~p] >.)) in
    for i = 1 to bd do
      z := !z >>> chr s.[i] $ (fun p -> .< let (f,s) = .~p in f @ [s] >.)
    done;
    !z $ (fun p -> .< Asp.Utilities.Unstaged.Unstaged.toString .~p >.)

  let var = chr '\'' >>> any [lower; upper] >>> star (any [lower; upper; digit; sym]) $
          (fun p -> .<Pair.uncurry3 (fun a b c ->
           Some (Tokens.T (Tokens.VAR, Asp.Utilities.Unstaged.Unstaged.toString @@ a :: b :: c))) .~p>.)

  (* My_parser.Parser.staged_complete "(int)d";; ...bad! *)
  let lex : Tokens_base.t option t =
    fix @@ fun self ->
           ((whitespace >>> self $ (fun x -> .< snd .~x >.))
            <|> var
            <|> (str "int"    $ (fun _ -> mk_token Tokens.INT))
            <|> (str "bool"   $ (fun _ -> mk_token Tokens.BOOL))
            <|> (str "unit"   $ (fun _ -> mk_token Tokens.UNIT))
            <|> (str "float"  $ (fun _ -> mk_token Tokens.FLOAT))
            <|> (str "string" $ (fun _ -> mk_token Tokens.STRING))
            <|> (str "ref"    $ (fun _ -> mk_token Tokens.REF))
            <|> (str "list"   $ (fun _ -> mk_token Tokens.LIST))
            <|> (str "array"  $ (fun _ -> mk_token Tokens.ARRAY))
            <|> (str "->"     $ (fun _ -> mk_token Tokens.RARROW))
            <|> (chr '*'      $ (fun _ -> mk_token Tokens.TIMES))
            <|> (chr '('      $ (fun _ -> mk_token Tokens.LPAREN))
            <|> (chr ')'      $ (fun _ -> mk_token Tokens.RPAREN))
            <|> (eps .<()>.   $ (fun _ -> .< None >.)))

  (* TODO: move all the following code to a library *)
  let lexcode =
    let module R = P1.Parser(Asp_streamcode.Stringcode) in
    R.compile (type_check lex)

  let staged_lexer = Runcode.run lexcode

  let next s =
    let i = ref 0 in
    fun _ ->
    let tok, i' = staged_lexer ~index:!i s in
    i := i';
    tok

  let staged_lexer_stream : string -> Tokens_base.t Stream.t =
    fun s -> Stream.from (next s)
end

module Parser =
struct
  open Tokens_base
  open P2

  type term = Type.t

  let paren p = (tok LPAREN >>> p >>> tok RPAREN) $ (fun p -> .< snd (fst .~p) >.)

  let mkop o e1 e2 = .< .~o .~e1 .~e2 >.
  let token exp op out_tk = (paren exp) >>> star (tok op) $ fun p ->
                            .< match .~p with
                            | (e,[])      -> e
                            | (e, g::ops) -> List.fold_left (fun z _ -> out_tk z) (out_tk e) ops >.

  let atom =  (    (tok VAR)                             $ fun p -> .< Type.Var .~p >.)
              <|> ((tok INT)                             $ fun _ -> .< Type.Int >.)
              <|> ((tok BOOL)                            $ fun _ -> .< Type.Bool >.)
              <|> ((tok UNIT)                            $ fun _ -> .< Type.Unit >.)
              <|> ((tok FLOAT)                           $ fun _ -> .< Type.Float >.)
              <|> ((tok STRING)                          $ fun _ -> .< Type.String >.)

  let postfix_exp exp = any [atom; paren exp] >>> star (any [
                          ((tok REF)  $ fun _ -> .< fun e -> Type.Ref   e >.);
                          ((tok LIST) $ fun _ -> .< fun e -> Type.List  e >.);
                          ((tok ARRAY)$ fun _ -> .< fun e -> Type.Array e >.)
                        ]) $ fun p ->
                        .< (function (e, ops) -> List.fold_left (fun z f -> f z) e ops) .~p >.

  let exp    = fix (fun exp ->
                   (infix mkop (postfix_exp exp) [
                     Left,  (tok TIMES  $ fun _ -> .< fun x y -> Type.Tuple (x, y) >.); (* higher order *)
                      Right, (tok RARROW $ fun _ -> .< fun x y -> Type.Fun (x, y) >.)
                      ])
                )

  (* TODO: move all the following code to a library *)
  let parsecode =
    let module R = P2.Parser(Asp_streamcode.Streamcode(struct type t = Tokens_base.t end)) in
    R.compile (type_check exp)

  let staged_parser : Tokens_base.t Stream.t -> term
    = Runcode.run parsecode

  let staged_complete : string -> term =
    fun s -> staged_parser (Lexer.staged_lexer_stream s)
end

