(* Yoann Padioleau
 *
 * Copyright (C) 2010, University of Copenhagen DIKU and INRIA.
 * Copyright (C) 2002, 2006 Yoann Padioleau
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common

(* Tricks used to handle the ambiguity in the grammar with the typedef
 * which impose a cooperation between the lexer and the parser.
 *
 * An example by Hug_es Cassé: "in the symbol table, local
 * definition must replace type definition in order to correctly parse
 * local variable in functions body. This is the only way to correctly
 * handle this kind of exception, that is,
 *
 * typedef ... ID; int f(int *p) {int ID; return (ID) * *p;} If ID
 * isn't overload, last expression is parsed as a type cast, if it
 * isn't, this a multiplication."
 *
 * Why parse_typedef_fix2 ? Cos when introduce new variable, for
 * instance when declare parameters for a function such as int var_t,
 * then the var_t must not be lexed as a typedef, so we must disable
 * temporaly the typedef mechanism to allow variable with same name as
 * a typedef. *)

(* parse_typedef_fix *)
let _handle_typedef = ref true

let _always_look_typedef = ref true

(* parse_typedef_fix2 *)
let enable_typedef ()  = _handle_typedef := true
let disable_typedef () = _handle_typedef := false

let is_enabled_typedef () = !_handle_typedef

let _kr_possible = ref true
let is_kr_possible _ = !_kr_possible
let kr_impossible _ =
  if not !Flag_parsing_c.force_kr then _kr_possible := false



type identkind = TypeDefI | IdentI

(* Ca marche ce code ? on peut avoir un typedef puis un ident puis
 * un typedef nested ? oui car Hashtbl (dans scoped_h_env) gere l'historique.
 *
 * oldsimple:  but slow,  take 2 secondes on some C files
 *    let (typedef: typedef list list ref) = ref [[]]
 *)
let (_typedef : (string, identkind) Common.scoped_h_env ref) =
  ref (Common.empty_scoped_h_env ())

let is_typedef s  =

  if !_handle_typedef || !_always_look_typedef then
  (match (Common.optionise (fun () -> Common.lookup_h_env s !_typedef)) with
  | Some TypeDefI -> true
  | Some IdentI -> false
  | None -> false
  )
  else false

let new_scope() = Common.new_scope_h _typedef
let del_scope() = Common.del_scope_h _typedef

let add_typedef  s =
  
  Common.add_in_scope_h _typedef (s, TypeDefI)
let add_ident s    = 
  Common.add_in_scope_h _typedef (s, IdentI)

let add_typedef_root s =
  
  if !Flag_parsing_c.add_typedef_root
  then
    Hashtbl.add !_typedef.scoped_h s TypeDefI
  else add_typedef s (* have far more .failed without this *)


(* Used by parse_c when do some error recovery. The parse error may
 * have some bad side effects on typedef hash, so recover this.
 *)
let _old_state = ref (Common.clone_scoped_h_env !_typedef)

let save_typedef_state () =
  _old_state := Common.clone_scoped_h_env !_typedef

let restore_typedef_state () =
  _typedef := !_old_state




type context =
  | InTopLevel
  | InFunction
  | InStruct
  | InParameter
  | InInitializer
  | InEnum
(* InExpr ? but then orthogonal to InFunction. Could assign InExpr for
 * instance after a '=' as in 'a = (irq_t) b;'
 *)

let is_top_or_struct = function
  | InTopLevel
  | InStruct
      -> true
  | _ -> false

type lexer_hint = {
  mutable context_stack: context Common.stack;
 }

let default_hint () = {
  context_stack = [InTopLevel];
}

let _lexer_hint = ref (default_hint())

let string_of_context ctx = 
  match ctx with
  | InTopLevel -> "InTopLevel \n"
  | InFunction -> "InFunction \n"
  | InStruct -> "InStruct \n"
  | InParameter -> "InParameter \n"
  | InInitializer -> "InInitializer \n"
  | InEnum -> "InEnum \n"

let current_context () = List.hd !_lexer_hint.context_stack
let push_context ctx =
  (* print_string "CONTEXT CHANGING;\n";
  ctx |> string_of_context |> print_string ;
  print_string "CONTEXT CHANGED END;\n"; *)
  !_lexer_hint.context_stack <- ctx::!_lexer_hint.context_stack

let pop_context () =
  (* print_string "CONTEXT CHANGING DUE TO POP_CONTEXT;\n";
  current_context () |> string_of_context |> print_string ;

  print_string " ->\n";
   List.tl !_lexer_hint.context_stack |> List.hd |> string_of_context |> print_string ;
  print_string "CONTEXT CHANGED END;\n"; *)
  
  !_lexer_hint.context_stack <- List.tl !_lexer_hint.context_stack
 


let lexer_reset_typedef saved_typedefs =
  begin
    _handle_typedef := true;
    _kr_possible := true;
    (match saved_typedefs with
      None -> _typedef := Common.empty_scoped_h_env ()
    | Some t -> _typedef := t);
    _lexer_hint := (default_hint ());
  end


(* Tracks *some* subtypes,  but not all! This is a Java-specific hack to handle cases Coccinelle couldn't  *)
let _subtypes_of = ref [] 

let add_subtype_of subtype t = 
	(* print_string "we have a name now\n;";
	print_string ("name is " ^ subtype ^ "\n");
	let (qualif, (ty, _)) = t in 
	(match ty with 
	| Ast_c.TypeName (n, _) -> print_string (Ast_c.str_of_name n); print_string "\n";
	| _ -> print_string "\n";
	); *)
  _subtypes_of  := (subtype, t) :: !_subtypes_of

let known_subtypes _ = 
  !_subtypes_of

let find_new_name init = 
  let name_from_new_expr expr = 
   let ((expr_bis, _), _) = expr in 
    (match expr_bis with 
    | Ast_c.New (None, Left((Ast_c.FunCall(((Ast_c.Ident(ident), _), _), _), _), _)) -> 
       (match ident with 
	| Ast_c.RegularName (nm, _) -> Some nm
	| _ -> None (* Not handling yet*)
	)
    | _ -> None (* Not handling other cases that maybe we can handle yet*)
    ) 
  in 
  (match init with 
  | Ast_c.NoInit -> None 
  | Ast_c.ValInit (_, (initialiser, _)) -> 
	(match initialiser with 
	| Ast_c.InitExpr e -> name_from_new_expr e
	| _ -> (* can't handle the other cases yet!*) None
	)
  | Ast_c.ConstrInit _ -> None)

let _known_outer_scope_vars = ref []

let add_outer_scope_variable name typ = 
  _known_outer_scope_vars:= (name, typ) :: !_known_outer_scope_vars

let get_outer_scope_variables _ = 
  !_known_outer_scope_vars