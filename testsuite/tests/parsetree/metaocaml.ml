(* TEST
   flags = "-dparsetree"
   * toplevel *)

(* Using a toplevel test and not an expect test, because the locs get shifted
   by the expect blocks and the output is therefore not stable. *)

(* We hide metocaml stuff inside attribute, because it can't be typechecked properly *)

[@@@no_typecheck_inside .< 1 >. ];;

[@@@no_typecheck_inside .~ 2 ];;
