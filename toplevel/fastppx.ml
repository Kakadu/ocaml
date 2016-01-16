

open Format

let all_ppx : Obj.t list ref = ref []

let add_extension ppf o =
  all_ppx := o :: !all_ppx;
  Format.fprintf ppf "Extension is added. There are %d extensions at the moment\n%!"
                 (List.length !all_ppx);
  Format.pp_print_flush ppf ()


let apply (ss: Parsetree.structure_item list) =
  let open Ast_mapper in
  match !all_ppx with
  | [] -> ss
  | ms ->
      List.map (fun si ->
        List.fold_left (fun acc m ->
          let m : Ast_mapper.mapper = Obj.obj m in
          m.structure_item m acc)
          si ms)
        ss

let clear () =
  Format.fprintf std_formatter  "All extensions are disabled\n%!";
  all_ppx := []
