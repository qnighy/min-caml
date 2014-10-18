type t = (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t (* arguments are uncurried *)
  | Tuple of t list
  | Array of t
  | Var of t option ref

let gentyp () = Var(ref None) (* 新しい型変数を作る *)

let rec pp pf = function
  | Unit -> Format.fprintf pf "unit"
  | Bool -> Format.fprintf pf "bool"
  | Int -> Format.fprintf pf "int"
  | Float -> Format.fprintf pf "float"
  | Fun ([], ret) ->
      Format.fprintf pf "@[<2>()@ ->@ %a@]" pp ret
  | Fun ((args_hd :: args_tl), ret) ->
      Format.fprintf pf "@[<2>(%a" pp args_hd;
      List.iter (fun arg ->
        Format.fprintf pf "@ *@ %a" pp arg
      ) args_tl;
      Format.fprintf pf ")@ ->@ %a@]" pp ret
  | Tuple [] ->
      Format.fprintf pf "@[()@]"
  | Tuple (tt_hd :: tt_tl) ->
      Format.fprintf pf "@[<2>(%a" pp tt_hd;
      List.iter (fun t ->
        Format.fprintf pf "@ *@ %a" pp t
      ) tt_tl;
      Format.fprintf pf ")@]"
  | Array te ->
      Format.fprintf pf "@[<2>%a@ array@]" pp te
  | Var tv ->
      begin match !tv with
      | None -> Format.fprintf pf "?"
      | Some tvv ->
          pp pf tvv
      end
