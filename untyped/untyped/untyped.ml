open Base.Option

type term =
  | TmVar of int * int
  | TmAbs of string * term
  | TmApp of term * term

type binding = NameBinding

type context = (string * binding) list

let term_shift d t =
  let rec aux c = function
    | TmVar (k, n) when k < c -> TmVar (k, n + d)
    | TmVar (k, n) -> TmVar (k + d, n + d)
    | TmAbs (s, t1) -> TmAbs (s, aux (c + 1) t1)
    | TmApp (t1, t2) -> TmApp (aux c t1, aux c t2)
  in
  aux 0 t

let term_subst j s t =
  let rec aux c = function
    | TmVar (k, _) when k = j + c -> term_shift c s
    | TmVar (k, n) -> TmVar (k, n)
    | TmAbs (s, t1) -> TmAbs (s, aux (c + 1) t1)
    | TmApp (t1, t2) -> TmApp (aux c t1, aux c t2)
  in
  aux 0 t

let term_subst_top s t = term_shift (-1) (term_subst 0 (term_shift 1 s) t)

let is_val = function TmAbs _ -> true | _ -> false

let rec eval' ctx = function
  | TmApp (TmAbs (_, t12), v2) when is_val v2 -> return (term_subst_top v2 t12)
  | TmApp (v1, t2) when is_val v1 -> eval' ctx t2 >>| fun t2 -> TmApp (v1, t2)
  | TmApp (t1, t2) -> eval' ctx t1 >>| fun t1 -> TmApp (t1, t2)
  | _ -> None

let rec eval ctx t = match eval' ctx t with Some t -> eval ctx t | None -> t