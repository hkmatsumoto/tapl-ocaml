open Base.Option

type term =
  | TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term

let rec is_numericval = function
  | TmZero -> true
  | TmSucc t -> is_numericval t
  | _ -> false

let is_val = function
  | TmTrue | TmFalse -> true
  | t when is_numericval t -> true
  | _ -> false

(* small-step evaluation *)
let rec eval' = function
  | TmIf (TmTrue, t2, _) -> return t2
  | TmIf (TmFalse, _, t3) -> return t3
  | TmIf (t1, t2, t3) -> eval' t1 >>| fun t1 -> TmIf (t1, t2, t3)
  | TmSucc t1 -> eval' t1 >>| fun t1 -> TmSucc t1
  | TmPred TmZero -> return TmZero
  | TmPred (TmSucc nv1) when is_numericval nv1 -> return nv1
  | TmPred t1 -> eval' t1 >>| fun t1 -> TmPred t1
  | TmIsZero TmZero -> return TmTrue
  | TmIsZero (TmSucc nv1) when is_numericval nv1 -> return TmFalse
  | TmIsZero t1 -> eval' t1 >>| fun t1 -> TmIsZero t1
  | _ -> None

let rec eval t = match eval' t with Some t -> eval t | None -> t
