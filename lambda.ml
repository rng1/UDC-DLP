(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyTuple of ty list
  | TyRecord of (string * ty) list
  | TyList of ty
;;

type 'a context =
  (string * 'a) list
;;

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term
  | TmString of string
  | TmConcat of term * term
  | TmTuple of term list
  | TmRecord of (string * term) list
  | TmProj of term * string
  | TmEmpty of ty
  | TmCons of ty * term * term
  | TmIsempty of ty * term
  | TmHead of ty * term
  | TmTail of ty * term
;;

type command =
    Eval of term
  | BindTm of string * term
  | BindTy of string * ty
;;

(* CONTEXT MANAGEMENT *)

let emptyctx =
  []
;;

let addbinding ctx x bind =
  (x, bind) :: ctx
;;

let getbinding ctx x =
  List.assoc x ctx
;;


(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = 
  match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyArr (ty1, ty2) ->
      string_of_ty ty1 ^ " -> " ^ string_of_ty ty2
  | TyString ->
      "String"
  | TyTuple ty ->
      let rec print_tyTuple l = 
        match l with
            h :: [] -> string_of_ty h
          | h :: t -> (string_of_ty h ^ ", ") ^ print_tyTuple t
          | [] -> raise (Invalid_argument "empty tuple") 
      in "{" ^ print_tyTuple ty ^ "}" 
  | TyRecord ty ->
      let rec print_tyRecord l = 
        match l with
            (i, h) :: [] -> i ^ ":" ^ string_of_ty h
          | (i, h) :: t -> (i ^ ":" ^ string_of_ty h ^ ", ") ^ print_tyRecord t
          | [] -> ""
      in "{" ^ print_tyRecord ty ^ "}"
  | TyList ty ->
      "List[" ^ string_of_ty ty ^ "] "
;;

exception Type_error of string
;;

let rec subtypeof tm1 tm2 = 
  match (tm1, tm2) with
    (TyRecord(l1), TyRecord(l2)) ->
    let check (x, ty) l =
      try 
        subtypeof ty (List.assoc x l)
      with Not_found -> false
    in let rec contains l1 l2 = 
      match l1 with
          [] -> true
        | h::t -> check h l2 && contains t l2
    in contains l1 l2
  | (TyArr(s1, s2), TyArr(t1, t2)) -> subtypeof s1 t1 && subtypeof t2 s2
  | (tm1, tm2) -> tm1 = tm2
;;

let rec typeof ctx tm = 
  match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")

    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try getbinding ctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let ctx' = addbinding ctx x tyT1 in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
            if subtypeof tyT11 tyT2 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addbinding ctx x tyT1 in
      typeof ctx' t2

    (* T-Fix *)
  | TmFix t1 ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
        TyArr (tyT11, tyT12) ->
          if subtypeof tyT11 tyT12 then tyT12
          else raise (Type_error "result of body not compatible with domain")
      | _ -> raise (Type_error "arrow type expected"))
  
    (* T-String *)
  | TmString _ ->
      TyString

    (* T-Concat *)
  | TmConcat (t1, t2) ->
      if typeof ctx t1 = TyString && typeof ctx t2 = TyString then TyString
      else raise (Type_error "argument is not a string")

    (* T-Tuple *)
  | TmTuple t1 ->
      TyTuple (List.map (typeof ctx) t1)

    (* T-Record *)
  | TmRecord t1 ->
      TyRecord (List.combine (List.map fst t1) (List.map (typeof ctx) (List.map snd t1)))

    (* T-Proj *)
  | TmProj (t, s) ->
      (match typeof ctx t with
          TyTuple l -> (try List.nth l (int_of_string s - 1) with
            | _ -> raise (Type_error ("label " ^ s ^ " not found")))
        | TyRecord l -> (try List.assoc s l with
            | _ -> raise (Type_error ("label " ^ s  ^ " not found")))
        | _ -> raise (Type_error "tuple/record type expected"))
  
    (* T-Empty *)
  | TmEmpty t1 ->
      TyList t1
  
    (* T-Cons *)
  | TmCons (ty, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT2 with
          TyList ty21 ->
            if (subtypeof tyT1 ty) && (subtypeof tyT2 (TyList(ty))) then TyList(ty)
            else raise (Type_error "list types don't match")
        | _ -> raise (Type_error "list type expected")
      )
  
    (* T-Isempty *)
  | TmIsempty (ty, t1) ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
          TyList _ -> TyBool
        | _ -> raise (Type_error "list type expected"))

    (* T-Head *)
  | TmHead (ty, t1) ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
          TyList _ -> ty
        | _ -> raise (Type_error "list type expected"))

    (* T-Tail *)
  | TmTail (ty, t1) ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
          TyList _ -> TyList ty
        | _ -> raise (Type_error "list type expected"))
;;


(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ string_of_if t1 ^
      " then " ^ string_of_then_else t2 ^
      " else " ^ string_of_then_else t3
  | TmZero ->
      "0"
  | TmSucc t ->
      let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ string_of_succ t
      in f 1 t
  | TmPred t ->
      "pred " ^ string_of_app t
  | TmIsZero t ->
      "iszero " ^ string_of_app t
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_abs t
  | TmApp (t1, t2) ->
      string_of_app t1 ^ " " ^ string_of_app t2
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_app t1 ^ " in " ^ string_of_app t2
  | TmFix t ->
      "fix " ^ string_of_app t
  | TmString s ->
      "\"" ^ s ^ "\""
  | TmConcat (t1, t2) ->
      "concat " ^ string_of_app t1 ^ " " ^ string_of_app t2
  | TmTuple l ->
      let rec aux = function
          [] -> ""
        | h :: [] -> string_of_term h
        | h :: t -> string_of_term h ^ ", " ^ aux t
    in "{" ^ aux l ^ "}"
  | TmRecord l ->
      let rec aux = function
          [] -> ""
        | (i, h) :: [] -> i ^ "=" ^ string_of_term h
        | (i, h) :: t -> i ^ "=" ^ string_of_term h ^ ", " ^ aux t
      in "{" ^ aux l ^ "}"
  | TmProj (t, s) ->
      string_of_term t ^ "." ^ s
  | TmEmpty ty ->
      "[]"
  | TmCons (ty, t1, t2) ->
      let rec aux acc = function
          TmEmpty _ -> "[" ^ String.concat ", " (List.rev acc) ^ "]"
        | TmCons (_, h, t) -> aux (string_of_term h :: acc) t
        | _ -> raise (Failure "invalid pattern in TmCons aux function")
      in aux [] (TmCons(ty, t1, t2))
  | TmIsempty (ty,t) ->
      "isempty[" ^ string_of_ty ty ^ "] " ^ string_of_term t
  | TmHead (ty,t) ->
      "head[" ^ string_of_ty ty ^ "] " ^ string_of_term t
  | TmTail (ty,t) ->
      "tail[" ^ string_of_ty ty ^ "] " ^ string_of_term t

and string_of_if t = 
  match t with
      TmApp (t1, t2) -> string_of_app t1  ^ " " ^ string_of_app t2
    | _ -> string_of_term t

and string_of_then_else t = 
  match t with
      TmApp (t1, t2) -> string_of_app t1  ^ " " ^ string_of_app t2
    | _ -> string_of_term t

and string_of_succ t = 
  match t with
      TmPred t1 -> string_of_term t
    | _ ->  string_of_term t

and string_of_abs t = 
  match t with
      TmApp (t1, t2) -> string_of_app t1  ^ " " ^ string_of_app t2
    | _ -> string_of_term t

and string_of_app t = 
  match t with
      TmVar s -> s
    | TmString s-> "\"" ^ s ^ "\""
    | TmTrue -> "true"
    | TmFalse -> "false"
    | TmZero -> "0"
    | TmPred t1 -> string_of_term t
    | _ -> "(" ^ string_of_term t  ^ ")"

;;

let rec ldif l1 l2 =
  match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = 
  match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = 
  match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
      free_vars t
  | TmString _ ->
      []
  | TmConcat (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmTuple t ->
      let rec aux l = 
        match l with
            h :: [] -> free_vars h
          | h :: t -> lunion (free_vars h) (aux t)
          | [] -> []
      in aux t
  | TmRecord t ->
      let rec aux l =
        match l with
            (i, h) :: [] -> free_vars h
          | (i, h) :: t -> lunion (free_vars h) (aux t)
          | [] -> []
      in aux t
  | TmProj (t, _) ->
      free_vars t
  | TmEmpty _ -> 
      []
  | TmCons (ty, t1, t2) -> 
      lunion (free_vars t1) (free_vars t2)
  | TmIsempty (ty, t) ->
      free_vars t
  | TmHead (ty, t) ->
      free_vars t
  | TmTail (ty, t) ->
      free_vars t
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;

let rec subst x s tm = 
  match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) ->
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t ->
      TmFix (subst x s t)
  | TmString st ->
      TmString st
  | TmConcat (t1, t2) ->
      TmConcat (subst x s t1, subst x s t2)
  | TmTuple t ->
      TmTuple (List.map (subst x s) t)
  | TmRecord t ->
      TmRecord (List.combine (List.map fst t) (List.map (subst x s) (List.map snd t)))
  | TmProj (t1, t2) ->
      TmProj (subst x s t1, t2)
  | TmEmpty t -> 
      TmEmpty t
  | TmCons (ty, t1, t2) -> 
      TmCons (ty, subst x s t1, subst x s t2)
  | TmIsempty (ty, t) ->
      TmIsempty (ty, subst x s t)
  | TmHead (ty, t) ->
      TmHead (ty, subst x s t)
  | TmTail (ty, t) ->
      TmTail (ty, subst x s t)
;;

let rec isnumericval tm = 
  match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = 
  match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmString _ -> true
  | TmTuple l -> List.for_all (fun t -> isval t) l
  | TmRecord l -> List.for_all (fun t -> isval t) (List.map snd l)
  | t when isnumericval t -> true
  | TmEmpty _ -> true
  | TmCons (_, t1, t2) -> isval t1 && isval t2
  | _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 vctx tm = 
  match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 vctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 vctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 vctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 vctx t1 in
      TmIsZero t1'

    (* E-Var *)
  | TmVar t1 ->
      getbinding vctx t1

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App1 *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 vctx t2 in
      TmApp (v1, t2')

    (* E-App2 *)
  | TmApp (t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmLetIn (x, t1', t2)

    (* E-Fix1 *)
  | TmFix (TmAbs (x, _, t2)) ->
      subst x tm t2
  
    (* E-Fix2 *)
  | TmFix t1 ->
      let t1' = eval1 vctx t1 in
      TmFix t1'

    (* E-Concat1 *)
  | TmConcat (TmString s1, TmString s2) ->
      TmString (s1 ^ s2)

    (* E-Concat2 *)
  | TmConcat (TmString s1, t2) ->
      let t2' = eval1 vctx t2 in
      TmConcat (TmString s1, t2')

    (* E-Concat3 *)
  | TmConcat (t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmConcat (t1', t2)

    (* E-Tuple *)
  | TmTuple l ->
      let rec aux = function
          h :: t when isval h -> let t' = aux t in h::t'
        | h :: t -> let h' = eval1 vctx h in h'::t
        | [] -> raise NoRuleApplies
      in TmTuple (aux l)

    (* E-Record *)
  | TmRecord l ->
      let rec aux = function
          (k, h) :: t when isval h -> let t' = aux t in (k, h) :: t'
        | (k, h) :: t -> let h' = eval1 vctx h in (k, h') :: t
        | [] -> raise NoRuleApplies
      in TmRecord (aux l)

    (* E-Proj-Tuple *)
  | TmProj (TmTuple l as v, s) when isval v ->
      List.nth l (int_of_string s - 1)

    (* E-Proj-Record *)
  | TmProj (TmRecord l as v, s) when isval v ->
      List.assoc s l

    (* E-Proj *)
  | TmProj (t, s) ->
      let t' = eval1 vctx t in TmProj (t', s)

    (* E-Cons1 *)
  | TmCons (ty, t1, t2) when isval t1 -> 
      TmCons (ty, t1, eval1 vctx t2) 

    (* E-Cons2 *)
  | TmCons (ty, t1, t2) -> 
      TmCons (ty, (eval1 vctx t1), t2)

    (* E-Isempty1 *)
  | TmIsempty (ty, TmEmpty(_)) -> 
      TmTrue  

    (* E-Isempty2 *)
  | TmIsempty (ty, TmCons(_, _, _)) -> 
      TmFalse

    (* E-Isempty3 *)
  | TmIsempty (ty, t) -> 
      TmIsempty (ty, eval1 vctx t)

    (* E-Head1 *)
  | TmHead (ty, TmCons(_, t, _)) -> 
      t

    (* E-Head2 *)
  | TmHead (ty,t) -> 
      TmHead (ty, eval1 vctx t)

    (* E-Tail1 *)
  | TmTail (ty, TmCons(_, _, t)) -> 
      t

    (* E-Tail2 *)
  | TmTail (ty, t) -> 
      TmTail (ty, eval1 vctx t)

  | _ ->
      raise NoRuleApplies
;;

let apply_ctx ctx tm =
  List.fold_left (fun t x -> subst x (getbinding ctx x) t) tm (free_vars tm)
;;

let rec eval vctx tm =
  try
    let tm' = eval1 vctx tm in
    eval vctx tm'
  with
    NoRuleApplies -> apply_ctx vctx tm
;;

let execute (vctx, tctx) = function
    Eval tm ->
      let tyTm = typeof tctx tm in
      let tm' = eval vctx tm in
      print_endline ("- : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
      (vctx, tctx)
  | BindTm (s, tm) ->
      let tyTm = typeof tctx tm in
      let tm' = eval vctx tm in
      print_endline (s ^ " : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
      (addbinding vctx s tm', addbinding tctx s tyTm)
;;
