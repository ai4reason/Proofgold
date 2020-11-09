(* Copyright (c) 2020 The Proofgold developers *)
(* Copyright (c) 2016 The Qeditas developers *)
(* Copyright (c) 2017-2018 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Ser
open Hashaux
open Hash
open Zarithint
open Logic
open Mathdata

(** val nth_error : 'a1 list -> int -> 'a1 option **)

let rec nth_error l n =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    match l with
    | [] -> None
    | x :: _ -> Some x)
    (fun n0 ->
    match l with
    | [] -> None
    | _ :: l0 -> nth_error l0 n0)
    n

(** val find : ('a1 -> bool) -> 'a1 list -> 'a1 option **)

let rec find f = function
| [] -> None
| x :: tl -> if f x then Some x else find f tl

(** val redex_amount : int **)

let redex_amount = 2048

(** val first : ((hashval * stp) * trm option) -> hashval **)

let first = function
| (p, _) -> let (a, _) = p in a

(** val findsnd2 : (hashval * trm) list -> hashval -> trm option **)

let rec findsnd2 k h =
  match find (fun x -> (=) h (fst x)) k with
  | Some p -> let (_, t) = p in Some t
  | None -> None

(** val findsnd3 :
    ((hashval * stp) * trm option) list -> hashval -> stp option **)

let rec findsnd3 o h =
  match o with
  | [] -> None
  | p :: l ->
    let (p0, _) = p in
    let (x, t) = p0 in if (=) x h then Some t else findsnd3 l h

(** val findthird :
    ((hashval * stp) * trm option) list -> hashval -> trm -> trm **)

let rec findthird k h def =
  match find (fun x -> (=) h (first x)) k with
  | Some p ->
    let (_, o) = p in
    (match o with
     | Some t -> t
     | None -> def)
  | None -> def

exception NotEta
exception RedexMax

(** val uptrm : trm -> int -> int -> trm **)

let rec uptrm t i j =
  match t with
  | DB k -> if (<) k i then DB k else let k2 = ((+) k j) in if k2 < 0 then raise NotEta else DB k2
  | Ap (t1, t2) -> Ap ((uptrm t1 i j), (uptrm t2 i j))
  | Lam (a1, t1) -> Lam (a1, (uptrm t1 ((+) i 1) j))
  | Imp (t1, t2) -> Imp ((uptrm t1 i j), (uptrm t2 i j))
  | All (b, t1) -> All (b, (uptrm t1 ((+) i 1) j))
  | Ex (b, t1) -> Ex (b, (uptrm t1 ((+) i 1) j))
  | Eq (b, t1, t2) -> Eq (b, (uptrm t1 i j), (uptrm t2 i j))
  | _ -> t

(** val subst_trmtrm : trm -> int -> trm -> trm **)

let rec subst_trmtrm t j s =
  match t with
  | DB k ->
    if (=) k j then uptrm s 0 j else if (<) j k then DB ((-) k 1) else DB k
  | Ap (t1, t2) -> Ap ((subst_trmtrm t1 j s), (subst_trmtrm t2 j s))
  | Lam (a1, t1) -> Lam (a1, (subst_trmtrm t1 ((+) j 1) s))
  | Imp (t1, t2) -> Imp ((subst_trmtrm t1 j s), (subst_trmtrm t2 j s))
  | All (b, t1) -> All (b, (subst_trmtrm t1 ((+) j 1) s))
  | Ex (b, t1) -> Ex (b, (subst_trmtrm t1 ((+) j 1) s))
  | Eq (b, t1, t2) -> Eq (b, (subst_trmtrm t1 j s), (subst_trmtrm t2 j s))
  | _ -> t

(** val free_trm_trm : trm -> int -> bool **)

let rec free_trm_trm t i =
  match t with
  | DB j -> (=) i j
  | Ap (m1, m2) -> (||) (free_trm_trm m1 i) (free_trm_trm m2 i)
  | Lam (_, m1) -> free_trm_trm m1 ((+) i 1)
  | Imp (m1, m2) -> (||) (free_trm_trm m1 i) (free_trm_trm m2 i)
  | All (_, m1) -> free_trm_trm m1 ((+) i 1)
  | Ex (_, m1) -> free_trm_trm m1 ((+) i 1)
  | Eq (_, m1, m2) -> (||) (free_trm_trm m1 i) (free_trm_trm m2 i)
  | _ -> false

(** val beta_eta_norm1 : trm -> trm * bool **)

let rec beta_eta_norm1 t1 = match t1 with
| Ap (m1, m2) ->
  let (t2, r1) = beta_eta_norm1 m1 in
  let (t3, r2) = beta_eta_norm1 m2 in
  (match t2 with
   | Lam (_, m) -> ((subst_trmtrm m 0 t3), false)
   | _ -> ((Ap (t2, t3)), ((&&) r1 r2)))
| Lam (a, m1) ->
  let (t2, r1) = beta_eta_norm1 m1 in
  (match t2 with
   | Ap (m, t) ->
     (match t with
      | DB z when z = 0 ->
	  begin
	    try
	      (uptrm m 0 ((~-) 1), false)
	    with NotEta ->
	      ((Lam (a, t2)), r1)
	  end
      | _ -> ((Lam (a, t2)), r1))
   | _ -> ((Lam (a, t2)), r1))
| Imp (m1, m2) ->
  let (t2, r1) = beta_eta_norm1 m1 in
  let (t3, r2) = beta_eta_norm1 m2 in ((Imp (t2, t3)), ((&&) r1 r2))
| All (a, m1) -> let (t2, r1) = beta_eta_norm1 m1 in ((All (a, t2)), r1)
| Ex (a, m1) ->
   let (n1, _) = beta_eta_norm1 m1 in
   let n1b = uptrm n1 1 1 in
   (All(Prop,Imp(All(a,Imp(n1b,DB(1))),DB(0))),false)
| Eq (a, m1, m2) ->
   let (n1, _) = beta_eta_norm1 m1 in
   let (n2, _) = beta_eta_norm1 m2 in
   let n1b = uptrm n1 0 1 in
   let n2b = uptrm n2 0 1 in
   (All(TpArr(a,TpArr(a,Prop)),Imp(Ap(Ap(DB(0),n1b),n2b),Ap(Ap(DB(0),n2b),n1b))), false)
| _ -> (t1, true)

(** val beta_eta_norm : trm -> int -> trm * bool **)

let rec beta_eta_norm t1 count =
  if count > 0 then
    let (s, b) = beta_eta_norm1 t1 in
    if b then
      (s, true)
    else
      beta_eta_norm s (count-1)
  else
    raise RedexMax

(** val is_norm : trm -> bool **)

let rec is_norm = function
| Ap (m1, m2) ->
  (match m1 with
   | Lam (_, _) -> false
   | _ -> (&&) (is_norm m1) (is_norm m2))
| Lam (_, m1) ->
  (match m1 with
   | Ap (f, t) ->
     (match t with
      | DB z ->
        ((fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
           (fun _ ->
           free_trm_trm f 0)
           (fun _ ->
           is_norm m1)
           (fun _ ->
           is_norm m1)
           z)
      | _ -> is_norm m1)
   | _ -> is_norm m1)
| Imp (m1, m2) -> (&&) (is_norm m1) (is_norm m2)
| All (_, m1) -> is_norm m1
| Ex (_, _) -> false
| Eq (_, _, _) -> false
| _ -> true

(** val delta_norm1 : trm -> gsign -> trm **)

let rec delta_norm1 t1 sg =
  match t1 with
  | TmH h -> findthird (fst sg) h t1
  | Ap (m1, m2) -> Ap ((delta_norm1 m1 sg), (delta_norm1 m2 sg))
  | Lam (a, m1) -> Lam (a, (delta_norm1 m1 sg))
  | Imp (m1, m2) -> Imp ((delta_norm1 m1 sg), (delta_norm1 m2 sg))
  | All (a, m1) -> All (a, (delta_norm1 m1 sg))
  | Ex (a, m1) -> Ex (a, (delta_norm1 m1 sg))
  | Eq (a, m1, m2) -> Eq (a, (delta_norm1 m1 sg), (delta_norm1 m2 sg))
  | _ -> t1

(** val delta_norm : trm -> gsign -> int -> trm **)

let rec delta_norm t1 sg count =
  if count > 0 then
    let t2 = delta_norm1 t1 sg in
    if t2 = t1 then
      t2
    else
      delta_norm t2 sg (count-1)
  else
    raise RedexMax

(** val beta_eta_delta_norm : trm -> gsign -> trm option **)

let rec beta_eta_delta_norm t sg =
  try
    let (s, res) =
      beta_eta_norm (delta_norm t sg redex_amount) redex_amount
    in
    if res then Some s else None
  with RedexMax -> None

(** val get_stp_trm :
    stp list -> gsign -> trm -> stp list -> stp option **)

let rec get_stp_trm ctx sgn t thy =
  match t with
  | DB i -> nth_error ctx i
  | TmH h -> findsnd3 (fst sgn) h
  | Prim i -> nth_error thy i
  | Ap (t1, t2) ->
    (match get_stp_trm ctx sgn t1 thy with
     | Some s ->
       (match s with
        | TpArr (b, alpha) ->
          (match get_stp_trm ctx sgn t2 thy with
           | Some b2 -> if (=) b2 b then Some alpha else None
           | None -> None)
        | _ -> None)
     | None -> None)
  | Lam (a1, t1) ->
    (match get_stp_trm (a1 :: ctx) sgn t1 thy with
          | Some b -> Some (TpArr (a1, b))
          | None -> None)
  | Imp (t1, t2) ->
    let a = get_stp_trm ctx sgn t1 thy in
    let b = get_stp_trm ctx sgn t2 thy in
    (match a with
     | Some s ->
       (match s with
        | Prop ->
          (match b with
           | Some s0 ->
             (match s0 with
              | Prop -> Some Prop
              | _ -> None)
           | None -> None)
        | _ -> None)
     | None -> None)
  | All (b, t1) ->
    (match get_stp_trm (b :: ctx) sgn t1 thy with
          | Some s ->
            (match s with
             | Prop -> Some Prop
             | _ -> None)
          | None -> None)
  | Ex (b, t1) ->
    (match get_stp_trm (b :: ctx) sgn t1 thy with
          | Some s ->
            (match s with
             | Prop -> Some Prop
             | _ -> None)
          | None -> None)
  | Eq (b, t1, t2) ->
    (match get_stp_trm ctx sgn t1 thy with
     | Some b1 when b1 = b ->
       (match get_stp_trm ctx sgn t2 thy with
        | Some b2 when b2 = b -> Some Prop
        | _ -> None)
     | _ -> None)

(** val correct_trm :
    stp list -> gsign -> trm -> stp -> stp list -> bool **)

let correct_trm ctx sgn t alpha thy =
  match get_stp_trm ctx sgn t thy with
  | Some b -> if (=) b alpha then true else false
  | None -> false

(** val get_prop_pf :
    stp list -> trm list -> gsign -> pf -> stp list -> trm option **)

let rec get_prop_pf ctx phi sg p thy =
  match p with
  | Known h ->
    (match findsnd2 (snd sg) h with
     | Some s -> beta_eta_delta_norm s sg
     | None -> None)
  | Hyp i -> nth_error phi ( i)
  | PrAp (p1, p2) ->
    (match get_prop_pf ctx phi sg p1 thy with
     | Some t ->
       (match t with
        | Imp (t1, t2) ->
          (match get_prop_pf ctx phi sg p2 thy with
           | Some m -> if (=) t1 m then Some t2 else None
           | None -> None)
        | _ -> None)
     | None -> None)
  | TmAp (p1, t1) ->
    (match get_prop_pf ctx phi sg p1 thy with
     | Some t ->
       (match t with
        | All (a, m) ->
          (match get_stp_trm ctx sg t1 thy with
           | Some b ->
             if (=) a b
             then beta_eta_delta_norm
                    (subst_trmtrm m 0
                      (delta_norm t1 sg redex_amount)) sg
             else None
           | None -> None)
        | _ -> None)
     | None -> None)
  | PrLa (s, p1) ->
    if not (correct_trm ctx sg s Prop thy)
    then None
    else (match beta_eta_delta_norm s sg with
          | Some q ->
            (match get_prop_pf ctx (q :: phi) sg p1 thy with
             | Some q2 -> Some (Imp (q, q2))
             | None -> None)
          | None -> None)
  | TmLa (a1, p1) ->
     (match get_prop_pf (a1 :: ctx)
                  (List.map (fun x -> uptrm x 0 1) phi) sg p1 thy with
          | Some m -> Some (All (a1, m))
          | None -> None)
  | Ext (a, b) ->
     let (extpropab,nrm) = beta_eta_norm (All(TpArr(a,b),All(TpArr(a,b),Imp(All(a,Eq(b,Ap(DB(2),DB(0)),Ap(DB(1),DB(0)))),Eq(TpArr(a,b),DB(1),DB(0)))))) 10 in
     if nrm then
       Some(extpropab)
     else
       None

(** val correct_pf :
    stp list -> trm list -> gsign -> pf -> trm -> stp list -> bool **)

let correct_pf ctx phi sg p t thy =
  match get_prop_pf ctx phi sg p thy with
  | Some l -> if (=) l t then true else false
  | None -> false

(** val check_theoryspec : theoryspec -> (theory * gsign) option **)

let rec check_theoryspec = function
| [] -> Some (([], []), ([], []))
| t0 :: tr ->
  (match t0 with
   | Thyprim tp ->
     (match check_theoryspec tr with
      | Some p ->
        let (t1, c) = p in
        let (a, b) = t1 in
        Some ((((@) a (tp :: [])), b), c)
      | None -> None)
   | Thyaxiom m ->
     (match check_theoryspec tr with
      | Some p ->
        let (t1, g) = p in
        let (a, b) = t1 in
        let (c, d) = g in
        if is_norm m
        then if correct_trm [] (c, d) m Prop a
             then let h = tm_hashroot m in
                  Some ((a, (h :: b)), (c, ((h, m) :: d)))
             else None
        else None
      | None -> None)
   | Thydef (tp, m) ->
     (match check_theoryspec tr with
      | Some p ->
        let (t1, g) = p in
        let (a, b) = t1 in
        let (c, d) = g in
        if is_norm m
        then if correct_trm [] (c, d) m tp a
             then let h = tm_hashroot m in
                  Some ((a, b), ((((h, tp), (Some m)) :: c), d))
             else None
        else None
      | None -> None))

(** val tp_of_tmh :
    ((hashval * stp) * trm option) list -> hashval -> stp option **)

let rec tp_of_tmh tpl h =
  match tpl with
  | [] -> None
  | p :: tpr ->
    let (p0, _) = p in
    let (k, a) = p0 in if (=) h k then Some a else tp_of_tmh tpr h

(** val tm_tp :
    (hashval option -> hashval -> stp -> bool) -> gsign -> hashval option ->
    hashval -> stp -> bool **)

let rec tm_tp gvtp sg th h a =
  let (tpl, _) = sg in
  (match tp_of_tmh tpl h with
   | Some b -> if (=) a b then true else false
   | None -> gvtp th h a)

(** val prop_of_known : (hashval * trm) list -> hashval -> trm option **)

let rec prop_of_known kl h =
  match kl with
  | [] -> None
  | p0 :: kr ->
    let (k, p) = p0 in if (=) k h then Some p else prop_of_known kr h

(** val known :
    (hashval option -> hashval -> bool) -> gsign -> hashval option -> hashval
    -> bool **)

let rec known gvkn sg th k =
  match prop_of_known (snd sg) k with
  | Some _ -> true
  | None -> gvkn th k

(** val check_signaspec :
    (hashval option -> hashval -> stp -> bool) -> (hashval option -> hashval
    -> bool) -> hashval option -> theory -> stree option -> signaspec ->
    (gsign * hashval list) option **)

let rec check_signaspec gvtp gvkn th t tr = function
| [] -> Some (([], []), [])
| s0 :: dr ->
  (match s0 with
   | Signasigna h ->
     (match check_signaspec gvtp gvkn th t tr dr with
      | Some p ->
        let (sg, imported) = p in
        (match tr with
         | Some str -> import_signatures th str (h :: []) sg imported
         | None -> None)
      | None -> None)
   | Signaparam (h, a) ->
     (match check_signaspec gvtp gvkn th t tr dr with
      | Some p ->
        let (g, imported) = p in
        let (tmtpl, kl) = g in
        if tm_tp gvtp (tmtpl, kl) th h a
        then Some (((((h, a), None) :: tmtpl), kl), imported)
        else None
      | None -> None)
   | Signadef (a, m) ->
     (match check_signaspec gvtp gvkn th t tr dr with
      | Some p ->
        let (g, imported) = p in
        let (tmtpl, kl) = g in
        if is_norm m
        then if correct_trm [] (tmtpl, kl) m a (fst t)
             then let h = tm_hashroot m in
                  (match m with
                   | TmH _ -> Some ((tmtpl, kl), imported)
                   | _ ->
                     Some (((((h, a), (Some m)) :: tmtpl), kl), imported))
             else None
        else None
      | None -> None)
   | Signaknown p ->
     (match check_signaspec gvtp gvkn th t tr dr with
      | Some p0 ->
        let (g, imported) = p0 in
        let (tmtpl, kl) = g in
        if is_norm p
        then if correct_trm [] (tmtpl, kl) p Prop (fst t)
             then let k = tm_hashroot p in
                  let (_, akl) = t in
                  if (||) (List.exists (fun x -> (=) x k) akl)
                       (known gvkn (tmtpl, kl) th k)
                  then Some ((tmtpl, ((k, p) :: kl)), imported)
                  else None
             else None
        else None
      | None -> None))

(** val check_doc :
    (hashval option -> hashval -> stp -> bool) -> (hashval option -> hashval
    -> bool) -> hashval option -> theory -> stree option -> doc ->
    (gsign * hashval list) option **)

let rec check_doc gvtp gvkn th thy str = function
| [] -> Some (([], []), [])
| d0 :: dr ->
  (match d0 with
   | Docsigna h ->
     (match check_doc gvtp gvkn th thy str dr with
      | Some p ->
        let (sg, imported) = p in
        (match str with
         | Some tr ->
	     import_signatures th tr (h :: []) sg imported
         | None -> None)
      | None -> None)
   | Docparam (h, a) ->
     (match check_doc gvtp gvkn th thy str dr with
      | Some p ->
        let (g, imported) = p in
        let (tmtpl, kl) = g in
        if tm_tp gvtp (tmtpl, kl) th h a
        then Some (((((h, a), None) :: tmtpl), kl), imported)
        else None
      | None -> None)
   | Docdef (a, m) ->
     (match check_doc gvtp gvkn th thy str dr with
      | Some p ->
        let (g, imported) = p in
        let (tmtpl, kl) = g in
        if is_norm m
        then if correct_trm [] (tmtpl, kl) m a (fst thy)
             then let h = tm_hashroot m in
                  (match m with
                   | TmH _ -> Some ((tmtpl, kl), imported)
                   | _ ->
                     Some (((((h, a), (Some m)) :: tmtpl), kl), imported))
             else None
        else None
      | None -> None)
   | Docknown p ->
     (match check_doc gvtp gvkn th thy str dr with
      | Some p0 ->
        let (g, imported) = p0 in
        let (tmtpl, kl) = g in
        if is_norm p
        then if correct_trm [] (tmtpl, kl) p Prop (fst thy)
             then let k = tm_hashroot p in
                  if (||) (List.exists (fun x -> (=) x k) (snd thy))
                       (known gvkn (tmtpl, kl) th k)
                  then Some ((tmtpl, ((k, p) :: kl)), imported)
                  else None
             else None
        else None
      | None -> None)
   | Docpfof (p, d) ->
     (match check_doc gvtp gvkn th thy str dr with
      | Some p0 ->
        let (g, imported) = p0 in
        let (tmtpl, kl) = g in
        if is_norm p
        then let k = tm_hashroot p in
             if correct_trm [] (tmtpl, kl) p Prop (fst thy)
             then (match beta_eta_delta_norm p (tmtpl, kl) with
                   | Some p2 ->
                     if correct_pf [] [] (tmtpl, kl) d p2 (fst thy)
                     then Some ((tmtpl, ((k, p) :: kl)), imported)
                     else None
                   | None -> None)
             else None
        else None
      | None -> None)
   | Docconj p ->
     (match check_doc gvtp gvkn th thy str dr with
      | Some p0 ->
        let (sgn, imported) = p0 in
        if is_norm p
        then if correct_trm [] sgn p Prop (fst thy)
             then Some (sgn, imported)
             else None
        else None
      | None -> None))

(** built in hf set theory for pow style bounties **)
let hfprimnamesa = [| "Eps_i";
"False";
"True";
"not";
"and";
"or";
"iff";
"In";
"Subq";
"Empty";
"Union";
"Power";
"Repl";
"TransSet";
"atleast2";
"atleast3";
"atleast4";
"atleast5";
"atleast6";
"exactly2";
"exactly3";
"exactly4";
"exactly5";
"exu_i";
"reflexive_i";
"irreflexive_i";
"symmetric_i";
"antisymmetric_i";
"transitive_i";
"eqreln_i";
"per_i";
"linear_i";
"trichotomous_or_i";
"partialorder_i";
"totalorder_i";
"strictpartialorder_i";
"stricttotalorder_i";
"If_i";
"exactly1of2";
"exactly1of3";
"nIn";
"nSubq";
"UPair";
"Sing";
"binunion";
"SetAdjoin";
"famunion";
"Sep";
"ReplSep";
"binintersect";
"setminus";
"inj";
"bij";
"atleastp";
"equip";
"In_rec_poly_G_i";
"In_rec_poly_i";
"ordsucc";
"nat_p";
"nat_primrec";
"add_nat";
"mul_nat";
"ordinal";
"V_";
"Inj1";
"Inj0";
"Unj";
"combine_funcs";
"setsum";
"proj0";
"proj1";
"binrep";
"lam";
"setprod";
"ap";
"setsum_p";
"tuple_p";
"Pi";
"setexp";
"Sep2";
"set_of_pairs";
"lam2";
"PNoEq_";
"PNoLt_";
"PNoLt";
"PNoLe";
"PNo_downc";
"PNo_upc";
"SNoElts_";
"SNo_";
"PSNo";
"SNo";
"SNoLev";
"SNoEq_";
"SNoLt";
"SNoLe";
"binop_on";
"Loop";
"Loop_with_defs";
"Loop_with_defs_cex1";
"Loop_with_defs_cex2";
"combinator";
"combinator_equiv";
"equip_mod" |]

let rec hf_stp_str a p =
  match a with
  | TpArr(a1,a2) ->
     Printf.sprintf "%s%s -> %s%s"
       (if p then "(" else "")
       (hf_stp_str a1 true)
       (hf_stp_str a2 false)
       (if p then ")" else "")
  | Prop -> "o"
  | Base(j) when j = 0 -> "i"
  | Base(j) -> Printf.sprintf "_%d" j

let rec hf_trm_str m vl =
  match m with
  | Prim(i) -> hfprimnamesa.(i)
  | DB(i) -> (try List.nth vl i with _ -> Printf.sprintf "?%d" (i - List.length vl))
  | Ap(_,_) -> Printf.sprintf "(%s)" (hf_spine_str m vl)
  | Lam(_,m1) ->
     let x = Printf.sprintf "X%d" (List.length vl) in
     Printf.sprintf "(fun %s => %s)" x (hf_trm_str m1 (x::vl))
  | Imp(m1,m2) -> Printf.sprintf "(%s -> %s)" (hf_trm_str m1 vl) (hf_trm_str m2 vl)
  | All(a,m1) when a = Base(0) ->
     let x = Printf.sprintf "X%d" (List.length vl) in
     Printf.sprintf "(forall %s, %s)" x (hf_trm_str m1 (x::vl))
  | All(a,m1) ->
     let x = Printf.sprintf "X%d" (List.length vl) in
     Printf.sprintf "(forall %s:%s, %s)" x (hf_stp_str a false) (hf_trm_str m1 (x::vl))
  | Ex(a,m1) when a = Base(0) ->
     let x = Printf.sprintf "X%d" (List.length vl) in
     Printf.sprintf "(exists %s, %s)" x (hf_trm_str m1 (x::vl))
  | Ex(a,m1) ->
     let x = Printf.sprintf "X%d" (List.length vl) in
     Printf.sprintf "(exists %s:%s, %s)" x (hf_stp_str a false) (hf_trm_str m1 (x::vl))
  | Eq(_,m1,m2) ->
     Printf.sprintf "(%s = %s)" (hf_trm_str m1 vl) (hf_trm_str m2 vl)
  | _ -> "*"
and hf_spine_str m vl =
  match m with
  | Ap(m1,m2) -> Printf.sprintf "%s %s" (hf_spine_str m1 vl) (hf_trm_str m2 vl)
  | _ -> hf_trm_str m vl

let rec aim_trm_str m vl =
  match m with
  | Prim(i) -> hfprimnamesa.(i)
  | DB(i) -> (try List.nth vl i with _ -> Printf.sprintf "?%d" (i - List.length vl))
  | Ap(_,_) -> Printf.sprintf "(%s)" (hf_spine_str m vl)
  | Lam(_,m1) ->
     let x = Printf.sprintf "X%d" (List.length vl) in
     Printf.sprintf "(fun %s => %s)" x (aim_trm_str m1 (x::vl))
  | Imp(m1,m2) -> Printf.sprintf "(%s\n -> %s)" (aim_trm_str m1 vl) (aim_trm_str m2 vl)
  | All(_,m1) ->
     let x =
       try
         List.nth ["X";"m";"b";"s";"e";"K";"a";"T";"L";"R";"I1";"J1";"I2";"J2"] (List.length vl)
       with _ ->
         Printf.sprintf "X%d" (List.length vl)
     in
     Printf.sprintf "(forall %s, %s)" x (aim_trm_str m1 (x::vl))
  | Ex(_,m1) ->
     let x = Printf.sprintf "X%d" (List.length vl) in
     Printf.sprintf "(exists %s, %s)" x (aim_trm_str m1 (x::vl))
  | Eq(_,m1,m2) ->
     Printf.sprintf "(%s = %s)" (aim_trm_str m1 vl) (aim_trm_str m2 vl)
  | _ -> "*"

let rec comb_trm_str m vl =
  match m with
  | Prim(i) -> hfprimnamesa.(i)
  | DB(i) -> (try List.nth vl i with _ -> Printf.sprintf "?%d" (i - List.length vl))
  | Ap(Prim(inj0),Prim(empty)) when inj0 = 65 && empty = 9 -> "K"
  | Ap(Prim(inj0),Ap(Prim(power),Prim(empty))) when inj0 = 65 && empty = 9 && power = 11 -> "S"
  | Ap(Prim(inj1),Ap(Ap(Prim(setsum),m1),m2)) when inj1 = 64 && setsum = 68 ->
     Printf.sprintf "(%s @ %s)" (comb_trm_str m1 vl) (comb_trm_str m2 vl)
  | Ap(Ap(Prim(combeq),m1),m2) when combeq = 102 ->
     Printf.sprintf "(%s == %s)" (comb_trm_str m1 vl) (comb_trm_str m2 vl)
  | Ap(_,_) -> Printf.sprintf "(%s)" (comb_spine_str m vl)
  | Lam(_,m1) ->
     let x = Printf.sprintf "X%d" (List.length vl) in
     Printf.sprintf "(fun %s => %s)" x (comb_trm_str m1 (x::vl))
  | Imp(m1,m2) -> Printf.sprintf "(%s\n -> %s)" (comb_trm_str m1 vl) (comb_trm_str m2 vl)
  | All(_,m1) ->
     let x = Printf.sprintf "X%d" (List.length vl) in
     Printf.sprintf "(forall %s, %s)" x (comb_trm_str m1 (x::vl))
  | Ex(_,m1) ->
     let x = Printf.sprintf "X%d" (List.length vl) in
     Printf.sprintf "(exists %s, %s)" x (comb_trm_str m1 (x::vl))
  | Eq(_,m1,m2) ->
     Printf.sprintf "(%s = %s)" (comb_trm_str m1 vl) (comb_trm_str m2 vl)
  | _ -> "*"
and comb_spine_str m vl =
  match m with
  | Ap(m1,m2) -> Printf.sprintf "%s %s" (comb_spine_str m1 vl) (comb_trm_str m2 vl)
  | _ -> comb_trm_str m vl
           
let hfprimtps = [TpArr(TpArr(Base(0),Prop),Base(0));
Prop;
Prop;
TpArr(Prop,Prop);
TpArr(Prop,TpArr(Prop,Prop));
TpArr(Prop,TpArr(Prop,Prop));
TpArr(Prop,TpArr(Prop,Prop));
TpArr(Base(0),TpArr(Base(0),Prop));
TpArr(Base(0),TpArr(Base(0),Prop));
Base(0);
TpArr(Base(0),Base(0));
TpArr(Base(0),Base(0));
TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),Base(0)));
TpArr(Base(0),Prop);
TpArr(Base(0),Prop);
TpArr(Base(0),Prop);
TpArr(Base(0),Prop);
TpArr(Base(0),Prop);
TpArr(Base(0),Prop);
TpArr(Base(0),Prop);
TpArr(Base(0),Prop);
TpArr(Base(0),Prop);
TpArr(Base(0),Prop);
TpArr(TpArr(Base(0),Prop),Prop);
TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop);
TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop);
TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop);
TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop);
TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop);
TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop);
TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop);
TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop);
TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop);
TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop);
TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop);
TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop);
TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop);
TpArr(Prop,TpArr(Base(0),TpArr(Base(0),Base(0))));
TpArr(Prop,TpArr(Prop,Prop));
TpArr(Prop,TpArr(Prop,TpArr(Prop,Prop)));
TpArr(Base(0),TpArr(Base(0),Prop));
TpArr(Base(0),TpArr(Base(0),Prop));
TpArr(Base(0),TpArr(Base(0),Base(0)));
TpArr(Base(0),Base(0));
TpArr(Base(0),TpArr(Base(0),Base(0)));
TpArr(Base(0),TpArr(Base(0),Base(0)));
TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),Base(0)));
TpArr(Base(0),TpArr(TpArr(Base(0),Prop),Base(0)));
TpArr(Base(0),TpArr(TpArr(Base(0),Prop),TpArr(TpArr(Base(0),Base(0)),Base(0))));
TpArr(Base(0),TpArr(Base(0),Base(0)));
TpArr(Base(0),TpArr(Base(0),Base(0)));
TpArr(Base(0),TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),Prop)));
TpArr(Base(0),TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),Prop)));
TpArr(Base(0),TpArr(Base(0),Prop));
TpArr(Base(0),TpArr(Base(0),Prop));
TpArr(TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),Base(0))),TpArr(Base(0),TpArr(Base(0),Prop)));
TpArr(TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),Base(0))),TpArr(Base(0),Base(0)));
TpArr(Base(0),Base(0));
TpArr(Base(0),Prop);
TpArr(Base(0),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(Base(0),Base(0))));
TpArr(Base(0),TpArr(Base(0),Base(0)));
TpArr(Base(0),TpArr(Base(0),Base(0)));
TpArr(Base(0),Prop);
TpArr(Base(0),Base(0));
TpArr(Base(0),Base(0));
TpArr(Base(0),Base(0));
TpArr(Base(0),Base(0));
TpArr(Base(0),TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),TpArr(TpArr(Base(0),Base(0)),TpArr(Base(0),Base(0))))));
TpArr(Base(0),TpArr(Base(0),Base(0)));
TpArr(Base(0),Base(0));
TpArr(Base(0),Base(0));
TpArr(Base(0),TpArr(Base(0),Base(0)));
TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),Base(0)));
TpArr(Base(0),TpArr(Base(0),Base(0)));
TpArr(Base(0),TpArr(Base(0),Base(0)));
TpArr(Base(0),Prop);
TpArr(Base(0),TpArr(Base(0),Prop));
TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),Base(0)));
TpArr(Base(0),TpArr(Base(0),Base(0)));
TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Base(0))));
TpArr(Base(0),Prop);
TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),Base(0))));
TpArr(Base(0),TpArr(TpArr(Base(0),Prop),TpArr(TpArr(Base(0),Prop),Prop)));
TpArr(Base(0),TpArr(TpArr(Base(0),Prop),TpArr(TpArr(Base(0),Prop),Prop)));
TpArr(Base(0),TpArr(TpArr(Base(0),Prop),TpArr(Base(0),TpArr(TpArr(Base(0),Prop),Prop))));
TpArr(Base(0),TpArr(TpArr(Base(0),Prop),TpArr(Base(0),TpArr(TpArr(Base(0),Prop),Prop))));
TpArr(TpArr(Base(0),TpArr(TpArr(Base(0),Prop),Prop)),TpArr(Base(0),TpArr(TpArr(Base(0),Prop),Prop)));
TpArr(TpArr(Base(0),TpArr(TpArr(Base(0),Prop),Prop)),TpArr(Base(0),TpArr(TpArr(Base(0),Prop),Prop)));
TpArr(Base(0),Base(0));
TpArr(Base(0),TpArr(Base(0),Prop));
TpArr(Base(0),TpArr(TpArr(Base(0),Prop),Base(0)));
TpArr(Base(0),Prop);
TpArr(Base(0),Base(0));
TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Prop)));
TpArr(Base(0),TpArr(Base(0),Prop));
TpArr(Base(0),TpArr(Base(0),Prop));
TpArr(Base(0),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),Prop));
TpArr(Base(0),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(Base(0),Prop)))));
TpArr(Base(0),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(Base(0),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),TpArr(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),Prop))))))))))))));
TpArr(Base(0),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(Base(0),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),TpArr(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),Prop))))))))))))));
TpArr(Base(0),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(Base(0),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),TpArr(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),Prop))))))))))))));
TpArr(Base(0),Prop);
TpArr(Base(0),TpArr(Base(0),Prop));
TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Prop)))]

let hfaxs = [All(TpArr(Base(0),Prop),All(Base(0),Imp(Ap(DB(1),DB(0)),Ap(DB(1),Ap(Prim(0),DB(1))))));
All(Prop,Imp(Ap(Prim(3),Ap(Prim(3),DB(0))),DB(0)));
All(Prop,All(Prop,Imp(Ap(Ap(Prim(6),DB(1)),DB(0)),Eq(Prop,DB(1),DB(0)))));
All(Base(0),All(Base(0),Imp(Ap(Ap(Prim(8),DB(1)),DB(0)),Imp(Ap(Ap(Prim(8),DB(0)),DB(1)),Eq(Base(0),DB(1),DB(0))))));
Ap(Prim(3),Ex(Base(0),Ap(Ap(Prim(7),DB(0)),Prim(9))));
All(Base(0),All(Base(0),Ap(Ap(Prim(6),Ap(Ap(Prim(7),DB(0)),Ap(Prim(10),DB(1)))),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(7),DB(1)),DB(0))),Ap(Ap(Prim(7),DB(0)),DB(2)))))));
All(Base(0),All(Base(0),Ap(Ap(Prim(6),Ap(Ap(Prim(7),DB(0)),Ap(Prim(11),DB(1)))),Ap(Ap(Prim(8),DB(0)),DB(1)))));
All(Base(0),All(TpArr(Base(0),Base(0)),All(Base(0),Ap(Ap(Prim(6),Ap(Ap(Prim(7),DB(0)),Ap(Ap(Prim(12),DB(2)),Lam(Base(0),Ap(DB(2),DB(0)))))),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(7),DB(0)),DB(3))),Eq(Base(0),DB(1),Ap(DB(2),DB(0)))))))));
All(TpArr(Base(0),Prop),Imp(All(Base(0),Imp(Ap(DB(1),DB(0)),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(1)),Ap(DB(2),DB(0)))))),Imp(Ap(DB(0),Prim(9)),Imp(All(Base(0),Imp(Ap(DB(1),DB(0)),Ap(DB(1),Ap(Prim(10),DB(0))))),Imp(All(Base(0),Imp(Ap(DB(1),DB(0)),Ap(DB(1),Ap(Prim(11),DB(0))))),Imp(All(Base(0),Imp(Ap(DB(1),DB(0)),All(TpArr(Base(0),Base(0)),Imp(All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(2)),Ap(DB(3),Ap(DB(1),DB(0))))),Ap(DB(2),Ap(Ap(Prim(12),DB(1)),Lam(Base(0),Ap(DB(1),DB(0))))))))),All(Base(0),Ap(DB(1),DB(0)))))))));
All(TpArr(Base(0),Prop),Imp(All(Base(0),Imp(All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(1)),Ap(DB(2),DB(0)))),Ap(DB(1),DB(0)))),All(Base(0),Ap(DB(1),DB(0)))));
Eq(Prop,Prim(1),All(Prop,DB(0)));
Eq(Prop,Prim(2),All(Prop,Imp(DB(0),DB(0))));
Eq(TpArr(Prop,Prop),Prim(3),Lam(Prop,Imp(DB(0),Prim(1))));
Eq(TpArr(Prop,TpArr(Prop,Prop)),Prim(4),Lam(Prop,Lam(Prop,All(Prop,Imp(Imp(DB(2),Imp(DB(1),DB(0))),DB(0))))));
Eq(TpArr(Prop,TpArr(Prop,Prop)),Prim(5),Lam(Prop,Lam(Prop,All(Prop,Imp(Imp(DB(2),DB(0)),Imp(Imp(DB(1),DB(0)),DB(0)))))));
Eq(TpArr(Prop,TpArr(Prop,Prop)),Prim(6),Lam(Prop,Lam(Prop,Ap(Ap(Prim(4),Imp(DB(1),DB(0))),Imp(DB(0),DB(1))))));
Eq(TpArr(Base(0),TpArr(Base(0),Prop)),Prim(8),Lam(Base(0),Lam(Base(0),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(2)),Ap(Ap(Prim(7),DB(0)),DB(1)))))));
Eq(TpArr(Base(0),Prop),Prim(13),Lam(Base(0),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(1)),Ap(Ap(Prim(8),DB(0)),DB(1))))));
Eq(TpArr(Base(0),Prop),Prim(14),Lam(Base(0),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(7),DB(0)),DB(1))),Ap(Prim(3),Ap(Ap(Prim(8),DB(1)),Ap(Prim(11),DB(0))))))));
Eq(TpArr(Base(0),Prop),Prim(15),Lam(Base(0),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(8),DB(0)),DB(1))),Ap(Ap(Prim(4),Ap(Prim(3),Ap(Ap(Prim(8),DB(1)),DB(0)))),Ap(Prim(14),DB(0)))))));
Eq(TpArr(Base(0),Prop),Prim(16),Lam(Base(0),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(8),DB(0)),DB(1))),Ap(Ap(Prim(4),Ap(Prim(3),Ap(Ap(Prim(8),DB(1)),DB(0)))),Ap(Prim(15),DB(0)))))));
Eq(TpArr(Base(0),Prop),Prim(17),Lam(Base(0),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(8),DB(0)),DB(1))),Ap(Ap(Prim(4),Ap(Prim(3),Ap(Ap(Prim(8),DB(1)),DB(0)))),Ap(Prim(16),DB(0)))))));
Eq(TpArr(Base(0),Prop),Prim(18),Lam(Base(0),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(8),DB(0)),DB(1))),Ap(Ap(Prim(4),Ap(Prim(3),Ap(Ap(Prim(8),DB(1)),DB(0)))),Ap(Prim(17),DB(0)))))));
Eq(TpArr(Base(0),Prop),Prim(19),Lam(Base(0),Ap(Ap(Prim(4),Ap(Prim(14),DB(0))),Ap(Prim(3),Ap(Prim(15),DB(0))))));
Eq(TpArr(Base(0),Prop),Prim(20),Lam(Base(0),Ap(Ap(Prim(4),Ap(Prim(15),DB(0))),Ap(Prim(3),Ap(Prim(16),DB(0))))));
Eq(TpArr(Base(0),Prop),Prim(21),Lam(Base(0),Ap(Ap(Prim(4),Ap(Prim(16),DB(0))),Ap(Prim(3),Ap(Prim(17),DB(0))))));
Eq(TpArr(Base(0),Prop),Prim(22),Lam(Base(0),Ap(Ap(Prim(4),Ap(Prim(17),DB(0))),Ap(Prim(3),Ap(Prim(18),DB(0))))));
Eq(TpArr(TpArr(Base(0),Prop),Prop),Prim(23),Lam(TpArr(Base(0),Prop),Ap(Ap(Prim(4),Ex(Base(0),Ap(DB(1),DB(0)))),All(Base(0),All(Base(0),Imp(Ap(DB(2),DB(1)),Imp(Ap(DB(2),DB(0)),Eq(Base(0),DB(1),DB(0)))))))));
Eq(TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop),Prim(24),Lam(TpArr(Base(0),TpArr(Base(0),Prop)),All(Base(0),Ap(Ap(DB(1),DB(0)),DB(0)))));
Eq(TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop),Prim(25),Lam(TpArr(Base(0),TpArr(Base(0),Prop)),All(Base(0),Ap(Prim(3),Ap(Ap(DB(1),DB(0)),DB(0))))));
Eq(TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop),Prim(26),Lam(TpArr(Base(0),TpArr(Base(0),Prop)),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(2),DB(1)),DB(0)),Ap(Ap(DB(2),DB(0)),DB(1)))))));
Eq(TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop),Prim(27),Lam(TpArr(Base(0),TpArr(Base(0),Prop)),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(2),DB(1)),DB(0)),Imp(Ap(Ap(DB(2),DB(0)),DB(1)),Eq(Base(0),DB(1),DB(0))))))));
Eq(TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop),Prim(28),Lam(TpArr(Base(0),TpArr(Base(0),Prop)),All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(3),DB(2)),DB(1)),Imp(Ap(Ap(DB(3),DB(1)),DB(0)),Ap(Ap(DB(3),DB(2)),DB(0)))))))));
Eq(TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop),Prim(29),Lam(TpArr(Base(0),TpArr(Base(0),Prop)),Ap(Ap(Prim(4),Ap(Ap(Prim(4),Ap(Prim(24),DB(0))),Ap(Prim(26),DB(0)))),Ap(Prim(28),DB(0)))));
Eq(TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop),Prim(30),Lam(TpArr(Base(0),TpArr(Base(0),Prop)),Ap(Ap(Prim(4),Ap(Prim(26),DB(0))),Ap(Prim(28),DB(0)))));
Eq(TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop),Prim(31),Lam(TpArr(Base(0),TpArr(Base(0),Prop)),All(Base(0),All(Base(0),Ap(Ap(Prim(5),Ap(Ap(DB(2),DB(1)),DB(0))),Ap(Ap(DB(2),DB(0)),DB(1)))))));
Eq(TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop),Prim(32),Lam(TpArr(Base(0),TpArr(Base(0),Prop)),All(Base(0),All(Base(0),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(DB(2),DB(1)),DB(0))),Eq(Base(0),DB(1),DB(0)))),Ap(Ap(DB(2),DB(0)),DB(1)))))));
Eq(TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop),Prim(33),Lam(TpArr(Base(0),TpArr(Base(0),Prop)),Ap(Ap(Prim(4),Ap(Ap(Prim(4),Ap(Prim(24),DB(0))),Ap(Prim(27),DB(0)))),Ap(Prim(28),DB(0)))));
Eq(TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop),Prim(34),Lam(TpArr(Base(0),TpArr(Base(0),Prop)),Ap(Ap(Prim(4),Ap(Prim(33),DB(0))),Ap(Prim(31),DB(0)))));
Eq(TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop),Prim(35),Lam(TpArr(Base(0),TpArr(Base(0),Prop)),Ap(Ap(Prim(4),Ap(Prim(25),DB(0))),Ap(Prim(28),DB(0)))));
Eq(TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Prop),Prim(36),Lam(TpArr(Base(0),TpArr(Base(0),Prop)),Ap(Ap(Prim(4),Ap(Prim(35),DB(0))),Ap(Prim(32),DB(0)))));
Eq(TpArr(Prop,TpArr(Base(0),TpArr(Base(0),Base(0)))),Prim(37),Lam(Prop,Lam(Base(0),Lam(Base(0),Ap(Prim(0),Lam(Base(0),Ap(Ap(Prim(5),Ap(Ap(Prim(4),DB(3)),Eq(Base(0),DB(0),DB(2)))),Ap(Ap(Prim(4),Ap(Prim(3),DB(3))),Eq(Base(0),DB(0),DB(1))))))))));
Eq(TpArr(Prop,TpArr(Prop,Prop)),Prim(38),Lam(Prop,Lam(Prop,Ap(Ap(Prim(5),Ap(Ap(Prim(4),DB(1)),Ap(Prim(3),DB(0)))),Ap(Ap(Prim(4),Ap(Prim(3),DB(1))),DB(0))))));
Eq(TpArr(Prop,TpArr(Prop,TpArr(Prop,Prop))),Prim(39),Lam(Prop,Lam(Prop,Lam(Prop,Ap(Ap(Prim(5),Ap(Ap(Prim(4),Ap(Ap(Prim(38),DB(2)),DB(1))),Ap(Prim(3),DB(0)))),Ap(Ap(Prim(4),Ap(Ap(Prim(4),Ap(Prim(3),DB(2))),Ap(Prim(3),DB(1)))),DB(0)))))));
Eq(TpArr(Base(0),TpArr(Base(0),Prop)),Prim(40),Lam(Base(0),Lam(Base(0),Ap(Prim(3),Ap(Ap(Prim(7),DB(1)),DB(0))))));
Eq(TpArr(Base(0),TpArr(Base(0),Prop)),Prim(41),Lam(Base(0),Lam(Base(0),Ap(Prim(3),Ap(Ap(Prim(8),DB(1)),DB(0))))));
Eq(TpArr(Base(0),TpArr(Base(0),Base(0))),Prim(42),Lam(Base(0),Lam(Base(0),Ap(Ap(Prim(12),Ap(Prim(11),Ap(Prim(11),Prim(9)))),Lam(Base(0),Ap(Ap(Ap(Prim(37),Ap(Ap(Prim(7),Prim(9)),DB(0))),DB(2)),DB(1)))))));
Eq(TpArr(Base(0),Base(0)),Prim(43),Lam(Base(0),Ap(Ap(Prim(42),DB(0)),DB(0))));
Eq(TpArr(Base(0),TpArr(Base(0),Base(0))),Prim(44),Lam(Base(0),Lam(Base(0),Ap(Prim(10),Ap(Ap(Prim(42),DB(1)),DB(0))))));
Eq(TpArr(Base(0),TpArr(Base(0),Base(0))),Prim(45),Lam(Base(0),Lam(Base(0),Ap(Ap(Prim(44),DB(1)),Ap(Prim(43),DB(0))))));
Eq(TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),Base(0))),Prim(46),Lam(Base(0),Lam(TpArr(Base(0),Base(0)),Ap(Prim(10),Ap(Ap(Prim(12),DB(1)),Lam(Base(0),Ap(DB(1),DB(0))))))));
Eq(TpArr(Base(0),TpArr(TpArr(Base(0),Prop),Base(0))),Prim(47),Lam(Base(0),Lam(TpArr(Base(0),Prop),Ap(Ap(Ap(Prim(37),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(7),DB(0)),DB(2))),Ap(DB(1),DB(0))))),Ap(Ap(Prim(12),DB(1)),Lam(Base(0),Ap(Lam(Base(0),Ap(Ap(Ap(Prim(37),Ap(DB(2),DB(0))),DB(0)),Ap(Prim(0),Lam(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(7),DB(0)),DB(4))),Ap(DB(3),DB(0))))))),DB(0))))),Prim(9)))));
Eq(TpArr(Base(0),TpArr(TpArr(Base(0),Prop),TpArr(TpArr(Base(0),Base(0)),Base(0)))),Prim(48),Lam(Base(0),Lam(TpArr(Base(0),Prop),Lam(TpArr(Base(0),Base(0)),Ap(Ap(Prim(12),Ap(Ap(Prim(47),DB(2)),Lam(Base(0),Ap(DB(2),DB(0))))),Lam(Base(0),Ap(DB(1),DB(0))))))));
Eq(TpArr(Base(0),TpArr(Base(0),Base(0))),Prim(49),Lam(Base(0),Lam(Base(0),Ap(Ap(Prim(47),DB(1)),Lam(Base(0),Ap(Ap(Prim(7),DB(0)),DB(1)))))));
Eq(TpArr(Base(0),TpArr(Base(0),Base(0))),Prim(50),Lam(Base(0),Lam(Base(0),Ap(Ap(Prim(47),DB(1)),Lam(Base(0),Ap(Ap(Prim(40),DB(0)),DB(1)))))));
Eq(TpArr(Base(0),TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),Prop))),Prim(51),Lam(Base(0),Lam(Base(0),Lam(TpArr(Base(0),Base(0)),Ap(Ap(Prim(4),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(3)),Ap(Ap(Prim(7),Ap(DB(1),DB(0))),DB(2))))),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(3)),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(4)),Imp(Eq(Base(0),Ap(DB(2),DB(1)),Ap(DB(2),DB(0))),Eq(Base(0),DB(1),DB(0))))))))))));
Eq(TpArr(Base(0),TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),Prop))),Prim(52),Lam(Base(0),Lam(Base(0),Lam(TpArr(Base(0),Base(0)),Ap(Ap(Prim(4),Ap(Ap(Ap(Prim(51),DB(2)),DB(1)),DB(0))),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(2)),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(7),DB(0)),DB(4))),Eq(Base(0),Ap(DB(2),DB(0)),DB(1)))))))))));
Eq(TpArr(Base(0),TpArr(Base(0),Prop)),Prim(53),Lam(Base(0),Lam(Base(0),Ex(TpArr(Base(0),Base(0)),Ap(Ap(Ap(Prim(51),DB(2)),DB(1)),DB(0))))));
Eq(TpArr(Base(0),TpArr(Base(0),Prop)),Prim(54),Lam(Base(0),Lam(Base(0),Ex(TpArr(Base(0),Base(0)),Ap(Ap(Ap(Prim(52),DB(2)),DB(1)),DB(0))))));
Eq(TpArr(TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),Base(0))),TpArr(Base(0),TpArr(Base(0),Prop))),Prim(55),Lam(TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),Base(0))),Lam(Base(0),Lam(Base(0),All(TpArr(Base(0),TpArr(Base(0),Prop)),Imp(All(Base(0),All(TpArr(Base(0),Base(0)),Imp(All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(2)),Ap(Ap(DB(3),DB(0)),Ap(DB(1),DB(0))))),Ap(Ap(DB(2),DB(1)),Ap(Ap(DB(5),DB(1)),DB(0)))))),Ap(Ap(DB(0),DB(2)),DB(1))))))));
Eq(TpArr(TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),Base(0))),TpArr(Base(0),Base(0))),Prim(56),Lam(TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),Base(0))),Lam(Base(0),Ap(Prim(0),Lam(Base(0),Ap(Ap(Ap(Prim(55),DB(2)),DB(1)),DB(0)))))));
Eq(TpArr(Base(0),Base(0)),Prim(57),Lam(Base(0),Ap(Ap(Prim(44),DB(0)),Ap(Prim(43),DB(0)))));
Eq(TpArr(Base(0),Prop),Prim(58),Lam(Base(0),All(TpArr(Base(0),Prop),Imp(Ap(DB(0),Prim(9)),Imp(All(Base(0),Imp(Ap(DB(1),DB(0)),Ap(DB(1),Ap(Prim(57),DB(0))))),Ap(DB(0),DB(1)))))));
Eq(TpArr(Base(0),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(Base(0),Base(0)))),Prim(59),Lam(Base(0),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Ap(Prim(56),Lam(Base(0),Lam(TpArr(Base(0),Base(0)),Ap(Ap(Ap(Prim(37),Ap(Ap(Prim(7),Ap(Prim(10),DB(1))),DB(1))),Ap(Ap(DB(2),Ap(Prim(10),DB(1))),Ap(DB(0),Ap(Prim(10),DB(1))))),DB(3))))))));
Eq(TpArr(Base(0),TpArr(Base(0),Base(0))),Prim(60),Lam(Base(0),Lam(Base(0),Ap(Ap(Ap(Prim(59),DB(1)),Lam(Base(0),Lam(Base(0),Ap(Prim(57),DB(0))))),DB(0)))));
Eq(TpArr(Base(0),TpArr(Base(0),Base(0))),Prim(61),Lam(Base(0),Lam(Base(0),Ap(Ap(Ap(Prim(59),Prim(9)),Lam(Base(0),Lam(Base(0),Ap(Ap(Prim(60),DB(3)),DB(0))))),DB(0)))));
Eq(TpArr(Base(0),Prop),Prim(62),Lam(Base(0),Ap(Ap(Prim(4),Ap(Prim(13),DB(0))),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(1)),Ap(Prim(13),DB(0)))))));
Eq(TpArr(Base(0),Base(0)),Prim(63),Ap(Prim(56),Lam(Base(0),Lam(TpArr(Base(0),Base(0)),Ap(Ap(Prim(46),DB(1)),Lam(Base(0),Ap(Prim(11),Ap(DB(1),DB(0)))))))));
Eq(TpArr(Base(0),Base(0)),Prim(64),Ap(Prim(56),Lam(Base(0),Lam(TpArr(Base(0),Base(0)),Ap(Ap(Prim(44),Ap(Prim(43),Prim(9))),Ap(Ap(Prim(12),DB(1)),Lam(Base(0),Ap(DB(1),DB(0)))))))));
Eq(TpArr(Base(0),Base(0)),Prim(65),Lam(Base(0),Ap(Ap(Prim(12),DB(0)),Lam(Base(0),Ap(Prim(64),DB(0))))));
Eq(TpArr(Base(0),Base(0)),Prim(66),Ap(Prim(56),Lam(Base(0),Lam(TpArr(Base(0),Base(0)),Ap(Ap(Prim(12),Ap(Ap(Prim(50),DB(1)),Ap(Prim(43),Prim(9)))),Lam(Base(0),Ap(DB(1),DB(0))))))));
Eq(TpArr(Base(0),TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),TpArr(TpArr(Base(0),Base(0)),TpArr(Base(0),Base(0)))))),Prim(67),Lam(Base(0),Lam(Base(0),Lam(TpArr(Base(0),Base(0)),Lam(TpArr(Base(0),Base(0)),Lam(Base(0),Ap(Ap(Ap(Prim(37),Eq(Base(0),DB(0),Ap(Prim(65),Ap(Prim(66),DB(0))))),Ap(DB(2),Ap(Prim(66),DB(0)))),Ap(DB(1),Ap(Prim(66),DB(0))))))))));
Eq(TpArr(Base(0),TpArr(Base(0),Base(0))),Prim(68),Lam(Base(0),Lam(Base(0),Ap(Ap(Prim(44),Ap(Ap(Prim(12),DB(1)),Lam(Base(0),Ap(Prim(65),DB(0))))),Ap(Ap(Prim(12),DB(0)),Lam(Base(0),Ap(Prim(64),DB(0))))))));
Eq(TpArr(Base(0),Base(0)),Prim(69),Lam(Base(0),Ap(Ap(Ap(Prim(48),DB(0)),Lam(Base(0),Ex(Base(0),Eq(Base(0),Ap(Prim(65),DB(0)),DB(1))))),Lam(Base(0),Ap(Prim(66),DB(0))))));
Eq(TpArr(Base(0),Base(0)),Prim(70),Lam(Base(0),Ap(Ap(Ap(Prim(48),DB(0)),Lam(Base(0),Ex(Base(0),Eq(Base(0),Ap(Prim(64),DB(0)),DB(1))))),Lam(Base(0),Ap(Prim(66),DB(0))))));
Eq(TpArr(Base(0),TpArr(Base(0),Base(0))),Prim(71),Lam(Base(0),Lam(Base(0),Ap(Ap(Prim(68),DB(1)),Ap(Prim(11),DB(0))))));
Eq(TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),Base(0))),Prim(72),Lam(Base(0),Lam(TpArr(Base(0),Base(0)),Ap(Ap(Prim(46),DB(1)),Lam(Base(0),Ap(Ap(Prim(12),Ap(DB(1),DB(0))),Lam(Base(0),Ap(Ap(Prim(68),DB(1)),DB(0)))))))));
Eq(TpArr(Base(0),TpArr(Base(0),Base(0))),Prim(73),Lam(Base(0),Lam(Base(0),Ap(Ap(Prim(72),DB(1)),Lam(Base(0),DB(1))))));
Eq(TpArr(Base(0),TpArr(Base(0),Base(0))),Prim(74),Lam(Base(0),Lam(Base(0),Ap(Ap(Ap(Prim(48),DB(1)),Lam(Base(0),Ex(Base(0),Eq(Base(0),DB(1),Ap(Ap(Prim(68),DB(2)),DB(0)))))),Lam(Base(0),Ap(Prim(70),DB(0)))))));
Eq(TpArr(Base(0),Prop),Prim(75),Lam(Base(0),Eq(Base(0),Ap(Ap(Prim(68),Ap(Ap(Prim(74),DB(0)),Prim(9))),Ap(Ap(Prim(74),DB(0)),Ap(Prim(57),Prim(9)))),DB(0))));
Eq(TpArr(Base(0),TpArr(Base(0),Prop)),Prim(76),Lam(Base(0),Lam(Base(0),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(1)),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(7),DB(0)),DB(3))),Ex(Base(0),Eq(Base(0),DB(2),Ap(Ap(Prim(68),DB(1)),DB(0)))))))))));
Eq(TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),Base(0))),Prim(77),Lam(Base(0),Lam(TpArr(Base(0),Base(0)),Ap(Ap(Prim(47),Ap(Prim(11),Ap(Ap(Prim(72),DB(1)),Lam(Base(0),Ap(Prim(10),Ap(DB(1),DB(0))))))),Lam(Base(0),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(3)),Ap(Ap(Prim(7),Ap(Ap(Prim(74),DB(1)),DB(0))),Ap(DB(2),DB(0))))))))));
Eq(TpArr(Base(0),TpArr(Base(0),Base(0))),Prim(78),Lam(Base(0),Lam(Base(0),Ap(Ap(Prim(77),DB(0)),Lam(Base(0),DB(2))))));
Eq(TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),TpArr(TpArr(Base(0),TpArr(Base(0),Prop)),Base(0)))),Prim(79),Lam(Base(0),Lam(TpArr(Base(0),Base(0)),Lam(TpArr(Base(0),TpArr(Base(0),Prop)),Ap(Ap(Prim(47),Ap(Ap(Prim(72),DB(2)),Lam(Base(0),Ap(DB(2),DB(0))))),Lam(Base(0),Ap(Ap(DB(1),Ap(Ap(Prim(74),DB(0)),Prim(9))),Ap(Ap(Prim(74),DB(0)),Ap(Prim(57),Prim(9))))))))));
Eq(TpArr(Base(0),Prop),Prim(80),Lam(Base(0),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(1)),Ex(Base(0),Ex(Base(0),Eq(Base(0),DB(2),Ap(Ap(Prim(72),Ap(Prim(57),Ap(Prim(57),Prim(9)))),Lam(Base(0),Ap(Ap(Ap(Prim(37),Eq(Base(0),DB(0),Prim(9))),DB(2)),DB(1)))))))))));
Eq(TpArr(Base(0),TpArr(TpArr(Base(0),Base(0)),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),Base(0)))),Prim(81),Lam(Base(0),Lam(TpArr(Base(0),Base(0)),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Ap(Ap(Prim(72),DB(2)),Lam(Base(0),Ap(Ap(Prim(72),Ap(DB(2),DB(0))),Lam(Base(0),Ap(Ap(DB(2),DB(1)),DB(0))))))))));
Eq(TpArr(Base(0),TpArr(TpArr(Base(0),Prop),TpArr(TpArr(Base(0),Prop),Prop))),Prim(82),Lam(Base(0),Lam(TpArr(Base(0),Prop),Lam(TpArr(Base(0),Prop),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(3)),Ap(Ap(Prim(6),Ap(DB(2),DB(0))),Ap(DB(1),DB(0)))))))));
Eq(TpArr(Base(0),TpArr(TpArr(Base(0),Prop),TpArr(TpArr(Base(0),Prop),Prop))),Prim(83),Lam(Base(0),Lam(TpArr(Base(0),Prop),Lam(TpArr(Base(0),Prop),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(7),DB(0)),DB(3))),Ap(Ap(Prim(4),Ap(Ap(Prim(4),Ap(Ap(Ap(Prim(82),DB(0)),DB(2)),DB(1))),Ap(Prim(3),Ap(DB(2),DB(0))))),Ap(DB(1),DB(0)))))))));
Eq(TpArr(Base(0),TpArr(TpArr(Base(0),Prop),TpArr(Base(0),TpArr(TpArr(Base(0),Prop),Prop)))),Prim(84),Lam(Base(0),Lam(TpArr(Base(0),Prop),Lam(Base(0),Lam(TpArr(Base(0),Prop),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Ap(Prim(83),Ap(Ap(Prim(49),DB(3)),DB(1))),DB(2)),DB(0))),Ap(Ap(Prim(4),Ap(Ap(Prim(4),Ap(Ap(Prim(7),DB(3)),DB(1))),Ap(Ap(Ap(Prim(82),DB(3)),DB(2)),DB(0)))),Ap(DB(0),DB(3))))),Ap(Ap(Prim(4),Ap(Ap(Prim(4),Ap(Ap(Prim(7),DB(1)),DB(3))),Ap(Ap(Ap(Prim(82),DB(1)),DB(2)),DB(0)))),Ap(Prim(3),Ap(DB(2),DB(1))))))))));
Eq(TpArr(Base(0),TpArr(TpArr(Base(0),Prop),TpArr(Base(0),TpArr(TpArr(Base(0),Prop),Prop)))),Prim(85),Lam(Base(0),Lam(TpArr(Base(0),Prop),Lam(Base(0),Lam(TpArr(Base(0),Prop),Ap(Ap(Prim(5),Ap(Ap(Ap(Ap(Prim(84),DB(3)),DB(2)),DB(1)),DB(0))),Ap(Ap(Prim(4),Eq(Base(0),DB(3),DB(1))),Ap(Ap(Ap(Prim(82),DB(3)),DB(2)),DB(0)))))))));
Eq(TpArr(TpArr(Base(0),TpArr(TpArr(Base(0),Prop),Prop)),TpArr(Base(0),TpArr(TpArr(Base(0),Prop),Prop))),Prim(86),Lam(TpArr(Base(0),TpArr(TpArr(Base(0),Prop),Prop)),Lam(Base(0),Lam(TpArr(Base(0),Prop),Ex(Base(0),Ap(Ap(Prim(4),Ap(Prim(62),DB(0))),Ex(TpArr(Base(0),Prop),Ap(Ap(Prim(4),Ap(Ap(DB(4),DB(1)),DB(0))),Ap(Ap(Ap(Ap(Prim(85),DB(3)),DB(2)),DB(1)),DB(0))))))))));
Eq(TpArr(TpArr(Base(0),TpArr(TpArr(Base(0),Prop),Prop)),TpArr(Base(0),TpArr(TpArr(Base(0),Prop),Prop))),Prim(87),Lam(TpArr(Base(0),TpArr(TpArr(Base(0),Prop),Prop)),Lam(Base(0),Lam(TpArr(Base(0),Prop),Ex(Base(0),Ap(Ap(Prim(4),Ap(Prim(62),DB(0))),Ex(TpArr(Base(0),Prop),Ap(Ap(Prim(4),Ap(Ap(DB(4),DB(1)),DB(0))),Ap(Ap(Ap(Ap(Prim(85),DB(1)),DB(0)),DB(3)),DB(2))))))))));
Eq(TpArr(Base(0),Base(0)),Prim(88),Lam(Base(0),Ap(Ap(Prim(44),DB(0)),Ap(Ap(Prim(12),DB(0)),Lam(Base(0),Ap(Lam(Base(0),Ap(Ap(Prim(45),DB(0)),Ap(Prim(43),Ap(Prim(57),Prim(9))))),DB(0)))))));
Eq(TpArr(Base(0),TpArr(Base(0),Prop)),Prim(89),Lam(Base(0),Lam(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(8),DB(0)),Ap(Prim(88),DB(1)))),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(2)),Ap(Ap(Prim(38),Ap(Ap(Prim(7),Ap(Lam(Base(0),Ap(Ap(Prim(45),DB(0)),Ap(Prim(43),Ap(Prim(57),Prim(9))))),DB(0))),DB(1))),Ap(Ap(Prim(7),DB(0)),DB(1)))))))));
Eq(TpArr(Base(0),TpArr(TpArr(Base(0),Prop),Base(0))),Prim(90),Lam(Base(0),Lam(TpArr(Base(0),Prop),Ap(Ap(Prim(44),Ap(Ap(Prim(47),DB(1)),Lam(Base(0),Ap(DB(1),DB(0))))),Ap(Ap(Ap(Prim(48),DB(1)),Lam(Base(0),Ap(Prim(3),Ap(DB(1),DB(0))))),Lam(Base(0),Ap(Lam(Base(0),Ap(Ap(Prim(45),DB(0)),Ap(Prim(43),Ap(Prim(57),Prim(9))))),DB(0))))))));
Eq(TpArr(Base(0),Prop),Prim(91),Lam(Base(0),Ex(Base(0),Ap(Ap(Prim(4),Ap(Prim(62),DB(0))),Ap(Ap(Prim(89),DB(0)),DB(1))))));
Eq(TpArr(Base(0),Base(0)),Prim(92),Lam(Base(0),Ap(Prim(0),Lam(Base(0),Ap(Ap(Prim(4),Ap(Prim(62),DB(0))),Ap(Ap(Prim(89),DB(0)),DB(1)))))));
Eq(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Prop))),Prim(93),Lam(Base(0),Lam(Base(0),Lam(Base(0),Ap(Ap(Ap(Prim(82),DB(2)),Lam(Base(0),Ap(Ap(Prim(7),DB(0)),DB(2)))),Lam(Base(0),Ap(Ap(Prim(7),DB(0)),DB(1))))))));
Eq(TpArr(Base(0),TpArr(Base(0),Prop)),Prim(94),Lam(Base(0),Lam(Base(0),Ap(Ap(Ap(Ap(Prim(84),Ap(Prim(92),DB(1))),Lam(Base(0),Ap(Ap(Prim(7),DB(0)),DB(2)))),Ap(Prim(92),DB(0))),Lam(Base(0),Ap(Ap(Prim(7),DB(0)),DB(1)))))));
Eq(TpArr(Base(0),TpArr(Base(0),Prop)),Prim(95),Lam(Base(0),Lam(Base(0),Ap(Ap(Ap(Ap(Prim(85),Ap(Prim(92),DB(1))),Lam(Base(0),Ap(Ap(Prim(7),DB(0)),DB(2)))),Ap(Prim(92),DB(0))),Lam(Base(0),Ap(Ap(Prim(7),DB(0)),DB(1)))))));
Eq(TpArr(Base(0),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),Prop)),Prim(96),Lam(Base(0),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(2)),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(3)),Ap(Ap(Prim(7),Ap(Ap(DB(2),DB(1)),DB(0))),DB(3)))))))));
Eq(TpArr(Base(0),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(Base(0),Prop))))),Prim(97),Lam(Base(0),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(4),Ap(Ap(Prim(4),Ap(Ap(Prim(4),Ap(Ap(Prim(96),DB(4)),DB(3))),Ap(Ap(Prim(96),DB(4)),DB(2)))),Ap(Ap(Prim(96),DB(4)),DB(1)))),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(5)),Ap(Ap(Prim(4),Eq(Base(0),Ap(Ap(DB(4),DB(1)),DB(0)),DB(0))),Eq(Base(0),Ap(Ap(DB(4),DB(0)),DB(1)),DB(0))))))),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(5)),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(6)),Ap(Ap(Prim(4),Ap(Ap(Prim(4),Ap(Ap(Prim(4),Eq(Base(0),Ap(Ap(DB(4),DB(1)),Ap(Ap(DB(5),DB(1)),DB(0))),DB(0))),Eq(Base(0),Ap(Ap(DB(5),DB(1)),Ap(Ap(DB(4),DB(1)),DB(0))),DB(0)))),Eq(Base(0),Ap(Ap(DB(3),Ap(Ap(DB(5),DB(1)),DB(0))),DB(0)),DB(1)))),Eq(Base(0),Ap(Ap(DB(5),Ap(Ap(DB(3),DB(1)),DB(0))),DB(0)),DB(1))))))))))))));
Eq(TpArr(Base(0),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(Base(0),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),TpArr(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),Prop)))))))))))))),Prim(98),Lam(Base(0),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(Base(0),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),Lam(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Ap(Ap(Prim(4),Ap(Ap(Prim(4),Ap(Ap(Prim(4),Ap(Ap(Prim(4),Ap(Ap(Ap(Ap(Ap(Prim(97),DB(13)),DB(12)),DB(11)),DB(10)),DB(9))),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(14)),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(15)),Eq(Base(0),Ap(Ap(DB(10),DB(1)),DB(0)),Ap(Ap(DB(13),Ap(Ap(DB(14),DB(0)),DB(1))),Ap(Ap(DB(14),DB(1)),DB(0)))))))))),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(14)),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(15)),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(16)),Eq(Base(0),Ap(Ap(Ap(DB(10),DB(2)),DB(1)),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(15),DB(2)),Ap(Ap(DB(15),DB(1)),DB(0)))),Ap(Ap(DB(15),Ap(Ap(DB(15),DB(2)),DB(1))),DB(0)))))))))))),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(14)),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(15)),Ap(Ap(Prim(4),Ap(Ap(Prim(4),Ap(Ap(Prim(4),Ap(Ap(Prim(4),Eq(Base(0),Ap(Ap(DB(8),DB(1)),DB(0)),Ap(Ap(DB(13),DB(1)),Ap(Ap(DB(14),DB(0)),DB(1))))),Eq(Base(0),Ap(Ap(DB(5),DB(1)),DB(0)),Ap(Ap(DB(14),DB(1)),Ap(Ap(DB(14),DB(0)),Ap(Ap(DB(13),DB(1)),DB(11))))))),Eq(Base(0),Ap(Ap(DB(4),DB(1)),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(14),Ap(Ap(DB(12),DB(11)),DB(1))),DB(0))),DB(1))))),Eq(Base(0),Ap(Ap(DB(3),DB(1)),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(13),DB(1)),DB(0))),Ap(Ap(DB(13),Ap(Ap(DB(13),DB(1)),DB(11))),DB(11)))))),Eq(Base(0),Ap(Ap(DB(2),DB(1)),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(12),DB(11)),Ap(Ap(DB(12),DB(11)),DB(1)))),Ap(Ap(DB(12),DB(0)),DB(1))))))))))),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(14)),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(15)),All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(16)),Ap(Ap(Prim(4),Eq(Base(0),Ap(Ap(Ap(DB(8),DB(2)),DB(1)),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(15),DB(1)),DB(2))),Ap(Ap(DB(15),DB(1)),Ap(Ap(DB(15),DB(2)),DB(0)))))),Eq(Base(0),Ap(Ap(Ap(DB(7),DB(2)),DB(1)),DB(0)),Ap(Ap(DB(13),Ap(Ap(DB(15),Ap(Ap(DB(15),DB(0)),DB(2))),DB(1))),Ap(Ap(DB(15),DB(2)),DB(1)))))))))))))))))))))))))));
Eq(TpArr(Base(0),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(Base(0),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),TpArr(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),Prop)))))))))))))),Prim(99),Lam(Base(0),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(Base(0),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),Lam(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Ap(Ap(Prim(4),Ap(Ap(Ap(Ap(Ap(Ap(Ap(Ap(Ap(Ap(Ap(Ap(Ap(Ap(Prim(98),DB(13)),DB(12)),DB(11)),DB(10)),DB(9)),DB(8)),DB(7)),DB(6)),DB(5)),DB(4)),DB(3)),DB(2)),DB(1)),DB(0))),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(7),DB(0)),DB(14))),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(7),DB(0)),DB(15))),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(7),DB(0)),DB(16))),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(7),DB(0)),DB(17))),Ap(Prim(3),Eq(Base(0),Ap(Ap(DB(12),Ap(Ap(DB(16),Ap(Ap(DB(15),Ap(Ap(Ap(DB(9),DB(2)),DB(1)),DB(3))),DB(13))),DB(3))),DB(0)),DB(13)))))))))))))))))))))))))));
Eq(TpArr(Base(0),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(Base(0),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),TpArr(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),TpArr(TpArr(Base(0),TpArr(Base(0),Base(0))),Prop)))))))))))))),Prim(100),Lam(Base(0),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(Base(0),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),Lam(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Base(0)))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Ap(Ap(Prim(4),Ap(Ap(Ap(Ap(Ap(Ap(Ap(Ap(Ap(Ap(Ap(Ap(Ap(Ap(Prim(98),DB(13)),DB(12)),DB(11)),DB(10)),DB(9)),DB(8)),DB(7)),DB(6)),DB(5)),DB(4)),DB(3)),DB(2)),DB(1)),DB(0))),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(7),DB(0)),DB(14))),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(7),DB(0)),DB(15))),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(7),DB(0)),DB(16))),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(7),DB(0)),DB(17))),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(7),DB(0)),DB(18))),Ap(Prim(3),Eq(Base(0),Ap(Ap(Ap(DB(12),DB(0)),Ap(Ap(DB(17),Ap(Ap(DB(15),DB(14)),DB(4))),Ap(Ap(Ap(DB(9),DB(3)),DB(2)),DB(4)))),DB(1)),DB(14)))))))))))))))))))))))))))));
Eq(TpArr(Base(0),Prop),Prim(101),Lam(Base(0),All(TpArr(Base(0),Prop),Imp(Ap(DB(0),Ap(Prim(65),Prim(9))),Imp(Ap(DB(0),Ap(Prim(65),Ap(Prim(11),Prim(9)))),Imp(All(Base(0),All(Base(0),Imp(Ap(DB(2),DB(1)),Imp(Ap(DB(2),DB(0)),Ap(DB(2),Ap(Prim(64),Ap(Ap(Prim(68),DB(1)),DB(0)))))))),Ap(DB(0),DB(1))))))));
Eq(TpArr(Base(0),TpArr(Base(0),Prop)),Prim(102),Lam(Base(0),Lam(Base(0),All(TpArr(Base(0),TpArr(Base(0),Prop)),Ap(Ap(Ap(Lam(Base(0),Lam(Base(0),Lam(TpArr(Base(0),TpArr(Base(0),Base(0))),Imp(Ap(Prim(30),DB(3)),Imp(All(Base(0),Imp(Ap(Prim(101),DB(0)),Ap(Ap(DB(4),DB(0)),DB(0)))),Imp(All(Base(0),All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Prim(101),DB(3)),Imp(Ap(Prim(101),DB(2)),Imp(Ap(Prim(101),DB(1)),Imp(Ap(Prim(101),DB(0)),Imp(Ap(Ap(DB(7),DB(3)),DB(1)),Imp(Ap(Ap(DB(7),DB(2)),DB(0)),Ap(Ap(DB(7),Ap(Ap(DB(4),DB(3)),DB(2))),Ap(Ap(DB(4),DB(1)),DB(0))))))))))))),Imp(All(Base(0),All(Base(0),Ap(Ap(DB(5),Ap(Ap(DB(2),Ap(Ap(DB(2),DB(4)),DB(1))),DB(0))),DB(1)))),Imp(All(Base(0),All(Base(0),All(Base(0),Ap(Ap(DB(6),Ap(Ap(DB(3),Ap(Ap(DB(3),Ap(Ap(DB(3),DB(4)),DB(2))),DB(1))),DB(0))),Ap(Ap(DB(3),Ap(Ap(DB(3),DB(2)),DB(0))),Ap(Ap(DB(3),DB(1)),DB(0))))))),Ap(Ap(DB(3),DB(5)),DB(4)))))))))),Ap(Prim(65),Prim(9))),Ap(Prim(65),Ap(Prim(11),Prim(9)))),Lam(Base(0),Lam(Base(0),Ap(Prim(64),Ap(Ap(Prim(68),DB(1)),DB(0))))))))));
Eq(TpArr(Base(0),TpArr(Base(0),TpArr(Base(0),Prop))),Prim(103),Lam(Base(0),Lam(Base(0),Lam(Base(0),Ex(Base(0),Ex(Base(0),Ap(Ap(Prim(5),Ap(Ap(Prim(4),Ap(Ap(Prim(54),Ap(Ap(Prim(68),DB(4)),DB(1))),DB(3))),Ap(Ap(Prim(54),Ap(Ap(Prim(73),DB(0)),DB(1))),DB(2)))),Ap(Ap(Prim(4),Ap(Ap(Prim(54),Ap(Ap(Prim(68),DB(3)),DB(1))),DB(4))),Ap(Ap(Prim(54),Ap(Ap(Prim(73),DB(0)),DB(1))),DB(2))))))))))]

let hfaxs = List.map (fun p -> let (p,b) = beta_eta_norm p 32 in if not b then raise (Failure "impossible") else (tm_hashroot p,p)) hfaxs

let hfthyspec : theoryspec = List.rev (List.map (fun a -> Thyprim(a)) hfprimtps) @ (List.map (fun (_,p) -> Thyaxiom(p)) hfaxs)
let hfthy : theory = theoryspec_theory hfthyspec

let hfthyid : hashval = (match hashtheory hfthy with None -> raise (Failure "impossible") | Some(h) -> h)
let initthytree : ttree = ottree_insert None (hashval_bitseq hfthyid) hfthy
let initthytreeroot : hashval = (match ottree_hashroot (Some(initthytree)) with None -> raise (Failure "impossible") | Some(h) -> h)

let propformersa = Array.make 48 (0,Prop)
let setformersa = Array.make 38 (0,Prop)

let hf_info : stp list * gsign =
  propformersa.(0) <- (13,List.nth hfprimtps 13);
  propformersa.(1) <- (14,List.nth hfprimtps 14);
  propformersa.(2) <- (15,List.nth hfprimtps 15);
  propformersa.(3) <- (16,List.nth hfprimtps 16);
  propformersa.(4) <- (17,List.nth hfprimtps 17);
  propformersa.(5) <- (18,List.nth hfprimtps 18);
  propformersa.(6) <- (19,List.nth hfprimtps 19);
  propformersa.(7) <- (20,List.nth hfprimtps 20);
  propformersa.(8) <- (21,List.nth hfprimtps 21);
  propformersa.(9) <- (22,List.nth hfprimtps 22);
  propformersa.(10) <- (58,List.nth hfprimtps 58);
  propformersa.(11) <- (62,List.nth hfprimtps 62);
  propformersa.(12) <- (75,List.nth hfprimtps 75);
  propformersa.(13) <- (80,List.nth hfprimtps 80);
  propformersa.(14) <- (91,List.nth hfprimtps 91);
  propformersa.(15) <- (7,List.nth hfprimtps 7);
  propformersa.(16) <- (8,List.nth hfprimtps 8);
  propformersa.(17) <- (53,List.nth hfprimtps 53);
  propformersa.(18) <- (54,List.nth hfprimtps 54);
  propformersa.(19) <- (76,List.nth hfprimtps 76);
  propformersa.(20) <- (89,List.nth hfprimtps 89);
  propformersa.(21) <- (94,List.nth hfprimtps 94);
  propformersa.(22) <- (95,List.nth hfprimtps 95);
  propformersa.(23) <- (24,List.nth hfprimtps 24);
  propformersa.(24) <- (25,List.nth hfprimtps 25);
  propformersa.(25) <- (26,List.nth hfprimtps 26);
  propformersa.(26) <- (27,List.nth hfprimtps 27);
  propformersa.(27) <- (28,List.nth hfprimtps 28);
  propformersa.(28) <- (29,List.nth hfprimtps 29);
  propformersa.(29) <- (30,List.nth hfprimtps 30);
  propformersa.(30) <- (31,List.nth hfprimtps 31);
  propformersa.(31) <- (32,List.nth hfprimtps 32);
  propformersa.(32) <- (33,List.nth hfprimtps 33);
  propformersa.(33) <- (34,List.nth hfprimtps 34);
  propformersa.(34) <- (35,List.nth hfprimtps 35);
  propformersa.(35) <- (36,List.nth hfprimtps 36);
  propformersa.(36) <- (38,List.nth hfprimtps 38);
  propformersa.(37) <- (39,List.nth hfprimtps 39);
  propformersa.(38) <- (51,List.nth hfprimtps 51);
  propformersa.(39) <- (52,List.nth hfprimtps 52);
  propformersa.(40) <- (82,List.nth hfprimtps 82);
  propformersa.(41) <- (83,List.nth hfprimtps 83);
  propformersa.(42) <- (84,List.nth hfprimtps 84);
  propformersa.(43) <- (85,List.nth hfprimtps 85);
  propformersa.(44) <- (86,List.nth hfprimtps 86);
  propformersa.(45) <- (87,List.nth hfprimtps 87);
  propformersa.(46) <- (93,List.nth hfprimtps 93);
  propformersa.(47) <- (96,List.nth hfprimtps 96);
  setformersa.(0) <- (9,List.nth hfprimtps 9);
  setformersa.(1) <- (10,List.nth hfprimtps 10);
  setformersa.(2) <- (11,List.nth hfprimtps 11);
  setformersa.(3) <- (43,List.nth hfprimtps 43);
  setformersa.(4) <- (57,List.nth hfprimtps 57);
  setformersa.(5) <- (63,List.nth hfprimtps 63);
  setformersa.(6) <- (64,List.nth hfprimtps 64);
  setformersa.(7) <- (65,List.nth hfprimtps 65);
  setformersa.(8) <- (66,List.nth hfprimtps 66);
  setformersa.(9) <- (69,List.nth hfprimtps 69);
  setformersa.(10) <- (70,List.nth hfprimtps 70);
  setformersa.(11) <- (88,List.nth hfprimtps 88);
  setformersa.(12) <- (92,List.nth hfprimtps 92);
  setformersa.(13) <- (42,List.nth hfprimtps 42);
  setformersa.(14) <- (44,List.nth hfprimtps 44);
  setformersa.(15) <- (45,List.nth hfprimtps 45);
  setformersa.(16) <- (49,List.nth hfprimtps 49);
  setformersa.(17) <- (50,List.nth hfprimtps 50);
  setformersa.(18) <- (60,List.nth hfprimtps 60);
  setformersa.(19) <- (61,List.nth hfprimtps 61);
  setformersa.(20) <- (68,List.nth hfprimtps 68);
  setformersa.(21) <- (71,List.nth hfprimtps 71);
  setformersa.(22) <- (73,List.nth hfprimtps 73);
  setformersa.(23) <- (74,List.nth hfprimtps 74);
  setformersa.(24) <- (78,List.nth hfprimtps 78);
  setformersa.(25) <- (12,List.nth hfprimtps 12);
  setformersa.(26) <- (37,List.nth hfprimtps 37);
  setformersa.(27) <- (46,List.nth hfprimtps 46);
  setformersa.(28) <- (47,List.nth hfprimtps 47);
  setformersa.(29) <- (48,List.nth hfprimtps 48);
  setformersa.(30) <- (56,List.nth hfprimtps 56);
  setformersa.(31) <- (59,List.nth hfprimtps 59);
  setformersa.(32) <- (67,List.nth hfprimtps 67);
  setformersa.(33) <- (72,List.nth hfprimtps 72);
  setformersa.(34) <- (77,List.nth hfprimtps 77);
  setformersa.(35) <- (79,List.nth hfprimtps 79);
  setformersa.(36) <- (81,List.nth hfprimtps 81);
  setformersa.(37) <- (90,List.nth hfprimtps 90);
  (hfprimtps,([],hfaxs))

let ahfctx = [TpArr(Base(0),Prop);TpArr(Base(0),Prop);TpArr(Base(0),Prop);TpArr(Base(0),Prop);TpArr(Base(0),Prop);TpArr(Base(0),Prop);TpArr(Base(0),Prop);TpArr(Base(0),Prop);TpArr(Base(0),Prop);TpArr(Base(0),Prop);TpArr(Base(0),Prop);TpArr(Base(0),TpArr(Base(0),Base(0)));TpArr(Base(0),TpArr(Base(0),Base(0)));TpArr(Base(0),TpArr(Base(0),Base(0)));TpArr(Base(0),Base(0));TpArr(Base(0),Base(0));Base(0);Base(0);Base(0);Base(0);Base(0);TpArr(Base(0),TpArr(Base(0),Prop));TpArr(Base(0),TpArr(Base(0),Prop));TpArr(Base(0),TpArr(Base(0),Prop))]

let ahfprops
  = [| (All(Base(0),All(Base(0),Ap(Ap(Prim(6),Ap(Ap(DB(24),DB(1)),DB(0))),All(Base(0),Imp(Ap(Ap(DB(26),DB(0)),DB(2)),Ap(Ap(DB(26),DB(0)),DB(1)))))))); (* "Subq_def" *)
       (All(Base(0),All(Base(0),Ap(Ap(Prim(6),Ap(Ap(DB(23),DB(1)),DB(0))),Eq(Base(0),Ap(Ap(DB(14),DB(1)),DB(0)),DB(22)))))); (* "disj_def" *)
       (All(Base(0),Ap(Ap(Prim(6),Ap(DB(11),DB(0))),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(DB(25),DB(0)),DB(1))),Ap(Prim(3),Ap(Ap(DB(24),DB(1)),Ap(DB(17),DB(0))))))))); (* "atleast2_def" *)
       (All(Base(0),Ap(Ap(Prim(6),Ap(DB(10),DB(0))),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(DB(24),DB(0)),DB(1))),Ap(Ap(Prim(4),Ap(Prim(3),Ap(Ap(DB(24),DB(1)),DB(0)))),Ap(DB(12),DB(0)))))))); (* "atleast3_def" *)
       (All(Base(0),Ap(Ap(Prim(6),Ap(DB(9),DB(0))),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(DB(24),DB(0)),DB(1))),Ap(Ap(Prim(4),Ap(Prim(3),Ap(Ap(DB(24),DB(1)),DB(0)))),Ap(DB(11),DB(0)))))))); (* "atleast4_def" *)
       (All(Base(0),Ap(Ap(Prim(6),Ap(DB(8),DB(0))),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(DB(24),DB(0)),DB(1))),Ap(Ap(Prim(4),Ap(Prim(3),Ap(Ap(DB(24),DB(1)),DB(0)))),Ap(DB(10),DB(0)))))))); (* "atleast5_def" *)
       (All(Base(0),Ap(Ap(Prim(6),Ap(DB(7),DB(0))),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(DB(24),DB(0)),DB(1))),Ap(Ap(Prim(4),Ap(Prim(3),Ap(Ap(DB(24),DB(1)),DB(0)))),Ap(DB(9),DB(0)))))))); (* "atleast6_def" *)
       (All(Base(0),Ap(Ap(Prim(6),Ap(DB(6),DB(0))),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(DB(24),DB(0)),DB(1))),Ap(Ap(Prim(4),Ap(Prim(3),Ap(Ap(DB(24),DB(1)),DB(0)))),Ap(DB(8),DB(0)))))))); (* "atleast7_def" *)
       (All(Base(0),Ap(Ap(Prim(6),Ap(DB(5),DB(0))),Ap(Ap(Prim(4),Ap(DB(11),DB(0))),Ap(Prim(3),Ap(DB(10),DB(0))))))); (* "exactly2_def" *)
       (All(Base(0),Ap(Ap(Prim(6),Ap(DB(4),DB(0))),Ap(Ap(Prim(4),Ap(DB(10),DB(0))),Ap(Prim(3),Ap(DB(9),DB(0))))))); (* "exactly3_def" *)
       (All(Base(0),Ap(Ap(Prim(6),Ap(DB(3),DB(0))),Ap(Ap(Prim(4),Ap(DB(9),DB(0))),Ap(Prim(3),Ap(DB(8),DB(0))))))); (* "exactly4_def" *)
       (All(Base(0),Ap(Ap(Prim(6),Ap(DB(2),DB(0))),Ap(Ap(Prim(4),Ap(DB(8),DB(0))),Ap(Prim(3),Ap(DB(7),DB(0))))))); (* "exactly5_def" *)
       (All(Base(0),Ap(Ap(Prim(6),Ap(DB(1),DB(0))),Ap(Ap(Prim(4),Ap(DB(7),DB(0))),Ap(Prim(3),Ap(DB(6),DB(0))))))); (* "exactly6_def" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(24),DB(1)),DB(0)),Imp(Ap(Ap(DB(24),DB(0)),DB(1)),Eq(Base(0),DB(1),DB(0))))))); (* "set_ext" *)
       (All(Base(0),Ap(Prim(3),Ap(Ap(DB(24),DB(0)),DB(0))))); (* "In_irref" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(1)),DB(0)),Ap(Prim(3),Ap(Ap(DB(25),DB(0)),DB(1))))))); (* "In_no2cycle" *)
       (All(Base(0),Ap(Prim(3),Ap(Ap(DB(24),DB(0)),DB(21))))); (* "EmptyE" *)
       (All(Base(0),Ap(Ap(Prim(6),Ap(Ap(DB(24),DB(0)),DB(20))),Eq(Base(0),DB(0),DB(21))))); (* "In_1_iff" *)
       (All(Base(0),Ap(Ap(Prim(6),Ap(Ap(DB(24),DB(0)),DB(19))),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))))); (* "In_2_iff" *)
       (All(Base(0),Ap(Ap(Prim(6),Ap(Ap(DB(24),DB(0)),DB(18))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))),Eq(Base(0),DB(0),DB(19)))))); (* "In_3_iff" *)
       (All(Base(0),Ap(Ap(Prim(6),Ap(Ap(DB(24),DB(0)),DB(17))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))),Eq(Base(0),DB(0),DB(19)))),Eq(Base(0),DB(0),DB(18)))))); (* "In_4_iff" *)
       (All(Base(0),All(Base(0),Ap(Ap(Prim(6),Ap(Ap(DB(25),DB(0)),Ap(DB(17),DB(1)))),Ap(Ap(DB(24),DB(0)),DB(1)))))); (* "Power_iff" *)
       (All(Base(0),All(Base(0),Ap(Ap(Prim(6),Ap(Ap(DB(25),DB(0)),Ap(DB(16),DB(1)))),Eq(Base(0),DB(0),DB(1)))))); (* "Sing_iff" *)
       (All(Base(0),All(Base(0),All(Base(0),Ap(Ap(Prim(6),Ap(Ap(DB(26),DB(0)),Ap(Ap(DB(16),DB(2)),DB(1)))),Ap(Ap(Prim(5),Ap(Ap(DB(26),DB(0)),DB(2))),Ap(Ap(DB(26),DB(0)),DB(1)))))))); (* "binunion_iff" *)
       (All(Base(0),All(Base(0),All(Base(0),Ap(Ap(Prim(6),Ap(Ap(DB(26),DB(0)),Ap(Ap(DB(15),DB(2)),DB(1)))),Ap(Ap(Prim(4),Ap(Ap(DB(26),DB(0)),DB(2))),Ap(Ap(DB(26),DB(0)),DB(1)))))))); (* "binintersect_iff" *)
       (All(Base(0),All(Base(0),All(Base(0),Ap(Ap(Prim(6),Ap(Ap(DB(26),DB(0)),Ap(Ap(DB(14),DB(2)),DB(1)))),Ap(Ap(Prim(4),Ap(Ap(DB(26),DB(0)),DB(2))),Ap(Prim(3),Ap(Ap(DB(26),DB(0)),DB(1))))))))); (* "setminus_iff" *)
       (Ap(Ap(DB(23),DB(20)),DB(19))); (* "In_0_1" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(20)),Eq(Base(0),DB(0),DB(21))))); (* "In_1_inv" *)
       (Ap(Ap(DB(23),DB(20)),DB(18))); (* "In_0_2" *)
       (Ap(Ap(DB(23),DB(19)),DB(18))); (* "In_1_2" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(19)),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))))); (* "In_2_inv" *)
       (Ap(Ap(DB(23),DB(20)),DB(17))); (* "In_0_3" *)
       (Ap(Ap(DB(23),DB(19)),DB(17))); (* "In_1_3" *)
       (Ap(Ap(DB(23),DB(18)),DB(17))); (* "In_2_3" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(18)),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))),Eq(Base(0),DB(0),DB(19)))))); (* "In_3_inv" *)
       (Ap(Ap(DB(23),DB(20)),DB(16))); (* "In_0_4" *)
       (Ap(Ap(DB(23),DB(19)),DB(16))); (* "In_1_4" *)
       (Ap(Ap(DB(23),DB(18)),DB(16))); (* "In_2_4" *)
       (Ap(Ap(DB(23),DB(17)),DB(16))); (* "In_3_4" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(17)),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))),Eq(Base(0),DB(0),DB(19)))),Eq(Base(0),DB(0),DB(18)))))); (* "In_4_inv" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(1)),Ap(Ap(DB(25),DB(0)),Ap(DB(17),DB(1))))))); (* "PowerI" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),Ap(DB(17),DB(1))),Ap(Ap(DB(24),DB(0)),DB(1)))))); (* "PowerE" *)
       (All(Base(0),Ap(Ap(DB(24),DB(0)),Ap(DB(15),DB(0))))); (* "SingI" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),Ap(DB(16),DB(1))),Eq(Base(0),DB(0),DB(1)))))); (* "SingE" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(26),DB(0)),DB(2)),Ap(Ap(DB(26),DB(0)),Ap(Ap(DB(16),DB(2)),DB(1)))))))); (* "binunionI1" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(26),DB(0)),DB(1)),Ap(Ap(DB(26),DB(0)),Ap(Ap(DB(16),DB(2)),DB(1)))))))); (* "binunionI2" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(26),DB(0)),Ap(Ap(DB(16),DB(2)),DB(1))),Ap(Ap(Prim(5),Ap(Ap(DB(26),DB(0)),DB(2))),Ap(Ap(DB(26),DB(0)),DB(1)))))))); (* "binunionE" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(26),DB(0)),DB(2)),Imp(Ap(Ap(DB(26),DB(0)),DB(1)),Ap(Ap(DB(26),DB(0)),Ap(Ap(DB(15),DB(2)),DB(1))))))))); (* "binintersectI" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(26),DB(0)),Ap(Ap(DB(15),DB(2)),DB(1))),Ap(Ap(DB(26),DB(0)),DB(2))))))); (* "binintersectE1" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(26),DB(0)),Ap(Ap(DB(15),DB(2)),DB(1))),Ap(Ap(DB(26),DB(0)),DB(1))))))); (* "binintersectE2" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(26),DB(0)),DB(2)),Imp(Ap(Prim(3),Ap(Ap(DB(26),DB(0)),DB(1))),Ap(Ap(DB(26),DB(0)),Ap(Ap(DB(14),DB(2)),DB(1))))))))); (* "setminusI" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(26),DB(0)),Ap(Ap(DB(14),DB(2)),DB(1))),Ap(Ap(DB(26),DB(0)),DB(2))))))); (* "setminusE1" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(26),DB(0)),Ap(Ap(DB(14),DB(2)),DB(1))),Ap(Prim(3),Ap(Ap(DB(26),DB(0)),DB(1)))))))); (* "setminusE2" *)
       (All(Base(0),Ap(Ap(DB(23),DB(0)),DB(0)))); (* "Subq_ref" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(2)),DB(1)),Imp(Ap(Ap(DB(25),DB(1)),DB(0)),Ap(Ap(DB(25),DB(2)),DB(0)))))))); (* "Subq_tra" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(2)),DB(1)),Imp(Ap(Prim(3),Ap(Ap(DB(26),DB(0)),DB(1))),Ap(Prim(3),Ap(Ap(DB(26),DB(0)),DB(2))))))))); (* "Subq_contra" *)
       (All(Base(0),Ap(Ap(DB(23),DB(21)),DB(0)))); (* "Subq_Empty" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),DB(21)),Eq(Base(0),DB(0),DB(21))))); (* "Empty_Subq_eq" *)
       (All(Base(0),Ap(Ap(DB(24),DB(21)),Ap(DB(16),DB(0))))); (* "Empty_In_Power" *)
       (All(Base(0),Ap(Ap(DB(24),DB(0)),Ap(DB(16),DB(0))))); (* "Self_In_Power" *)
       (All(Base(0),All(Base(0),Ap(Ap(DB(24),DB(1)),Ap(Ap(DB(15),DB(1)),DB(0)))))); (* "binunion_Subq_1" *)
       (All(Base(0),All(Base(0),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(15),DB(1)),DB(0)))))); (* "binunion_Subq_2" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(2)),DB(0)),Imp(Ap(Ap(DB(25),DB(1)),DB(0)),Ap(Ap(DB(25),Ap(Ap(DB(16),DB(2)),DB(1))),DB(0)))))))); (* "binunion_Subq_min" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(1)),DB(0)),Ap(Ap(DB(25),Ap(Ap(DB(16),DB(2)),DB(1))),Ap(Ap(DB(16),DB(2)),DB(0)))))))); (* "Subq_binunion_R" *)
       (All(Base(0),All(Base(0),All(Base(0),Ap(Ap(DB(25),Ap(Ap(DB(16),DB(2)),Ap(Ap(DB(16),DB(1)),DB(0)))),Ap(Ap(DB(16),Ap(Ap(DB(16),DB(2)),DB(1))),DB(0))))))); (* "binunion_asso_Subq_1" *)
       (All(Base(0),All(Base(0),All(Base(0),Ap(Ap(DB(25),Ap(Ap(DB(16),Ap(Ap(DB(16),DB(2)),DB(1))),DB(0))),Ap(Ap(DB(16),DB(2)),Ap(Ap(DB(16),DB(1)),DB(0)))))))); (* "binunion_asso_Subq_2" *)
       (All(Base(0),All(Base(0),All(Base(0),Eq(Base(0),Ap(Ap(DB(16),DB(2)),Ap(Ap(DB(16),DB(1)),DB(0))),Ap(Ap(DB(16),Ap(Ap(DB(16),DB(2)),DB(1))),DB(0))))))); (* "binunion_asso" *)
       (All(Base(0),All(Base(0),Ap(Ap(DB(24),Ap(Ap(DB(15),DB(1)),DB(0))),Ap(Ap(DB(15),DB(0)),DB(1)))))); (* "binunion_com_Subq" *)
       (All(Base(0),All(Base(0),Eq(Base(0),Ap(Ap(DB(15),DB(1)),DB(0)),Ap(Ap(DB(15),DB(0)),DB(1)))))); (* "binunion_com" *)
       (All(Base(0),All(Base(0),Ap(Ap(DB(24),Ap(Ap(DB(14),DB(1)),DB(0))),DB(0))))); (* "binintersect_Subq_2" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),DB(2)),Imp(Ap(Ap(DB(25),DB(0)),DB(1)),Ap(Ap(DB(25),DB(0)),Ap(Ap(DB(15),DB(2)),DB(1))))))))); (* "binintersect_Subq_max" *)
       (All(Base(0),All(Base(0),Ap(Ap(DB(24),Ap(Ap(DB(13),DB(1)),DB(0))),DB(1))))); (* "setminus_Subq" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),DB(1)),Ap(Ap(DB(25),Ap(Ap(DB(14),DB(2)),DB(1))),Ap(Ap(DB(14),DB(2)),DB(0)))))))); (* "setminus_Subq_contra" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(All(Base(0),Imp(Ap(Ap(DB(27),DB(0)),DB(3)),Imp(Ap(Ap(DB(27),DB(0)),DB(2)),Ap(Ap(DB(27),DB(0)),DB(1))))),Ap(Ap(DB(25),Ap(Ap(DB(15),DB(2)),DB(1))),DB(0))))))); (* "binintersect_Subq_test" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(All(Base(0),Imp(Ap(Ap(DB(27),DB(0)),DB(3)),Imp(Ap(Ap(DB(27),DB(0)),DB(2)),Ap(Ap(DB(27),DB(0)),DB(1))))),Imp(Ap(Ap(DB(25),DB(0)),DB(2)),Imp(Ap(Ap(DB(25),DB(0)),DB(1)),Eq(Base(0),Ap(Ap(DB(15),DB(2)),DB(1)),DB(0))))))))); (* "binintersect_eq_test" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(All(Base(0),Imp(Ap(Ap(DB(27),DB(0)),DB(3)),Imp(Ap(Prim(3),Ap(Ap(DB(27),DB(0)),DB(2))),Ap(Ap(DB(27),DB(0)),DB(1))))),Ap(Ap(DB(25),Ap(Ap(DB(14),DB(2)),DB(1))),DB(0))))))); (* "setminus_Subq_test" *)
       (All(Base(0),All(Base(0),Imp(All(Base(0),Imp(Ap(Ap(DB(26),DB(0)),DB(2)),Ap(Prim(3),Ap(Ap(DB(26),DB(0)),DB(1))))),Ap(Ap(DB(23),DB(1)),DB(0)))))); (* "disj_I" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(24),DB(2)),DB(1)),Imp(Ap(Ap(DB(26),DB(0)),DB(2)),Ap(Prim(3),Ap(Ap(DB(26),DB(0)),DB(1))))))))); (* "disj_E" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(23),DB(1)),DB(0)),Ap(Ap(DB(23),DB(0)),DB(1)))))); (* "disj_sym" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(24),DB(2)),DB(1)),Imp(Ap(Ap(DB(25),DB(0)),DB(1)),Ap(Ap(DB(24),DB(2)),DB(0)))))))); (* "disj_Subq" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(24),DB(2)),DB(1)),Imp(Ap(Ap(DB(25),Ap(Ap(DB(16),DB(2)),DB(1))),Ap(Ap(DB(16),DB(2)),DB(0))),Ap(Ap(DB(25),DB(1)),DB(0)))))))); (* "disj_Subq_R" *)
       (All(Base(0),All(Base(0),Ap(Ap(DB(23),Ap(Ap(DB(13),DB(1)),DB(0))),Ap(Ap(DB(14),DB(1)),DB(0)))))); (* "disj_part" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),DB(1)),Ap(Prim(3),Ap(Ap(DB(24),DB(1)),Ap(Ap(DB(13),DB(1)),Ap(DB(16),DB(0))))))))); (* "remove_nSubq" *)
       (All(Base(0),All(Base(0),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(1))),Ap(Prim(3),Ap(Ap(DB(25),DB(0)),Ap(DB(16),DB(1)))))))); (* "nIn_Sing_I" *)
       (All(Base(0),All(Base(0),Imp(Ap(Prim(3),Ap(Ap(DB(25),DB(0)),Ap(DB(16),DB(1)))),Ap(Prim(3),Eq(Base(0),DB(0),DB(1))))))); (* "nIn_Sing_E" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Prim(3),Ap(Ap(DB(26),DB(0)),DB(2))),Imp(Ap(Prim(3),Ap(Ap(DB(26),DB(0)),DB(1))),Ap(Prim(3),Ap(Ap(DB(26),DB(0)),Ap(Ap(DB(16),DB(2)),DB(1)))))))))); (* "nIn_binunion_I" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Prim(3),Ap(Ap(DB(26),DB(0)),DB(2))),Ap(Prim(3),Ap(Ap(DB(26),DB(0)),Ap(Ap(DB(14),DB(2)),DB(1))))))))); (* "nIn_setminus_I1" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(26),DB(0)),DB(1)),Ap(Prim(3),Ap(Ap(DB(26),DB(0)),Ap(Ap(DB(14),DB(2)),DB(1))))))))); (* "nIn_setminus_I2" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(1)),DB(0)),Ap(DB(12),Ap(DB(17),DB(0))))))); (* "nonempty_Power_atleast2" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(26),DB(1)),DB(2)),Imp(Ap(Ap(DB(26),DB(0)),DB(2)),Imp(Ap(Prim(3),Ap(Ap(DB(25),DB(0)),DB(1))),Ap(DB(13),DB(2))))))))); (* "atleast2_I" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(26),DB(1)),DB(2)),Imp(Ap(Ap(DB(26),DB(0)),DB(2)),Imp(Ap(Prim(3),Eq(Base(0),DB(1),DB(0))),Ap(DB(13),DB(2))))))))); (* "atleast2_I2" *)
       (All(Base(0),All(Base(0),Imp(Ap(Prim(3),Eq(Base(0),DB(1),DB(0))),Ap(DB(12),Ap(Ap(DB(15),Ap(DB(16),DB(1))),Ap(DB(16),DB(0)))))))); (* "binunion_Sing_atleast2" *)
       (All(Base(0),All(Base(0),Imp(Ap(Prim(3),Ap(Ap(DB(25),DB(0)),DB(1))),Imp(Ap(DB(12),DB(1)),Ap(DB(11),Ap(Ap(DB(15),DB(1)),Ap(DB(16),DB(0))))))))); (* "atleast3_adj" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),DB(1)),Ap(Ap(DB(24),Ap(DB(16),DB(0))),DB(1)))))); (* "In_Sing_Subq" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),DB(1)),Imp(Ap(Prim(3),Ap(DB(10),DB(1))),Ap(Prim(3),Ap(DB(11),Ap(Ap(DB(13),DB(1)),Ap(DB(16),DB(0)))))))))); (* "atmost3_rem" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),DB(1)),Imp(Ap(Prim(3),Ap(DB(9),DB(1))),Ap(Prim(3),Ap(DB(10),Ap(Ap(DB(13),DB(1)),Ap(DB(16),DB(0)))))))))); (* "atmost4_rem" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),DB(1)),Imp(Ap(Prim(3),Ap(DB(8),DB(1))),Ap(Prim(3),Ap(DB(9),Ap(Ap(DB(13),DB(1)),Ap(DB(16),DB(0)))))))))); (* "atmost5_rem" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),DB(1)),Imp(Ap(Prim(3),Ap(DB(7),DB(1))),Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(13),DB(1)),Ap(DB(16),DB(0)))))))))); (* "atmost6_rem" *)
       (Ap(Ap(DB(22),DB(19)),Ap(DB(14),DB(20)))); (* "Subq_1_Sing_0" *)
       (Ap(Ap(DB(22),Ap(DB(14),DB(20))),DB(19))); (* "Subq_Sing_0_1" *)
       (Eq(Base(0),DB(19),Ap(DB(14),DB(20)))); (* "eq_1_Sing_0" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(1)),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(1)),DB(0))),Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(DB(26),DB(0)),DB(2))),Ap(Ap(DB(25),DB(1)),Ap(Ap(DB(14),DB(2)),Ap(DB(17),DB(0))))))))))); (* "prop_subq_ex" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(24),DB(1)),DB(0)),Imp(Ap(DB(12),DB(1)),Ap(DB(12),DB(0))))))); (* "atleast2_mon" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(24),DB(1)),DB(0)),Imp(Ap(DB(11),DB(1)),Ap(DB(11),DB(0))))))); (* "atleast3_mon" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(24),DB(1)),DB(0)),Imp(Ap(DB(10),DB(1)),Ap(DB(10),DB(0))))))); (* "atleast4_mon" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(24),DB(1)),DB(0)),Imp(Ap(DB(9),DB(1)),Ap(DB(9),DB(0))))))); (* "atleast5_mon" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(24),DB(1)),DB(0)),Imp(Ap(DB(8),DB(1)),Ap(DB(8),DB(0))))))); (* "atleast6_mon" *)
       (All(Base(0),Imp(Ap(DB(9),DB(0)),Ap(DB(10),DB(0))))); (* "atleast4_atleast3" *)
       (All(Base(0),Ap(Prim(3),Ap(DB(11),Ap(DB(15),DB(0)))))); (* "atmost1_Sing" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(23),DB(1)),DB(0)),Imp(Ap(DB(12),DB(0)),All(Base(0),Imp(Ap(Ap(DB(26),DB(0)),DB(1)),Ap(Prim(3),Ap(Ap(DB(25),Ap(Ap(DB(16),DB(2)),DB(1))),Ap(Ap(DB(16),DB(2)),Ap(DB(17),DB(0)))))))))))); (* "atleast4_2_2_part_lem1" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(23),DB(1)),DB(0)),Imp(Ap(DB(12),DB(1)),All(Base(0),Imp(Ap(Ap(DB(26),DB(0)),DB(1)),Ap(DB(12),Ap(Ap(DB(16),DB(2)),Ap(DB(17),DB(0))))))))))); (* "atleast4_2_2_part_lem2" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(23),DB(1)),DB(0)),Imp(Ap(DB(12),DB(1)),Imp(Ap(DB(12),DB(0)),Ap(DB(10),Ap(Ap(DB(15),DB(1)),DB(0))))))))); (* "atleast4_2_2_part" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(23),DB(1)),DB(0)),Imp(Ap(DB(12),DB(1)),Imp(Ap(DB(11),DB(0)),Ap(DB(9),Ap(Ap(DB(15),DB(1)),DB(0))))))))); (* "atleast5_2_3_part" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(23),DB(1)),DB(0)),Imp(Ap(DB(12),DB(1)),Imp(Ap(DB(10),DB(0)),Ap(DB(8),Ap(Ap(DB(15),DB(1)),DB(0))))))))); (* "atleast6_2_4_part" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(23),DB(1)),DB(0)),Imp(Ap(DB(10),DB(1)),Imp(Ap(DB(12),DB(0)),Ap(DB(8),Ap(Ap(DB(15),DB(1)),DB(0))))))))); (* "atleast6_4_2_part" *)
       (All(Base(0),All(Base(0),Ap(Ap(DB(24),DB(1)),Ap(Ap(DB(15),Ap(Ap(DB(13),DB(1)),DB(0))),Ap(Ap(DB(14),DB(1)),DB(0))))))); (* "partition_Subq_1" *)
       (All(Base(0),All(Base(0),Ap(Ap(DB(24),Ap(Ap(DB(15),Ap(Ap(DB(13),DB(1)),DB(0))),Ap(Ap(DB(14),DB(1)),DB(0)))),DB(1))))); (* "partition_Subq_2" *)
       (All(Base(0),All(Base(0),Eq(Base(0),DB(1),Ap(Ap(DB(15),Ap(Ap(DB(13),DB(1)),DB(0))),Ap(Ap(DB(14),DB(1)),DB(0))))))); (* "partition_eq" *)
       (All(Base(0),All(Base(0),Imp(Ap(Prim(3),Ap(DB(8),DB(1))),Imp(Ap(DB(12),Ap(Ap(DB(14),DB(1)),DB(0))),Ap(Prim(3),Ap(DB(10),Ap(Ap(DB(13),DB(1)),DB(0))))))))); (* "atmost3_setminus_5_2" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(1)),Ap(DB(17),Ap(DB(16),DB(0)))),Ap(Ap(Prim(5),Eq(Base(0),DB(1),DB(22))),Eq(Base(0),DB(1),Ap(DB(16),DB(0)))))))); (* "In_Power_Sing_inv" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(24),DB(1)),Ap(DB(17),Ap(DB(16),DB(0)))),Imp(Ap(DB(12),DB(1)),Ap(Ap(DB(24),Ap(DB(17),Ap(DB(16),DB(0)))),DB(1))))))); (* "atmost2_Power_Sing_lem" *)
       (All(Base(0),Ap(Prim(3),Ap(DB(10),Ap(DB(16),Ap(DB(15),DB(0))))))); (* "atmost2_Power_Sing" *)
       (All(Base(0),Ap(DB(5),Ap(DB(16),Ap(DB(15),DB(0)))))); (* "Power_Sing_exactly2" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),Ap(Ap(DB(16),DB(2)),DB(1))),Imp(Ap(Prim(3),Ap(Ap(DB(25),DB(0)),DB(2))),Imp(Ap(Prim(3),Ap(Ap(DB(25),DB(0)),DB(1))),Imp(Ap(Prim(3),Ap(Ap(DB(25),Ap(Ap(DB(16),DB(2)),DB(1))),DB(0))),Ap(Ap(Prim(5),Ap(DB(13),DB(2))),Ap(DB(13),DB(1))))))))))); (* "atleast3_2_2_cover_prop" *)
       (All(Base(0),All(Base(0),Imp(Ap(DB(11),Ap(Ap(DB(15),DB(1)),DB(0))),Ap(Ap(Prim(5),Ap(DB(12),DB(1))),Ap(DB(12),DB(0))))))); (* "atleast3_2_2_cover" *)
       (All(Base(0),All(Base(0),Ap(Prim(3),Ap(DB(11),Ap(Ap(DB(15),Ap(DB(16),DB(1))),Ap(DB(16),DB(0)))))))); (* "binunion_Sing_atmost2" *)
       (All(Base(0),All(Base(0),Imp(Ap(Prim(3),Eq(Base(0),DB(1),DB(0))),Ap(DB(6),Ap(Ap(DB(15),Ap(DB(16),DB(1))),Ap(DB(16),DB(0)))))))); (* "binunion_Sing_exactly2" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),DB(1)),Eq(Base(0),Ap(Ap(DB(14),DB(1)),Ap(DB(16),DB(0))),Ap(DB(16),DB(0))))))); (* "binintersect_In_Sing_R" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),DB(1)),Imp(Ap(DB(11),DB(1)),Ap(DB(12),Ap(Ap(DB(13),DB(1)),Ap(DB(16),DB(0))))))))); (* "atleast3_rem" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),DB(1)),Imp(Ap(DB(5),DB(1)),Ap(DB(6),Ap(Ap(DB(13),DB(1)),Ap(DB(16),DB(0))))))))); (* "exactly3_rem" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),Ap(Ap(DB(16),DB(2)),DB(1))),All(Base(0),Imp(Ap(Prim(3),Ap(Ap(DB(27),DB(0)),DB(1))),Ap(Ap(DB(26),DB(1)),Ap(Ap(DB(17),Ap(Ap(DB(15),DB(3)),Ap(DB(18),DB(0)))),DB(2)))))))))); (* "Subq_binunion_remL" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),Ap(Ap(DB(16),DB(2)),DB(1))),All(Base(0),Imp(Ap(Ap(DB(27),DB(0)),DB(3)),Imp(Ap(Prim(3),Ap(Ap(DB(27),DB(0)),DB(1))),Imp(Ap(DB(13),DB(1)),Ap(DB(13),Ap(Ap(DB(17),Ap(Ap(DB(15),DB(3)),Ap(DB(18),DB(0)))),DB(2)))))))))))); (* "atleast3_Subq_binunion_remL_prop" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),Ap(Ap(DB(16),DB(2)),DB(1))),All(Base(0),Imp(Ap(Ap(DB(27),DB(0)),DB(3)),Imp(Ap(Prim(3),Ap(Ap(DB(27),DB(0)),DB(1))),Imp(Ap(DB(13),DB(1)),Ap(Ap(Prim(5),Ap(DB(13),DB(3))),Ap(DB(14),DB(2)))))))))))); (* "atleast4_3_2_cover_prop" *)
       (All(Base(0),All(Base(0),Imp(Ap(DB(10),Ap(Ap(DB(15),DB(1)),DB(0))),Ap(Ap(Prim(5),Ap(DB(11),DB(1))),Ap(DB(12),DB(0))))))); (* "atleast4_3_2_cover" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),DB(1)),Imp(Ap(DB(10),DB(1)),Ap(DB(11),Ap(Ap(DB(13),DB(1)),Ap(DB(16),DB(0))))))))); (* "atleast4_rem" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),DB(1)),Imp(Ap(DB(4),DB(1)),Ap(DB(5),Ap(Ap(DB(13),DB(1)),Ap(DB(16),DB(0))))))))); (* "exactly4_rem" *)
       (All(Base(0),All(Base(0),Imp(Ap(Prim(3),Ap(DB(11),DB(1))),Ap(Prim(3),Ap(DB(10),Ap(Ap(DB(15),DB(1)),Ap(DB(16),DB(0))))))))); (* "atmost3_adj" *)
       (All(Base(0),All(Base(0),Imp(Ap(DB(10),Ap(Ap(DB(15),DB(1)),DB(0))),Ap(Ap(Prim(5),Ap(DB(12),DB(1))),Ap(DB(11),DB(0))))))); (* "atleast4_2_3_cover" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),Ap(Ap(DB(16),DB(2)),DB(1))),All(Base(0),Imp(Ap(Ap(DB(27),DB(0)),DB(3)),Imp(Ap(Prim(3),Ap(Ap(DB(27),DB(0)),DB(1))),Imp(Ap(DB(12),DB(1)),Ap(DB(12),Ap(Ap(DB(17),Ap(Ap(DB(15),DB(3)),Ap(DB(18),DB(0)))),DB(2)))))))))))); (* "atleast4_Subq_binunion_remL_prop" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),Ap(Ap(DB(16),DB(2)),DB(1))),All(Base(0),Imp(Ap(Ap(DB(27),DB(0)),DB(3)),Imp(Ap(Prim(3),Ap(Ap(DB(27),DB(0)),DB(1))),Imp(Ap(DB(12),DB(1)),Ap(Ap(Prim(5),Ap(DB(12),DB(3))),Ap(DB(14),DB(2)))))))))))); (* "atleast5_4_2_cover_prop" *)
       (All(Base(0),All(Base(0),Imp(Ap(DB(9),Ap(Ap(DB(15),DB(1)),DB(0))),Ap(Ap(Prim(5),Ap(DB(10),DB(1))),Ap(DB(12),DB(0))))))); (* "atleast5_4_2_cover" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),DB(1)),Imp(Ap(DB(9),DB(1)),Ap(DB(10),Ap(Ap(DB(13),DB(1)),Ap(DB(16),DB(0))))))))); (* "atleast5_rem" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),DB(1)),Imp(Ap(DB(3),DB(1)),Ap(DB(4),Ap(Ap(DB(13),DB(1)),Ap(DB(16),DB(0))))))))); (* "exactly5_rem" *)
       (All(Base(0),All(Base(0),Imp(Ap(Prim(3),Ap(DB(10),DB(1))),Ap(Prim(3),Ap(DB(9),Ap(Ap(DB(15),DB(1)),Ap(DB(16),DB(0))))))))); (* "atmost4_adj" *)
       (All(Base(0),All(Base(0),Imp(Ap(DB(9),Ap(Ap(DB(15),DB(1)),DB(0))),Ap(Ap(Prim(5),Ap(DB(12),DB(1))),Ap(DB(10),DB(0))))))); (* "atleast5_2_4_cover" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),Ap(Ap(DB(16),DB(2)),DB(1))),All(Base(0),Imp(Ap(Ap(DB(27),DB(0)),DB(3)),Imp(Ap(Prim(3),Ap(Ap(DB(27),DB(0)),DB(1))),Imp(Ap(DB(12),DB(1)),Ap(Ap(Prim(5),Ap(DB(13),DB(3))),Ap(DB(13),DB(2)))))))))))); (* "atleast5_3_3_cover_prop1" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),Ap(Ap(DB(16),DB(2)),DB(1))),All(Base(0),Imp(Ap(Ap(DB(27),DB(0)),DB(2)),Imp(Ap(Prim(3),Ap(Ap(DB(27),DB(0)),DB(1))),Imp(Ap(DB(12),DB(1)),Ap(Ap(Prim(5),Ap(DB(13),DB(3))),Ap(DB(13),DB(2)))))))))))); (* "atleast5_3_3_cover_prop2" *)
       (All(Base(0),All(Base(0),Imp(Ap(DB(9),Ap(Ap(DB(15),DB(1)),DB(0))),Ap(Ap(Prim(5),Ap(DB(11),DB(1))),Ap(DB(11),DB(0))))))); (* "atleast5_3_3_cover" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),Ap(Ap(DB(16),DB(2)),DB(1))),All(Base(0),Imp(Ap(Ap(DB(27),DB(0)),DB(3)),Imp(Ap(Prim(3),Ap(Ap(DB(27),DB(0)),DB(1))),Imp(Ap(DB(11),DB(1)),Ap(DB(11),Ap(Ap(DB(17),Ap(Ap(DB(15),DB(3)),Ap(DB(18),DB(0)))),DB(2)))))))))))); (* "atleast5_Subq_binunion_remL_prop" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),Ap(Ap(DB(16),DB(2)),DB(1))),All(Base(0),Imp(Ap(Ap(DB(27),DB(0)),DB(3)),Imp(Ap(Prim(3),Ap(Ap(DB(27),DB(0)),DB(1))),Imp(Ap(DB(11),DB(1)),Ap(Ap(Prim(5),Ap(DB(11),DB(3))),Ap(DB(14),DB(2)))))))))))); (* "atleast6_5_2_cover_prop" *)
       (All(Base(0),All(Base(0),Imp(Ap(DB(8),Ap(Ap(DB(15),DB(1)),DB(0))),Ap(Ap(Prim(5),Ap(DB(9),DB(1))),Ap(DB(12),DB(0))))))); (* "atleast6_5_2_cover" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),DB(1)),Imp(Ap(DB(8),DB(1)),Ap(DB(9),Ap(Ap(DB(13),DB(1)),Ap(DB(16),DB(0))))))))); (* "atleast6_rem" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),DB(1)),Imp(Ap(DB(2),DB(1)),Ap(DB(3),Ap(Ap(DB(13),DB(1)),Ap(DB(16),DB(0))))))))); (* "exactly6_rem" *)
       (All(Base(0),All(Base(0),Imp(Ap(Prim(3),Ap(DB(9),DB(1))),Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(15),DB(1)),Ap(DB(16),DB(0))))))))); (* "atmost5_adj" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),Ap(Ap(DB(16),DB(2)),DB(1))),All(Base(0),Imp(Ap(Ap(DB(27),DB(0)),DB(3)),Imp(Ap(Prim(3),Ap(Ap(DB(27),DB(0)),DB(1))),Imp(Ap(DB(10),DB(1)),Ap(DB(10),Ap(Ap(DB(17),Ap(Ap(DB(15),DB(3)),Ap(DB(18),DB(0)))),DB(2)))))))))))); (* "atleast6_Subq_binunion_remL_prop" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),Ap(Ap(DB(16),DB(2)),DB(1))),All(Base(0),Imp(Ap(Ap(DB(27),DB(0)),DB(3)),Imp(Ap(Prim(3),Ap(Ap(DB(27),DB(0)),DB(1))),Imp(Ap(DB(10),DB(1)),Ap(Ap(Prim(5),Ap(DB(10),DB(3))),Ap(DB(14),DB(2)))))))))))); (* "atleast7_6_2_cover_prop" *)
       (All(Base(0),All(Base(0),Imp(Ap(DB(7),Ap(Ap(DB(15),DB(1)),DB(0))),Ap(Ap(Prim(5),Ap(DB(8),DB(1))),Ap(DB(12),DB(0))))))); (* "atleast7_6_2_cover" *)
       (All(Base(0),All(Base(0),Imp(Ap(Prim(3),Ap(DB(8),DB(1))),Ap(Prim(3),Ap(DB(7),Ap(Ap(DB(15),DB(1)),Ap(DB(16),DB(0))))))))); (* "atmost6_adj" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),DB(1)),Imp(Ap(DB(11),Ap(Ap(DB(13),DB(1)),Ap(DB(16),DB(0)))),Ap(DB(10),DB(1))))))); (* "atleast3_4_setminus" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(25),DB(0)),DB(1)),Imp(Ap(DB(10),Ap(Ap(DB(13),DB(1)),Ap(DB(16),DB(0)))),Ap(DB(9),DB(1))))))); (* "atleast4_5_setminus" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(23),DB(1)),DB(0)),Imp(Ap(DB(6),DB(1)),Imp(Ap(DB(6),DB(0)),Ap(DB(4),Ap(Ap(DB(15),DB(1)),DB(0))))))))); (* "exactly4_2_2_part" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(26),DB(0)),Ap(Ap(DB(16),DB(2)),DB(1))),Imp(Ap(DB(12),Ap(Ap(DB(14),Ap(Ap(DB(16),DB(2)),DB(1))),Ap(DB(17),DB(0)))),Ap(Ap(Prim(5),Ap(DB(12),DB(2))),Ap(DB(13),DB(1))))))))); (* "atleast3_3_2_setminus_cover" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(26),DB(0)),Ap(Ap(DB(16),DB(2)),DB(1))),Imp(Ap(DB(11),Ap(Ap(DB(14),Ap(Ap(DB(16),DB(2)),DB(1))),Ap(DB(17),DB(0)))),Ap(Ap(Prim(5),Ap(DB(13),DB(2))),Ap(DB(11),DB(1))))))))); (* "atleast4_2_4_setminus_cover" *)
       (All(Base(0),All(Base(0),All(Base(0),Imp(Ap(Ap(DB(26),DB(0)),Ap(Ap(DB(16),DB(2)),DB(1))),Imp(Ap(DB(11),Ap(Ap(DB(14),Ap(Ap(DB(16),DB(2)),DB(1))),Ap(DB(17),DB(0)))),Ap(Ap(Prim(5),Ap(DB(12),DB(2))),Ap(DB(12),DB(1))))))))); (* "atleast4_3_3_setminus_cover" *)
       (All(Base(0),All(Base(0),Imp(Ap(DB(9),DB(1)),Imp(Ap(Prim(3),Ap(DB(11),Ap(Ap(DB(14),DB(1)),DB(0)))),Ap(DB(11),Ap(Ap(DB(13),DB(1)),DB(0)))))))); (* "atleast3_setminus_5_2" *)
       (All(Base(0),All(Base(0),Imp(Ap(DB(3),DB(1)),Imp(Ap(DB(6),Ap(Ap(DB(14),DB(1)),DB(0))),Ap(DB(5),Ap(Ap(DB(13),DB(1)),DB(0)))))))); (* "exactly3_setminus_5_2" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(20)),DB(20)))); (* "nIn_0_0" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(19)),DB(20)))); (* "nIn_1_0" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(18)),DB(20)))); (* "nIn_2_0" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(19)),DB(19)))); (* "nIn_1_1" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(18)),DB(19)))); (* "nIn_2_1" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(18)),DB(18)))); (* "nIn_2_2" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(17)),DB(18)))); (* "nIn_3_2" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(17)),DB(17)))); (* "nIn_3_3" *)
       (Ap(Ap(DB(22),DB(19)),DB(18))); (* "Subq_1_2" *)
       (Ap(Prim(3),Ap(Ap(DB(22),DB(18)),DB(19)))); (* "nSubq_2_1" *)
       (Ap(Ap(DB(22),DB(18)),DB(17))); (* "Subq_2_3" *)
       (Ap(Prim(3),Ap(Ap(DB(22),DB(17)),DB(18)))); (* "nSubq_3_2" *)
       (Ap(Ap(DB(22),DB(17)),DB(16))); (* "Subq_3_4" *)
       (Ap(Prim(3),Ap(Ap(DB(22),DB(16)),DB(17)))); (* "nSubq_4_3" *)
       (Ap(Prim(3),Eq(Base(0),DB(20),DB(19)))); (* "neq_0_1" *)
       (Ap(Prim(3),Eq(Base(0),DB(20),DB(18)))); (* "neq_0_2" *)
       (Ap(Prim(3),Eq(Base(0),DB(19),DB(18)))); (* "neq_1_2" *)
       (Ap(Prim(3),Eq(Base(0),DB(20),DB(17)))); (* "neq_0_3" *)
       (Ap(Prim(3),Eq(Base(0),DB(19),DB(17)))); (* "neq_1_3" *)
       (Ap(Prim(3),Eq(Base(0),DB(18),DB(17)))); (* "neq_2_3" *)
       (Ap(Prim(3),Ap(Ap(DB(22),DB(19)),DB(20)))); (* "nSubq_1_0" *)
       (Ap(Prim(3),Ap(Ap(DB(22),DB(18)),DB(20)))); (* "nSubq_2_0" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),DB(19)),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(21)),DB(0))),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(20)),DB(0))),Eq(Base(0),DB(0),DB(21))))))); (* "Subq_2_inv_lem1" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),DB(19)),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(20)),DB(0))),Ap(Ap(DB(23),DB(0)),DB(20)))))); (* "Subq_2_inv_no1_Subq_1" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(21)),DB(0)),Ap(Ap(DB(23),DB(20)),DB(0))))); (* "In_0_Subq_1" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),DB(19)),Imp(Ap(Ap(DB(24),DB(21)),DB(0)),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(20)),DB(0))),Eq(Base(0),DB(0),DB(20))))))); (* "Subq_2_inv_lem2" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),DB(19)),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(21)),DB(0))),Ap(Ap(DB(23),DB(0)),Ap(DB(15),DB(20))))))); (* "Subq_2_inv_no0_Subq_2" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),DB(19)),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(21)),DB(0))),Imp(Ap(Ap(DB(24),DB(20)),DB(0)),Eq(Base(0),DB(0),Ap(DB(15),DB(20)))))))); (* "Subq_2_inv_lem3" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),DB(19)),Imp(Ap(Ap(DB(24),DB(21)),DB(0)),Imp(Ap(Ap(DB(24),DB(20)),DB(0)),Eq(Base(0),DB(0),DB(19))))))); (* "Subq_2_inv_lem4" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),DB(19)),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))),Eq(Base(0),DB(0),DB(19)))))); (* "Subq_2_inv" *)
       (All(Base(0),All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(DB(16),DB(1))),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(22))),Eq(Base(0),DB(0),Ap(DB(16),DB(1)))))))); (* "Subq_Sing_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(DB(15),DB(20))),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))))); (* "Subqa_2_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(DB(15),Ap(DB(15),DB(20)))),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))))); (* "Subqa_4_inv" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(19))),DB(20)))); (* "nIna_2_0" *)
       (Ap(Ap(DB(23),DB(19)),Ap(DB(14),DB(19)))); (* "Ina_1_2" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))); (* "Ina_2_4" *)
       (Ap(Ap(DB(23),DB(20)),Ap(DB(15),Ap(DB(14),DB(19))))); (* "Ina_0_5" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "Ina_1_6" *)
       (Ap(Ap(DB(23),DB(20)),Ap(DB(15),DB(18)))); (* "Ina_0_15" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(20)),Ap(DB(14),DB(18))))); (* "nIna_0_8" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18))))); (* "Ina_0_7" *)
       (Ap(Ap(DB(23),DB(18)),Ap(DB(14),DB(18)))); (* "Ina_3_8" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(20)),Ap(DB(14),DB(19))))); (* "nIna_0_2" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))); (* "Ina_0_9" *)
       (Ap(Ap(DB(23),DB(19)),Ap(DB(15),DB(18)))); (* "Ina_1_15" *)
       (Ap(Prim(3),Ap(Ap(DB(22),DB(19)),Ap(DB(14),DB(19))))); (* "nSubqa_1_2" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(19)),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "nIna_1_5" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "Ina_1_10" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(DB(15),DB(20))),Eq(Base(0),DB(0),DB(20))))); (* "Ina_2_inv" *)
       (Ap(Ap(DB(22),Ap(DB(14),DB(19))),DB(18))); (* "Subqa_2_3" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(DB(15),DB(18)))); (* "Ina_2_15" *)
       (Ap(Prim(3),Eq(Base(0),DB(20),Ap(DB(14),DB(19))))); (* "neqa_0_2" *)
       (Ap(Prim(3),Eq(Base(0),DB(19),Ap(DB(14),DB(19))))); (* "neqa_1_2" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(19))),DB(18)))); (* "nIna_2_3" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18)))); (* "Ina_2_12" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19))))); (* "Ina_0_13" *)
       (Ap(Prim(3),Ap(Ap(DB(22),DB(19)),Ap(DB(14),DB(18))))); (* "nSubqa_1_8" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(19)),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "nIna_1_257" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "Ina_1_14" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(19))),DB(19)))); (* "nIna_2_1" *)
       (Ap(Prim(3),Eq(Base(0),DB(20),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "neqa_0_4" *)
       (Ap(Prim(3),Eq(Base(0),DB(20),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "neqa_0_5" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(19)),Ap(DB(14),DB(18))))); (* "nIna_1_8" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18))))); (* "Ina_1_7" *)
       (Ap(Prim(3),Eq(Base(0),DB(20),Ap(DB(14),DB(18))))); (* "neqa_0_8" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(18))),DB(19)))); (* "nIna_8_1" *)
       (Ap(Prim(3),Eq(Base(0),DB(20),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))); (* "neqa_0_9" *)
       (Ap(Prim(3),Eq(Base(0),DB(20),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))); (* "neqa_0_12" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(20)),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "nIna_0_4" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(DB(15),Ap(DB(14),DB(19))))); (* "Ina_2_5" *)
       (Ap(Ap(DB(22),DB(19)),Ap(DB(15),Ap(DB(14),DB(19))))); (* "Subqa_1_5" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "Ina_2_6" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "nIna_0_6" *)
       (Ap(Prim(3),Eq(Base(0),Ap(DB(14),DB(19)),DB(18)))); (* "neqa_2_3" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(DB(14),DB(18))))); (* "nIna_2_8" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18))))); (* "Ina_2_7" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(18)),Ap(DB(14),DB(19))))); (* "nIna_3_2" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))); (* "Ina_3_9" *)
       (Ap(Ap(DB(22),DB(19)),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))); (* "Subqa_1_9" *)
       (Ap(Ap(DB(23),DB(18)),Ap(DB(15),DB(18)))); (* "Ina_3_15" *)
       (Ap(Prim(3),Ap(Ap(DB(22),DB(18)),Ap(DB(14),DB(19))))); (* "nSubqa_3_2" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(18)),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "nIna_3_5" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "Ina_3_10" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18)))); (* "Ina_3_12" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))); (* "nIna_0_12" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19))))); (* "Ina_3_13" *)
       (Ap(Prim(3),Ap(Ap(DB(22),DB(18)),Ap(DB(14),DB(18))))); (* "nSubqa_3_8" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(18)),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "nIna_3_257" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "Ina_3_14" *)
       (Ap(Ap(DB(23),DB(20)),Ap(DB(15),Ap(DB(14),DB(18))))); (* "Ina_0_257" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))); (* "nIna_0_14" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(DB(14),DB(19))))); (* "nIna_2_2" *)
       (Ap(Prim(3),Eq(Base(0),DB(19),Ap(DB(14),DB(18))))); (* "neqa_1_8" *)
       (Ap(Prim(3),Eq(Base(0),DB(19),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))); (* "neqa_1_9" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(17)),Ap(DB(14),DB(19))))); (* "nIna_11_2" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19))))); (* "Ina_2_13" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(DB(14),DB(19))),Ap(DB(14),DB(18))))); (* "nSubqa_2_8" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "nIna_2_257" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "Ina_2_14" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(19)),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "nIna_1_4" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "nSubqa_2_4" *)
       (Ap(Ap(DB(22),Ap(DB(14),DB(19))),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "Subqa_2_6" *)
       (Ap(Ap(DB(22),Ap(DB(14),DB(19))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18))))); (* "Subqa_2_7" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))); (* "nIna_1_9" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))); (* "nIna_1_12" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))))); (* "nIna_1_13" *)
       (Ap(Prim(3),Eq(Base(0),DB(19),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "neqa_1_4" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),Ap(DB(14),DB(19)))),DB(18)))); (* "nIna_4_3" *)
       (Ap(Prim(3),Eq(Base(0),DB(19),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "neqa_1_5" *)
       (Ap(Prim(3),Eq(Base(0),DB(20),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "neqa_0_6" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(18))),DB(18)))); (* "nIna_8_3" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),DB(18)))); (* "nIna_9_3" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(15),DB(18))),DB(18)))); (* "nIna_15_3" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(DB(14),DB(18))),DB(18)))); (* "nSubqa_8_3" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),DB(18)))); (* "nSubqa_9_3" *)
       (Ap(Prim(3),Ap(Ap(DB(22),DB(18)),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))); (* "nSubqa_3_9" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),DB(18)))); (* "nSubqa_12_3" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),DB(18)))); (* "nSubqa_14_3" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(18)),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "nIna_3_4" *)
       (Ap(Prim(3),Eq(Base(0),Ap(DB(14),DB(19)),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "neqa_2_6" *)
       (Ap(Prim(3),Eq(Base(0),Ap(DB(14),DB(19)),Ap(DB(14),DB(18))))); (* "neqa_2_8" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(18))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "nIna_8_4" *)
       (Ap(Prim(3),Eq(Base(0),Ap(DB(14),DB(19)),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))); (* "neqa_2_9" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "nIna_9_4" *)
       (Ap(Prim(3),Eq(Base(0),Ap(DB(14),DB(19)),DB(17)))); (* "neqa_2_11" *)
       (Ap(Prim(3),Eq(Base(0),Ap(DB(14),DB(19)),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))); (* "neqa_2_12" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "nIna_12_4" *)
       (Ap(Prim(3),Eq(Base(0),Ap(DB(14),DB(19)),Ap(DB(15),DB(18))))); (* "neqa_2_15" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "nIna_15_4" *)
       (Ap(Prim(3),Eq(Base(0),Ap(DB(14),DB(19)),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "neqa_2_4" *)
       (Ap(Prim(3),Eq(Base(0),DB(18),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "neqa_3_4" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(DB(15),Ap(DB(15),DB(20)))),Eq(Base(0),DB(0),Ap(DB(15),DB(20)))))); (* "Ina_4_inv" *)
       (Ap(Ap(DB(22),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(DB(15),Ap(DB(14),DB(19))))); (* "Subqa_4_5" *)
       (Ap(Ap(DB(22),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "Subqa_4_6" *)
       (Ap(Ap(DB(22),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18))))); (* "Subqa_4_7" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(19))),DB(17)))); (* "nIna_2_11" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))); (* "nIna_2_9" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))))); (* "nIna_2_10" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "nSubqa_15_4" *)
       (Ap(Prim(3),Eq(Base(0),DB(18),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "neqa_3_5" *)
       (Ap(Prim(3),Eq(Base(0),Ap(DB(14),Ap(DB(14),DB(19))),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "neqa_4_5" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(DB(16),Ap(DB(15),DB(20)))),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))))); (* "Ina_5_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0)),Imp(Ap(Ap(DB(24),DB(21)),DB(0)),Ap(Ap(DB(23),Ap(DB(16),Ap(DB(15),DB(20)))),DB(0)))))); (* "Subqa_5_inv_lem1_Subq" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(DB(16),Ap(DB(15),DB(20)))),Imp(Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0)),Imp(Ap(Ap(DB(24),DB(21)),DB(0)),Eq(Base(0),DB(0),Ap(DB(16),Ap(DB(15),DB(20))))))))); (* "Subqa_5_inv_lem1" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(DB(16),Ap(DB(15),DB(20)))),Imp(Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0)),Imp(Ap(Ap(DB(24),DB(21)),DB(0)),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))),Eq(Base(0),DB(0),Ap(DB(16),Ap(DB(15),DB(20)))))))))); (* "Subqa_5_inv_lem2" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(DB(16),Ap(DB(15),DB(20)))),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(21)),DB(0))),Ap(Ap(DB(23),DB(0)),Ap(DB(15),Ap(DB(15),DB(20)))))))); (* "Subqa_5_inv_lem3_Subq" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(DB(16),Ap(DB(15),DB(20)))),Imp(Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0)),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(21)),DB(0))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20))))))))); (* "Subqa_5_inv_lem3" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(DB(16),Ap(DB(15),DB(20)))),Imp(Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0)),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(21)),DB(0))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))),Eq(Base(0),DB(0),Ap(DB(16),Ap(DB(15),DB(20)))))))))); (* "Subqa_5_inv_lem4" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(DB(16),Ap(DB(15),DB(20)))),Imp(Ap(Prim(3),Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0))),Ap(Ap(DB(23),DB(0)),DB(20)))))); (* "Subqa_5_inv_lem5_Subq" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(DB(16),Ap(DB(15),DB(20)))),Imp(Ap(Prim(3),Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0))),Imp(Ap(Ap(DB(24),DB(21)),DB(0)),Eq(Base(0),DB(0),DB(20))))))); (* "Subqa_5_inv_lem5" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(DB(16),Ap(DB(15),DB(20)))),Imp(Ap(Prim(3),Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0))),Imp(Ap(Ap(DB(24),DB(21)),DB(0)),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))),Eq(Base(0),DB(0),Ap(DB(16),Ap(DB(15),DB(20)))))))))); (* "Subqa_5_inv_lem6" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(DB(16),Ap(DB(15),DB(20)))),Imp(Ap(Prim(3),Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0))),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(21)),DB(0))),Eq(Base(0),DB(0),DB(21))))))); (* "Subqa_5_inv_lem7" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(DB(16),Ap(DB(15),DB(20)))),Imp(Ap(Prim(3),Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0))),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(21)),DB(0))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))),Eq(Base(0),DB(0),Ap(DB(16),Ap(DB(15),DB(20)))))))))); (* "Subqa_5_inv_lem8" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(DB(16),Ap(DB(15),DB(20)))),Imp(Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0)),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))),Eq(Base(0),DB(0),Ap(DB(16),Ap(DB(15),DB(20))))))))); (* "Subqa_5_inv_lem9" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(DB(16),Ap(DB(15),DB(20)))),Imp(Ap(Prim(3),Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))),Eq(Base(0),DB(0),Ap(DB(16),Ap(DB(15),DB(20))))))))); (* "Subqa_5_inv_lem10" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(DB(16),Ap(DB(15),DB(20)))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))),Eq(Base(0),DB(0),Ap(DB(16),Ap(DB(15),DB(20)))))))); (* "Subqa_5_inv" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "nSubqa_15_5" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "nIna_3_6" *)
       (Ap(Prim(3),Eq(Base(0),Ap(DB(14),Ap(DB(14),DB(19))),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "neqa_4_6" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "nSubqa_7_6" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18))))); (* "Subqa_6_7" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(18))),Ap(DB(15),DB(18))))); (* "nIna_8_15" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(DB(15),DB(18))))); (* "nIna_9_15" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(17)),Ap(DB(15),DB(18))))); (* "nIna_11_15" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(DB(15),DB(18))))); (* "nIna_12_15" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),DB(18))))); (* "nIna_14_15" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18)))))); (* "nIna_14_7" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(DB(15),DB(18))))); (* "nIna_15_15" *)
       (Ap(Prim(3),Eq(Base(0),DB(18),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))); (* "neqa_3_9" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(DB(14),DB(18))))); (* "nIna_9_8" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(17)),Ap(DB(14),DB(18))))); (* "nIna_11_8" *)
       (Ap(Prim(3),Eq(Base(0),DB(18),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))); (* "neqa_3_12" *)
       (Ap(Prim(3),Eq(Base(0),DB(18),Ap(DB(15),DB(18))))); (* "neqa_3_15" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(DB(14),DB(18))))); (* "nIna_15_8" *)
       (Ap(Prim(3),Eq(Base(0),DB(18),Ap(DB(14),DB(18))))); (* "neqa_3_8" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(DB(14),DB(18))))); (* "nSubqa_9_8" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(DB(15),DB(19))),Eq(Base(0),DB(0),DB(19))))); (* "Ina_8_inv" *)
       (Ap(Prim(3),Ap(Ap(DB(22),DB(17)),Ap(DB(14),DB(18))))); (* "nSubqa_11_8" *)
       (Ap(Ap(DB(22),Ap(DB(14),DB(18))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "Subqa_8_14" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(DB(15),DB(18))),Ap(DB(14),DB(18))))); (* "nSubqa_15_8" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),Ap(DB(14),DB(19)))),DB(17)))); (* "nIna_4_11" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(15),Ap(DB(14),DB(19)))),DB(17)))); (* "nIna_5_11" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(18))),DB(17)))); (* "nIna_8_11" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(18))),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))); (* "nIna_8_9" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(17)),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))); (* "nIna_11_9" *)
       (Ap(Prim(3),Eq(Base(0),DB(19),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))); (* "neqa_1_12" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),DB(17)))); (* "nIna_12_11" *)
       (Ap(Prim(3),Eq(Base(0),DB(20),Ap(DB(15),DB(18))))); (* "neqa_0_15" *)
       (Ap(Prim(3),Eq(Base(0),DB(19),Ap(DB(15),DB(18))))); (* "neqa_1_15" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(15),DB(18))),DB(17)))); (* "nIna_15_11" *)
       (Ap(Prim(3),Eq(Base(0),Ap(DB(14),DB(18)),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))); (* "neqa_8_9" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),DB(18)),Ap(DB(15),DB(20)))),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(19)))))); (* "Ina_9_inv" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19))))); (* "Subqa_9_13" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(DB(15),DB(18)))); (* "Subqa_9_15" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(DB(16),DB(19))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))),Eq(Base(0),DB(0),DB(19)))))); (* "Ina_15_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(20))))),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(20))),Eq(Base(0),DB(0),DB(19)))))); (* "Ina_10_inv" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "Subqa_10_14" *)
       (Ap(Prim(3),Eq(Base(0),Ap(DB(14),Ap(DB(14),DB(19))),DB(17)))); (* "neqa_4_11" *)
       (Ap(Prim(3),Eq(Base(0),Ap(DB(15),Ap(DB(14),DB(19))),DB(17)))); (* "neqa_5_11" *)
       (Ap(Prim(3),Eq(Base(0),Ap(DB(14),DB(18)),DB(17)))); (* "neqa_8_11" *)
       (Ap(Prim(3),Eq(Base(0),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))),DB(17)))); (* "neqa_9_11" *)
       (Ap(Ap(DB(22),DB(17)),Ap(DB(15),DB(18)))); (* "Subqa_11_15" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(18))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))); (* "nIna_8_12" *)
       (Ap(Ap(DB(21),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))); (* "disj_12_257" *)
       (Ap(Prim(3),Eq(Base(0),Ap(DB(14),DB(18)),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))); (* "neqa_8_12" *)
       (Ap(Prim(3),Eq(Base(0),DB(17),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))); (* "neqa_11_12" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))); (* "nSubqa_13_12" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19))))); (* "Subqa_12_13" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))); (* "nSubqa_14_12" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),DB(19))),Ap(Ap(Prim(5),Eq(Base(0),DB(0),Ap(DB(15),DB(20)))),Eq(Base(0),DB(0),DB(19)))))); (* "Ina_12_inv" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "Subqa_12_14" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(18))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))))); (* "nIna_8_13" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(17)),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))))); (* "nIna_11_13" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))))); (* "nIna_14_13" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))); (* "nIna_9_14" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(DB(15),DB(18))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))); (* "nSubqa_15_14" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),DB(18)))); (* "Subqa_14_15" *)
       (Ap(Prim(3),Eq(Base(0),Ap(DB(14),DB(18)),Ap(DB(15),DB(18))))); (* "neqa_8_15" *)
       (Ap(Prim(3),Eq(Base(0),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))),Ap(DB(15),DB(18))))); (* "neqa_9_15" *)
       (Ap(Prim(3),Eq(Base(0),DB(17),Ap(DB(15),DB(18))))); (* "neqa_11_15" *)
       (Ap(Prim(3),Eq(Base(0),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18)),Ap(DB(15),DB(18))))); (* "neqa_12_15" *)
       (Ap(Prim(3),Eq(Base(0),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(15),DB(18))))); (* "neqa_14_15" *)
       (Ap(Ap(DB(23),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "Ina_4_16" *)
       (Ap(Ap(DB(23),DB(20)),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "Ina_0_17" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "nIna_2_17" *)
       (Ap(Ap(DB(23),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "Ina_4_17" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "nIna_15_17" *)
       (Ap(Ap(DB(23),DB(20)),Ap(DB(15),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "Ina_0_51" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(20)),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19))))))); (* "nIna_0_32" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(11),Ap(DB(15),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19))))))); (* "Ina_0_19" *)
       (Ap(Ap(DB(23),DB(19)),Ap(DB(15),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "Ina_1_51" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(19)),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19))))))); (* "nIna_1_32" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(11),Ap(DB(15),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19))))))); (* "Ina_1_19" *)
       (Ap(Ap(DB(23),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(DB(15),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "Ina_4_51" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19))))))); (* "nIna_4_32" *)
       (Ap(Ap(DB(23),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),Ap(DB(15),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19))))))); (* "Ina_4_19" *)
       (Ap(Ap(DB(23),Ap(DB(15),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "Ina_5_32" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(15),Ap(DB(14),DB(19))))))); (* "nIna_15_51" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(Ap(DB(11),Ap(DB(15),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19)))))))); (* "nIna_15_19" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_2_20" *)
       (Ap(Ap(DB(23),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_4_20" *)
       (Ap(Ap(DB(23),DB(20)),Ap(DB(15),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_0_85" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(20)),Ap(DB(14),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))))); (* "nIna_0_64" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(11),Ap(DB(15),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))))); (* "Ina_0_21" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(DB(15),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_2_85" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(DB(14),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))))); (* "nIna_2_64" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(Ap(DB(11),Ap(DB(15),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))))); (* "Ina_2_21" *)
       (Ap(Ap(DB(23),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(DB(15),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_4_85" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))))); (* "nIna_4_64" *)
       (Ap(Ap(DB(23),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),Ap(DB(15),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))))); (* "Ina_4_21" *)
       (Ap(Ap(DB(23),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(DB(14),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_6_64" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_0_23" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_1_23" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_2_23" *)
       (Ap(Ap(DB(23),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_4_23" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_0_27" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_1_27" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))))); (* "nIna_2_27" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_3_27" *)
       (Ap(Ap(DB(23),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_4_27" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_0_29" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_2_29" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_3_29" *)
       (Ap(Ap(DB(23),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_4_29" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_1_30" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_2_30" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_3_30" *)
       (Ap(Ap(DB(23),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_4_30" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_0_31" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_1_31" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_2_31" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_3_31" *)
       (Ap(Ap(DB(23),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Ina_4_31" *)
       (Ap(Ap(DB(23),Ap(DB(15),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19))))))); (* "Ina_5_43" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "nIna_2_256" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))); (* "Ina_8_256" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "nIna_9_256" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(17)),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "nIna_11_256" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "nIna_12_256" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "nIna_15_256" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))); (* "Ina_8_257" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "nIna_9_257" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(17)),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "nIna_11_257" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "nIna_15_257" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "Ina_0_259" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "Ina_1_259" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "Ina_8_259" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(17)),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(15),Ap(DB(14),DB(18))))))); (* "nIna_11_259" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19))))); (* "Ina_0_2057" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(19))),DB(16)))); (* "nIna_2_2059" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19))))); (* "Ina_3_2057" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "Ina_8_271" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(18))),DB(16)))); (* "nIna_8_2059" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))))); (* "nIna_9_271" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(17)),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))))); (* "nIna_11_271" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))))); (* "nIna_12_271" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))))); (* "nIna_15_271" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "Ina_0_265" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))); (* "nIna_1_265" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))); (* "nIna_2_265" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "Ina_3_265" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "Ina_8_265" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(17)),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))); (* "nIna_11_265" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))); (* "nIna_15_265" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "Ina_0_267" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "Ina_1_267" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "Ina_3_267" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(18))),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "Ina_8_267" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(14),DB(18))))))); (* "nIna_15_267" *)
       (Ap(Ap(DB(23),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))); (* "Ina_12_4096" *)
       (Ap(Ap(DB(23),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))); (* "Ina_9_512" *)
       (Ap(Ap(DB(23),DB(20)),Ap(DB(15),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))); (* "Ina_0_771" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(18)),Ap(DB(15),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))))); (* "nIna_3_771" *)
       (Ap(Ap(DB(23),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(DB(15),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))); (* "Ina_9_771" *)
       (Ap(Ap(DB(23),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))))); (* "Ina_9_523" *)
       (Ap(Ap(DB(23),DB(19)),Ap(DB(15),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))); (* "Ina_1_771" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(20)),Ap(DB(14),DB(17))))); (* "nIna_0_2048" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(19)),Ap(DB(14),DB(17))))); (* "nIna_1_2048" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(18)),Ap(DB(14),DB(17))))); (* "nIna_3_2048" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(18))),Ap(DB(14),DB(17))))); (* "nIna_8_2048" *)
       (Ap(Ap(DB(23),DB(17)),Ap(DB(14),DB(17)))); (* "Ina_11_2048" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),DB(19)),Ap(DB(14),DB(17))))); (* "Ina_0_2049" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(13),DB(19)),Ap(DB(14),DB(17)))))); (* "nIna_3_2049" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(18))),Ap(Ap(DB(13),DB(19)),Ap(DB(14),DB(17)))))); (* "nIna_8_2049" *)
       (Ap(Ap(DB(21),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),DB(19)),Ap(DB(14),DB(17))))); (* "disj_264_2049" *)
       (Ap(Ap(DB(23),DB(17)),Ap(Ap(DB(13),DB(19)),Ap(DB(14),DB(17))))); (* "Ina_11_2049" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),DB(17))))); (* "Ina_1_2050" *)
       (Ap(Ap(DB(23),DB(17)),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),DB(17))))); (* "Ina_11_2050" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(18))))); (* "Ina_0_2051" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(18))))); (* "Ina_1_2051" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),Ap(DB(14),DB(19)))),DB(16)))); (* "nIna_4_2059" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(15),Ap(DB(14),DB(19)))),DB(16)))); (* "nIna_5_2059" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),DB(16)))); (* "nIna_9_2059" *)
       (Ap(Ap(DB(23),DB(17)),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(18))))); (* "Ina_11_2051" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),DB(16)))); (* "nIna_12_2059" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(15),DB(18))),DB(16)))); (* "nIna_15_2059" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(DB(14),DB(17)))))); (* "nIna_0_2052" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(DB(14),DB(17)))))); (* "nIna_1_2052" *)
       (Ap(Ap(DB(21),DB(18)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(DB(14),DB(17))))); (* "disj_3_2052" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),DB(16)))); (* "Ina_2_2063" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(11),DB(16)),DB(18))))); (* "nIna_0_2056" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(Ap(DB(11),DB(16)),DB(18))))); (* "nIna_2_2056" *)
       (Ap(Ap(DB(21),Ap(DB(15),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),DB(16)),DB(18)))); (* "disj_5_2056" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(11),DB(16)),DB(18)))); (* "Ina_3_2056" *)
       (Ap(Ap(DB(23),DB(17)),Ap(Ap(DB(11),DB(16)),DB(18)))); (* "Ina_11_2056" *)
       (Ap(Ap(DB(23),DB(17)),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19))))); (* "Ina_11_2057" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(11),DB(16)),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "Ina_1_2058" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(11),DB(16)),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "Ina_3_2058" *)
       (Ap(Ap(DB(23),DB(17)),Ap(Ap(DB(11),DB(16)),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "Ina_11_2058" *)
       (Ap(Ap(DB(23),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),DB(16)))); (* "Ina_4_2075" *)
       (Ap(Ap(DB(23),Ap(DB(15),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19))))),DB(16)))); (* "Ina_5_2091" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17))))); (* "Ina_0_2305" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17)))))); (* "nIna_3_2305" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17))))); (* "Ina_8_2305" *)
       (Ap(Ap(DB(23),DB(17)),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17))))); (* "Ina_11_2305" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))); (* "Ina_1_2315" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))))); (* "Ina_1_2306" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))))); (* "nIna_2_2315" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))); (* "Ina_8_2315" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(18))),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))))); (* "Ina_8_2306" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))))); (* "nIna_9_2315" *)
       (Ap(Ap(DB(23),DB(17)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))); (* "Ina_11_2315" *)
       (Ap(Ap(DB(23),DB(17)),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))))); (* "Ina_11_2306" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))))); (* "nIna_12_2315" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))))); (* "nIna_15_2315" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17))))))); (* "nIna_3_2311" *)
       (Ap(Ap(DB(23),DB(17)),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17)))))); (* "Ina_11_2311" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))); (* "Ina_0_2315" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))); (* "Ina_3_2315" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))))); (* "Ina_0_2319" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))))); (* "Ina_2_2319" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))))); (* "Ina_3_2319" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))))); (* "nIna_9_2319" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))))); (* "nIna_12_2319" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))))); (* "nIna_15_2319" *)
       (Ap(Ap(DB(23),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18)))))); (* "Ina_12_4107" *)
       (Ap(Ap(DB(23),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18)))))); (* "Ina_12_6155" *)
       (Ap(Ap(DB(23),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))); (* "Ina_14_16384" *)
       (Ap(Ap(DB(23),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))))); (* "Ina_14_16391" *)
       (Ap(Ap(DB(23),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))))); (* "Ina_14_16397" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(20)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "nIna_0_32768" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(19)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "nIna_1_32768" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "nIna_2_32768" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(18))),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "nIna_8_32768" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "nIna_9_32768" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(17)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "nIna_11_32768" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "nIna_12_32768" *)
       (Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(15),DB(18))))); (* "Ina_15_32768" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),DB(19)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Ina_0_32769" *)
       (Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(Ap(DB(13),DB(19)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Ina_15_32769" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),DB(18)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Ina_0_32771" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(13),DB(18)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Ina_1_32771" *)
       (Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(Ap(DB(13),DB(18)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Ina_15_32771" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(17)),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "nIna_11_32783" *)
       (Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Ina_15_32783" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Ina_0_32779" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Ina_1_32779" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Ina_3_32779" *)
       (Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Ina_15_32779" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),DB(19)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_0_32785" *)
       (Ap(Ap(DB(23),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),DB(19)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_4_32785" *)
       (Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),DB(19)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_15_32785" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),DB(18)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_0_32787" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),DB(18)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_1_32787" *)
       (Ap(Ap(DB(23),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),DB(18)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_4_32787" *)
       (Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),DB(18)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_15_32787" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Ina_8_33024" *)
       (Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Ina_15_33024" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Ina_0_33025" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "nIna_9_33025" *)
       (Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Ina_15_33025" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_1_33027" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_3_33033" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))))); (* "nIna_9_33033" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_1_33034" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_3_33034" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_8_33034" *)
       (Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_15_33034" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_0_33039" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_1_33039" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_3_33039" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))))); (* "nIna_9_33039" *)
       (Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_15_33039" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),Ap(DB(14),DB(17))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "nIna_0_34816" *)
       (Ap(Prim(3),Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(13),Ap(DB(14),DB(17))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "nIna_1_34816" *)
       (Ap(Ap(DB(21),DB(18)),Ap(Ap(DB(13),Ap(DB(14),DB(17))),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "disj_3_34816" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Ina_0_34827" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "nIna_2_34827" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(18))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "nIna_8_34827" *)
       (Ap(Ap(DB(23),DB(17)),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Ina_11_34827" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "nIna_12_34827" *)
       (Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Ina_15_34827" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Ina_1_34827" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Ina_3_34827" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_0_34831" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_1_34831" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_2_34831" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_3_34831" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))))); (* "nIna_8_34831" *)
       (Ap(Ap(DB(23),DB(17)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_11_34831" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))))); (* "nIna_12_34831" *)
       (Ap(Ap(DB(23),DB(20)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_0_35083" *)
       (Ap(Ap(DB(23),DB(19)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_1_35083" *)
       (Ap(Prim(3),Ap(Ap(DB(23),Ap(DB(14),DB(19))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))))); (* "nIna_2_35083" *)
       (Ap(Ap(DB(23),DB(18)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_3_35083" *)
       (Ap(Ap(DB(23),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_8_35083" *)
       (Ap(Ap(DB(23),DB(17)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_11_35083" *)
       (Ap(Ap(DB(23),Ap(DB(15),DB(18))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Ina_15_35083" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(20))),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))))); (* "Ina_6_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0)),Imp(Ap(Ap(DB(24),DB(20)),DB(0)),Ap(Ap(DB(23),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))),DB(0)))))); (* "Subqa_6_inv_lem1_Subq" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))),Imp(Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0)),Imp(Ap(Ap(DB(24),DB(20)),DB(0)),Eq(Base(0),DB(0),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20)))))))))); (* "Subqa_6_inv_lem1" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))),Imp(Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0)),Imp(Ap(Ap(DB(24),DB(20)),DB(0)),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))),Eq(Base(0),DB(0),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))))))))); (* "Subqa_6_inv_lem2" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(20)),DB(0))),Ap(Ap(DB(23),DB(0)),Ap(DB(15),Ap(DB(15),DB(20)))))))); (* "Subqa_6_inv_lem3_Subq" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))),Imp(Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0)),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(20)),DB(0))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20))))))))); (* "Subqa_6_inv_lem3" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))),Imp(Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0)),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(20)),DB(0))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))),Eq(Base(0),DB(0),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))))))))); (* "Subqa_6_inv_lem4" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))),Imp(Ap(Prim(3),Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0))),Ap(Ap(DB(23),DB(0)),Ap(DB(15),DB(20))))))); (* "Subqa_6_inv_lem5_Subq" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))),Imp(Ap(Prim(3),Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0))),Imp(Ap(Ap(DB(24),DB(20)),DB(0)),Eq(Base(0),DB(0),Ap(DB(15),DB(20)))))))); (* "Subqa_6_inv_lem5" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))),Imp(Ap(Prim(3),Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0))),Imp(Ap(Ap(DB(24),DB(20)),DB(0)),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))),Eq(Base(0),DB(0),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))))))))); (* "Subqa_6_inv_lem6" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))),Imp(Ap(Prim(3),Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0))),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(20)),DB(0))),Eq(Base(0),DB(0),DB(21))))))); (* "Subqa_6_inv_lem7" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))),Imp(Ap(Prim(3),Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0))),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(20)),DB(0))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))),Eq(Base(0),DB(0),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))))))))); (* "Subqa_6_inv_lem8" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))),Imp(Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0)),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))),Eq(Base(0),DB(0),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20)))))))))); (* "Subqa_6_inv_lem9" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))),Imp(Ap(Prim(3),Ap(Ap(DB(24),Ap(DB(15),DB(20))),DB(0))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))),Eq(Base(0),DB(0),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20)))))))))); (* "Subqa_6_inv_lem10" *)
       (All(Base(0),Imp(Ap(Ap(DB(23),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))),Eq(Base(0),DB(0),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))))))); (* "Subqa_6_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(15),DB(19)))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))))); (* "Ina_7_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(15),DB(20)))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))),Eq(Base(0),DB(0),DB(19)))))); (* "Ina_13_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(20))),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))),Eq(Base(0),DB(0),DB(19)))))); (* "Ina_14_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(DB(15),Ap(DB(15),Ap(DB(15),DB(20))))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20))))))); (* "Ina_16_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(DB(16),Ap(DB(15),Ap(DB(15),DB(20))))),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))))); (* "Ina_17_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(DB(16),Ap(DB(16),Ap(DB(15),DB(20))))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))),Eq(Base(0),DB(0),Ap(DB(16),Ap(DB(15),DB(20)))))))); (* "Ina_51_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),Ap(DB(16),Ap(DB(15),DB(20))))),Ap(DB(15),Ap(DB(16),Ap(DB(15),DB(20)))))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))))); (* "Ina_19_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),Ap(DB(15),DB(20)))),Ap(DB(15),Ap(DB(15),Ap(DB(15),DB(20)))))),Ap(Ap(Prim(5),Eq(Base(0),DB(0),Ap(DB(15),DB(20)))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))))); (* "Ina_20_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(DB(16),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20)))))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))),Eq(Base(0),DB(0),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))))))); (* "Ina_85_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20)))))),Ap(DB(15),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))))); (* "Ina_21_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))),Ap(DB(16),Ap(DB(15),Ap(DB(15),DB(20)))))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))))); (* "Ina_23_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(20))))),Ap(DB(16),Ap(DB(15),Ap(DB(15),DB(20)))))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))),Eq(Base(0),DB(0),DB(19)))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))))); (* "Ina_27_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),DB(19))),Ap(DB(16),Ap(DB(15),Ap(DB(15),DB(20)))))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))),Eq(Base(0),DB(0),DB(19)))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))))); (* "Ina_29_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(DB(15),Ap(DB(15),Ap(DB(15),DB(20)))))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(20))),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))),Eq(Base(0),DB(0),DB(19)))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))))); (* "Ina_30_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(DB(16),Ap(DB(15),Ap(DB(15),DB(20)))))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))),Eq(Base(0),DB(0),DB(19)))),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))))); (* "Ina_31_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(17)),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(19))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))),Eq(Base(0),DB(0),DB(18))))))); (* "Ina_2051_inv_lem" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),DB(17)),Ap(DB(15),DB(19)))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(20)))),Eq(Base(0),DB(0),DB(18)))))); (* "Ina_2051_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(17)),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(0)),DB(19))),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(19))),Eq(Base(0),DB(0),DB(18))))))); (* "Ina_2056_inv_lem" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),DB(17)),DB(19))),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(19))),Eq(Base(0),DB(0),DB(18)))))); (* "Ina_2056_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(17)),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(20))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(19)))),Eq(Base(0),DB(0),DB(18))))))); (* "Ina_2057_inv_lem" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),DB(17)),Ap(DB(15),DB(20)))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(21))),Eq(Base(0),DB(0),DB(19)))),Eq(Base(0),DB(0),DB(18)))))); (* "Ina_2057_inv" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(17)),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(0)),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(20))),Eq(Base(0),DB(0),DB(19)))),Eq(Base(0),DB(0),DB(18))))))); (* "Ina_2058_inv_lem" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),DB(17)),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(Ap(Prim(5),Ap(Ap(Prim(5),Eq(Base(0),DB(0),DB(20))),Eq(Base(0),DB(0),DB(19)))),Eq(Base(0),DB(0),DB(18)))))); (* "Ina_2058_inv" *)
       (Ap(Ap(DB(22),DB(18)),Ap(Ap(DB(11),Ap(DB(15),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19))))))); (* "Subqa_3_19" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19)))))),DB(18)))); (* "nSubqa_19_3" *)
       (Ap(Ap(DB(22),DB(18)),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "Subqa_3_259" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(15),Ap(DB(14),DB(18))))),DB(18)))); (* "nSubqa_259_3" *)
       (Ap(Ap(DB(22),DB(18)),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(18))))); (* "Subqa_3_2051" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(18)))),DB(18)))); (* "nSubqa_2051_3" *)
       (Ap(Ap(DB(22),DB(18)),Ap(Ap(DB(13),DB(18)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Subqa_3_32771" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),DB(18)),Ap(DB(14),Ap(DB(15),DB(18))))),DB(18)))); (* "nSubqa_32771_3" *)
       (Ap(Ap(DB(22),DB(17)),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Subqa_11_27" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))),DB(17)))); (* "nSubqa_27_11" *)
       (Ap(Ap(DB(22),DB(17)),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19))))))); (* "Subqa_11_43" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19)))))),DB(17)))); (* "nSubqa_43_11" *)
       (Ap(Ap(DB(22),DB(17)),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "Subqa_11_267" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(14),DB(18))))),DB(17)))); (* "nSubqa_267_11" *)
       (Ap(Ap(DB(22),DB(17)),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18)))))); (* "Subqa_11_4107" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))),DB(17)))); (* "nSubqa_4107_11" *)
       (Ap(Ap(DB(22),DB(17)),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Subqa_11_32779" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(15),DB(18))))),DB(17)))); (* "nSubqa_32779_11" *)
       (Ap(Ap(DB(22),DB(17)),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Subqa_11_33039" *)
       (Ap(Ap(DB(22),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Subqa_256_33039" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Subqa_267_33039" *)
       (Ap(Ap(DB(22),DB(16)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),DB(16)))); (* "Subqa_2059_2063" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),DB(16))),DB(16)))); (* "nSubqa_2063_2059" *)
       (Ap(Ap(DB(22),DB(16)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),DB(16)))); (* "Subqa_2059_2075" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),DB(16))),DB(16)))); (* "nSubqa_2075_2059" *)
       (Ap(Ap(DB(22),DB(16)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19))))),DB(16)))); (* "Subqa_2059_2091" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19))))),DB(16))),DB(16)))); (* "nSubqa_2091_2059" *)
       (Ap(Ap(DB(22),DB(16)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))); (* "Subqa_2059_2315" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))),DB(16)))); (* "nSubqa_2315_2059" *)
       (Ap(Ap(DB(22),DB(16)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))))); (* "Subqa_2059_2319" *)
       (Ap(Ap(DB(22),DB(16)),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18)))))); (* "Subqa_2059_6155" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))),DB(16)))); (* "nSubqa_6155_2059" *)
       (Ap(Ap(DB(22),DB(16)),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Subqa_2059_34827" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))),DB(16)))); (* "nSubqa_34827_2059" *)
       (Ap(Ap(DB(22),DB(16)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Subqa_2059_34831" *)
       (Ap(Ap(DB(22),DB(16)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Subqa_2059_35083" *)
       (Ap(Ap(DB(22),DB(17)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Subqa_11_35083" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Subqa_267_35083" *)
       (Ap(Ap(DB(22),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),DB(19)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Subqa_17_32785" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),DB(19)),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "nSubqa_32785_17" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19))))); (* "Subqa_9_2057" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))); (* "nSubqa_2057_9" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "Subqa_9_265" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))); (* "nSubqa_265_9" *)
       (Ap(Ap(DB(22),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(11),Ap(DB(15),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))))); (* "Subqa_17_21" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "nSubqa_21_17" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19)))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),DB(18)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Subqa_19_32787" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),DB(18)),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(Ap(DB(11),Ap(DB(15),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19)))))))); (* "nSubqa_32787_19" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))))); (* "Subqa_13_16397" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))))); (* "nSubqa_16397_13" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))))); (* "Subqa_7_16391" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18)))))); (* "nSubqa_16391_7" *)
       (Ap(Ap(DB(22),Ap(DB(15),DB(18))),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Subqa_15_32783" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(DB(15),DB(18))))); (* "nSubqa_32783_15" *)
       (Ap(Ap(DB(22),Ap(DB(15),DB(18))),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "Subqa_15_271" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(DB(15),DB(18))))); (* "nSubqa_271_15" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Subqa_10_31" *)
       (Ap(Ap(DB(22),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Subqa_17_31" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "Subqa_27_31" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))))); (* "nSubqa_31_27" *)
       (Ap(Ap(DB(22),Ap(DB(15),DB(18))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Subqa_15_34831" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Subqa_34827_35083" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "nSubqa_35083_34827" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Subqa_34827_34831" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "nSubqa_34831_34827" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))))); (* "Subqa_2315_2319" *)
       (Ap(Prim(3),Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))))); (* "nSubqa_2319_2315" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(18)),Ap(DB(14),DB(20)))),Ap(DB(14),DB(19)))); (* "remove_elt_a_0_3_Subq_1" *)
       (Ap(Ap(DB(22),Ap(DB(14),DB(19))),Ap(Ap(DB(11),DB(18)),Ap(DB(14),DB(20))))); (* "remove_elt_a_0_3_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),DB(18)),Ap(DB(14),DB(20))),Ap(DB(14),DB(19)))); (* "remove_elt_a_0_3_eq" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(18)),Ap(DB(14),DB(19)))),DB(19))); (* "remove_elt_a_1_3_Subq_1" *)
       (Ap(Ap(DB(22),DB(19)),Ap(Ap(DB(11),DB(18)),Ap(DB(14),DB(19))))); (* "remove_elt_a_1_3_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),DB(18)),Ap(DB(14),DB(19))),DB(19))); (* "remove_elt_a_1_3_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(18)),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(21))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(20))))))))); (* "remove_elt_a_0_11_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(20)))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "remove_elt_a_0_11_Subq_1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(20))))); (* "remove_elt_a_0_11_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(20))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "remove_elt_a_0_11_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(18)),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(19))),Ap(Ap(DB(24),DB(0)),DB(19)))))); (* "remove_elt_a_3_11_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(18)))),DB(18))); (* "remove_elt_a_3_11_Subq_1" *)
       (Ap(Ap(DB(22),DB(18)),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(18))))); (* "remove_elt_a_3_11_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(18))),DB(18))); (* "remove_elt_a_3_11_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(15),DB(19)))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(21))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))))))); (* "remove_elt_a_0_7_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18)))),Ap(DB(14),DB(20)))),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "remove_elt_a_0_7_Subq_1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18)))),Ap(DB(14),DB(20))))); (* "remove_elt_a_0_7_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18)))),Ap(DB(14),DB(20))),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "remove_elt_a_0_7_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(15),DB(19)))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(20))),Ap(Ap(DB(24),DB(0)),Ap(DB(16),Ap(DB(15),DB(20)))))))); (* "remove_elt_a_1_7_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18)))),Ap(DB(14),DB(19)))),Ap(DB(15),Ap(DB(14),DB(19))))); (* "remove_elt_a_1_7_Subq_1" *)
       (Ap(Ap(DB(22),Ap(DB(15),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18)))),Ap(DB(14),DB(19))))); (* "remove_elt_a_1_7_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18)))),Ap(DB(14),DB(19))),Ap(DB(15),Ap(DB(14),DB(19))))); (* "remove_elt_a_1_7_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(15),DB(19)))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),Ap(DB(15),DB(20)))),Ap(Ap(DB(24),DB(0)),DB(19)))))); (* "remove_elt_a_2_7_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(14),DB(19))))),DB(18))); (* "remove_elt_a_2_7_Subq_1" *)
       (Ap(Ap(DB(22),DB(18)),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "remove_elt_a_2_7_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(14),DB(19)))),DB(18))); (* "remove_elt_a_2_7_eq" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(DB(14),DB(20)))),Ap(DB(14),DB(18)))); (* "remove_elt_a_0_9_Subq_1" *)
       (Ap(Ap(DB(22),Ap(DB(14),DB(18))),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(DB(14),DB(20))))); (* "remove_elt_a_0_9_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(DB(14),DB(20))),Ap(DB(14),DB(18)))); (* "remove_elt_a_0_9_eq" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(DB(14),DB(18)))),DB(19))); (* "remove_elt_a_3_9_Subq_1" *)
       (Ap(Ap(DB(22),DB(19)),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(DB(14),DB(18))))); (* "remove_elt_a_3_9_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(DB(14),DB(18))),DB(19))); (* "remove_elt_a_3_9_eq" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(14),DB(19)))),Ap(DB(14),DB(18)))); (* "remove_elt_a_1_10_Subq_1" *)
       (Ap(Ap(DB(22),Ap(DB(14),DB(18))),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(14),DB(19))))); (* "remove_elt_a_1_10_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(14),DB(19))),Ap(DB(14),DB(18)))); (* "remove_elt_a_1_10_eq" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(14),DB(18)))),Ap(DB(14),DB(19)))); (* "remove_elt_a_3_10_Subq_1" *)
       (Ap(Ap(DB(22),Ap(DB(14),DB(19))),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(14),DB(18))))); (* "remove_elt_a_3_10_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(14),DB(18))),Ap(DB(14),DB(19)))); (* "remove_elt_a_3_10_eq" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(DB(14),DB(18)))); (* "remove_elt_a_2_12_Subq_1" *)
       (Ap(Ap(DB(22),Ap(DB(14),DB(18))),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "remove_elt_a_2_12_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(DB(14),DB(18)))); (* "remove_elt_a_2_12_eq" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(14),DB(19))))); (* "remove_elt_a_3_12_Subq_1" *)
       (Ap(Ap(DB(22),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(DB(14),DB(18))))); (* "remove_elt_a_3_12_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(DB(14),DB(18))),Ap(DB(14),Ap(DB(14),DB(19))))); (* "remove_elt_a_3_12_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(15),DB(20)))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(21))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),DB(19))))))); (* "remove_elt_a_0_13_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))),Ap(DB(14),DB(20)))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18)))); (* "remove_elt_a_0_13_Subq_1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))),Ap(DB(14),DB(20))))); (* "remove_elt_a_0_13_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))),Ap(DB(14),DB(20))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18)))); (* "remove_elt_a_0_13_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(15),DB(20)))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),Ap(DB(15),DB(20)))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),DB(18)),Ap(DB(15),DB(20)))))))); (* "remove_elt_a_2_13_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))); (* "remove_elt_a_2_13_Subq_1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "remove_elt_a_2_13_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))); (* "remove_elt_a_2_13_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(15),DB(20)))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(19))),Ap(Ap(DB(24),DB(0)),Ap(DB(16),Ap(DB(15),DB(20)))))))); (* "remove_elt_a_3_13_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))),Ap(DB(14),DB(18)))),Ap(DB(15),Ap(DB(14),DB(19))))); (* "remove_elt_a_3_13_Subq_1" *)
       (Ap(Ap(DB(22),Ap(DB(15),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))),Ap(DB(14),DB(18))))); (* "remove_elt_a_3_13_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))); (* "remove_elt_a_3_13_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(20))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),DB(19))))))); (* "remove_elt_a_1_14_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18)))); (* "remove_elt_a_1_14_Subq_1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),DB(19))))); (* "remove_elt_a_1_14_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),DB(19))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18)))); (* "remove_elt_a_1_14_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),Ap(DB(15),DB(20)))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(20))))))))); (* "remove_elt_a_2_14_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "remove_elt_a_2_14_Subq_1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "remove_elt_a_2_14_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "remove_elt_a_2_14_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(19))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))))))); (* "remove_elt_a_3_14_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "remove_elt_a_3_14_Subq_1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),DB(18))))); (* "remove_elt_a_3_14_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "remove_elt_a_3_14_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(DB(16),DB(19))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(21))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))))))); (* "remove_elt_a_0_15_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(20)))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "remove_elt_a_0_15_Subq_1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(20))))); (* "remove_elt_a_0_15_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(20))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "remove_elt_a_0_15_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(DB(16),DB(19))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),Ap(DB(15),DB(20)))),Ap(Ap(DB(24),DB(0)),DB(18)))))); (* "remove_elt_a_2_15_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(19))))),DB(17))); (* "remove_elt_a_2_15_Subq_1" *)
       (Ap(Ap(DB(22),DB(17)),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "remove_elt_a_2_15_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(19)))),DB(17))); (* "remove_elt_a_2_15_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20)))))),Ap(DB(15),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(21))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),Ap(DB(15),DB(20)))),Ap(DB(15),Ap(DB(15),Ap(DB(15),DB(20)))))))))); (* "remove_elt_a_0_21_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))),Ap(DB(14),DB(20)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "remove_elt_a_0_21_Subq_1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))),Ap(DB(14),DB(20))))); (* "remove_elt_a_0_21_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))),Ap(DB(14),DB(20))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "remove_elt_a_0_21_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20)))))),Ap(DB(15),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),Ap(DB(15),DB(20)))),Ap(Ap(DB(24),DB(0)),Ap(DB(16),Ap(DB(15),Ap(DB(15),DB(20))))))))); (* "remove_elt_a_2_21_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "remove_elt_a_2_21_Subq_1" *)
       (Ap(Ap(DB(22),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "remove_elt_a_2_21_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "remove_elt_a_2_21_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20)))))),Ap(DB(15),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20))))),Ap(Ap(DB(24),DB(0)),Ap(DB(16),Ap(DB(15),DB(20)))))))); (* "remove_elt_a_4_21_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(15),Ap(DB(14),DB(19))))); (* "remove_elt_a_4_21_Subq_1" *)
       (Ap(Ap(DB(22),Ap(DB(15),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "remove_elt_a_4_21_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),DB(19))))); (* "remove_elt_a_4_21_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(DB(16),Ap(DB(15),Ap(DB(15),DB(20)))))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(21))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(DB(15),Ap(DB(15),Ap(DB(15),DB(20)))))))))); (* "remove_elt_a_0_31_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),DB(20)))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "remove_elt_a_0_31_Subq_1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),DB(20))))); (* "remove_elt_a_0_31_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),DB(20))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "remove_elt_a_0_31_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(DB(16),Ap(DB(15),Ap(DB(15),DB(20)))))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(20))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),DB(19))),Ap(DB(16),Ap(DB(15),Ap(DB(15),DB(20)))))))))); (* "remove_elt_a_1_31_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "remove_elt_a_1_31_Subq_1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),DB(19))))); (* "remove_elt_a_1_31_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),DB(19))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "remove_elt_a_1_31_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(DB(16),Ap(DB(15),Ap(DB(15),DB(20)))))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),Ap(DB(15),DB(20)))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(20))))),Ap(DB(16),Ap(DB(15),Ap(DB(15),DB(20)))))))))); (* "remove_elt_a_2_31_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "remove_elt_a_2_31_Subq_1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "remove_elt_a_2_31_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "remove_elt_a_2_31_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(DB(16),Ap(DB(15),Ap(DB(15),DB(20)))))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(19))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))),Ap(DB(16),Ap(DB(15),Ap(DB(15),DB(20)))))))))); (* "remove_elt_a_3_31_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "remove_elt_a_3_31_Subq_1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),DB(18))))); (* "remove_elt_a_3_31_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "remove_elt_a_3_31_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(DB(16),Ap(DB(15),Ap(DB(15),DB(20)))))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20))))),Ap(Ap(DB(24),DB(0)),Ap(DB(16),DB(19))))))); (* "remove_elt_a_4_31_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(15),DB(18)))); (* "remove_elt_a_4_31_Subq_1" *)
       (Ap(Ap(DB(22),Ap(DB(15),DB(18))),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "remove_elt_a_4_31_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(DB(15),DB(18)))); (* "remove_elt_a_4_31_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),DB(17)),Ap(DB(15),DB(19)))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(21))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),DB(18)))))))); (* "remove_elt_a_0_2051_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(18)))),Ap(DB(14),DB(20)))),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),DB(17))))); (* "remove_elt_a_0_2051_Subq_1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),DB(17)))),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(18)))),Ap(DB(14),DB(20))))); (* "remove_elt_a_0_2051_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(18)))),Ap(DB(14),DB(20))),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),DB(17))))); (* "remove_elt_a_0_2051_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),DB(17)),Ap(DB(15),DB(19)))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(20))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),DB(20)),Ap(DB(15),DB(18)))))))); (* "remove_elt_a_1_2051_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(18)))),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(19)),Ap(DB(14),DB(17))))); (* "remove_elt_a_1_2051_Subq_1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),DB(19)),Ap(DB(14),DB(17)))),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(18)))),Ap(DB(14),DB(19))))); (* "remove_elt_a_1_2051_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(18)))),Ap(DB(14),DB(19))),Ap(Ap(DB(13),DB(19)),Ap(DB(14),DB(17))))); (* "remove_elt_a_1_2051_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),DB(17)),Ap(DB(15),DB(19)))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(18))),Ap(Ap(DB(24),DB(0)),DB(19)))))); (* "remove_elt_a_11_2051_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17)))),DB(18))); (* "remove_elt_a_11_2051_Subq_1" *)
       (Ap(Ap(DB(22),DB(18)),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17))))); (* "remove_elt_a_11_2051_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17))),DB(18))); (* "remove_elt_a_11_2051_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),DB(17)),DB(19))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(19))),Ap(Ap(DB(24),DB(0)),Ap(DB(15),DB(18))))))); (* "remove_elt_a_3_2056_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),DB(18))),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17)))); (* "remove_elt_a_3_2056_Subq_1" *)
       (Ap(Ap(DB(22),Ap(DB(14),DB(17))),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),DB(18))),Ap(DB(14),DB(18))))); (* "remove_elt_a_3_2056_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),DB(18))),Ap(DB(14),DB(18))),Ap(DB(14),DB(17)))); (* "remove_elt_a_3_2056_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),DB(17)),DB(19))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(18))),Ap(Ap(DB(24),DB(0)),Ap(DB(15),DB(19))))))); (* "remove_elt_a_11_2056_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),DB(18))),Ap(DB(14),DB(17)))),Ap(DB(14),DB(18)))); (* "remove_elt_a_11_2056_Subq_1" *)
       (Ap(Ap(DB(22),Ap(DB(14),DB(18))),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),DB(18))),Ap(DB(14),DB(17))))); (* "remove_elt_a_11_2056_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),DB(18))),Ap(DB(14),DB(17))),Ap(DB(14),DB(18)))); (* "remove_elt_a_11_2056_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),DB(17)),Ap(DB(15),DB(20)))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(21))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),DB(17)),DB(19))))))); (* "remove_elt_a_0_2057_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19)))),Ap(DB(14),DB(20)))),Ap(Ap(DB(11),DB(16)),DB(18)))); (* "remove_elt_a_0_2057_Subq_1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(16)),DB(18))),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19)))),Ap(DB(14),DB(20))))); (* "remove_elt_a_0_2057_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19)))),Ap(DB(14),DB(20))),Ap(Ap(DB(11),DB(16)),DB(18)))); (* "remove_elt_a_0_2057_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),DB(17)),Ap(DB(15),DB(20)))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(19))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),DB(20)),Ap(DB(15),DB(18)))))))); (* "remove_elt_a_3_2057_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19)))),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(19)),Ap(DB(14),DB(17))))); (* "remove_elt_a_3_2057_Subq_1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),DB(19)),Ap(DB(14),DB(17)))),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19)))),Ap(DB(14),DB(18))))); (* "remove_elt_a_3_2057_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19)))),Ap(DB(14),DB(18))),Ap(Ap(DB(13),DB(19)),Ap(DB(14),DB(17))))); (* "remove_elt_a_3_2057_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),DB(17)),Ap(DB(15),DB(20)))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(18))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),DB(18)),Ap(DB(15),DB(20)))))))); (* "remove_elt_a_11_2057_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19)))),Ap(DB(14),DB(17)))),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))); (* "remove_elt_a_11_2057_Subq_1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19)))),Ap(DB(14),DB(17))))); (* "remove_elt_a_11_2057_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19)))),Ap(DB(14),DB(17))),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))); (* "remove_elt_a_11_2057_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),DB(17)),Ap(DB(16),Ap(DB(15),DB(19))))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(20))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),DB(17)),DB(19))))))); (* "remove_elt_a_1_2058_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),DB(16)),DB(18)))); (* "remove_elt_a_1_2058_Subq_1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(16)),DB(18))),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),DB(19))))); (* "remove_elt_a_1_2058_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),DB(19))),Ap(Ap(DB(11),DB(16)),DB(18)))); (* "remove_elt_a_1_2058_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),DB(17)),Ap(DB(16),Ap(DB(15),DB(19))))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(19))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),DB(18)))))))); (* "remove_elt_a_3_2058_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),DB(17))))); (* "remove_elt_a_3_2058_Subq_1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),DB(17)))),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),DB(18))))); (* "remove_elt_a_3_2058_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),DB(17))))); (* "remove_elt_a_3_2058_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),DB(17)),Ap(DB(16),Ap(DB(15),DB(19))))),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(18))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(20))))))))); (* "remove_elt_a_11_2058_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),DB(17)))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "remove_elt_a_11_2058_Subq_1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),DB(17))))); (* "remove_elt_a_11_2058_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),DB(17))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "remove_elt_a_11_2058_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(17)),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(21))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),DB(17)),Ap(DB(16),Ap(DB(15),DB(19))))))))); (* "remove_elt_a_0_2059_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(20)))),Ap(Ap(DB(11),DB(16)),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "remove_elt_a_0_2059_Subq_1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(16)),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(20))))); (* "remove_elt_a_0_2059_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(20))),Ap(Ap(DB(11),DB(16)),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "remove_elt_a_0_2059_eq" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(17)),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(18))),Ap(Ap(DB(24),DB(0)),DB(18)))))); (* "remove_elt_a_11_2059_Subq_1_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(17)))),DB(17))); (* "remove_elt_a_11_2059_Subq_1" *)
       (Ap(Ap(DB(22),DB(17)),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(17))))); (* "remove_elt_a_11_2059_Subq_2" *)
       (Eq(Base(0),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(17))),DB(17))); (* "remove_elt_a_11_2059_eq" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "Subqa_267_271" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Subqa_265_33033" *)
       (Ap(Ap(DB(22),DB(17)),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "Subqa_11_271" *)
       (Ap(Ap(DB(22),DB(17)),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18)))))); (* "Subqa_11_6155" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "Subqa_265_271" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))))); (* "Subqa_265_777" *)
       (Ap(Ap(DB(22),DB(18)),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))))); (* "Subqa_3_523" *)
       (Ap(Ap(DB(22),DB(18)),Ap(DB(15),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))); (* "Subqa_3_771" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Subqa_9_34827" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "Subqa_259_2311_lem1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17)))))); (* "Subqa_259_2311_lem2" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17)))))); (* "Subqa_259_2311" *)
       (Ap(Ap(DB(22),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))); (* "Subqa_257_2315" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))); (* "Subqa_259_2315" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))); (* "Subqa_265_2315" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))))); (* "Subqa_265_2319" *)
       (Ap(Ap(DB(22),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Subqa_8_33039" *)
       (Ap(Ap(DB(22),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Subqa_257_33039" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Subqa_265_33039" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "Subqa_9_271" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),DB(16))); (* "Subqa_9_2059" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),DB(16)))); (* "Subqa_9_2063" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))); (* "Subqa_9_2315" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Subqa_9_32783" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),DB(16))); (* "Subqa_10_2059" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Subqa_10_34827" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(18)),Imp(Ap(Ap(DB(24),DB(0)),Ap(DB(16),Ap(Ap(DB(12),DB(18)),Ap(DB(15),DB(20))))),Ap(Ap(DB(24),DB(0)),DB(19)))))); (* "Subqa_m_i_523_771_3_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),Ap(DB(15),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),DB(18))); (* "Subqa_m_i_523_771_3" *)
       (Ap(Ap(DB(22),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "Subqa_257_267" *)
       (Eq(Base(0),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "eq_i_271_777_265" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(17)),Ap(Ap(Prim(5),Ap(Ap(DB(24),DB(0)),DB(18))),Eq(Base(0),DB(0),DB(18)))))); (* "Subq_4_3_except" *)
       (Eq(Base(0),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))),DB(17))); (* "eq_i_271_6155_11" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(16),DB(19))),Ap(DB(15),Ap(DB(15),DB(19))))),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(19))),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),Ap(DB(16),DB(19)))))),Ap(Prim(3),Ap(Ap(DB(24),DB(0)),Ap(DB(15),Ap(DB(16),DB(19))))))))); (* "eq_i_271_33033_265_lem1" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(16),DB(19))),Ap(DB(15),Ap(DB(15),DB(19))))),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(19))),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),Ap(DB(16),DB(19)))))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))))))); (* "eq_i_271_33033_265_lem2" *)
       (Eq(Base(0),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "eq_i_271_33033_265" *)
       (Eq(Base(0),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(DB(15),DB(18)))); (* "eq_i_271_34831_15" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(DB(16),DB(19))),Ap(Ap(Prim(5),Ap(Ap(DB(24),DB(0)),DB(18))),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))))); (* "Subqa_15_11_except" *)
       (Eq(Base(0),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "eq_i_271_35083_267" *)
       (Eq(Base(0),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "eq_i_777_2319_265" *)
       (Eq(Base(0),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "eq_i_777_33033_265" *)
       (Eq(Base(0),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "eq_i_777_33039_265" *)
       (Eq(Base(0),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))),DB(16))); (* "eq_i_2319_6155_2059" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),Ap(DB(15),DB(20)))),Ap(Ap(DB(14),Ap(DB(15),Ap(DB(15),DB(19)))),DB(17)))),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(19))),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),Ap(DB(16),DB(19)))))),Ap(Prim(3),Ap(Ap(DB(24),DB(0)),Ap(DB(15),Ap(DB(16),DB(19))))))))); (* "eq_i_2319_33033_265_lem1" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),Ap(DB(15),DB(20)))),Ap(Ap(DB(14),Ap(DB(15),Ap(DB(15),DB(19)))),DB(17)))),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(19))),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),Ap(DB(16),DB(19)))))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))))))); (* "eq_i_2319_33033_265_lem2" *)
       (Eq(Base(0),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "eq_i_2319_33033_265" *)
       (Eq(Base(0),Ap(Ap(DB(12),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))),DB(16))); (* "eq_i_6155_34831_2059" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19)))))); (* "Subq_9_i_271_2057" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19))))),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))); (* "Subq_i_271_2057_9" *)
       (Eq(Base(0),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))); (* "eq_i_271_2057_9" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),DB(16))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))); (* "Subq_i_2063_265_9" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),DB(16))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))); (* "Subq_9_i_2063_265" *)
       (Eq(Base(0),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),DB(16))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))); (* "eq_i_2063_265_9" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19))))),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))); (* "Subq_i_2315_13_9" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))))); (* "Subq_9_i_2315_13" *)
       (Eq(Base(0),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))); (* "eq_i_2315_13_9" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19))))),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))); (* "Subq_i_32783_2057_9" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19)))))); (* "Subq_9_i_32783_2057" *)
       (Eq(Base(0),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))); (* "eq_i_32783_2057_9" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(12),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "Subq_i_34827_14_10" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(Ap(DB(12),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))); (* "Subq_10_i_34827_14" *)
       (Eq(Base(0),Ap(Ap(DB(12),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "eq_i_34827_14_10" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(12),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))); (* "Subq_i_34827_265_9" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))),Ap(Ap(DB(12),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))); (* "Subq_9_i_34827_265" *)
       (Eq(Base(0),Ap(Ap(DB(12),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))); (* "eq_i_34827_265_9" *)
       (Ap(Prim(3),Ap(DB(10),DB(19)))); (* "atmost1_1" *)
       (Ap(Prim(3),Ap(DB(10),Ap(DB(14),DB(19))))); (* "atmost1_2" *)
       (Ap(Prim(3),Ap(DB(10),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "atmost1_4" *)
       (Ap(Prim(3),Ap(DB(10),Ap(DB(14),DB(18))))); (* "atmost1_8" *)
       (Ap(Prim(3),Ap(DB(10),Ap(DB(14),DB(17))))); (* "atmost1_2048" *)
       (Ap(Prim(3),Ap(DB(10),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "atmost1_32768" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(19)),Ap(Prim(3),Ap(DB(11),Ap(Ap(DB(12),DB(19)),Ap(DB(15),DB(0)))))))); (* "atmost2_3_lem" *)
       (Ap(Prim(3),Ap(DB(9),DB(18)))); (* "atmost2_3" *)
       (Ap(Prim(3),Ap(DB(9),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "atmost2_5" *)
       (Ap(Prim(3),Ap(DB(9),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "atmost2_6" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),DB(18)),Ap(DB(15),DB(20)))),Ap(Prim(3),Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(12),DB(18)),Ap(DB(15),DB(20)))),Ap(DB(15),DB(0)))))))); (* "atmost2_9_lem" *)
       (Ap(Prim(3),Ap(DB(9),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))); (* "atmost2_9" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(20))))),Ap(Prim(3),Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(20))))),Ap(DB(15),DB(0)))))))); (* "atmost2_10_lem" *)
       (Ap(Prim(3),Ap(DB(9),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))))); (* "atmost2_10" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),DB(19))),Ap(Prim(3),Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(12),Ap(DB(16),DB(19))),DB(19))),Ap(DB(15),DB(0)))))))); (* "atmost2_12_lem" *)
       (Ap(Prim(3),Ap(DB(9),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))); (* "atmost2_12" *)
       (Ap(Prim(3),Ap(DB(9),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "atmost2_17" *)
       (Ap(Prim(3),Ap(DB(9),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19)))))))); (* "atmost2_20" *)
       (Ap(Prim(3),Ap(DB(9),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "atmost2_257" *)
       (Ap(Prim(3),Ap(DB(9),Ap(Ap(DB(13),DB(19)),Ap(DB(14),DB(17)))))); (* "atmost2_2049" *)
       (Ap(Prim(3),Ap(DB(9),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),DB(17)))))); (* "atmost2_2050" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(17)),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(0)),DB(19))),Ap(Prim(3),Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(12),DB(17)),DB(19))),Ap(DB(15),DB(0))))))))); (* "atmost2_2056_lem" *)
       (Ap(Prim(3),Ap(DB(9),Ap(Ap(DB(11),DB(16)),DB(18))))); (* "atmost2_2056" *)
       (Ap(Prim(3),Ap(DB(9),Ap(Ap(DB(13),DB(19)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "atmost2_32769" *)
       (Ap(Prim(3),Ap(DB(9),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "atmost2_33024" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(15),DB(19)))),Ap(Prim(3),Ap(DB(10),Ap(Ap(DB(12),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(15),DB(19)))),Ap(DB(15),DB(0)))))))); (* "atmost3_7_lem" *)
       (Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18)))))); (* "atmost3_7" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(18)),Ap(Prim(3),Ap(DB(10),Ap(Ap(DB(12),DB(18)),Ap(DB(15),DB(0)))))))); (* "atmost3_11_lem" *)
       (Ap(Prim(3),Ap(DB(8),DB(17)))); (* "atmost3_11" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(15),DB(20)))),Ap(Prim(3),Ap(DB(10),Ap(Ap(DB(12),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(15),DB(20)))),Ap(DB(15),DB(0)))))))); (* "atmost3_13_lem" *)
       (Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))))); (* "atmost3_13" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(Prim(3),Ap(DB(10),Ap(Ap(DB(12),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(DB(15),DB(0)))))))); (* "atmost3_14_lem" *)
       (Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))); (* "atmost3_14" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20)))))),Ap(DB(15),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))))),Ap(Prim(3),Ap(DB(10),Ap(Ap(DB(12),Ap(Ap(DB(12),Ap(DB(16),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20)))))),Ap(DB(15),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))))),Ap(DB(15),DB(0)))))))); (* "atmost3_21_lem" *)
       (Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(11),Ap(DB(15),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))))); (* "atmost3_21" *)
       (Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(15),Ap(DB(14),DB(18))))))); (* "atmost3_259" *)
       (Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))); (* "atmost3_265" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(17)),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(19))),Ap(Prim(3),Ap(DB(10),Ap(Ap(DB(12),Ap(Ap(DB(12),DB(17)),Ap(DB(15),DB(19)))),Ap(DB(15),DB(0))))))))); (* "atmost3_2051_lem" *)
       (Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(18)))))); (* "atmost3_2051" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(17)),Imp(Ap(Prim(3),Eq(Base(0),DB(0),DB(20))),Ap(Prim(3),Ap(DB(10),Ap(Ap(DB(12),Ap(Ap(DB(12),DB(17)),Ap(DB(15),DB(20)))),Ap(DB(15),DB(0))))))))); (* "atmost3_2057_lem" *)
       (Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19)))))); (* "atmost3_2057" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(17)),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(0)),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(Prim(3),Ap(DB(10),Ap(Ap(DB(12),Ap(Ap(DB(12),DB(17)),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(DB(15),DB(0))))))))); (* "atmost3_2058_lem" *)
       (Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(11),DB(16)),Ap(DB(15),Ap(DB(14),DB(18))))))); (* "atmost3_2058" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),DB(18)))),Ap(Prim(3),Ap(DB(10),Ap(Ap(DB(12),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),DB(18)))),Ap(DB(15),DB(0)))))))); (* "atmost3_2305_lem" *)
       (Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17)))))); (* "atmost3_2305" *)
       (Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(13),DB(18)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "atmost3_32771" *)
       (Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),DB(19)),Ap(DB(14),Ap(DB(15),DB(18)))))))); (* "atmost3_32785" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),Ap(DB(16),DB(19))))),Ap(Prim(3),Ap(DB(10),Ap(Ap(DB(12),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),Ap(DB(16),DB(19))))),Ap(DB(15),DB(0)))))))); (* "atmost3_33025_lem" *)
       (Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "atmost3_33025" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(DB(16),DB(19))),Ap(Prim(3),Ap(DB(9),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(15),DB(0)))))))); (* "atmost4_15_lem" *)
       (Ap(Prim(3),Ap(DB(7),Ap(DB(15),DB(18))))); (* "atmost4_15" *)
       (Ap(Prim(3),Ap(DB(7),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))))); (* "atmost4_23" *)
       (Ap(Prim(3),Ap(DB(7),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))))); (* "atmost4_27" *)
       (Ap(Prim(3),Ap(DB(7),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))))); (* "atmost4_29" *)
       (Ap(Prim(3),Ap(DB(7),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19)))))))); (* "atmost4_30" *)
       (Ap(Prim(3),Ap(DB(7),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19)))))))); (* "atmost4_43" *)
       (Ap(Prim(3),Ap(DB(7),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(14),DB(18))))))); (* "atmost4_267" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(17)),Ap(Prim(3),Ap(DB(9),Ap(Ap(DB(12),DB(17)),Ap(DB(15),DB(0)))))))); (* "atmost4_2059_lem" *)
       (Ap(Prim(3),Ap(DB(7),DB(16)))); (* "atmost4_2059" *)
       (Ap(Prim(3),Ap(DB(7),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))))); (* "atmost4_4107" *)
       (Ap(Prim(3),Ap(DB(7),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))))); (* "atmost4_16391" *)
       (Ap(Prim(3),Ap(DB(7),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))))); (* "atmost4_16397" *)
       (Ap(Prim(3),Ap(DB(7),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "atmost4_32779" *)
       (Ap(Prim(3),Ap(DB(7),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),DB(18)),Ap(DB(14),Ap(DB(15),DB(18)))))))); (* "atmost4_32787" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),Ap(DB(16),DB(19)))))),Ap(Prim(3),Ap(DB(9),Ap(Ap(DB(12),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),Ap(DB(16),DB(19)))))),Ap(DB(15),DB(0)))))))); (* "atmost4_33027_lem" *)
       (Ap(Prim(3),Ap(DB(7),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))))); (* "atmost4_33027" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(19))),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),Ap(DB(16),DB(19)))))),Ap(Prim(3),Ap(DB(9),Ap(Ap(DB(12),Ap(Ap(DB(14),Ap(DB(15),DB(19))),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),Ap(DB(16),DB(19)))))),Ap(DB(15),DB(0)))))))); (* "atmost4_33033_lem" *)
       (Ap(Prim(3),Ap(DB(7),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))))); (* "atmost4_33033" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(20))))),Ap(Ap(DB(14),Ap(DB(15),Ap(DB(15),DB(19)))),Ap(DB(15),Ap(DB(16),DB(19)))))),Ap(Prim(3),Ap(DB(9),Ap(Ap(DB(12),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(20))))),Ap(Ap(DB(14),Ap(DB(15),Ap(DB(15),DB(19)))),Ap(DB(15),Ap(DB(16),DB(19)))))),Ap(DB(15),DB(0)))))))); (* "atmost4_33034_lem" *)
       (Ap(Prim(3),Ap(DB(7),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))))); (* "atmost4_33034" *)
       (All(Base(0),Imp(Ap(DB(8),Ap(Ap(DB(12),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(DB(16),Ap(DB(15),Ap(DB(15),DB(20)))))),Ap(DB(15),DB(0)))),Ap(Prim(3),Eq(Base(0),DB(0),DB(21)))))); (* "atmost5_31_lem1" *)
       (All(Base(0),Imp(Ap(DB(8),Ap(Ap(DB(12),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(DB(16),Ap(DB(15),Ap(DB(15),DB(20)))))),Ap(DB(15),DB(0)))),Ap(Prim(3),Eq(Base(0),DB(0),DB(20)))))); (* "atmost5_31_lem2" *)
       (All(Base(0),Imp(Ap(DB(8),Ap(Ap(DB(12),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(DB(16),Ap(DB(15),Ap(DB(15),DB(20)))))),Ap(DB(15),DB(0)))),Ap(Prim(3),Eq(Base(0),DB(0),Ap(DB(15),DB(20))))))); (* "atmost5_31_lem3" *)
       (All(Base(0),Imp(Ap(DB(8),Ap(Ap(DB(12),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(DB(16),Ap(DB(15),Ap(DB(15),DB(20)))))),Ap(DB(15),DB(0)))),Ap(Prim(3),Eq(Base(0),DB(0),DB(19)))))); (* "atmost5_31_lem4" *)
       (All(Base(0),Imp(Ap(DB(8),Ap(Ap(DB(12),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(DB(16),Ap(DB(15),Ap(DB(15),DB(20)))))),Ap(DB(15),DB(0)))),Ap(Prim(3),Eq(Base(0),DB(0),Ap(DB(15),Ap(DB(15),DB(20)))))))); (* "atmost5_31_lem5" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(DB(16),Ap(DB(15),Ap(DB(15),DB(20)))))),Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(12),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(DB(16),Ap(DB(15),Ap(DB(15),DB(20)))))),Ap(DB(15),DB(0)))))))); (* "atmost5_31_lem6" *)
       (Ap(Prim(3),Ap(DB(6),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))))); (* "atmost5_31" *)
       (Ap(Prim(3),Ap(DB(6),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))))); (* "atmost5_271" *)
       (Ap(Prim(3),Ap(DB(6),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),DB(16))))); (* "atmost5_2063" *)
       (Ap(Prim(3),Ap(DB(6),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),DB(16))))); (* "atmost5_2075" *)
       (Ap(Prim(3),Ap(DB(6),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19))))),DB(16))))); (* "atmost5_2091" *)
       (Ap(Prim(3),Ap(DB(6),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))))); (* "atmost5_2315" *)
       (Ap(Prim(3),Ap(DB(6),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))))); (* "atmost5_6155" *)
       (Ap(Prim(3),Ap(DB(6),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "atmost5_32783" *)
       (Ap(Prim(3),Ap(DB(6),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "atmost5_34827" *)
       (Ap(Prim(3),Ap(DB(5),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))))); (* "atmost6_2319" *)
       (Ap(Prim(3),Ap(DB(5),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))))); (* "atmost6_34831" *)
       (Ap(Prim(3),Ap(DB(5),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))))); (* "atmost6_35083" *)
       (Ap(DB(10),DB(18))); (* "atleast2_3" *)
       (Ap(DB(10),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "atleast2_6" *)
       (Ap(DB(10),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))); (* "atleast2_9" *)
       (Ap(DB(10),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "atleast2_10" *)
       (Ap(DB(10),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18)))); (* "atleast2_12" *)
       (Ap(DB(10),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "atleast2_17" *)
       (Ap(DB(10),Ap(Ap(DB(11),DB(16)),DB(18)))); (* "atleast2_2056" *)
       (Ap(DB(9),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18))))); (* "atleast3_7" *)
       (Ap(DB(9),DB(17))); (* "atleast3_11" *)
       (Ap(DB(9),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19))))); (* "atleast3_13" *)
       (Ap(DB(9),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "atleast3_14" *)
       (Ap(DB(9),Ap(Ap(DB(11),Ap(DB(15),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19))))))); (* "atleast3_19" *)
       (Ap(DB(9),Ap(Ap(DB(11),Ap(DB(15),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))))); (* "atleast3_21" *)
       (Ap(DB(9),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "atleast3_259" *)
       (Ap(DB(9),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "atleast3_265" *)
       (Ap(DB(9),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(18))))); (* "atleast3_2051" *)
       (Ap(DB(9),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19))))); (* "atleast3_2057" *)
       (Ap(DB(9),Ap(Ap(DB(13),DB(18)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "atleast3_32771" *)
       (Ap(DB(9),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),DB(19)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "atleast3_32785" *)
       (Ap(DB(8),Ap(DB(15),DB(18)))); (* "atleast4_15" *)
       (Ap(DB(8),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "atleast4_27" *)
       (Ap(DB(8),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19))))))); (* "atleast4_43" *)
       (Ap(DB(8),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "atleast4_267" *)
       (Ap(DB(8),DB(16))); (* "atleast4_2059" *)
       (Ap(DB(8),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18)))))); (* "atleast4_4107" *)
       (Ap(DB(8),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))))); (* "atleast4_16391" *)
       (Ap(DB(8),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))))); (* "atleast4_16397" *)
       (Ap(DB(8),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "atleast4_32779" *)
       (Ap(DB(8),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),DB(18)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "atleast4_32787" *)
       (Ap(DB(7),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "atleast5_31" *)
       (Ap(DB(7),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "atleast5_271" *)
       (Ap(DB(7),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),DB(16)))); (* "atleast5_2063" *)
       (Ap(DB(7),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),DB(16)))); (* "atleast5_2075" *)
       (Ap(DB(7),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19))))),DB(16)))); (* "atleast5_2091" *)
       (Ap(DB(7),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))); (* "atleast5_2315" *)
       (Ap(DB(7),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18)))))); (* "atleast5_6155" *)
       (Ap(DB(7),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "atleast5_32783" *)
       (Ap(DB(7),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "atleast5_34827" *)
       (Ap(DB(6),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))))); (* "atleast6_2319" *)
       (Ap(DB(6),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "atleast6_34831" *)
       (Ap(DB(6),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "atleast6_35083" *)
       (Ap(DB(9),Ap(Ap(DB(12),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),Ap(DB(15),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))))); (* "atleast3_i_523_771" *)
       (Ap(DB(7),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))))); (* "atleast5_i_33039_35083" *)
       (Ap(DB(4),DB(18))); (* "exactly2_3" *)
       (Ap(DB(4),Ap(DB(15),Ap(DB(14),DB(19))))); (* "exactly2_5" *)
       (Ap(DB(4),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "exactly2_6" *)
       (Ap(DB(4),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))); (* "exactly2_9" *)
       (Ap(DB(4),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19)))))); (* "exactly2_i_271_2057" *)
       (Ap(DB(4),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))))); (* "exactly2_i_2315_13" *)
       (Ap(DB(4),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19)))))); (* "exactly2_i_32783_2057" *)
       (Ap(DB(4),Ap(Ap(DB(12),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))); (* "exactly2_i_34827_265" *)
       (Ap(DB(4),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19)))))); (* "exactly2_10" *)
       (Ap(DB(4),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18)))); (* "exactly2_12" *)
       (Ap(DB(4),Ap(DB(15),Ap(DB(14),DB(18))))); (* "exactly2_257" *)
       (Ap(DB(4),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "exactly2_264" *)
       (Ap(DB(4),Ap(Ap(DB(13),DB(19)),Ap(DB(14),DB(17))))); (* "exactly2_2049" *)
       (Ap(DB(4),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(DB(14),DB(17))))); (* "exactly2_2052" *)
       (Ap(DB(4),Ap(Ap(DB(11),DB(16)),DB(18)))); (* "exactly2_2056" *)
       (Ap(DB(4),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),DB(16))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))); (* "exactly2_i_2063_265" *)
       (Ap(DB(4),Ap(Ap(DB(13),DB(19)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "exactly2_32769" *)
       (Ap(DB(4),Ap(Ap(DB(13),Ap(DB(14),DB(17))),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "exactly2_34816" *)
       (Ap(DB(4),Ap(Ap(DB(13),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "exactly2_49152" *)
       (Ap(DB(4),Ap(Ap(DB(12),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))); (* "exactly2_i_34827_14" *)
       (Ap(DB(3),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18))))); (* "exactly3_7" *)
       (Ap(DB(3),DB(17))); (* "exactly3_11" *)
       (Ap(DB(4),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(18))))); (* "exactly2_rem_11_3" *)
       (Ap(DB(3),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19))))); (* "exactly3_13" *)
       (Ap(DB(3),Ap(Ap(DB(11),Ap(DB(15),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))))); (* "exactly3_21" *)
       (Ap(DB(4),Ap(Ap(DB(11),Ap(Ap(DB(11),Ap(DB(15),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))))),Ap(DB(14),DB(20))))); (* "exactly2_rem_21_0" *)
       (Ap(DB(3),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "exactly3_265" *)
       (Ap(DB(3),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))))); (* "exactly3_i_271_777" *)
       (Ap(DB(3),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(18))))); (* "exactly3_2051" *)
       (Ap(DB(4),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(18)))),Ap(DB(14),DB(19))))); (* "exactly2_rem_2051_1" *)
       (Ap(DB(4),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17))))); (* "exactly2_rem_2051_11" *)
       (Ap(DB(3),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19))))); (* "exactly3_2057" *)
       (Ap(DB(4),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19)))),Ap(DB(14),DB(20))))); (* "exactly2_rem_2057_0" *)
       (Ap(DB(4),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19)))),Ap(DB(14),DB(18))))); (* "exactly2_rem_2057_3" *)
       (Ap(DB(4),Ap(Ap(DB(11),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19)))),Ap(DB(14),DB(17))))); (* "exactly2_rem_2057_11" *)
       (Ap(DB(3),Ap(Ap(DB(13),DB(18)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "exactly3_32771" *)
       (Ap(DB(3),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),DB(19)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "exactly3_32785" *)
       (Ap(DB(3),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))))); (* "exactly3_i_271_6155" *)
       (Ap(DB(3),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))))); (* "exactly3_i_271_33033" *)
       (Ap(DB(3),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))))); (* "exactly3_i_777_2319" *)
       (Ap(DB(3),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))))); (* "exactly3_i_777_33033" *)
       (Ap(DB(3),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))))); (* "exactly3_i_777_33039" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(18)),Imp(Ap(Ap(DB(24),DB(0)),Ap(DB(16),Ap(Ap(DB(12),DB(18)),Ap(DB(15),DB(20))))),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(0)),Ap(DB(15),DB(21)))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(Ap(DB(12),DB(18)),Ap(DB(15),DB(20))))))))))); (* "Subq_m_i_523_771_0_e_1_9_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),Ap(DB(15),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),Ap(DB(14),DB(20)))),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))))); (* "Subq_m_i_523_771_0_e_1_9" *)
       (Ap(Prim(3),Ap(DB(9),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),Ap(DB(15),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),Ap(DB(14),DB(20)))))); (* "atmost3_i_523_771_lem1a" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(18)),Imp(Ap(Ap(DB(24),DB(0)),Ap(DB(16),Ap(Ap(DB(12),DB(18)),Ap(DB(15),DB(20))))),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(0)),Ap(DB(15),DB(20)))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(21))),Ap(DB(15),Ap(Ap(DB(12),DB(18)),Ap(DB(15),DB(20))))))))))); (* "Subq_m_i_523_771_1_e_0_9_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),Ap(DB(15),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),DB(20))),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))))); (* "Subq_m_i_523_771_1_e_0_9" *)
       (Ap(Prim(3),Ap(DB(9),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),Ap(DB(15),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),Ap(DB(14),DB(19)))))); (* "atmost3_i_523_771_lem1b" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(18)),Imp(Ap(Ap(DB(24),DB(0)),Ap(DB(16),Ap(Ap(DB(12),DB(18)),Ap(DB(15),DB(20))))),Ap(Prim(3),Ap(DB(10),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(14),DB(18)),Ap(DB(15),Ap(Ap(DB(12),DB(18)),Ap(DB(15),DB(20)))))),Ap(DB(16),Ap(Ap(DB(12),DB(18)),Ap(DB(15),DB(20)))))),Ap(DB(15),DB(0))))))))); (* "atmost3_i_523_771_lem1" *)
       (Ap(Prim(3),Ap(DB(9),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),Ap(DB(15),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))))); (* "atmost3_i_523_771_lem2" *)
       (Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(12),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),Ap(DB(15),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))))); (* "atmost3_i_523_771" *)
       (Ap(DB(3),Ap(Ap(DB(12),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19)))))),Ap(DB(15),Ap(Ap(DB(11),DB(17)),Ap(DB(14),DB(19))))))); (* "exactly3_i_523_771" *)
       (Ap(DB(3),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))))); (* "exactly3_i_2319_33033" *)
       (Ap(DB(2),Ap(DB(15),DB(18)))); (* "exactly4_15" *)
       (Ap(DB(3),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(20))))); (* "exactly3_rem_15_0" *)
       (Ap(DB(3),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(19)))))); (* "exactly3_rem_15_2" *)
       (Ap(DB(2),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19))))))); (* "exactly4_43" *)
       (Ap(DB(2),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "exactly4_267" *)
       (Ap(DB(3),Ap(Ap(DB(11),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(DB(14),DB(18))))); (* "exactly3_rem_267_3" *)
       (Ap(DB(2),DB(16))); (* "exactly4_2059" *)
       (Ap(DB(3),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(20))))); (* "exactly3_rem_2059_0" *)
       (Ap(DB(3),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(17))))); (* "exactly3_rem_2059_11" *)
       (Ap(DB(2),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),DB(19)),Ap(DB(14),DB(17)))))); (* "exactly4_u_264_2049" *)
       (Ap(DB(2),Ap(Ap(DB(13),DB(18)),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(DB(14),DB(17)))))); (* "exactly4_u_3_2052" *)
       (Ap(DB(2),Ap(Ap(DB(13),DB(18)),Ap(Ap(DB(13),Ap(DB(14),DB(17))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "exactly4_u_3_34816" *)
       (Ap(DB(2),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(19)))),Ap(Ap(DB(11),DB(16)),DB(18))))); (* "exactly4_u_5_2056" *)
       (Ap(DB(2),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "exactly4_u_12_257" *)
       (Ap(DB(2),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18)))))); (* "exactly4_4107" *)
       (Ap(DB(2),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))))); (* "exactly4_16391" *)
       (Ap(DB(2),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18)))))))); (* "exactly4_16397" *)
       (Ap(DB(2),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "exactly4_32779" *)
       (Ap(DB(2),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),DB(18)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "exactly4_32787" *)
       (Ap(DB(2),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))))); (* "exactly4_i_271_34831" *)
       (Ap(DB(2),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))))); (* "exactly4_i_271_35083" *)
       (Ap(DB(2),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))))); (* "exactly4_i_2319_6155" *)
       (Ap(DB(2),Ap(Ap(DB(12),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))))); (* "exactly4_i_6155_34831" *)
       (Ap(DB(3),Ap(Ap(DB(11),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(DB(14),DB(20))))); (* "exactly3_rem_32779_0" *)
       (Ap(DB(3),Ap(Ap(DB(11),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(DB(14),DB(19))))); (* "exactly3_rem_32779_1" *)
       (Ap(DB(3),Ap(Ap(DB(11),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(DB(14),DB(18))))); (* "exactly3_rem_32779_3" *)
       (Ap(DB(3),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),DB(18)),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(DB(14),DB(20))))); (* "exactly3_rem_32787_0" *)
       (Ap(DB(1),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19))))))); (* "exactly5_31" *)
       (Ap(DB(2),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(DB(15),Ap(DB(14),Ap(DB(14),DB(19)))))),Ap(DB(14),DB(20))))); (* "exactly4_rem_31_0" *)
       (Ap(DB(1),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "exactly5_271" *)
       (Ap(DB(1),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),DB(16)))); (* "exactly5_2063" *)
       (Ap(DB(1),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),Ap(DB(14),DB(19))))),DB(16)))); (* "exactly5_2075" *)
       (Ap(DB(1),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(15),Ap(DB(14),DB(19))))),DB(16)))); (* "exactly5_2091" *)
       (Ap(DB(1),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))); (* "exactly5_2315" *)
       (Ap(DB(2),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))),Ap(DB(14),DB(19))))); (* "exactly4_rem_2315_1" *)
       (Ap(DB(2),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))),Ap(DB(14),DB(18))))); (* "exactly4_rem_2315_3" *)
       (Ap(DB(2),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))),Ap(DB(14),DB(17))))); (* "exactly4_rem_2315_11" *)
       (Ap(DB(1),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(Ap(DB(11),Ap(DB(15),DB(18))),DB(18)))))); (* "exactly5_6155" *)
       (Ap(DB(1),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "exactly5_32783" *)
       (Ap(DB(1),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "exactly5_34827" *)
       (Ap(DB(2),Ap(Ap(DB(11),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(DB(14),DB(20))))); (* "exactly4_rem_34827_0" *)
       (Ap(DB(2),Ap(Ap(DB(11),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(DB(14),DB(19))))); (* "exactly4_rem_34827_1" *)
       (Ap(DB(2),Ap(Ap(DB(11),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(DB(14),DB(18))))); (* "exactly4_rem_34827_3" *)
       (Ap(DB(2),Ap(Ap(DB(11),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(DB(14),DB(17))))); (* "exactly4_rem_34827_11" *)
       (Ap(DB(0),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))))); (* "exactly6_2319" *)
       (Ap(DB(0),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "exactly6_34831" *)
       (Ap(DB(0),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "exactly6_35083" *)
       (Ap(DB(3),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19)))))); (* "exactly3_262" *)
       (Ap(DB(3),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),DB(16))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))); (* "exactly3_2054" *)
       (Ap(DB(3),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))))); (* "exactly3_2306" *)
       (Ap(DB(3),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(DB(15),DB(18))),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(19)))))); (* "exactly3_32774" *)
       (Ap(DB(3),Ap(Ap(DB(11),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))); (* "exactly3_34817" *)
       (Ap(DB(3),Ap(Ap(DB(11),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))))); (* "exactly3_34818" *)
       (Ap(DB(8),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))))); (* "atleast4_i_2311_2315" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(Ap(DB(11),DB(16)),Ap(DB(14),DB(18))))); (* "Subq_m_i_2311_2315_8_2051" *)
       (Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))),Ap(DB(14),Ap(DB(14),DB(18))))))); (* "atmost4_i_2311_2315_lem1" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(17)),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(0)),Ap(DB(15),DB(21)))),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),DB(18))))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(Ap(DB(14),Ap(DB(15),Ap(DB(15),DB(19)))),DB(17))),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(15),DB(20)))))))))); (* "Subq_m_i_2311_2315_0_2306_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))),Ap(DB(14),DB(20)))),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19)))))); (* "Subq_m_i_2311_2315_0_2306" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(17)),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(0)),Ap(DB(15),DB(20)))),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),DB(18))))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),DB(18))))))))); (* "Subq_m_i_2311_2315_1_2305_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17))))); (* "Subq_m_i_2311_2315_1_2305" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(17)),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(0)),Ap(DB(15),DB(18)))),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),DB(18))))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(16),Ap(DB(15),DB(19)))))))))); (* "Subq_m_i_2311_2315_11_259_lem" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))),Ap(DB(14),DB(17)))),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(15),Ap(DB(14),DB(18)))))); (* "Subq_m_i_2311_2315_11_259" *)
       (Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(14),DB(19))))))); (* "atmost3_2306" *)
       (Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))),Ap(DB(14),DB(20)))))); (* "atmost4_i_2311_2315_lem2a" *)
       (Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))),Ap(DB(14),DB(19)))))); (* "atmost4_i_2311_2315_lem2b" *)
       (Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))),Ap(DB(14),DB(17)))))); (* "atmost4_i_2311_2315_lem2c" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),DB(17)),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),DB(18))))),Ap(Prim(3),Ap(DB(9),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(14),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(DB(15),Ap(DB(15),DB(20))))),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),DB(18))))),Ap(Ap(DB(14),Ap(DB(15),Ap(DB(15),DB(19)))),DB(17)))),Ap(DB(15),DB(0))))))))); (* "atmost4_i_2311_2315_lem2" *)
       (Ap(Prim(3),Ap(DB(7),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))))); (* "atmost4_i_2311_2315" *)
       (Ap(DB(2),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(DB(14),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),DB(17))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16))))); (* "exactly4_i_2311_2315" *)
       (Ap(DB(1),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))),Ap(DB(14),DB(20))))); (* "exactly5_rem_2319_0" *)
       (Ap(DB(1),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),DB(16)))),Ap(DB(14),DB(18))))); (* "exactly5_rem_2319_3" *)
       (Ap(DB(1),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(DB(14),DB(19))))); (* "exactly5_rem_34831_1" *)
       (Ap(DB(1),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(DB(14),DB(18))))); (* "exactly5_rem_34831_3" *)
       (Ap(DB(1),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(DB(14),DB(17))))); (* "exactly5_rem_34831_11" *)
       (Ap(DB(1),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(DB(14),DB(20))))); (* "exactly5_rem_35083_0" *)
       (Ap(DB(1),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(DB(14),DB(19))))); (* "exactly5_rem_35083_1" *)
       (Ap(DB(1),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(DB(14),DB(18))))); (* "exactly5_rem_35083_3" *)
       (Ap(DB(1),Ap(Ap(DB(11),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(DB(14),DB(17))))); (* "exactly5_rem_35083_11" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),Ap(DB(15),DB(19)))),Ap(Ap(DB(14),DB(17)),Ap(DB(15),Ap(DB(16),DB(19)))))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(20))))),Ap(Ap(DB(14),Ap(DB(15),Ap(DB(15),DB(19)))),Ap(DB(15),Ap(DB(16),DB(19)))))))))); (* "Subq_m_i_33039_35083_0_33034_lem1" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(DB(16),Ap(DB(15),DB(19)))),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(0)),Ap(DB(15),DB(21)))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(20))))),Ap(Ap(DB(14),Ap(DB(15),Ap(DB(15),DB(19)))),Ap(DB(15),Ap(DB(16),DB(19)))))))))); (* "Subq_m_i_33039_35083_0_33034_lem2" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))),Ap(DB(14),DB(20)))),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(19))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Subq_m_i_33039_35083_0_33034" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),Ap(DB(15),DB(19)))),Ap(Ap(DB(14),DB(17)),Ap(DB(15),Ap(DB(16),DB(19)))))),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(0)),Ap(DB(15),DB(20)))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(19))),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),Ap(DB(16),DB(19))))))))))); (* "Subq_m_i_33039_35083_1_33033_lem1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Subqa_33025_33033" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))),Ap(DB(14),DB(19)))),Ap(Ap(DB(13),Ap(DB(14),DB(18))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Subq_m_i_33039_35083_1_33033" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),Ap(DB(15),DB(19)))),Ap(Ap(DB(14),DB(17)),Ap(DB(15),Ap(DB(16),DB(19)))))),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(0)),Ap(DB(15),DB(19)))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),DB(20))),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),Ap(DB(16),DB(19))))))))))); (* "Subq_m_i_33039_35083_3_33027_lem1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Subqa_33025_33027" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),Ap(DB(14),DB(19))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "Subq_m_i_33039_35083_3_33027" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),Ap(DB(15),DB(19)))),Ap(Ap(DB(14),DB(17)),Ap(DB(15),Ap(DB(16),DB(19)))))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),DB(18)),Ap(DB(15),Ap(DB(16),DB(19))))))))); (* "Subq_m_i_33039_35083_8_32779_lem1" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(DB(16),Ap(DB(15),DB(19)))),Imp(Ap(Prim(3),Ap(Ap(DB(24),DB(0)),Ap(DB(15),Ap(DB(15),DB(19))))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),DB(18)),Ap(DB(15),Ap(DB(16),DB(19))))))))); (* "Subq_m_i_33039_35083_8_32779_lem2" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))),Ap(DB(14),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(15),DB(18)))))); (* "Subq_m_i_33039_35083_8_32779" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),Ap(DB(15),DB(19)))),Ap(Ap(DB(14),DB(17)),Ap(DB(15),Ap(DB(16),DB(19)))))),Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),DB(18)),Ap(DB(15),Ap(DB(15),DB(19))))))))); (* "Subq_m_i_33039_35083_15_267_lem1" *)
       (Ap(Ap(DB(22),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))),Ap(DB(14),Ap(DB(15),DB(18))))),Ap(Ap(DB(13),DB(17)),Ap(DB(14),Ap(DB(14),DB(18)))))); (* "Subq_m_i_33039_35083_15_267" *)
       (Ap(Prim(3),Ap(DB(7),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))),Ap(DB(14),DB(19)))))); (* "atmost5_i_33039_35083_lem1a" *)
       (Ap(Prim(3),Ap(DB(7),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))),Ap(DB(14),DB(18)))))); (* "atmost5_i_33039_35083_lem1b" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(15),Ap(DB(15),DB(19)))),Ap(Ap(DB(14),DB(17)),Ap(DB(15),Ap(DB(16),DB(19)))))),Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),Ap(DB(16),DB(19)))))),Ap(Ap(DB(14),Ap(DB(15),Ap(DB(15),DB(19)))),Ap(Ap(DB(14),DB(17)),Ap(DB(15),Ap(DB(16),DB(19))))))),Ap(DB(15),DB(0))))))))); (* "atmost5_i_33039_35083_lem1" *)
       (Ap(Prim(3),Ap(DB(7),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))),Ap(DB(14),DB(20)))))); (* "atmost5_i_33039_35083_lem2a" *)
       (Ap(Prim(3),Ap(DB(7),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))),Ap(DB(14),Ap(DB(14),DB(18))))))); (* "atmost5_i_33039_35083_lem2b" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),Ap(DB(16),DB(19)))))),Ap(Ap(DB(14),Ap(DB(15),Ap(DB(15),DB(19)))),Ap(Ap(DB(14),DB(17)),Ap(DB(15),Ap(DB(16),DB(19))))))),Ap(DB(15),DB(0)))))))); (* "atmost5_i_33039_35083_lem2" *)
       (Ap(Prim(3),Ap(DB(7),Ap(Ap(DB(11),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))),Ap(DB(14),Ap(DB(15),DB(18))))))); (* "atmost5_i_33039_35083_lem3" *)
       (All(Base(0),Imp(Ap(Ap(DB(24),DB(0)),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),Ap(DB(16),DB(19))))),Ap(Prim(3),Ap(DB(8),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(14),Ap(Ap(DB(12),Ap(DB(16),DB(19))),Ap(DB(16),Ap(DB(15),DB(19))))),Ap(Ap(DB(14),Ap(DB(16),Ap(DB(15),DB(19)))),Ap(DB(15),Ap(DB(16),DB(19)))))),Ap(Ap(DB(14),Ap(DB(15),Ap(DB(15),DB(19)))),Ap(Ap(DB(14),DB(17)),Ap(DB(15),Ap(DB(16),DB(19))))))),Ap(DB(15),DB(0)))))))); (* "atmost5_i_33039_35083_lem4" *)
       (Ap(Prim(3),Ap(DB(6),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18))))))))); (* "atmost5_i_33039_35083" *)
       (Ap(DB(1),Ap(Ap(DB(12),Ap(Ap(DB(13),Ap(Ap(DB(11),Ap(DB(15),DB(18))),Ap(DB(15),Ap(DB(14),DB(18))))),Ap(Ap(DB(13),Ap(DB(15),Ap(DB(14),DB(18)))),Ap(DB(14),Ap(DB(15),DB(18)))))),Ap(Ap(DB(13),Ap(DB(14),Ap(DB(14),DB(18)))),Ap(Ap(DB(13),DB(16)),Ap(DB(14),Ap(DB(15),DB(18)))))))) (* "exactly5_i_33039_35083" *)
    |]

let rec ahf_trm_str m vl =
  match m with
  | Prim(i) -> hfprimnamesa.(i)
  | DB(i) -> (try List.nth vl i with _ -> Printf.sprintf "?%d" (i - List.length vl))
  | Ap(_,_) -> Printf.sprintf "(%s)" (hf_spine_str m vl)
  | Lam(_,m1) ->
     let x = Printf.sprintf "X%d" (List.length vl) in
     Printf.sprintf "(fun %s => %s)" x (ahf_trm_str m1 (x::vl))
  | Imp(m1,m2) -> Printf.sprintf "(%s\n -> %s)" (ahf_trm_str m1 vl) (ahf_trm_str m2 vl)
  | All(a1,m1) ->
     let x =
       try
         List.nth ["ain";"asubq";"adisjoint";"a0";"a1";"a2";"a3";"a4";"apow";"asing";"aun";"aint";"asm";"aal2";"aal3";"aal4";"aal5";"aal6";"aal7";"aex2";"aex3";"aex4";"aex5";"aex6"] (List.length vl)
       with _ ->
         Printf.sprintf "X%d" (List.length vl)
     in
     if a1 = Base(0) then
       Printf.sprintf "(forall %s, %s)" x (ahf_trm_str m1 (x::vl))
     else
       Printf.sprintf "(forall %s:%s, %s)" x (hf_stp_str a1 false) (ahf_trm_str m1 (x::vl))
  | Ex(a1,m1) ->
     let x = Printf.sprintf "X%d" (List.length vl) in
     if a1 = Base(0) then
       Printf.sprintf "(exists %s, %s)" x (ahf_trm_str m1 (x::vl))
     else
       Printf.sprintf "(exists %s:%s, %s)" x (hf_stp_str a1 false) (ahf_trm_str m1 (x::vl))
  | Eq(_,m1,m2) ->
     Printf.sprintf "(%s = %s)" (ahf_trm_str m1 vl) (ahf_trm_str m2 vl)
  | _ -> "*"
    
exception HFPropFailure

let bin_hf_rep n =
  let binrep = Prim(71) in
  let pow = Prim(11) in
  let empty = Prim(9) in
  let rec bin_hf_rep_r n i =
    if n = 0L then
      empty
    else
      let n2 = Int64.shift_right_logical n 1 in
      let m = bin_hf_rep_r (Int64.of_int i) 0 in
      if Int64.logand n 1L = 1L then
        if n2 = 0L then
	  Ap(pow,m)
	else
          let w = bin_hf_rep_r n2 (i+1) in
	  Ap(Ap(binrep,w),m)
      else
        let w = bin_hf_rep_r n2 (i+1) in
	w
  in
  bin_hf_rep_r n 0

let rec un_hf_rep n =
  if n > 0 then
    Prim(9)
  else
    Ap(Prim(57),un_hf_rep (n-1))

let rec rtp a = match a with TpArr(_,a) -> rtp a | _ -> a

let bitsused c = let (_,_,_,bu,j) = c in bu * 8 + j
let bytesused c = let (_,_,_,bu,_) = c in bu

let rec sei_choosedb n i c =
  if n = 1 then
    (0,c)
  else if n = 2 then
    let (b,c) = i 1 c in
    (b,c)
  else if n = 3 then
    let (b,c) = i 1 c in
    if b = 0 then
      (0,c)
    else
      let (b,c) = i 1 c in
      (1+b,c)
  else if n = 4 then
    let (b,c) = i 2 c in
    (b,c)
  else if n > 4 then
    let (b,c) = i 1 c in
    if b = 0 then
      let (b,c) = i 2 c in
      (b,c)
    else
      let (n,c) = sei_choosedb (n-4) i c in
      (4+n,c)
  else
    raise HFPropFailure

let sei_hf_poss_neg i (p,c) =
  let (b,c) = i 1 c in
  if b = 0 then
    (Ap(Prim(3),p),c)
  else
    (p,c)    

let rec sei_hf_prop l i c uhf =
  (* let bu = bytesused c in
     Printf.printf "sei_hf_prop in %d\n" bu; *)
  let (p,c) = sei_hf_prop_0 l i c uhf in
  (* Printf.printf "sei_hf_prop out %d %d %s\n" bu (bytesused c) (hf_trm_str p []); *)
  (p,c)
and sei_hf_prop_0 l i c uhf =
  if (List.length l > 4 && uhf) then
    sei_hf_prop_2 l i c uhf
  else if List.length l < 2 then
    sei_hf_prop_1 l i c uhf
  else
    let (b,c) = i 1 c in
    if b = 0 then
      sei_hf_prop_1 l i c uhf
    else
      sei_hf_prop_2 l i c uhf
and sei_hf_prop_1 l i c uhf =
  let (b,c) = i 1 c in
  if b = 0 then
    if uhf then
      let (b,c) = i 2 c in
      if b = 0 then
        let (p,c) = sei_hf_prop (Base(0)::l) i c uhf in
        (All(Base(0),p),c)
      else if b = 1 then
        let (m,c) = sei_hf_set l i c uhf in
        let (p,c) = sei_hf_prop (Base(0)::l) i c uhf in
        (All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),uptrm m 0 1),p)),c)
      else if b = 2 then
        let (m,c) = sei_hf_set l i c uhf in
        let (p,c) = sei_hf_prop (Base(0)::l) i c uhf in
        (All(Base(0),Imp(Ap(Ap(Prim(8),DB(0)),uptrm m 0 1),p)),c)
      else
        let (q,c) = sei_hf_prop (Base(0)::l) i c uhf in
        let (p,c) = sei_hf_prop (Base(0)::l) i c uhf in
        (All(Base(0),Imp(q,p)),c)
    else
      let (b,c) = i 1 c in
      if b = 0 then
        let (p,c) = sei_hf_prop (Base(0)::l) i c uhf in
        (All(Base(0),p),c)
      else
        let (q,c) = sei_hf_prop (Base(0)::l) i c uhf in
        let (p,c) = sei_hf_prop (Base(0)::l) i c uhf in
        (All(Base(0),Imp(q,p)),c)
  else
    if uhf then
      let (b,c) = i 2 c in
      if b = 0 then
        let (p,c) = sei_hf_prop (Base(0)::l) i c uhf in
        (Ex(Base(0),p),c)
      else if b = 1 then
        let (m,c) = sei_hf_set l i c uhf in
        let (p,c) = sei_hf_prop (Base(0)::l) i c uhf in
        (Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(7),DB(0)),uptrm m 0 1)),p)),c)
      else if b = 2 then
        let (m,c) = sei_hf_set l i c uhf in
        let (p,c) = sei_hf_prop (Base(0)::l) i c uhf in
        (Ex(Base(0),Ap(Ap(Prim(4),Ap(Ap(Prim(8),DB(0)),uptrm m 0 1)),p)),c)
      else
        let (q,c) = sei_hf_prop (Base(0)::l) i c uhf in
        let (p,c) = sei_hf_prop (Base(0)::l) i c uhf in
        (Ex(Base(0),Ap(Ap(Prim(4),q),p)),c)
    else
      let (b,c) = i 1 c in
      if b = 0 then
        let (p,c) = sei_hf_prop (Base(0)::l) i c uhf in
        (Ex(Base(0),p),c)
      else
        let (q,c) = sei_hf_prop (Base(0)::l) i c uhf in
        let (p,c) = sei_hf_prop (Base(0)::l) i c uhf in
        (Ex(Base(0),Ap(Ap(Prim(4),q),p)),c)
and sei_hf_prop_2 l i c uhf =
  let (b,c) = i 1 c in
  if b = 0 then
    sei_hf_prop_3 l i c uhf
  else
    let (b,c) = i 1 c in
    if b = 0 then
      let (q,c) = sei_hf_prop l i c uhf in
      let (p,c) = sei_hf_prop l i c uhf in
      (Imp(q,p),c)
    else
      let (q,c) = sei_hf_prop l i c uhf in
      let (p,c) = sei_hf_prop l i c uhf in
      (Ap(Ap(Prim(4),q),p),c) (** conjunction **)
and sei_hf_prop_3 l i c uhf =
  let dbj = ref [] in
  let j = ref 0 in
  List.iter
    (fun a -> (if rtp a = Prop then dbj := (!j,a)::!dbj); incr j)
    l;
  if uhf then
    if !dbj = [] then
      sei_hf_poss_neg i (sei_hf_prop_4 l i c uhf)
    else
      let (b,c) = i 1 c in
      if b = 0 then
        let (n,c) = sei_choosedb (List.length !dbj) i c in
        let (j,a) = List.nth (List.rev !dbj) n in
        sei_hf_poss_neg i (sei_hf_spine l i c (DB(j)) a uhf)
      else
        sei_hf_poss_neg i (sei_hf_prop_4 l i c uhf)
  else
    if !dbj = [] then
      let (m1,c) = sei_hf_set l i c uhf in
      let (m2,c) = sei_hf_set l i c uhf in
      (Eq(Base(0),m1,m2),c)
    else
      let (b,c) = i 1 c in
      if b = 0 then
        let (n,c) = sei_choosedb (List.length !dbj) i c in
        let (j,a) = List.nth (List.rev !dbj) n in
        sei_hf_poss_neg i (sei_hf_spine l i c (DB(j)) a uhf)
      else
        let (m1,c) = sei_hf_set l i c uhf in
        let (m2,c) = sei_hf_set l i c uhf in
        (Eq(Base(0),m1,m2),c)
and sei_hf_prop_4 l i c uhf =
  let (b,c) = i 1 c in
  if b = 0 then (** half the time, choose one of the first 8 unary predicates **)
    let (b,c) = i 3 c in
    let (j,a) = propformersa.(b) in
    sei_hf_spine l i c (Prim(j)) a uhf
  else
    let (b,c) = i 1 c in (** quarter of the time, choose one of the next 4 unary predicates **)
    if b = 0 then (** quarter of the time, choose one of the next 4 unary predicates **)
      let (b,c) = i 2 c in
      let (j,a) = propformersa.(8 + b) in
      sei_hf_spine l i c (Prim(j)) a uhf
    else
      let (b,c) = i 1 c in (** eighth of the time, choose In or one of 3 unary's setsum_p, set_of_pairs or SNo **)
      if b = 0 then
        let (b,c) = i 2 c in
        let (j,a) = propformersa.(12 + b) in
        sei_hf_spine l i c (Prim(j)) a uhf
      else
        let (b,c) = i 1 c in (** sixteenth of the time, choose one of 7 binary predicates or equality **)
        if b = 0 then
          let (b,c) = i 3 c in
          if b = 7 then
            let (m1,c) = sei_hf_set l i c uhf in
            let (m2,c) = sei_hf_set l i c uhf in
            (Eq(Base(0),m1,m2),c)
          else
            let (j,a) = propformersa.(16 + b) in
            sei_hf_spine l i c (Prim(j)) a uhf
        else (** rest of the time choose one of the others **)
          let (b,c) = i 1 c in
          if b = 0 then
            let (b,c) = i 1 c in
            if b = 0 then
              let (j,a) = propformersa.(23) in
              sei_hf_spine l i c (Prim(j)) a uhf
            else
              let (b,c) = i 3 c in
              let (j,a) = propformersa.(24 + b) in
              sei_hf_spine l i c (Prim(j)) a uhf
          else
            let (b,c) = i 4 c in
            let (j,a) = propformersa.(32 + b) in
            sei_hf_spine l i c (Prim(j)) a uhf
and sei_hf_set l i c uhf =
  (* let bu = bytesused c in
  Printf.printf "sei_hf_set in %d\n" bu; *)
  let (m,c) = sei_hf_set_0 l i c uhf in
  (* Printf.printf "sei_hf_set out %d %d %s\n" bu (bytesused c) (hf_trm_str m []); *)
  (m,c)
and sei_hf_set_0 l i c uhf =
  let dbj = ref [] in
  let j = ref 0 in
  List.iter
    (fun a -> (if rtp a = Base(0) then dbj := (!j,a)::!dbj); incr j)
    l;
  let dbjl = List.length !dbj in
  if uhf then
    if dbjl = 0 then
      sei_hf_set_2 l i c uhf
    else if dbjl = 1 then
      let (b,c) = i 1 c in
      if b = 0 then
        let (j,a) = List.nth (List.rev !dbj) 0 in
        sei_hf_spine l i c (DB(j)) a uhf
      else
        sei_hf_set_2 l i c uhf
    else if dbjl = 2 then
      let (b,c) = i 1 c in
      if b = 0 then
        let (j,a) = List.nth (List.rev !dbj) 0 in
        sei_hf_spine l i c (DB(j)) a uhf
      else
        let (b,c) = i 1 c in
        if b = 0 then
          let (j,a) = List.nth (List.rev !dbj) 1 in
          sei_hf_spine l i c (DB(j)) a uhf
        else
          sei_hf_set_2 l i c uhf
    else if dbjl = 3 then
      let (b,c) = i 2 c in
      if b < 3 then
        let (j,a) = List.nth (List.rev !dbj) b in
        sei_hf_spine l i c (DB(j)) a uhf
      else
        sei_hf_set_2 l i c uhf
    else
      let (b,c) = i 2 c in
      if b < 2 then
        let (j,a) = List.nth (List.rev !dbj) b in
        sei_hf_spine l i c (DB(j)) a uhf
      else if b = 2 then
        let (n,c) = sei_choosedb (dbjl-2) i c in
        let (j,a) = List.nth (List.rev !dbj) (n+2) in
        sei_hf_spine l i c (DB(j)) a uhf
      else
        sei_hf_set_2 l i c uhf
  else
    if dbjl = 0 then
      (Prim(9),c) (** just use empty **)
    else
      let (n,c) = sei_choosedb dbjl i c in
      let (j,a) = List.nth (List.rev !dbj) n in
      sei_hf_spine l i c (DB(j)) a uhf
and sei_hf_set_2 l i c uhf =
  let (b,c) = i 1 c in (** 50% of the time use a term giving a binary representation of a number between 5 and 20 **)
  if b = 0 then
    let (b,c) = i 4 c in
    (bin_hf_rep (Int64.of_int (b + 5)),c)
  else
    let (b,c) = i 1 c in (** 25% of the time use a term giving a unary representation of a number between 2 and 5 **)
    if b = 0 then
      let (b,c) = i 2 c in
      (un_hf_rep (b + 2),c)
    else (** remainder of the time prefer unary functions, then binary, then other **)
      let (b,c) = i 1 c in
      if b = 0 then
        let (b,c) = i 3 c in
        let (j,a) = setformersa.(b) in
        sei_hf_spine l i c (Prim(j)) a uhf
      else
        let (b,c) = i 1 c in
        if b = 0 then
          let (b,c) = i 3 c in
          let (j,a) = setformersa.(b + 8) in
          sei_hf_spine l i c (Prim(j)) a uhf
        else
          let (b,c) = i 1 c in
          if b = 0 then
            let (b,c) = i 3 c in
            let (j,a) = setformersa.(b + 16) in
            sei_hf_spine l i c (Prim(j)) a uhf
          else
            let (b,c) = i 1 c in
            if b = 0 then
              let (b,c) = i 3 c in
              let (j,a) = setformersa.(b + 24) in
              sei_hf_spine l i c (Prim(j)) a uhf
            else
              let (b,c) = i 1 c in
              if b = 0 then
                let (b,c) = i 2 c in
                let (j,a) = setformersa.(b + 32) in
                sei_hf_spine l i c (Prim(j)) a uhf
              else
                let (b,c) = i 1 c in
                let (j,a) = setformersa.(b + 36) in
                sei_hf_spine l i c (Prim(j)) a uhf
and sei_hf_spine l i c m a uhf =
  (* let bu = bytesused c in
     Printf.printf "sei_hf_spine in %d %s\n" bu (hf_trm_str m []); *)
  let (m,c) = sei_hf_spine_0 l i c m a uhf in
  (* Printf.printf "sei_hf_spine out %d %d %s\n" bu (bytesused c) (hf_trm_str m []); *)
  (m,c)
and sei_hf_spine_0 l i c m a uhf =
  match a with
  | TpArr(a1,a2) ->
     let (n,c) = sei_hf_trm l i c a1 uhf in
     sei_hf_spine l i c (Ap(m,n)) a2 uhf
  | _ -> (m,c)
and sei_hf_trm l i c a uhf =
  match a with
  | TpArr(a1,a2) ->
     let (m,c) = sei_hf_trm (a1::l) i c a2 uhf in
     (Lam(a1,m),c)
  | Prop -> sei_hf_prop l i c uhf
  | Base(0) -> sei_hf_set l i c uhf
  | _ -> raise HFPropFailure

let sei_poly3 i c =
  let setsum = Prim(68) in
  let setprod = Prim(73) in
  let setexp = Prim(78) in
  let p = ref None in
  let expon v n =
    if n = 1 then
      v
    else
      Ap(Ap(setexp,v),bin_hf_rep (Int64.of_int n))
  in
  let mon b x y z =
    let coeff = bin_hf_rep (Int64.of_int b) in
    let coeff_fun xyz =
      if b > 1 then
        Ap(Ap(setprod,coeff),xyz)
      else
        xyz
    in
    if x = 0 then
      if y = 0 then
        if z = 0 then
          coeff (** just constant **)
        else
          coeff_fun (expon (DB(0)) z)
      else if z = 0 then
        coeff_fun (expon (DB(1)) y)
      else
        coeff_fun (Ap(Ap(setprod,expon (DB(1)) y),expon (DB(0)) z))
    else
      if y = 0 then
        if z = 0 then
          coeff_fun (expon (DB(2)) x)
        else
          coeff_fun (Ap(Ap(setprod,expon (DB(2)) x),expon (DB(0)) z))
      else if z = 0 then
        coeff_fun (Ap(Ap(setprod,expon (DB(2)) x),expon (DB(1)) y))
      else
        coeff_fun (Ap(Ap(setprod,expon (DB(2)) x),Ap(Ap(setprod,expon (DB(1)) y),expon (DB(0)) z)))
  in
  let update_p b x y z =
    if b > 0 then (** no change if monomial has 0 coefficient **)
      match !p with
      | None -> p := Some(mon b x y z)
      | Some(q) -> p := Some(Ap(Ap(setsum,mon b x y z),q))
  in (** 128 bits used below **)
  for x = 0 to 3 do
    for y = 0 to 3 do
      for z = 0 to 3 do
        let (b,c) = i 4 c in
        update_p b x y z
      done
    done
  done;
  match !p with
  | None -> (Prim(9),c) (** empty set, zero polynomial **)
  | Some(q) -> (q,c)

let sei_abstr_hf_prop i c =
  let (b,c) = i 11 c in
  let concl = b mod 1229 in
  let p = ref (ahfprops.(concl)) in
  let cr = ref c in
  for j = 1228 downto 0 do
    if not (j = concl) then
      let (b,c) = i 4 (!cr) in
      cr := c;
      if b = 0 then p := Imp(ahfprops.(j),!p)
  done;
  List.iter
    (fun a -> p := All(a,!p))
    ahfctx;
  !p
    
let sei_diophantine i c =
  let (p1,c) = sei_poly3 i c in
  let (p2,c) = sei_poly3 i c in
  let (b,c) = i 1 c in
  let setsum = Prim(68) in
  let r = Prim(53 + b) in (** atleastp if b = 0, equip if b = 1 **)
  let i = Base(0) in
  let p = All(i,All(i,All(i,Imp(Ap(Ap(r,Ap(Ap(setsum,p1),bin_hf_rep 16L)),p2),Prim(1))))) in (** in the form that says there is no soln **)
  p

let sei_diophantine_mod i c =
  let (p1,c) = sei_poly3 i c in
  let (p2,c) = sei_poly3 i c in
  let (b,c) = i 30 c in
  let (big,c) = i 1 c in
  let m =
    if big = 0 then
      Int64.add 2L (Int64.of_int b)
    else
      let (b2,c) = i 30 c in
      if b = 0 && b2 < 2 then
        2L
      else
        Int64.logor (Int64.shift_left (Int64.of_int b) 30) (Int64.of_int b)
  in
  let setsum = Prim(68) in
  let equipmod = Prim(103) in
  let i = Base(0) in
  let p = All(i,All(i,All(i,Imp(Ap(Ap(Ap(equipmod,Ap(Ap(setsum,p1),bin_hf_rep 16L)),p2),bin_hf_rep m),Prim(1))))) in (** in the form that says there is no soln **)
  p

let sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c n =
  let (b,c) = i 2 c in
  if b = 0 then
    (DB(t+n),c)
  else if b = 1 then
    (DB(i1+n),c)
  else if b = 2 then
    (DB(i2+n),c)
  else
    let (b,c) = i 1 c in
    if b = 0 then
      (DB(i2+n),c)
    else
      (DB(j2+n),c)

let sei_aim_innmap2 (j2,i2,j1,i1,r,l,t) i c n =
  let (b,c) = i 1 c in
  if b = 0 then
    (DB(r+n),c)
  else
    (DB(l+n),c)

let dup_some l i c =
  let dupd = ref [0] in
  let rec ins x l c =
    match l with
    | y::z::r ->
       let (b,c) = i 2 c in
       if b = 0 then
         (x::l,c)
       else if b = 1 then
         (y::x::z::r,c)
       else if b = 2 then
         (y::z::x::r,c)
       else
         let (l,c) = ins x r c in
         (y::z::l,c)
    | [y] ->
       let (b,c) = i 1 c in
       if b = 0 then
         ([x;y],c)
       else
         ([y;x],c)
    | [] -> ([x],c)
  in
  let rec dup_some_r l c =
    match l with
    | [] -> ([],c)
    | x::r ->
       if List.mem x !dupd then
         let (r,c) = dup_some_r r c in
         (x::r,c)
       else
         let (b,c) = i 1 c in
         if b = 0 then
           let (r,c) = dup_some_r r c in
           (x::r,c)
         else
           let (r,c) = ins x r c in
           dupd := x::!dupd;
           let (r,c) = dup_some_r r c in
           (x::r,c)
  in
  dup_some_r l c

let rec randperm l l2 i c =
  match l with
  | x::r ->
     let (b,c) = i 1 c in
     if b = 0 then
       randperm r (x::l2) i c
     else
       randperm r (l2 @ [x]) i c
  | [] -> (l2,c)
  
let sei_aim_trm m m1 l i c n =
  let rec sei_aim_trm_r m m1 l c n =
    match l with
    | [] -> (m1,c)
    | x::r ->
       match m1 with
       | Ap(Ap(_,m1a),m1b) ->
          let (b,c) = i 1 c in
          let m1 =
            if b = 0 then
              Ap(Ap(DB(m+n),m1),DB(x))
            else
              Ap(Ap(DB(m+n),m1a),Ap(Ap(DB(m+n),m1b),DB(x)))
          in
          sei_aim_trm_r m m1 r c n
       | _ ->
          let m1 = Ap(Ap(DB(m+n),m1),DB(x)) in
          sei_aim_trm_r m m1 r c n
  in
  let (l,c) = randperm l [] i c in
  sei_aim_trm_r m m1 l c n

(* Use c to pseudorandomly choose inner mappings for assumed eqns that some inner mappings commute. *)
let sei_aim loopwithdefscex i c =
  let (j2,i2,j1,i1,r,l,t,a,k,e,s,b,m,x) = (0,1,2,3,4,5,6,7,8,9,10,11,12,13) in
  let relall n p = All(Base(0),Imp(Ap(Ap(Prim(7),DB(0)),DB(x+n)),p)) in
  let rec relallrec n p =
    if n > 0 then
      relallrec (n-1) (relall n p)
    else
      p
  in
  let q = ref (Prim(1)) in (* False *)
  let addid n lhs rhs =
    if not (lhs = rhs) then (** don't add if syntactically equal **)
      q := Imp(relallrec n (Eq(Base(0),lhs,rhs)),!q)
  in
  let cr = ref c in
  for j = 1 to (if !Config.testnet then 1 else 9) do
    let c = !cr in
    let (m1a,c) = sei_aim_innmap2 (j2,i2,j1,i1,r,l,t) i c 8 in
    let (m1b,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 8 in
    let (m1c,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 8 in
    let (m2a,c) = sei_aim_innmap2 (j2,i2,j1,i1,r,l,t) i c 8 in
    let (m2b,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 8 in
    cr := c;
    let m1f y z w v u = Ap(Ap(Ap(m1a,y),z),Ap(Ap(m1b,w),Ap(Ap(m1c,v),u))) in
    let m2f y z w u = Ap(Ap(Ap(m2a,y),z),Ap(Ap(m2b,w),u)) in
    addid 8
      (m1f (DB(7)) (DB(6)) (DB(5)) (DB(4)) (m2f (DB(3)) (DB(2)) (DB(1)) (DB(0))))
      (m2f (DB(3)) (DB(2)) (DB(1)) (m1f (DB(7)) (DB(6)) (DB(5)) (DB(4)) (DB(0))))
  done;
  for j = 1 to (if !Config.testnet then 1 else 2) do
    let c = !cr in
    let (m1a,c) = sei_aim_innmap2 (j2,i2,j1,i1,r,l,t) i c 7 in
    let (m1b,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 7 in
    let (m2a,c) = sei_aim_innmap2 (j2,i2,j1,i1,r,l,t) i c 7 in
    let (m2b,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 7 in
    cr := c;
    let m1f y z w u = Ap(Ap(Ap(m1a,y),z),Ap(Ap(m1b,w),u)) in
    let m2f y z w u = Ap(Ap(Ap(m2a,y),z),Ap(Ap(m2b,w),u)) in
    addid 7
      (m1f (DB(6)) (DB(5)) (DB(4)) (m2f (DB(3)) (DB(2)) (DB(1)) (DB(0))))
      (m2f (DB(3)) (DB(2)) (DB(1)) (m1f (DB(6)) (DB(5)) (DB(4)) (DB(0))))
  done;
  for j = 1 to (if !Config.testnet then 1 else 2) do
    let c = !cr in
    let (m1a,c) = sei_aim_innmap2 (j2,i2,j1,i1,r,l,t) i c 6 in
    let (m1b,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 6 in
    let (m2a,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 6 in
    let (m2b,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 6 in
    cr := c;
    let m1f y z w u = Ap(Ap(Ap(m1a,y),z),Ap(Ap(m1b,w),u)) in
    let m2f y z u = Ap(Ap(m2a,y),Ap(Ap(m2b,z),u)) in
    addid 6
      (m1f (DB(5)) (DB(4)) (DB(3)) (m2f (DB(2)) (DB(1)) (DB(0))))
      (m2f (DB(2)) (DB(1)) (m1f (DB(5)) (DB(4)) (DB(3)) (DB(0))))
  done;
  for j = 1 to (if !Config.testnet then 1 else 5) do
    let c = !cr in
    let (m1a,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 5 in
    let (m1b,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 5 in
    let (m2a,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 5 in
    let (m2b,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 5 in
    cr := c;
    let m1f y z u = Ap(Ap(m1a,y),Ap(Ap(m1b,z),u)) in
    let m2f y z u = Ap(Ap(m2a,y),Ap(Ap(m2b,z),u)) in
    addid 5
      (m1f (DB(4)) (DB(3)) (m2f (DB(2)) (DB(1)) (DB(0))))
      (m2f (DB(2)) (DB(1)) (m1f (DB(4)) (DB(3)) (DB(0))))
  done;
  let c = !cr in
  let (m1,c) = sei_aim_innmap2 (j2,i2,j1,i1,r,l,t) i c 5 in
  let (m2a,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 5 in
  let (m2b,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 5 in
  let m1f y z u = Ap(Ap(Ap(m1,y),z),u) in
  let m2f y z u = Ap(Ap(m2a,y),Ap(Ap(m2b,z),u)) in
  addid 5
    (m1f (DB(4)) (DB(3)) (m2f (DB(2)) (DB(1)) (DB(0))))
    (m2f (DB(2)) (DB(1)) (m1f (DB(4)) (DB(3)) (DB(0))));
  let (m1,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 4 in
  let (m2a,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 4 in
  let (m2b,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 4 in
  let m1f y u = Ap(Ap(m1,y),u) in
  let m2f y z u = Ap(Ap(m2a,y),Ap(Ap(m2b,z),u)) in
  addid 4
    (m1f (DB(3)) (m2f (DB(2)) (DB(1)) (DB(0))))
    (m2f (DB(2)) (DB(1)) (m1f (DB(3)) (DB(0))));
  cr := c;
  for j = 1 to (if !Config.testnet then 1 else 2) do (** these are identities that may not hold in all AIM loops; each says the composition of some inner mappings gives identity inner mapping **)
    let c = !cr in
    let (b,c) = i 2 c in
    if b = 0 then
      let (m1,c) = sei_aim_innmap2 (j2,i2,j1,i1,r,l,t) i c 3 in
      let (m2a,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 3 in
      let (m2b,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 3 in
      let m1f y z u = Ap(Ap(Ap(m1,y),z),u) in
      let m2f y z u = Ap(Ap(m2a,y),Ap(Ap(m2b,z),u)) in
      let m12f y z u = m1f y z (m2f y z u) in
      let rec m12fpow n y z u =
        if n > 0 then
          m12f y z (m12fpow (n-1) y z u)
        else
          u
      in
      let (b,c) = i 2 c in (** m12f has order b+2 [between 2 and 5] **)
      cr := c;
      addid 3 (m12fpow (b+2) (DB(2)) (DB(1)) (DB(0))) (DB(0))
    else if b = 1 then
      let (m1a,c) = sei_aim_innmap2 (j2,i2,j1,i1,r,l,t) i c 4 in
      let (m1b,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 4 in
      let (m2a,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 4 in
      let (m2b,c) = sei_aim_innmap2 (j2,i2,j1,i1,r,l,t) i c 4 in
      let m1f y z w u = Ap(Ap(Ap(m1a,y),z),Ap(Ap(m1b,w),u)) in
      let m2f y z w u = Ap(Ap(m2a,y),Ap(Ap(Ap(m2b,z),w),u)) in
      let m12f y z w u = m1f y z w (m2f y z w u) in
      let rec m12fpow n y z w u =
        if n > 0 then
          m12f y z w (m12fpow (n-1) y z w u)
        else
          u
      in
      let (b,c) = i 2 c in (** m12f has order b+2 [between 2 and 5] **)
      cr := c;
      addid 4 (m12fpow (b+2) (DB(3)) (DB(2)) (DB(1)) (DB(0))) (DB(0))
    else if b = 2 then
      let (m1a,c) = sei_aim_innmap2 (j2,i2,j1,i1,r,l,t) i c 4 in
      let (m1b,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 4 in
      let (m2a,c) = sei_aim_innmap2 (j2,i2,j1,i1,r,l,t) i c 4 in
      let (m2b,c) = sei_aim_innmap1 (j2,i2,j1,i1,r,l,t) i c 4 in
      let m1f y z w u = Ap(Ap(Ap(m1a,y),z),Ap(Ap(m1b,w),u)) in
      let m2f y z w u = Ap(Ap(Ap(m2a,y),z),Ap(Ap(m2b,w),u)) in
      let m12f y z w u = m1f y z w (m2f y z w u) in
      let rec m12fpow n y z w u =
        if n > 0 then
          m12f y z w (m12fpow (n-1) y z w u)
        else
          u
      in
      let (b,c) = i 2 c in (** m12f has order b+2 [between 2 and 5] **)
      cr := c;
      addid 4 (m12fpow (b+2) (DB(3)) (DB(2)) (DB(1)) (DB(0))) (DB(0))
    else (** no extra eqn **)
      ()
  done;
  let i = Base(0) in
  let ii = TpArr(i,i) in
  let iii = TpArr(i,ii) in
  let iiii = TpArr(i,iii) in
  let loophyp = Ap(Ap(Ap(Ap(Ap(Ap(Ap(Ap(Ap(Ap(Ap(Ap(Ap(Ap(loopwithdefscex,DB(x)),DB(m)),DB(b)),DB(s)),DB(e)),DB(k)),DB(a)),DB(t)),DB(l)),DB(r)),DB(i1)),DB(j1)),DB(i2)),DB(j2)) in
  All(i,All(iii,All(iii,All(iii,All(i,All(iii,All(iiii,All(iii,All(iiii,All(iiii,All(iii,All(iii,All(iii,All(iii,Imp(loophyp,!q)))))))))))))))

let sei_aim1 i c =
  let loopwithdefscex1 = Prim(99) in
  sei_aim loopwithdefscex1 i c

let sei_aim2 i c =
  let loopwithdefscex2 = Prim(100) in
  sei_aim loopwithdefscex2 i c

let sei_qbf_prop i c =
  let relset rel n =
    let rell = ref [] in
    for j = n-1 downto 0 do
      if Int64.logand (Int64.shift_right_logical rel j) 1L = 1L then
        rell := j::!rell
    done;
    !rell
  in
  let sei_qbf_split_rel rel n i c =
    let rel1 = ref 0L in
    let rel2 = ref 0L in
    let cr = ref c in
    let dcnt = ref 0 in
    for j = 0 to n-1 do
      if Int64.logand (Int64.shift_right_logical rel j) 1L = 1L then
        if !dcnt < 3 then
          let (b,c) = i 1 !cr in
          if b = 0 then
            let (b,c) = i 1 c in
            cr := c;
            if b = 0 then
              rel1 := Int64.logor (Int64.shift_left 1L j) !rel1
            else
              rel2 := Int64.logor (Int64.shift_left 1L j) !rel2
          else
            begin
              cr := c;
              incr dcnt;
              rel1 := Int64.logor (Int64.shift_left 1L j) !rel1;
              rel2 := Int64.logor (Int64.shift_left 1L j) !rel2
            end
        else
          let (b,c) = i 1 !cr in
          cr := c;
          if b = 0 then
            rel1 := Int64.logor (Int64.shift_left 1L j) !rel1
          else
            rel2 := Int64.logor (Int64.shift_left 1L j) !rel2
    done;
    (!rel1,!rel2,!cr)
  in
  let rec sei_qbf_imp ql p i c =
    match ql with
    | [] -> (DB(p),c)
    | q::qr ->
       let (pr,c) = sei_qbf_imp qr p i c in
       let (b,c) = i 2 c in
       if b = 0 then
         (Imp(DB(q),pr),c)
       else if b = 1 then
         (Imp(Ap(Prim(3),DB(q)),pr),c)
       else if b = 2 then
         (Ap(Ap(Prim(6),DB(q)),pr),c)
       else
         (Ap(Ap(Prim(6),Ap(Prim(3),DB(q))),pr),c)
  in
  let rec sei_qbf_imp_fal ql i c =
    match ql with
    | [] -> (Prim(1),c)
    | q::qr ->
       let (b,c) = i 1 c in
       if b = 0 then
         sei_qbf_imp qr q i c
       else
         let (pr,c) = sei_qbf_imp_fal qr i c in
         let (b,c) = i 1 c in
         if b = 0 then
           (Imp(DB(q),pr),c)
         else
           (Ap(Ap(Prim(6),DB(q)),pr),c)
  in
  let rec sei_qbf_prop_2 rel n i c =
    let rell = relset rel n in
    if List.length rell > 4 then
      let (rel1,rel2,c) = sei_qbf_split_rel rel n i c in
      let (q1,c) = sei_qbf_prop_2 rel1 n i c in
      let (q2,c) = sei_qbf_prop_2 rel2 n i c in
      let (b,c) = i 1 c in
      if b = 0 then
        let (b,c) = i 1 c in
        if b = 0 then
          (Imp(q1,q2),c)
        else
          (Ap(Prim(3),Imp(q2,q1)),c)
      else
        (Ap(Ap(Prim(6),q1),q2),c) (** iff **)
    else
      let (q,c) = sei_qbf_imp_fal rell i c in
      let (b,c) = i 1 c in
      if b = 0 then
        (q,c)
      else
        (Ap(Prim(3),q),c)
  in
  let rec sei_qbf_prop_1 n i c =
(**    let bu = bytesused c in
    Printf.printf "sei_qbf_prop_1 in %d %d\n" bu n; flush stdout; **)
    if n >= 55 then
      let rel = Int64.sub (Int64.shift_left 1L n) 1L in
      let (q1,c) = sei_qbf_prop_2 rel n i c in
      let (q2,c) = sei_qbf_prop_2 rel n i c in
      (Ap(Ap(Prim(6),q1),q2),c)
    else if n < 50 then (** must involve more quantifiers **)
      let (b,c) = i 1 c in
      if b = 0 then
        let (q,c) = sei_qbf_prop_1 (n+1) i c in
        (All(Prop,q),c)
      else
        let (q,c) = sei_qbf_prop_1 (n+1) i c in
        (Ap(Prim(3),All(Prop,q)),c)
    else
      let (b,c) = i 1 c in
      if b = 0 then
        let (b,c) = i 1 c in
        if b = 0 then
          let (q,c) = sei_qbf_prop_1 (n+1) i c in
          (All(Prop,q),c)
        else
          let (q,c) = sei_qbf_prop_1 (n+1) i c in
          (Ap(Prim(3),All(Prop,q)),c)
      else
        let rel = Int64.sub (Int64.shift_left 1L n) 1L in
        let (q1,c) = sei_qbf_prop_2 rel n i c in
        let (q2,c) = sei_qbf_prop_2 rel n i c in
        (Ap(Ap(Prim(6),q1),q2),c)
  in
  sei_qbf_prop_1 0 i c

let rec sei_choose_num n i c =
  if n > 0 then
    if n = 1 then
      (0,c)
    else if n = 2 then
      i 1 c
    else if n = 3 then
      let (b,c) = i 1 c in
      if b = 0 then
        (2,c)
      else
        i 1 c
    else if n = 4 then
      i 2 c
    else
      let (b,c) = i 1 c in
      if b = 0 then
        let (k,c) = sei_choose_num (n-4) i c in
        (k+4,c)
      else
        i 2 c
  else
    raise HFPropFailure

let sei_ho_unif_flex_rigid x tl i c =
  let ii = TpArr(Base(0),Base(0)) in
  let iii = TpArr(Base(0),ii) in
  let rec rand_ho_ind tl i c lev =
    let (h,a,c) =
      if lev < 2 then
        let (b,c) = i 2 c in
        if b = 0 then
          let (b,c) = i 1 c in
          if b = 0 then
            (Prim(64),ii,c)
          else
            (Prim(65),ii,c)
        else if b = 1 then
          let (b,c) = i 1 c in
          (Prim(68),iii,c)
        else if b = 2 then
          let (b,c) = i 2 c in
          (DB(b),List.nth tl b,c)
        else
          let (b,c) = i 2 c in
          let j = (List.length tl) - (b+1) in
          (DB(j),List.nth tl j,c)
      else if lev > 8 then
        (Prim(9),Base(0),c)
      else
        let (b,c) = i 2 c in
        if b = 0 then
          (Prim(9),Base(0),c)
        else if b = 1 then
          let (b,c) = i 1 c in
          if b = 0 then
            let (b,c) = i 1 c in
            if b = 0 then
              (Prim(64),ii,c)
            else
              (Prim(65),ii,c)
          else
            (Prim(68),iii,c)
        else if b = 2 then
          let (b,c) = i 2 c in
          (DB(b),List.nth tl b,c)
        else
          let (b,c) = i 2 c in
          let j = (List.length tl) - (b+1) in
          (DB(j),List.nth tl j,c)
    in
    rand_ho_spine h a tl i c (lev+1)
  and rand_ho_trm a tl i c lev =
    match a with
    | TpArr(a1,a2) ->
       let (m,c) = rand_ho_trm a2 (a1::tl) i c lev in
       (Lam(a1,m),c)
    | Base(j) when j = 0 -> rand_ho_ind tl i c lev
    | _ -> raise HFPropFailure (* should never happen *)
  and rand_ho_spine m a tl i c lev =
    match a with
    | TpArr(a1,a2) ->
       let (ma,c) = rand_ho_trm a1 tl i c (lev+1) in
       rand_ho_spine (Ap(m,ma)) a2 tl i c lev
    | _ -> (m,c)
  in
  let rand_rigid_ho_imit tl i c lev =
    let (b,c) = i 1 c in
    if b = 0 then
      let (b,c) = i 1 c in
      if b = 0 then
        rand_ho_spine (Prim(64)) ii tl i c (lev+1) (** Inj1 **)
      else
        rand_ho_spine (Prim(65)) ii tl i c (lev+1) (** Inj0 **)
    else
      rand_ho_spine (Prim(68)) iii tl i c (lev+1) (** setsum **)
  in
  let rand_rigid_ho tl i c lev =
    let (b,c) = i 1 c in
    if b = 0 then
      rand_rigid_ho_imit tl i c lev
    else
      let (b,c) = i 2 c in
      rand_ho_spine (DB(b)) (List.nth tl b) tl i c (lev+1)
  in
  let rec db_head m =
    match m with
    | Lam(_,m1) ->
       let i = db_head m1 in
       if i > 0 then (i-1) else raise Not_found
    | Ap(m1,_) -> db_head m1
    | DB(i) -> i
    | _ -> raise Not_found
  in
  let rec db_args m =
    match m with
    | Ap(m1,m2) ->
       begin
         let il = db_args m1 in
         try
           let i = db_head m2 in
           if List.mem i il then
             il
           else
             (i::il)
         with Not_found -> il
       end
    | _ -> []
  in
  let (m1,c) = rand_ho_spine (DB(x)) (List.nth tl x) tl i c (if !Config.testnet then 3 else 1) in
  let allowedprojs = db_args m1 in
  let (m2,c) =
    if allowedprojs = [] then
      rand_rigid_ho tl i c (if !Config.testnet then 2 else 0)
    else
      let (b,c) = i 1 c in
      if b = 0 then
        let (k,c) = sei_choose_num (List.length allowedprojs) i c in
        let j = List.nth allowedprojs k in
        rand_ho_spine (DB(j)) (List.nth tl j) tl i c (if !Config.testnet then 2 else 0)
      else
        rand_rigid_ho_imit tl i c (if !Config.testnet then 2 else 0)
  in
  (Eq(Base(0),m1,m2),c)

let rec sei_stp rtp i c lev =
  if lev < 2 then
    let (a1,c) = sei_stp (Base(0)) i c (lev+1) in
    let (a2,c) = sei_stp rtp i c (lev+1) in
    (TpArr(a1,a2),c)
  else
    if lev > 5 then
      (rtp,c)
    else
      let (b,c) = i 1 c in
      if b = 0 then
        (rtp,c)
      else
        let (a1,c) = sei_stp (Base(0)) i c (lev+1) in
        let (a2,c) = sei_stp rtp i c (lev+1) in
        (TpArr(a1,a2),c)

let rec sei_stpl rtp i c lev n =
  if n > 0 then
    let (a,c) = sei_stp rtp i c lev in
    let (l,c) = sei_stpl rtp i c lev (n-1) in
    (a::l,c)
  else
    ([],c)

let sei_ho_unif_prop i c =
  let rec allho tl p =
    match tl with
    | [] -> p
    | a::tr -> allho tr (All(a,p))
  in
  let (tl1,c) = sei_stpl (Base(0)) i c 0 4 in
  let (tl2,c) = sei_stpl (Base(0)) i c 2 4 in
  let tl = tl2 @ tl1 in
  let (fr1,c) = (sei_ho_unif_flex_rigid 4 tl i c) in
  let fr1 = allho tl2 fr1 in
  let (tl2,c) = sei_stpl (Base(0)) i c 2 4 in
  let tl = tl2 @ tl1 in
  let (fr2,c) = sei_ho_unif_flex_rigid 4 tl i c in
  let fr2 = allho tl2 fr2 in
  let (tl2,c) = sei_stpl (Base(0)) i c 2 4 in
  let tl = tl2 @ tl1 in
  let (fr3,c) = sei_ho_unif_flex_rigid 5 tl i c in
  let fr3 = allho tl2 fr3 in
  let (tl2,c) = sei_stpl (Base(0)) i c 2 4 in
  let tl = tl2 @ tl1 in
  let (fr4,c) = sei_ho_unif_flex_rigid 5 tl i c in
  let fr4 = allho tl2 fr4 in
  let (tl2,c) = sei_stpl (Base(0)) i c 2 4 in
  let tl = tl2 @ tl1 in
  let (fr5,c) = sei_ho_unif_flex_rigid 6 tl i c in
  let fr5 = allho tl2 fr5 in
  let (tl2,c) = sei_stpl (Base(0)) i c 2 4 in
  let tl = tl2 @ tl1 in
  let (fr6,c) = sei_ho_unif_flex_rigid 6 tl i c in
  let fr6 = allho tl2 fr6 in
  let (tl2,c) = sei_stpl (Base(0)) i c 2 4 in
  let tl = tl2 @ tl1 in
  let (fr7,c) = sei_ho_unif_flex_rigid 7 tl i c in
  let fr7 = allho tl2 fr7 in
  let (tl2,c) = sei_stpl (Base(0)) i c 2 4 in
  let tl = tl2 @ tl1 in
  let (fr8,c) = sei_ho_unif_flex_rigid 7 tl i c in
  let fr8 = allho tl2 fr8 in
  (allho tl1
     (Imp(fr1,Imp(fr2,Imp(fr3,Imp(fr4,Imp(fr5,Imp(fr6,Imp(fr7,Imp(fr8,Prim(1)))))))))),c)

let sei_ho_setconstr pol x tl i c =
  let ii = TpArr(Base(0),Base(0)) in
  let iii = TpArr(Base(0),ii) in
  let rec rand_ho_ind tl i c lev =
    let (h,a,c) =
      if lev < 2 then
        let (b,c) = i 2 c in
        if b = 0 then
          (Prim(64),ii,c)
        else if b = 1 then
          (Prim(65),ii,c)
        else if b = 2 then
          let (b,c) = i 1 c in
          (Prim(68),iii,c)
        else
          let (b,c) = i 2 c in
          (DB(b),List.nth tl b,c)
      else if lev > 8 then
        (Prim(9),Base(0),c)
      else
        let (b,c) = i 2 c in
        if b = 0 then
          (Prim(9),Base(0),c)
        else if b = 1 then
          let (b,c) = i 1 c in
          if b = 0 then
            (Prim(64),ii,c)
          else
            (Prim(65),ii,c)
        else if b = 2 then
            (Prim(68),iii,c)
        else
          let (b,c) = i 2 c in
          (DB(b),List.nth tl b,c)
    in
    rand_ho_spine h a tl i c (lev+1)
  and rand_ho_trm a tl i c lev =
    match a with
    | TpArr(a1,a2) ->
       let (m,c) = rand_ho_trm a2 (a1::tl) i c lev in
       (Lam(a1,m),c)
    | Base(j) when j = 0 -> rand_ho_ind tl i c lev
    | _ -> raise HFPropFailure (* should never happen *)
  and rand_ho_spine m a tl i c lev =
    match a with
    | TpArr(a1,a2) ->
       let (ma,c) = rand_ho_trm a1 tl i c (lev+1) in
       rand_ho_spine (Ap(m,ma)) a2 tl i c lev
    | _ -> (m,c)
  in
  let rand_rigid_prop tl i c = (** use In as the only rigid relation **)
    rand_ho_spine (Prim(7)) (TpArr(Base(0),TpArr(Base(0),Prop))) tl i c 0
  in
  let (p1,c) = rand_ho_spine (DB(x)) (List.nth tl x) tl i c 1 in
  let (b,c) = i 2 c in
  if b = 0 then
    if pol then
      (p1,c)
    else
      (Imp(p1,Prim(1)),c)
  else if b = 1 then
    let (p2,c) = rand_rigid_prop tl i c in
    if pol then
      (Imp(p2,p1),c)
    else
      (Imp(p1,p2),c)
  else if b = 2 then
    let (p2,c) = rand_rigid_prop tl i c in
    let (b,c) = i 2 c in
    let (p3,c) = rand_ho_spine (DB(b+4)) (List.nth tl (b+4)) tl i c 1 in
    if pol then
      (Imp(p2,Imp(p3,p1)),c)
    else
      (Imp(p2,Imp(p1,p3)),c)
  else
    let (b,c) = i 2 c in
    let (p3,c) = rand_ho_spine (DB(b+4)) (List.nth tl (b+4)) tl i c 1 in
    if pol then
      (Imp(p3,p1),c)
    else
      (Imp(p1,p3),c)
    
let sei_ho_setconstr_prop i c =
  let rec allho tl p =
    match tl with
    | [] -> p
    | a::tr -> allho tr (All(a,p))
  in
  let (tl1,c) = sei_stpl Prop i c 0 4 in
  let (tl2,c) = sei_stpl (Base(0)) i c 2 4 in
  let tl = tl2 @ tl1 in
  let (scon1,c) = (sei_ho_setconstr true 4 tl i c) in
  let scon1 = allho tl2 scon1 in
  let (tl2,c) = sei_stpl (Base(0)) i c 2 4 in
  let tl = tl2 @ tl1 in
  let (scon2,c) = sei_ho_setconstr false 4 tl i c in
  let scon2 = allho tl2 scon2 in
  let (tl2,c) = sei_stpl (Base(0)) i c 2 4 in
  let tl = tl2 @ tl1 in
  let (scon3,c) = sei_ho_setconstr true 5 tl i c in
  let scon3 = allho tl2 scon3 in
  let (tl2,c) = sei_stpl (Base(0)) i c 2 4 in
  let tl = tl2 @ tl1 in
  let (scon4,c) = sei_ho_setconstr false 5 tl i c in
  let scon4 = allho tl2 scon4 in
  let (tl2,c) = sei_stpl (Base(0)) i c 2 4 in
  let tl = tl2 @ tl1 in
  let (scon5,c) = sei_ho_setconstr true 6 tl i c in
  let scon5 = allho tl2 scon5 in
  let (tl2,c) = sei_stpl (Base(0)) i c 2 4 in
  let tl = tl2 @ tl1 in
  let (scon6,c) = sei_ho_setconstr false 6 tl i c in
  let scon6 = allho tl2 scon6 in
  let (tl2,c) = sei_stpl (Base(0)) i c 2 4 in
  let tl = tl2 @ tl1 in
  let (scon7,c) = sei_ho_setconstr true 7 tl i c in
  let scon7 = allho tl2 scon7 in
  let (tl2,c) = sei_stpl (Base(0)) i c 2 4 in
  let tl = tl2 @ tl1 in
  let (scon8,c) = sei_ho_setconstr false 7 tl i c in
  let scon8 = allho tl2 scon8 in
  (allho tl1
     (Imp(scon1,Imp(scon2,Imp(scon3,Imp(scon4,Imp(scon5,Imp(scon6,Imp(scon7,Imp(scon8,Prim(1)))))))))),c)

let sei_comb_unif_flex_rigid x i c =
  let empty = Prim(9) in
  let power = Prim(11) in
  let setsum = Prim(68) in
  let inj0 = Prim(65) in
  let inj1 = Prim(64) in
  let combk = Ap(inj0,empty) in
  let combs = Ap(inj0,Ap(power,empty)) in
  let combap m1 m2 = Ap(inj1,Ap(Ap(setsum,m1),m2)) in
  let rec rand_rigid_comb i c lev =
    let (b,c) = i 1 c in
    if b = 0 then
      let (b,c) = i 1 c in
      if b = 0 then
        let (m1,c) = rand_comb i c (lev+1) in
        (combap combk m1,c)
      else
        let (m1,c) = rand_comb i c (lev+1) in
        let (b,c) = i 1 c in
        if b = 0 then
          (combap combs m1,c)
        else
          let (m2,c) = rand_comb i c (lev+1) in
          (combap (combap combs m1) m2,c)
    else
      let (b,c) = i 2 c in
      rand_comb_spine (DB(b)) i c (lev+1)
  and rand_comb i c lev =
    let (h,c) =
      let (b,c) = i 2 c in
      if b = 0 then
        (combk,c)
      else if b = 1 then
        (combs,c)
      else if b = 2 then
        let (b,c) = i 2 c in
        (DB(b),c)
      else
        let (b,c) = i 2 c in
        (DB(b+4),c)
    in
    if lev > 7 then
      (h,c)
    else
      rand_comb_spine h i c (lev+1)
  and rand_comb_spine m i c lev =
    if lev < 4 then
      rand_comb_spine_1 m i c lev
    else (* decide whether to include another argument *)
      let (b,c) = i 1 c in
      if b = 0 then
        (m,c)
      else
        rand_comb_spine_1 m i c lev
  and rand_comb_spine_1 m i c lev = (* at least one argument *)
    let (ma,c) = rand_comb i c (lev+1) in
    rand_comb_spine (combap m ma) i c (lev+1)
  in
  let (a1,c) = i 2 c in
  let (a2,c) = i 2 c in
  let (a3,c) = i 2 c in
  let (a4,c) = i 2 c in
  let (m1,c) = rand_comb_spine (combap (combap (combap (combap (DB(x)) (DB(a1))) (DB(a2))) (DB(a3))) (DB(a4))) i c (if !Config.testnet then 3 else 1) in
  let (m2,c) = rand_rigid_comb i c (if !Config.testnet then 2 else 0) in
  (Ap(Ap(Prim(102),m1),m2),c)

let sei_comb_unif_prop i c =
  let (fr1,c) = sei_comb_unif_flex_rigid 4 i c in
  let (fr2,c) = sei_comb_unif_flex_rigid 4 i c in
  let (fr3,c) = sei_comb_unif_flex_rigid 5 i c in
  let (fr4,c) = sei_comb_unif_flex_rigid 5 i c in
  let (fr5,c) = sei_comb_unif_flex_rigid 6 i c in
  let (fr6,c) = sei_comb_unif_flex_rigid 6 i c in
  let (fr7,c) = sei_comb_unif_flex_rigid 7 i c in
  let (fr8,c) = sei_comb_unif_flex_rigid 7 i c in
  let allcomb p = All(Base(0),Imp(Ap(Prim(101),DB(0)),p)) in
  let allcomb4 p = allcomb (allcomb (allcomb (allcomb p))) in
  (allcomb4
     (Imp(allcomb4 fr1,Imp(allcomb4 fr2,Imp(allcomb4 fr3,Imp(allcomb4 fr4,Imp(allcomb4 fr5,Imp(allcomb4 fr6,Imp(allcomb4 fr7,Imp(allcomb4 fr8,Prim(1)))))))))),c)

let reward_bounty_prop_main h =
  let sb = Buffer.create 2048 in
  for i = 0 to 63 do
    Buffer.add_string sb (hexstring_string (hashval_hexstring (hashtag h (Int32.of_int i))))
  done;
  let s = Buffer.contents sb in
  let (_,_,_,_,_,_,x1,x0) = h in
  let i = Base(0) in
  let io = TpArr(i,Prop) in
  let ii = TpArr(i,i) in
  let iio = TpArr(i,io) in
  let iii = TpArr(i,ii) in
  try
    let x = Int32.logand x0 7l in
    let y = Int32.logand x1 1l in
    if x = 0l then (** random **)
      if y = 0l then (** random HF **)
        begin
          let (p,c) = sei_hf_prop [] seis (s,2048,None,0,0) true in
          if not !Config.testnet && bytesused c < 10 then raise HFPropFailure;
          (0,p)
        end
      else (** random HF with an uninterp pred and uninterp func **)
        begin
          let (p,c) = sei_hf_prop [ii;io] seis (s,2048,None,0,0) true in
          if not !Config.testnet && bytesused c < 10 then raise HFPropFailure;
          (1,All(io,All(ii,p)))
        end
    else if x = 1l then (** random prop not using (most) HF constructs, but several uninterp preds and funcs **)
      begin
        let (p,c) = sei_hf_prop [i;i;i;ii;io;io;iii;iio] seis (s,2048,None,0,0) false in
        if not !Config.testnet && bytesused c < 10 then raise HFPropFailure;
        (2,All(iio,All(iii,All(io,All(io,All(ii,All(i,All(i,All(i,p)))))))))
      end
    else if x = 2l then
      if y = 0l then (** random QBF **)
        begin
          let (p,c) = sei_qbf_prop seis (s,2048,None,0,0) in
          if not !Config.testnet && bytesused c < 10 then raise HFPropFailure;
          (3,p)
        end
      else (** HO Set Constraints Problem **)
        begin
          let (p,c) = sei_ho_setconstr_prop seis (s,2048,None,0,0) in
          if not !Config.testnet && bytesused c < 10 then raise HFPropFailure;
          (4,p)
        end
    else if x = 3l then (** Unification Problem **)
      if y = 0l then (** HO (Typed) Unification Problem **)
        begin
          let (p,c) = sei_ho_unif_prop seis (s,2048,None,0,0) in
          if not !Config.testnet && bytesused c < 10 then raise HFPropFailure;
          (5,p)
        end
      else (** Untyped Combinator Unification Problem **)
        begin
          let (p,c) = sei_comb_unif_prop seis (s,2048,None,0,0) in
          if not !Config.testnet && bytesused c < 10 then raise HFPropFailure;
          (6,p)
        end
    else if x = 4l then (** Abstract HF FO Problems **)
      (7,sei_abstr_hf_prop seis (s,2048,None,0,0))
    else if x = 5l then (** Diophantine modulo **)
      (8,sei_diophantine_mod seis (s,2048,None,0,0))
    else if x = 6l then (** AIM 1 **)
      (9,sei_aim1 seis (s,2048,None,0,0))
    else (** AIM 2 **)
      (10,sei_aim2 seis (s,2048,None,0,0))
  with
  | _ -> (** fall back on Diophantine in case of failure **)
      (11,sei_diophantine seis (s,2048,None,0,0))

let reward_bounty_prop h =
  let (cls,p) = reward_bounty_prop_main h in
  try
    match beta_eta_delta_norm p ([],[]) with
    | Some(q) -> (cls,p,q)
    | None -> raise HFPropFailure
  with _ -> (cls,p,p) (** this shouldn't happen, but if it does, the bounty is likely burned since p is not normal **)