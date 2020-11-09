(* Copyright (c) 2020 The Proofgold developers *)
(* Copyright (c) 2016 The Qeditas developers *)
(* Copyright (c) 2017-2018 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Hash
open Logic
open Mathdata

exception RedexMax

val beta_eta_delta_norm : trm -> gsign -> trm option

val correct_trm : stp list -> gsign -> trm -> stp -> stp list -> bool
val correct_pf : stp list -> trm list -> gsign -> pf -> trm -> stp list -> bool

val check_theoryspec : theoryspec -> (theory * gsign) option

val check_signaspec :
  (hashval option -> hashval -> stp -> bool) -> (hashval option -> hashval ->
  bool) -> hashval option -> theory -> stree option -> signaspec ->
  (gsign * hashval list) option

val check_doc :
  (hashval option -> hashval -> stp -> bool) -> (hashval option -> hashval ->
  bool) -> hashval option -> theory -> stree option -> doc ->
  (gsign * hashval list) option

val hfprimnamesa : string array
val hfprimtps : stp list
val hf_info : stp list * gsign
val hfthyspec : theoryspec
val hfthy : theory
val hfthyid : hashval
val initthytree : ttree
val initthytreeroot : hashval

exception HFPropFailure

val propformersa : (int * Logic.stp) array
val setformersa : (int * Logic.stp) array
val ahfctx : Logic.stp list
val ahfprops : Logic.trm array

val sei_hf_prop : stp list -> (int -> Ser.seist -> int * Ser.seist) -> Ser.seist -> bool -> trm * Ser.seist
val sei_hf_set : stp list -> (int -> Ser.seist -> int * Ser.seist) -> Ser.seist -> bool -> trm * Ser.seist

val sei_qbf_prop : (int -> Ser.seist -> int * Ser.seist) -> Ser.seist -> trm * Ser.seist
val sei_ho_setconstr_prop : (int -> Ser.seist -> int * Ser.seist) -> Ser.seist -> trm * Ser.seist
val sei_ho_unif_prop : (int -> Ser.seist -> int * Ser.seist) -> Ser.seist -> trm * Ser.seist
val sei_comb_unif_prop : (int -> Ser.seist -> int * Ser.seist) -> Ser.seist -> trm * Ser.seist
val sei_abstr_hf_prop : (int -> Ser.seist -> int * Ser.seist) -> Ser.seist -> trm
val sei_diophantine_mod : (int -> Ser.seist -> int * Ser.seist) -> Ser.seist -> trm
val sei_diophantine : (int -> Ser.seist -> int * Ser.seist) -> Ser.seist -> trm
val sei_aim1 : (int -> Ser.seist -> int * Ser.seist) -> Ser.seist -> trm
val sei_aim2 : (int -> Ser.seist -> int * Ser.seist) -> Ser.seist -> trm

val reward_bounty_prop : hashval -> int * trm * trm
val hf_trm_str : trm -> string list -> string
val ahf_trm_str : trm -> string list -> string
val aim_trm_str : trm -> string list -> string
val comb_trm_str : trm -> string list -> string

val ahf_fof_prob : out_channel -> trm -> unit
val aim1_fof_prob : out_channel -> trm -> unit
val aim2_fof_prob : out_channel -> trm -> unit
val comb_fof_prob : out_channel -> trm -> unit
val qbf_fof_prob : out_channel -> trm -> unit

val hf_thf_prob : out_channel -> trm -> unit
val hf_mg_prob : out_channel -> trm -> unit
