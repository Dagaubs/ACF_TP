theory tp89
imports Main (* "~~/src/HOL/Library/Code_Target_Nat" *)
begin

(* 
quickcheck_params [size=6,tester=narrowing,timeout=120]
nitpick_params [timeout=120]
*)

type_synonym transid= "nat*nat*nat"

datatype message= 
  Pay transid nat
| Ack transid nat
| Cancel transid

datatype valueProposed = 
  Undef 
| Def nat

datatype transactionState=
  Validate transid nat
| Current transid valueProposed valueProposed
| Abort transid

type_synonym transaction= "transid * nat"

type_synonym transBdd = "transactionState list"

(* exemple de message List *)

(* result m1 = [] *)
definition "m1 = [(Ack (1,2,1) 20), (Pay (1,2,1) 18)]"

(* result m2 = [(1,2,1) 20] *)
definition "m2 = [(Ack (1,2,1) 20), (Pay (1,2,1) 20)]"

(* result m3 = [] *)
definition "m3 = [(Ack (1,2,1) 20), (Pay (1,2,1) 18), (Pay (1,2,2) 21)]"

(* result m4 = [(1,2,1) 20] *)
definition "m4 = [(Ack (1,2,1) 21), (Pay (1,2,1) 20), (Ack (1,2,1) 20)]"

(*
fun validateTransactionAux::"message list \<Rightarrow> transaction list \<Rightarrow> transaction list"
where
"validateTransactionAux [] tl = []"|
"validateTransactionAux ((Pay t i)#ls) tl =(if(validatePayement t i tl) then ((t,i)#(validateTransactionAux ls tl)) else  validateTransactionAux ls tl)"|
"validateTransactionAux ((Ack t i)#ls) tl ="|
"validateTransactionAux ((Cancel t)#ls) tl ="
*)

fun traiterMessage::"message \<Rightarrow> transBdd \<Rightarrow> transBdd"
where
"traiterMessage (Pay t i) ((Validate t2 n)#bdd) = ( if t=t2 then ((Validate t2 n)#bdd) else (Validate t2 n)#(traiterMessage (Pay t i) bdd))" |
"traiterMessage (Pay t i) ((Current t2 pc pm)#bdd) =( if t=t2 then ( case (pc,pm) of (Undef, Undef) \<Rightarrow> ((Current t2 (Def i) Undef)#bdd) |
                                                                                      (Def c, Undef) \<Rightarrow> (if i \<le> c then (Current t2 (Def c) Undef)#bdd else (Current t2 (Def i) Undef)#bdd) |
                                                                                      (Def c, Def m) \<Rightarrow> (if i \<le> c then (Current t2 (Def c) Undef)#bdd else ( if(i < m) then (Current t2 (Def i) (Def m))#bdd else (Validate t2 i)#bdd)) |
                                                                                      (Undef, Def m) \<Rightarrow> (if i \<le> m then (Current t2 (Def i) (Def m))#bdd else (Validate t2 i)#bdd)) 
                                                               else ((Current t2 pc pm)#(traiterMessage (Pay t i) bdd)))" |
"traiterMessage (Pay t i) ((Abort t2)#bdd) = ( if t = t2 then ((Abort t2)#bdd) else (Abort t2)#(traiterMessage (Pay t i) bdd))" |
"traiterMessage (Pay t i) [] = [(Current t (Def i) Undef)]" |
"traiterMessage (Ack t i) ((Validate t2 n)#bdd) = ( if t = t2 then ((Validate t2 n)#bdd) else (Validate t2 n)#(traiterMessage (Ack t i) bdd))" |
"traiterMessage (Ack t i) ((Current t2 pc pm)#bdd) =( if(t=t2) then ( case (pc,pm) of (Undef, Undef) \<Rightarrow> ((Current t2 Undef (Def i))#bdd) |
                                                                                      (Def c, Undef) \<Rightarrow> (if i \<le> c then (Validate t2 c)#bdd else (Current t2 (Def c) (Def i))#bdd) |
                                                                                      (Def c, Def m) \<Rightarrow> (if i \<le> c then (Validate t2 c)#bdd else ( if(i < m) then (Current t2 (Def c) (Def i))#bdd else (Current t2 (Def c) (Def m))#bdd)) |
                                                                                      (Undef, Def m) \<Rightarrow> (if i \<le> m then (Current t2 Undef (Def i))#bdd else (Current t2 Undef (Def m))#bdd)) 
                                                               else ((Current t2 pc pm)#(traiterMessage (Ack t i) bdd)))" |
"traiterMessage (Ack t i) ((Abort t2)#bdd) = ( if t = t2 then ((Abort t2)#bdd) else (Abort t2)#(traiterMessage (Ack t i) bdd))" |
"traiterMessage (Ack t i) [] = [(Current t Undef (Def i))]" |
"traiterMessage (Cancel t) ((Validate t2 n)#bdd) = ( if t = t2 then ((Validate t2 n)#bdd) else (Validate t2 n)#(traiterMessage (Cancel t) bdd))" |
"traiterMessage (Cancel t) ((Current t2 pc pm)#bdd) =( if(t=t2) then (Abort t)#bdd else (Current t2 pc pm)#(traiterMessage (Cancel t) bdd))" |
"traiterMessage (Cancel t) ((Abort t2)#bdd) = ( if t = t2 then ((Abort t2)#bdd) else (Abort t2)#(traiterMessage (Cancel t) bdd))" |
"traiterMessage (Cancel t) [] = [(Abort t)]"

fun addTransactionStateToList::"transactionState \<Rightarrow> transaction list \<Rightarrow> transaction list"
where
"addTransactionStateToList (Validate t n) ts = (t, n)#ts" |
"addTransactionStateToList _ ts = ts"

fun sortirTransactionList::"transBdd \<Rightarrow> transaction list \<Rightarrow> transaction list"
where
"sortirTransactionList (x#bdd) t = (sortirTransactionList bdd (addTransactionStateToList x t))" |
"sortirTransactionList [] t = t"

fun messageListToTransBdd::"message list \<Rightarrow> transBdd \<Rightarrow> transBdd"
where
"messageListToTransBdd (m#ls) t = (messageListToTransBdd ls (traiterMessage m t))" |
"messageListToTransBdd [] t = t" 

fun validateTransaction::"message list \<Rightarrow> transaction list"
where
"validateTransaction messageList = sortirTransactionList (messageListToTransBdd messageList []) []"

value "valisateTransaction m1"
value "validatetransaction m2"

(* ----- Exportation en Scala (Isabelle 2014) -------*)

(* Directive d'exportation *)
export_code export traiterMessage in Scala



end

