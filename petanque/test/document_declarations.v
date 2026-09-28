From Coq Require Import Init.Logic.

Theorem top : True. exact I. Qed.

Inductive even : nat -> Prop :=
| even_O : even 0
| even_S : forall n, odd n -> even (S n)
with odd : nat -> Prop :=
| odd_S : forall n, even n -> odd (S n).

Theorem mutual_first : forall n, even n -> True
with mutual_second : forall n, odd n -> True.
Proof.
- intros. exact I.
- intros. exact I.
Qed.

Module A.
Theorem same : True. exact I. Qed.
Definition value := 1.
Module Inner.
Lemma deep : True. exact I. Qed.
End Inner.
End A.

Module B.
Fact same : True. exact I. Qed.
End B.

Section S.
Remark section_leaf : True. exact I. Qed.
End S.

Axiom permitted_axiom : True.
Theorem admitted_leaf : True. Admitted.
Theorem aborted_leaf : True. Abort.
