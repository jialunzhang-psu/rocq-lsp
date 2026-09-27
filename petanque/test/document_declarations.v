From Coq Require Import Init.Logic.

Theorem top : True. exact I. Qed.

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
