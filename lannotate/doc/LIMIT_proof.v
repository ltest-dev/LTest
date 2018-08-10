Require Import ZArith.BinInt.

Theorem th : forall (a n : Z),
  (0 <= n ->
  Z.abs a <= n <-> a <= n /\ -a <= n)%Z.
Proof.
  intros a n n_pos. split.
  - intros H. destruct a as [|a'|a'].
    + split.
      * apply n_pos.
      * simpl. apply n_pos.
    + split.
      * simpl in H. apply H.
      * simpl. apply Z.le_trans with (m:=0%Z).
        apply Pos2Z.neg_is_nonpos. apply n_pos.
    + split.
      * apply Z.le_trans with (m:=0%Z).
        apply Pos2Z.neg_is_nonpos. apply n_pos.
      * simpl. simpl in H. apply H.
  - intros [po ne]. destruct a as [|a'|a'].
    + simpl. apply n_pos.
    + simpl. apply po.
    + simpl. simpl in ne. apply ne.
Qed. 