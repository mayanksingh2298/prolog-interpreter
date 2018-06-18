ancestor(bob, susan).
ancestor(A, X1) :- parent(A, X1).
ancestor(A, X2) :- parent(A, C1), ancestor(C1, X2).
parent(fred, sally).
parent(tina, sally).
parent(sally, john).
parent(sally, diane).
parent(sam, bill).