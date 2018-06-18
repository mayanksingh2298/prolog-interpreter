likes(joe,books).
likes(joe,mary).
likes(mary,books).
likes(john,books).
likes(sue,joe).
likes(john,mary).
likes(mary,joe).
likes(mary,movies).
likes(bill,X1) :- likes(X1,books), likes(X1,movies).
likes(alice,X2) :- likes(X2,mary).