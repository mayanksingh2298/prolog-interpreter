edge(a,b).
edge(b,c).
edge(c,d).
edge(a,d).
path(P1,Q1) :- edge(P1,Q1).
path(P,Q) :- edge(P,R), path(R,Q).
