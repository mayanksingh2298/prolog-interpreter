edge(a,b).
edge(b,c).
edge(c,a).
path(Y1,Y1).
path(P,Q):-edge(P,R),path(R,Q).
cycle(X1):-path(X1,X1).