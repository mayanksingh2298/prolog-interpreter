edge(a,b).
edge(b,c).
edge(c,a).
path(a,a).
path(X,Y):-edge(X,Z),path(Z,Y).
