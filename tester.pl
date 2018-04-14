task(a).
task(b).
task(c).
task(d).
task(e).
task(f).
task(g).
task(h).

testnode(23,[e]).
testnode(14,[c]).
testnode(8,[b]).
testnode(999,[a]).
testnode(99,[d]).

machine(1).
machine(2).
machine(3).
machine(4).
machine(5).
machine(6).
machine(7).
machine(8).

bad(1).
bad(2).
bad(b).
bad(c).
bad([a]).
bad([b]).
findlowest:-
	forall(testnode(X,Y),anus(X,Y)).
	
anus(X,Y):-
	lowpen(Z),
	lowlist(W),
	( X=<Z->
	  write("UPDATE IN PROGRESS"),
	  retractall(lowlist(Y)),
	  asserta(lowlist(W)),
	  retractall(lowpen(Z)),
	  asserta(lowpen(X))
	).
lowlist([]).
lowpen(9999999999999999).
:-dynamic(
lowlist/1,
lowpen/1
).

nodevalid(0,[]).
nodevalid(Something, [H|T]) :-
	is_set([H|T]),
	Something_less is Something-1,
	reverse([H|T],Reversed),
	Reversed=[HR|TR],
	reverse(TR,TRR),
	nodevalid(Something_less, TRR).
	

correctassign([]).
correctassign([T|H]) :-
	task(T),
	correctassign(H).
	

toonear(a,b).
toonear(g,f).
forcedpartial(a,3).

tncheck(Inputlist):-
	forall(toonear(X,Y),closecheck(Inputlist,X,Y)).

closecheck(Inputlist,X,Y):-
	append([X],[Y],Z1),
	append([Y],[X],Z2),
	not(sublist(Z1,Inputlist)),
	not(sublist(Z2,Inputlist)).
	
fpcheck(Inlist,Indepth):-
	forcedpartial(X,Y),
	reverse(Inputlist,Reversed),
	Reversed=[H|T],
	Y=Inputdepth,
	H=X.

nodegrow(8,Y) :-
	asserta(node(8,Y)).
nodegrow(X,Y) :-
	X2 is X+1,
	subtract([a,b,c,d,e,f,g,h],Y,Remaining),
	branchalot(X2,Y,Remaining).
	
branchalot(X,Y,[]).
branchalot(X,Y,[H|T]) :-

	append(Y,[H],Total),
	masterrule(X, Y) -> nodegrow(X,Total),

	branchalot(X,Y,T).
	
sublist( [], _ ).
sublist( [X|XS], [X|XSS] ) :- sublist( XS, XSS ).
sublist( [X|XS], [_|XSS] ) :- sublist( [X|XS], XSS ).

masterrule(Depth, Assignment ) :-
	\+ (Assignment).


machinepen(Input, Returnpen, Depth) :-
    nth1(Depth, Input, Lastelement),
    mc(Depth, Lastelement, Returnpen).

	