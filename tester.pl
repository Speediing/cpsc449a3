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

toonear(a,a).
forcedpartial(a,0).
forbidden(a,0).
toonearp(a,a,0).

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
	
:-dynamic lowlist/1.
:-dynamic lowpen/1.
lowlist([]).
lowpen(9999999999999999).

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
	
%toonear(a,b).
%toonear(g,f).
%toonear(b,c).
%toonear(d,e).

%forcedpartial(a,1).
%forcedpartial(b,2).
%forcedpartial(c,3).
%forcedpartial(d,4).
%forcedpartial(e,5).

%forbidden(a,2).
%forbidden(b,2).
%forbidden(c,2).
%forbidden(d,2).
%forbidden(e,2).

hardcons(List):-
	not(tntcheck(List)),
	fpcheck(List),
	fmcheck(List).

tntcheck(List):-
	forall(toonear(X,Y),closecheck(List,X,Y)).

length_1(0,[]).
length_1(L+1, [H|T]) :- length_1(L,T).

closecheck(List,X,Y):-
	X==Y;
	proper_length(List,8),
	Imputlist = [H|T],
	reverse(List,Rlist),
	Rlist = [HR|TR],
	X==HR,
	Y==H;
	length_1(L,List),
	M is L,
	(M==1->true;
	nth1(M,List,W),	
	N is L-1,
	nth1(N,List,V),
	X==V,
	Y==W).
	
fpcheck(Inputlist):-
	forall(forcedpartial(X,Y),forcedtaskcheck(Inputlist,X,Y)).
	
forcedtaskcheck(List,X,Y):-
	not(proper_length(List,Y));
	proper_length(List,Y),
	last(List,X).	
	
fmcheck(Inputlist):-
	forall(forbidden(X,Y),forbiddencheck(Inputlist,X,Y)).

forbiddencheck(List,X,Y):-
	not(proper_length(List,Y));
	proper_length(List,Y),
	not(last(List,X)).

nodegrow(8,Y) :-
	asserta(node(8,Y)).
nodegrow(X,Y) :-
	write("nodegrow"),
	write(Y),
	X2 is X+1,
	subtract([a,b,c,d,e,f,g,h],Y,Remaining),
	branchalot(X2,Y,Remaining).
	
branchalot(X,Y,[]).
branchalot(X,Y,[H|T]) :-
	write("branchalot"),
	write(Y),
	append(Y,[H],Total),
	nodegrow(X,Total),
	branchalot(X,Y,T).

masterrule(Depth, Assignment ) :-
	\+ (Assignment).


machinepen(Input, Returnpen, Depth) :-
    nth1(Depth, Input, Lastelement),
    mc(Depth, Lastelement, Returnpen).


 penalty(1, a, 4).
 penalty(1, b, 4).
 penalty(1, c, 5).
 penalty(2, d, 22).
 penalty(3, e, 21).
 penalty(1, f, 7).
 person(name,age).
	
youngest(penalty(Task, Machine, Penalty)) :-
	penalty(Task, Machine, Penalty),
  	\+  (penalty(Task2, Machine2, Penalty2),Penalty2 < Penalty).

findPen(task, mach) :-
	penalty(task, mach, X).