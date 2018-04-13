:- initialization main.
:- dynamic(
  fileContents/1,
  hardConstraints/3,
  softConstraints/1,
  forcedPartial/2,
  forbiddenMachine/2,
  tooNearTask/2,
  tooNearPenalties/2
).

myTree(hardConstraints, softConstraints,matrix).
hardConstraints(forcedPartial,forbiddenMachine,tooNearTask).
softConstraints(tooNearPenalties).
/*forcedPartial(Machine,Task).
forbiddenMachine(Machine, Task).
tooNearTask(Task, Task).
tooNearPenalties(Task, Task, Penalty).
*/
machine(1).
machine(2).
machine(3).
machine(4).
machine(5).
machine(6).
machine(7).
machine(8).

task('A').
task('B').
task('C').
task('D').
task('E').
task('F').
task('G').
task('H').



main :-
  current_prolog_flag(argv, Argv),
  /*format('Hello world, argv:~w\n', [Argv]),*/
  nth0(0, Argv, InputFile),
  nth0(1, Argv, OutPutFile),
  write(InputFile),
  write(' '),
  write(OutPutFile), nl,
  /*phrase_from_file(lines(Ls), InputFile),
  atom_codes(A,Ls),
  nth0(0, Ls, L),
  nth0(0, L, L1),
  char_code(Char, L1),
  */
  startInput(InputFile,OutPutFile),
  /*print(fileContents()),
  */halt(0).

  startInput(X,Y) :-
    see(X),
    storeToList(3,Z),
    seen,
    select(-1, Z, Z1),
  %  write(Z1),
  /*  atomics_to_string(Z1,Contents),*/
  %  print(Z1),nl,nl,nl,
  %  atom_codes(A,Z1),
  %  write(A),
   select(13,Z1, Z2),
    %write(Z2),nl,
    split_on_delimiter(Z2,10, Z3),
    print(Z3),
    printlist(Z3),!,
  checkName(Z3,0,Z3,L1),!,
  (L1 == ["exit 1"] -> write_file(Y,"Error while parsing input file");true),
  checkFPA(L1,L1,L2),!,
  (L2 == ["exit 1"] -> write_file(Y,"Error while parsing input file");true),
  (L2 == ["exit 3"] -> write_file(Y,"invalid machine/task");true),
  checkFM(L2,L2,L3),!,
  (L3 == ["exit 1"] -> write_file(Y,"Error while parsing input file");true),
  (L3 == ["exit 3"] -> write_file(Y,"invalid machine/task");true),
  checkTNT(L3,L3,L4),!,
  write(L4),nl,
  (L4 == ["exit 1"] -> write_file(Y,"Error while parsing input file");true),
  (L4 == ["exit 3"] -> write_file(Y,"invalid machine/task");true),
  checkMP(L4,0,L4,L5),!,
  write(L5),nl,
  (L5 == ["exit 1"] -> write_file(Y,"Error while parsing input file");true),
  (L5 == ["exit 4"] -> write_file(Y,"machine penalty error");true),
  (L5 == ["exit 6"] -> write_file(Y,"invalid penalty");true),
  checkTNP(L5,L5,L6),!,
  (L6 == ["exit 1"] -> write_file(Y,"Error while parsing input file");true),
  (L6 == ["exit 5"] -> write_file(Y,"invalid task");true),
  (L6 == ["exit 6"] -> write_file(Y,"invalid penalty");true),!.

    write_file(X,Y):-
      tell(X),
      outputstuff(Y),
      told,!.

    outputstuff(Y):-
      atom_codes(X,Y),
      write(X).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

checkFPA([H|T],[H2|_],Y) :-
  phrase(isSpaceLine, H) -> checkFPA(T,H,Y);checkFPA11([H|T],[H2|_],Y).
checkFPA11([H|T],[H2|_],Y) :-
    phrase(fmString, H), phrase(isSpaceLine, [H2]) -> Y = T;checkFPA22([H|T],[H2|_],Y).
checkFPA22([H|T],[H2|_],Y) :-
    phrase(fpaString, H) -> Y = ["exit 1"];checkFPA33([H|T],[H2|_],Y).
% checkFPA33([H|T],[H2|_],Y) :-
%       phrase(isSpaceLine, H) -> Y = ["exit 1"];checkFPA44([H|T],[H2|_],Y).
checkFPA33([H|T],[H2|_],Y) :-
      phrase(fpaFormat, H), phrase(fpaError, H) -> checkFPA(T,H,Y);checkFPA44([H|T],[H2|_],Y).
checkFPA44([H|T],[H2|_],Y) :-
      \+ phrase(fpaFormat, H), \+ phrase(fmString,H) -> Y = ["exit 1"];checkFPA55([H|T],[H2|_],Y).
checkFPA55([H|T],[H2|_],Y) :-
      \+ phrase(fpaError, H), \+ phrase(fmString, H) -> Y = ["exit 3"];checkFPA66([H|T],[H2|_],Y).
checkFPA66([H|T],[H2|_],Y) :-
      phrase(tntSting, H) -> Y = ["exit 1"];checkFPA77([H|T],[H2|_],Y).
checkFPA77([H|T],[H2|_],Y) :-
      phrase(mpString, H) -> Y = ["exit 1"];checkFPA88([H|T],[H2|_],Y).
checkFPA88([H|T],[H2|_],Y) :-
      phrase(tnpString, H) -> Y = ["exit 1"];checkFPA99([H|T],[H2|_],Y).
checkFPA99([H|T],[H2|_],Y) :-
    Y = ["exit 1"].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

checkFM([H|T],[H2|_],Y) :-
  phrase(isSpaceLine, H) -> checkFM(T,H,Y);checkFM11([H|T],[H2|_],Y).
checkFM11([H|T],[H2|_],Y) :-
    phrase(tntString, H), phrase(isSpaceLine, [H2]) -> Y = T;checkFM22([H|T],[H2|_],Y).
checkFM22([H|T],[H2|_],Y) :-
    phrase(fmString, H) -> Y = ["exit 1"];checkFM44([H|T],[H2|_],Y).
% checkFM33([H|T],[H2|_],Y) :-
%       phrase(isSpaceLine, H) -> Y = ["exit 1"];checkFM44([H|T],[H2|_],Y).
checkFM44([H|T],[H2|_],Y) :-
      phrase(fmFormat, H), phrase(tntError, H) -> checkFM(T,H,Y);checkFM55([H|T],[H2|_],Y).
checkFM55([H|T],[H2|_],Y) :-
      \+ phrase(fmFormat, H), \+ phrase(tntString,H) -> Y = ["exit 1"];checkFM66([H|T],[H2|_],Y).
checkFM66([H|T],[H2|_],Y) :-
      \+ phrase(fmError, H), \+ phrase(tntString, H) -> Y = ["exit 3"];checkFM88([H|T],[H2|_],Y).
% checkFM77([H|T],[H2|_],Y) :-
%       phrase(tntSting, H) -> Y = ["exit 1"];checkFM88([H|T],[H2|_],Y).
checkFM88([H|T],[H2|_],Y) :-
      phrase(mpString, H) -> Y = ["exit 1"];checkFM99([H|T],[H2|_],Y).
checkFM99([H|T],[H2|_],Y) :-
      phrase(tnpString, H) -> Y = ["exit 1"];checkFM100([H|T],[H2|_],Y).
checkFM100([H|T],[H2|_],Y) :-
    Y = ["exit 1"].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
checkTNT([H|T],[H2|_],Y) :-
  phrase(isSpaceLine, H) -> checkTNT(T,H,Y);checkTNT11([H|T],[H2|_],Y).
checkTNT11([H|T],[H2|_],Y) :-
    phrase(mpString, H), phrase(isSpaceLine, [H2]) -> Y = T;checkTNT22([H|T],[H2|_],Y).
checkTNT22([H|T],[H2|_],Y) :-
    phrase(tntString, H) -> Y = ["exit 1"];checkTNT44([H|T],[H2|_],Y).
% checkTNT33([H|T],[H2|_],Y) :-
%       phrase(isSpaceLine, H) -> Y = ["exit 1"];checkTNT44([H|T],[H2|_],Y).
checkTNT44([H|T],[H2|_],Y) :-
      phrase(tntFormat, H), phrase(tntError, H) -> checkTNT(T,H,Y);checkTNT55([H|T],[H2|_],Y).
checkTNT55([H|T],[H2|_],Y) :-
      \+ phrase(tntFormat, H), \+ phrase(mpString,H) -> Y = ["exit 1"];checkTNT66([H|T],[H2|_],Y).
checkTNT66([H|T],[H2|_],Y) :-
      \+ phrase(tntError, H), \+ phrase(mpString, H) -> Y = ["exit 3"];checkTNT99([H|T],[H2|_],Y).
% checkTNT77([H|T],[H2|_],Y) :-
%       phrase(tntSting, H) -> Y = ["exit 1"];checkTNT88([H|T],[H2|_],Y).
% checkTNT88([H|T],[H2|_],Y) :-
%       phrase(mpString, H) -> Y = ["exit 1"];checkTNT99([H|T],[H2|_],Y).
checkTNT99([H|T],[H2|_],Y) :-
      phrase(tnpString, H) -> Y = ["exit 1"];checkTNT100([H|T],[H2|_],Y).
checkTNT100([H|T],[H2|_],Y) :-
    Y = ["exit 1"].
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
    checkMP([H|T],Counter,[H2|_],Y) :-
      Counter > 8 , phrase(tnpString, H) -> Y = ["exit 4"];checkMP11([H|T],Counter,[H2|_],Y).
    checkMP11([H|T],Counter,[H2|_],Y) :-
      Counter < 8, phrase(tnpString, H)  -> Y = ["exit 4"];checkMP22([H|T],Counter,[H2|_],Y).
    checkMP22([H|T],Counter,[H2|_],Y) :-
        phrase(isSpaceLine, H) -> checkMP(T,Counter,H,Y);checkMP44([H|T],Counter,[H2|_],Y).
    checkMP33([H|T],Counter,[H2|_],Y) :-
       phrase(mpStartsWithWhite, H) -> Y = ["exit 1"];checkMP44([H|T],Counter,[H2|_],Y).
    checkMP44([H|T],Counter,[H2|_],Y) :-
        phrase(tnpString, H), phrase(isSpaceLine, [H2]), Counter == 8 -> Y = T;checkMP55([H|T],Counter,[H2|_],Y).
    checkMP55([H|T],Counter,[H2|_],Y) :-
        phrase(tnpString, H), phrase(isSpaceLine, [H2]), Counter \== 8 -> Y = ["exit4"];checkMP66([H|T],Counter,[H2|_],Y).
    checkMP66([H|T],Counter,[H2|_],Y) :-
        phrase(mpFormat, H) -> checkMP99([H|T],Counter,[H2|_],Y); Y = ["exit 4"].

    checkMP99([H|T],Counter,[H2|_],Y) :-
        phrase(mpError, H) -> Counter1 is Counter+1,checkMP(T,Counter1,H,Y);checkMP1111([H|T],Counter,[H2|_],Y).
    checkMP1111([H|T],Counter,[H2|_], Y) :-
        \+ phrase(mpError, H) ->  Y = ["exit 6"] ; checkMP100([H|T],Counter,[H2|_],Y).
    checkMP100([H|T],Counter,[H2|_],Y) :-
          phrase(tnpString, H) -> Y = ["exit 1"];checkMP110([H|T],Counter,[H2|_],Y).
    checkMP110([H|T],Couunter,[H2|_],Y) :-
        Y = ["exit 1"].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
checkTNP([],[],[]).
checkTNP([H|T],[H2|_],Y) :-
  phrase(isSpaceLine, H) -> checkTNP(T,H,Y);checkTNP22([H|T],[H2|_],Y).
% checkTNP11([H|T],[H2|_],Y) :-
%     (T == []  -> Y = T, checkTNP(Y,Y,Y4);checkTNP22([H|T],[H2|_],Y).
checkTNP22([H|T],[H2|_],Y) :-
    phrase(tnpString, H) -> Y = ["exit 1"];checkTNP44([H|T],[H2|_],Y).
% checkTNP33([H|T],[H2|_],Y) :-
%       phrase(isSpaceLine, H) -> Y = ["exit 1"];checkTNP44([H|T],[H2|_],Y).
checkTNP44([H|T],[H2|_],Y) :-
      phrase(tnpFormat, H), phrase(tnpError1, H),phrase(tnpError2, H) -> checkTNP(T,[],Y);checkTNP55([H|T],[H2|_],Y).
checkTNP55([H|T],[H2|_],Y) :-
      \+ phrase(tnpFormat, H) -> Y = ["exit 1"];checkTNP66([H|T],[H2|_],Y).
checkTNP66([H|T],[H2|_],Y) :-
      \+ phrase(tntError1, H)  -> Y = ["exit 5"];checkTNP99([H|T],[H2|_],Y).
checkTNP99([H|T],[H2|_],Y) :-
      \+ phrase(tntError2, H)  -> Y = ["exit 6"];checkTNP100([H|T],[H2|_],Y).
% checkTNP77([H|T],[H2|_],Y) :-
%       phrase(tntSting, H) -> Y = ["exit 1"];checkTNP88([H|T],[H2|_],Y).
% checkTNP88([H|T],[H2|_],Y) :-
%       phrase(mpString, H) -> Y = ["exit 1"];checkTNP99([H|T],[H2|_],Y).
checkTNP100([H|T],[H2|_],Y) :-
      phrase(tnpString, H) -> Y = ["exit 1"];checkTNP110([H|T],[H2|_],Y).
checkTNP110([H|T],[H2|_],Y) :-
    Y = ["exit 1"].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

checkName([H|T],Counter,[H2|_],Y) :-
  phrase(isSpaceLine, H) -> checkName(T,Counter,H,Y);checkName11([H|T],Counter,[H2|_],Y).
checkName11([H|T],Counter,[H2|_],Y) :-

  phrase(nameString, H) -> Counter1 is Counter + 1, checkName(T,Counter1,H,Y);checkName22([H|T],Counter,[H2|_],Y).
checkName22([H|T],Counter,[H2|_],Y) :-

  phrase(nameContents, H) -> Counter1 is Counter + 1, checkName(T,Counter1,H,Y);checkName33([H|T],Counter,[H2|_],Y).
checkName33([H|T],Counter,[H2|_],Y) :-
  write(H2),nl,write(Counter),nl,write(H),nl,
  phrase(fpaString,H), Counter == 2, phrase(isSpaceLine, [H2]) -> Y = T;checkName44([H|T],Counter,[H2|_],Y).
checkName44([H|T],Counter,[H2|_],Y) :-
 phrase(fpaString,H), Counter \== 2 -> Y = ["exit 1"];checkName55([H|T],Counter,[H2|_],Y),!.
checkName55([H|T],Counter,[H2|_],Y) :-
  \+ phrase(fpaString,H), Counter == 2 -> Y = ["exit 1"];checkName66([H|T],Counter,[H2|_],Y).
checkName66([H|T],Counter,[H2|_],Y) :-
  Y = ["exit 1"].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5










    printlist([]).

        printlist([X|List]) :-
            write(X),nl,
            printlist(List).

    split_on_delimiter(L, D, S) :-
        split_on_delimiter_(L, D, R),
        findall(X, (member(X, R), length(X,Length), Length > 0), S).

    split_on_delimiter_([], _, [[]]).
    split_on_delimiter_([D|T], D, [[]|T2]) :-
        split_on_delimiter_(T, D, T2).
    split_on_delimiter_([H|T], D, [[H|T2]|T3]) :-
        dif(H, D),
        split_on_delimiter_(T, D, [T2|T3]).

  storeToList(-1, []).
  storeToList(_,Y):-
    get0(Z),
    storeToList(Z,W),
    Y = [Z|W].


poop([]).
poop([H|T]) :-
  checkWhiteSpace(H),
  poop(T).

checkWhiteSpace([H|T]) :-
  not(char_type(H, space)) ->
    write('Fuck you');
    write('Cool beans').

    split_at(N, List, [H|[T]]) :- append(H, T, List), length(H, N).


    nameString   --> "Name:",isSpace,!.


    nameContents -->"!",nameContents;"\"",nameContents;"#",nameContents;"$",nameContents;"&",nameContents;"\'",nameContents;"(",nameContents;")",nameContents;"*",nameContents;"+",nameContents;",",nameContents;
    "-",nameContents;".",nameContents;"/",nameContents;"0",nameContents;"1",nameContents;"2",nameContents;"3",nameContents;"4",nameContents;"5",nameContents;"6",nameContents;"7",nameContents;"8",nameContents;
    "9",nameContents;":",nameContents;";",nameContents;"<",nameContents;"=",nameContents;">",nameContents;"?",nameContents;"@",nameContents;"A",nameContents;"B",nameContents;"C",nameContents;"D",nameContents;
    "E",nameContents;"F",nameContents;"G",nameContents;"H",nameContents;"I",nameContents;"J",nameContents;"K",nameContents;"L",nameContents;"M",nameContents;"N",nameContents;"O",nameContents;"P",nameContents;
    "Q",nameContents;"R",nameContents;"S",nameContents;"T",nameContents;"U",nameContents;"V",nameContents;"W",nameContents;"X",nameContents;"Y",nameContents;"Z",nameContents;"[",nameContents;"\\",nameContents;
    "]",nameContents;"^",nameContents;"_",nameContents;"`",nameContents;"a",nameContents;"b",nameContents;"c",nameContents;"d",nameContents;"e",nameContents;"f",nameContents;"g",nameContents;"h",nameContents;
    "i",nameContents;"j",nameContents;"k",nameContents;"l",nameContents;"m",nameContents;"n",nameContents;"o",nameContents;"p",nameContents;"q",nameContents;"r",nameContents;"s",nameContents;"t",nameContents;
    "u",nameContents;"v",nameContents;"w",nameContents;"x",nameContents;"y",nameContents;"z",nameContents;"{",nameContents;"|",nameContents;"}",nameContents;"~",nameContents;isSpace,!.
    nameContents --> [].


    anything --> "!",anything;"\"",anything;"#",anything;"$",anything;"&",anything;"\'",anything;"(",anything;")",anything;"*",anything;"+",anything;
    "-",anything;".",anything;"/",anything;"0",anything;"1",anything;"2",anything;"3",anything;"4",anything;"5",anything;"6",anything;"7",anything;"8",anything;
    "9",anything;":",anything;";",anything;"<",anything;"=",anything;">",anything;"?",anything;"@",anything;"A",anything;"B",anything;"C",anything;"D",anything;
    "E",anything;"F",anything;"G",anything;"H",anything;"I",anything;"J",anything;"K",anything;"L",anything;"M",anything;"N",anything;"O",anything;"P",anything;
    "Q",anything;"R",anything;"S",anything;"T",anything;"U",anything;"V",anything;"W",anything;"X",anything;"Y",anything;"Z",anything;"[",anything;"\\",anything;
    "]",anything;"^",anything;"_",anything;"`",anything;"a",anything;"b",anything;"c",anything;"d",anything;"e",anything;"f",anything;"g",anything;"h",anything;
    "i",anything;"j",anything;"k",anything;"l",anything;"m",anything;"n",anything;"o",anything;"p",anything;"q",anything;"r",anything;"s",anything;"t",anything;
    "u",anything;"v",anything;"w",anything;"x",anything;"y",anything;"z",anything;"{",anything;"|",anything;"}",anything;"~",anything;",".
    anything --> [].

    anything2 --> "!",anything2;"\"",anything2;"#",anything2;"$",anything2;"&",anything2;"\'",anything2;"(",anything2;")",anything2;"*",anything2;"+";",",anything2;
    "-",anything2;".",anything2;"/",anything2;"0",anything2;"1",anything2;"2",anything2;"3",anything2;"4",anything2;"5",anything2;"6",anything2;"7",anything2;"8",anything2;
    "9",anything2;":",anything2;";",anything2;"<",anything2;"=",anything2;">",anything2;"?",anything2;"@",anything2;"A",anything2;"B",anything2;"C",anything2;"D",anything2;
    "E",anything2;"F",anything2;"G",anything2;"H",anything2;"I",anything2;"J",anything2;"K",anything2;"L",anything2;"M",anything2;"N",anything2;"O",anything2;"P",anything2;
    "Q",anything2;"R",anything2;"S",anything2;"T",anything2;"U",anything2;"V",anything2;"W",anything2;"X",anything2;"Y",anything2;"Z",anything2;"[",anything2;"\\",anything2;
    "]",anything2;"^",anything2;"_",anything2;"`",anything2;"a",anything2;"b",anything2;"c",anything2;"d",anything2;"e",anything2;"f",anything2;"g",anything2;"h",anything2;
    "i",anything2;"j",anything2;"k",anything2;"l",anything2;"m",anything2;"n",anything2;"o",anything2;"p",anything2;"q",anything2;"r",anything2;"s",anything2;"t",anything2;
    "u",anything2;"v",anything2;"w",anything2;"x",anything2;"y",anything2;"z",anything2;"{",anything2;"|",anything2;"}",anything2;"~",anything2.
    anything2 --> [].

    anything3 --> "!",anything3;"\"",anything3;"#",anything3;"$",anything3;"&",anything3;"\'",anything3;"(",anything3;")",anything3;"*",anything3;"+",anything3;",",anything2;
    "-",anything3;".",anything3;"/",anything3;"0",anything3;"1",anything3;"2",anything3;"3",anything3;"4",anything3;"5",anything3;"6",anything3;"7",anything3;"8",anything3;
    "9",anything3;":",anything3;";",anything3;"<",anything3;"=",anything3;">",anything3;"?",anything3;"@",anything3;"A",anything3;"B",anything3;"C",anything3;"D",anything3;
    "E",anything3;"F",anything3;"G",anything3;"H",anything3;"I",anything3;"J",anything3;"K",anything3;"L",anything3;"M",anything3;"N",anything3;"O",anything3;"P",anything3;
    "Q",anything3;"R",anything3;"S",anything3;"T",anything3;"U",anything3;"V",anything3;"W",anything3;"X",anything3;"Y",anything3;"Z",anything3;"[",anything3;"\\",anything3;
    "]",anything3;"^",anything3;"_",anything3;"`",anything3;"a",anything3;"b",anything3;"c",anything3;"d",anything3;"e",anything3;"f",anything3;"g",anything3;"h",anything3;
    "i",anything3;"j",anything3;"k",anything3;"l",anything3;"m",anything3;"n",anything3;"o",anything3;"p",anything3;"q",anything3;"r",anything3;"s",anything3;"t",anything3;
    "u",anything3;"v",anything3;"w",anything3;"x",anything3;"y",anything3;"z",anything3;"{",anything3;"|",anything3;"}",anything3;"~",anything3; "\r".
    anything3 --> [].


    fpaString    --> "forced partial assignment:", isSpace,!.
    fmString     --> "forbidden machine:", isSpace, !.
    tntString    --> "too-near tasks:", isSpace,!.
    mpString     --> "machine penalties:", isSpace,!.
    tnpString    --> "too-near penalities", isSpace,!.

    fpaFormat    --> "(",anything,",",anything,")", isSpace,!.
    fmFormat     --> "(",anything,",",anything,")", isSpace,!.
    tntFormat    --> "(",anything,",",anything,")", isSpace,!.

    mpFormat     --> anything2, " ",anything2, " ",anything2, " ",anything2, " ",anything2, " ",anything2, " ",anything2, " ",anything2, isSpace,!.
    mpStartsWithWhite --> " ", anything3,"\r",!.

    tnpFormat    --> "(",anything,",",anything,",",anything,")", isSpace,!.

    mpError      --> isValidPenalty, " ",isValidPenalty, " ",isValidPenalty, " ",isValidPenalty, " ",isValidPenalty, " ",isValidPenalty, " ",isValidPenalty, " ",isValidPenalty,isSpace,!.
    isValidPenalty --> "0",isValidPenalty;"1",isValidPenalty;"2",isValidPenalty;"3",isValidPenalty;"4",isValidPenalty;"5",isValidPenalty;"6",isValidPenalty;"7",isValidPenalty;"8",isValidPenalty;"9",isValidPenalty;" ".
    isValidPenalty --> [].
    fpaError     --> "(",isValidMachine,",",isValidTask,")", isSpace,!.
    fmError      --> "(",isValidMachine,",",isValidTask,")", isSpace,!.
    tntError     --> "(",isValidTask,",",isValidTask,")", isSpace,!.

    tnpError1    --> "(",isValidTask,",",isValidTask,",",anything,")", isSpace,!.
    isValidTask  --> "A";"B";"C";"D";"E";"F","H".

    tnpError2    --> "(",anything,",",anything,",",isValidPenalty,")", isSpace,!.
    isValidMachine --> "1";"2";"3";"4";"5";"6";"7";"8".

    isSpace  -->  " ",isSpace;"\t", isSpace;"\r", isSpace.
    isSpace  -->  [].

    isSpaceLine  --> " ",isSpace;"\t", isSpace;"\r", isSpace,!.
    isSpaceLine  --> [].

:- use_module(library(pio)).

    lines([])           --> call(eos), !.
    lines([Line|Lines]) --> line(Line), lines(Lines).

    eos([], []).

    line([])     --> ( "\n" ; call(eos) ), !.
    line([L|Ls]) --> [L], line(Ls).
