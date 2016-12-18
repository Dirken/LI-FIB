:-include(entradaHoraris).
:-include(displaySol).
:-dynamic(varNumber/3).
symbolicOutput(0). % set to 1 to see symbolic output only; 0 otherwise.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% We want to compute the weekly schedule of a university. Classes should
% be allocated from 8h to 20h (12 hours per day) from Monday to
% Friday. File entradaHoraris.pl contains all necessary input data.

%  - there is a certain number of years, courses, rooms and teachers.
%  - each course belongs to a certain year, has a certain number of
%     1-hour lectures per week (at most 1 lecture per day), can be taught
%     only at some rooms and only by some teachers.
%  - the courses of a year can only be taught in the morning (8-14)
%     or in the afternoon (14-20).
%  - every teacher expresses his availability to teach (morning, afternoon or both).

% There is an additional set of constraints to be considered:

%  - all lectures of a course are taught by the same teacher and at the same room 
%  - it is not possible to hold two lectures at the same room simultaneously,
%    and similarly, a teacher cannot teach two lectures simultaneously
%  - at most 5 lectures of a given year can be taught every day
%  - two lectures belonging to the same year cannot be taught at the same time
%
%  File displaySol.pl contains the predicate drawSchedule, which will output
%  the resulting schedule in a readable format. If your code is incorrect and
%  produces a wrong schedule, its output is unpredictable.

%%%%%% Some helpful definitions to make the code cleaner:
day(D)               :- between(1,5,D).
hour(H)              :- between(1,12,H).
year(Y)              :- numYears(N), between(1,N,Y).
course(C)            :- numCourses(N), between(1,N,C).
teacher(T)           :- numTeachers(N), between(1,N,T).
room(R)              :- numRooms(N), between(1,N,R).
lectureOfCourse(C,L) :- course(_,C,N,_,_), between(1,N,L). % given C, computes L
courseOfYear(Y,C)    :- course(Y,C,_,_,_). % given Y, computes C

slot(S)                 :- between(1,60,S). % Monday slots [1,..,12], Tuesday [13,..24]
slotOfDay(D,S)          :- hour(H), S is (D-1)*12 + H. % given D, computes S
morningSlotOfDay(D,S)   :- between(1,6,H),  S is (D-1)*12 + H. % given D, computes S
afternoonSlotOfDay(D,S) :- between(7,12,H), S is (D-1)*12 + H. % given D, computes S

%%%%%%  Variables: It is mandatory to use AT LEAST these variables!
% ct-C-T    : course is taught by teacher T
% cr-C-R    : course C is taught at room R
% cls-C-L-S : lecture L of course C is given at slot S (slots between 1 and 60)

concat([],L,L).
concat([X|L1],L2,[X|L3]) :- concat(L1,L2,L3). 

writeClauses:- 
    oneSlotPerLecture,
    atMost1LecturePerDay,
    morningRestriction,
    afternoonRestriction,
    oneRoomAtOnce,
    teacherEverywhere,
    teacherMorning,
    teacherAfternoon,
    notTwoLecturesSameRoom,
    notTwoLecturesSameTeacher,
    defineCS,
    defineYS,
    atMost5,
    sameYearSameTime,
    true.


%one lecture for each Slot
oneSlotPerLecture:-
    course(C),
    lectureOfCourse(C,L),
    findall(cls-C-L-S,slot(S),Res),
    exactly(1,Res),
    fail.
oneSlotPerLecture.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%MyCode%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Ricard Meyerhofer Parra, 2016-2017Q1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Definitions of variables that we used in our program:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

defineCS:-
    course(C),
    slot(S),
    findall(cls-C-L-S, lectureOfCourse(C,L), Res),
    expressOr(cs-C-S, Res),
    fail.
defineCS.

% ys-Y-S
defineYS:-
    year(Y,Time), 
    slot(S,Time),
    findall(cls-C-L-S,(courseOfYear(Y,C),lectureOfCourse(C,L)),Res),
    expressOr(ys-Y-S,Res),
    fail.
defineYS.

morningSlot(S)          :- slot(S), S12 is (S - 1) mod 12, between(0,5,S12).
afternoonSlot(S)        :- slot(S), S12 is (S - 1) mod 12, between(6,11,S12).
slot(S,morning)         :- morningSlot(S).
slot(S,afternoon)       :- afternoonSlot(S).
slotOfDay(D,S,morning)  :- morningSlotOfDay(D,S).
slotOfDay(D,S,afternoon):- afternoonSlotOfDay(D,S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% (at most 1 lecture per day)
atMost1LecturePerDay:-
    course(C),
    day(D),
    findall(cls-C-L-S, (slotOfDay(D,S), lectureOfCourse(C,L)), Res),
    atMost(1,Res),
    fail.
atMost1LecturePerDay.

%  - the courses of a year can only be taught in the morning (8-14)
morningRestriction:-
    year(Y,afternoon),
    courseOfYear(Y,C),
    lectureOfCourse(C,L), 
    day(D),
    findall(cls-C-L-S, morningSlotOfDay(D,S) ,Res),
    exactly(0,Res),
    fail.
morningRestriction.

%     or in the afternoon (14-20).
afternoonRestriction:-
    year(Y,morning),
    courseOfYear(Y,C),
    lectureOfCourse(C,L),
    day(D),
    findall(cls-C-L-S, afternoonSlotOfDay(D,S) ,Res),
    exactly(0,Res),
    fail.
afternoonRestriction.

%  all lectures of a course are taught by the same teacher 
teacherEverywhere:-
    course(_,C,_,_,Teachers),
    findall(ct-C-T, member(T,Teachers), Res),
    exactly(1,Res),
    fail.
teacherEverywhere.

%  all lectures of a course are at the same room 
oneRoomAtOnce:-
    course(_,C,_,Rooms,_),
    findall(cr-C-R, member(R,Rooms), Res),
    exactly(1,Res),
    fail.
oneRoomAtOnce.

%  every teacher expresses his availability to teach (morning, afternoon or both).
teacherMorning:-
    teacher(T,morning),
    year(Y,afternoon),
    findall(ct-C-T, courseOfYear(Y,C),Res),
    exactly(0,Res),
    fail.
teacherMorning.

teacherAfternoon:-
    teacher(T,afternoon),
    year(Y,morning),
    findall(ct-C-T, courseOfYear(Y,C),Res),
    exactly(0,Res),
    fail.
teacherAfternoon.

notTwoLecturesSameRoom:-
    room(R),
    slot(S),
    course(_,C1,_,R1,_),
    course(_,C2,_,R2,_),
    C1 \= C2,
    member(R,R2),
    member(R,R1),
    atMost(3, [cs-C1-S,cs-C2-S,cr-C1-R, cr-C2-R]),
    fail.
notTwoLecturesSameRoom.

notTwoLecturesSameTeacher:-
    teacher(T),
    slot(S),
    course(_,C1,_,_,T1),
    course(_,C2,_,_,T2),
    C1 \= C2,
    member(T,T1),
    member(T,T2),
    atMost(3, [cs-C1-S,cs-C2-S,ct-C1-T, ct-C2-T]),
    fail.
notTwoLecturesSameTeacher.

%  - at most 5 lectures of a given year can be taught every day
atMost5:-
    year(Y,Time), 
    day(D),
    findall(ys-Y-S,slotOfDay(D,S,Time),Res),
    negateAll(Res,R2), 
    atLeast(1,R2),
    fail.
atMost5.

%  - two lectures belonging to the same year cannot be taught at the same time
sameYearSameTime:-
    year(Y),
    day(D),
    slotOfDay(D,S),
    findall(cls-C-L-S, (courseOfYear(Y,C),lectureOfCourse(C,L)), Lits),
    atMost(1,Lits),
    fail.
sameYearSameTime.


%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% show the solution. Here M contains the literals that are true in the model:

displaySol(M):-write(M),nl,fail.
displaySol(M):-displaySchedule(M),fail.
displaySol(_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Express that Var is equivalent to the disjunction of Lits:
expressOr( Var, Lits ):- member(Lit,Lits), negate(Lit,NLit), writeClause([ NLit, Var ]), fail.
expressOr( Var, Lits ):- negate(Var,NVar), writeClause([ NVar | Lits ]),!.


%%%%%% Cardinality constraints on arbitrary sets of literals Lits:

exactly(K,Lits):- atLeast(K,Lits), atMost(K,Lits),!.

atMost(K,Lits):-   % l1+...+ln <= k:  in all subsets of size k+1, at least one is false:
    negateAll(Lits,NLits), 
    K1 is K+1,    subsetOfSize(K1,NLits,Clause), writeClause(Clause),fail.
atMost(_,_).

atLeast(K,Lits):-  % l1+...+ln >= k: in all subsets of size n-k+1, at least one is true:
    length(Lits,N),
    K1 is N-K+1,  subsetOfSize(K1, Lits,Clause), writeClause(Clause),fail.
atLeast(_,_).

negateAll( [], [] ).
negateAll( [Lit|Lits], [NLit|NLits] ):- negate(Lit,NLit), negateAll( Lits, NLits ),!.

negate(\+Lit,  Lit):-!.
negate(  Lit,\+Lit):-!.

subsetOfSize(0,_,[]):-!.
subsetOfSize(N,[X|L],[X|S]):- N1 is N-1, length(L,Leng), Leng>=N1, subsetOfSize(N1,L,S).
subsetOfSize(N,[_|L],   S ):-            length(L,Leng), Leng>=N,  subsetOfSize( N,L,S).


%%%%%% main:

main:-  symbolicOutput(1), !, writeClauses, halt.   % print the clauses in symbolic form and halt
main:-  initClauseGeneration,
        tell(clauses), writeClauses, told,          % generate the (numeric) SAT clauses and call the solver
    tell(header),  writeHeader,  told,
    numVars(N), numClauses(C),
    write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
    shell('cat header clauses > infile.cnf',_),
    write('Calling solver....'), nl, 
    shell('picosat -v -o model infile.cnf', Result),  % if sat: Result=10; if unsat: Result=20.
    treatResult(Result),!.

treatResult(20):- write('Unsatisfiable'), nl, halt.
treatResult(10):- write('Solution found: '), nl, see(model), symbolicModel(M), seen, displaySol(M), nl,nl,halt.

initClauseGeneration:-  %initialize all info about variables and clauses:
    retractall(numClauses(   _)), 
    retractall(numVars(      _)), 
    retractall(varNumber(_,_,_)),
    assert(numClauses( 0 )), 
    assert(numVars(    0 )),     !.

writeClause([]):- symbolicOutput(1),!, nl.
writeClause([]):- countClause, write(0), nl.
writeClause([Lit|C]):- w(Lit), writeClause(C),!.
w( Lit ):- symbolicOutput(1), write(Lit), write(' '),!.
w(\+Var):- var2num(Var,N), write(-), write(N), write(' '),!.
w(  Var):- var2num(Var,N),           write(N), write(' '),!.


% given the symbolic variable V, find its variable number N in the SAT solver:
var2num(V,N):- hash_term(V,Key), existsOrCreate(V,Key,N),!.
existsOrCreate(V,Key,N):- varNumber(Key,V,N),!.                            % V already existed with num N
existsOrCreate(V,Key,N):- newVarNumber(N), assert(varNumber(Key,V,N)), !.  % otherwise, introduce new N for V

writeHeader:- numVars(N),numClauses(C), write('p cnf '),write(N), write(' '),write(C),nl.

countClause:-     retract( numClauses(N0) ), N is N0+1, assert( numClauses(N) ),!.
newVarNumber(N):- retract( numVars(   N0) ), N is N0+1, assert(    numVars(N) ),!.
 
% Getting the symbolic model M from the output file:
symbolicModel(M):- get_code(Char), readWord(Char,W), symbolicModel(M1), addIfPositiveInt(W,M1,M),!.
symbolicModel([]).
addIfPositiveInt(W,L,[Var|L]):- W = [C|_], between(48,57,C), number_codes(N,W), N>0, varNumber(_,Var,N),!.
addIfPositiveInt(_,L,L).
readWord( 99,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ c
readWord(115,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ s
readWord(-1,_):-!, fail. %end of file
readWord(C,[]):- member(C,[10,32]), !. % newline or white space marks end of word
readWord(Char,[Char|W]):- get_code(Char1), readWord(Char1,W), !.
%========================================================================================