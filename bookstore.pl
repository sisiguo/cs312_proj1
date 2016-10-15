%%%
%%% Prolog program for a bookstore
%%%

% noun_phrase(T0,T4,Ind,C0,C4) is true if
%  T0 and T4 are list of words, such that
%        T4 is an ending of T0
%        the words in T0 before T4 (written T0-T4) form a noun phrase
%  Ind is the individual that the noun phrase is referring to
%  C0 and C4 are lists of relations such that
%        C0 is an ending of C4 and
%        the relations in C4-C0 give the constraints on Ind implied by the noun phrase
% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional modifying phrase:
noun_phrase(T0,T4,Ind,C0,C4) :-
    det(T0,T1,Ind,C0,C1),
    adjectives(T1,T2,Ind,C1,C2),
    noun(T2,T3,Ind,C2,C3),
    mp(T3,T4,Ind,C3,C4).

% Determiners (articles) are ignored in this oversimplified example.
% They do not provide any extra constaints.
det([the | T],T,_,C,C).
det([a | T],T,_,C,C).
det([an | T],T,_,C,C).
det(T,T,_,C,C).

% Adjectives consist of a sequence of adjectives.
% The meaning of the arguments is the same as for noun_phrase
adjectives(T0,T2,Ind,C0,C2) :-
    adj(T0,T1,Ind,C0,C1),
    adjectives(T1,T2,Ind,C1,C2).
adjectives(T,T,_,C,C).

% An optional modifying phrase / relative clause is either
% a relation (verb or preposition) followed by a noun_phrase or
% 'that' followed by a relation then a noun_phrase or
% nothing 
mp(T0,T2,I1,C0,C2) :-
    reln(T0,T1,I1,I2,C0,C1),
    noun_phrase(T1,T2,I2,C1,C2).
mp([that|T0],T2,I1,C0,C2) :-
    reln(T0,T1,I1,I2,C0,C1),
    noun_phrase(T1,T2,I2,C1,C2).
mp(T,T,_,C,C).


%% 
%% Dictionary
%% 

% noun(T0,T1,Ind,C0,C1) is true if T0-T1 is a noun that provides properties C1-C0 to Ind
noun([author | T],T,Ind,C,[author(Ind)|C]).
noun([book | T],T,Ind,C,[book(Ind)|C]).
noun([publisher | T],T,Ind,C,[publisher(Ind)|C]).
% The following are for proper nouns:
noun([Ind | T],T,Ind,C,C) :- author(Ind).
noun([Ind | T],T,Ind,C,C) :- book(Ind).
noun([Ind | T],T,Ind,C,C) :- publisher(Ind).

% adj(T0,T1,Ind,C0,C1) is true if T0-T1 is an adjective that provides properties C1-C0 to Ind
adj([fiction | T],T,Ind,C,[fiction(Ind)|C]).
adj([historical | T],T,Ind,C,[historical(Ind)|C]).
adj([cost | T],T,Ind,C,[price(Ind,_)|C]).
adj([long | T],T,Ind,C,[num_pages(Ind,_)|C]).
adj([more,than | T],[_,pages | T],Ind,C,[num_pages(Ind,_)|C]).
adj([less,than | T],[_,pages | T],Ind,C,[num_pages(Ind,_)|C]).
adj([more,than | T],[_,dollars | T],Ind,C,[price(Ind,_)|C]).
adj([less,than | T],[_,dollars | T],Ind,C,[price(Ind,_)|C]).

% reln(T0,T1,I1,I2,R0,R1) is true if T0-T1 is a relation
%   that provides relations R1-R0 on individuals I1 and I2
reln([wrote | T],T,I1,I2,C,[wrote(I1,I2)|C]).
reln([by | T],T,I1,I2,C,[wrote(I1,I2)|C]).
reln([published | T],T,I1,I2,C,[published(I1,I2)|C]).
reln([costs | T],T,I1,I2,C,[price(I1,I2)|C]).
reln([author,of | T],T,I1,I2,C,[wrote(I1,I2)|C]).
reln([publisher,of | T],T,I1,I2,C,[published(I1,I2)|C]).
reln([published, by | T],T,I1,I2,C,[published(I1,I2)|C]).

% question(Question,QR,Indect,Q0,Query) is true if Query-Q0 provides an answer about Indect to Question-QR
question([what,is | T0],T1,Ind,C0,C1) :-
    mp(T0,T1,Ind,C0,C1).
question([what,is | T0],T1,Ind,C0,C1) :-
    noun_phrase(T0,T1,Ind,C0,C1).
question([what,is| T0],T2,Obj,C0,C2) :-
    noun_phrase(T0,T1,Obj,C0,C1),
    mp(T1,T2,Obj,C1,C2).
question([what,is| T0],T2,Obj,C0,C2) :-
    mp(T0,T1,Obj,C0,C1),
    noun_phrase(T1,T2,Obj,C1,C2).
question([is | T0],T2,Ind,C0,C2) :-
    noun_phrase(T0,T1,Ind,C0,C1),
    mp(T1,T2,Ind,C1,C2).
question([who,is | T0],T1,Ind,C0,C1) :-
    mp(T0,T1,Ind,C0,C1).
question([who,is | T0],T1,Ind,C0,C1) :-
    noun_phrase(T0,T1,Ind,C0,C1).
question([who,is | T0],T1,Ind,C0,C1) :-
    adjectives(T0,T1,Ind,C0,C1).
question([what | T0],T2,Ind,C0,C2) :-      % allows for a "what ... is ..."
    noun_phrase(T0,[is|T1],Ind,C0,C1),
    mp(T1,T2,Ind,C1,C2).
question([what | T0],T2,Ind,C0,C2) :-
    noun_phrase(T0,T1,Ind,C0,C1),
    mp(T1,T2,Ind,C1,C2).

% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
    question(Q,[],A,[],C),
    prove_all(C).

% prove_all(L) proves all elements of L against the database
prove_all([]).
prove_all([H|T]) :-
     H,
    prove_all(T).


%% 
%% The Database of facts to be queried
%% 

%
% Individuals (noun)
%

% author([F,L]) is true if F is a first name and L is a last name of an author
author(F) :- author_full_name(F,L).
author_full_name(emma,donoghue).

% book(T) is true if T is a list representing the title of a book
book([the,wonder]).

% publisher(P) is true if P is a publisher
publisher(harperCollins).


%
% Relations (btw/ 2 individuals)
%

% wrote(A,B) is true if author A wrote book B
wrote(author([emma,donoghue]), book([the,wonder])).

% published(P,B) is true if publisher P published book B
published(publisher(harperCollins), book([the,wonder])).


%
% Adjectives (adjective)
%

% num_pages(B,N) is true if book B has N number of pages
num_pages(book([the,wonder]),304).

% price(B,N) is true if book B costs N dollars
price(book([the,wonder]),33).

% more_than_dollars(B,N) is true if book B costs more than N dollars
more_than_dollars(B,N) :- price(B,C), C > N.

% more_than_pages(B,N) is true if book B has more than N pages
more_than_pages(B,N) :- num_pages(B,L), L > N.

% less_than_dollars(B,N) is true if book B costs less than N dollars
less_than_dollars(B,N) :- price(B,C), C < N.

% less_than_pages(B,N) is true if book B has less than N pages
less_than_pages(B,N) :- num_pages(B,L), L < N.


%
% Categories (adjective)
%

% fiction(B) is true if book B is a fiction book
fiction(book([the,wonder])).

% non_fiction(B) is true if book B is a non-fiction book
% TODO

% historical(B) is true if book B is a historical book
historical(book([the,wonder])).

