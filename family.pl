% facts
childOf(john, sue).
childOf(john, sam).

childOf(jane, sue).
childOf(jane, sam).

childOf(sue, george).
childOf(sue, gina).

female(jane).
female(june).
female(sue).

male(john).
male(sam).
male(george).

% relations
parent(X, Y) :- childOf(Y, X).

father(X, Y) :- childOf(Y, X), male(X).
