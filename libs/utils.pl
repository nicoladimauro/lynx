/***************************************************************************************************
  Lynx

	Copyright 2010-11 University of Bari "Aldo Moro"
	                                                                          
  Author: Nicola Di Mauro                                                 

  $Date: 2011-07-11 13:28:57 +0200 (lun, 11 lug 2011) $

 ***************************************************************************************************

	The Lynx Software is made available under the terms and conditions of the Artistic License 2.0.
  LICENSEE shall acknowledge University of Bari "Aldo Moro" as the provider of the Software.

 ***************************************************************************************************/

:- module(utils,[
		divlist/3,
		var2consts/2,
		alldiff/1,
		mullists/3,
		make_numeric_list/3,
		remove_last/2,
		zeros/2
	]).

:- yap_flag(unknown,fail).
:- yap_flag(discontiguous_warnings,on).
:- yap_flag(single_var_warnings,on).

:-use_module(library(lists)).
:-use_module(library(terms)).


/***************************************************************************
 ***************************************************************************/
remove_last([_X],[]):-
	!.
remove_last([X,Y|R],[X|R1]):-
	remove_last([Y|R],R1).

/***************************************************************************
 ***************************************************************************/
divlist([],_,[]).
divlist([X|R],D,[X1|R1]):-
	divlist(R,D,R1),
	X1 is X / D.

/***************************************************************************
 ***************************************************************************/
mullists([],[],[]).
mullists([X|R],[X1|R1],[X2|R2]):-
	X2 is X * X1,
	mullists(R,R1,R2).


/***************************************************************************
 ***************************************************************************/
sumlists([],[],[],[]).
sumlists([X1|R1],[X2|R2],[X3|R3],[X4|R4]):-
	X4 is X1 + X2 + X3,
	sumlists(R1,R2,R3,R4).

/***************************************************************************
 ***************************************************************************/
sumlists([],[],[]).
sumlists([X1|R1],[X2|R2],[X4|R4]):-
	X4 is X1 + X2,
	sumlists(R1,R2,R4).

/***************************************************************************
 ***************************************************************************/
var2consts(List1,List11) :-
	copy_term(List1,List11),
	term_variables(List11,Vars),
	length(Vars,NumVars),
	length(Consts,NumVars),
	consts(Consts,1,Consts1),
	Vars = Consts1.

/***************************************************************************
 ***************************************************************************/
consts([],_,[]).
consts([_X|R],N,[N|R1]) :-
	N1 is N + 1,
	consts(R,N1,R1).

/***************************************************************************
 ***************************************************************************/
ndm_variant(Lits,Lits1):-
	length(Lits,N),
	length(Lits1,N),
	copy_term(Lits,LitsN),
	var2consts(Lits1,Lits1N),
	ndm_variant1(LitsN,Lits1N),
	!.

ndm_variant1([],[]).
ndm_variant1([Elem|Elems],S):-
	my_rest(S,Elem,S1),
	ndm_variant1(Elems,S1).

my_rest([Elem|R],Elem,R).
my_rest([ElemP|R],Elem,[ElemP|R1]) :-
	my_rest(R,Elem,R1).

/***************************************************************************
 ***************************************************************************/
remove_variants([],[]).
remove_variants([X|R],R1):-
	remove_variants1(R,X),!,
	remove_variants(R,R1).
remove_variants([X|R],[X|R1]):-
	remove_variants2(R,X,R2),
	remove_variants(R2,R1).

remove_variants1([[X,_]|_R],[Z,_]) :-
	ndm_variant(X,Z),
	!.
remove_variants1([_|R],Z) :-
	remove_variants1(R,Z).

remove_variants2([],_,[]).
remove_variants2([[Z,Iz]|R],[X,_],[[Z,Iz]|R1]) :-
	\+ ndm_variant(X,Z),!,
	remove_variants2(R,[X,_],R1).
remove_variants2([_|R],[X,_],Z) :-
	remove_variants2(R,[X,_],Z).


m_remove_variants([],[]).
m_remove_variants([X|R],[X|R1]):-
	\+ m_variant(X),!,
	m_remove_variants(R,R1).
m_remove_variants([_X|R],R1):-
	m_remove_variants(R,R1).

m_variant(X) :-
	'$pattern'(Pattern,_),
	ndm_variant(Pattern,X),
	!.

/***************************************************************************
 ***************************************************************************/
alldiff([]).
alldiff([X|R]):-
	someequal(X,R),
	!,
	fail.
alldiff([_X|R]):-
	alldiff(R).

/***************************************************************************
 ***************************************************************************/
someequal(X,[Y|_R]) :-
	X == Y,
	!.
someequal(X,[_|R]) :-
	someequal(X,R).


/***************************************************************************
 ***************************************************************************/
zeros(0,[]) :-
	!.
zeros(N,[0|R]) :-
	N1 is N - 1,
	zeros(N1,R).

/***************************************************************************
 ***************************************************************************/
make_numeric_list(Min,Max,[]):-
	Min > Max,
	!.
make_numeric_list(Min,Max,[Min|Rest]):-
	Min1 is Min + 1,
	make_numeric_list(Min1,Max,Rest).
