/***************************************************************************************************
  Lynx

	Copyright 2010-11 University of Bari "Aldo Moro"
	                                                                          
  Author: Nicola Di Mauro                                                 

  $Date: 2011-10-26 12:59:32 +0200 (mer, 26 ott 2011) $

 ***************************************************************************************************

	The Lynx Software is made available under the terms and conditions of the Artistic License 2.0.
  LICENSEE shall acknowledge University of Bari "Aldo Moro" as the provider of the Software.

 ***************************************************************************************************/

:- module(confmatrix, [
    write_confusion_matrix/2,
    build_confusion_matrix/1,
    add_to_confusion_matrix/4
]).

:- yap_flag(unknown,fail).
:- yap_flag(discontiguous_warnings,on).
:- yap_flag(single_var_warnings,on).

:- use_module(library(lists)).

:- use_module(flags).

write_confusion_matrix(Classes,Examples):-
	trunkated_names(Classes,Classes1),
	format("~10+",[]),
	write_classes(Classes1),
	format("~n",[]),
	write_confusion_matrix1(Classes1,Examples).

trunkated_names([],[]).
trunkated_names([Class|RestClass],[Class1|RestClass1]):-
	atom_length(Class,L),
	L > 9,
	!,
	sub_atom(Class,_,9,_,Class1),
  trunkated_names(RestClass,RestClass1).
trunkated_names([Class|RestClass],[Class|RestClass1]):-
  trunkated_names(RestClass,RestClass1).


write_classes([]).
write_classes([Class|Rest]):-
	format("~t~w~t~10+",[Class]),
	write_classes(Rest).

/***************************************************************************
 ***************************************************************************/
write_confusion_matrix1([],[]).
write_confusion_matrix1([C|RC],[X|R]):-
	format("~t~w~t~10+",[C]),
	write_confusion_matrix2(X),
	format("~n",[]),
	write_confusion_matrix1(RC,R).

write_confusion_matrix2([]).
write_confusion_matrix2([C|R]):-
	format("~t~w~t~10+",[C]),
	write_confusion_matrix2(R).

/***************************************************************************
 ***************************************************************************/
build_confusion_matrix(InitialCM):-
	get_lynx_flag('$classes',Classes),
	length(Classes,L),
	build_confusion_matrix(L,L,InitialCM).
build_confusion_matrix(0,_,[]):-
	!.
build_confusion_matrix(N,M,[L|R]):-
	build_confusion_matrix1(M,L),
	N1 is N - 1,
	build_confusion_matrix(N1,M,R).
build_confusion_matrix1(0,[]):-
	!.
build_confusion_matrix1(N,[0|R]):-
	N1 is N - 1,
	build_confusion_matrix1(N1,R).

/***************************************************************************
 ***************************************************************************/
add_to_confusion_matrix(Class1,Class2,CM,CM1):-
	get_lynx_flag('$classes',Classes),
	nth(N1,Classes,Class1),
	nth(N2,Classes,Class2),
	nth(N1,CM,L1),
	nth(N2,L1,X),
	X1 is X + 1,
	nsubstitute(L1,N2,X1,L2),
	nsubstitute(CM,N1,L2,CM1),!.
add_to_confusion_matrix(_,_,A,A).

nsubstitute(L1,P,E,L2):-
	nsubstitute(L1,1,P,E,L2).
nsubstitute([_|R],N,N,E,[E|R]):-
	!.
nsubstitute([X|R],N,P,E,[X|R1]):-
	N1 is N + 1,
	nsubstitute(R,N1,P,E,R1).
	


