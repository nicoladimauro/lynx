/***************************************************************************************************
  Lynx

	Copyright 2010-11 University of Bari "Aldo Moro"
	                                                                          
  Author: Nicola Di Mauro                                                 

  $Date: 2011-07-11 16:33:45 +0200 (lun, 11 lug 2011) $

 ***************************************************************************************************

	The Lynx Software is made available under the terms and conditions of the Artistic License 2.0.
  LICENSEE shall acknowledge University of Bari "Aldo Moro" as the provider of the Software.

 ***************************************************************************************************/

:- module(prover,[
		  prove_pattern_module_oi/3,
		  prove_pattern_module/2,
		  prove_patterns_for_classes/3,
		  prove_constraints/2,
		  prove_initial_patterns/2
		  ]).

:- yap_flag(unknown,fail).
:- yap_flag(discontiguous_warnings,on).
:- yap_flag(single_var_warnings,on).

:-use_module(library(lists)).
:-use_module(library(terms)).

:- use_module('flags').
:- use_module('utils').
:- use_module('../lynx').

/***************************************************************************
 ***************************************************************************/
/*  la dimostrazione del pattern avviene secondo Object Identity */

prove_pattern_module(P,Mod) :-
    (get_lynx_flag('$object_identity',true) ->
      term_variables(P,Vars),
      prove_patternoi_module1(P,Mod),
      alldiff(Vars)
    ;
      prove_patternoi_module1(P,Mod)
    ),
    !.
prove_patternoi_module1([],_Module).
prove_patternoi_module1([X|R],Module) :-
  call(models:m(Module,X)),
  prove_patternoi_module1(R,Module).
/* per la dimostrazione dei letterali dimensionali */
prove_patternoi_module1([X|R],Module) :-
  call(models:X),
  prove_patternoi_module1(R,Module).

/* ----- */


prove_constraints([Constraint|_Rest],Pattern) :-
	(get_lynx_flag('$object_identity',true) ->
	 term_variables(Constraint,V),
	 prove_constraint(Constraint,Pattern),
	 alldiff(V)
	;
	 prove_constraint(Constraint,Pattern)
	),
	!.
prove_constraints([_Constraint|Rest],Pattern) :-
	prove_constraints(Rest,Pattern).

prove_constraint([],_Pattern).
prove_constraint([Lit|RestLit],Pattern) :-
	prove_litc(Lit,Pattern,RestPattern),
	prove_constraint(RestLit,RestPattern),!.
prove_constraint([OrConstraint],Pattern):-
	prove_or_constraint(OrConstraint,0,Pattern).

prove_or_constraint((A;B),N,Pattern) :-
	prove_litc(A,Pattern,_),
	!,
	N1 is N + 1,
	(N1 == 1 ->
	 prove_or_constraint(B,N1,Pattern)
	;
	 true
	).
prove_or_constraint((_A;B),N,Pattern) :-
	prove_or_constraint(B,N,Pattern).
prove_or_constraint(A,1,Pattern) :-
	prove_litc(A,Pattern,_).
	
/* -------------------------------------------------------------------------------------------
	 prove_litc/3
*/ 	
prove_litc(A\==B,Pattern,Pattern) :-
	!,
	A \== B.
prove_litc(Lit,[Lit|Pattern],Pattern).
prove_litc(Lit,[L|Pattern],[L|RestPattern]):-
	prove_litc(Lit,Pattern,RestPattern).

/* ------------------------------------------------------------------------------------------- */	
prove_patterns_for_classes(_P,[],[]).
prove_patterns_for_classes(P,[Class|RestClasses],[ClassFreq|RestFrequencies]):-
	prove_patterns_for_class(P,Class,ClassFreq),
	!,
	prove_patterns_for_classes(P,RestClasses,RestFrequencies).


prove_patterns_for_class(P,[Class,InitialFreq,_OldFreq,Modules],[Class,InitialFreq,NewFreq,Modules1]):-
	/* NewFreq conta il numero di sequenze per cui il pattern ha almeno una frequenza minima */
%	write('\n\t\t'),write(Class),
	prove_patterns_for_modules(P,Modules,Modules1,NewFreq).

prove_patterns_for_modules(_P,[],[],0).
prove_patterns_for_modules(P,[Module|RestModules],[Module|RestModules1],Freq):-
	prove_patterns_for_module(P,Module),
	!,
	prove_patterns_for_modules(P,RestModules,RestModules1,Freq1),
	Freq is Freq1 + 1.
prove_patterns_for_modules(P,[_Module|RestModules],RestModules1,Freq):-
	prove_patterns_for_modules(P,RestModules,RestModules1,Freq).

prove_patterns_for_module(Pattern,Module):-
	copy_term(Pattern,P1),
	prove_pattern_module(P1,Module),
	!.

/* -------------------------------------------------------------------------------------------
	 prove_initial_patterns/2
	 Calcola le frequenze dei pattern iniziali
*/	
prove_initial_patterns(P,Frequencies):-
	'$models'(Models),
	prove_initial_patterns_for_classes(P,Models,Frequencies).
prove_initial_patterns_for_classes(_P,[],[]).
prove_initial_patterns_for_classes(P,[Class|RestClasses],[ClassFreq|RestFrequencies]):-
	prove_initial_patterns_for_class(P,Class,ClassFreq),
	prove_initial_patterns_for_classes(P,RestClasses,RestFrequencies).
prove_initial_patterns_for_class(P,[Class,Modules],[Class,Freq,Freq,FreqModules]):-
	prove_initial_patterns_for_modules(P,Modules,FreqModules,Freq).

prove_initial_patterns_for_modules(_P,[],[],0).
prove_initial_patterns_for_modules(P,[Module|RestModules],[Module|RestModules1],Freq):-
	copy_term(P,P1),
	prove_initial_patterns_for_module(P1,Module),
	!,
	prove_initial_patterns_for_modules(P,RestModules,RestModules1,Freq1),
	Freq is Freq1 + 1.
prove_initial_patterns_for_modules(P,[_Module|RestModules],RestModules1,Freq):-
	prove_initial_patterns_for_modules(P,RestModules,RestModules1,Freq).

/* -------------------------------------------------------------------------------------------
	 prove_initial_patterns_for_module/3
*/
prove_initial_patterns_for_module([P],Module):-
	(get_lynx_flag('$object_identity',true) ->
	  term_variables(P,V),
	  call(models:m(Module,P)),
	  alldiff(V)   % for Object Identity
	;
	  call(models:m(Module,P))
	),
	!.


