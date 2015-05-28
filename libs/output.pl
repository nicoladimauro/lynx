/***************************************************************************************************
  Lynx

	Copyright 2010-11 University of Bari "Aldo Moro"
	                                                                          
  Author: Nicola Di Mauro                                                 
  $Date: 2011-11-08 13:45:45 +0100 (mar, 08 nov 2011) $

 ***************************************************************************************************

	The Lynx Software is made available under the terms and conditions of the Artistic License 2.0.
  LICENSEE shall acknowledge University of Bari "Aldo Moro" as the provider of the Software.

 ***************************************************************************************************/

:- module(output,[
	portraylist/1,
	portraylist/2,
	print_line/0,
	print_line/1,	
	format_outfile/2,
	open_comment_line/1,
	close_comment_line/1
	]).

:- yap_flag(unknown,fail).
:- yap_flag(discontiguous_warnings,on).
:- yap_flag(single_var_warnings,on).

:- use_module('flags').


format_outfile(String,Arguments) :-
	get_lynx_flag('$fileout',StreamOut),	
	format(StreamOut,String,Arguments).

/***************************************************************************
 ***************************************************************************/
print_line :-
	format("------------------------------------------------------------------------------------------------------------------------~n",[]).

print_line(Stream) :-
	format(Stream,"------------------------------------------------------------------------------------------------------------------------~n",[]).

open_comment_line(Stream) :-
	format(Stream,"/***********************************************************************************************************************~n",[]).
close_comment_line(Stream) :-
	format(Stream,"***********************************************************************************************************************/~n",[]).

/***************************************************************************
 ***************************************************************************/
portraylist(P) :-
	'$portraylist'(P),
	fail.
portraylist(_).

'$portraylist'(List) :- 
	'$beautify_vars'(List),
	writeq(List).

portraylist(Stream,P) :-
	'$portraylist'(Stream,P),
	fail.
portraylist(_,_).

'$portraylist'(Stream,List) :- 
	'$beautify_vars'(List),
	writeq(Stream,List).

/* =========================================================================

   Definitions of predicates

  '$beautify_vars'/1
  '$list_get_vars'/3
  '$list_transform'/2

  are taken from listing.yap contained in the
  Yap Prolog 5.1.1 distribution.
  
  Yap Prolog was developed at NCCUP - Universidade do Porto
  Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 

   ========================================================================= */

'$beautify_vars'(T) :-
	'$list_get_vars'(T,[],L),
	msort(L,SL),
	'$list_transform'(SL,0).

'$list_get_vars'(V,L,[V|L] ) :- var(V), !.
'$list_get_vars'(Atomic, M, M) :-
	primitive(Atomic), !.
'$list_get_vars'([Arg|Args], M, N) :-  !,
	'$list_get_vars'(Arg, M, K),
	'$list_get_vars'(Args, K, N).
'$list_get_vars'(Term, M, N) :-
	Term =.. [_|Args],
	'$list_get_vars'(Args, M, N).

'$list_transform'([],_) :- !.
'$list_transform'([X,Y|L],M) :- X == Y, X = '$VAR'(M), !, N is M+1,
			'$list_transform'(L,N).
'$list_transform'('$VAR'(-1).L,M) :- !, '$list_transform'(L,M).
'$list_transform'(_.L,M) :- '$list_transform'(L,M).

