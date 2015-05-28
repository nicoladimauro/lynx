/***************************************************************************************************
  Lynx

	Copyright 2010-11 University of Bari "Aldo Moro"
	                                                                          
  Author: Nicola Di Mauro                                                 

  $Date: 2011-11-08 13:45:35 +0100 (mar, 08 nov 2011) $

 ***************************************************************************************************

	The Lynx Software is made available under the terms and conditions of the Artistic License 2.0.
  LICENSEE shall acknowledge University of Bari "Aldo Moro" as the provider of the Software.

 ***************************************************************************************************/

:- module(flags, [
	set_lynx_flag/2,
	get_lynx_flag/2,
	lynx_flags/0,
	lynx_flags/1,
	set_auc_filename/2,
	get_auc_filename/2,
	set_all_auc_filenames/0
]).

:- yap_flag(unknown,fail).
:- yap_flag(discontiguous_warnings,on).
:- yap_flag(single_var_warnings,on).

:- use_module('output').


:- use_module(library(lists)).

:- dynamic '$classes'/1.
:- dynamic '$problem_name'/1.
:- dynamic '$output_file_name'/1.
:- dynamic '$minfreq'/1.
:- dynamic '$maxsize'/1.
:- dynamic '$minclassrecall'/1.
:- dynamic '$maxclassprecision'/1.
:- dynamic '$maxfollowat'/1.
:- dynamic '$lynx_verbose'/1.
:- dynamic '$discrscore'/1.
:- dynamic '$mincover'/1.
:- dynamic '$pattern_type'/1.
:- dynamic '$num_best_patterns'/1.
:- dynamic '$unbalanced_data'/1.
:- dynamic '$auc_filename'/1.
:- dynamic '$object_identity'/1.
:- dynamic '$bkfilename'/1.
:- dynamic '$classification_file_name'/1.

/* default parameters */ 


lynx_flags :-
	print_line,
	format(" Flag settings\n",[]),
	print_line,

	get_lynx_flag('$classes',Classes),	
	get_lynx_flag('$problem_name',Problem_name),	
	get_lynx_flag('$output_file_name',Output_file_name),	
	get_lynx_flag('$minfreq',Min_freq),
	get_lynx_flag('$maxsize',Max_size),
	get_lynx_flag('$minclassrecall', Min_class_recall),
	get_lynx_flag('$maxclassprecision', Max_class_precision),
	get_lynx_flag('$maxfollowat', Max_follow_at),
	get_lynx_flag('$lynx_verbose', Verbose),
	get_lynx_flag('$discrscore',Discr_score),
	get_lynx_flag('$mincover', Min_cover),
	get_lynx_flag('$pattern_type', Pattern_type),
	get_lynx_flag('$num_best_patterns', Num_best_patterns),
	get_lynx_flag('$unbalanced_data', Unbalanced_data),
	
	format(" Classes: ~w\n",[Classes]),
	format(" Problem name: ~w\n",[Problem_name]),
	format(" Output file name: ~w\n",[Output_file_name]),
	format(" Min_freq: ~w\n",[Min_freq]),
	format(" Max_size: ~w\n",[Max_size]),
	format(" Min_class_recall: ~w\n",[Min_class_recall]),
	format(" Max_class_precision: ~w\n",[Max_class_precision]),
	format(" Max_follow_at: ~w\n",[Max_follow_at]),
	format(" Verbose: ~w\n",[Verbose]),
	format(" Discr_score: ~w\n",[Discr_score]),
	format(" Min_cover: ~w\n",[Min_cover]),
	format(" Pattern_type: ~w\n",[Pattern_type]),
	format(" Num_best_patterns: ~w\n",[Num_best_patterns]),
	format(" Unbalanced data: ~w\n",[Unbalanced_data]),
	
	print_line.

	/*
	get_lynx_flag(min_covered_examples,Min_covered_examples),
	format("Each example should be covered by at least ~w patterns\n",[Min_covered_examples]),
	
	*/

lynx_flags(S) :-
	print_line(S),
	format(S,"Flag settings\n",[]),
	print_line(S),

	get_lynx_flag('$classes',Classes),	
	get_lynx_flag('$problem_name',Problem_name),	
	get_lynx_flag('$output_file_name',Output_file_name),	
	get_lynx_flag('$minfreq',Min_freq),
	get_lynx_flag('$maxsize',Max_size),
	get_lynx_flag('$minclassrecall', Min_class_recall),
	get_lynx_flag('$maxclassprecision', Max_class_precision),
	get_lynx_flag('$maxfollowat', Max_follow_at),
	get_lynx_flag('$lynx_verbose', Verbose),
	get_lynx_flag('$discrscore',Discr_score),
	get_lynx_flag('$mincover', Min_cover),
	get_lynx_flag('$pattern_type', Pattern_type),
	get_lynx_flag('$num_best_patterns', Num_best_patterns),
	get_lynx_flag('$unbalanced_data', Unbalanced_data),
	
	format(S," Classes: ~w\n",[Classes]),
	format(S," Problem name: ~w\n",[Problem_name]),
	format(S," Output file name: ~w\n",[Output_file_name]),
	format(S," Min_freq: ~w\n",[Min_freq]),
	format(S," Max_size: ~w\n",[Max_size]),
	format(S," Min_class_recall: ~w\n",[Min_class_recall]),
	format(S," Max_class_precision: ~w\n",[Max_class_precision]),
	format(S," Max_follow_at: ~w\n",[Max_follow_at]),
	format(S," Verbose: ~w\n",[Verbose]),
	format(S," Discr_score: ~w\n",[Discr_score]),
	format(S," Min_cover: ~w\n",[Min_cover]),
	format(S," Pattern_type: ~w\n",[Pattern_type]),
	format(S," Num_best_patterns: ~w\n",[Num_best_patterns]),
	format(S," Unbalanced data: ~w\n",[Unbalanced_data]),
	
	print_line(S).


set_lynx_flag(Flag_name,Flag_value):-
	Predicate_anon =.. [Flag_name,Flag_value],	
	Predicate =.. [Flag_name,Flag_value],
	retractall(Predicate_anon),
	assert(Predicate).
get_lynx_flag(Flag_name,Flag_value):-
	Predicate =.. [Flag_name,Flag_value],
	Predicate,
	!.
get_lynx_flag(_Flag_name,?).


set_all_auc_filenames :-
  get_lynx_flag('$classes',Classes),
  set_all_auc_filenames(Classes).


set_all_auc_filenames([]).
set_all_auc_filenames([C|R]) :-
  set_auc_filename(C),
  set_all_auc_filenames(R).

  
set_auc_filename(Class):-
  get_lynx_flag('$problem_name',Problem_name),	
  atom_chars(Problem_name,Problem_name_c),
  atom_chars(Class,Class_c),
  append(Problem_name_c,['_'],L1),
  append(L1,Class_c,L2),
  append(L2,['.',a,u,c,l,i,s,t],L4),
  atom_chars(Filename,L4),
  open(Filename,'write',Stream),
  close(Stream),
  assert('$auc_filename'(Class,Filename)).


get_auc_filename(Class,Filename):-
  '$auc_filename'(Class,Filename).
	
:- set_lynx_flag('$unbalanced_data', false).
:- set_lynx_flag('$object_identity',false).
