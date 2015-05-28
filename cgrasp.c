/***************************************************************************************************
  Lynx

	This file is part of Lynx
	http://www.di.uniba.it/~ndm/lynx/

	Copyright 2010-11 University of Bari "Aldo Moro"
	                                                                          
  Author: Nicola Di Mauro                                                 

	$Id: cgrasp.c 17 2011-10-26 10:59:07Z ndm $

  $Rev: 17 $
  $Author: ndm $
  $Date: 2011-10-26 12:59:07 +0200 (mer, 26 ott 2011) $

 ***************************************************************************************************

  The Lynx Software is made available under the terms and conditions of the Artistic License 2.0.
  LICENSEE shall acknowledge University of Bari "Aldo Moro" as the provider of the Software.

 ***************************************************************************************************
	
Artistic License 2.0
[OSI Approved License]
Artistic License 2.0

Copyright (c) 2000-2006, The Perl Foundation.

Everyone is permitted to copy and distribute  verbatim copies of this license document, but changing
it is not allowed.

Preamble

This  license establishes  the  terms under  which  a given  free software  Package  may be  copied,
modified, distributed, and/or redistributed. The intent  is that the Copyright Holder maintains some
artistic control over the  development of that Package while still keeping  the Package available as
open source and free software.

You are  always permitted  to make  arrangements wholly outside  of this  license directly  with the
Copyright Holder of  a given Package. If the terms  of this license do not permit  the full use that
you propose  to make of the  Package, you should contact  the Copyright Holder and  seek a different
licensing arrangement.

Definitions

"Copyright Holder" means the individual(s) or  organization(s) named in the copyright notice for the
entire Package.

"Contributor"  means any  party that  has contributed  code  or other  material to  the Package,  in
accordance with the Copyright Holder's procedures.

"You" and "your" means any person who would like to copy, distribute, or modify the Package.

"Package" means the collection of files distributed by the Copyright Holder, and derivatives of that
collection and/or of those  files. A given Package may consist of either  the Standard Version, or a
Modified Version.

"Distribute" means providing a copy of the Package or making it accessible to anyone else, or in the
case of a company or organization, to others outside of your company or organization.

"Distributor Fee" means any  fee that you charge for Distributing this  Package or providing support
for this Package to another party. It does not mean licensing fees.

"Standard Version" refers to  the Package if it has not been modified, or  has been modified only in
ways explicitly requested by the Copyright Holder.

"Modified Version" means the  Package, if it has been changed, and  such changes were not explicitly
requested by the Copyright Holder.

"Original  License" means this  Artistic License  as Distributed  with the  Standard Version  of the
Package, in its current version or as it may be modified by The Perl Foundation in the future.

"Source" form means the source code, documentation source, and configuration files for the Package.

"Compiled" form means the  compiled bytecode, object code, binary, or any  other form resulting from
mechanical transformation or translation of the Source form.

Permission for Use and Modification Without Distribution

(1) You  are permitted  to use the  Standard Version and  create and  use Modified Versions  for any
purpose without restriction, provided that you do not Distribute the Modified Version.

Permissions for Redistribution of the Standard Version

(2) You may Distribute verbatim copies of the Source form of the Standard Version of this Package in
any medium without restriction, either gratis or  for a Distributor Fee, provided that you duplicate
all of the original copyright notices  and associated disclaimers. At your discretion, such verbatim
copies may or may not include a Compiled form of the Package.


(3) You may  apply any bug fixes,  portability changes, and other modifications  made available from
the Copyright Holder.  The resulting Package will  still be considered the Standard  Version, and as
such will be subject to the Original License.

Distribution of Modified Versions of the Package as Source

(4) You may Distribute your Modified Version as  Source (either gratis or for a Distributor Fee, and
with or without a  Compiled form of the Modified Version) provided that  you clearly document how it
differs  from the  Standard Version,  including, but  not limited  to, documenting  any non-standard
features, executables, or modules, and provided that you do at least ONE of the following:


(a) make the Modified  Version available to the Copyright Holder of  the Standard Version, under the
Original  License, so  that the  Copyright Holder  may include  your modifications  in  the Standard
Version.
(b)  ensure that  installation of  your Modified  Version does  not prevent  the user  installing or
running the Standard Version.  In addition, the Modified Version must bear  a name that is different
from the name of the Standard Version.
(c) allow anyone who receives a copy of the Modified Version to make the Source form of the Modified
Version available to others under

(i) the Original License or
(ii)  a license  that permits  the licensee  to freely  copy, modify  and redistribute  the Modified
Version  using the same  licensing terms  that apply  to the  copy that  the licensee  received, and
requires that  the Source form of the  Modified Version, and of  any works derived from  it, be made
freely available in that license fees are prohibited but Distributor Fees are allowed.

Distribution of Compiled Forms of the Standard Version or Modified Versions without the Source

(5) You may Distribute Compiled forms of  the Standard Version without the Source, provided that you
include complete instructions  on how to get  the Source of the Standard  Version. Such instructions
must be  valid at the time of  your distribution. If these  instructions, at any time  while you are
carrying out such distribution, become invalid, you must provide new instructions on demand or cease
further distribution.  If you provide  valid instructions or  cease distribution within  thirty days
after you become aware that the instructions are invalid, then you do not forfeit any of your rights
under this license.


(6) You  may Distribute a Modified  Version in Compiled form  without the Source,  provided that you
comply with Section 4 with respect to the Source of the Modified Version.

Aggregating or Linking the Package

(7) You  may aggregate  the Package  (either the Standard  Version or  Modified Version)  with other
packages and Distribute  the resulting aggregation provided  that you do not charge  a licensing fee
for the  Package. Distributor Fees  are permitted,  and licensing fees  for other components  in the
aggregation  are permitted. The  terms of  this license  apply to  the use  and Distribution  of the
Standard or Modified Versions as included in the aggregation.

(8) You are permitted to link Modified and  Standard Versions with other works, to embed the Package
in a larger  work of your own, or  to build stand-alone binary or bytecode  versions of applications
that include  the Package, and Distribute the  result without restriction, provided  the result does
not expose a direct interface to the Package.

Items That are Not Considered Part of a Modified Version

(9) Works (including, but not limited to, modules and scripts) that merely extend or make use of the
Package, do not, by themselves, cause the Package  to be a Modified Version. In addition, such works
are not considered parts of the Package itself, and are not subject to the terms of this license.

General Provisions

(10) Any  use, modification, and distribution  of the Standard  or Modified Versions is  governed by
this Artistic License. By using, modifying or  distributing the Package, you accept this license. Do
not use, modify, or distribute the Package, if you do not accept this license.

(11) If your  Modified Version has been derived  from a Modified Version made by  someone other than
you,  you  are  nevertheless required  to  ensure  that  your  Modified  Version complies  with  the
requirements of this license.

(12) This  license does not grant  you the right to  use any trademark, service  mark, tradename, or
logo of the Copyright Holder.

(13) This license includes the non-exclusive, worldwide, free-of-charge patent license to make, have
made, use, offer to sell, sell, import and otherwise transfer the Package with respect to any patent
claims licensable  by the Copyright  Holder that  are necessarily infringed  by the Package.  If you
institute patent  litigation (including  a cross-claim or  counterclaim) against any  party alleging
that the Package constitutes direct or  contributory patent infringement, then this Artistic License
to you shall terminate on the date that such litigation is filed.

(14) Disclaimer of  Warranty: THE PACKAGE IS  PROVIDED BY THE COPYRIGHT HOLDER  AND CONTRIBUTORS "AS
IS'  AND WITHOUT  ANY EXPRESS  OR  IMPLIED WARRANTIES.  THE IMPLIED  WARRANTIES OF  MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE, OR NON-INFRINGEMENT ARE DISCLAIMED TO THE EXTENT PERMITTED BY YOUR
LOCAL LAW. UNLESS REQUIRED BY LAW, NO COPYRIGHT HOLDER OR CONTRIBUTOR WILL BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL,  OR CONSEQUENTIAL DAMAGES ARISING  IN ANY WAY OUT  OF THE USE  OF THE PACKAGE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

***************************************************************************************************/

#include <Yap/YapInterface.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <malloc.h>
#include <math.h>

#define frand() ((double) rand() / (RAND_MAX+1.0))
#define SPS(sequence,pattern,class)  seqPatScore[sequence*numClasses*numPatterns+pattern*numClasses+class]
#define SOLVAL(sequence,class)  solVal[sequence*numClasses+class]

void
WT (YAP_Term t)
{
  YAP_Term aa[1];
  aa[0] = t;
  YAP_Term G1 =
    YAP_MkApplTerm (YAP_MkFunctor (YAP_LookupAtom ("write"), 1), 1, aa);
  YAP_CallProlog (G1);
}

void
printIntArray (int *array, int l)
{
  int i;
  printf (" [");
  for (i = 0; i < l; i++)
    printf ("%d ", array[i]);
  printf ("]");
}

static int
p_cgrasp (void)
{

  time_t seconds;
  seconds = time (NULL);
  printf ("\n\n>>> GRASP likelihood optimization starts");
  printf (" (srand(%ld))", seconds);
  srand (seconds);
  // srand (1240332355);

  int i, j, k;
  YAP_Term suppTerm;

  YAP_Term numPatternsTerm = YAP_ARG1;
  YAP_Term numClassesTerm = YAP_ARG2;
  YAP_Term numSequencesTerm = YAP_ARG3;
  YAP_Term ProbMatrix = YAP_ARG4;
  YAP_Term pclasses = YAP_ARG5;
  YAP_Term maxIterTerm = YAP_ARG6;
  YAP_Term alphaTerm = YAP_ARG7;

  YAP_Int numPatterns = YAP_IntOfTerm (numPatternsTerm);
  YAP_Int numClasses = YAP_IntOfTerm (numClassesTerm);
  YAP_Int numSequences = YAP_IntOfTerm (numSequencesTerm);
  YAP_Int maxIter = YAP_IntOfTerm (maxIterTerm);
  double alpha = YAP_FloatOfTerm (alphaTerm);

  printf ("\n  Patterns:%d Classes: %d Total Models:%d", (int) numPatterns,
	  (int) numClasses, (int) numSequences);
  fflush (stdout);

  double *seqPatScore =
    (double *) malloc (numPatterns * numSequences * numClasses *
		       sizeof (double));
  double *classProb = (double *) malloc (numClasses * sizeof (double));
  int *seqClass = (int *) malloc (numSequences * sizeof (int));
  int *nSeqForClass = (int *) malloc (numClasses * sizeof (int));

  int seqs;
  YAP_Term Probs;
  YAP_Term listModels;
  YAP_Term models1;
  YAP_Term G;


  for (i = 0; i < numClasses; i++)
    nSeqForClass[i] = 0;

  if (YAP_IsPairTerm (pclasses))
    for (i = 0; i < numClasses; i++)
      {
	suppTerm = YAP_HeadOfTerm (pclasses);
	if (YAP_IsFloatTerm (suppTerm))
	  classProb[i] = YAP_FloatOfTerm (suppTerm);
	pclasses = YAP_TailOfTerm (pclasses);
      }
  else
    return FALSE;

  seqs = 0;
  i = 0;
  while (YAP_IsPairTerm (ProbMatrix))
    {
      listModels = YAP_HeadOfTerm (ProbMatrix);
      while (YAP_IsPairTerm (listModels))
	{
	  for (j = 0; j < numPatterns; j++)
	    {
	      Probs = YAP_HeadOfTerm (listModels);
	      for (k = 0; k < numClasses; k++)
		{
		  SPS (seqs, j, k) = YAP_FloatOfTerm (YAP_HeadOfTerm (Probs));
		  Probs = YAP_TailOfTerm (Probs);
		}
	      listModels = YAP_TailOfTerm (listModels);
	    }
	  seqClass[seqs] = i;
	  nSeqForClass[i]++;
	  seqs++;
	}
      ProbMatrix = YAP_TailOfTerm (ProbMatrix);
      i++;
    }

  printf (" [");
  for (j = 0; j < numClasses; j++)
    printf ("%d ", nSeqForClass[j]);
  printf ("]");

  int iter, posmax, selectedPattern;
  double minValue;
  int *solution = (int *) malloc (numPatterns * sizeof (int));
  int lengthSolution;
  double valueSolution;
  double *classValues = (double *) malloc (numClasses * sizeof (double));
  int *classErrors = (int *) malloc (numClasses * sizeof (int));
  int *bestClassErrors = (int *) malloc (numClasses * sizeof (int));
  int *selectable =
    (int *) malloc (numPatterns * sizeof (int)), numSelectable, h;
  double errors;
  double max;

  int *bestSolution = (int *) malloc (numPatterns * sizeof (int));
  int lengthBestSolution;
  double valueBestSolution;

  int *rclSolutions = (int *) malloc (numPatterns * sizeof (int));
  double *rclSolutionsValue =
    (double *) malloc (numPatterns * sizeof (double));
  int rclNumSolutions;
  int *rclSet = (int *) malloc (numPatterns * sizeof (int));
  double *rclSetValue = (double *) malloc (numPatterns * sizeof (double));
  int rclNumSet;

  int *localSolution = (int *) malloc (numPatterns * sizeof (int));
  int lengthLocalSolution;
  double valueLocalSolution;

  int taken, jj;
  double minValueLocalSolution;
  int *bestLocalSolution = (int *) malloc (numPatterns * sizeof (int));
  int lengthBestLocalSolution;
  double valueBestLocalSolution;

  double *solVal =
    (double *) malloc (numSequences * numClasses * sizeof (double));

  valueBestSolution = (double) INT_MAX;
  lengthBestSolution = INT_MAX;

	double likelihood;
	double bestlikelihood = - (double) INT_MAX;


  printf ("\n  Class probabilities: ");
  for (k = 0; k < numClasses; k++)
    printf ("%f ", classProb[k]);

  printf ("\n");

  for (iter = 0; iter < maxIter; iter++)
    {
      printf ("+");fflush(stdout);
      /*printf ("\n [Iteration %d]", iter + 1); */

      /*
       * if (iter == 0) { valueSolution = INT_MAX; lengthSolution = 0;
       * for (i = 0; i < numPatterns; i++) { solution[i] = i;
       * lengthSolution++; } } else { 
       */
      /*
       * ad ogni iterazione scelgo un alpha random fra 0 ed 1 
       */
      alpha = frand ();
      /*printf (" alpha %f", alpha); */

      for (i = 0; i < numPatterns; i++)
	selectable[i] = i;
      numSelectable = numPatterns;

      /*
       * grasp construction 
       */
      /*printf ("\n   --> Construction: "); */
      lengthSolution = 0;
      valueSolution = (double) INT_MAX;
			likelihood = 1.0;
      double newValue;
      int added = 1;

      for (j = 0; j < numSequences; j++)
	{
	  for (k = 0; k < numClasses; k++)
	    SOLVAL (j, k) = classProb[k];
	}

      do
	{
	  rclNumSolutions = 0;
	  minValue = (double) INT_MAX;
	  for (i = 0; i < numSelectable; i++)
	    {
	      //             printf("\n#C: pattern %d",selectable[i]);
	      errors = 0;
	      for (j = 0; j < numClasses; j++)
		classErrors[j] = 0;
	      for (j = 0; j < numSequences; j++)
		{
		  for (k = 0; k < numClasses; k++)
		    classValues[k] =
		      SOLVAL (j, k) * SPS (j, selectable[i], k);

		  /*
		   * find max position 
		   */
		  max = classValues[0];
		  posmax = 0;
		  for (k = 1; k < numClasses; k++)
		    {
		      if (classValues[k] > max)
			{
			  max = classValues[k];
			  posmax = k;
			}
		    }
		  if (seqClass[j] != posmax)
		    {
		      errors++;
		      classErrors[seqClass[j]]++;
		    }
		}
	      rclSolutions[rclNumSolutions] = i;
	      rclSolutionsValue[rclNumSolutions] = 0.0;
	      for (k = 0; k < numClasses; k++)
		{
		  rclSolutionsValue[rclNumSolutions] +=
		    (double) classErrors[k] / (double) nSeqForClass[k];
		}
	      rclSolutionsValue[rclNumSolutions] /= (double) numClasses;
	      rclNumSolutions++;


	    }

	  /*
	   * computing rcl set and selection 
	   */
	  /*
	   * find min and max values 
	   */
	  double minrclval, maxrclval;
	  minrclval = rclSolutionsValue[0];
	  maxrclval = rclSolutionsValue[0];
	  for (k = 1; k < rclNumSolutions; k++)
	    {
	      if (rclSolutionsValue[k] > maxrclval)
		maxrclval = rclSolutionsValue[k];
	      if (rclSolutionsValue[k] < minrclval)
		minrclval = rclSolutionsValue[k];
	    }

	  /*
	   * compute rcl set 
	   */
	  double limitRcl = (minrclval + alpha * (maxrclval - minrclval));
	  rclNumSet = 0;
	  for (k = 0; k < rclNumSolutions; k++)
	    {
	      if (rclSolutionsValue[k] <= limitRcl)
		{
		  rclSet[rclNumSet] = rclSolutions[k];
		  rclSetValue[rclNumSet] = rclSolutionsValue[k];
		  rclNumSet++;
		}
	    }
	  /*
	   * select the value 
	   */
	  int rclSelected = (int) (frand () * rclNumSet);
	  selectedPattern = rclSet[rclSelected];
	  minValue = rclSetValue[rclSelected];
	  newValue = minValue;
	  if (newValue <= valueSolution)
	    {
	      solution[lengthSolution] = selectable[selectedPattern];
	      valueSolution = newValue;
	      for (j = 0; j < numSequences; j++)
		for (k = 0; k < numClasses; k++)
		  SOLVAL (j, k) *= SPS (j, solution[lengthSolution], k);
	      lengthSolution++;
	      numSelectable--;
	      selectable[selectedPattern] = selectable[numSelectable];
	    }
	  else
	    added = 0;
	}
      while (numSelectable && added);
      /*
       * grasp local 
       */


      int found = 1;
      while (found)
	{
	  found = 0;

	  /*
	   * finding neighboors 
	   */
	  numSelectable = 0;
	  for (i = 0; i < numPatterns; i++)
	    {
	      taken = 1;
	      for (j = 0; j < lengthSolution && taken; j++)
		if (solution[j] == i)
		  taken = 0;
	      if (taken)
		{
		  selectable[numSelectable] = i;
		  numSelectable++;
		}
	    }

	  /*
	   * printf("\nSle "); printIntArray(selectable,numSelectable); 
	   */

	  for (i = 0; i < lengthSolution; i++)
	    localSolution[i] = solution[i];
	  valueLocalSolution = valueSolution;
	  lengthLocalSolution = lengthSolution;

	  minValueLocalSolution = valueSolution;

	  /*
	   * REMOVE a pattern 
	   */
	  for (i = 0; i < lengthSolution; i++)
	    {
	      /*
	       * calcolo il valore di questa soluzione locale 
	       */
	      errors = 0;
	      for (k = 0; k < numClasses; k++)
		classErrors[k] = 0;
	      for (jj = 0; jj < numSequences; jj++)
		{
		  for (k = 0; k < numClasses; k++)
		    classValues[k] =
		      SOLVAL (jj, k) / SPS (jj, solution[i], k);
		  /*
		   * find max position 
		   */
		  max = classValues[0];
		  posmax = 0;
		  for (k = 1; k < numClasses; k++)
		    {
		      if (classValues[k] > max)
			{
			  max = classValues[k];
			  posmax = k;
			}
		    }
		  if (seqClass[jj] != posmax)
		    {
		      errors++;
		      classErrors[seqClass[jj]]++;
		    }
		}

	      errors = 0.0;
	      for (k = 0; k < numClasses; k++)
		{
		  errors +=
		    (double) classErrors[k] / (double) nSeqForClass[k];
		}
	      errors /= (double) numClasses;

	      if (errors < minValueLocalSolution)
		{
		  found = 1;
		  for (k = 0; k < lengthSolution; k++)
		    if (k != i)
		      bestLocalSolution[k] = solution[k];
		  lengthBestLocalSolution = lengthSolution - 1;
		  valueBestLocalSolution = errors;
		}
	    }
	  /*
	   * ADD a pattern 
	   */
	  // printf(" $a: ");
	  for (j = 0; j < numSelectable; j++)
	    {
	      /*
	       * calcolo il valore di questa soluzione locale 
	       */
	      errors = 0.0;
	      for (k = 0; k < numClasses; k++)
		classErrors[k] = 0.0;

	      for (jj = 0; jj < numSequences; jj++)
		{
		  for (k = 0; k < numClasses; k++)
		    classValues[k] =
		      SOLVAL (jj, k) * SPS (jj, selectable[j], k);

		  /*
		   * find max position 
		   */
		  max = classValues[0];
		  posmax = 0;
		  for (k = 1; k < numClasses; k++)
		    {
		      if (classValues[k] > max)
			{
			  max = classValues[k];
			  posmax = k;
			}
		    }
		  if (seqClass[jj] != posmax)
		    {
		      errors++;
		      classErrors[seqClass[jj]]++;
		    }
		}

	      errors = 0.0;
	      for (k = 0; k < numClasses; k++)
		{
		  errors +=
		    (double) classErrors[k] / (double) nSeqForClass[k];
		}
	      errors /= (double) numClasses;

	      if (errors < minValueLocalSolution)
		{
		  // printf("!");
		  found = 1;
		  for (k = 0; k < lengthSolution; k++)
		    bestLocalSolution[k] = solution[k];
		  bestLocalSolution[lengthSolution] = selectable[j];
		  lengthBestLocalSolution = lengthSolution + 1;
		  valueBestLocalSolution = errors;
		}
	    }

	  if (found)
	    {
	      for (i = 0; i < lengthBestLocalSolution; i++)
		solution[i] = bestLocalSolution[i];
	      valueSolution = valueBestLocalSolution;
	      lengthSolution = lengthBestLocalSolution;
	      /*
	       * aggiorno SOLVAL 
	       */
	      for (jj = 0; jj < numSequences; jj++)
		for (k = 0; k < numClasses; k++)
		  SOLVAL (jj, k) = classProb[k];
	      for (jj = 0; jj < numSequences; jj++)
		for (h = 0; h < lengthSolution; h++)
		  for (k = 0; k < numClasses; k++)
		    SOLVAL (jj, k) =
		      SOLVAL (jj, k) * SPS (jj, solution[h], k);
	    }
	}


			/** compute the likelihood of the solution **/

			likelihood = 0.0;
			for (j = 0; j < numSequences; j++)
				likelihood = likelihood + log(SOLVAL (j,seqClass[j]));
			//likelihood = likelihood / lengthSolution;
			/******************/

			
			if ((valueSolution < valueBestSolution)
					/*					|| ((valueSolution == valueBestSolution)
											&& (lengthSolution < lengthBestSolution))*/
					|| ((valueSolution == valueBestSolution)
					&& (likelihood > bestlikelihood))) 

			//	if (valueSolution < valueBestSolution)
	{
	  printf
	    ("\n --> newValue %f, oldValue %f --- newLength %d, oldLength %d, oldlikelihood %f, likelihood %f\n",
	     valueSolution, valueBestSolution, lengthSolution,
	     lengthBestSolution, bestlikelihood, likelihood);
	  for (j = 0; j < lengthSolution; j++)
	    bestSolution[j] = solution[j];
	  lengthBestSolution = lengthSolution;
	  valueBestSolution = valueSolution;
		bestlikelihood = likelihood;
	}
    }


  printf ("\n *** Best Solution: ");
  printIntArray (bestSolution, lengthBestSolution);
  printf (" value %f", valueBestSolution);


  /* print errors */
  for (j = 0; j < numSequences; j++)
    {
      for (k = 0; k < numClasses; k++)
	SOLVAL (j, k) = classProb[k];
    }
  for (k = 0; k < numClasses; k++)
    classErrors[k] = 0.0;

  for (jj = 0; jj < numSequences; jj++)
    {
      for (k = 0; k < numClasses; k++)
	{
	  classValues[k] = classProb[k];
	  for (j = 0; j < lengthBestSolution; j++)
	    classValues[k] *= SPS (jj, bestSolution[j], k);
	}

      max = classValues[0];
      posmax = 0;
      for (k = 1; k < numClasses; k++)
	{
	  if (classValues[k] > max)
	    {
	      max = classValues[k];
	      posmax = k;
	    }
	}
      if (seqClass[jj] != posmax)
	classErrors[seqClass[jj]]++;
    }
  int TotErrorors;
  printf (" errors ");
  TotErrorors = 0;
  for (k = 0; k < numClasses; k++)
    {
      printf ("%d/%d ", classErrors[k], nSeqForClass[k]);
      TotErrorors += classErrors[k];
    }
  printf (" [%d/%d]", TotErrorors, (int) numSequences);

			/***********/


  YAP_Term out = YAP_MkAtomTerm (YAP_LookupAtom ("[]"));
  for (j = lengthBestSolution; j > 0; j--)
    out = YAP_MkPairTerm (YAP_MkIntTerm (bestSolution[j - 1] + 1), out);

  printf ("\n>>> GRASP likelihood optimization ends");
  return (YAP_Unify (YAP_ARG8, out));


}

static int
p_nlist (void)
{

  int i;

  YAP_Term emptyList = YAP_MkAtomTerm (YAP_LookupAtom ("[]"));

  YAP_Term num = YAP_ARG1;
  YAP_Term SOLUTION = YAP_ARG6;

  YAP_Int n = YAP_IntOfTerm (num);

  int *seq = (int *) malloc (n * sizeof (int));
  for (i = 0; i < n; i++)
    seq[i] = i;

  YAP_Term out = YAP_MkAtomTerm (YAP_LookupAtom ("[]"));
  for (i = n; i > 0; i--)
    out = YAP_MkPairTerm (YAP_MkIntTerm (seq[i - 1] + 1), out);

  return (YAP_Unify (SOLUTION, out));
}

void
init_my_predicates ()
{
  YAP_UserCPredicate ("cgrasp", p_cgrasp, 8);
  YAP_UserCPredicate ("nlist", p_nlist, 6);
  return;
}
