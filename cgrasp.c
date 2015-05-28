/***************************************************************************************************
  Lynx

	Copyright 2010-11 University of Bari "Aldo Moro"
  Author: Nicola Di Mauro                                                 

  $Date: 2011-10-26 12:59:07 +0200 (mer, 26 ott 2011) $

 ***************************************************************************************************

  The Lynx Software is made available under the terms and conditions of the Artistic License 2.0.
  LICENSEE shall acknowledge University of Bari "Aldo Moro" as the provider of the Software.

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
