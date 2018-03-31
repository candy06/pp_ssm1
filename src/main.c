/*
********************************************************************************
*                               Polytech'Nice Sophia
*
*
* Filename      : main.c
* Programmer    : Loïc ROSE
* Description   : algorithm to find the maximal subsequence of an array
********************************************************************************
*/


/*
********************************************************************************
*                               INCLUDE FILES
********************************************************************************
*/

#include <stdlib.h>
#include <stdio.h>
#include <omp.h>
#include <math.h>
#include <limits.h>


/*
********************************************************************************
*                               CONSTANTS & MACRO
********************************************************************************
*/

#define   SUFFIX_MODE       1
#define   PREFIX_MODE       2
#define   SUM_OPERATION     3
#define   MAX_OPERATION     4

#define   MAX(X,Y)          (((X) > (Y)) ? (X) : (Y))
#define   SUM(X,Y)          ((X) + (Y))


/*
********************************************************************************
*                               GLOBAL VARIABLES
********************************************************************************
*/

int m;

struct tablo {
    int * tab;
    int size;
};

/*
********************************************************************************
*                               getLog2()
*
* Description :
* Arguments   :
* Returns     :
********************************************************************************
*/

int getLog2(int x)
{
  return log(x) / log(2);
}

/*
********************************************************************************
*                               printArray()
*
* Description :
* Arguments   :
* Returns     :
********************************************************************************
*/

void printArray(struct tablo * tmp)
{
  printf("---- Array of size %i ----\n", tmp->size);
  int size = tmp->size;
  for (int i = 0; i < size; ++i) {
    printf("%i ", tmp->tab[i]);
  }
  printf("\n");
}


/*
********************************************************************************
*                               allocateTablo()
*
* Description :
* Arguments   :
* Returns     :
********************************************************************************
*/

struct tablo * allocateTablo(int size)
{
  struct tablo * tmp = malloc(sizeof(struct tablo));
  tmp->size = size;
  tmp->tab = malloc(size*sizeof(int));
  tmp->tab[0] = 0;
  return tmp;
}


/*
********************************************************************************
*                               generateArray()
*
* Description :
* Arguments   :
* Returns     :
********************************************************************************
*/

void generateArray(struct tablo * s)
{
  s->size = 16;
  s->tab = malloc(s->size*sizeof(int));
  s->tab[0] = 3;
  s->tab[1] = 2;
  s->tab[2] = -7;
  s->tab[3] = 11;
  s->tab[4] = 10;
  s->tab[5] = -6;
  s->tab[6] = 4;
  s->tab[7] = 9;
  s->tab[8] = -6;
  s->tab[9] = 1;
  s->tab[10] = -2;
  s->tab[11] = -3;
  s->tab[12] = 4;
  s->tab[13] = -3;
  s->tab[14] = 0;
  s->tab[15] = 2;
}


/*
********************************************************************************
*                               up()
*
* Description :
* Arguments   :
* Returns     :
********************************************************************************
*/

void up(struct tablo * src, struct tablo * dst, int operation)
{

  int size_src = src->size;     /* size of the source array */
  int size_dst = dst->size;     /* size of the destination array */

  // Copy the source array at the end of the destination array
  for (int i = size_dst - 1; i > (size_dst/2) - 1; i--) {
    dst->tab[i] = src->tab[i - size_src];
  }

  for (int l = m - 1; l >= 0; l--) {
    int start = pow(2, l);
    int end = pow(2, l+1) -1;
    #pragma omp parallel for
    for (int j = start; j <= end; j++) {
      dst->tab[j] = (operation == SUM_OPERATION) ? SUM(dst->tab[2*j], dst->tab[2*j+1]) : MAX(dst->tab[2*j], dst->tab[2*j+1]);
    }
  }

}


/*
********************************************************************************
*                               down()
*
* Description :
* Arguments   :
* Returns     :
********************************************************************************
*/

void down(struct tablo * a, struct tablo * b, int mode, int operation) {

     b->tab[1] = (operation == SUM_OPERATION) ? 0 : INT_MIN;

    for (int l = 1; l <= m; l++) {
      int start = pow(2, l);
      int end = pow(2, l+1) - 1;
      #pragma omp parallel for
      for (int j = start; j <= end; j++) {
        if (j%2 == 0) {
          if (mode == PREFIX_MODE) b->tab[j] = b->tab[j/2];
          if (mode == SUFFIX_MODE)
            b->tab[j] = (operation == SUM_OPERATION) ? SUM(b->tab[j/2], a->tab[j+1]) : MAX(b->tab[j/2], a->tab[j+1]);
        } else {
          if (mode == PREFIX_MODE)
            b->tab[j] = (operation == SUM_OPERATION) ? SUM(b->tab[j/2], a->tab[j-1]) : MAX(b->tab[j/2], a->tab[j-1]);
          if (mode == SUFFIX_MODE) b->tab[j] = b->tab[j/2];
        }
      }
    }

}


/*
********************************************************************************
*                               final()
*
* Description :
* Arguments   :
* Returns     :
********************************************************************************
*/

void final(struct tablo * a, struct tablo * b, int operation) {

    int start = pow(2, m);
    int end = pow(2, m + 1) - 1;
    #pragma omp parallel for
    for (int j = start; j <= end; j++) {
      b->tab[j] = (operation == SUM_OPERATION) ? SUM(b->tab[j], a->tab[j]) : MAX(b->tab[j], a->tab[j]);
    }

}


/*
********************************************************************************
*                               doAlgo()
*
* Description :
* Arguments   :
* Returns     :
********************************************************************************
*/

void do_up_down_final(struct tablo * src, struct tablo * dst, int mode, int operation)
{

  int size_src = src->size;     /* size of the source array */
  int size_dst = dst->size;     /* size of the destination array */

  struct tablo * a = allocateTablo(size_src*2);
  struct tablo * b = allocateTablo(size_src*2);

  up(src, a, operation);
  down(a, b, mode, operation);
  final(a, b, operation);

  int size_of_b = b->size;

  for (int i = 0; i < size_dst; i++) {
    dst->tab[i] = b->tab[(size_of_b/2) + i];
  }

}


/*
********************************************************************************
*                               findM()
*
* Description :
* Arguments   :
* Returns     :
********************************************************************************
*/

struct tablo * findMaxTablo(struct tablo * src, struct tablo * PSUM, struct tablo * SSUM, struct tablo * PMAX, struct tablo * SMAX)
{

    int size_src = src->size;

    struct tablo * Ms = allocateTablo(size_src);
    struct tablo * Mp = allocateTablo(size_src);
    struct tablo * M = allocateTablo(size_src);

    #pragma omp parallel for
    for (int i = 0; i < size_src; i++) {
      Ms->tab[i] = PMAX->tab[i] - SSUM->tab[i] + src->tab[i];
      Mp->tab[i] = SMAX->tab[i] - PSUM->tab[i] + src->tab[i];
      M->tab[i] = Ms->tab[i] + Mp->tab[i] - src->tab[i];
    }

    return M;

}

/*
********************************************************************************
*                               main()
*
* Description :
* Arguments   :
* Returns     :
********************************************************************************
*/

int main(int argc, char **argv) {

  struct tablo source;
  generateArray(&source);

  m = getLog2(source.size);   /* size of source = n = pow(2, m) */

  struct tablo * PSUM = allocateTablo(source.size);
  do_up_down_final(&source, PSUM, PREFIX_MODE, SUM_OPERATION);
  printArray(PSUM);

  struct tablo * SSUM = allocateTablo(source.size);
  do_up_down_final(&source, SSUM, SUFFIX_MODE, SUM_OPERATION);
  printArray(SSUM);

  struct tablo * SMAX = allocateTablo(source.size);
  do_up_down_final(PSUM, SMAX, SUFFIX_MODE, MAX_OPERATION);
  printArray(SMAX);

  struct tablo * PMAX = allocateTablo(source.size);
  do_up_down_final(SSUM, PMAX, PREFIX_MODE, MAX_OPERATION);
  printArray(PMAX);

  struct tablo * M = findMaxTablo(&source, PSUM, SSUM, PMAX, SMAX);
  printArray(M);

}
