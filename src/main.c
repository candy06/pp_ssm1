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
* Description : method to get the log base 2 of a number
* Arguments   : an integer
* Returns     : the log2 of the given number
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
* Description : display the array given in parameter
* Arguments   : a struct tablo (that represents an array)
* Returns     : void
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
* Description : allocate enough memory for a tablo struct
* Arguments   : an integer
* Returns     : struct tablo with the specified size given in parameters
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
* Description : method to generate and fill a sample array with custom values
* Arguments   : struct tablo
* Returns     : void
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
* Description : first step of the algorithm presented in class
* Arguments   : 2 struct tablo (source, destination) and an integer that
                represents the operation that needs to be done (SUM_OPERATION
                or MAX_OPERATION)
* Returns     : void
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
* Description : second step of the algorithm presented in class
* Arguments   : 2 struct tablo (a and b) and 2 integers - the first integer
                represents the mode (PREFIX_MODE or SUFFIX_MODE) and the second
                integer represents the operation that needs to be done (SUM_OPERATION
                or MAX_OPERATION)
* Returns     : void
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
* Description : the last step of the algorithm presented in class
* Arguments   : 2 struct tablo (a and b) and the operation that needs to be
                done (SUM_OPERATION or MAX_OPERATION)
* Returns     : void
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
*                               do_up_down_final()
*
* Description : used to do all the algorithm presented in class from the
                begining to the end (up, down and final)
* Arguments   : 2 struct tablo (the source and the destination), an integer that
                represents the mode (SUFFIX_MODE or PREFIX_MODE) and an integer
                that represents the operation (SUM_OPERATION or MAX_OPERATION)
* Returns     : void
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
*                               findMaxTablo()
*
* Description : 5th step of the algorithm that (cf. TD)
* Arguments   : 5 struct tablo (the source, PSUM, SSUM, PMAX and SMAX)
* Returns     : the tablo of max M
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
*                               find_sequence()
*
* Description : display the final result of the algorithm (the max sum and the
                subsequence associated)
* Arguments   : 2 struct tablo (the source and the max)
* Returns     : void
********************************************************************************
*/

void find_sequence(struct tablo * src, struct tablo * M)
{
    int size_of_m = M->size;

    int max = INT_MIN;
    int start = 0;
    int end = 0;

    for (int i = 0; i < size_of_m; i++) {
      if (max < M->tab[i]) {
        start = i;
        end = i;
        max = M->tab[i];
      } else if (max == M->tab[i]) {
        end = i;
      }
    }

    printf("%d ", max);
    for (int i = start; i <= end; i++) {
      printf("%d ", src->tab[i]);
    }
    printf("\n");

}


/*
********************************************************************************
*                               generateArrayFromFile()
*
* Description : generate the array by giving a file as parameter
* Arguments   : a struct tablo (that will be the source) and a char* that
                represents the filename
* Returns     : void
********************************************************************************
*/

void generateArrayFromFile(struct tablo * s, char * filename)
{
  FILE *file = NULL;
  file = fopen(filename, "r");
  if (file != NULL)
  {

    int * numbers = {0};        /* array of integers deduced from the file */
    int current_number;         /* the current integer in process */

    fseek(file, 0, SEEK_END);           /* Go to the end of the file */
    long length_of_file = ftell(file);  /* Get the length of the file */
    fseek(file, 0, SEEK_SET);           /* Go the the begining of the file */

    numbers = malloc(sizeof(int)*length_of_file);

    int number_of_numbers = 0;

    // Read the file...
    while(!feof(file)) {
      // If we find a number, we add it in the array and modify the number of numbers
      if (fscanf(file, "%d", &current_number) == 1) {
        numbers[number_of_numbers++] = current_number;
      }
    }

    /* Update of the struct tablo given in parameter */
    s->size = number_of_numbers;
    s->tab = malloc(number_of_numbers*sizeof(int));
    for (int i = 0; i < number_of_numbers; i++) {
      s->tab[i] = numbers[i];
    }

    m = getLog2(number_of_numbers); /* size of source = n = pow(2, m) */

    fclose(file);
  }
  else
  {
    perror(filename);
  }
}

/*
********************************************************************************
*                               main()
*
* Description : the main method of the program
* Arguments   : argc and argv - classic
* Returns     : integer to say if the execution is done corectly or not
********************************************************************************
*/

int main(int argc, char **argv)
{

  struct tablo source;
  generateArrayFromFile(&source, argv[1]);

  struct tablo * PSUM = allocateTablo(source.size);
  do_up_down_final(&source, PSUM, PREFIX_MODE, SUM_OPERATION);

  struct tablo * SSUM = allocateTablo(source.size);
  do_up_down_final(&source, SSUM, SUFFIX_MODE, SUM_OPERATION);

  struct tablo * SMAX = allocateTablo(source.size);
  do_up_down_final(PSUM, SMAX, SUFFIX_MODE, MAX_OPERATION);

  struct tablo * PMAX = allocateTablo(source.size);
  do_up_down_final(SSUM, PMAX, PREFIX_MODE, MAX_OPERATION);

  struct tablo * M = findMaxTablo(&source, PSUM, SSUM, PMAX, SMAX);

  find_sequence(&source, M);

  return 0;

}
