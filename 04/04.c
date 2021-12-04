#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>

typedef unsigned int hptr_t;

unsigned int heap[65536] = {0};
hptr_t hptr = 0;

unsigned int heap_mark[65536] = {0};

typedef unsigned int array;

#define dim0(a) (heap[(a)])
#define dim1(a) (heap[(a)+1])
#define data_idx(a) ((a)+2)
#define at(a, x, y) (heap[data_idx(a) + (y) * dim0(a) + (x)])
#define mark_offset(a, o) (heap_mark[data_idx(a) + (o)])
#define mark_xy(a, x, y) mark_offset(a, (y) * dim0(a) + (x))


void display(array a)
{
  int i, j;
  unsigned int d0 = dim0(a), d1 = dim1(a);
  printf("(%u, %u)\n", d0, d1);
  for(j = 0; j < d1; j++) {
    for(i = 0; i < d0; i++) {
      /*printf("%2u ", heap[data_idx(a) + j * dim0(a) + i]);*/
      printf("%2u", at(a, i, j));
      if(mark_xy(a, i, j))
	printf("* ");
      else
	printf("  ");
    }
    printf("\n");
  }
  printf("\n");
}

int check(array a)
{
  int i, j, result;
  for(j = 0; j < dim1(a); j++) {
    result = 1;
    for(i = 0; i < dim0(a); i++) {
      if(!mark_xy(a, i, j))
	result = 0;
    }
    if(result)
      return 1;
  }

  for(i = 0; i < dim0(a); i++) {
    result = 1;
    for(j = 0; j < dim1(a); j++) {
      if(!mark_xy(a, i, j))
	 result = 0;
    }
    if(result)
      return 1;
  }
 
  return 0;
}

void dump_heap()
{
  int i;
  printf("heap:\n");
  for(i=0; i< hptr; i++)
    printf("%u ", heap[i]);
  printf("\n");
}

void dump_array(array a)
{
  int i;
  printf("array:\n");
  for(i = 0; i < dim0(a) * dim1(a); i++)
    printf("%u ", heap[data_idx(a)+i]);
  printf("\n");
}

int main()
{
  array a[1000] = {0};
  unsigned int acnt = 0, x, y, i, j, k, l, m, score;
  char c;

  
  a[acnt] = hptr;
  hptr+=2;
  x = 0; y=0;
  do {
    scanf("%u", &heap[hptr++]);
    x++;
  } while((c = getchar()) == ',');
  dim0(a[acnt]) = x;
  dim1(a[acnt]) = 1;
  acnt++;

  while(!feof(stdin)) {
    while(!isdigit(c = getchar()) && !feof(stdin));
    if(feof(stdin))
      break;
    ungetc(c, stdin);
    a[acnt] = hptr;
    hptr+=2;
    y=0;
    do {
      x = 0;
      do {
	scanf("%u", &heap[hptr++]);
	x++;
      } while((c=getchar()) == ' ');
      if(y == 0) {
;
	dim0(a[acnt]) = x;
      }
      if(c != -1)
	y++;
      if(c != '\n') {
	if(c == -1)
	  break;
	fprintf(stderr, "Invalid input\n");
	exit(1);
      }
      if((c=getchar()) == '\n')
	break;
      else
	ungetc(c, stdin);
    } while(1);
    dim1(a[acnt]) = y;
    acnt++;

    
  }
  for(i =0; i < acnt; i++)
    display(a[i]);
  /*dump_array(a[3]);
    dump_heap();*/

  
  for(i = 0; i < dim0(a[0]); i++) {
    /*printf("turn number %u: %u\n", i, at(a[0], i, 0));*/
    for(j = 1; j < acnt; j++) {
      for(k = 0; k < dim1(a[j]); k++) {
	for(l = 0; l < dim0(a[j]); l++) {
	  if(at(a[j], k, l) == at(a[0], i, 0)) {
	    mark_xy(a[j], k, l) = 1;
	  }
	}
      }
      if(check(a[j]))
	goto win;
    }
    /*for(m =0; m < acnt; m++)
      display(a[m]);*/
    /* sleep(1); */
  }

  fprintf(stderr, "Nobody wins :(");
  exit(1);
 win:
  printf("board %u wins after %u turns. Last number : %u\n", j, i, at(a[0], i, 0));
  display(a[j]);

  score = 0;
  for(k = 0; k < dim1(a[j]); k++)
    for(l = 0; l < dim0(a[j]); l++)
      if(!mark_xy(a[j], k, l))
	score += at(a[j], k, l);

  score *= at(a[0], i, 0);

  printf("score : %u\n", score);

  
  return 0;
}
