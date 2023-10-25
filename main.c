#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/queue.h>


#define MY_TRUE 0xffffffff
#define MY_FALSE 0x7fffffff


extern int our_code_starts_here() asm("our_code_starts_here");
extern int print(int val) asm("print");
extern void error(int val) asm("error");
extern int  input(int i)   asm("input");

extern int* HEAP_END     asm("HEAP_END");
extern int* STACK_BOTTOM asm("STACK_BOTTOM");



// -------------------------------------------
// Global variables used in garbage collection
// -------------------------------------------
size_t HEAP_SIZE;
int* STACK_BOTTOM;
int* HEAP;
int* HEAP_END;
// -------------------------------------------



int  INPUT_COUNT = 0;
int* INPUTS      = NULL;

const long int INT_MIN = - (1 << 30);
const long int INT_MAX = (1 << 30) - 1;

void error(int error_code) {
  
  if(error_code == 1)
    fprintf(stderr, "Expected a number");
  else if(error_code == 2)
    fprintf(stderr, "Expected a boolean");
  else if (error_code == 3)
    fprintf(stderr, "Overflow! due to last operation");
  else if (error_code == 4)
      fprintf(stderr, "Not a Tuple");
  else if (error_code == 5)
      fprintf(stderr, "index too Large");
  else if (error_code == 6)
      fprintf(stderr, "index too Small");


  exit(123456);
}

int print(int val) {
  if(val & 0x00000001 ^ 0x00000001) {
    printf("%d\n", val >> 1);
  }
  else if(val == 0xFFFFFFFF) {
    printf("true\n");
  }
  else if(val == 0x7FFFFFFF) {
    printf("false\n");
  }
  else {
    printf("Unknown value: %#010x\n", val);
  }
  return val;
}

int input(int i) {
  i = i >> 1;

  if (i < 0 || i >= INPUT_COUNT) {
    fprintf(stderr, "input index out of bounds (given:%d #args:%d) \n", i, INPUT_COUNT);
    exit(1);
  }
  
  return INPUTS[i];
}

int parse_input(const char* in) {
  if (strcmp(in, "true") == 0) {
    return MY_TRUE;

  } else if (strcmp(in, "false") == 0) {
    return MY_FALSE;

  } else {
    size_t l = strlen(in);
    if (l == 0) {
      fprintf(stderr, "input is empty\n");
      exit(1);
    }
      
    char* endptr = (char*) &in[l];
    long int r = strtol(in, &endptr, 10);
    
    if (*endptr != '\0') {
      fprintf(stderr, "input '%s' is not a number or a boolean\n", in);
      exit(1);
    }

    if (r < INT_MIN || r > INT_MAX) {
      fprintf(stderr, "input '%s' is not a representable number\n", in);
      exit(1);
    }
      
    return (int) r * 2;
  }
}




int main(int argc, char** argv) {

  if(argc > 1) {
    HEAP_SIZE = atoi(argv[1]);
  } else {
    HEAP_SIZE = 100000;
  }

  int* HEAP = calloc(HEAP_SIZE, sizeof (int));

   if (HEAP == NULL) {
    fprintf(stderr, "HEAP is null\n");
    exit(1);
  } else if (((int) HEAP) & 0x3) {
    fprintf(stderr, "last 2 bits of HEAP is not 0 !\n");
    exit(1);
  }

  INPUT_COUNT = argc > 2 ? argc - 2 : 0;

  if (INPUT_COUNT > 0) {
      INPUTS = calloc(INPUT_COUNT, sizeof(int));

      int i = 0;
      for (; i < argc - 2; i++) {
        INPUTS[i] = parse_input(argv[i+2]);
      }
    }

  
  HEAP_END = HEAP + HEAP_SIZE;


  int result = (our_code_starts_here(HEAP));

  if((result&1) == 0)
  {
    // that means it is a number
    printf("%d\n", result/2);
  }
  else
  {
    // that means it is a bool
    if(result == MY_TRUE)
    {
      printf("true");
    } else if (result == MY_FALSE)
    {
      printf("false");
    } else
    {
      if((result&3) == 1)
        printf("It's a tuple, %X",(result - 1));
      else
        printf("error while dealing with runtime value");
    }
    
  }
    
  return 0;
}