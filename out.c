/* cisc.c
 * Mock-assembly programming for a CISC-like architecture
 * 
 * Programmer: Mayer Goldberg, 2010
 */

#include <stdio.h>
#include <stdlib.h>

#include "cisc.h"

/* change to 0 for no debug info to be printed: */
#define DO_SHOW 1

/* for debugging only, use SHOW("<some message>, <arg> */
#if DO_SHOW
#define SHOW(msg, x) { printf("%s %s = %ld\n", (msg), (#x), (x)); }
#else
#define SHOW(msg, x) {}
#endif



int main()
{
  START_MACHINE;

  void print_heap(){
	int i;
	printf("printing heap\n");
	for (i=ADDR(0); i>=0; i--){
		printf("	 element %d: ", i);
		SHOW(" ",ADDR(i));
		}
	}
	
  void print_stack(char* comment){
	int i;
	printf("printing stack, FP: %d SP: %d %s\n", (int)(FP), (int)(SP), comment);
	for(i=SP+5; i>=0; --i){
		if(SP == i){
			printf("SP ");
		}
		if(FP == i){
			printf("FP");
		}
		printf("\telement %d: ", i);
			SHOW(" ", STACK(i));
		}
}


  JUMP(CONTINUE);

#include "scheme.lib"
#include "char.lib"
#include "io.lib"
#include "math.lib"
#include "string.lib"
#include "system.lib"
#include "primitive.lib"

 CONTINUE:
  /* initial the stack */
  PUSH(IMM(0));
  PUSH(IMM(T_NIL));
  PUSH(&&END);
  PUSH(FP);
  MOV(FP,SP);
	
 /* initial the heap */
  PUSH(IMM(9));
  CALL(MALLOC);
  CALL(MAKE_SOB_VOID);
  CALL(MAKE_SOB_NIL);
  PUSH(IMM(0));
  CALL(MAKE_SOB_BOOL);
  PUSH(IMM(1));
  CALL(MAKE_SOB_BOOL);
  DROP(3);
  
 /* initial constants-list*/
 /*finish initiate constants-list*/
	MOV(R0,14);
	PUSH(R0);
	CALL(SCHEME_NOT);
	DROP(1);
 END:
	print_heap();
	print_stack("no comment");
	PUSH(R0);
	CALL(WRITE_SOB);
	DROP(IMM(1));
	
  STOP_MACHINE;

  return 0;
}
