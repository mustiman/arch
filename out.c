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

 int i,j;		/* start applic*/
	MOV(R0,12);
	PUSH(R0);
	MOV(R0,12);
	PUSH(R0);
	MOV(R0,14);
	PUSH(R0);
	PUSH(3);

		/* start lambda-opt*/
	MOV(R3,IMM(1));
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(R1,R0);
	MOV(IND(R1),IMM(T_CLOSURE));
	PUSH(R3);
	CALL(MALLOC);
	DROP(1);
	MOV(INDD(R1,1),R0);
	
								for(i=0, j=1; i < R3-1;j++,i++){
									MOV(INDD(INDD(R1,1),j),INDD(FPARG(0),i));
							} 
	PUSH(FPARG(1));
	CALL(MALLOC);
	DROP(1);
	MOV(R2,INDD(R1,1));
	MOV(INDD(R2,0),R0);
	for(i=0; i < FPARG(1);i++){
									MOV(INDD(INDD(R2,0),i),FPARG(i+2));
								} 
	MOV(INDD(R1,2),&& L_CLOS_OPT_CODE_2 );
	MOV(R0,R1);
	JUMP( L_CLOS_OPT_EXIT_3 );
  L_CLOS_OPT_CODE_2: 
	PUSH(FP);
	MOV(FP,SP);
printf(" 0- %d \n 1- %d \n 2- %d \n",FPARG(0),FPARG(1),FPARG(2));
	MOV(R6,FPARG(1));
	MOV(FPARG(1), IMM(2));
	MOV(R0,IMM(11));
for(i=R6; i > 1; i--){
									PUSH(R0);
									MOV(R7,R6);
									ADD(R7,IMM(2));
									PUSH(FPARG(R7));
									CALL(MAKE_SOB_PAIR);
									DROP(2);
							} 
	MOV(R7,R0);
	MOV(R8,SP);
	SUB(R6, IMM(1));
	SUB(R6,IMM(1));
	SUB(R8,R6);
printf("%d\n",R8);
print_heap();print_stack("in optional");
		 /* start code-gen body (in lambda opt) */ 
		/* start pvar*/
	MOV(R0,FPARG(3));
		 /* finish code-gen body and finishing lambda-opt */ 
	POP(FP);
	RETURN;
  L_CLOS_OPT_EXIT_3: 
	CMP(IND(R0), T_CLOSURE);
	JUMP_NE ( L_APPLIC_ERROR_NOT_A_CLOS_1) ;
	PUSH(INDD(R0,1));
	CALLA(INDD(R0,2));
	DROP(5);
  L_APPLIC_ERROR_NOT_A_CLOS_1: 
 END:
	print_heap();
	print_stack("no comment");
	PUSH(R0);
	CALL(WRITE_SOB);
	DROP(IMM(1));
	
  STOP_MACHINE;

  return 0;
}
