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
	PUSH(0);

		/* start lambda-var*/
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
		/* starts extend env loop */
	MOV(R4,IMM(0));
	MOV(R5,IMM(1));
	SUB(R3,IMM(1));
  L_ENV_LOOP_11: 
	CMP(R4,R3);
	JUMP_EQ(  L_ENV_LOOP_END_10 );
	MOV(INDD(INDD(R1,1),R5),INDD(FPARG(0),R4));
	ADD(R4,IMM(1));
	ADD(R5,IMM(1));
	JUMP(  L_ENV_LOOP_11 );
		/* ends extend env loop */
  L_ENV_LOOP_END_10: 
	PUSH(FPARG(1));
	CALL(MALLOC);
	DROP(1);
	MOV(R2,INDD(R1,1));
	MOV(INDD(R2,0),R0);
		/* starts extend env[0] with params loop */
	MOV(R4,IMM(0));
  L_PARAMS_LOOP_9: 
	CMP(R4,FPARG(1));
	JUMP_EQ(  L_PARAMS_LOOP_END_8 );
	MOV(R5,R4);
	ADD(R5,IMM(2));
	MOV(INDD(INDD(R2,IMM(0)),R4),FPARG(R5));
	ADD(R4,IMM(1));
	JUMP(  L_PARAMS_LOOP_9 );
		/* ends extend env[0] with param loop */
  L_PARAMS_LOOP_END_8: 
	MOV(INDD(R1,IMM(2)),&& L_CLOS_VAR_CODE_13 );
	MOV(R0,R1);
	JUMP( L_CLOS_VAR_EXIT_12 );
  L_CLOS_VAR_CODE_13: 
	PUSH(FP);
	MOV(FP,SP);
printf(" 0- %d \n 1- %d \n 2- %d \n",FPARG(0),FPARG(1),FPARG(2));
	MOV(R6,FPARG(1));
	MOV(FPARG(1), IMM(1));
	MOV(R0,IMM(11));
		/* starts create pairs loop */
	MOV(R9,R6);
  L_PAIR_LOOP_START_7: 
	CMP(R9,IMM(0));
	JUMP_EQ(  L_PAIR_LOOP_END_6 );
	PUSH(R0);
	MOV(R7,R9);
	ADD(R7,IMM(1));
	PUSH(FPARG(R7));
	CALL(MAKE_SOB_PAIR);
	DROP(2);
	SUB(R9,IMM(1));
	JUMP(  L_PAIR_LOOP_START_7 );
		/* ends create pairs loop */
  L_PAIR_LOOP_END_6: 
	MOV(R7,R0);
	MOV(R8,SP);
	MOV(R11,R6);
	SUB(R6,IMM(1));
	SUB(R8,R6);
		/* starts fixing the stack */
	CMP(R6,IMM(-1));
	JUMP_EQ(  L_FALSE_START_3 );
		/* starts copying down loop */
	MOV(R12,IMM(2));
	MOV(R13,R11);
	ADD(R13,IMM(1));
  L_TRUE_LOOP_START_5: 
	CMP(R12,IMM(-3));
	JUMP_EQ(  L_FIX_LOOP_END_4 );
	MOV(FPARG(R13),FPARG(R12));
	SUB(R12,IMM(1));
	SUB(R13,IMM(1));
	JUMP(  L_TRUE_LOOP_START_5 );
  L_FALSE_START_3: 
		/* starts copying up loop */
	MOV(R12,IMM(-2));
	MOV(R13,IMM(2));
  L_FALSE_LOOP_START_2: 
	CMP(R12,R13);
	JUMP_EQ(  L_FIX_LOOP_END_4 );
	MOV(R14,R12);
	SUB(R14,IMM(1));
	MOV(FPARG(R14),FPARG(R12));
	ADD(R12,IMM(1));
	JUMP(  L_FALSE_LOOP_START_2 );
		/* finish fixing the stack */
  L_FIX_LOOP_END_4: 
	MOV(SP,R8);
	MOV(FP,SP);
	MOV(FPARG(IMM(2)),R7);
		 /* start code-gen body (in lambda var) */ 
		/* start pvar*/
	MOV(R0,FPARG(2));
		 /* finish code-gen body and finishing lambda-var */ 
	POP(FP);
	RETURN;
  L_CLOS_VAR_EXIT_12: 
	CMP(IND(R0), T_CLOSURE);
	JUMP_NE ( L_APPLIC_ERROR_NOT_A_CLOS_1) ;
	PUSH(INDD(R0,1));
	CALLA(INDD(R0,2));
	MOV(R3,STARG(0));
	ADD(R3,IMM(2));
	DROP(R3);
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
