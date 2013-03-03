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
	PUSH(IMM(8));
	CALL(MAKE_SOB_INTEGER);
	DROP(1);
	PUSH(IMM(5));
	CALL(MAKE_SOB_INTEGER);
	DROP(1);
	PUSH(IMM(4));
	CALL(MAKE_SOB_INTEGER);
	DROP(1);
 /*finish initiate constants-list*/
		/* start applic*/
	MOV(R0,IMM(16));
	PUSH(R0);
	MOV(R0,IMM(18));
	PUSH(R0);
	PUSH(2);

		/* start lambda-simple*/
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
  L_ENV_LOOP_5: 
	CMP(R4,R3);
	JUMP_EQ(  L_ENV_LOOP_END_4 );
	MOV(INDD(INDD(R1,1),R5),INDD(FPARG(0),R4));
	ADD(R4,IMM(1));
	ADD(R5,IMM(1));
	JUMP(  L_ENV_LOOP_5 );
		/* ends extend env loop */
  L_ENV_LOOP_END_4: 
	PUSH(FPARG(1));
	CALL(MALLOC);
	DROP(1);
	MOV(R2,INDD(R1,1));
	MOV(INDD(R2,0),R0);
		/* starts extend env[0] with params loop */
	MOV(R4,IMM(0));
  L_PARAMS_LOOP_3: 
	CMP(R4,FPARG(1));
	JUMP_EQ(  L_PARAMS_LOOP_END_2 );
	MOV(R5,R4);
	ADD(R5,IMM(2));
	MOV(INDD(INDD(R2,IMM(0)),R4),FPARG(R5));
	ADD(R4,IMM(1));
	JUMP(  L_PARAMS_LOOP_3 );
		/* ends extend env[0] with param loop */
  L_PARAMS_LOOP_END_2: 
	MOV(INDD(R1,IMM(2)),&& L_CLOS_CODE_7 );
	MOV(R0,R1);
	JUMP( L_CLOS_EXIT_6 );
  L_CLOS_CODE_7: 
	PUSH(FP);
	MOV(FP,SP);
		 /* start code-gen body */ 
		/* start if*/
		/* start primitive applic*/
		/* start pvar*/
	MOV(R0,FPARG(3));
	PUSH(R0);
		/* start pvar*/
	MOV(R0,FPARG(2));
	PUSH(R0);
	CALL(SCHEME_GT);
	DROP(2);
	CMP(INDD(R0,1), INDD(12,1));
	JUMP_EQ( L_DIF_8 );
	MOV(R0,IMM(20));
	JUMP( L_IF_EXIT_9 );

 L_DIF_8: 
	MOV(R0,12);

 L_IF_EXIT_9: 
		 /* finish code-gen body and finishing lambda */ 
	POP(FP);
	RETURN;
  L_CLOS_EXIT_6: 
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
