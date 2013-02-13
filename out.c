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
		/* start lambda-simple*/
	MOV(R3,IMM(2)); /* change R3 !!!*/
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
	printf("starg(-1) - %d, args -1",STARG(-1));

	CALL(MALLOC);
	DROP(1);
	MOV(R2,INDD(R1,1));
	MOV(INDD(R2,0),R0);
	printf("FPARG(1) - %d, args -1",FPARG(1));
				
	for(i=0; i < FPARG(1);i++){
		MOV(INDD(INDD(R2,0),i),FPARG(i+2));
	}

	MOV(INDD(R1,2),&& L_CLOS_CODE_30 );
	MOV(R0,R1);
	JUMP( L_CLOS_EXIT_31 );
  L_CLOS_CODE_30: 
	PUSH(FP);
	MOV(FP,SP);
		/* start lambda-simple*/
	MOV(R3,IMM(3)); /* change R3 !!!*/
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
	printf("starg(-1) - %d, args -1",STARG(-1));

	CALL(MALLOC);
	DROP(1);
	MOV(R2,INDD(R1,1));
	MOV(INDD(R2,0),R0);
	printf("FPARG(1) - %d, args -1",FPARG(1));
				
	for(i=0; i < FPARG(1);i++){
		MOV(INDD(INDD(R2,0),i),FPARG(i+2));
	}

	MOV(INDD(R1,2),&& L_CLOS_CODE_32 );
	MOV(R0,R1);
	JUMP( L_CLOS_EXIT_33 );
  L_CLOS_CODE_32: 
	PUSH(FP);
	MOV(FP,SP);
		/* start bvar*/
	MOV(R0, FPARG(0));
	MOV(R0, INDD(R0,1));
	MOV(R0, INDD(R0,0));
	POP(FP);
	RETURN;
  L_CLOS_EXIT_33: 
	POP(FP);
	RETURN;
  L_CLOS_EXIT_31: 
	PUSH(R0);
	PUSH(1);

		/* start applic*/
		/* start lambda-simple*/
	MOV(R3,IMM(2)); /* change R3 !!!*/
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
	printf("starg(-1) - %d, args -1",STARG(-1));

	CALL(MALLOC);
	DROP(1);
	MOV(R2,INDD(R1,1));
	MOV(INDD(R2,0),R0);
	printf("FPARG(1) - %d, args -1",FPARG(1));
				
	for(i=0; i < FPARG(1);i++){
		MOV(INDD(INDD(R2,0),i),FPARG(i+2));
	}

	MOV(INDD(R1,2),&& L_CLOS_CODE_26 );
	MOV(R0,R1);
	JUMP( L_CLOS_EXIT_27 );
  L_CLOS_CODE_26: 
	PUSH(FP);
	MOV(FP,SP);
		/* start applic*/
	MOV(R0,12);
	PUSH(R0);
	PUSH(1);

		/* start applic*/
	MOV(R0,14);
	PUSH(R0);
	PUSH(1);

		/* start pvar*/
	MOV(R0,FPARG(2));
	CMP(IND(R0), T_CLOSURE);
	JUMP_NE ( L_APPLIC_ERROR_NOT_A_CLOS_29) ;
	PUSH(INDD(R0,1));
	CALLA(INDD(R0,2));
	DROP(3);
  L_APPLIC_ERROR_NOT_A_CLOS_29: 
	CMP(IND(R0), T_CLOSURE);
	JUMP_NE ( L_APPLIC_ERROR_NOT_A_CLOS_28) ;
	PUSH(INDD(R0,1));
	CALLA(INDD(R0,2));
	DROP(3);
  L_APPLIC_ERROR_NOT_A_CLOS_28: 
	POP(FP);
	RETURN;
  L_CLOS_EXIT_27: 
	PUSH(R0);
	PUSH(1);

		/* start applic*/
		/* start lambda-simple*/
	MOV(R3,IMM(2)); /* change R3 !!!*/
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
	printf("starg(-1) - %d, args -1",STARG(-1));

	CALL(MALLOC);
	DROP(1);
	MOV(R2,INDD(R1,1));
	MOV(INDD(R2,0),R0);
	printf("FPARG(1) - %d, args -1",FPARG(1));
				
	for(i=0; i < FPARG(1);i++){
		MOV(INDD(INDD(R2,0),i),FPARG(i+2));
	}

	MOV(INDD(R1,2),&& L_CLOS_CODE_15 );
	MOV(R0,R1);
	JUMP( L_CLOS_EXIT_16 );
  L_CLOS_CODE_15: 
	PUSH(FP);
	MOV(FP,SP);
		/* start applic*/
		/* start lambda-simple*/
	MOV(R3,IMM(3)); /* change R3 !!!*/
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
	printf("starg(-1) - %d, args -1",STARG(-1));

	CALL(MALLOC);
	DROP(1);
	MOV(R2,INDD(R1,1));
	MOV(INDD(R2,0),R0);
	printf("FPARG(1) - %d, args -1",FPARG(1));
				
	for(i=0; i < FPARG(1);i++){
		MOV(INDD(INDD(R2,0),i),FPARG(i+2));
	}

	MOV(INDD(R1,2),&& L_CLOS_CODE_18 );
	MOV(R0,R1);
	JUMP( L_CLOS_EXIT_19 );
  L_CLOS_CODE_18: 
	PUSH(FP);
	MOV(FP,SP);
		/* start lambda-simple*/
	MOV(R3,IMM(4)); /* change R3 !!!*/
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
	printf("starg(-1) - %d, args -1",STARG(-1));

	CALL(MALLOC);
	DROP(1);
	MOV(R2,INDD(R1,1));
	MOV(INDD(R2,0),R0);
	printf("FPARG(1) - %d, args -1",FPARG(1));
				
	for(i=0; i < FPARG(1);i++){
		MOV(INDD(INDD(R2,0),i),FPARG(i+2));
	}

	MOV(INDD(R1,2),&& L_CLOS_CODE_20 );
	MOV(R0,R1);
	JUMP( L_CLOS_EXIT_21 );
  L_CLOS_CODE_20: 
	PUSH(FP);
	MOV(FP,SP);
		/* start lambda-simple*/
	MOV(R3,IMM(5)); /* change R3 !!!*/
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
	printf("starg(-1) - %d, args -1",STARG(-1));

	CALL(MALLOC);
	DROP(1);
	MOV(R2,INDD(R1,1));
	MOV(INDD(R2,0),R0);
	printf("FPARG(1) - %d, args -1",FPARG(1));
				
	for(i=0; i < FPARG(1);i++){
		MOV(INDD(INDD(R2,0),i),FPARG(i+2));
	}

	MOV(INDD(R1,2),&& L_CLOS_CODE_22 );
	MOV(R0,R1);
	JUMP( L_CLOS_EXIT_23 );
  L_CLOS_CODE_22: 
	PUSH(FP);
	MOV(FP,SP);
		/* start applic*/
		/* start bvar*/
	MOV(R0, FPARG(0));
	MOV(R0, INDD(R0,2));
	MOV(R0, INDD(R0,0));
	PUSH(R0);
	PUSH(1);

		/* start applic*/
		/* start bvar*/
	MOV(R0, FPARG(0));
	MOV(R0, INDD(R0,1));
	MOV(R0, INDD(R0,0));
	PUSH(R0);
	PUSH(1);

		/* start pvar*/
	MOV(R0,FPARG(2));
	CMP(IND(R0), T_CLOSURE);
	JUMP_NE ( L_APPLIC_ERROR_NOT_A_CLOS_25) ;
	PUSH(INDD(R0,1));
	CALLA(INDD(R0,2));
	DROP(3);
  L_APPLIC_ERROR_NOT_A_CLOS_25: 
	CMP(IND(R0), T_CLOSURE);
	JUMP_NE ( L_APPLIC_ERROR_NOT_A_CLOS_24) ;
	PUSH(INDD(R0,1));
	CALLA(INDD(R0,2));
	DROP(3);
  L_APPLIC_ERROR_NOT_A_CLOS_24: 
	POP(FP);
	RETURN;
  L_CLOS_EXIT_23: 
	POP(FP);
	RETURN;
  L_CLOS_EXIT_21: 
	POP(FP);
	RETURN;
  L_CLOS_EXIT_19: 
	PUSH(R0);
	PUSH(1);

		/* start pvar*/
	MOV(R0,FPARG(2));
	CMP(IND(R0), T_CLOSURE);
	JUMP_NE ( L_APPLIC_ERROR_NOT_A_CLOS_17) ;
	PUSH(INDD(R0,1));
	CALLA(INDD(R0,2));
	DROP(3);
  L_APPLIC_ERROR_NOT_A_CLOS_17: 
	POP(FP);
	RETURN;
  L_CLOS_EXIT_16: 
	PUSH(R0);
	PUSH(1);

		/* start applic*/
		/* start lambda-simple*/
	MOV(R3,IMM(2)); /* change R3 !!!*/
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
	printf("starg(-1) - %d, args -1",STARG(-1));

	CALL(MALLOC);
	DROP(1);
	MOV(R2,INDD(R1,1));
	MOV(INDD(R2,0),R0);
	printf("FPARG(1) - %d, args -1",FPARG(1));
				
	for(i=0; i < FPARG(1);i++){
		MOV(INDD(INDD(R2,0),i),FPARG(i+2));
	}

	MOV(INDD(R1,2),&& L_CLOS_CODE_9 );
	MOV(R0,R1);
	JUMP( L_CLOS_EXIT_10 );
  L_CLOS_CODE_9: 
	PUSH(FP);
	MOV(FP,SP);
		/* start lambda-simple*/
	MOV(R3,IMM(3)); /* change R3 !!!*/
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
	printf("starg(-1) - %d, args -1",STARG(-1));

	CALL(MALLOC);
	DROP(1);
	MOV(R2,INDD(R1,1));
	MOV(INDD(R2,0),R0);
	printf("FPARG(1) - %d, args -1",FPARG(1));
				
	for(i=0; i < FPARG(1);i++){
		MOV(INDD(INDD(R2,0),i),FPARG(i+2));
	}

	MOV(INDD(R1,2),&& L_CLOS_CODE_11 );
	MOV(R0,R1);
	JUMP( L_CLOS_EXIT_12 );
  L_CLOS_CODE_11: 
	PUSH(FP);
	MOV(FP,SP);
		/* start applic*/
		/* start applic*/
		/* start pvar*/
	MOV(R0,FPARG(2));
	PUSH(R0);
	PUSH(1);

		/* start bvar*/
	MOV(R0, FPARG(0));
	MOV(R0, INDD(R0,1));
	MOV(R0, INDD(R0,0));
	CMP(IND(R0), T_CLOSURE);
	JUMP_NE ( L_APPLIC_ERROR_NOT_A_CLOS_14) ;
	PUSH(INDD(R0,1));
	CALLA(INDD(R0,2));
	DROP(3);
  L_APPLIC_ERROR_NOT_A_CLOS_14: 
	PUSH(R0);
	PUSH(1);

		/* start bvar*/
	MOV(R0, FPARG(0));
	MOV(R0, INDD(R0,1));
	MOV(R0, INDD(R0,0));
	CMP(IND(R0), T_CLOSURE);
	JUMP_NE ( L_APPLIC_ERROR_NOT_A_CLOS_13) ;
	PUSH(INDD(R0,1));
	CALLA(INDD(R0,2));
	DROP(3);
  L_APPLIC_ERROR_NOT_A_CLOS_13: 
	POP(FP);
	RETURN;
  L_CLOS_EXIT_12: 
	POP(FP);
	RETURN;
  L_CLOS_EXIT_10: 
	PUSH(R0);
	PUSH(1);

		/* start lambda-simple*/
	MOV(R3,IMM(2)); /* change R3 !!!*/
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
	printf("starg(-1) - %d, args -1",STARG(-1));

	CALL(MALLOC);
	DROP(1);
	MOV(R2,INDD(R1,1));
	MOV(INDD(R2,0),R0);
	printf("FPARG(1) - %d, args -1",FPARG(1));
				
	for(i=0; i < FPARG(1);i++){
		MOV(INDD(INDD(R2,0),i),FPARG(i+2));
	}

	MOV(INDD(R1,2),&& L_CLOS_CODE_5 );
	MOV(R0,R1);
	JUMP( L_CLOS_EXIT_6 );
  L_CLOS_CODE_5: 
	PUSH(FP);
	MOV(FP,SP);
		/* start applic*/
		/* start applic*/
		/* start pvar*/
	MOV(R0,FPARG(2));
	PUSH(R0);
	PUSH(1);

		/* start pvar*/
	MOV(R0,FPARG(2));
	CMP(IND(R0), T_CLOSURE);
	JUMP_NE ( L_APPLIC_ERROR_NOT_A_CLOS_8) ;
	PUSH(INDD(R0,1));
	CALLA(INDD(R0,2));
	DROP(3);
  L_APPLIC_ERROR_NOT_A_CLOS_8: 
	PUSH(R0);
	PUSH(1);

		/* start pvar*/
	MOV(R0,FPARG(2));
	CMP(IND(R0), T_CLOSURE);
	JUMP_NE ( L_APPLIC_ERROR_NOT_A_CLOS_7) ;
	PUSH(INDD(R0,1));
	CALLA(INDD(R0,2));
	DROP(3);
  L_APPLIC_ERROR_NOT_A_CLOS_7: 
	POP(FP);
	RETURN;
  L_CLOS_EXIT_6: 
	CMP(IND(R0), T_CLOSURE);
	JUMP_NE ( L_APPLIC_ERROR_NOT_A_CLOS_4) ;
	PUSH(INDD(R0,1));
	CALLA(INDD(R0,2));
	DROP(3);
  L_APPLIC_ERROR_NOT_A_CLOS_4: 
	CMP(IND(R0), T_CLOSURE);
	JUMP_NE ( L_APPLIC_ERROR_NOT_A_CLOS_3) ;
	PUSH(INDD(R0,1));
	CALLA(INDD(R0,2));
	DROP(3);
  L_APPLIC_ERROR_NOT_A_CLOS_3: 
	CMP(IND(R0), T_CLOSURE);
	JUMP_NE ( L_APPLIC_ERROR_NOT_A_CLOS_2) ;
	PUSH(INDD(R0,1));
	CALLA(INDD(R0,2));
	DROP(3);
  L_APPLIC_ERROR_NOT_A_CLOS_2: 
	CMP(IND(R0), T_CLOSURE);
	JUMP_NE ( L_APPLIC_ERROR_NOT_A_CLOS_1) ;
	PUSH(INDD(R0,1));
	CALLA(INDD(R0,2));
	DROP(3);
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
