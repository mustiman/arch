/* primitive/sub.asm
 * Takes n Integer Scheme objects , reduce them 
 * and returns its result.
 * 
 * Programmer: T & l 2013
 */

 SCHEME_SUB:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  MOV(R0,INDD(R0,1));
  MOV(R1, FPARG(1));
  MOV(R1,INDD(R1,1));
  SUB(R0,R1);
  PUSH(R0);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  POP(FP);
  RETURN;
