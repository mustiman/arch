/* primitive/multi.asm
 * Takes n Integer Scheme objects ,multiply them 
 * and returns its result in R0.
 * 
 * Programmer: T & l 2013
 */

 SCHEME_MULTI:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  MOV(R0,INDD(R0,1));
  MOV(R1, FPARG(1));
  MOV(R1,INDD(R1,1));
  MUL(R0,R1);
  PUSH(R0);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  POP(FP);
  RETURN;
