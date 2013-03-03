/* scheme/cdr.asm
 * Take pointers to SOB PAIR, and place the corresponding 
 * cdr in R0
 * 
 * Programmer: T & L 2013
 */

 CDR:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  MOV(R0,INDD(R0,2));
  POP(FP);
  RETURN;

