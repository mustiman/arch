/* primitive/string_length.asm
 * Take pointers to a Scheme object, and places in R0 either #t or #f
 * and returns its length.
 * 
 * Programmer: T & l 2013
 */

 STRING_LENGTH:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  MOV(R0,INDD(R0,1));
  PUSH(R0);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  POP(FP);
  RETURN;
