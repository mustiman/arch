/* char/char_to_int.asm
 * return the ascii number of a char as SOB Ibteger
 *
 * Programmer: T & L 2013
 */

 CHAR_TO_INT:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  PUSH(INDD(R0,1));
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  POP(FP);
  RETURN;
