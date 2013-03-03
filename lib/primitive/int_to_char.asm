/* char/int_to_char.asm
 * return the SOB Integer of the ascii number of a SOB char
 *
 * Programmer: T & L 2013
 */

 INT_TO_CHAR:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  PUSH(INDD(R0,1));
  CALL(MAKE_SOB_CHAR);
  DROP(1);
  POP(FP);
  RETURN;
