/* primitive/is_string.asm
 * Take pointers to a Scheme object, and places in R0 either #f or #t
 * depending on whether the argument is a string.
 * 
 * Programmer: T & l 2013
 */

 IS_STRING:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(IND(R0), T_STRING);
  JUMP_EQ(L_IS_STRING_TRUE);
  MOV(R0, IMM(12));
  JUMP(L_IS_STRING_EXIT);
 L_IS_STRING_TRUE:
  MOV(R0, IMM(14));
 L_IS_STRING_EXIT:
  POP(FP);
  RETURN;


