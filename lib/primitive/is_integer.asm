/* primitive/is_integer.asm
 * Take pointers to a Scheme object, and places in R0 either #t or #f
 * (long, not Scheme integer objects or Scheme boolean objets),
 * depending on whether the argument is integer.
 * 
 * Programmer: T & l 2013
 */

 IS_INTEGER:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(IND(R0), T_INTEGER);
  JUMP_EQ(L_IS_INTEGER_TRUE);
  MOV(R0, IMM(12));
  JUMP(L_IS_INTEGER_EXIT);
 L_IS_INTEGER_TRUE:
  MOV(R0, IMM(14));
 L_IS_INTEGER_EXIT:
  POP(FP);
  RETURN;


