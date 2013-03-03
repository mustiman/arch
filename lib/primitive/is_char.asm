/* primitive/char.asm
 * Take pointers to a Scheme object, and places in R0 either #t or #f
 * (long, not Scheme integer objects or Scheme boolean objets),
 * depending on whether the argument is char.
 * 
 * Programmer: T & L 2013
 */

 IS_CHAR:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(IND(R0), T_CHAR);
  JUMP_EQ(L_IS_CHAR_TRUE);
  MOV(R0, IMM(12));
  JUMP(L_IS_CHAR_EXIT);
 L_IS_CHAR_TRUE:
  MOV(R0, IMM(14));
 L_IS_CHAR_EXIT:
  POP(FP);
  RETURN;

