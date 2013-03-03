/* primitive/boolean.asm
 * Take pointers to a Scheme object, and places in R0 either #t or #f
 * (long, not Scheme integer objects or Scheme boolean objets),
 * depending on whether the argument is Boolean.
 * 
 * Programmer: T & L 2013
 */

 IS_BOOLEAN:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(IND(R0), T_BOOL);
  JUMP_EQ(L_IS_BOOLEAN_TRUE);
  MOV(R0, IMM(12));
  JUMP(L_IS_BOOLEAN_EXIT);
 L_IS_BOOLEAN_TRUE:
  MOV(R0, IMM(14));
 L_IS_BOOLEAN_EXIT:
  POP(FP);
  RETURN;
