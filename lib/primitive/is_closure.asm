/* primitive/is_closure.asm
 * Take pointers to a Scheme object, and places in R0 either #f or #t
 * (long, not Scheme integer objects or Scheme boolean objets),
 * depending on whether the argument is a closure.
 * 
 * Programmer: T & l 2013
 */

 IS_CLOSURE:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(IND(R0), T_CLOSURE);
  JUMP_EQ(L_IS_CLOSURE_TRUE);
  MOV(R0, IMM(12));
  JUMP(L_IS_CLOSURE_EXIT);
 L_IS_CLOSURE_TRUE:
  MOV(R0, IMM(14));
 L_IS_CLOSURE_EXIT:
  POP(FP);
  RETURN;


