/* primitive/is_symbol.asm
 * Take pointers to a Scheme object, and places in R0 either #f or #t
 * depending on whether the argument is a symbol.
 * 
 * Programmer: T & l 2013
 */

 IS_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(IND(R0), T_SYMBOL);
  JUMP_EQ(L_IS_SYMBOL_TRUE);
  MOV(R0, IMM(12));
  JUMP(L_IS_SYMBOL_EXIT);
 L_IS_SYMBOL_TRUE:
  MOV(R0, IMM(14));
 L_IS_SYMBOL_EXIT:
  POP(FP);
  RETURN;


