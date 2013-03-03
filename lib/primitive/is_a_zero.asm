/* primitive/is_a_zero.asm
 * Take pointers to a Scheme object, and places in R0 either #f or #t
 * depending on whether the argument is a zero.
 * 
 * Programmer: T & l 2013
 */

 IS_A_ZERO:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  PUSH(R0);
  CALL(IS_SOB_INTEGER);
  DROP(1);
  CMP(R0,IMM(1));
  JUMP_EQ(L_IS_A_ZERO_INTEGER);
  MOV(R0, IMM(12));
  JUMP(L_IS_A_ZERO_EXIT);
  L_IS_A_ZERO_INTEGER:
  MOV(R0, FPARG(0));
  CMP(INDD(R0,1), IMM(0));
  JUMP_EQ(L_IS_A_ZERO_TRUE);
  MOV(R0, IMM(12));
  JUMP(L_IS_A_ZERO_EXIT);
 L_IS_A_ZERO_TRUE:
  MOV(R0, IMM(14));
 L_IS_A_ZERO_EXIT:
  POP(FP);
  RETURN;


