/* primitive/is_vector.asm
 * Take pointers to a Scheme object, and places in R0 either #f or #t
 * depending on whether the argument is a vecor.
 * 
 * Programmer: T & l 2013
 */

 IS_VECTOR:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(IND(R0), T_VECTOR);
  JUMP_EQ(L_IS_VECTOR_TRUE);
  MOV(R0, IMM(12));
  JUMP(L_IS_VECTOR_EXIT);
 L_IS_VECTOR_TRUE:
  MOV(R0, IMM(14));
 L_IS_VECTOR_EXIT:
  POP(FP);
  RETURN;


