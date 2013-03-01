/* scheme/not.asm
 * not primitive
 * 
 * Programmer: T & L
 */

 SCHEME_NOT:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(IND(R0), T_BOOL);
  JUMP_EQ(L_NOT_BOOLEAN);
  MOV(R0, IMM(12));
  JUMP(L_NOT_EXIT);
 L_NOT_BOOLEAN:
  CMP(INDD(R0,1),IMM(0));
  JUMP_EQ(L_NOT_BOOLEAN_FALSE);
  MOV(R0,IMM(12));
  JUMP(L_NOT_EXIT);
 L_NOT_BOOLEAN_FALSE:
  MOV(R0,IMM(14));
 L_NOT_EXIT:
  POP(FP);
  RETURN;


