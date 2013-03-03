/* scheme/smaller.asm
 * < primitive
 * 
 * Programmer: T & L 2013
 */

 SCHEME_LT:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  MOV(R0,INDD(R0,1));
  MOV(R1, FPARG(1));
  MOV(R1,INDD(R1,1));
  CMP(R0, R1);
  JUMP_LT(L_LT_TRUE);
  MOV(R0, IMM(12));
  JUMP(L_LT_EXIT);
 L_LT_TRUE:
  MOV(R0, IMM(14));
 L_LT_EXIT:
  POP(FP);
  RETURN;


