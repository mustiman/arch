/* scheme/grater.asm
 * > primitive
 * 
 * Programmer: T & L 2013
 */

 SCHEME_GT:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  MOV(R0,INDD(R0,1));
  MOV(R1, FPARG(1));
  MOV(R1,INDD(R1,1));
  CMP(R0, R1);
  JUMP_GT(L_GT_TRUE);
  MOV(R0, IMM(12));
  JUMP(L_GT_EXIT);
 L_GT_TRUE:
  MOV(R0, IMM(14));
 L_GT_EXIT:
  POP(FP);
  RETURN;


