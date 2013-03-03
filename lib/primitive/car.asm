/* scheme/car.asm
 * Take pointers to SOB PAIR, and place the corresponding 
 * car in R0
 * 
 * Programmer: T & L 2013
 */

 CAR:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  MOV(R0,INDD(R0,1));
  POP(FP);
  RETURN;

