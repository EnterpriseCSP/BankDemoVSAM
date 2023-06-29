000100***************************************************************** cbankd12
000200*                                                               * cbankd12
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankd12
000400*                                                               * cbankd12
000500***************************************************************** cbankd12
000600                                                                  cbankd12
000700***************************************************************** cbankd12
000800* CBANKD12.CPY                                                  * cbankd12
000900*---------------------------------------------------------------* cbankd12
001000* This area is used to pass data between a requesting program   * cbankd12
001100* and the I/O program (DBANK01P) which retrieves the customer   * cbankd12
001200* information.                                                  * cbankd12
001300***************************************************************** cbankd12
001400   05  CD12-DATA.                                                 cbankd12
001500     10  CD12I-DATA.                                              cbankd12
001600       15  CD12I-PERSON-PID                  PIC X(5).            cbankd12
001700     10  CD12O-DATA.                                              cbankd12
001800       15  CD12O-RESULT                      PIC X(1).            cbankd12
001900         88  CD12O-RESULT-OK                  VALUE '0'.          cbankd12
002000         88  CD12O-RESULT-FAIL                VALUE '1'.          cbankd12
002100                                                                  cbankd12
002200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankd12
