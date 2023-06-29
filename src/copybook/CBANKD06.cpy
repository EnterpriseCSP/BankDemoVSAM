000100***************************************************************** cbankd06
000200*                                                               * cbankd06
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankd06
000400*                                                               * cbankd06
000500***************************************************************** cbankd06
000600                                                                  cbankd06
000700***************************************************************** cbankd06
000800* CBANKD06.CPY                                                  * cbankd06
000900*---------------------------------------------------------------* cbankd06
001000* This area is used to pass data between a requesting program   * cbankd06
001100* and the I/O program (DBANK06P) which inserts transaction      * cbankd06
001200* records to provide an audit trail.                            * cbankd06
001300***************************************************************** cbankd06
001400   05  CD06-DATA.                                                 cbankd06
001500     10  CD06I-DATA.                                              cbankd06
001600       15  CD06I-TIMESTAMP                   PIC X(26).           cbankd06
001700       15  CD06I-FROM-PID                    PIC X(5).            cbankd06
001800       15  CD06I-FROM-ACC                    PIC X(9).            cbankd06
001900       15  CD06I-FROM-AMOUNT                 PIC S9(7)V99 COMP-3. cbankd06
002000       15  CD06I-FROM-DESC                   PIC X(30).           cbankd06
002100       15  CD06I-TO-PID                      PIC X(5).            cbankd06
002200       15  CD06I-TO-ACC                      PIC X(9).            cbankd06
002300       15  CD06I-TO-AMOUNT                   PIC S9(7)V99 COMP-3. cbankd06
002400       15  CD06I-TO-DESC                     PIC X(30).           cbankd06
002500     10  CD06O-DATA.                                              cbankd06
002600       15  CD06O-RESULT                      PIC X(1).            cbankd06
002700         88  CD06O-UPDATE-OK                 VALUE '0'.           cbankd06
002800         88  CD06O-UPDATE-FAIL               VALUE '1'.           cbankd06
002900       15  CD06O-MSG                         PIC X(62).           cbankd06
003000                                                                  cbankd06
003100* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankd06
