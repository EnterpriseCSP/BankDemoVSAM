000100***************************************************************** cbankd04
000200*                                                               * cbankd04
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankd04
000400*                                                               * cbankd04
000500***************************************************************** cbankd04
000600                                                                  cbankd04
000700***************************************************************** cbankd04
000800* CBANKD04.CPY                                                  * cbankd04
000900*---------------------------------------------------------------* cbankd04
001000* This area is used to pass data between a requesting program   * cbankd04
001100* and the I/O program (DBANK04P) which updates account          * cbankd04
001200* information.                                                  * cbankd04
001300***************************************************************** cbankd04
001400   05  CD04-DATA.                                                 cbankd04
001500     10  CD04I-DATA.                                              cbankd04
001600       15  CD04I-FROM-PID                    PIC X(5).            cbankd04
001700       15  CD04I-FROM-ACC                    PIC X(9).            cbankd04
001800       15  CD04I-FROM-OLD-BAL                PIC S9(7)V99 COMP-3. cbankd04
001900       15  CD04I-FROM-NEW-BAL                PIC S9(7)V99 COMP-3. cbankd04
002000       15  CD04I-TO-PID                      PIC X(5).            cbankd04
002100       15  CD04I-TO-ACC                      PIC X(9).            cbankd04
002200       15  CD04I-TO-OLD-BAL                  PIC S9(7)V99 COMP-3. cbankd04
002300       15  CD04I-TO-NEW-BAL                  PIC S9(7)V99 COMP-3. cbankd04
002400     10  CD04O-DATA.                                              cbankd04
002500       15  CD04O-RESULT                      PIC X(1).            cbankd04
002600         88  CD04O-UPDATE-OK                 VALUE '0'.           cbankd04
002700         88  CD04O-UPDATE-FAIL               VALUE '1'.           cbankd04
002800       15  CD04O-TIMESTAMP                   PIC X(26).           cbankd04
002900       15  CD04O-TIMESTAMP-R REDEFINES CD04O-TIMESTAMP.           cbankd04
003000         20  CD04O-DATE                      PIC X(10).           cbankd04
003100         20  CD040-FIL1                      PIC X(1).            cbankd04
003200         20  CD04O-TIME                      PIC X(8).            cbankd04
003300         20  CD040-FIL2                      PIC X(1).            cbankd04
003400         20  CD040-MSEC                      PIC X(6).            cbankd04
003500       15  CD04O-MSG                         PIC X(62).           cbankd04
003600                                                                  cbankd04
003700* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankd04
