000100***************************************************************** cbankd11
000200*                                                               * cbankd11
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankd11
000400*                                                               * cbankd11
000500***************************************************************** cbankd11
000600                                                                  cbankd11
000700***************************************************************** cbankd11
000800* CBANKD11.CPY                                                  * cbankd11
000900*---------------------------------------------------------------* cbankd11
001000* This area is used to pass data between a requesting program   * cbankd11
001100* and the I/O program (DBANK11P) which retrieves information    * cbankd11
001200* regarding customer's accounts                                 * cbankd11
001300***************************************************************** cbankd11
001400   05  CD11-DATA.                                                 cbankd11
001500     10  CD11I-DATA.                                              cbankd11
001600       15  CD11I-ACCNO                       PIC X(9).            cbankd11
001700     10  CD11O-DATA.                                              cbankd11
001800       15  CD11O-ACCNO                       PIC X(9).            cbankd11
001900       15  CD11O-DESC                        PIC X(15).           cbankd11
002000       15  CD11O-BAL                         PIC X(9).            cbankd11
002100       15  CD11O-BAL-N REDEFINES CD11O-BAL   PIC S9(7)V99.        cbankd11
002200       15  CD11O-DTE                         PIC X(10).           cbankd11
002300       15  CD11O-TRANS                       PIC X(5).            cbankd11
002400       15  CD11O-ATM-ENABLED                 PIC X(1).            cbankd11
002500       15  CD11O-ATM-LIM                     PIC X(3).            cbankd11
002600       15  CD11O-ATM-LIM-N REDEFINES CD11O-ATM-LIM                cbankd11
002700                                             PIC 9(3).            cbankd11
002800       15  CD11O-ATM-LDTE                    PIC X(10).           cbankd11
002900       15  CD11O-ATM-LAMT                    PIC X(3).            cbankd11
003000       15  CD11O-ATM-LAMT-N REDEFINES CD11O-ATM-LAMT              cbankd11
003100                                             PIC 9(3).            cbankd11
003200       15  CD11O-RP1DAY                      PIC X(2).            cbankd11
003300       15  CD11O-RP1AMT                      PIC X(7).            cbankd11
003400       15  CD11O-RP1AMT-N REDEFINES CD11O-RP1AMT                  cbankd11
003500                                             PIC S9(5)V99.        cbankd11
003600       15  CD11O-RP1PID                      PIC X(5).            cbankd11
003700       15  CD11O-RP1ACC                      PIC X(9).            cbankd11
003800       15  CD11O-RP1DTE                      PIC X(10).           cbankd11
003900       15  CD11O-RP2DAY                      PIC X(2).            cbankd11
004000       15  CD11O-RP2AMT                      PIC X(7).            cbankd11
004100       15  CD11O-RP2AMT-N REDEFINES CD11O-RP2AMT                  cbankd11
004200                                             PIC S9(5)V99.        cbankd11
004300       15  CD11O-RP2PID                      PIC X(5).            cbankd11
004400       15  CD11O-RP2ACC                      PIC X(9).            cbankd11
004500       15  CD11O-RP2DTE                      PIC X(10).           cbankd11
004600       15  CD11O-RP3DAY                      PIC X(2).            cbankd11
004700       15  CD11O-RP3AMT                      PIC X(7).            cbankd11
004800       15  CD11O-RP3AMT-N REDEFINES CD11O-RP3AMT                  cbankd11
004900                                             PIC S9(5)V99.        cbankd11
005000       15  CD11O-RP3PID                      PIC X(5).            cbankd11
005100       15  CD11O-RP3ACC                      PIC X(9).            cbankd11
005200       15  CD11O-RP3DTE                      PIC X(10).           cbankd11
005300                                                                  cbankd11
005400* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankd11
