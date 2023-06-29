000100***************************************************************** cbankd03
000200*                                                               * cbankd03
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankd03
000400*                                                               * cbankd03
000500***************************************************************** cbankd03
000600                                                                  cbankd03
000700***************************************************************** cbankd03
000800* CBANKD03.CPY          SHOW HIS TO RRD                         * cbankd03
000900*---------------------------------------------------------------* cbankd03
001000* This area is used to pass data between a requesting program   * cbankd03
001100* and the I/O program (DBANK03P) which retrieves information    * cbankd03
001200* regarding customer's accounts                                 * cbankd03
001300***************************************************************** cbankd03
001400   05  CD03-DATA.                                                 cbankd03
001500     10  CD03I-DATA.                                              cbankd03
001600       15  CD03I-CONTACT-ID                  PIC X(5).            cbankd03
001700     10  CD03O-DATA.                                              cbankd03
001800       15  CD03O-ACC1                        PIC X(9).            cbankd03
001900       15  CD03O-DSC1                        PIC X(15).           cbankd03
002000       15  CD03O-BAL1                        PIC X(9).            cbankd03
002100       15  CD03O-BAL1N REDEFINES CD03O-BAL1  PIC S9(7)V99.        cbankd03
002200       15  CD03O-DTE1                        PIC X(10).           cbankd03
002300       15  CD03O-TXN1                        PIC X(1).            cbankd03
002400       15  CD03O-ACC2                        PIC X(9).            cbankd03
002500       15  CD03O-DSC2                        PIC X(15).           cbankd03
002600       15  CD03O-BAL2                        PIC X(9).            cbankd03
002700       15  CD03O-BAL2N REDEFINES CD03O-BAL2  PIC S9(7)V99.        cbankd03
002800       15  CD03O-DTE2                        PIC X(10).           cbankd03
002900       15  CD03O-TXN2                        PIC X(1).            cbankd03
003000       15  CD03O-ACC3                        PIC X(9).            cbankd03
003100       15  CD03O-DSC3                        PIC X(15).           cbankd03
003200       15  CD03O-BAL3                        PIC X(9).            cbankd03
003300       15  CD03O-BAL3N REDEFINES CD03O-BAL3  PIC S9(7)V99.        cbankd03
003400       15  CD03O-DTE3                        PIC X(10).           cbankd03
003500       15  CD03O-TXN3                        PIC X(1).            cbankd03
003600       15  CD03O-ACC4                        PIC X(9).            cbankd03
003700       15  CD03O-DSC4                        PIC X(15).           cbankd03
003800       15  CD03O-BAL4                        PIC X(9).            cbankd03
003900       15  CD03O-BAL4N REDEFINES CD03O-BAL4  PIC S9(7)V99.        cbankd03
004000       15  CD03O-DTE4                        PIC X(10).           cbankd03
004100       15  CD03O-TXN4                        PIC X(1).            cbankd03
004200       15  CD03O-ACC5                        PIC X(9).            cbankd03
004300       15  CD03O-DSC5                        PIC X(15).           cbankd03
004400       15  CD03O-BAL5                        PIC X(9).            cbankd03
004500       15  CD03O-BAL5N REDEFINES CD03O-BAL5  PIC S9(7)V99.        cbankd03
004600       15  CD03O-DTE5                        PIC X(10).           cbankd03
004700       15  CD03O-TXN5                        PIC X(1).            cbankd03
004800       15  CD03O-ACC6                        PIC X(9).            cbankd03
004900       15  CD03O-DSC6                        PIC X(15).           cbankd03
005000       15  CD03O-BAL6                        PIC X(9).            cbankd03
005100       15  CD03O-BAL6N REDEFINES CD03O-BAL6  PIC S9(7)V99.        cbankd03
005200       15  CD03O-DTE6                        PIC X(10).           cbankd03
005300       15  CD03O-TXN6                        PIC X(1).            cbankd03
005400     10  CD03O-DATA-R REDEFINES CD03O-DATA.                       cbankd03
005500       15  CD03O-ACC-INFO                    OCCURS 6 TIMES.      cbankd03
005600         20  CD03O-ACC-NO                    PIC X(9).            cbankd03
005700         20  CD03O-ACC-DESC                  PIC X(15).           cbankd03
005800         20  CD03O-ACC-BAL                   PIC X(9).            cbankd03
005900         20  CD03O-ACC-BAL-N REDEFINES CD03O-ACC-BAL              cbankd03
006000                                             PIC S9(7)V99.        cbankd03
006100         20  CD03O-DTE                       PIC X(10).           cbankd03
006200         20  CD03O-TXN                       PIC X(1).            cbankd03
006300                                                                  cbankd03
006400* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankd03
