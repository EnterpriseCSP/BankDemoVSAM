000100***************************************************************** ccashd02
000200*                                                               * ccashd02
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * ccashd02
000400*                                                               * ccashd02
000500***************************************************************** ccashd02
000600                                                                  ccashd02
000700***************************************************************** ccashd02
000800* CCASHD02.CPY                                                  * ccashd02
000900*---------------------------------------------------------------* ccashd02
001000* This area is used to pass data between a requesting program   * ccashd02
001100* and the I/O program (DCASHD02) which retrieves information    * ccashd02
001200* regarding customer's accounts                                 * ccashd02
001300***************************************************************** ccashd02
001400   05  CD02-DATA.                                                 ccashd02
001500     10  CD02I-DATA.                                              ccashd02
001600       15  CD02I-CONTACT-ID                  PIC X(5).            ccashd02
001700     10  CD02O-DATA.                                              ccashd02
001800       15  CD02O-DET1.                                            ccashd02
001900         20  CD02O-ACC1                      PIC X(9).            ccashd02
002000         20  CD02O-DSC1                      PIC X(15).           ccashd02
002100         20  CD02O-BAL1                      PIC X(13).           ccashd02
002200         20  CD02O-DAY-LIMIT1                PIC X(3).            ccashd02
002300         20  CD02O-DATE-USED1                PIC X(10).           ccashd02
002400         20  CD02O-DATE-AMT1                 PIC X(3).            ccashd02
002500       15  CD02O-DET2.                                            ccashd02
002600         20  CD02O-ACC2                      PIC X(9).            ccashd02
002700         20  CD02O-DSC2                      PIC X(15).           ccashd02
002800         20  CD02O-BAL2                      PIC X(13).           ccashd02
002900         20  CD02O-DAY-LIMIT2                PIC X(3).            ccashd02
003000         20  CD02O-DATE-USED2                PIC X(10).           ccashd02
003100         20  CD02O-DATE-AMT2                 PIC X(3).            ccashd02
003200       15  CD02O-DET3.                                            ccashd02
003300         20  CD02O-ACC3                      PIC X(9).            ccashd02
003400         20  CD02O-DSC3                      PIC X(15).           ccashd02
003500         20  CD02O-BAL3                      PIC X(13).           ccashd02
003600         20  CD02O-DAY-LIMIT3                PIC X(3).            ccashd02
003700         20  CD02O-DATE-USED3                PIC X(10).           ccashd02
003800         20  CD02O-DATE-AMT3                 PIC X(3).            ccashd02
003900       15  CD02O-DET4.                                            ccashd02
004000         20  CD02O-ACC4                      PIC X(9).            ccashd02
004100         20  CD02O-DSC4                      PIC X(15).           ccashd02
004200         20  CD02O-BAL4                      PIC X(13).           ccashd02
004300         20  CD02O-DAY-LIMIT4                PIC X(3).            ccashd02
004400         20  CD02O-DATE-USED4                PIC X(10).           ccashd02
004500         20  CD02O-DATE-AMT4                 PIC X(3).            ccashd02
004600       15  CD02O-DET5.                                            ccashd02
004700         20  CD02O-ACC5                      PIC X(9).            ccashd02
004800         20  CD02O-DSC5                      PIC X(15).           ccashd02
004900         20  CD02O-BAL5                      PIC X(13).           ccashd02
005000         20  CD02O-DAY-LIMIT5                PIC X(3).            ccashd02
005100         20  CD02O-DATE-USED5                PIC X(10).           ccashd02
005200         20  CD02O-DATE-AMT5                 PIC X(3).            ccashd02
005300     10  CD02O-DATA-R REDEFINES CD02O-DATA.                       ccashd02
005400       15  CD02O-ACC-INFO                    OCCURS 5 TIMES.      ccashd02
005500         20  CD02O-ACC-NO                    PIC X(9).            ccashd02
005600         20  CD02O-ACC-DESC                  PIC X(15).           ccashd02
005700         20  CD02O-ACC-BAL                   PIC X(13).           ccashd02
005800         20  CD02O-ACC-DAY-LIMIT             PIC X(3).            ccashd02
005900         20  CD02O-ACC-DATE-USED             PIC X(10).           ccashd02
006000         20  CD02O-ACC-DATE-AMT              PIC X(3).            ccashd02
006100                                                                  ccashd02
006200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     ccashd02
