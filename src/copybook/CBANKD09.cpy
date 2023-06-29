000100***************************************************************** cbankd09
000200*                                                               * cbankd09
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankd09
000400*                                                               * cbankd09
000500***************************************************************** cbankd09
000600                                                                  cbankd09
000700***************************************************************** cbankd09
000800* CBANKD09.CPY                                                  * cbankd09
000900*---------------------------------------------------------------* cbankd09
001000* This area is used to pass data between a requesting program   * cbankd09
001100* and the I/O program (DBANK09P) which retrieves contact        * cbankd09
001200* information to send printed statements                        * cbankd09
001300***************************************************************** cbankd09
001400   05  CD09-DATA.                                                 cbankd09
001500     10  CD09I-DATA.                                              cbankd09
001600       15  CD09I-CONTACT-ID                  PIC X(5).            cbankd09
001700     10  CD09O-DATA.                                              cbankd09
001800       15  CD09O-CONTACT-ID                  PIC X(5).            cbankd09
001900       15  CD09O-CONTACT-NAME                PIC X(25).           cbankd09
002000       15  CD09O-CONTACT-ADDR1               PIC X(25).           cbankd09
002100       15  CD09O-CONTACT-ADDR2               PIC X(25).           cbankd09
002200       15  CD09O-CONTACT-STATE               PIC X(2).            cbankd09
002300       15  CD09O-CONTACT-CNTRY               PIC X(6).            cbankd09
002400       15  CD09O-CONTACT-PSTCDE              PIC X(6).            cbankd09
002500       15  CD09O-CONTACT-EMAIL               PIC X(30).           cbankd09
002600                                                                  cbankd09
002700* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankd09
