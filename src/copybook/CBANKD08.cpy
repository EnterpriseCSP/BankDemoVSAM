000100***************************************************************** cbankd08
000200*                                                               * cbankd08
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankd08
000400*                                                               * cbankd08
000500***************************************************************** cbankd08
000600                                                                  cbankd08
000700***************************************************************** cbankd08
000800* CBANKD08.CPY                                                  * cbankd08
000900*---------------------------------------------------------------* cbankd08
001000* This area is used to pass data between a requesting program   * cbankd08
001100* and the I/O program (DBANK03P) which retrieves information    * cbankd08
001200* regarding customer's accounts                                 * cbankd08
001300***************************************************************** cbankd08
001400   05  CD08-DATA.                                                 cbankd08
001500     10  CD08I-DATA.                                              cbankd08
001600       15  CD08I-CONTACT-ID                  PIC X(5).            cbankd08
001700     10  CD08O-DATA.                                              cbankd08
001800       15  CD08O-COUNT                       PIC 9(3).            cbankd08
001900                                                                  cbankd08
002000* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankd08
