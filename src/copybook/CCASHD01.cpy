000100***************************************************************** ccashd01
000200*                                                               * ccashd01
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * ccashd01
000400*                                                               * ccashd01
000500***************************************************************** ccashd01
000600                                                                  ccashd01
000700***************************************************************** ccashd01
000800* CCASHD01.CPY                                                  * ccashd01
000900*---------------------------------------------------------------* ccashd01
001000* This area is used to pass data between a requesting program   * ccashd01
001100* and the I/O program (DCASHD01) which retrieves information    * ccashd01
001200* regarding customer's accounts                                 * ccashd01
001300***************************************************************** ccashd01
001400   05  CD01-DATA.                                                 ccashd01
001500     10  CD01I-DATA.                                              ccashd01
001600       15  CD01I-CONTACT-ID                  PIC X(5).            ccashd01
001700     10  CD01O-DATA.                                              ccashd01
001800       15  CD01O-PIN                         PIC X(4).            ccashd01
001900                                                                  ccashd01
002000* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     ccashd01
