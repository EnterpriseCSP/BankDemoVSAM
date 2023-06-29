000100***************************************************************** cstmtd01
000200*                                                               * cstmtd01
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cstmtd01
000400*                                                               * cstmtd01
000500***************************************************************** cstmtd01
000600                                                                  cstmtd01
000700***************************************************************** cstmtd01
000800* CSTMTD01.CPY                                                  * cstmtd01
000900*---------------------------------------------------------------* cstmtd01
001000* This area is used to pass data between a requesting program   * cstmtd01
001100* and the "I/O" program (SSTMT01P) which retrieves contact      * cstmtd01
001200* information to send printed statements                        * cstmtd01
001300***************************************************************** cstmtd01
001400   05  CSTMTD01-DATA.                                             cstmtd01
001500     10  CSTMTD01I-DATA.                                          cstmtd01
001600       15  CSTMTD01I-CONTACT-ID              PIC X(5).            cstmtd01
001700       15  CSTMTD01I-OPTION                  PIC X(1).            cstmtd01
001800         88  CSTMTD01I-POST                  VALUE '1'.           cstmtd01
001900         88  CSTMTD01I-EMAIL                 VALUE '2'.           cstmtd01
002000           10  CSTMTD01O-DATA.                                    cstmtd01
002100       15  CSTMTD01O-CONTACT-ID              PIC X(5).            cstmtd01
002200         88  CSTMTD01O-OK                    VALUES SPACES.       cstmtd01
002300         88  CSTMTD01O-ERROR                 VALUES 'ERROR'.      cstmtd01
002400                                                                  cstmtd01
002500* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cstmtd01
