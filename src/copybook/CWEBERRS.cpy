000100* *************************************************************** cweberrs
000200*                                                               * cweberrs
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cweberrs
000400*                                                               * cweberrs
000500***************************************************************** cweberrs
000600                                                                  cweberrs
000700***************************************************************** cweberrs
000800* CWEBERRS.CPY                                                  * cweberrs
000900*---------------------------------------------------------------* cweberrs
001000***************************************************************** cweberrs
001100     10  WEB-ERROR-DATA                      PIC X(1024).         cweberrs
001200     10  ECI-ERROR-DATA REDEFINES WEB-ERROR-DATA.                 cweberrs
001300       15  ECI-ERR-CODE-N                    PIC -9999.           cweberrs
001400       15  ECI-ERR-CODE-TXT                  PIC X(32).           cweberrs
001500       15  ECI-ERR-PGM                       PIC X(8).            cweberrs
001600       15  ECI-ERR-USERID                    PIC X(8).            cweberrs
001700       15  ECI-ERR-TRAN                      PIC X(8).            cweberrs
001800       15  ECI-ERR-ABEND-CODE                PIC X(4).            cweberrs
001900       15  ECI-ERR-SYSTEM                    PIC X(8).            cweberrs
002000     10  MQ-ERROR-DATA REDEFINES WEB-ERROR-DATA.                  cweberrs
002100       15  MQ-ERR-FILLER                     PIC X(1).            cweberrs
002200                                                                  cweberrs
002300* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cweberrs
