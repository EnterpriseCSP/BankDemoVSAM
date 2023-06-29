000100***************************************************************** CDEMOEXT
000200*                                                               * CDEMOEXT
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CDEMOEXT
000400*                                                               * CDEMOEXT
000500***************************************************************** CDEMOEXT
000600                                                                  CDEMOEXT
000700***************************************************************** CDEMOEXT
000800* CDEMOEXT.CPY                                                  * CDEMOEXT
000900*---------------------------------------------------------------* CDEMOEXT
001000* External data for DEMOnstration application                   * CDEMOEXT
001100***************************************************************** CDEMOEXT
001200   05  EXT-DEMO-DATA                         PIC X(512).          CDEMOEXT
001300                                                                  CDEMOEXT
001400   05  EXT-IP-DATA REDEFINES EXT-DEMO-DATA.                       CDEMOEXT
001500     10  EXT-IP-AID                          PIC X(5).            CDEMOEXT
001600     10  EXT-IP-AREA                         PIC X(16).           CDEMOEXT
001700     10  EXT-IP10-DATA REDEFINES EXT-IP-AREA.                     CDEMOEXT
001800       15  EXT-IP10-SEL1                     PIC X(1).            CDEMOEXT
001900       15  EXT-IP10-SEL2                     PIC X(1).            CDEMOEXT
002000       15  EXT-IP10-SEL3                     PIC X(1).            CDEMOEXT
002100       15  EXT-IP10-SEL4                     PIC X(1).            CDEMOEXT
002200                                                                  CDEMOEXT
002300   05  EXT-OP-DATA REDEFINES EXT-DEMO-DATA.                       CDEMOEXT
002400     10  EXT-OP-TRAN                         PIC X(4).            CDEMOEXT
002500     10  EXT-OP-SCREEN                       PIC X(8).            CDEMOEXT
002600     10  EXT-OP-DATE                         PIC X(11).           CDEMOEXT
002700     10  EXT-OP-TIME                         PIC X(8).            CDEMOEXT
002800     10  EXT-OP-HEAD1                        PIC X(50).           CDEMOEXT
002900     10  EXT-OP-HEAD2                        PIC X(50).           CDEMOEXT
003000     10  EXT-OP-ERR-MSG                      PIC X(75).           CDEMOEXT
003100     10  EXT-OP-AREA                         PIC X(300).          CDEMOEXT
003200     10  EXT-OP10-DATA REDEFINES EXT-OP-AREA.                     CDEMOEXT
003300       15  EXT-OP10-SEL1                     PIC X(1).            CDEMOEXT
003400       15  EXT-OP10-TXT1                     PIC X(70).           CDEMOEXT
003500       15  EXT-OP10-SEL2                     PIC X(1).            CDEMOEXT
003600       15  EXT-OP10-TXT2                     PIC X(70).           CDEMOEXT
003700       15  EXT-OP10-SEL3                     PIC X(1).            CDEMOEXT
003800       15  EXT-OP10-TXT3                     PIC X(70).           CDEMOEXT
003900       15  EXT-OP10-SEL4                     PIC X(1).            CDEMOEXT
004000       15  EXT-OP10-TXT4                     PIC X(70).           CDEMOEXT
004100                                                                  CDEMOEXT
