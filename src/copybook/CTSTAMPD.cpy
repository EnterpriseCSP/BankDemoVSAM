000100***************************************************************** ctstampd
000200*                                                               * ctstampd
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * ctstampd
000400*                                                               * ctstampd
000500***************************************************************** ctstampd
000600                                                                  ctstampd
000700***************************************************************** ctstampd
000800* CTSTAMPD.CPY                                                  * ctstampd
000900*---------------------------------------------------------------* ctstampd
001000* Work areas for timestamp creation                             * ctstampd
001100***************************************************************** ctstampd
001200 01  WS-TIMESTAMP-AREAS.                                          ctstampd
001300   05  WS-ACCEPT-DATE                        PIC 9(8).            ctstampd
001400   05  WS-ACCEPT-TIME                        PIC 9(8).            ctstampd
001500   05  WS-TIMESTAMP.                                              ctstampd
001600     10  WS-TS-DATE.                                              ctstampd
001700       15  WS-TS-DATE-YYYY                   PIC X(4).            ctstampd
001800       15  WS-TS-DATE-DASH1                  PIC X(1).            ctstampd
001900       15  WS-TS-DATE-MM                     PIC X(2).            ctstampd
002000       15  WS-TS-DATE-DASH2                  PIC X(1).            ctstampd
002100       15  WS-TS-DATE-DD                     PIC X(2).            ctstampd
002200       15  WS-TS-DATE-DASH3                  PIC X(1).            ctstampd
002300     10  WS-TS-TIME.                                              ctstampd
002400       15  WS-TS-TIME-HH                     PIC X(2).            ctstampd
002500       15  WS-TS-TIME-DOT1                   PIC X(1).            ctstampd
002600       15  WS-TS-TIME-MM                     PIC X(2).            ctstampd
002700       15  WS-TS-TIME-DOT2                   PIC X(1).            ctstampd
002800       15  WS-TS-TIME-SS                     PIC X(2).            ctstampd
002900       15  WS-TS-TIME-DOT3                   PIC X(1).            ctstampd
003000       15  WS-TS-TIME-DDDDDD                 PIC X(6).            ctstampd
003100                                                                  ctstampd
003200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     ctstampd
