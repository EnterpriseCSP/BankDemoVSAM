000100***************************************************************** ctstampp
000200*                                                               * ctstampp
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * ctstampp
000400*                                                               * ctstampp
000500***************************************************************** ctstampp
000600                                                                  ctstampp
000700***************************************************************** ctstampp
000800* CTIMERP.CPY                                                   * ctstampp
000900*---------------------------------------------------------------* ctstampp
001000* Simulate SQL TIMESTAMP function                                 ctstampp
001100***************************************************************** ctstampp
001200     ACCEPT WS-ACCEPT-DATE FROM DATE YYYYMMDD.                    ctstampp
001300     ACCEPT WS-ACCEPT-TIME FROM TIME.                             ctstampp
001400     MOVE WS-ACCEPT-DATE (1:4) TO WS-TS-DATE-YYYY.                ctstampp
001500     MOVE '-' TO WS-TS-DATE-DASH1.                                ctstampp
001600     MOVE WS-ACCEPT-DATE (5:2) TO WS-TS-DATE-MM.                  ctstampp
001700     MOVE '-' TO WS-TS-DATE-DASH2.                                ctstampp
001800     MOVE WS-ACCEPT-DATE (7:2) TO WS-TS-DATE-DD.                  ctstampp
001900     MOVE '-' TO WS-TS-DATE-DASH3.                                ctstampp
002000     MOVE WS-ACCEPT-TIME (1:2) TO WS-TS-TIME-HH.                  ctstampp
002100     MOVE '.' TO WS-TS-TIME-DOT1.                                 ctstampp
002200     MOVE WS-ACCEPT-TIME (3:2) TO WS-TS-TIME-MM.                  ctstampp
002300     MOVE '.' TO WS-TS-TIME-DOT2.                                 ctstampp
002400     MOVE WS-ACCEPT-TIME (5:2) TO WS-TS-TIME-SS.                  ctstampp
002500     MOVE '.' TO WS-TS-TIME-DOT3.                                 ctstampp
002600     MOVE WS-ACCEPT-TIME (7:2) TO WS-TS-TIME-DDDDDD (1:2).        ctstampp
002700     MOVE '0000' TO WS-TS-TIME-DDDDDD (3:4).                      ctstampp
002800                                                                  ctstampp
002900* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     ctstampp
