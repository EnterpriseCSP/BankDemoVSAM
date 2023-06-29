000100***************************************************************** ctimerp
000200*                                                               * ctimerp
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * ctimerp
000400*                                                               * ctimerp
000500***************************************************************** ctimerp
000600                                                                  ctimerp
000700***************************************************************** ctimerp
000800* CTIMERP.CPY                                                   * ctimerp
000900*---------------------------------------------------------------* ctimerp
001000* Procedure code to calculate & display run time                * ctimerp
001100***************************************************************** ctimerp
001200***************************************************************** ctimerp
001300* Establish program run time                                    * ctimerp
001400***************************************************************** ctimerp
001500 RUN-TIME.                                                        ctimerp
001600     IF TIMER-START IS EQUAL TO ZERO                              ctimerp
001700        ACCEPT TIMER-START FROM TIME                              ctimerp
001800        MOVE 'Timer started' TO WS-CONSOLE-MESSAGE                ctimerp
001900        PERFORM DISPLAY-CONSOLE-MESSAGE                           ctimerp
002000     ELSE                                                         ctimerp
002100        ACCEPT TIMER-END FROM TIME                                ctimerp
002200        MOVE 'Timer stopped' TO WS-CONSOLE-MESSAGE                ctimerp
002300        PERFORM DISPLAY-CONSOLE-MESSAGE                           ctimerp
002400        COMPUTE TIMER-ELAPSED =                                   ctimerp
002500                  ((TIMER-END-HH * 60 * 60 * 100) +               ctimerp
002600                   (TIMER-END-MM * 60 * 100) +                    ctimerp
002700                   (TIMER-END-SS * 100) +                         ctimerp
002800                    TIMER-END-DD) -                               ctimerp
002900                  ((TIMER-START-HH * 60 * 60 * 100) +             ctimerp
003000                   (TIMER-START-MM * 60 * 100) +                  ctimerp
003100                   (TIMER-START-SS * 100) +                       ctimerp
003200                    TIMER-START-DD)                               ctimerp
003300        MOVE TIMER-ELAPSED-R TO TIMER-RUN-TIME-ELAPSED            ctimerp
003400        MOVE TIMER-RUN-TIME TO WS-CONSOLE-MESSAGE                 ctimerp
003500        PERFORM DISPLAY-CONSOLE-MESSAGE                           ctimerp
003600     END-IF.                                                      ctimerp
003700                                                                  ctimerp
003800* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     ctimerp
