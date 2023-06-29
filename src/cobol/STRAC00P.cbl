000100***************************************************************** STRAC00P
000200*                                                               * STRAC00P
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * STRAC00P
000400*   This demonstration program is provided for use by users     * STRAC00P
000500*   of Micro Focus products and may be used, modified and       * STRAC00P
000600*   distributed as part of your application provided that       * STRAC00P
000700*   you properly acknowledge the copyright of Micro Focus       * STRAC00P
000800*   in this material.                                           * STRAC00P
000900*                                                               * STRAC00P
001000***************************************************************** STRAC00P
001100                                                                  STRAC00P
001200***************************************************************** STRAC00P
001300* Program:     STRAC00P.CBL (CICS Version)                      * STRAC00P
001400* Layer:       Screen handling                                  * STRAC00P
001500* Function:    Display activity on system log                   * STRAC00P
001600***************************************************************** STRAC00P
001700                                                                  STRAC00P
001800 IDENTIFICATION DIVISION.                                         STRAC00P
001900 PROGRAM-ID.                                                      STRAC00P
002000     STRAC00P.                                                    STRAC00P
002100 DATE-WRITTEN.                                                    STRAC00P
002200     September 2002.                                              STRAC00P
002300 DATE-COMPILED.                                                   STRAC00P
002400     Today.                                                       STRAC00P
002500                                                                  STRAC00P
002600 ENVIRONMENT DIVISION.                                            STRAC00P
002700                                                                  STRAC00P
002800 DATA DIVISION.                                                   STRAC00P
002900 WORKING-STORAGE SECTION.                                         STRAC00P
003000 01  WS-MISC-STORAGE.                                             STRAC00P
003100   05  WS-PROGRAM-ID                         PIC X(8)             STRAC00P
003200       VALUE 'STRAC00P'.                                          STRAC00P
003300   05  WS-TRAN-ID                            PIC X(4).            STRAC00P
003400   05  WS-WTO-DATA.                                               STRAC00P
003500     10  FILLER                              PIC X(7)             STRAC00P
003600         VALUE 'Termid:'.                                         STRAC00P
003700     10  WS-WTO-TERM                         PIC X(4).            STRAC00P
003800     10  FILLER                              PIC X(9)             STRAC00P
003900         VALUE ', Tranid:'.                                       STRAC00P
004000     10  WS-WTO-TRAN                         PIC X(4).            STRAC00P
004100     10  FILLER                              PIC X(10)            STRAC00P
004200         VALUE ', Program:'.                                      STRAC00P
004300     10  WS-WTO-PROG                         PIC X(8).            STRAC00P
004400                                                                  STRAC00P
004500 COPY DFHAID.                                                     STRAC00P
004600                                                                  STRAC00P
004700 COPY DFHBMSCA.                                                   STRAC00P
004800                                                                  STRAC00P
004900 COPY CABENDD.                                                    STRAC00P
005000                                                                  STRAC00P
005100 LINKAGE SECTION.                                                 STRAC00P
005200 01  DFHCOMMAREA.                                                 STRAC00P
005300   05  LK-CALLING-RTN                        PIC X(8).            STRAC00P
005400                                                                  STRAC00P
005500 PROCEDURE DIVISION.                                              STRAC00P
005600***************************************************************** STRAC00P
005700* Store our transaction-id in msg                               * STRAC00P
005800***************************************************************** STRAC00P
005900     MOVE EIBTRNID TO WS-WTO-TRAN.                                STRAC00P
006000                                                                  STRAC00P
006100***************************************************************** STRAC00P
006200* Store our terminal id in msg                                  * STRAC00P
006300***************************************************************** STRAC00P
006400     MOVE EIBTRMID TO WS-WTO-TERM                                 STRAC00P
006500                                                                  STRAC00P
006600***************************************************************** STRAC00P
006700* Store any passed data in msg                                  * STRAC00P
006800***************************************************************** STRAC00P
006900     IF EIBCALEN IS EQUAL TO 0                                    STRAC00P
007000        MOVE 'Unknown' TO WS-WTO-PROG                             STRAC00P
007100     ELSE                                                         STRAC00P
007200        MOVE LK-CALLING-RTN(1:EIBCALEN) TO WS-WTO-PROG            STRAC00P
007300     END-IF.                                                      STRAC00P
007400                                                                  STRAC00P
007500***************************************************************** STRAC00P
007600* Display the msg                                               * STRAC00P
007700***************************************************************** STRAC00P
007800     EXEC CICS WRITE                                              STRAC00P
007900               OPERATOR                                           STRAC00P
008000               TEXT(WS-WTO-DATA)                                  STRAC00P
008100               TEXTLENGTH(LENGTH OF WS-WTO-DATA)                  STRAC00P
008200     END-EXEC.                                                    STRAC00P
008300                                                                  STRAC00P
008400***************************************************************** STRAC00P
008500* Now we have to have finished and can return to our invoker.   * STRAC00P
008600***************************************************************** STRAC00P
008700     EXEC CICS                                                    STRAC00P
008800          RETURN                                                  STRAC00P
008900     END-EXEC.                                                    STRAC00P
009000     GOBACK.                                                      STRAC00P
009100                                                                  STRAC00P
