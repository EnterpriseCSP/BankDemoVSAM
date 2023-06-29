000100***************************************************************** BDEMO99P
000200*                                                               * BDEMO99P
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * BDEMO99P
000400*   This demonstration program is provided for use by users     * BDEMO99P
000500*   of Micro Focus products and may be used, modified and       * BDEMO99P
000600*   distributed as part of your application provided that       * BDEMO99P
000700*   you properly acknowledge the copyright of Micro Focus       * BDEMO99P
000800*   in this material.                                           * BDEMO99P
000900*                                                               * BDEMO99P
001000***************************************************************** BDEMO99P
001100                                                                  BDEMO99P
001200***************************************************************** BDEMO99P
001300* Program:     BDEMO99P.CBL                                     * BDEMO99P
001400* Layer:       Business logic                                   * BDEMO99P
001500* Function:    Terminate the pseudo converation G               * BDEMO99P
001600***************************************************************** BDEMO99P
001700                                                                  BDEMO99P
001800 IDENTIFICATION DIVISION.                                         BDEMO99P
001900 PROGRAM-ID.                                                      BDEMO99P
002000     BDEMO99P.                                                    BDEMO99P
002100 DATE-WRITTEN.                                                    BDEMO99P
002200     September 2002.                                              BDEMO99P
002300 DATE-COMPILED.                                                   BDEMO99P
002400     Today.                                                       BDEMO99P
002500                                                                  BDEMO99P
002600 ENVIRONMENT DIVISION.                                            BDEMO99P
002700                                                                  BDEMO99P
002800 DATA DIVISION.                                                   BDEMO99P
002900 WORKING-STORAGE SECTION.                                         BDEMO99P
003000 01  WS-MISC-STORAGE.                                             BDEMO99P
003100   05  WS-PROGRAM-ID                         PIC X(8)             BDEMO99P
003200       VALUE 'BDEMO99P'.                                          BDEMO99P
003300   05  WS-INPUT-FLAG                         PIC X(1).            BDEMO99P
003400     88  INPUT-OK                            VALUE '0'.           BDEMO99P
003500     88  INPUT-ERROR                         VALUE '1'.           BDEMO99P
003600   05  WS-ERROR-MSG                          PIC X(75).           BDEMO99P
003700                                                                  BDEMO99P
003800 01  WS-DEMO-DATA.                                                BDEMO99P
003900 COPY CDEMODAT.                                                   BDEMO99P
004000                                                                  BDEMO99P
004100 COPY CABENDD.                                                    BDEMO99P
004200                                                                  BDEMO99P
004300 LINKAGE SECTION.                                                 BDEMO99P
004400 01  DFHCOMMAREA.                                                 BDEMO99P
004500   05  LK-COMMAREA                           PIC X(6144).         BDEMO99P
004600                                                                  BDEMO99P
004700 COPY CENTRY.                                                     BDEMO99P
004800                                                                  BDEMO99P
004900***************************************************************** BDEMO99P
005000* Make ourselves re-entrant                                     * BDEMO99P
005100***************************************************************** BDEMO99P
005200     MOVE SPACES TO WS-ERROR-MSG.                                 BDEMO99P
005300                                                                  BDEMO99P
005400***************************************************************** BDEMO99P
005500* Move the passed area to our area                              * BDEMO99P
005600***************************************************************** BDEMO99P
005700     MOVE DFHCOMMAREA (1:LENGTH OF WS-DEMO-DATA) TO WS-DEMO-DATA. BDEMO99P
005800                                                                  BDEMO99P
005900***************************************************************** BDEMO99P
006000* Ensure error message is cleared                               * BDEMO99P
006100***************************************************************** BDEMO99P
006200     MOVE SPACES TO DEMO-ERROR-MSG.                               BDEMO99P
006300                                                                  BDEMO99P
006400***************************************************************** BDEMO99P
006500* This is the main process                                      * BDEMO99P
006600***************************************************************** BDEMO99P
006700     MOVE SPACES TO DEMO-CICS-TRANCODE.                           BDEMO99P
006800     MOVE 'BDEMO99P' TO DEMO-LAST-PROG                            BDEMO99P
006900     MOVE 'BDEMO99P' TO DEMO-NEXT-PROG                            BDEMO99P
007000     MOVE 'MDEMO99' TO DEMO-LAST-MAPSET                           BDEMO99P
007100     MOVE 'DEMO99A' TO DEMO-LAST-MAP                              BDEMO99P
007200     MOVE 'MDEMO99' TO DEMO-NEXT-MAPSET                           BDEMO99P
007300     MOVE 'DEMO99A' TO DEMO-NEXT-MAP                              BDEMO99P
007400     GO TO COMMON-RETURN.                                         BDEMO99P
007500                                                                  BDEMO99P
007600 COMMON-RETURN.                                                   BDEMO99P
007700     MOVE WS-DEMO-DATA TO DFHCOMMAREA (1:LENGTH OF WS-DEMO-DATA). BDEMO99P
007800 COPY CRETURN.                                                    BDEMO99P
007900                                                                  BDEMO99P
