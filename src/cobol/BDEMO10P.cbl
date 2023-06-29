000100***************************************************************** BDEMO10P
000200*                                                               * BDEMO10P
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * BDEMO10P
000400*   This demonstration program is provided for use by users     * BDEMO10P
000500*   of Micro Focus products and may be used, modified and       * BDEMO10P
000600*   distributed as part of your application provided that       * BDEMO10P
000700*   you properly acknowledge the copyright of Micro Focus       * BDEMO10P
000800*   in this material.                                           * BDEMO10P
000900*                                                               * BDEMO10P
001000***************************************************************** BDEMO10P
001100                                                                  BDEMO10P
001200***************************************************************** BDEMO10P
001300* Program:     BDEMO10P.CBL                                     * BDEMO10P
001400* Layer:       Business logic                                   * BDEMO10P
001500* Function:    Signon to system to identify user                * BDEMO10P
001600***************************************************************** BDEMO10P
001700                                                                  BDEMO10P
001800 IDENTIFICATION DIVISION.                                         BDEMO10P
001900 PROGRAM-ID.                                                      BDEMO10P
002000     BDEMO10P.                                                    BDEMO10P
002100 DATE-WRITTEN.                                                    BDEMO10P
002200     September 2002.                                              BDEMO10P
002300 DATE-COMPILED.                                                   BDEMO10P
002400     Today.                                                       BDEMO10P
002500                                                                  BDEMO10P
002600 ENVIRONMENT DIVISION.                                            BDEMO10P
002700                                                                  BDEMO10P
002800 DATA DIVISION.                                                   BDEMO10P
002900 WORKING-STORAGE SECTION.                                         BDEMO10P
003000 01  WS-MISC-STORAGE.                                             BDEMO10P
003100   05  WS-PROGRAM-ID                         PIC X(8)             BDEMO10P
003200       VALUE 'BDEMO10P'.                                          BDEMO10P
003300   05  WS-INPUT-FLAG                         PIC X(1).            BDEMO10P
003400     88  INPUT-OK                            VALUE '0'.           BDEMO10P
003500     88  INPUT-ERROR                         VALUE '1'.           BDEMO10P
003600   05  WS-RETURN-FLAG                        PIC X(1).            BDEMO10P
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    BDEMO10P
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           BDEMO10P
003900   05  WS-RETURN-MSG                         PIC X(75).           BDEMO10P
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        BDEMO10P
004100   05  WS-PFK-FLAG                           PIC X(1).            BDEMO10P
004200     88  PFK-VALID                           VALUE '0'.           BDEMO10P
004300     88  PFK-INVALID                         VALUE '1'.           BDEMO10P
004400   05  WS-ERROR-MSG                          PIC X(75).           BDEMO10P
004500   05  WS-SEL-COUNT                          PIC 9(3).            BDEMO10P
004600                                                                  BDEMO10P
004700 01  WS-DEMO-DATA.                                                BDEMO10P
004800 COPY CDEMODAT.                                                   BDEMO10P
004900                                                                  BDEMO10P
005000 01  WS-HELP-DATA.                                                BDEMO10P
005100 COPY CHELPD01.                                                   BDEMO10P
005200                                                                  BDEMO10P
005300 01  WS-DEMO-OPTIONS.                                             BDEMO10P
005400 COPY COPTIONS.                                                   BDEMO10P
005500                                                                  BDEMO10P
005600 COPY CABENDD.                                                    BDEMO10P
005700                                                                  BDEMO10P
005800 LINKAGE SECTION.                                                 BDEMO10P
005900 01  DFHCOMMAREA.                                                 BDEMO10P
006000   05  LK-COMMAREA                           PIC X(6144).         BDEMO10P
006100                                                                  BDEMO10P
006200 COPY CENTRY.                                                     BDEMO10P
006300***************************************************************** BDEMO10P
006400* Make ourselves re-entrant                                     * BDEMO10P
006500***************************************************************** BDEMO10P
006600     MOVE SPACES TO WS-ERROR-MSG.                                 BDEMO10P
006700                                                                  BDEMO10P
006800***************************************************************** BDEMO10P
006900* Move the passed area to our area                              * BDEMO10P
007000***************************************************************** BDEMO10P
007100     MOVE DFHCOMMAREA (1:LENGTH OF WS-DEMO-DATA) TO WS-DEMO-DATA. BDEMO10P
007200                                                                  BDEMO10P
007300***************************************************************** BDEMO10P
007400* Ensure error message is cleared                               * BDEMO10P
007500***************************************************************** BDEMO10P
007600     MOVE SPACES TO DEMO-ERROR-MSG.                               BDEMO10P
007700                                                                  BDEMO10P
007800***************************************************************** BDEMO10P
007900* If this is the first time in, then we have to do set up the   * BDEMO10P
008000* COMMAREA and ask for the first map to be displayed.           * BDEMO10P
008100***************************************************************** BDEMO10P
008200     IF DEMO-NO-CONV-IN-PROGRESS                                  BDEMO10P
008300        SET DEMO-CONV-IN-PROGRESS TO TRUE                         BDEMO10P
008400        MOVE 'BDEMO10P' TO DEMO-LAST-PROG                         BDEMO10P
008500        MOVE 'BDEMO10P' TO DEMO-NEXT-PROG                         BDEMO10P
008600        MOVE LOW-VALUES TO DEMO-SCR10-SEL1                        BDEMO10P
008700        MOVE LOW-VALUES TO DEMO-SCR10-SEL2                        BDEMO10P
008800        MOVE LOW-VALUES TO DEMO-SCR10-SEL3                        BDEMO10P
008900        MOVE LOW-VALUES TO DEMO-SCR10-SEL4                        BDEMO10P
009000        MOVE SPACES TO DEMO-LAST-MAPSET                           BDEMO10P
009100        MOVE SPACES TO DEMO-LAST-MAP                              BDEMO10P
009200        MOVE 'MDEMO10' TO DEMO-NEXT-MAPSET                        BDEMO10P
009300        MOVE 'DEMO10A' TO DEMO-NEXT-MAP                           BDEMO10P
009400        GO TO COMMON-RETURN                                       BDEMO10P
009500     END-IF.                                                      BDEMO10P
009600                                                                  BDEMO10P
009700***************************************************************** BDEMO10P
009800* This is the main process                                      * BDEMO10P
009900***************************************************************** BDEMO10P
010000                                                                  BDEMO10P
010100***************************************************************** BDEMO10P
010200* Save the passed return flag and then turn it off              * BDEMO10P
010300***************************************************************** BDEMO10P
010400     MOVE DEMO-RETURN-FLAG TO WS-RETURN-FLAG.                     BDEMO10P
010500     SET DEMO-RETURN-FLAG-OFF TO TRUE.                            BDEMO10P
010600                                                                  BDEMO10P
010700***************************************************************** BDEMO10P
010800* Check the AID to see if its valid at this point               * BDEMO10P
010900***************************************************************** BDEMO10P
011000     SET PFK-INVALID TO TRUE.                                     BDEMO10P
011100     IF DEMO-AID-ENTER OR                                         BDEMO10P
011200        DEMO-AID-PFK03                                            BDEMO10P
011300        SET PFK-VALID TO TRUE                                     BDEMO10P
011400     END-IF.                                                      BDEMO10P
011500     IF DEMO-AID-PFK01 AND                                        BDEMO10P
011600        DEMO-HELP-INACTIVE                                        BDEMO10P
011700        SET DEMO-HELP-ACTIVE TO TRUE                              BDEMO10P
011800        SET PFK-VALID TO TRUE                                     BDEMO10P
011900     END-IF.                                                      BDEMO10P
012000     IF DEMO-AID-PFK04 AND                                        BDEMO10P
012100        DEMO-HELP-ACTIVE                                          BDEMO10P
012200        SET PFK-VALID TO TRUE                                     BDEMO10P
012300     END-IF.                                                      BDEMO10P
012400     IF PFK-INVALID                                               BDEMO10P
012500        SET DEMO-AID-ENTER TO TRUE                                BDEMO10P
012600     END-IF.                                                      BDEMO10P
012700                                                                  BDEMO10P
012800***************************************************************** BDEMO10P
012900* Check the AID to see if we have to quit                       * BDEMO10P
013000***************************************************************** BDEMO10P
013100     IF DEMO-AID-PFK03                                            BDEMO10P
013200        MOVE 'BDEMO10P' TO DEMO-LAST-PROG                         BDEMO10P
013300        MOVE 'BDEMO99P' TO DEMO-NEXT-PROG                         BDEMO10P
013400        MOVE 'MDEMO10' TO DEMO-LAST-MAPSET                        BDEMO10P
013500        MOVE 'DEMO10A' TO DEMO-LAST-MAP                           BDEMO10P
013600        MOVE 'MDEMO99' TO DEMO-NEXT-MAPSET                        BDEMO10P
013700        MOVE 'DEMO99A' TO DEMO-NEXT-MAP                           BDEMO10P
013800        GO TO COMMON-RETURN                                       BDEMO10P
013900     END-IF.                                                      BDEMO10P
014000                                                                  BDEMO10P
014100***************************************************************** BDEMO10P
014200* Check the to see if user needs or has been using help         * BDEMO10P
014300***************************************************************** BDEMO10P
014400     IF DEMO-HELP-ACTIVE                                          BDEMO10P
014500        IF DEMO-AID-PFK04                                         BDEMO10P
014600           SET DEMO-HELP-INACTIVE TO TRUE                         BDEMO10P
014700           MOVE 00 TO DEMO-HELP-SCREEN                            BDEMO10P
014800           MOVE 'BDEMO10P' TO DEMO-LAST-PROG                      BDEMO10P
014900           MOVE 'BDEMO10P' TO DEMO-NEXT-PROG                      BDEMO10P
015000           MOVE 'MDEMO10' TO DEMO-LAST-MAPSET                     BDEMO10P
015100           MOVE 'HELP01A' TO DEMO-LAST-MAP                        BDEMO10P
015200           MOVE 'MDEMO10' TO DEMO-NEXT-MAPSET                     BDEMO10P
015300           MOVE 'DEMO10A' TO DEMO-NEXT-MAP                        BDEMO10P
015400           GO TO COMMON-RETURN                                    BDEMO10P
015500        ELSE                                                      BDEMO10P
015600           MOVE 01 TO DEMO-HELP-SCREEN                            BDEMO10P
015700           MOVE 'BDEMO10P' TO DEMO-LAST-PROG                      BDEMO10P
015800           MOVE 'BDEMO10P' TO DEMO-NEXT-PROG                      BDEMO10P
015900           MOVE 'MDEMO10' TO DEMO-LAST-MAPSET                     BDEMO10P
016000           MOVE 'DEMO10A' TO DEMO-LAST-MAP                        BDEMO10P
016100           MOVE 'MDEMO10' TO DEMO-NEXT-MAPSET                     BDEMO10P
016200           MOVE 'HELP01A' TO DEMO-NEXT-MAP                        BDEMO10P
016300           MOVE 'DEMO10' TO HELP01I-SCRN                          BDEMO10P
016400           COPY CHELPX01.                                         BDEMO10P
016500           MOVE HELP01O-DATA TO DEMO-HELP-DATA                    BDEMO10P
016600           GO TO COMMON-RETURN                                    BDEMO10P
016700     END-IF.                                                      BDEMO10P
016800                                                                  BDEMO10P
016900     PERFORM VALIDATE-DATA THRU                                   BDEMO10P
017000             VALIDATE-DATA-EXIT.                                  BDEMO10P
017100                                                                  BDEMO10P
017200* If we had an error display error and return                     BDEMO10P
017300     IF INPUT-ERROR                                               BDEMO10P
017400        MOVE WS-ERROR-MSG TO DEMO-ERROR-MSG                       BDEMO10P
017500        MOVE 'SDEMO10P' TO DEMO-LAST-PROG                         BDEMO10P
017600        MOVE 'SDEMO10P' TO DEMO-NEXT-PROG                         BDEMO10P
017700        MOVE 'MDEMO10' TO DEMO-LAST-MAPSET                        BDEMO10P
017800        MOVE 'DEMO10A' TO DEMO-LAST-MAP                           BDEMO10P
017900        MOVE 'MDEMO10' TO DEMO-NEXT-MAPSET                        BDEMO10P
018000        MOVE 'DEMO10A' TO DEMO-NEXT-MAP                           BDEMO10P
018100        GO TO COMMON-RETURN                                       BDEMO10P
018200     END-IF.                                                      BDEMO10P
018300                                                                  BDEMO10P
018400     EVALUATE TRUE                                                BDEMO10P
018500       WHEN DEMO-SCR10-SEL1 IS NOT EQUAL TO LOW-VALUES            BDEMO10P
018600         MOVE DEMO-OPTN-PROG (1) TO DEMO-NEXT-PROG                BDEMO10P
018700         MOVE DEMO-OPTN-TRAN (1) TO DEMO-NEXT-TRAN                BDEMO10P
018800       WHEN DEMO-SCR10-SEL2 IS NOT EQUAL TO LOW-VALUES            BDEMO10P
018900         MOVE DEMO-OPTN-PROG (2) TO DEMO-NEXT-PROG                BDEMO10P
019000         MOVE DEMO-OPTN-TRAN (2) TO DEMO-NEXT-TRAN                BDEMO10P
019100       WHEN DEMO-SCR10-SEL3 IS NOT EQUAL TO LOW-VALUES            BDEMO10P
019200         MOVE DEMO-OPTN-PROG (3) TO DEMO-NEXT-PROG                BDEMO10P
019300         MOVE DEMO-OPTN-TRAN (3) TO DEMO-NEXT-TRAN                BDEMO10P
019400       WHEN DEMO-SCR10-SEL4 IS NOT EQUAL TO LOW-VALUES            BDEMO10P
019500         MOVE DEMO-OPTN-PROG (4) TO DEMO-NEXT-PROG                BDEMO10P
019600         MOVE DEMO-OPTN-TRAN (4) TO DEMO-NEXT-TRAN                BDEMO10P
019700       WHEN OTHER                                                 BDEMO10P
019800         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      BDEMO10P
019900         MOVE '0001' TO ABEND-CODE                                BDEMO10P
020000         MOVE SPACES TO ABEND-REASON                              BDEMO10P
020100         COPY CABENDPO.                                           BDEMO10P
020200     END-EVALUATE.                                                BDEMO10P
020300                                                                  BDEMO10P
020400     GO TO COMMON-RETURN.                                         BDEMO10P
020500                                                                  BDEMO10P
020600 COMMON-RETURN.                                                   BDEMO10P
020700     MOVE WS-DEMO-DATA TO DFHCOMMAREA (1:LENGTH OF WS-DEMO-DATA). BDEMO10P
020800 COPY CRETURN.                                                    BDEMO10P
020900                                                                  BDEMO10P
021000 VALIDATE-DATA.                                                   BDEMO10P
021100     SET INPUT-OK TO TRUE.                                        BDEMO10P
021200     MOVE ZERO TO WS-SEL-COUNT.                                   BDEMO10P
021300                                                                  BDEMO10P
021400* Must select a single option                                     BDEMO10P
021500     IF DEMO-SCR10-SEL1 IS NOT EQUAL TO LOW-VALUES                BDEMO10P
021600        ADD 1 TO WS-SEL-COUNT                                     BDEMO10P
021700     END-IF.                                                      BDEMO10P
021800                                                                  BDEMO10P
021900     IF DEMO-SCR10-SEL2 IS NOT EQUAL TO LOW-VALUES                BDEMO10P
022000        ADD 1 TO WS-SEL-COUNT                                     BDEMO10P
022100     END-IF.                                                      BDEMO10P
022200                                                                  BDEMO10P
022300     IF DEMO-SCR10-SEL3 IS NOT EQUAL TO LOW-VALUES                BDEMO10P
022400        ADD 1 TO WS-SEL-COUNT                                     BDEMO10P
022500     END-IF.                                                      BDEMO10P
022600                                                                  BDEMO10P
022700     IF DEMO-SCR10-SEL4 IS NOT EQUAL TO LOW-VALUES                BDEMO10P
022800        ADD 1 TO WS-SEL-COUNT                                     BDEMO10P
022900     END-IF.                                                      BDEMO10P
023000                                                                  BDEMO10P
023100     IF WS-SEL-COUNT IS EQUAL TO ZERO                             BDEMO10P
023200        MOVE 'Please select an option' TO WS-ERROR-MSG            BDEMO10P
023300        GO TO VALIDATE-DATA-ERROR                                 BDEMO10P
023400     END-IF.                                                      BDEMO10P
023500                                                                  BDEMO10P
023600     IF WS-SEL-COUNT IS GREATER THAN 1                            BDEMO10P
023700        MOVE 'Please select a single option' TO WS-ERROR-MSG      BDEMO10P
023800        GO TO VALIDATE-DATA-ERROR                                 BDEMO10P
023900     END-IF.                                                      BDEMO10P
024000                                                                  BDEMO10P
024100     GO TO VALIDATE-DATA-EXIT.                                    BDEMO10P
024200                                                                  BDEMO10P
024300 VALIDATE-DATA-ERROR.                                             BDEMO10P
024400     SET INPUT-ERROR TO TRUE.                                     BDEMO10P
024500 VALIDATE-DATA-EXIT.                                              BDEMO10P
024600     EXIT.                                                        BDEMO10P
024700                                                                  BDEMO10P
