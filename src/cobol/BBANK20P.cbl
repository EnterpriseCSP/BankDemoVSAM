000100***************************************************************** BBANK20P
000200*   :-)                                                            * BBANK
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * BBANK20P
000400*   This demonstration program is provided for use by users     * BBANK20P
000500*   of Micro Focus products and may be used, modified and       * BBANK20P
000600*   distributed as part of your application provided that       * BBANK20P
000700*   you properly acknowledge the copyright of Micro Focus       * BBANK20P
000800*   in this material.                                           * BBANK20P
000900*                                                               * BBANK20P
001000***************************************************************** BBANK20P
001100                                                                  BBANK20P
001200***************************************************************** BBANK20P
001300* Program:     BBANK20P.CBL                                     * BBANK20P
001400* Layer:       Business logic                                   * BBANK20P
001500* Function:    Determine users options                          * BBANK20P
001600***************************************************************** BBANK20P
001700                                                                  BBANK20P
001800 IDENTIFICATION DIVISION.                                         BBANK20P
001900 PROGRAM-ID.                                                      BBANK20P
002000     BBANK20P.                                                    BBANK20P
002100 DATE-WRITTEN.                                                    BBANK20P
002200     September 2002.                                              BBANK20P
002300 DATE-COMPILED.                                                   BBANK20P
002400     Today.                                                       BBANK20P
002500                                                                  BBANK20P
002600 ENVIRONMENT DIVISION.                                            BBANK20P
002700                                                                  BBANK20P
002800 DATA DIVISION.                                                   BBANK20P
002900 WORKING-STORAGE SECTION.                                         BBANK20P
003000 01  WS-MISC-STORAGE.                                             BBANK20P
003100   05  WS-PROGRAM-ID                         PIC X(8)             BBANK20P
003200       VALUE 'BBANK20P'.                                          BBANK20P
003300   05  WS-INPUT-FLAG                         PIC X(1).            BBANK20P
003400     88  INPUT-OK                            VALUE '0'.           BBANK20P
003500     88  INPUT-ERROR                         VALUE '1'.           BBANK20P
003600   05  WS-RETURN-FLAG                        PIC X(1).            BBANK20P
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    BBANK20P
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           BBANK20P
003900   05  WS-RETURN-MSG                         PIC X(75).           BBANK20P
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        BBANK20P
004100   05  WS-PFK-FLAG                           PIC X(1).            BBANK20P
004200     88  PFK-VALID                           VALUE '0'.           BBANK20P
004300     88  PFK-INVALID                         VALUE '1'.           BBANK20P
004400   05  WS-ERROR-MSG                          PIC X(75).           BBANK20P
004500   05  WS-SUB1                               PIC S9(4) COMP.      BBANK20P
004600   05  WS-SUB1-LIMIT                         PIC S9(4) COMP.      BBANK20P
004700   05  WS-SEL-COUNT                          PIC 9(1).            BBANK20P
004800   05  WS-SEL-OPTION                         PIC X(1).            BBANK20P
004900     88  WS-SEL-OPTION-NULL                  VALUES ' ', '.'.     BBANK20P
005000     88  WS-SEL-OPTION-DISPLAY               VALUE 'D'.           BBANK20P
005100     88  WS-SEL-OPTION-TRANSFER              VALUE 'X'.           BBANK20P
005200     88  WS-SEL-OPTION-UPDATE                VALUE 'U'.           BBANK20P
005300     88  WS-SEL-OPTION-LOAN                  VALUE 'L'.           BBANK20P
005400     88  WS-SEL-OPTION-INFO                  VALUE 'I'.           BBANK20P
005500   05  WS-SEL-MATRIX                         PIC X(5).            BBANK20P
005600                                                                  BBANK20P
005700 01  WS-BANK-DATA.                                                BBANK20P
005800 COPY CBANKDAT.                                                   BBANK20P
005900                                                                  BBANK20P
006000 01  WS-HELP-DATA.                                                BBANK20P
006100 COPY CHELPD01.                                                   BBANK20P
006200                                                                  BBANK20P
006300 01  WS-ACCOUNT-DATA.                                             BBANK20P
006400 COPY CBANKD08.                                                   BBANK20P
006500                                                                  BBANK20P
006600 COPY CABENDD.                                                    BBANK20P
006700                                                                  BBANK20P
006800 LINKAGE SECTION.                                                 BBANK20P
006900 01  DFHCOMMAREA.                                                 BBANK20P
007000   05  LK-COMMAREA                           PIC X(6144).         BBANK20P
007100                                                                  BBANK20P
007200 COPY CENTRY.                                                     BBANK20P
007300***************************************************************** BBANK20P
007400* Make ourselves re-entrant                                     * BBANK20P
007500***************************************************************** BBANK20P
007600     MOVE SPACES TO WS-ERROR-MSG.                                 BBANK20P
007700                                                                  BBANK20P
007800***************************************************************** BBANK20P
007900* Move the passed area to our area                              * BBANK20P
008000***************************************************************** BBANK20P
008100     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. BBANK20P
008200                                                                  BBANK20P
008300***************************************************************** BBANK20P
008400* Ensure error message is cleared                               * BBANK20P
008500***************************************************************** BBANK20P
008600     MOVE SPACES TO BANK-ERROR-MSG.                               BBANK20P
008700                                                                  BBANK20P
008800***************************************************************** BBANK20P
008900* This is the main process                                      * BBANK20P
009000***************************************************************** BBANK20P
009100                                                                  BBANK20P
009200***************************************************************** BBANK20P
009300* Initialize the list of options available                      * BBANK20P
009400***************************************************************** BBANK20P
009500     MOVE SPACES TO WS-SEL-MATRIX.                                BBANK20P
009600                                                                  BBANK20P
009700***************************************************************** BBANK20P
009800* Save the passed return message and then turn it off           * BBANK20P
009900***************************************************************** BBANK20P
010000     MOVE BANK-RETURN-MSG TO WS-RETURN-MSG.                       BBANK20P
010100     SET BANK-RETURN-MSG-OFF TO TRUE.                             BBANK20P
010200                                                                  BBANK20P
010300     MOVE WS-RETURN-MSG TO WS-ERROR-MSG.                          BBANK20P
010400                                                                  BBANK20P
010500***************************************************************** BBANK20P
010600* Check the AID to see if its valid at this point               * BBANK20P
010700***************************************************************** BBANK20P
010800     SET PFK-INVALID TO TRUE.                                     BBANK20P
010900     IF BANK-AID-ENTER OR                                         BBANK20P
011000        BANK-AID-PFK03 OR                                         BBANK20P
011100        BANK-AID-PFK04                                            BBANK20P
011200        SET PFK-VALID TO TRUE                                     BBANK20P
011300     END-IF.                                                      BBANK20P
011400     IF BANK-AID-PFK01 AND                                        BBANK20P
011500        BANK-HELP-INACTIVE                                        BBANK20P
011600        SET BANK-HELP-ACTIVE TO TRUE                              BBANK20P
011700        SET PFK-VALID TO TRUE                                     BBANK20P
011800     END-IF.                                                      BBANK20P
011900     IF PFK-INVALID                                               BBANK20P
012000        SET BANK-AID-ENTER TO TRUE                                BBANK20P
012100     END-IF.                                                      BBANK20P
012200                                                                  BBANK20P
012300***************************************************************** BBANK20P
012400* Check the AID to see if we have to quit                       * BBANK20P
012500***************************************************************** BBANK20P
012600     IF BANK-AID-PFK03                                            BBANK20P
012700        MOVE 'BBANK20P' TO BANK-LAST-PROG                         BBANK20P
012800        MOVE 'BBANK99P' TO BANK-NEXT-PROG                         BBANK20P
012900        MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        BBANK20P
013000        MOVE 'BANK99A' TO BANK-NEXT-MAP                           BBANK20P
013100        GO TO COMMON-RETURN                                       BBANK20P
013200     END-IF.                                                      BBANK20P
013300                                                                  BBANK20P
013400***************************************************************** BBANK20P
013500* Check the to see if user needs or has been using help         * BBANK20P
013600***************************************************************** BBANK20P
013700     IF BANK-HELP-ACTIVE                                          BBANK20P
013800        IF BANK-AID-PFK04                                         BBANK20P
013900           SET BANK-HELP-INACTIVE TO TRUE                         BBANK20P
014000           MOVE 00 TO BANK-HELP-SCREEN                            BBANK20P
014100           MOVE 'BBANK20P' TO BANK-LAST-PROG                      BBANK20P
014200           MOVE 'BBANK20P' TO BANK-NEXT-PROG                      BBANK20P
014300           MOVE 'MBANK20' TO BANK-LAST-MAPSET                     BBANK20P
014400           MOVE 'HELP20A' TO BANK-LAST-MAP                        BBANK20P
014500           MOVE 'MBANK20' TO BANK-NEXT-MAPSET                     BBANK20P
014600           MOVE 'BANK20A' TO BANK-NEXT-MAP                        BBANK20P
014700           PERFORM POPULATE-OPTIONS THRU                          BBANK20P
014800                   POPULATE-OPTIONS-EXIT                          BBANK20P
014900           GO TO COMMON-RETURN                                    BBANK20P
015000        ELSE                                                      BBANK20P
015100           MOVE 01 TO BANK-HELP-SCREEN                            BBANK20P
015200           MOVE 'BBANK20P' TO BANK-LAST-PROG                      BBANK20P
015300           MOVE 'BBANK20P' TO BANK-NEXT-PROG                      BBANK20P
015400           MOVE 'MBANK20' TO BANK-LAST-MAPSET                     BBANK20P
015500           MOVE 'BANK20A' TO BANK-LAST-MAP                        BBANK20P
015600           MOVE 'MBANK20' TO BANK-NEXT-MAPSET                     BBANK20P
015700           MOVE 'HELP20A' TO BANK-NEXT-MAP                        BBANK20P
015800           MOVE 'BANK20' TO HELP01I-SCRN                          BBANK20P
015900           COPY CHELPX01.                                         BBANK20P
016000           MOVE HELP01O-DATA TO BANK-HELP-DATA                    BBANK20P
016100           GO TO COMMON-RETURN                                    BBANK20P
016200     END-IF.                                                      BBANK20P
016300                                                                  BBANK20P
016400***************************************************************** BBANK20P
016500* Check the AID to see if we have to return to previous screen  * BBANK20P
016600***************************************************************** BBANK20P
016700     IF BANK-AID-PFK04                                            BBANK20P
016800        MOVE 'BBANK20P' TO BANK-LAST-PROG                         BBANK20P
016900        MOVE 'BBANK10P' TO BANK-NEXT-PROG                         BBANK20P
017000        MOVE 'MBANK10' TO BANK-NEXT-MAPSET                        BBANK20P
017100        MOVE 'BANK10A' TO BANK-NEXT-MAP                           BBANK20P
017200        SET BANK-AID-ENTER TO TRUE                                BBANK20P
017300        SET BANK-NO-CONV-IN-PROGRESS TO TRUE                      BBANK20P
017400        GO TO COMMON-RETURN                                       BBANK20P
017500     END-IF.                                                      BBANK20P
017600                                                                  BBANK20P
017700* Check if we have set the screen up before or is this 1st time   BBANK20P
017800     IF BANK-LAST-MAPSET IS NOT EQUAL TO 'MBANK20'                BBANK20P
017900        MOVE WS-RETURN-MSG TO BANK-ERROR-MSG                      BBANK20P
018000        MOVE 'BBANK20P' TO BANK-LAST-PROG                         BBANK20P
018100        MOVE 'BBANK20P' TO BANK-NEXT-PROG                         BBANK20P
018200        MOVE 'MBANK20' TO BANK-LAST-MAPSET                        BBANK20P
018300        MOVE 'BANK20A' TO BANK-LAST-MAP                           BBANK20P
018400        MOVE 'MBANK20' TO BANK-NEXT-MAPSET                        BBANK20P
018500        MOVE 'BANK20A' TO BANK-NEXT-MAP                           BBANK20P
018600        MOVE LOW-VALUES TO BANK-SCR20-SEL1IP                      BBANK20P
018700        MOVE LOW-VALUES TO BANK-SCR20-SEL2IP                      BBANK20P
018800        MOVE LOW-VALUES TO BANK-SCR20-SEL3IP                      BBANK20P
018900        MOVE LOW-VALUES TO BANK-SCR20-SEL4IP                      BBANK20P
019000        MOVE LOW-VALUES TO BANK-SCR20-SEL5IP                      BBANK20P
019100        IF GUEST                                                  BBANK20P
019200           MOVE 'LI   ' TO WS-SEL-MATRIX                          BBANK20P
019300        ELSE                                                      BBANK20P
019400          MOVE SPACES TO CD08-DATA                                BBANK20P
019500          MOVE BANK-USERID TO CD08I-CONTACT-ID                    BBANK20P
019600* Now go get the data                                             BBANK20P
019700          COPY CBANKX08.                                          BBANK20P
019800          IF CD08O-COUNT IS EQUAL TO 0                            BBANK20P
019900             MOVE 'LI   ' TO WS-SEL-MATRIX                        BBANK20P
020000          END-IF                                                  BBANK20P
020100          IF CD08O-COUNT IS EQUAL TO 1                            BBANK20P
020200             MOVE 'DULI ' TO WS-SEL-MATRIX                        BBANK20P
020300          END-IF                                                  BBANK20P
020400          IF CD08O-COUNT IS GREATER THAN 1                        BBANK20P
020500             MOVE 'DXULI' TO WS-SEL-MATRIX                        BBANK20P
020600          END-IF                                                  BBANK20P
020700        END-IF                                                    BBANK20P
020800        PERFORM POPULATE-OPTIONS THRU                             BBANK20P
020900                POPULATE-OPTIONS-EXIT                             BBANK20P
021000        GO TO COMMON-RETURN                                       BBANK20P
021100     END-IF.                                                      BBANK20P
021200                                                                  BBANK20P
021300     PERFORM VALIDATE-DATA THRU                                   BBANK20P
021400             VALIDATE-DATA-EXIT.                                  BBANK20P
021500                                                                  BBANK20P
021600* If we had an error display error and return                     BBANK20P
021700     IF INPUT-ERROR                                               BBANK20P
021800        MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                       BBANK20P
021900        MOVE 'BBANK20P' TO BANK-LAST-PROG                         BBANK20P
022000        MOVE 'BBANK20P' TO BANK-NEXT-PROG                         BBANK20P
022100        MOVE 'MBANK20' TO BANK-LAST-MAPSET                        BBANK20P
022200        MOVE 'BANK20A' TO BANK-LAST-MAP                           BBANK20P
022300        MOVE 'MBANK20' TO BANK-NEXT-MAPSET                        BBANK20P
022400        MOVE 'BANK20A' TO BANK-NEXT-MAP                           BBANK20P
022500        PERFORM POPULATE-OPTIONS THRU                             BBANK20P
022600                POPULATE-OPTIONS-EXIT                             BBANK20P
022700        GO TO COMMON-RETURN                                       BBANK20P
022800     END-IF.                                                      BBANK20P
022900                                                                  BBANK20P
023000     IF BANK-SCR20-SEL1IP IS NOT EQUAL TO LOW-VALUES              BBANK20P
023100        MOVE BANK-SCR20-SEL1ID TO WS-SEL-OPTION                   BBANK20P
023200     END-IF.                                                      BBANK20P
023300     IF BANK-SCR20-SEL2IP IS NOT EQUAL TO LOW-VALUES              BBANK20P
023400        MOVE BANK-SCR20-SEL2ID TO WS-SEL-OPTION                   BBANK20P
023500     END-IF.                                                      BBANK20P
023600     IF BANK-SCR20-SEL3IP IS NOT EQUAL TO LOW-VALUES              BBANK20P
023700        MOVE BANK-SCR20-SEL3ID TO WS-SEL-OPTION                   BBANK20P
023800     END-IF.                                                      BBANK20P
023900     IF BANK-SCR20-SEL4IP IS NOT EQUAL TO LOW-VALUES              BBANK20P
024000        MOVE BANK-SCR20-SEL4ID TO WS-SEL-OPTION                   BBANK20P
024100     END-IF.                                                      BBANK20P
024200     IF BANK-SCR20-SEL5IP IS NOT EQUAL TO LOW-VALUES              BBANK20P
024300        MOVE BANK-SCR20-SEL5ID TO WS-SEL-OPTION                   BBANK20P
024400     END-IF.                                                      BBANK20P
024500                                                                  BBANK20P
024600     IF WS-SEL-OPTION IS EQUAL TO 'D'                             BBANK20P
024700        MOVE 'BBANK30P' TO BANK-NEXT-PROG                         BBANK20P
024800        GO TO COMMON-RETURN                                       BBANK20P
024900     END-IF.                                                      BBANK20P
025000                                                                  BBANK20P
025100     IF WS-SEL-OPTION IS EQUAL TO 'X'                             BBANK20P
025200        MOVE 'BBANK50P' TO BANK-NEXT-PROG                         BBANK20P
025300        GO TO COMMON-RETURN                                       BBANK20P
025400     END-IF.                                                      BBANK20P
025500                                                                  BBANK20P
025600     IF WS-SEL-OPTION IS EQUAL TO 'U'                             BBANK20P
025700        MOVE 'BBANK60P' TO BANK-NEXT-PROG                         BBANK20P
025800        MOVE SPACES TO BANK-SCREEN60-DATA                         BBANK20P
025900        MOVE BANK-USERID TO BANK-SCR60-CONTACT-ID                 BBANK20P
026000        MOVE BANK-USERID-NAME TO BANK-SCR60-CONTACT-NAME          BBANK20P
026100        SET ADDR-CHANGE-REQUEST TO TRUE                           BBANK20P
026200        GO TO COMMON-RETURN                                       BBANK20P
026300     END-IF.                                                      BBANK20P
026400                                                                  BBANK20P
026500     IF WS-SEL-OPTION IS EQUAL TO 'L'                             BBANK20P
026600        MOVE 'BBANK70P' TO BANK-NEXT-PROG                         BBANK20P
026700        GO TO COMMON-RETURN                                       BBANK20P
026800     END-IF.                                                      BBANK20P
026900                                                                  BBANK20P
027000     IF WS-SEL-OPTION IS EQUAL TO 'I'                             BBANK20P
027100        MOVE 'BBANK80P' TO BANK-NEXT-PROG                         BBANK20P
027200        GO TO COMMON-RETURN                                       BBANK20P
027300     END-IF.                                                      BBANK20P
027400                                                                  BBANK20P
027500***************************************************************** BBANK20P
027600* If we get this far then we have an error in our logic as we   * BBANK20P
027700* don't know where to go next.                                  * BBANK20P
027800***************************************************************** BBANK20P
027900     IF BANK-ENV-CICS                                             BBANK20P
028000        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       BBANK20P
028100        MOVE '0001' TO ABEND-CODE                                 BBANK20P
028200        MOVE SPACES TO ABEND-REASON                               BBANK20P
028300        COPY CABENDPO.                                            BBANK20P
028400     END-IF.                                                      BBANK20P
028500     GOBACK.                                                      BBANK20P
028600                                                                  BBANK20P
028700 COMMON-RETURN.                                                   BBANK20P
028800     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). BBANK20P
028900 COPY CRETURN.                                                    BBANK20P
029000                                                                  BBANK20P
029100 VALIDATE-DATA.                                                   BBANK20P
029200     SET INPUT-OK TO TRUE.                                        BBANK20P
029300     MOVE ZERO TO WS-SEL-COUNT.                                   BBANK20P
029400                                                                  BBANK20P
029500     IF BANK-SCR20-SEL1IP IS NOT EQUAL TO LOW-VALUES              BBANK20P
029600        ADD 1 TO WS-SEL-COUNT                                     BBANK20P
029700     END-IF.                                                      BBANK20P
029800     IF BANK-SCR20-SEL2IP IS NOT EQUAL TO LOW-VALUES              BBANK20P
029900        ADD 1 TO WS-SEL-COUNT                                     BBANK20P
030000     END-IF.                                                      BBANK20P
030100     IF BANK-SCR20-SEL3IP IS NOT EQUAL TO LOW-VALUES              BBANK20P
030200        ADD 1 TO WS-SEL-COUNT                                     BBANK20P
030300     END-IF.                                                      BBANK20P
030400     IF BANK-SCR20-SEL4IP IS NOT EQUAL TO LOW-VALUES              BBANK20P
030500        ADD 1 TO WS-SEL-COUNT                                     BBANK20P
030600     END-IF.                                                      BBANK20P
030700     IF BANK-SCR20-SEL5IP IS NOT EQUAL TO LOW-VALUES              BBANK20P
030800        ADD 1 TO WS-SEL-COUNT                                     BBANK20P
030900     END-IF.                                                      BBANK20P
031000                                                                  BBANK20P
031100     IF WS-SEL-COUNT IS EQUAL TO ZERO                             BBANK20P
031200        MOVE 'Please select an option' TO WS-ERROR-MSG            BBANK20P
031300        GO TO VALIDATE-DATA-ERROR                                 BBANK20P
031400     END-IF.                                                      BBANK20P
031500                                                                  BBANK20P
031600     IF WS-SEL-COUNT IS GREATER THAN 1                            BBANK20P
031700        MOVE 'Please select a single option' TO WS-ERROR-MSG      BBANK20P
031800        GO TO VALIDATE-DATA-ERROR                                 BBANK20P
031900     END-IF.                                                      BBANK20P
032000                                                                  BBANK20P
032100     GO TO VALIDATE-DATA-EXIT.                                    BBANK20P
032200 VALIDATE-DATA-ERROR.                                             BBANK20P
032300     SET INPUT-ERROR TO TRUE.                                     BBANK20P
032400 VALIDATE-DATA-EXIT.                                              BBANK20P
032500     EXIT.                                                        BBANK20P
032600                                                                  BBANK20P
032700 POPULATE-OPTIONS.                                                BBANK20P
032800     MOVE 0 TO WS-SUB1.                                           BBANK20P
032900     DIVIDE LENGTH BANK-SCREEN20-FIELD                            BBANK20P
033000       INTO LENGTH OF BANK-SCREEN20-DATA-R                        BBANK20P
033100         GIVING WS-SUB1-LIMIT.                                    BBANK20P
033200 POPULATE-OPTIONS-LOOP.                                           BBANK20P
033300     ADD 1 TO WS-SUB1.                                            BBANK20P
033400     IF WS-SUB1 IS GREATER THAN WS-SUB1-LIMIT                     BBANK20P
033500        GO TO POPULATE-OPTIONS-EXIT                               BBANK20P
033600     END-IF.                                                      BBANK20P
033700     IF WS-SEL-MATRIX IS NOT EQUAL TO SPACES                      BBANK20P
033800        MOVE WS-SEL-MATRIX (WS-SUB1:1) TO BANK-SCR20-ID (WS-SUB1) BBANK20P
033900     END-IF.                                                      BBANK20P
034000     MOVE SPACES TO BANK-SCR20-TX (WS-SUB1).                      BBANK20P
034100     IF BANK-SCR20-ID (WS-SUB1) IS EQUAL TO 'D'                   BBANK20P
034200        MOVE 'Display your account balances'                      BBANK20P
034300          TO BANK-SCR20-TX (WS-SUB1)                              BBANK20P
034400     END-IF.                                                      BBANK20P
034500     IF BANK-SCR20-ID (WS-SUB1) IS EQUAL TO 'X'                   BBANK20P
034600        MOVE 'Transfer funds between your accounts'               BBANK20P
034700          TO BANK-SCR20-TX (WS-SUB1)                              BBANK20P
034800     END-IF.                                                      BBANK20P
034900     IF BANK-SCR20-ID (WS-SUB1) IS EQUAL TO 'U'                   BBANK20P
035000        MOVE 'Update your contact information'                    BBANK20P
035100          TO BANK-SCR20-TX (WS-SUB1)                              BBANK20P
035200     END-IF.                                                      BBANK20P
035300     IF BANK-SCR20-ID (WS-SUB1) IS EQUAL TO 'L'                   BBANK20P
035400        MOVE 'Calculate the cost of a loan'                       BBANK20P
035500          TO BANK-SCR20-TX (WS-SUB1)                              BBANK20P
035600     END-IF.                                                      BBANK20P
035700     IF BANK-SCR20-ID (WS-SUB1) IS EQUAL TO 'I'                   BBANK20P
035800        MOVE 'Obtain more information'                            BBANK20P
035900          TO BANK-SCR20-TX (WS-SUB1)                              BBANK20P
036000     END-IF.                                                      BBANK20P
036100     GO TO POPULATE-OPTIONS-LOOP.                                 BBANK20P
036200 POPULATE-OPTIONS-EXIT.                                           BBANK20P
036300     EXIT.                                                        BBANK20P
036400                                                                  BBANK20P
