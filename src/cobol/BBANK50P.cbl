000100***************************************************************** bbank50p
000200*                                                               * bbank50p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * bbank50p
000400*   This demonstration program is provided for use by users     * bbank50p
000500*   of Micro Focus products and may be used, modified and       * bbank50p
000600*   distributed as part of your application provided that       * bbank50p
000700*   you properly acknowledge the copyright of Micro Focus       * bbank50p
000800*   in this material.                                           * bbank50p
000900*                                                               * bbank50p
001000***************************************************************** bbank50p
001100                                                                  bbank50p
001200***************************************************************** bbank50p
001300* Program:     BBANK50P.CBL                                     * bbank50p
001400* Layer:       Business logic                                   * bbank50p
001500* Function:    Transfer funds between accounts                  * bbank50p
001600***************************************************************** bbank50p
001700                                                                  bbank50p
001800 IDENTIFICATION DIVISION.                                         bbank50p
001900 PROGRAM-ID.                                                      bbank50p
002000     BBANK50P.                                                    bbank50p
002100 DATE-WRITTEN.                                                    bbank50p
002200     September 2002.                                              bbank50p
002300 DATE-COMPILED.                                                   bbank50p
002400     Today.                                                       bbank50p
002500                                                                  bbank50p
002600 ENVIRONMENT DIVISION.                                            bbank50p
002700                                                                  bbank50p
002800 DATA DIVISION.                                                   bbank50p
002900 WORKING-STORAGE SECTION.                                         bbank50p
003000 01  WS-MISC-STORAGE.                                             bbank50p
003100   05  WS-PROGRAM-ID                         PIC X(8)             bbank50p
003200       VALUE 'BBANK50P'.                                          bbank50p
003300   05  WS-INPUT-FLAG                         PIC X(1).            bbank50p
003400     88  INPUT-OK                            VALUE '0'.           bbank50p
003500     88  INPUT-ERROR                         VALUE '1'.           bbank50p
003600   05  WS-RETURN-FLAG                        PIC X(1).            bbank50p
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    bbank50p
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           bbank50p
003900   05  WS-RETURN-MSG                         PIC X(75).           bbank50p
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        bbank50p
004100   05  WS-PFK-FLAG                           PIC X(1).            bbank50p
004200     88  PFK-VALID                           VALUE '0'.           bbank50p
004300     88  PFK-INVALID                         VALUE '1'.           bbank50p
004400   05  WS-ERROR-MSG                          PIC X(75).           bbank50p
004500   05  WS-EDIT-BALANCE                       PIC Z,ZZZ,ZZ9.99-.   bbank50p
004600   05  WS-SEL-COUNT                          PIC 9(1).            bbank50p
004700   05  WS-XFER-AMT                           PIC X(8).            bbank50p
004800   05  WS-XFER-AMT-TMP                       PIC X(9).            bbank50p
004900   05  WS-XFER-AMT-TMP-N REDEFINES WS-XFER-AMT-TMP                bbank50p
005000                                             PIC S9(7)V99.        bbank50p
005100   05  WS-XFER-AMT-NUM                       PIC X(9).            bbank50p
005200   05  WS-XFER-AMT-NUM-N REDEFINES WS-XFER-AMT-NUM                bbank50p
005300                                             PIC S9(7)V99.        bbank50p
005400   05  WS-XFER-ACCT-FROM                     PIC X(9).            bbank50p
005500   05  WS-XFER-ACCT-FROM-BAL                 PIC X(13).           bbank50p
005600   05  WS-XFER-ACCT-FROM-BAL-N               PIC S9(9)V99.        bbank50p
005700   05  WS-XFER-ACCT-FROM-NEW-BAL-N           PIC S9(9)V99.        bbank50p
005800   05  WS-XFER-ACCT-TO                       PIC X(9).            bbank50p
005900   05  WS-XFER-ACCT-TO-BAL                   PIC X(13).           bbank50p
006000   05  WS-XFER-ACCT-TO-BAL-N                 PIC S9(9)V99.        bbank50p
006100   05  WS-XFER-ACCT-TO-NEW-BAL-N             PIC S9(9)V999.       bbank50p
006200                                                                  bbank50p
006300 01  WS-BANK-DATA.                                                bbank50p
006400 COPY CBANKDAT.                                                   bbank50p
006500                                                                  bbank50p
006600 01  WS-HELP-DATA.                                                bbank50p
006700 COPY CHELPD01.                                                   bbank50p
006800                                                                  bbank50p
006900 01  WS-ACCT-DATA.                                                bbank50p
007000 COPY CBANKD03.                                                   bbank50p
007100                                                                  bbank50p
007200 01  WS-XFER-DATA.                                                bbank50p
007300 COPY CBANKD04.                                                   bbank50p
007400                                                                  bbank50p
007500 01  WS-TXN-DATA.                                                 bbank50p
007600 COPY CBANKD06.                                                   bbank50p
007700                                                                  bbank50p
007800 COPY CABENDD.                                                    bbank50p
007900                                                                  bbank50p
008000 LINKAGE SECTION.                                                 bbank50p
008100 01  DFHCOMMAREA.                                                 bbank50p
008200   05  LK-COMMAREA                           PIC X(6144).         bbank50p
008300                                                                  bbank50p
008400 COPY CENTRY.                                                     bbank50p
008500***************************************************************** bbank50p
008600* Make ourselves re-entrant                                     * bbank50p
008700***************************************************************** bbank50p
008800     MOVE SPACES TO WS-ERROR-MSG.                                 bbank50p
008900                                                                  bbank50p
009000***************************************************************** bbank50p
009100* Move the passed area to Bour area                              *bbank50p
009200***************************************************************** bbank50p
009300     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. bbank50p
009400                                                                  bbank50p
009500***************************************************************** bbank50p
009600* Ensure error message is cleared                               * bbank50p
009700***************************************************************** bbank50p
009800     MOVE SPACES TO BANK-ERROR-MSG.                               bbank50p
009900                                                                  bbank50p
010000***************************************************************** bbank50p
010100* This is the main process                                      * bbank50p
010200***************************************************************** bbank50p
010300                                                                  bbank50p
010400***************************************************************** bbank50p
010500* Save the passed return message and then turn it off           * bbank50p
010600***************************************************************** bbank50p
010700     MOVE BANK-RETURN-MSG TO WS-RETURN-MSG.                       bbank50p
010800     SET BANK-RETURN-MSG-OFF TO TRUE.                             bbank50p
010900                                                                  bbank50p
011000***************************************************************** bbank50p
011100* Check the AID to see if its valid at this point               * bbank50p
011200***************************************************************** bbank50p
011300     SET PFK-INVALID TO TRUE.                                     bbank50p
011400     IF BANK-AID-ENTER OR                                         bbank50p
011500        BANK-AID-PFK03 OR                                         bbank50p
011600        BANK-AID-PFK04                                            bbank50p
011700        SET PFK-VALID TO TRUE                                     bbank50p
011800     END-IF.                                                      bbank50p
011900     IF BANK-AID-PFK01 AND                                        bbank50p
012000        BANK-HELP-INACTIVE                                        bbank50p
012100        SET BANK-HELP-ACTIVE TO TRUE                              bbank50p
012200        SET PFK-VALID TO TRUE                                     bbank50p
012300     END-IF.                                                      bbank50p
012400     IF PFK-INVALID                                               bbank50p
012500        SET BANK-AID-ENTER TO TRUE                                bbank50p
012600     END-IF.                                                      bbank50p
012700                                                                  bbank50p
012800***************************************************************** bbank50p
012900* Check the AID to see if we have to quit                       * bbank50p
013000***************************************************************** bbank50p
013100     IF BANK-AID-PFK03                                            bbank50p
013200        MOVE 'BBANK50P' TO BANK-LAST-PROG                         bbank50p
013300        MOVE 'BBANK99P' TO BANK-NEXT-PROG                         bbank50p
013400        MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        bbank50p
013500        MOVE 'BANK99A' TO BANK-NEXT-MAP                           bbank50p
013600        GO TO COMMON-RETURN                                       bbank50p
013700     END-IF.                                                      bbank50p
013800                                                                  bbank50p
013900***************************************************************** bbank50p
014000* Check the to see if user needs or has been using help         * bbank50p
014100***************************************************************** bbank50p
014200     IF BANK-HELP-ACTIVE                                          bbank50p
014300        IF BANK-AID-PFK04                                         bbank50p
014400           SET BANK-HELP-INACTIVE TO TRUE                         bbank50p
014500           MOVE 00 TO BANK-HELP-SCREEN                            bbank50p
014600           MOVE 'BBANK50P' TO BANK-LAST-PROG                      bbank50p
014700           MOVE 'BBANK50P' TO BANK-NEXT-PROG                      bbank50p
014800           MOVE 'MBANK50' TO BANK-LAST-MAPSET                     bbank50p
014900           MOVE 'HELP50A' TO BANK-LAST-MAP                        bbank50p
015000           MOVE 'MBANK50' TO BANK-NEXT-MAPSET                     bbank50p
015100           MOVE 'BANK50A' TO BANK-NEXT-MAP                        bbank50p
015200           GO TO COMMON-RETURN                                    bbank50p
015300        ELSE                                                      bbank50p
015400           MOVE 01 TO BANK-HELP-SCREEN                            bbank50p
015500           MOVE 'BBANK50P' TO BANK-LAST-PROG                      bbank50p
015600           MOVE 'BBANK50P' TO BANK-NEXT-PROG                      bbank50p
015700           MOVE 'MBANK50' TO BANK-LAST-MAPSET                     bbank50p
015800           MOVE 'BANK50A' TO BANK-LAST-MAP                        bbank50p
015900           MOVE 'MBANK50' TO BANK-NEXT-MAPSET                     bbank50p
016000           MOVE 'HELP50A' TO BANK-NEXT-MAP                        bbank50p
016100           MOVE 'BANK50' TO HELP01I-SCRN                          bbank50p
016200           COPY CHELPX01.                                         bbank50p
016300           MOVE HELP01O-DATA TO BANK-HELP-DATA                    bbank50p
016400           GO TO COMMON-RETURN                                    bbank50p
016500     END-IF.                                                      bbank50p
016600                                                                  bbank50p
016700***************************************************************** bbank50p
016800* Check the AID to see if we have to return to previous screen  * bbank50p
016900***************************************************************** bbank50p
017000     IF BANK-AID-PFK04                                            bbank50p
017100        MOVE 'BBANK50P' TO BANK-LAST-PROG                         bbank50p
017200        MOVE 'BBANK20P' TO BANK-NEXT-PROG                         bbank50p
017300        MOVE 'MBANK20' TO BANK-NEXT-MAPSET                        bbank50p
017400        MOVE 'BANK20A' TO BANK-NEXT-MAP                           bbank50p
017500        SET BANK-AID-ENTER TO TRUE                                bbank50p
017600        GO TO COMMON-RETURN                                       bbank50p
017700     END-IF.                                                      bbank50p
017800                                                                  bbank50p
017900* Check if we have set the screen up before or is this 1st time   bbank50p
018000     IF BANK-LAST-MAPSET IS NOT EQUAL TO 'MBANK50'                bbank50p
018100        MOVE SPACES TO BANK-SCR50-XFER                            bbank50p
018200        MOVE '_' TO BANK-SCR50-FRM1                               bbank50p
018300        MOVE '_' TO BANK-SCR50-FRM2                               bbank50p
018400        MOVE '_' TO BANK-SCR50-FRM3                               bbank50p
018500        MOVE '_' TO BANK-SCR50-FRM4                               bbank50p
018600        MOVE '_' TO BANK-SCR50-FRM5                               bbank50p
018700        MOVE '_' TO BANK-SCR50-FRM6                               bbank50p
018800        MOVE '_' TO BANK-SCR50-TO1                                bbank50p
018900        MOVE '_' TO BANK-SCR50-TO2                                bbank50p
019000        MOVE '_' TO BANK-SCR50-TO3                                bbank50p
019100        MOVE '_' TO BANK-SCR50-TO4                                bbank50p
019200        MOVE '_' TO BANK-SCR50-TO5                                bbank50p
019300        MOVE '_' TO BANK-SCR50-TO6                                bbank50p
019400        MOVE WS-RETURN-MSG TO BANK-ERROR-MSG                      bbank50p
019500        MOVE 'BBANK50P' TO BANK-LAST-PROG                         bbank50p
019600        MOVE 'BBANK50P' TO BANK-NEXT-PROG                         bbank50p
019700        MOVE 'MBANK50' TO BANK-LAST-MAPSET                        bbank50p
019800        MOVE 'BANK50A' TO BANK-LAST-MAP                           bbank50p
019900        MOVE 'MBANK50' TO BANK-NEXT-MAPSET                        bbank50p
020000        MOVE 'BANK50A' TO BANK-NEXT-MAP                           bbank50p
020100        PERFORM POPULATE-SCREEN-DATA THRU                         bbank50p
020200                POPULATE-SCREEN-DATA-EXIT                         bbank50p
020300        GO TO COMMON-RETURN                                       bbank50p
020400     END-IF.                                                      bbank50p
020500                                                                  bbank50p
020600     PERFORM VALIDATE-DATA THRU                                   bbank50p
020700             VALIDATE-DATA-EXIT.                                  bbank50p
020800                                                                  bbank50p
020900* If we had an error display error and return                     bbank50p
021000     IF INPUT-ERROR                                               bbank50p
021100        MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                       bbank50p
021200        MOVE 'BBANK50P' TO BANK-LAST-PROG                         bbank50p
021300        MOVE 'BBANK50P' TO BANK-NEXT-PROG                         bbank50p
021400        MOVE 'MBANK50' TO BANK-LAST-MAPSET                        bbank50p
021500        MOVE 'BANK50A' TO BANK-LAST-MAP                           bbank50p
021600        MOVE 'MBANK50' TO BANK-NEXT-MAPSET                        bbank50p
021700        MOVE 'BANK50A' TO BANK-NEXT-MAP                           bbank50p
021800        GO TO COMMON-RETURN                                       bbank50p
021900     END-IF.                                                      bbank50p
022000                                                                  bbank50p
022100***************************************************************** bbank50p
022200* If we paying money to the bank (account 99999999n) then we    * bbank50p
022300* don't know the bank's balance so we pass the transfer amount  * bbank50p
022400* to the I/O routine and let it adjust the balance without      * bbank50p
022500* checking it first.                                            * bbank50p
022600***************************************************************** bbank50p
022700     MOVE SPACES TO CD04-DATA.                                    bbank50p
022800     MOVE BANK-USERID TO CD04I-FROM-PID.                          bbank50p
022900     MOVE WS-XFER-ACCT-FROM TO CD04I-FROM-ACC.                    bbank50p
023000     MOVE WS-XFER-ACCT-FROM-BAL-N TO CD04I-FROM-OLD-BAL.          bbank50p
023100     SUBTRACT WS-XFER-AMT-NUM-N FROM WS-XFER-ACCT-FROM-BAL-N      bbank50p
023200       GIVING WS-XFER-ACCT-FROM-NEW-BAL-N.                        bbank50p
023300     MOVE WS-XFER-ACCT-FROM-NEW-BAL-N TO CD04I-FROM-NEW-BAL.      bbank50p
023400     IF WS-XFER-ACCT-TO(1:8) IS EQUAL TO '99999999'               bbank50p
023500        MOVE 'BANK ' TO CD04I-TO-PID                              bbank50p
023600        MOVE WS-XFER-ACCT-TO TO CD04I-TO-ACC                      bbank50p
023700        MOVE WS-XFER-ACCT-TO-BAL-N TO CD04I-TO-OLD-BAL            bbank50p
023800        COMPUTE WS-XFER-ACCT-TO-NEW-BAL-N =                       bbank50p
023900                WS-XFER-AMT-NUM-N                                 bbank50p
024000     ELSE                                                         bbank50p
024100        MOVE BANK-USERID TO CD04I-TO-PID                          bbank50p
024200        MOVE WS-XFER-ACCT-TO TO CD04I-TO-ACC                      bbank50p
024300        MOVE WS-XFER-ACCT-TO-BAL-N TO CD04I-TO-OLD-BAL            bbank50p
024400        COMPUTE WS-XFER-ACCT-TO-NEW-BAL-N =                       bbank50p
024500                WS-XFER-AMT-NUM-N + WS-XFER-ACCT-TO-BAL-N         bbank50p
024600     END-IF.                                                      bbank50p
024700     MOVE WS-XFER-ACCT-TO-NEW-BAL-N TO CD04I-TO-NEW-BAL.          bbank50p
024800     MOVE WS-XFER-ACCT-FROM-BAL-N TO CD04I-FROM-OLD-BAL.          bbank50p
024900* Now go attempt to update the data                               bbank50p
025000 COPY CBANKX04.                                                   bbank50p
025100     IF NOT CD04O-UPDATE-OK                                       bbank50p
025200        MOVE 'Unable to transfer funds. Update failed.'           bbank50p
025300          TO BANK-ERROR-MSG                                       bbank50p
025400     ELSE                                                         bbank50p
025500        MOVE SPACES TO BANK-ERROR-MSG                             bbank50p
025600        STRING 'Transferred ' DELIMITED BY SIZE                   bbank50p
025700               BANK-SCR50-XFER DELIMITED BY SIZE                  bbank50p
025800               ' from ' DELIMITED BY SIZE                         bbank50p
025900               WS-XFER-ACCT-FROM DELIMITED BY SIZE                bbank50p
026000               ' to ' DELIMITED BY SIZE                           bbank50p
026100               WS-XFER-ACCT-TO DELIMITED BY SIZE                  bbank50p
026200          INTO BANK-ERROR-MSG                                     bbank50p
026300* Now produce the audit trail                                     bbank50p
026400        MOVE SPACES TO CD06-DATA                                  bbank50p
026500        MOVE CD04O-TIMESTAMP TO CD06I-TIMESTAMP                   bbank50p
026600        MOVE BANK-USERID TO CD06I-FROM-PID                        bbank50p
026700        MOVE CD04I-FROM-ACC TO CD06I-FROM-ACC                     bbank50p
026800        MULTIPLY WS-XFER-AMT-NUM-N BY -1                          bbank50p
026900          GIVING CD06I-FROM-AMOUNT                                bbank50p
027000        STRING 'Transferred to a/c ' DELIMITED BY SIZE            bbank50p
027100               CD04I-TO-ACC DELIMITED BY SIZE                     bbank50p
027200          INTO CD06I-FROM-DESC                                    bbank50p
027300        MOVE BANK-USERID TO CD06I-TO-PID                          bbank50p
027400        MOVE CD04I-TO-ACC TO CD06I-TO-ACC                         bbank50p
027500        MULTIPLY WS-XFER-AMT-NUM-N BY +1                          bbank50p
027600          GIVING CD06I-TO-AMOUNT                                  bbank50p
027700        STRING 'Transferred from a/c ' DELIMITED BY SIZE          bbank50p
027800               CD04I-FROM-ACC DELIMITED BY SIZE                   bbank50p
027900          INTO CD06I-TO-DESC                                      bbank50p
028000 COPY CBANKX06.                                                   bbank50p
028100                                                                  bbank50p
028200        MOVE SPACES TO BANK-SCR50-XFER                            bbank50p
028300        MOVE '_' TO BANK-SCR50-FRM1                               bbank50p
028400        MOVE '_' TO BANK-SCR50-FRM2                               bbank50p
028500        MOVE '_' TO BANK-SCR50-FRM3                               bbank50p
028600        MOVE '_' TO BANK-SCR50-FRM4                               bbank50p
028700        MOVE '_' TO BANK-SCR50-FRM5                               bbank50p
028800        MOVE '_' TO BANK-SCR50-FRM6                               bbank50p
028900        MOVE '_' TO BANK-SCR50-TO1                                bbank50p
029000        MOVE '_' TO BANK-SCR50-TO2                                bbank50p
029100        MOVE '_' TO BANK-SCR50-TO3                                bbank50p
029200        MOVE '_' TO BANK-SCR50-TO4                                bbank50p
029300        MOVE '_' TO BANK-SCR50-TO5                                bbank50p
029400        MOVE '_' TO BANK-SCR50-TO6                                bbank50p
029500     END-IF.                                                      bbank50p
029600                                                                  bbank50p
029700     PERFORM POPULATE-SCREEN-DATA THRU                            bbank50p
029800             POPULATE-SCREEN-DATA-EXIT.                           bbank50p
029900                                                                  bbank50p
030000     GO TO COMMON-RETURN.                                         bbank50p
030100                                                                  bbank50p
030200 COMMON-RETURN.                                                   bbank50p
030300     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). bbank50p
030400 COPY CRETURN.                                                    bbank50p
030500                                                                  bbank50p
030600 VALIDATE-DATA.                                                   bbank50p
030700     SET INPUT-OK TO TRUE.                                        bbank50p
030800                                                                  bbank50p
030900     MOVE BANK-SCR50-XFER TO WS-XFER-AMT.                         bbank50p
031000     PERFORM VALIDATE-XFER THRU                                   bbank50p
031100             VALIDATE-XFER-EXIT.                                  bbank50p
031200     IF NOT INPUT-OK                                              bbank50p
031300        GO TO VALIDATE-DATA-ERROR                                 bbank50p
031400     END-IF.                                                      bbank50p
031500                                                                  bbank50p
031600     MOVE ZERO TO WS-SEL-COUNT.                                   bbank50p
031700                                                                  bbank50p
031800     MOVE SPACES TO WS-XFER-ACCT-FROM.                            bbank50p
031900     MOVE SPACES TO WS-XFER-ACCT-FROM-BAL.                        bbank50p
032000     MOVE SPACES TO WS-XFER-ACCT-TO.                              bbank50p
032100                                                                  bbank50p
032200     IF BANK-SCR50-FRM1 IS NOT EQUAL TO LOW-VALUES                bbank50p
032300        ADD 1 TO WS-SEL-COUNT                                     bbank50p
032400        MOVE BANK-SCR50-ACC1 TO WS-XFER-ACCT-FROM                 bbank50p
032500        MOVE BANK-SCR50-BAL1 TO WS-XFER-ACCT-FROM-BAL             bbank50p
032600     END-IF.                                                      bbank50p
032700     IF BANK-SCR50-FRM2 IS NOT EQUAL TO LOW-VALUES                bbank50p
032800        ADD 1 TO WS-SEL-COUNT                                     bbank50p
032900        MOVE BANK-SCR50-ACC2 TO WS-XFER-ACCT-FROM                 bbank50p
033000        MOVE BANK-SCR50-BAL2 TO WS-XFER-ACCT-FROM-BAL             bbank50p
033100     END-IF.                                                      bbank50p
033200     IF BANK-SCR50-FRM3 IS NOT EQUAL TO LOW-VALUES                bbank50p
033300        ADD 1 TO WS-SEL-COUNT                                     bbank50p
033400        MOVE BANK-SCR50-ACC3 TO WS-XFER-ACCT-FROM                 bbank50p
033500        MOVE BANK-SCR50-BAL3 TO WS-XFER-ACCT-FROM-BAL             bbank50p
033600     END-IF.                                                      bbank50p
033700     IF BANK-SCR50-FRM4 IS NOT EQUAL TO LOW-VALUES                bbank50p
033800        ADD 1 TO WS-SEL-COUNT                                     bbank50p
033900        MOVE BANK-SCR50-ACC4 TO WS-XFER-ACCT-FROM                 bbank50p
034000        MOVE BANK-SCR50-BAL4 TO WS-XFER-ACCT-FROM-BAL             bbank50p
034100     END-IF.                                                      bbank50p
034200     IF BANK-SCR50-FRM5 IS NOT EQUAL TO LOW-VALUES                bbank50p
034300        ADD 1 TO WS-SEL-COUNT                                     bbank50p
034400        MOVE BANK-SCR50-ACC5 TO WS-XFER-ACCT-FROM                 bbank50p
034500        MOVE BANK-SCR50-BAL5 TO WS-XFER-ACCT-FROM-BAL             bbank50p
034600     END-IF.                                                      bbank50p
034700     IF BANK-SCR50-FRM6 IS NOT EQUAL TO LOW-VALUES                bbank50p
034800        ADD 1 TO WS-SEL-COUNT                                     bbank50p
034900        MOVE BANK-SCR50-ACC6 TO WS-XFER-ACCT-FROM                 bbank50p
035000        MOVE BANK-SCR50-BAL6 TO WS-XFER-ACCT-FROM-BAL             bbank50p
035100     END-IF.                                                      bbank50p
035200                                                                  bbank50p
035300     IF WS-SEL-COUNT IS EQUAL TO ZERO                             bbank50p
035400        MOVE 'Please select an account to transfer from'          bbank50p
035500          TO WS-ERROR-MSG                                         bbank50p
035600        GO TO VALIDATE-DATA-ERROR                                 bbank50p
035700     END-IF.                                                      bbank50p
035800                                                                  bbank50p
035900     IF WS-SEL-COUNT IS GREATER THAN 1                            bbank50p
036000        MOVE 'Please select a single account to transfer from'    bbank50p
036100          TO WS-ERROR-MSG                                         bbank50p
036200        GO TO VALIDATE-DATA-ERROR                                 bbank50p
036300     END-IF.                                                      bbank50p
036400                                                                  bbank50p
036500     MOVE ZERO TO WS-SEL-COUNT.                                   bbank50p
036600                                                                  bbank50p
036700     IF BANK-SCR50-TO1 IS NOT EQUAL TO LOW-VALUES                 bbank50p
036800        ADD 1 TO WS-SEL-COUNT                                     bbank50p
036900        MOVE BANK-SCR50-ACC1 TO WS-XFER-ACCT-TO                   bbank50p
037000        MOVE BANK-SCR50-BAL1 TO WS-XFER-ACCT-TO-BAL               bbank50p
037100     END-IF.                                                      bbank50p
037200     IF BANK-SCR50-TO2 IS NOT EQUAL TO LOW-VALUES                 bbank50p
037300        ADD 1 TO WS-SEL-COUNT                                     bbank50p
037400        MOVE BANK-SCR50-ACC2 TO WS-XFER-ACCT-TO                   bbank50p
037500        MOVE BANK-SCR50-BAL2 TO WS-XFER-ACCT-TO-BAL               bbank50p
037600     END-IF.                                                      bbank50p
037700     IF BANK-SCR50-TO3 IS NOT EQUAL TO LOW-VALUES                 bbank50p
037800        ADD 1 TO WS-SEL-COUNT                                     bbank50p
037900        MOVE BANK-SCR50-ACC3 TO WS-XFER-ACCT-TO                   bbank50p
038000        MOVE BANK-SCR50-BAL3 TO WS-XFER-ACCT-TO-BAL               bbank50p
038100     END-IF.                                                      bbank50p
038200     IF BANK-SCR50-TO4 IS NOT EQUAL TO LOW-VALUES                 bbank50p
038300        ADD 1 TO WS-SEL-COUNT                                     bbank50p
038400        MOVE BANK-SCR50-ACC4 TO WS-XFER-ACCT-TO                   bbank50p
038500        MOVE BANK-SCR50-BAL4 TO WS-XFER-ACCT-TO-BAL               bbank50p
038600     END-IF.                                                      bbank50p
038700     IF BANK-SCR50-TO5 IS NOT EQUAL TO LOW-VALUES                 bbank50p
038800        ADD 1 TO WS-SEL-COUNT                                     bbank50p
038900        MOVE BANK-SCR50-ACC5 TO WS-XFER-ACCT-TO                   bbank50p
039000        MOVE BANK-SCR50-BAL5 TO WS-XFER-ACCT-TO-BAL               bbank50p
039100     END-IF.                                                      bbank50p
039200     IF BANK-SCR50-TO6 IS NOT EQUAL TO LOW-VALUES                 bbank50p
039300        ADD 1 TO WS-SEL-COUNT                                     bbank50p
039400        MOVE BANK-SCR50-ACC6 TO WS-XFER-ACCT-TO                   bbank50p
039500        MOVE BANK-SCR50-BAL6 TO WS-XFER-ACCT-TO-BAL               bbank50p
039600     END-IF.                                                      bbank50p
039700                                                                  bbank50p
039800     IF WS-SEL-COUNT IS EQUAL TO ZERO                             bbank50p
039900        MOVE 'Please select an account to transfer to'            bbank50p
040000          TO WS-ERROR-MSG                                         bbank50p
040100        GO TO VALIDATE-DATA-ERROR                                 bbank50p
040200     END-IF.                                                      bbank50p
040300                                                                  bbank50p
040400     IF WS-SEL-COUNT IS GREATER THAN 1                            bbank50p
040500        MOVE 'Please select a single account to transfer to'      bbank50p
040600          TO WS-ERROR-MSG                                         bbank50p
040700        GO TO VALIDATE-DATA-ERROR                                 bbank50p
040800     END-IF.                                                      bbank50p
040900                                                                  bbank50p
041000     IF WS-XFER-ACCT-FROM IS EQUAL TO WS-XFER-ACCT-TO             bbank50p
041100        MOVE 'Please select an different to & from accounts'      bbank50p
041200          TO WS-ERROR-MSG                                         bbank50p
041300        GO TO VALIDATE-DATA-ERROR                                 bbank50p
041400     END-IF.                                                      bbank50p
041500                                                                  bbank50p
041600* Reformat balance in from a/c from 9,999,999.99- to 999999999.   bbank50p
041700     MOVE WS-XFER-ACCT-FROM-BAL (1:1) TO WS-XFER-AMT-TMP (1:1).   bbank50p
041800     MOVE WS-XFER-ACCT-FROM-BAL (3:3) TO WS-XFER-AMT-TMP (2:3).   bbank50p
041900     MOVE WS-XFER-ACCT-FROM-BAL (7:3) TO WS-XFER-AMT-TMP (5:3).   bbank50p
042000     MOVE WS-XFER-ACCT-FROM-BAL (11:2) TO WS-XFER-AMT-TMP (8:2).  bbank50p
042100     INSPECT WS-XFER-AMT-TMP REPLACING LEADING SPACES BY ZEROS.   bbank50p
042200     IF WS-XFER-ACCT-FROM-BAL (13:1) IS EQUAL TO '-'              bbank50p
042300        MULTIPLY -1 BY WS-XFER-AMT-TMP-N                          bbank50p
042400          GIVING WS-XFER-ACCT-FROM-BAL-N                          bbank50p
042500     ELSE                                                         bbank50p
042600        MULTIPLY +1 BY WS-XFER-AMT-TMP-N                          bbank50p
042700          GIVING WS-XFER-ACCT-FROM-BAL-N                          bbank50p
042800     END-IF.                                                      bbank50p
042900* Reformat balance in to a/c from 9,999,999.99- to 999999999.     bbank50p
043000     MOVE WS-XFER-ACCT-TO-BAL (1:1) TO WS-XFER-AMT-TMP (1:1).     bbank50p
043100     MOVE WS-XFER-ACCT-TO-BAL (3:3) TO WS-XFER-AMT-TMP (2:3).     bbank50p
043200     MOVE WS-XFER-ACCT-TO-BAL (7:3) TO WS-XFER-AMT-TMP (5:3).     bbank50p
043300     MOVE WS-XFER-ACCT-TO-BAL (11:2) TO WS-XFER-AMT-TMP (8:2).    bbank50p
043400     INSPECT WS-XFER-AMT-TMP REPLACING LEADING SPACES BY ZEROS.   bbank50p
043500     IF WS-XFER-ACCT-TO-BAL (13:1) IS EQUAL TO '-'                bbank50p
043600        MULTIPLY -1 BY WS-XFER-AMT-TMP-N                          bbank50p
043700          GIVING WS-XFER-ACCT-TO-BAL-N                            bbank50p
043800     ELSE                                                         bbank50p
043900        MULTIPLY +1 BY WS-XFER-AMT-TMP-N                          bbank50p
044000          GIVING WS-XFER-ACCT-TO-BAL-N                            bbank50p
044100     END-IF.                                                      bbank50p
044200     IF WS-XFER-ACCT-FROM-BAL-N IS LESS THAN ZERO                 bbank50p
044300        MOVE 'Cannot transfer from a negative balance'            bbank50p
044400          TO WS-ERROR-MSG                                         bbank50p
044500        GO TO VALIDATE-DATA-ERROR                                 bbank50p
044600     END-IF.                                                      bbank50p
044700     IF WS-XFER-AMT-NUM-N IS GREATER THAN WS-XFER-ACCT-FROM-BAL-N bbank50p
044800        MOVE 'Insufficient funds in from account'                 bbank50p
044900          TO WS-ERROR-MSG                                         bbank50p
045000        GO TO VALIDATE-DATA-ERROR                                 bbank50p
045100     END-IF.                                                      bbank50p
045200                                                                  bbank50p
045300     GO TO VALIDATE-DATA-EXIT.                                    bbank50p
045400                                                                  bbank50p
045500 VALIDATE-DATA-ERROR.                                             bbank50p
045600     SET INPUT-ERROR TO TRUE.                                     bbank50p
045700 VALIDATE-DATA-EXIT.                                              bbank50p
045800     EXIT.                                                        bbank50p
045900                                                                  bbank50p
046000 VALIDATE-XFER.                                                   bbank50p
046100     CONTINUE.                                                    bbank50p
046200 VALIDATE-XFER-RIGHT-JUSTIFY.                                     bbank50p
046300     IF WS-XFER-AMT IS EQUAL TO SPACES OR                         bbank50p
046400        WS-XFER-AMT IS EQUAL TO LOW-VALUES                        bbank50p
046500        MOVE 'Please enter transfer amount'                       bbank50p
046600          TO WS-ERROR-MSG                                         bbank50p
046700        GO TO VALIDATE-XFER-ERROR                                 bbank50p
046800     END-IF.                                                      bbank50p
046900     IF WS-XFER-AMT (8:1) IS EQUAL TO SPACE OR                    bbank50p
047000        WS-XFER-AMT (8:1) IS EQUAL TO LOW-VALUE                   bbank50p
047100        MOVE WS-XFER-AMT (1:7) TO WS-XFER-AMT-TMP                 bbank50p
047200        MOVE SPACES TO WS-XFER-AMT                                bbank50p
047300        MOVE WS-XFER-AMT-TMP TO WS-XFER-AMT (2:7)                 bbank50p
047400        GO TO VALIDATE-XFER-RIGHT-JUSTIFY                         bbank50p
047500     END-IF.                                                      bbank50p
047600     IF WS-XFER-AMT (6:1) IS NOT EQUAL TO '.'                     bbank50p
047700        MOVE 'Period missing/misplaced in transfer amount'        bbank50p
047800          TO WS-ERROR-MSG                                         bbank50p
047900        GO TO VALIDATE-XFER-ERROR                                 bbank50p
048000     END-IF.                                                      bbank50p
048100     MOVE SPACES TO WS-XFER-AMT-NUM.                              bbank50p
048200     MOVE WS-XFER-AMT (1:5) TO WS-XFER-AMT-NUM (3:5).             bbank50p
048300     MOVE WS-XFER-AMT (7:2) TO WS-XFER-AMT-NUM (8:2).             bbank50p
048400     INSPECT WS-XFER-AMT-NUM REPLACING LEADING SPACES BY ZEROS.   bbank50p
048500     IF WS-XFER-AMT-NUM IS NOT NUMERIC                            bbank50p
048600        MOVE 'Transfer amount is invalid (not numeric)'           bbank50p
048700          TO WS-ERROR-MSG                                         bbank50p
048800        GO TO VALIDATE-XFER-ERROR                                 bbank50p
048900     END-IF.                                                      bbank50p
049000     IF WS-XFER-AMT-NUM IS EQUAL TO ZERO                          bbank50p
049100        MOVE 'Please enter a non-zero transfer amount'            bbank50p
049200          TO WS-ERROR-MSG                                         bbank50p
049300        GO TO VALIDATE-XFER-ERROR                                 bbank50p
049400     END-IF.                                                      bbank50p
049500                                                                  bbank50p
049600     GO TO VALIDATE-XFER-EXIT.                                    bbank50p
049700                                                                  bbank50p
049800 VALIDATE-XFER-ERROR.                                             bbank50p
049900     SET INPUT-ERROR TO TRUE.                                     bbank50p
050000 VALIDATE-XFER-EXIT.                                              bbank50p
050100     EXIT.                                                        bbank50p
050200                                                                  bbank50p
050300 POPULATE-SCREEN-DATA.                                            bbank50p
050400     MOVE SPACES TO CD03-DATA.                                    bbank50p
050500     MOVE BANK-USERID TO CD03I-CONTACT-ID.                        bbank50p
050600* Now go get the data                                             bbank50p
050700 COPY CBANKX03.                                                   bbank50p
050800     MOVE CD03O-ACC1 TO BANK-SCR50-ACC1.                          bbank50p
050900     MOVE CD03O-DSC1 TO BANK-SCR50-DSC1.                          bbank50p
051000     IF CD03O-BAL1 IS EQUAL TO SPACES                             bbank50p
051100        MOVE CD03O-BAL1 TO BANK-SCR50-BAL1                        bbank50p
051200     ELSE                                                         bbank50p
051300        MOVE CD03O-BAL1N TO WS-EDIT-BALANCE                       bbank50p
051400        MOVE WS-EDIT-BALANCE TO BANK-SCR50-BAL1.                  bbank50p
051500                                                                  bbank50p
051600     MOVE CD03O-ACC2 TO BANK-SCR50-ACC2.                          bbank50p
051700     MOVE CD03O-DSC2 TO BANK-SCR50-DSC2.                          bbank50p
051800     IF CD03O-BAL2 IS EQUAL TO SPACES                             bbank50p
051900        MOVE CD03O-BAL2 TO BANK-SCR50-BAL2                        bbank50p
052000     ELSE                                                         bbank50p
052100        MOVE CD03O-BAL2N TO WS-EDIT-BALANCE                       bbank50p
052200        MOVE WS-EDIT-BALANCE TO BANK-SCR50-BAL2.                  bbank50p
052300     MOVE CD03O-ACC3 TO BANK-SCR50-ACC3.                          bbank50p
052400     MOVE CD03O-DSC3 TO BANK-SCR50-DSC3.                          bbank50p
052500     IF CD03O-BAL3 IS EQUAL TO SPACES                             bbank50p
052600        MOVE CD03O-BAL3 TO BANK-SCR50-BAL3                        bbank50p
052700     ELSE                                                         bbank50p
052800        MOVE CD03O-BAL3N TO WS-EDIT-BALANCE                       bbank50p
052900        MOVE WS-EDIT-BALANCE TO BANK-SCR50-BAL3.                  bbank50p
053000     MOVE CD03O-ACC4 TO BANK-SCR50-ACC4.                          bbank50p
053100     MOVE CD03O-DSC4 TO BANK-SCR50-DSC4.                          bbank50p
053200     IF CD03O-BAL4 IS EQUAL TO SPACES                             bbank50p
053300        MOVE CD03O-BAL4 TO BANK-SCR50-BAL4                        bbank50p
053400     ELSE                                                         bbank50p
053500        MOVE CD03O-BAL4N TO WS-EDIT-BALANCE                       bbank50p
053600        MOVE WS-EDIT-BALANCE TO BANK-SCR50-BAL4.                  bbank50p
053700     MOVE CD03O-ACC5 TO BANK-SCR50-ACC5.                          bbank50p
053800     MOVE CD03O-DSC5 TO BANK-SCR50-DSC5.                          bbank50p
053900     IF CD03O-BAL5 IS EQUAL TO SPACES                             bbank50p
054000        MOVE CD03O-BAL5 TO BANK-SCR50-BAL5                        bbank50p
054100     ELSE                                                         bbank50p
054200        MOVE CD03O-BAL5N TO WS-EDIT-BALANCE                       bbank50p
054300        MOVE WS-EDIT-BALANCE TO BANK-SCR50-BAL5.                  bbank50p
054400     MOVE CD03O-ACC6 TO BANK-SCR50-ACC6.                          bbank50p
054500     MOVE CD03O-DSC6 TO BANK-SCR50-DSC6.                          bbank50p
054600     IF CD03O-BAL6 IS EQUAL TO SPACES                             bbank50p
054700        MOVE CD03O-BAL6 TO BANK-SCR50-BAL6                        bbank50p
054800     ELSE                                                         bbank50p
054900        MOVE CD03O-BAL6N TO WS-EDIT-BALANCE                       bbank50p
055000        MOVE WS-EDIT-BALANCE TO BANK-SCR50-BAL6.                  bbank50p
055100                                                                  bbank50p
055200 POPULATE-SCREEN-DATA-EXIT.                                       bbank50p
055300     EXIT.                                                        bbank50p
055400                                                                  bbank50p
055500* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     bbank50p
