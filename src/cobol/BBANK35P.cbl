000100***************************************************************** bbank35p
000200*                                                               * bbank35p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * bbank35p
000400*   This demonstration program is provided for use by users     * bbank35p
000500*   of Micro Focus products and may be used, modified and       * bbank35p
000600*   distributed as part of your application provided that       * bbank35p
000700*   you properly acknowledge the copyright of Micro Focus       * bbank35p
000800*   in this material.                                           * bbank35p
000900*                                                               * bbank35p
001000***************************************************************** bbank35p
001100                                                                  bbank35p
001200***************************************************************** bbank35p
001300* Program:     BBANK35P.CBL                                     * bbank35p
001400* Layer:       Business logic                                   * bbank35p
001500* Function:    Populate details of specific account.            * bbank35p
001600***************************************************************** bbank35p
001700                                                                  bbank35p
001800 IDENTIFICATION DIVISION.                                         bbank35p
001900 PROGRAM-ID.                                                      bbank35p
002000     BBANK35P.                                                    bbank35p
002100 DATE-WRITTEN.                                                    bbank35p
002200     September 2002.                                              bbank35p
002300 DATE-COMPILED.                                                   bbank35p
002400     Today.                                                       bbank35p
002500                                                                  bbank35p
002600 ENVIRONMENT DIVISION.                                            bbank35p
002700                                                                  bbank35p
002800 DATA DIVISION.                                                   bbank35p
002900 WORKING-STORAGE SECTION.                                         bbank35p
003000 01  WS-MISC-STORAGE.                                             bbank35p
003100   05  WS-PROGRAM-ID                         PIC X(8)             bbank35p
003200       VALUE 'BBANK35P'.                                          bbank35p
003300   05  WS-INPUT-FLAG                         PIC X(1).            bbank35p
003400     88  INPUT-OK                            VALUE '0'.           bbank35p
003500     88  INPUT-ERROR                         VALUE '1'.           bbank35p
003600   05  WS-RETURN-FLAG                        PIC X(1).            bbank35p
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    bbank35p
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           bbank35p
003900   05  WS-RETURN-MSG                         PIC X(75).           bbank35p
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        bbank35p
004100   05  WS-PFK-FLAG                           PIC X(1).            bbank35p
004200     88  PFK-VALID                           VALUE '0'.           bbank35p
004300     88  PFK-INVALID                         VALUE '1'.           bbank35p
004400   05  WS-ERROR-MSG                          PIC X(75).           bbank35p
004500   05  WS-EDIT-AMT-3                         PIC ZZ9.             bbank35p
004600   05  WS-EDIT-AMT-5-2                       PIC ZZ,ZZ9.99-.      bbank35p
004700   05  WS-EDIT-AMT-7-2                       PIC Z,ZZZ,ZZ9.99-.   bbank35p
004800   05  WS-DYNAMIC-PGM                        PIC X(8)             bbank35p
004900       VALUE 'UNKNOWN'.                                           bbank35p
005000   05  WS-SUB1                               PIC S9(4) COMP.      bbank35p
005100   05  WS-SUB-LIMIT                          PIC S9(4) COMP.      bbank35p
005200                                                                  bbank35p
005300 01  WS-TIME-DATE-WORK-AREA.                                      bbank35p
005400 COPY CDATED.                                                     bbank35p
005500                                                                  bbank35p
005600 01  WS-BANK-DATA.                                                bbank35p
005700 COPY CBANKDAT.                                                   bbank35p
005800                                                                  bbank35p
005900 01  WS-HELP-DATA.                                                bbank35p
006000 COPY CHELPD01.                                                   bbank35p
006100                                                                  bbank35p
006200 01  WS-TXN-LIST.                                                 bbank35p
006300 COPY CBANKD11.                                                   bbank35p
006400                                                                  bbank35p
006500 COPY CABENDD.                                                    bbank35p
006600                                                                  bbank35p
006700 LINKAGE SECTION.                                                 bbank35p
006800 01  DFHCOMMAREA.                                                 bbank35p
006900   05  LK-COMMAREA                           PIC X(6144).         bbank35p
007000                                                                  bbank35p
007100 COPY CENTRY.                                                     bbank35p
007200***************************************************************** bbank35p
007300* Make ourselves re-entrant                                     * bbank35p
007400***************************************************************** bbank35p
007500     MOVE SPACES TO WS-ERROR-MSG.                                 bbank35p
007600                                                                  bbank35p
007700***************************************************************** bbank35p
007800* Move the passed area to our area                              * bbank35p
007900***************************************************************** bbank35p
008000     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. bbank35p
008100                                                                  bbank35p
008200***************************************************************** bbank35p
008300* Ensure error message is cleared                               * bbank35p
008400***************************************************************** bbank35p
008500     MOVE SPACES TO BANK-ERROR-MSG.                               bbank35p
008600                                                                  bbank35p
008700***************************************************************** bbank35p
008800* This is the main process                                      * bbank35p
008900***************************************************************** bbank35p
009000                                                                  bbank35p
009100***************************************************************** bbank35p
009200* Save the passed return flag and then turn it off              * bbank35p
009300***************************************************************** bbank35p
009400     MOVE BANK-RETURN-FLAG TO WS-RETURN-FLAG.                     bbank35p
009500     SET BANK-RETURN-FLAG-OFF TO TRUE.                            bbank35p
009600                                                                  bbank35p
009700***************************************************************** bbank35p
009800* Check the AID to see if its valid at this point               * bbank35p
009900***************************************************************** bbank35p
010000     SET PFK-INVALID TO TRUE.                                     bbank35p
010100     IF BANK-AID-ENTER OR                                         bbank35p
010200        BANK-AID-PFK03 OR                                         bbank35p
010300        BANK-AID-PFK04                                            bbank35p
010400        SET PFK-VALID TO TRUE                                     bbank35p
010500     END-IF.                                                      bbank35p
010600     IF BANK-AID-PFK01 AND                                        bbank35p
010700        BANK-HELP-INACTIVE                                        bbank35p
010800        SET BANK-HELP-ACTIVE TO TRUE                              bbank35p
010900        SET PFK-VALID TO TRUE                                     bbank35p
011000     END-IF.                                                      bbank35p
011100     IF BANK-AID-PFK06 AND                                        bbank35p
011200        BANK-SCR35-TRANS(1:1) IS NUMERIC                          bbank35p
011300        SET PFK-VALID TO TRUE                                     bbank35p
011400     END-IF.                                                      bbank35p
011500     IF PFK-INVALID                                               bbank35p
011600        SET BANK-AID-ENTER TO TRUE                                bbank35p
011700     END-IF.                                                      bbank35p
011800                                                                  bbank35p
011900***************************************************************** bbank35p
012000* Check the AID to see if we have to quit                       * bbank35p
012100***************************************************************** bbank35p
012200     IF BANK-AID-PFK03                                            bbank35p
012300        MOVE 'BBANK35P' TO BANK-LAST-PROG                         bbank35p
012400        MOVE 'BBANK99P' TO BANK-NEXT-PROG                         bbank35p
012500        MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        bbank35p
012600        MOVE 'BANK99A' TO BANK-NEXT-MAP                           bbank35p
012700        GO TO COMMON-RETURN                                       bbank35p
012800     END-IF.                                                      bbank35p
012900                                                                  bbank35p
013000***************************************************************** bbank35p
013100* Check the to see if user needs or has been using help         * bbank35p
013200***************************************************************** bbank35p
013300     IF BANK-HELP-ACTIVE                                          bbank35p
013400        IF BANK-AID-PFK04                                         bbank35p
013500           SET BANK-HELP-INACTIVE TO TRUE                         bbank35p
013600           MOVE 00 TO BANK-HELP-SCREEN                            bbank35p
013700           MOVE 'BBANK35P' TO BANK-LAST-PROG                      bbank35p
013800           MOVE 'BBANK35P' TO BANK-NEXT-PROG                      bbank35p
013900           MOVE 'MBANK35' TO BANK-LAST-MAPSET                     bbank35p
014000           MOVE 'HELP35A' TO BANK-LAST-MAP                        bbank35p
014100           MOVE 'MBANK35' TO BANK-NEXT-MAPSET                     bbank35p
014200           MOVE 'BANK35A' TO BANK-NEXT-MAP                        bbank35p
014300           GO TO COMMON-RETURN                                    bbank35p
014400        ELSE                                                      bbank35p
014500           MOVE 01 TO BANK-HELP-SCREEN                            bbank35p
014600           MOVE 'BBANK35P' TO BANK-LAST-PROG                      bbank35p
014700           MOVE 'BBANK35P' TO BANK-NEXT-PROG                      bbank35p
014800           MOVE 'MBANK35' TO BANK-LAST-MAPSET                     bbank35p
014900           MOVE 'BANK35A' TO BANK-LAST-MAP                        bbank35p
015000           MOVE 'MBANK35' TO BANK-NEXT-MAPSET                     bbank35p
015100           MOVE 'HELP35A' TO BANK-NEXT-MAP                        bbank35p
015200           MOVE 'BANK35' TO HELP01I-SCRN                          bbank35p
015300           COPY CHELPX01.                                         bbank35p
015400           MOVE HELP01O-DATA TO BANK-HELP-DATA                    bbank35p
015500           GO TO COMMON-RETURN                                    bbank35p
015600     END-IF.                                                      bbank35p
015700                                                                  bbank35p
015800***************************************************************** bbank35p
015900* Check the AID to see if we have to return to previous screen  * bbank35p
016000***************************************************************** bbank35p
016100     IF BANK-AID-PFK04                                            bbank35p
016200        MOVE 'BBANK35P' TO BANK-LAST-PROG                         bbank35p
016300        MOVE 'BBANK30P' TO BANK-NEXT-PROG                         bbank35p
016400        MOVE 'MBANK30' TO BANK-NEXT-MAPSET                        bbank35p
016500        MOVE 'BANK30A' TO BANK-NEXT-MAP                           bbank35p
016600        SET BANK-AID-ENTER TO TRUE                                bbank35p
016700        GO TO COMMON-RETURN                                       bbank35p
016800     END-IF.                                                      bbank35p
016900                                                                  bbank35p
017000***************************************************************** bbank35p
017100* Check the AID to see if we have to show transactions          * bbank35p
017200***************************************************************** bbank35p
017300     IF BANK-AID-PFK06                                            bbank35p
017400        MOVE BANK-SCR35-ACC TO BANK-SCR40-ACC                     bbank35p
017500        MOVE BANK-SCR35-DSC TO BANK-SCR40-ACCTYPE                 bbank35p
017600        MOVE 'BBANK35P' TO BANK-LAST-PROG                         bbank35p
017700        MOVE 'BBANK40P' TO BANK-NEXT-PROG                         bbank35p
017800        MOVE 'MBANK40' TO BANK-NEXT-MAPSET                        bbank35p
017900        MOVE 'BANK40A' TO BANK-NEXT-MAP                           bbank35p
018000        MOVE 'BBANK35P' TO BANK-RETURN-TO-PROG                    bbank35p
018100        SET BANK-AID-ENTER TO TRUE                                bbank35p
018200        GO TO COMMON-RETURN                                       bbank35p
018300     END-IF.                                                      bbank35p
018400                                                                  bbank35p
018500* Check if we have set the screen up before or is this 1st time   bbank35p
018600     IF BANK-LAST-MAPSET IS NOT EQUAL TO 'MBANK35'                bbank35p
018700        MOVE 'BBANK35P' TO BANK-LAST-PROG                         bbank35p
018800        MOVE 'BBANK35P' TO BANK-NEXT-PROG                         bbank35p
018900        MOVE 'MBANK35' TO BANK-LAST-MAPSET                        bbank35p
019000        MOVE 'BANK35A' TO BANK-LAST-MAP                           bbank35p
019100        MOVE 'MBANK35' TO BANK-NEXT-MAPSET                        bbank35p
019200        MOVE 'BANK35A' TO BANK-NEXT-MAP                           bbank35p
019300        SET BANK-PAGING-OFF TO TRUE                               bbank35p
019400        PERFORM POPULATE-SCREEN-DATA THRU                         bbank35p
019500                POPULATE-SCREEN-DATA-EXIT                         bbank35p
019600        GO TO COMMON-RETURN                                       bbank35p
019700     END-IF.                                                      bbank35p
019800                                                                  bbank35p
019900                                                                  bbank35p
020000* Check if we have set the screen up before or is this 1st time   bbank35p
020100     IF BANK-LAST-MAPSET IS EQUAL TO 'MBANK35'                    bbank35p
020200        MOVE 'BBANK35P' TO BANK-LAST-PROG                         bbank35p
020300        MOVE 'BBANK35P' TO BANK-NEXT-PROG                         bbank35p
020400        MOVE 'MBANK35' TO BANK-LAST-MAPSET                        bbank35p
020500        MOVE 'BANK35A' TO BANK-LAST-MAP                           bbank35p
020600        MOVE 'MBANK35' TO BANK-NEXT-MAPSET                        bbank35p
020700        MOVE 'BANK35A' TO BANK-NEXT-MAP                           bbank35p
020800        PERFORM POPULATE-SCREEN-DATA THRU                         bbank35p
020900                POPULATE-SCREEN-DATA-EXIT                         bbank35p
021000        GO TO COMMON-RETURN                                       bbank35p
021100     END-IF.                                                      bbank35p
021200                                                                  bbank35p
021300***************************************************************** bbank35p
021400* If we get this far then we have an error in our logic as we   * bbank35p
021500* don't know where to go next.                                  * bbank35p
021600***************************************************************** bbank35p
021700     IF NOT BANK-ENV-CICS                                         bbank35p
021800        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       bbank35p
021900        MOVE '0001' TO ABEND-CODE                                 bbank35p
022000        MOVE SPACES TO ABEND-REASON                               bbank35p
022100        COPY CABENDPO.                                            bbank35p
022200     END-IF.                                                      bbank35p
022300     GOBACK.                                                      bbank35p
022400                                                                  bbank35p
022500 COMMON-RETURN.                                                   bbank35p
022600     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). bbank35p
022700 COPY CRETURN.                                                    bbank35p
022800                                                                  bbank35p
022900 POPULATE-SCREEN-DATA.                                            bbank35p
023000     MOVE SPACES TO BANK-SCR35-ATM-FIELDS.                        bbank35p
023100     MOVE SPACES TO BANK-SCR35-RP-FIELDS.                         bbank35p
023200     MOVE SPACES TO CD11-DATA.                                    bbank35p
023300* Set criteria for search to populate screen                      bbank35p
023400     MOVE BANK-SCR35-ACC TO CD11I-ACCNO.                          bbank35p
023500* Now go get the data                                             bbank35p
023600 COPY CBANKX11.                                                   bbank35p
023700                                                                  bbank35p
023800     IF CD11O-ACCNO IS NOT EQUAL TO SPACES                        bbank35p
023900        MOVE CD11O-BAL TO BANK-SCR35-BAL                          bbank35p
024000        IF CD11O-BAL IS EQUAL TO SPACES                           bbank35p
024100           MOVE CD11O-BAL TO BANK-SCR35-BAL                       bbank35p
024200        ELSE                                                      bbank35p
024300           MOVE CD11O-BAL-N TO WS-EDIT-AMT-7-2                    bbank35p
024400           MOVE WS-EDIT-AMT-7-2 TO BANK-SCR35-BAL                 bbank35p
024500        END-IF                                                    bbank35p
024600        IF CD11O-DTE IS EQUAL TO SPACES                           bbank35p
024700           MOVE CD11O-DTE TO BANK-SCR35-DTE                       bbank35p
024800        ELSE                                                      bbank35p
024900           MOVE CD11O-DTE TO DDI-DATA                             bbank35p
025000           SET DDI-ISO TO TRUE                                    bbank35p
025100           SET DDO-DD-MMM-YYYY TO TRUE                            bbank35p
025200           PERFORM CALL-DATECONV THRU                             bbank35p
025300                  CALL-DATECONV-EXIT                              bbank35p
025400           MOVE DDO-DATA TO BANK-SCR35-DTE                        bbank35p
025500        END-IF                                                    bbank35p
025600        MOVE CD11O-TRANS TO BANK-SCR35-TRANS                      bbank35p
025700        MOVE CD11O-ATM-ENABLED TO BANK-SCR35-ATM-ENABLED          bbank35p
025800        IF CD11O-ATM-LIM IS EQUAL TO SPACES                       bbank35p
025900           MOVE CD11O-ATM-LIM TO BANK-SCR35-ATM-LIM               bbank35p
026000        ELSE                                                      bbank35p
026100           MOVE CD11O-ATM-LIM-N TO WS-EDIT-AMT-3                  bbank35p
026200           MOVE WS-EDIT-AMT-3 TO BANK-SCR35-ATM-LIM               bbank35p
026300        END-IF                                                    bbank35p
026400        IF CD11O-ATM-LDTE IS EQUAL TO SPACES                      bbank35p
026500           MOVE CD11O-ATM-LDTE TO BANK-SCR35-DTE                  bbank35p
026600        ELSE                                                      bbank35p
026700           MOVE CD11O-ATM-LDTE TO DDI-DATA                        bbank35p
026800           SET DDI-ISO TO TRUE                                    bbank35p
026900           SET DDO-DD-MMM-YYYY TO TRUE                            bbank35p
027000           PERFORM CALL-DATECONV THRU                             bbank35p
027100                  CALL-DATECONV-EXIT                              bbank35p
027200           MOVE DDO-DATA TO BANK-SCR35-ATM-LDTE                   bbank35p
027300        END-IF                                                    bbank35p
027400        IF CD11O-ATM-LAMT IS EQUAL TO SPACES                      bbank35p
027500           MOVE CD11O-ATM-LAMT TO BANK-SCR35-ATM-LAMT             bbank35p
027600        ELSE                                                      bbank35p
027700           MOVE CD11O-ATM-LAMT-N TO WS-EDIT-AMT-3                 bbank35p
027800           MOVE WS-EDIT-AMT-3 TO BANK-SCR35-ATM-LAMT              bbank35p
027900        END-IF                                                    bbank35p
028000        MOVE CD11O-RP1DAY TO BANK-SCR35-RP1DAY                    bbank35p
028100        IF CD11O-RP1AMT IS EQUAL TO SPACES                        bbank35p
028200           MOVE CD11O-RP1AMT TO BANK-SCR35-RP1AMT                 bbank35p
028300        ELSE                                                      bbank35p
028400           MOVE CD11O-RP1AMT-N TO WS-EDIT-AMT-5-2                 bbank35p
028500           MOVE WS-EDIT-AMT-5-2 TO BANK-SCR35-RP1AMT              bbank35p
028600        END-IF                                                    bbank35p
028700        MOVE CD11O-RP1PID TO BANK-SCR35-RP1PID                    bbank35p
028800        MOVE CD11O-RP1ACC TO BANK-SCR35-RP1ACC                    bbank35p
028900        IF CD11O-RP1DTE IS EQUAL TO SPACES                        bbank35p
029000           MOVE CD11O-RP1DTE TO BANK-SCR35-RP1DTE                 bbank35p
029100        ELSE                                                      bbank35p
029200           MOVE CD11O-RP1DTE TO DDI-DATA                          bbank35p
029300           SET DDI-ISO TO TRUE                                    bbank35p
029400           SET DDO-DD-MMM-YYYY TO TRUE                            bbank35p
029500           PERFORM CALL-DATECONV THRU                             bbank35p
029600                  CALL-DATECONV-EXIT                              bbank35p
029700           MOVE DDO-DATA TO BANK-SCR35-RP1DTE                     bbank35p
029800        END-IF                                                    bbank35p
029900        MOVE CD11O-RP2DAY TO BANK-SCR35-RP2DAY                    bbank35p
030000        IF CD11O-RP2AMT IS EQUAL TO SPACES                        bbank35p
030100           MOVE CD11O-RP2AMT TO BANK-SCR35-RP2AMT                 bbank35p
030200        ELSE                                                      bbank35p
030300           MOVE CD11O-RP2AMT-N TO WS-EDIT-AMT-5-2                 bbank35p
030400           MOVE WS-EDIT-AMT-5-2 TO BANK-SCR35-RP2AMT              bbank35p
030500        END-IF                                                    bbank35p
030600        MOVE CD11O-RP2PID TO BANK-SCR35-RP2PID                    bbank35p
030700        MOVE CD11O-RP2ACC TO BANK-SCR35-RP2ACC                    bbank35p
030800        IF CD11O-RP2DTE IS EQUAL TO SPACES                        bbank35p
030900           MOVE CD11O-RP2DTE TO BANK-SCR35-RP2DTE                 bbank35p
031000        ELSE                                                      bbank35p
031100           MOVE CD11O-RP2DTE TO DDI-DATA                          bbank35p
031200           SET DDI-ISO TO TRUE                                    bbank35p
031300           SET DDO-DD-MMM-YYYY TO TRUE                            bbank35p
031400           PERFORM CALL-DATECONV THRU                             bbank35p
031500                  CALL-DATECONV-EXIT                              bbank35p
031600           MOVE DDO-DATA TO BANK-SCR35-RP2DTE                     bbank35p
031700        END-IF                                                    bbank35p
031800        MOVE CD11O-RP3DAY TO BANK-SCR35-RP3DAY                    bbank35p
031900        MOVE CD11O-RP3AMT TO BANK-SCR35-RP3AMT                    bbank35p
032000        IF CD11O-RP3AMT IS EQUAL TO SPACES                        bbank35p
032100           MOVE CD11O-RP3AMT TO BANK-SCR35-RP3AMT                 bbank35p
032200        ELSE                                                      bbank35p
032300           MOVE CD11O-RP3AMT-N TO WS-EDIT-AMT-5-2                 bbank35p
032400           MOVE WS-EDIT-AMT-5-2 TO BANK-SCR35-RP3AMT              bbank35p
032500        END-IF                                                    bbank35p
032600        MOVE CD11O-RP3PID TO BANK-SCR35-RP3PID                    bbank35p
032700        MOVE CD11O-RP3ACC TO BANK-SCR35-RP3ACC                    bbank35p
032800        IF CD11O-RP3DTE IS EQUAL TO SPACES                        bbank35p
032900           MOVE CD11O-RP3DTE TO BANK-SCR35-RP3DTE                 bbank35p
033000        ELSE                                                      bbank35p
033100           MOVE CD11O-RP3DTE TO DDI-DATA                          bbank35p
033200           SET DDI-ISO TO TRUE                                    bbank35p
033300           SET DDO-DD-MMM-YYYY TO TRUE                            bbank35p
033400           PERFORM CALL-DATECONV THRU                             bbank35p
033500                  CALL-DATECONV-EXIT                              bbank35p
033600           MOVE DDO-DATA TO BANK-SCR35-RP3DTE                     bbank35p
033700        END-IF                                                    bbank35p
033800     END-IF.                                                      bbank35p
033900                                                                  bbank35p
034000 POPULATE-SCREEN-DATA-EXIT.                                       bbank35p
034100     EXIT.                                                        bbank35p
034200                                                                  bbank35p
034300***************************************************************** bbank35p
034400* Call common routine to perform date conversions               * bbank35p
034500***************************************************************** bbank35p
034600 CALL-DATECONV.                                                   bbank35p
034700     MOVE BANK-ENV TO DD-ENV.                                     bbank35p
034800     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           bbank35p
034900     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            bbank35p
035000 CALL-DATECONV-EXIT.                                              bbank35p
035100     EXIT.                                                        bbank35p
035200                                                                  bbank35p
035300* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     bbank35p
