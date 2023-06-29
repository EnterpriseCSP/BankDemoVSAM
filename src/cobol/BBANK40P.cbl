000100***************************************************************** bbank40p
000200*                                                               * bbank40p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * bbank40p
000400*   This demonstration program is provided for use by users     * bbank40p
000500*   of Micro Focus products and may be used, modified and       * bbank40p
000600*   distributed as part of your application provided that       * bbank40p
000700*   you properly acknowledge the copyright of Micro Focus       * bbank40p
000800*   in this material.                                           * bbank40p
000900*                                                               * bbank40p
001000***************************************************************** bbank40p
001100                                                                  bbank40p
001200***************************************************************** bbank40p
001300* Program:     BBANK40P.CBL                                     * bbank40p
001400* Layer:       Business logic                                   * bbank40p
001500* Function:    Populate transaction deltails list for user      * bbank40p
001600***************************************************************** bbank40p
001700                                                                  bbank40p
001800 IDENTIFICATION DIVISION.                                         bbank40p
001900 PROGRAM-ID.                                                      bbank40p
002000     BBANK40P.                                                    bbank40p
002100 DATE-WRITTEN.                                                    bbank40p
002200     September 2002.                                              bbank40p
002300 DATE-COMPILED.                                                   bbank40p
002400     Today.                                                       bbank40p
002500                                                                  bbank40p
002600 ENVIRONMENT DIVISION.                                            bbank40p
002700                                                                  bbank40p
002800 DATA DIVISION.                                                   bbank40p
002900 WORKING-STORAGE SECTION.                                         bbank40p
003000 01  WS-MISC-STORAGE.                                             bbank40p
003100   05  WS-PROGRAM-ID                         PIC X(8)             bbank40p
003200       VALUE 'BBANK40P'.                                          bbank40p
003300   05  WS-INPUT-FLAG                         PIC X(1).            bbank40p
003400     88  INPUT-OK                            VALUE '0'.           bbank40p
003500     88  INPUT-ERROR                         VALUE '1'.           bbank40p
003600   05  WS-RETURN-FLAG                        PIC X(1).            bbank40p
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    bbank40p
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           bbank40p
003900   05  WS-RETURN-MSG                         PIC X(75).           bbank40p
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        bbank40p
004100   05  WS-PFK-FLAG                           PIC X(1).            bbank40p
004200     88  PFK-VALID                           VALUE '0'.           bbank40p
004300     88  PFK-INVALID                         VALUE '1'.           bbank40p
004400   05  WS-ERROR-MSG                          PIC X(75).           bbank40p
004500   05  WS-EDIT-AMT                           PIC Z,ZZZ,ZZ9.99-.   bbank40p
004600   05  WS-DYNAMIC-PGM                        PIC X(8)             bbank40p
004700       VALUE 'UNKNOWN'.                                           bbank40p
004800   05  WS-SUB1                               PIC S9(4) COMP.      bbank40p
004900   05  WS-SUB-LIMIT                          PIC S9(4) COMP.      bbank40p
005000   05  WS-TEMP-TIME-IP                       PIC X(8).            bbank40p
005100   05  WS-TEMP-TIME-OP                       PIC X(8).            bbank40p
005200                                                                  bbank40p
005300 01  WS-TIME-DATE-WORK-AREA.                                      bbank40p
005400 COPY CDATED.                                                     bbank40p
005500                                                                  bbank40p
005600 01  WS-BANK-DATA.                                                bbank40p
005700 COPY CBANKDAT.                                                   bbank40p
005800                                                                  bbank40p
005900 01  WS-HELP-DATA.                                                bbank40p
006000 COPY CHELPD01.                                                   bbank40p
006100                                                                  bbank40p
006200 01  WS-TXN-LIST.                                                 bbank40p
006300 COPY CBANKD05.                                                   bbank40p
006400                                                                  bbank40p
006500 COPY CABENDD.                                                    bbank40p
006600                                                                  bbank40p
006700 LINKAGE SECTION.                                                 bbank40p
006800 01  DFHCOMMAREA.                                                 bbank40p
006900   05  LK-COMMAREA                           PIC X(6144).         bbank40p
007000                                                                  bbank40p
007100 COPY CENTRY.                                                     bbank40p
007200***************************************************************** bbank40p
007300* Make ourselves re-entrant                                     * bbank40p
007400***************************************************************** bbank40p
007500     MOVE SPACES TO WS-ERROR-MSG.                                 bbank40p
007600                                                                  bbank40p
007700***************************************************************** bbank40p
007800* Move the passed area to our area                              * bbank40p
007900***************************************************************** bbank40p
008000     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. bbank40p
008100                                                                  bbank40p
008200***************************************************************** bbank40p
008300* Ensure error message is cleared                               * bbank40p
008400***************************************************************** bbank40p
008500     MOVE SPACES TO BANK-ERROR-MSG.                               bbank40p
008600                                                                  bbank40p
008700***************************************************************** bbank40p
008800* This is the main process                                      * bbank40p
008900***************************************************************** bbank40p
009000                                                                  bbank40p
009100***************************************************************** bbank40p
009200* Save the passed return flag and then turn it off              * bbank40p
009300***************************************************************** bbank40p
009400     MOVE BANK-RETURN-FLAG TO WS-RETURN-FLAG.                     bbank40p
009500     SET BANK-RETURN-FLAG-OFF TO TRUE.                            bbank40p
009600                                                                  bbank40p
009700***************************************************************** bbank40p
009800* Check the AID to see if its valid at this point               * bbank40p
009900***************************************************************** bbank40p
010000     SET PFK-INVALID TO TRUE.                                     bbank40p
010100     IF BANK-AID-ENTER OR                                         bbank40p
010200        BANK-AID-PFK03 OR                                         bbank40p
010300        BANK-AID-PFK04 OR                                         bbank40p
010400        BANK-AID-PFK07 OR                                         bbank40p
010500        BANK-AID-PFK08                                            bbank40p
010600        SET PFK-VALID TO TRUE                                     bbank40p
010700     END-IF.                                                      bbank40p
010800     IF BANK-AID-PFK01 AND                                        bbank40p
010900        BANK-HELP-INACTIVE                                        bbank40p
011000        SET BANK-HELP-ACTIVE TO TRUE                              bbank40p
011100        SET PFK-VALID TO TRUE                                     bbank40p
011200     END-IF.                                                      bbank40p
011300     IF PFK-INVALID                                               bbank40p
011400        SET BANK-AID-ENTER TO TRUE                                bbank40p
011500     END-IF.                                                      bbank40p
011600                                                                  bbank40p
011700***************************************************************** bbank40p
011800* Check the AID to see if we have to quit                       * bbank40p
011900***************************************************************** bbank40p
012000     IF BANK-AID-PFK03                                            bbank40p
012100        MOVE 'BBANK40P' TO BANK-LAST-PROG                         bbank40p
012200        MOVE 'BBANK99P' TO BANK-NEXT-PROG                         bbank40p
012300        MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        bbank40p
012400        MOVE 'BANK99A' TO BANK-NEXT-MAP                           bbank40p
012500        GO TO COMMON-RETURN                                       bbank40p
012600     END-IF.                                                      bbank40p
012700                                                                  bbank40p
012800***************************************************************** bbank40p
012900* Check the to see if user needs or has been using help         * bbank40p
013000***************************************************************** bbank40p
013100     IF BANK-HELP-ACTIVE                                          bbank40p
013200        IF BANK-AID-PFK04                                         bbank40p
013300           SET BANK-HELP-INACTIVE TO TRUE                         bbank40p
013400           MOVE 00 TO BANK-HELP-SCREEN                            bbank40p
013500           MOVE 'BBANK40P' TO BANK-LAST-PROG                      bbank40p
013600           MOVE 'BBANK40P' TO BANK-NEXT-PROG                      bbank40p
013700           MOVE 'MBANK40' TO BANK-LAST-MAPSET                     bbank40p
013800           MOVE 'HELP40A' TO BANK-LAST-MAP                        bbank40p
013900           MOVE 'MBANK40' TO BANK-NEXT-MAPSET                     bbank40p
014000           MOVE 'BANK40A' TO BANK-NEXT-MAP                        bbank40p
014100           GO TO COMMON-RETURN                                    bbank40p
014200        ELSE                                                      bbank40p
014300           MOVE 01 TO BANK-HELP-SCREEN                            bbank40p
014400           MOVE 'BBANK40P' TO BANK-LAST-PROG                      bbank40p
014500           MOVE 'BBANK40P' TO BANK-NEXT-PROG                      bbank40p
014600           MOVE 'MBANK40' TO BANK-LAST-MAPSET                     bbank40p
014700           MOVE 'BANK40A' TO BANK-LAST-MAP                        bbank40p
014800           MOVE 'MBANK40' TO BANK-NEXT-MAPSET                     bbank40p
014900           MOVE 'HELP40A' TO BANK-NEXT-MAP                        bbank40p
015000           MOVE 'BANK40' TO HELP01I-SCRN                          bbank40p
015100           COPY CHELPX01.                                         bbank40p
015200           MOVE HELP01O-DATA TO BANK-HELP-DATA                    bbank40p
015300           GO TO COMMON-RETURN                                    bbank40p
015400     END-IF.                                                      bbank40p
015500                                                                  bbank40p
015600***************************************************************** bbank40p
015700* Check the AID to see if we have to return to previous screen  * bbank40p
015800***************************************************************** bbank40p
015900     IF BANK-AID-PFK04                                            bbank40p
016000        MOVE 'BBANK40P' TO BANK-LAST-PROG                         bbank40p
016100        IF BANK-RETURN-TO-PROG IS EQUAL TO 'BBANK35P'             bbank40p
016200           MOVE 'BBANK35P' TO BANK-NEXT-PROG                      bbank40p
016300           MOVE 'MBANK35' TO BANK-NEXT-MAPSET                     bbank40p
016400           MOVE 'BANK35A' TO BANK-NEXT-MAP                        bbank40p
016500        ELSE                                                      bbank40p
016600           MOVE 'BBANK30P' TO BANK-NEXT-PROG                      bbank40p
016700           MOVE 'MBANK30' TO BANK-NEXT-MAPSET                     bbank40p
016800           MOVE 'BANK30A' TO BANK-NEXT-MAP                        bbank40p
016900        END-IF                                                    bbank40p
017000        SET BANK-AID-ENTER TO TRUE                                bbank40p
017100        GO TO COMMON-RETURN                                       bbank40p
017200     END-IF.                                                      bbank40p
017300                                                                  bbank40p
017400* Check if we have set the screen up before or is this 1st time   bbank40p
017500     IF BANK-LAST-MAPSET IS NOT EQUAL TO 'MBANK40'                bbank40p
017600        MOVE 'BBANK40P' TO BANK-LAST-PROG                         bbank40p
017700        MOVE 'BBANK40P' TO BANK-NEXT-PROG                         bbank40p
017800        MOVE 'MBANK40' TO BANK-LAST-MAPSET                        bbank40p
017900        MOVE 'BANK40A' TO BANK-LAST-MAP                           bbank40p
018000        MOVE 'MBANK40' TO BANK-NEXT-MAPSET                        bbank40p
018100        MOVE 'BANK40A' TO BANK-NEXT-MAP                           bbank40p
018200        SET BANK-PAGING-OFF TO TRUE                               bbank40p
018300        PERFORM POPULATE-SCREEN-DATA THRU                         bbank40p
018400                POPULATE-SCREEN-DATA-EXIT                         bbank40p
018500        GO TO COMMON-RETURN                                       bbank40p
018600     END-IF.                                                      bbank40p
018700                                                                  bbank40p
018800***************************************************************** bbank40p
018900* Check to see if we have a paging request                      * bbank40p
019000***************************************************************** bbank40p
019100     IF BANK-AID-PFK07 OR                                         bbank40p
019200        BANK-AID-PFK08                                            bbank40p
019300        MOVE 'BBANK40P' TO BANK-LAST-PROG                         bbank40p
019400        MOVE 'BBANK40P' TO BANK-NEXT-PROG                         bbank40p
019500        MOVE 'MBANK40' TO BANK-LAST-MAPSET                        bbank40p
019600        MOVE 'BANK40A' TO BANK-LAST-MAP                           bbank40p
019700        MOVE 'MBANK40' TO BANK-NEXT-MAPSET                        bbank40p
019800        MOVE 'BANK40A' TO BANK-NEXT-MAP                           bbank40p
019900        IF BANK-AID-PFK07 AND                                     bbank40p
020000           (BANK-PAGING-OFF OR                                    bbank40p
020100            BANK-PAGING-FIRST)                                    bbank40p
020200           MOVE 'Already at first page. Cannot page back.'        bbank40p
020300             TO BANK-ERROR-MSG                                    bbank40p
020400           GO TO COMMON-RETURN                                    bbank40p
020500        END-IF                                                    bbank40p
020600        IF BANK-AID-PFK08 AND                                     bbank40p
020700           (BANK-PAGING-OFF OR                                    bbank40p
020800            BANK-PAGING-LAST)                                     bbank40p
020900           MOVE 'Already at last page. Cannot page forward.'      bbank40p
021000             TO BANK-ERROR-MSG                                    bbank40p
021100           GO TO COMMON-RETURN                                    bbank40p
021200        END-IF                                                    bbank40p
021300        PERFORM POPULATE-SCREEN-DATA THRU                         bbank40p
021400                POPULATE-SCREEN-DATA-EXIT                         bbank40p
021500        GO TO COMMON-RETURN                                       bbank40p
021600     END-IF.                                                      bbank40p
021700                                                                  bbank40p
021800* Check if we have set the screen up before or is this 1st time   bbank40p
021900     IF BANK-LAST-MAPSET IS EQUAL TO 'MBANK40'                    bbank40p
022000        MOVE 'BBANK40P' TO BANK-LAST-PROG                         bbank40p
022100        MOVE 'BBANK40P' TO BANK-NEXT-PROG                         bbank40p
022200        MOVE 'MBANK40' TO BANK-LAST-MAPSET                        bbank40p
022300        MOVE 'BANK40A' TO BANK-LAST-MAP                           bbank40p
022400        MOVE 'MBANK40' TO BANK-NEXT-MAPSET                        bbank40p
022500        MOVE 'BANK40A' TO BANK-NEXT-MAP                           bbank40p
022600        PERFORM POPULATE-SCREEN-DATA THRU                         bbank40p
022700                POPULATE-SCREEN-DATA-EXIT                         bbank40p
022800        GO TO COMMON-RETURN                                       bbank40p
022900     END-IF.                                                      bbank40p
023000                                                                  bbank40p
023100***************************************************************** bbank40p
023200* If we get this far then we have an error in our logic as we   * bbank40p
023300* don't know where to go next.                                  * bbank40p
023400***************************************************************** bbank40p
023500     IF NOT BANK-ENV-CICS                                         bbank40p
023600        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       bbank40p
023700        MOVE '0001' TO ABEND-CODE                                 bbank40p
023800        MOVE SPACES TO ABEND-REASON                               bbank40p
023900        COPY CABENDPO.                                            bbank40p
024000     END-IF.                                                      bbank40p
024100     GOBACK.                                                      bbank40p
024200                                                                  bbank40p
024300 COMMON-RETURN.                                                   bbank40p
024400     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). bbank40p
024500 COPY CRETURN.                                                    bbank40p
024600                                                                  bbank40p
024700 POPULATE-SCREEN-DATA.                                            bbank40p
024800     MOVE SPACES TO CD05-DATA.                                    bbank40p
024900     MOVE BANK-SCR40-ACC TO CD05I-ACC.                            bbank40p
025000* Set criteria for search to populate screen                      bbank40p
025100     IF BANK-PAGING-OFF                                           bbank40p
025200        MOVE LOW-VALUES TO CD05I-START-ID                         bbank40p
025300        MOVE '0001-01-01-00.00.00.000000' TO CD05I-START-ID       bbank40p
025400        SET CD05-START-EQUAL TO TRUE                              bbank40p
025500     ELSE                                                         bbank40p
025600        IF WS-RETURN-FLAG-ON                                      bbank40p
025700           MOVE BANK-PAGING-FIRST-ENTRY TO CD05I-START-ID         bbank40p
025800           SET CD05-START-EQUAL TO TRUE                           bbank40p
025900        END-IF                                                    bbank40p
026000        IF WS-RETURN-FLAG-OFF                                     bbank40p
026100           IF BANK-AID-PFK07                                      bbank40p
026200              MOVE BANK-PAGING-FIRST-ENTRY TO CD05I-START-ID      bbank40p
026300              SET CD05-START-LOW TO TRUE                          bbank40p
026400           ELSE                                                   bbank40p
026500              IF BANK-AID-PFK08                                   bbank40p
026600                 MOVE BANK-PAGING-LAST-ENTRY TO CD05I-START-ID    bbank40p
026700                 SET CD05-START-HIGH TO TRUE                      bbank40p
026800              ELSE                                                bbank40p
026900                 MOVE BANK-PAGING-FIRST-ENTRY TO CD05I-START-ID   bbank40p
027000                 SET CD05-START-EQUAL TO TRUE                     bbank40p
027100               END-IF                                             bbank40p
027200           END-IF                                                 bbank40p
027300        END-IF                                                    bbank40p
027400     END-IF.                                                      bbank40p
027500* Now go get the data                                             bbank40p
027600 COPY CBANKX05.                                                   bbank40p
027700     IF WS-RETURN-FLAG-OFF                                        bbank40p
027800        IF BANK-PAGING-OFF AND                                    bbank40p
027900           CD05-IS-MORE-DATA                                      bbank40p
028000           SET BANK-PAGING-FIRST TO TRUE                          bbank40p
028100        ELSE                                                      bbank40p
028200           IF NOT BANK-AID-ENTER                                  bbank40p
028300              IF BANK-PAGING-FIRST                                bbank40p
028400                 IF CD05-IS-MORE-DATA                             bbank40p
028500                    SET BANK-PAGING-MIDDLE TO TRUE                bbank40p
028600                 END-IF                                           bbank40p
028700                 IF CD05-NO-MORE-DATA                             bbank40p
028800                    SET BANK-PAGING-LAST TO TRUE                  bbank40p
028900                 END-IF                                           bbank40p
029000              ELSE                                                bbank40p
029100                 IF BANK-PAGING-MIDDLE                            bbank40p
029200                    IF BANK-AID-PFK08 AND                         bbank40p
029300                       CD05-NO-MORE-DATA                          bbank40p
029400                       SET BANK-PAGING-LAST TO TRUE               bbank40p
029500                    END-IF                                        bbank40p
029600                    IF BANK-AID-PFK07 AND                         bbank40p
029700                       CD05-NO-MORE-DATA                          bbank40p
029800                       SET BANK-PAGING-FIRST TO TRUE              bbank40p
029900                    END-IF                                        bbank40p
030000                 ELSE                                             bbank40p
030100                    IF BANK-PAGING-LAST                           bbank40p
030200                       IF CD05-IS-MORE-DATA                       bbank40p
030300                          SET BANK-PAGING-MIDDLE TO TRUE          bbank40p
030400                       END-IF                                     bbank40p
030500                    IF CD05-NO-MORE-DATA                          bbank40p
030600                       SET BANK-PAGING-FIRST TO TRUE              bbank40p
030700                    END-IF                                        bbank40p
030800                 END-IF                                           bbank40p
030900              END-IF                                              bbank40p
031000           END-IF                                                 bbank40p
031100        END-IF                                                    bbank40p
031200     END-IF.                                                      bbank40p
031300     MOVE LOW-VALUES TO BANK-SCR40-TXN-FIELDS.                    bbank40p
031400     MOVE CD05O-ID1 TO BANK-PAGING-FIRST-ENTRY.                   bbank40p
031500     MOVE CD05O-ID1 TO BANK-PAGING-LAST-ENTRY.                    bbank40p
031600     MOVE 0 TO WS-SUB1.                                           bbank40p
031700     PERFORM POPULATE-ENTRY THRU                                  bbank40p
031800             POPULATE-ENTRY-EXIT 8 TIMES.                         bbank40p
031900     GO TO POPULATE-SCREEN-DATA-EXIT.                             bbank40p
032000 POPULATE-ENTRY.                                                  bbank40p
032100     ADD 1 TO WS-SUB1.                                            bbank40p
032200     IF CD05O-DATE (WS-SUB1) IS EQUAL TO SPACES                   bbank40p
032300        MOVE CD05O-DATE (WS-SUB1) TO BANK-SCR40-DATE (WS-SUB1)    bbank40p
032400     ELSE                                                         bbank40p
032500        MOVE CD05O-DATE (WS-SUB1) TO DDI-DATA                     bbank40p
032600        SET DDI-ISO TO TRUE                                       bbank40p
032700        SET DDO-DD-MMM-YYYY TO TRUE                               bbank40p
032800        PERFORM CALL-DATECONV THRU                                bbank40p
032900               CALL-DATECONV-EXIT                                 bbank40p
033000        MOVE DDO-DATA TO BANK-SCR40-DATE (WS-SUB1)                bbank40p
033100     END-IF.                                                      bbank40p
033200     IF CD05O-TIME (WS-SUB1) IS EQUAL TO SPACES                   bbank40p
033300        MOVE CD05O-TIME (WS-SUB1) TO BANK-SCR40-TIME (WS-SUB1)    bbank40p
033400     ELSE                                                         bbank40p
033500        MOVE CD05O-TIME (WS-SUB1) TO WS-TEMP-TIME-IP              bbank40p
033600        MOVE WS-TEMP-TIME-IP (1:2) TO WS-TEMP-TIME-OP (1:2)       bbank40p
033700        MOVE ':' TO WS-TEMP-TIME-OP (3:1)                         bbank40p
033800        MOVE WS-TEMP-TIME-IP (4:2) TO WS-TEMP-TIME-OP (4:2)       bbank40p
033900        MOVE ':' TO WS-TEMP-TIME-OP (6:1)                         bbank40p
034000        MOVE WS-TEMP-TIME-IP (7:2) TO WS-TEMP-TIME-OP (7:2)       bbank40p
034100        MOVE WS-TEMP-TIME-OP TO BANK-SCR40-TIME (WS-SUB1)         bbank40p
034200     END-IF.                                                      bbank40p
034300     IF CD05O-AMT (WS-SUB1) IS EQUAL TO SPACES                    bbank40p
034400        MOVE CD05O-AMT (WS-SUB1) TO BANK-SCR40-AMNT (WS-SUB1)     bbank40p
034500     ELSE                                                         bbank40p
034600        MOVE CD05O-AMT-N (WS-SUB1) TO WS-EDIT-AMT                 bbank40p
034700        MOVE WS-EDIT-AMT TO BANK-SCR40-AMNT (WS-SUB1)             bbank40p
034800     END-IF.                                                      bbank40p
034900     MOVE CD05O-DESC (WS-SUB1) TO BANK-SCR40-DESC (WS-SUB1).      bbank40p
035000     MOVE CD05O-ID (WS-SUB1) TO BANK-PAGING-LAST-ENTRY.           bbank40p
035100 POPULATE-ENTRY-EXIT.                                             bbank40p
035200     EXIT.                                                        bbank40p
035300                                                                  bbank40p
035400 POPULATE-SCREEN-DATA-EXIT.                                       bbank40p
035500     EXIT.                                                        bbank40p
035600                                                                  bbank40p
035700***************************************************************** bbank40p
035800* Call common routine to perform date conversions               * bbank40p
035900***************************************************************** bbank40p
036000 CALL-DATECONV.                                                   bbank40p
036100     MOVE BANK-ENV TO DD-ENV.                                     bbank40p
036200     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           bbank40p
036300     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            bbank40p
036400 CALL-DATECONV-EXIT.                                              bbank40p
036500     EXIT.                                                        bbank40p
036600                                                                  bbank40p
036700* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     bbank40p
