000100***************************************************************** sbank30p
000200*                                                               * sbank30p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * sbank30p
000400*   This demonstration program is provided for use by users     * sbank30p
000500*   of Micro Focus products and may be used, modified and       * sbank30p
000600*   distributed as part of your application provided that       * sbank30p
000700*   you properly acknowledge the copyright of Micro Focus       * sbank30p
000800*   in this material.                                           * sbank30p
000900*                                                               * sbank30p
001000***************************************************************** sbank30p
001100                                                                  sbank30p
001200***************************************************************** sbank30p
001300* Program:     SBANK30P.CBL (CICS Version)                      * sbank30p
001400* Layer:       Screen handling                                  * sbank30p
001500* Function:    Display account balances                         * sbank30p
001600***************************************************************** sbank30p
001700                                                                  sbank30p
001800 IDENTIFICATION DIVISION.                                         sbank30p
001900 PROGRAM-ID.                                                      sbank30p
002000     SBANK30P.                                                    sbank30p
002100 DATE-WRITTEN.                                                    sbank30p
002200     September 2002.                                              sbank30p
002300 DATE-COMPILED.                                                   sbank30p
002400     Today.                                                       sbank30p
002500                                                                  sbank30p
002600 ENVIRONMENT DIVISION.                                            sbank30p
002700                                                                  sbank30p
002800 DATA DIVISION.                                                   sbank30p
002900 WORKING-STORAGE SECTION.                                         sbank30p
003000 01  WS-MISC-STORAGE.                                             sbank30p
003100   05  WS-PROGRAM-ID                         PIC X(8)             sbank30p
003200       VALUE 'SBANK30P'.                                          sbank30p
003300   05  WS-TRAN-ID                            PIC X(4).            sbank30p
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             sbank30p
003500       VALUE SPACES.                                              sbank30p
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             sbank30p
003700       VALUE 'UNKNOWN'.                                           sbank30p
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      sbank30p
003900   05  WS-WORK1                              PIC X(1).            sbank30p
004000   05  WS-SUB1                               PIC S9(4) COMP.      sbank30p
         05  WS-VERSION                            PIC X(7).
                                                                        sbank30p
004100                                                                  
004200 01  MAPAREA                                 PIC X(2048).         sbank30p
004300 COPY MBANK30.                                                    sbank30p
004400                                                                  sbank30p
004500 01  WS-TIME-DATE-WORK-AREA.                                      sbank30p
004600 COPY CDATED.                                                     sbank30p
004700                                                                  sbank30p
004800 01  WS-BANK-DATA-AREAS.                                          sbank30p
004900   05  WS-BANK-DATA.                                              sbank30p
005000 COPY CBANKDAT.                                                   sbank30p
005100   05  WS-BANK-EXT-DATA.                                          sbank30p
005200 COPY CBANKEXT.                                                   sbank30p
005300                                                                  sbank30p
005400 COPY CSCRNHDD.                                                   sbank30p
005500                                                                  sbank30p
005600 COPY CVERSND.                                                    sbank30p
005700                                                                  sbank30p
005800 COPY DFHAID.                                                     sbank30p
005900                                                                  sbank30p
006000 COPY DFHBMSCA.                                                   sbank30p
006100                                                                  sbank30p
006200 COPY CABENDD.                                                    sbank30p
006300                                                                  sbank30p
006400 LINKAGE SECTION.                                                 sbank30p
006500 01  DFHCOMMAREA.                                                 sbank30p
006600   05  FILLER                                PIC X(1)             sbank30p
006700       OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.             sbank30p
006800                                                                  sbank30p
006900 PROCEDURE DIVISION.                                              sbank30p
007000***************************************************************** sbank30p
007100* Write entry to log to show we have been invoked               * sbank30p
007200***************************************************************** sbank30p
007300     COPY CTRACE.                                                 sbank30p
007400                                                                  sbank30p
007500***************************************************************** sbank30p
007600* Store our transaction-id                                      * sbank30p
007700***************************************************************** sbank30p
007800     MOVE EIBTRNID TO WS-TRAN-ID.                                 sbank30p
007900                                                                  sbank30p
008000***************************************************************** sbank30p
008100* Store passed data or abend if there wasn't any                * sbank30p
008200***************************************************************** sbank30p
008300     IF EIBCALEN IS EQUAL TO 0                                    sbank30p
008400        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       sbank30p
008500        MOVE '0001' TO ABEND-CODE                                 sbank30p
008600        MOVE SPACES TO ABEND-REASON                               sbank30p
008700        COPY CABENDPO.                                            sbank30p
008800     ELSE                                                         sbank30p
008900        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        sbank30p
009000        MOVE LOW-VALUES TO WS-BANK-DATA                           sbank30p
009100        MOVE DFHCOMMAREA (1:EIBCALEN)                             sbank30p
009200          TO WS-BANK-DATA-AREAS (1:LENGTH OF WS-BANK-DATA-AREAS)  sbank30p
009300     END-IF.                                                      sbank30p
009400                                                                  sbank30p
009500***************************************************************** sbank30p
009600* This is the main process                                      * sbank30p
009700***************************************************************** sbank30p
009800                                                                  sbank30p
009900***************************************************************** sbank30p
010000* Determine what we have to do (read from or send to screen)    * sbank30p
010100***************************************************************** sbank30p
010200     MOVE LOW-VALUE TO MAPAREA.                                   sbank30p
010300     EVALUATE TRUE                                                sbank30p
010400       WHEN BANK-MAP-FUNCTION-GET                                 sbank30p
010500         PERFORM SCREEN30-READ THRU                               sbank30p
010600                 SCREEN30-READ-EXIT                               sbank30p
010700       WHEN BANK-MAP-FUNCTION-PUT                                 sbank30p
010800         PERFORM SCREEN30-BUILD-AND-SEND THRU                     sbank30p
010900                 SCREEN30-BUILD-AND-SEND-EXIT                     sbank30p
011000       WHEN OTHER                                                 sbank30p
011100         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      sbank30p
011200         MOVE '0002' TO ABEND-CODE                                sbank30p
011300         MOVE SPACES TO ABEND-REASON                              sbank30p
011400         COPY CABENDPO.                                           sbank30p
011500     END-EVALUATE.                                                sbank30p
011600                                                                  sbank30p
011700* Call the appropriate routine to handle the business logic       sbank30p
011800     IF BANK-MAP-FUNCTION-GET                                     sbank30p
011900        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             sbank30p
012000                       COMMAREA(WS-BANK-DATA)                     sbank30p
012100                       LENGTH(LENGTH OF WS-BANK-DATA)             sbank30p
012200        END-EXEC                                                  sbank30p
012300     END-IF.                                                      sbank30p
012400                                                                  sbank30p
012500***************************************************************** sbank30p
012600* Now we have to have finished and can return to our invoker.   * sbank30p
012700***************************************************************** sbank30p
012800* Now return to CICS                                              sbank30p
012900     MOVE WS-BANK-DATA-AREAS (1:LENGTH OF WS-BANK-DATA-AREAS)     sbank30p
013000       TO DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      sbank30p
013100     EXEC CICS                                                    sbank30p
013200          RETURN                                                  sbank30p
013300     END-EXEC.                                                    sbank30p
013400     GOBACK.                                                      sbank30p
013500                                                                  sbank30p
013600***************************************************************** sbank30p
013700* Screen processing for MBANK30                                 * sbank30p
013800*---------------------------------------------------------------* sbank30p
013900* Retrieve data from screen and format it                       * sbank30p
014000***************************************************************** sbank30p
014100 SCREEN30-READ.                                                   sbank30p
014200     MOVE 'BBANK30P' TO WS-BUSINESS-LOGIC-PGM.                    sbank30p
014300     IF BANK-AID-CLEAR                                            sbank30p
014400        SET BANK-AID-PFK03 TO TRUE                                sbank30p
014500        GO TO SCREEN30-READ-EXIT                                  sbank30p
014600     END-IF.                                                      sbank30p
014700     IF BANK-ENV-CICS                                             sbank30p
014800        GO TO SCREEN30-READ-CICS                                  sbank30p
014900     ELSE                                                         sbank30p
015000        GO TO SCREEN30-READ-INET                                  sbank30p
015100     END-IF.                                                      sbank30p
015200                                                                  sbank30p
015300 SCREEN30-READ-CICS.                                              sbank30p
015400     IF BANK-HELP-INACTIVE                                        sbank30p
015500        EXEC CICS RECEIVE MAP('BANK30A')                          sbank30p
015600                          MAPSET('MBANK30')                       sbank30p
015700        END-EXEC                                                  sbank30p
015800     ELSE                                                         sbank30p
015900        EXEC CICS RECEIVE MAP('HELP30A')                          sbank30p
016000                          MAPSET('MBANK30')                       sbank30p
016100        END-EXEC                                                  sbank30p
016200        GO TO SCREEN30-READ-EXIT                                  sbank30p
016300     END-IF.                                                      sbank30p
016400                                                                  sbank30p
016500     IF DET1L IN BANK30AI IS EQUAL TO 0                           sbank30p
016600           MOVE LOW-VALUES TO BANK-SCR30-DET1                     sbank30p
016700     ELSE                                                         sbank30p
016800        MOVE DET1I IN BANK30AI TO BANK-SCR30-DET1                 sbank30p
016900        IF BANK-SCR30-DET1 IS EQUAL TO SPACES OR                  sbank30p
017000           BANK-SCR30-DET1 IS EQUAL TO ALL '_'                    sbank30p
017100           MOVE LOW-VALUES TO BANK-SCR30-DET1                     sbank30p
017200     END-IF.                                                      sbank30p
017300                                                                  sbank30p
017400     IF DET2L IN BANK30AI IS EQUAL TO 0                           sbank30p
017500           MOVE LOW-VALUES TO BANK-SCR30-DET2                     sbank30p
017600     ELSE                                                         sbank30p
017700        MOVE DET2I IN BANK30AI TO BANK-SCR30-DET2                 sbank30p
017800        IF BANK-SCR30-DET2 IS EQUAL TO SPACES OR                  sbank30p
017900           BANK-SCR30-DET2 IS EQUAL TO ALL '_'                    sbank30p
018000           MOVE LOW-VALUES TO BANK-SCR30-DET2                     sbank30p
018100     END-IF.                                                      sbank30p
018200                                                                  sbank30p
018300     IF DET3L IN BANK30AI IS EQUAL TO 0                           sbank30p
018400           MOVE LOW-VALUES TO BANK-SCR30-DET3                     sbank30p
018500     ELSE                                                         sbank30p
018600        MOVE DET3I IN BANK30AI TO BANK-SCR30-DET3                 sbank30p
018700        IF BANK-SCR30-DET3 IS EQUAL TO SPACES OR                  sbank30p
018800           BANK-SCR30-DET3 IS EQUAL TO ALL '_'                    sbank30p
018900           MOVE LOW-VALUES TO BANK-SCR30-DET3                     sbank30p
019000     END-IF.                                                      sbank30p
019100                                                                  sbank30p
019200     IF DET4L IN BANK30AI IS EQUAL TO 0                           sbank30p
019300           MOVE LOW-VALUES TO BANK-SCR30-DET4                     sbank30p
019400     ELSE                                                         sbank30p
019500        MOVE DET4I IN BANK30AI TO BANK-SCR30-DET4                 sbank30p
019600        IF BANK-SCR30-DET4 IS EQUAL TO SPACES OR                  sbank30p
019700           BANK-SCR30-DET4 IS EQUAL TO ALL '_'                    sbank30p
019800           MOVE LOW-VALUES TO BANK-SCR30-DET4                     sbank30p
019900     END-IF.                                                      sbank30p
020000                                                                  sbank30p
020100     IF DET5L IN BANK30AI IS EQUAL TO 0                           sbank30p
020200           MOVE LOW-VALUES TO BANK-SCR30-DET5                     sbank30p
020300     ELSE                                                         sbank30p
020400        MOVE DET5I IN BANK30AI TO BANK-SCR30-DET5                 sbank30p
020500        IF BANK-SCR30-DET5 IS EQUAL TO SPACES OR                  sbank30p
020600           BANK-SCR30-DET5 IS EQUAL TO ALL '_'                    sbank30p
020700           MOVE LOW-VALUES TO BANK-SCR30-DET5                     sbank30p
020800     END-IF.                                                      sbank30p
020900                                                                  sbank30p
021000     IF DET6L IN BANK30AI IS EQUAL TO 0                           sbank30p
021100           MOVE LOW-VALUES TO BANK-SCR30-DET6                     sbank30p
021200     ELSE                                                         sbank30p
021300        MOVE DET6I IN BANK30AI TO BANK-SCR30-DET6                 sbank30p
021400        IF BANK-SCR30-DET6 IS EQUAL TO SPACES OR                  sbank30p
021500           BANK-SCR30-DET6 IS EQUAL TO ALL '_'                    sbank30p
021600           MOVE LOW-VALUES TO BANK-SCR30-DET6                     sbank30p
021700     END-IF.                                                      sbank30p
021800                                                                  sbank30p
021900     GO TO SCREEN30-READ-EXIT.                                    sbank30p
022000                                                                  sbank30p
022100 SCREEN30-READ-INET.                                              sbank30p
022200     MOVE EXT-IP30-DET1 TO BANK-SCR30-DET1.                       sbank30p
022300     MOVE EXT-IP30-DET2 TO BANK-SCR30-DET2.                       sbank30p
022400     MOVE EXT-IP30-DET3 TO BANK-SCR30-DET3.                       sbank30p
022500     MOVE EXT-IP30-DET4 TO BANK-SCR30-DET4.                       sbank30p
022600     MOVE EXT-IP30-DET5 TO BANK-SCR30-DET5.                       sbank30p
022700     MOVE EXT-IP30-DET6 TO BANK-SCR30-DET6.                       sbank30p
022800     GO TO SCREEN30-READ-EXIT.                                    sbank30p
022900                                                                  sbank30p
023000 SCREEN30-READ-EXIT.                                              sbank30p
023100     EXIT.                                                        sbank30p
023200                                                                  sbank30p
023300***************************************************************** sbank30p
023400* Screen processing for SCREEN30 (BANK30/HELP30)                * sbank30p
023500*---------------------------------------------------------------* sbank30p
023600* Build the output screen and send it                           * sbank30p
023700***************************************************************** sbank30p
023800 SCREEN30-BUILD-AND-SEND.                                         sbank30p
023900* Clear map area, get date & time and move to the map             sbank30p
024000     MOVE LOW-VALUES TO BANK30AO.                                 sbank30p
024100     MOVE EIBTIME TO DD-TIME-INPUT-N.                             sbank30p
024200     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      sbank30p
024300     SET DDI-YYDDD TO TRUE.                                       sbank30p
024400     SET DDO-DD-MMM-YYYY TO TRUE.                                 sbank30p
024500     PERFORM CALL-DATECONV THRU                                   sbank30p
024600             CALL-DATECONV-EXIT.                                  sbank30p
024700* Ensure the last map fields are correct                          sbank30p
024800     IF BANK-HELP-ACTIVE                                          sbank30p
024900        MOVE 'MBANK30' TO BANK-LAST-MAPSET                        sbank30p
025000        MOVE 'HELP30A' TO BANK-LAST-MAP                           sbank30p
025100     ELSE                                                         sbank30p
025200        MOVE 'MBANK30' TO BANK-LAST-MAPSET                        sbank30p
025300        MOVE 'BANK30A' TO BANK-LAST-MAP                           sbank30p
025400     END-IF.                                                      sbank30p
025500     IF BANK-ENV-CICS                                             sbank30p
025600        GO TO SCREEN30-BUILD-AND-SEND-CICS                        sbank30p
025700     ELSE                                                         sbank30p
025800        GO TO SCREEN30-BUILD-AND-SEND-INET                        sbank30p
025900     END-IF.                                                      sbank30p
026000                                                                  sbank30p
026100 SCREEN30-BUILD-AND-SEND-CICS.                                    sbank30p
026200     IF BANK-LAST-MAP IS EQUAL TO 'BANK30A'                       sbank30p
026300        GO TO BANK30-BUILD-AND-SEND-CICS                          sbank30p
026400     END-IF.                                                      sbank30p
026500     IF BANK-LAST-MAP IS EQUAL TO 'HELP30A'                       sbank30p
026600        GO TO HELP30-BUILD-AND-SEND-CICS                          sbank30p
026700     END-IF.                                                      sbank30p
026800     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                          sbank30p
026900     MOVE '0003' TO ABEND-CODE                                    sbank30p
027000     MOVE SPACES TO ABEND-REASON                                  sbank30p
027100     COPY CABENDPO.                                               sbank30p
027200     GOBACK.                                                      sbank30p
027300                                                                  sbank30p
027400 BANK30-BUILD-AND-SEND-CICS.                                      sbank30p
027500     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANK30AO==.        sbank30p
027600     COPY CVERSNP1 REPLACING ==<<SCRN>>== BY ==BANK30AO==.        sbank30p
027700     MOVE WS-TRAN-ID TO TRANO IN BANK30AO.                        sbank30p
027800     MOVE DD-TIME-OUTPUT TO TIMEO IN BANK30AO.                    sbank30p
027900     MOVE DDO-DATA TO DATEO IN BANK30AO.                          sbank30p
028000* Move in any error message                                       sbank30p
028100     MOVE BANK-ERROR-MSG TO ERRMSGO IN BANK30AO.                  sbank30p
028200* Move in screen specific fields                                  sbank30p
028300     MOVE BANK-USERID TO USERIDO IN BANK30AO.                     sbank30p
028400     MOVE BANK-USERID-NAME TO USERNMO IN BANK30AO.                sbank30p
028500                                                                  sbank30p
028600     IF BANK-SCR30-ACC1 IS NOT EQUAL TO SPACES                    sbank30p
028700        MOVE -1 TO DET1L IN BANK30AI                              sbank30p
028800        MOVE BANK-SCR30-DET1 TO DET1O IN BANK30AO                 sbank30p
028900     ELSE                                                         sbank30p
029000        MOVE DFHBMASK TO DET1A IN BANK30AI                        sbank30p
029100        MOVE SPACE TO DET1O IN BANK30AO                           sbank30p
029200     END-IF.                                                      sbank30p
029300     MOVE BANK-SCR30-ACC1 TO ACC1O IN BANK30AO.                   sbank30p
029400     MOVE BANK-SCR30-DSC1 TO DSC1O IN BANK30AO.                   sbank30p
029500     MOVE BANK-SCR30-BAL1 TO BAL1O IN BANK30AO.                   sbank30p
029600     MOVE BANK-SCR30-SRV1 TO SRV1O IN BANK30AO.                   sbank30p
029700     MOVE BANK-SCR30-DTE1 TO DTE1O IN BANK30AO.                   sbank30p
029800     MOVE BANK-SCR30-TXN1 TO TXN1O IN BANK30AO.                   sbank30p
029900                                                                  sbank30p
030000     IF BANK-SCR30-ACC2 IS NOT EQUAL TO SPACES                    sbank30p
030100        MOVE -1 TO DET2L IN BANK30AI                              sbank30p
030200        MOVE BANK-SCR30-DET2 TO DET2O IN BANK30AO                 sbank30p
030300     ELSE                                                         sbank30p
030400        MOVE DFHBMASK TO DET2A IN BANK30AI                        sbank30p
030500        MOVE SPACE TO DET2O IN BANK30AO                           sbank30p
030600     END-IF.                                                      sbank30p
030700     MOVE BANK-SCR30-ACC2 TO ACC2O IN BANK30AO.                   sbank30p
030800     MOVE BANK-SCR30-DSC2 TO DSC2O IN BANK30AO.                   sbank30p
030900     MOVE BANK-SCR30-BAL2 TO BAL2O IN BANK30AO.                   sbank30p
031000     MOVE BANK-SCR30-SRV2 TO SRV2O IN BANK30AO.                   sbank30p
031100     MOVE BANK-SCR30-DTE2 TO DTE2O IN BANK30AO.                   sbank30p
031200     MOVE BANK-SCR30-TXN2 TO TXN2O IN BANK30AO.                   sbank30p
031300                                                                  sbank30p
031400     IF BANK-SCR30-ACC3 IS NOT EQUAL TO SPACES                    sbank30p
031500        MOVE -1 TO DET3L IN BANK30AI                              sbank30p
031600        MOVE BANK-SCR30-DET3 TO DET3O IN BANK30AO                 sbank30p
031700     ELSE                                                         sbank30p
031800        MOVE DFHBMASK TO DET3A IN BANK30AI                        sbank30p
031900        MOVE SPACE TO DET3O IN BANK30AO                           sbank30p
032000     END-IF.                                                      sbank30p
032100     MOVE BANK-SCR30-ACC3 TO ACC3O IN BANK30AO.                   sbank30p
032200     MOVE BANK-SCR30-DSC3 TO DSC3O IN BANK30AO.                   sbank30p
032300     MOVE BANK-SCR30-BAL3 TO BAL3O IN BANK30AO.                   sbank30p
032400     MOVE BANK-SCR30-SRV3 TO SRV3O IN BANK30AO.                   sbank30p
032500     MOVE BANK-SCR30-DTE3 TO DTE3O IN BANK30AO.                   sbank30p
032600     MOVE BANK-SCR30-TXN3 TO TXN3O IN BANK30AO.                   sbank30p
032700                                                                  sbank30p
032800     IF BANK-SCR30-ACC4 IS NOT EQUAL TO SPACES                    sbank30p
032900        MOVE -1 TO DET4L IN BANK30AI                              sbank30p
033000        MOVE BANK-SCR30-DET4 TO DET4O IN BANK30AO                 sbank30p
033100     ELSE                                                         sbank30p
033200        MOVE DFHBMASK TO DET4A IN BANK30AI                        sbank30p
033300        MOVE SPACE TO DET4O IN BANK30AO                           sbank30p
033400     END-IF.                                                      sbank30p
033500     MOVE BANK-SCR30-ACC4 TO ACC4O IN BANK30AO.                   sbank30p
033600     MOVE BANK-SCR30-DSC4 TO DSC4O IN BANK30AO.                   sbank30p
033700     MOVE BANK-SCR30-BAL4 TO BAL4O IN BANK30AO.                   sbank30p
033800     MOVE BANK-SCR30-SRV4 TO SRV4O IN BANK30AO.                   sbank30p
033900     MOVE BANK-SCR30-DTE4 TO DTE4O IN BANK30AO.                   sbank30p
034000     MOVE BANK-SCR30-TXN4 TO TXN4O IN BANK30AO.                   sbank30p
034100                                                                  sbank30p
034200     IF BANK-SCR30-ACC5 IS NOT EQUAL TO SPACES                    sbank30p
034300        MOVE -1 TO DET5L IN BANK30AI                              sbank30p
034400        MOVE BANK-SCR30-DET5 TO DET5O IN BANK30AO                 sbank30p
034500     ELSE                                                         sbank30p
034600        MOVE DFHBMASK TO DET5A IN BANK30AI                        sbank30p
034700        MOVE SPACE TO DET5O IN BANK30AO                           sbank30p
034800     END-IF.                                                      sbank30p
034900     MOVE BANK-SCR30-ACC5 TO ACC5O IN BANK30AO.                   sbank30p
035000     MOVE BANK-SCR30-DSC5 TO DSC5O IN BANK30AO.                   sbank30p
035100     MOVE BANK-SCR30-BAL5 TO BAL5O IN BANK30AO.                   sbank30p
035200     MOVE BANK-SCR30-SRV5 TO SRV5O IN BANK30AO.                   sbank30p
035300     MOVE BANK-SCR30-DTE5 TO DTE5O IN BANK30AO.                   sbank30p
035400     MOVE BANK-SCR30-TXN5 TO TXN5O IN BANK30AO.                   sbank30p
035500                                                                  sbank30p
035600     IF BANK-SCR30-ACC6 IS NOT EQUAL TO SPACES                    sbank30p
035700        MOVE -1 TO DET6L IN BANK30AI                              sbank30p
035800        MOVE BANK-SCR30-DET6 TO DET6O IN BANK30AO                 sbank30p
035900     ELSE                                                         sbank30p
036000        MOVE DFHBMASK TO DET6A IN BANK30AI                        sbank30p
036100        MOVE SPACE TO DET6O IN BANK30AO                           sbank30p
036200     END-IF.                                                      sbank30p
036300     MOVE BANK-SCR30-ACC6 TO ACC6O IN BANK30AO.                   sbank30p
036400     MOVE BANK-SCR30-DSC6 TO DSC6O IN BANK30AO.                   sbank30p
036500     MOVE BANK-SCR30-BAL6 TO BAL6O IN BANK30AO.                   sbank30p
036600     MOVE BANK-SCR30-SRV6 TO SRV6O IN BANK30AO.                   sbank30p
036700     MOVE BANK-SCR30-DTE6 TO DTE6O IN BANK30AO.                   sbank30p
036800     MOVE BANK-SCR30-TXN6 TO TXN6O IN BANK30AO.                   sbank30p
036900                                                                  sbank30p
037000     MOVE BANK-SCR30-SRVMSG TO SRVMSGO IN BANK30AO.               sbank30p
037100* Turn colour off if required                                     sbank30p
037200     IF COLOUR-OFF                                                sbank30p
037300        MOVE DFHGREEN TO TXT01C IN BANK30AO                       sbank30p
037400        MOVE DFHGREEN TO SCRNC IN BANK30AO                        sbank30p
037500        MOVE DFHGREEN TO HEAD1C IN BANK30AO                       sbank30p
037600        MOVE DFHGREEN TO DATEC IN BANK30AO                        sbank30p
037700        MOVE DFHGREEN TO TXT02C IN BANK30AO                       sbank30p
037800        MOVE DFHGREEN TO TRANC IN BANK30AO                        sbank30p
037900        MOVE DFHGREEN TO HEAD2C IN BANK30AO                       sbank30p
038000        MOVE DFHGREEN TO TIMEC IN BANK30AO                        sbank30p
038100        MOVE DFHGREEN TO TXT03C IN BANK30AO                       sbank30p
038200        MOVE DFHGREEN TO USERIDC IN BANK30AO                      sbank30p
038300        MOVE DFHGREEN TO TXT04C IN BANK30AO                       sbank30p
038400        MOVE DFHGREEN TO USERNMC IN BANK30AO                      sbank30p
038500        MOVE DFHGREEN TO TXT05C IN BANK30AO                       sbank30p
038600        MOVE DFHGREEN TO TXT06C IN BANK30AO                       sbank30p
038700        MOVE DFHGREEN TO TXT07C IN BANK30AO                       sbank30p
038800        MOVE DFHGREEN TO TXT08C IN BANK30AO                       sbank30p
038900        MOVE DFHGREEN TO TXT09C IN BANK30AO                       sbank30p
039000        MOVE DFHGREEN TO TXT10C IN BANK30AO                       sbank30p
039100        MOVE DFHGREEN TO TXT11C IN BANK30AO                       sbank30p
039200        MOVE DFHGREEN TO TXT12C IN BANK30AO                       sbank30p
039300        MOVE DFHGREEN TO TXT13C IN BANK30AO                       sbank30p
039400        MOVE DFHGREEN TO TXT14C IN BANK30AO                       sbank30p
039500        MOVE DFHGREEN TO TXT15C IN BANK30AO                       sbank30p
039600        MOVE DFHGREEN TO DET1C IN BANK30AO                        sbank30p
039700        MOVE DFHGREEN TO ACC1C IN BANK30AO                        sbank30p
039800        MOVE DFHGREEN TO DSC1C IN BANK30AO                        sbank30p
039900        MOVE DFHGREEN TO BAL1C IN BANK30AO                        sbank30p
040000        MOVE DFHGREEN TO SRV1C IN BANK30AO                        sbank30p
040100        MOVE DFHGREEN TO DTE1C IN BANK30AO                        sbank30p
040200        MOVE DFHGREEN TO TXN1C IN BANK30AO                        sbank30p
040300        MOVE DFHGREEN TO DET2C IN BANK30AO                        sbank30p
040400        MOVE DFHGREEN TO ACC2C IN BANK30AO                        sbank30p
040500        MOVE DFHGREEN TO DSC2C IN BANK30AO                        sbank30p
040600        MOVE DFHGREEN TO BAL2C IN BANK30AO                        sbank30p
040700        MOVE DFHGREEN TO SRV2C IN BANK30AO                        sbank30p
040800        MOVE DFHGREEN TO DTE2C IN BANK30AO                        sbank30p
040900        MOVE DFHGREEN TO TXN2C IN BANK30AO                        sbank30p
041000        MOVE DFHGREEN TO ACC3C IN BANK30AO                        sbank30p
041100        MOVE DFHGREEN TO DSC3C IN BANK30AO                        sbank30p
041200        MOVE DFHGREEN TO BAL3C IN BANK30AO                        sbank30p
041300        MOVE DFHGREEN TO SRV3C IN BANK30AO                        sbank30p
041400        MOVE DFHGREEN TO DTE3C IN BANK30AO                        sbank30p
041500        MOVE DFHGREEN TO TXN3C IN BANK30AO                        sbank30p
041600        MOVE DFHGREEN TO DET4C IN BANK30AO                        sbank30p
041700        MOVE DFHGREEN TO ACC4C IN BANK30AO                        sbank30p
041800        MOVE DFHGREEN TO DSC4C IN BANK30AO                        sbank30p
041900        MOVE DFHGREEN TO BAL4C IN BANK30AO                        sbank30p
042000        MOVE DFHGREEN TO SRV4C IN BANK30AO                        sbank30p
042100        MOVE DFHGREEN TO DTE4C IN BANK30AO                        sbank30p
042200        MOVE DFHGREEN TO TXN4C IN BANK30AO                        sbank30p
042300        MOVE DFHGREEN TO DET5C IN BANK30AO                        sbank30p
042400        MOVE DFHGREEN TO ACC5C IN BANK30AO                        sbank30p
042500        MOVE DFHGREEN TO DSC5C IN BANK30AO                        sbank30p
042600        MOVE DFHGREEN TO BAL5C IN BANK30AO                        sbank30p
042700        MOVE DFHGREEN TO SRV5C IN BANK30AO                        sbank30p
042800        MOVE DFHGREEN TO DTE5C IN BANK30AO                        sbank30p
042900        MOVE DFHGREEN TO TXN5C IN BANK30AO                        sbank30p
043000        MOVE DFHGREEN TO DET6C IN BANK30AO                        sbank30p
043100        MOVE DFHGREEN TO ACC6C IN BANK30AO                        sbank30p
043200        MOVE DFHGREEN TO DSC6C IN BANK30AO                        sbank30p
043300        MOVE DFHGREEN TO BAL6C IN BANK30AO                        sbank30p
043400        MOVE DFHGREEN TO SRV6C IN BANK30AO                        sbank30p
043500        MOVE DFHGREEN TO DTE6C IN BANK30AO                        sbank30p
043600        MOVE DFHGREEN TO TXN6C IN BANK30AO                        sbank30p
043700        MOVE DFHGREEN TO SRVMSGC IN BANK30AO                      sbank30p
043800        MOVE DFHGREEN TO ERRMSGC IN BANK30AO                      sbank30p
043900        MOVE DFHGREEN TO TXT16C IN BANK30AO                       sbank30p
044000        MOVE DFHGREEN TO VERC IN BANK30AO                         sbank30p
044100     END-IF.                                                      sbank30p
044200                                                                  sbank30p
044300     EXEC CICS SEND MAP('BANK30A')                                sbank30p
044400                    MAPSET('MBANK30')                             sbank30p
044500                    ERASE                                         sbank30p
044600                    FREEKB                                        sbank30p
044700     END-EXEC.                                                    sbank30p
044800     GO TO SCREEN30-BUILD-AND-SEND-EXIT.                          sbank30p
044900                                                                  sbank30p
045000 HELP30-BUILD-AND-SEND-CICS.                                      sbank30p
045100     COPY CSCRNHP2 REPLACING ==:OPTN:== BY ==BANK==               sbank30p
045200                             ==<<SCRN>>== BY ==HELP30AO==.        sbank30p
045300                                                                  sbank30p
045400     EXEC CICS SEND MAP('HELP30A')                                sbank30p
045500                    MAPSET('MBANK30')                             sbank30p
045600                    ERASE                                         sbank30p
045700                    FREEKB                                        sbank30p
045800     END-EXEC.                                                    sbank30p
045900     GO TO SCREEN30-BUILD-AND-SEND-EXIT.                          sbank30p
046000                                                                  sbank30p
046100 SCREEN30-BUILD-AND-SEND-INET.                                    sbank30p
046200     MOVE SPACES TO EXT-OP-DATA.                                  sbank30p
046300     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              sbank30p
046400     MOVE DDO-DATA TO EXT-OP-DATE.                                sbank30p
046500     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          sbank30p
046600     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         sbank30p
046700     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          sbank30p
046800     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          sbank30p
046900     CALL 'SVERSONP' USING SCREEN-TITLES.                         sbank30p
047000     MOVE VERSION TO EXT-OP-VERSION.                              sbank30p
047100* Move in screen name                                             sbank30p
047200     MOVE 'BANK30' TO EXT-OP-SCREEN.                              sbank30p
047300* Move in userid and any error message                            sbank30p
047400     MOVE BANK-ERROR-MSG TO EXT-OP-ERR-MSG.                       sbank30p
047500     MOVE BANK-USERID TO EXT-OP-USERID.                           sbank30p
047600     MOVE BANK-USERID-NAME TO EXT-OP-NAME.                        sbank30p
047700* Move in screen specific fields                                  sbank30p
047800     MOVE BANK-SCR30-DET1 TO EXT-OP30-DET1.                       sbank30p
047900     MOVE BANK-SCR30-ACC1 TO EXT-OP30-ACC1.                       sbank30p
048000     MOVE BANK-SCR30-DSC1 TO EXT-OP30-DSC1.                       sbank30p
048100     MOVE BANK-SCR30-BAL1 TO EXT-OP30-BAL1.                       sbank30p
048200     MOVE BANK-SCR30-SRV1 TO EXT-OP30-SRV1.                       sbank30p
048300     MOVE BANK-SCR30-DTE1 TO EXT-OP30-DTE1.                       sbank30p
048400     MOVE BANK-SCR30-TXN1 TO EXT-OP30-TXN1.                       sbank30p
048500     MOVE BANK-SCR30-DET2 TO EXT-OP30-DET2.                       sbank30p
048600     MOVE BANK-SCR30-ACC2 TO EXT-OP30-ACC2.                       sbank30p
048700     MOVE BANK-SCR30-DSC2 TO EXT-OP30-DSC2.                       sbank30p
048800     MOVE BANK-SCR30-BAL2 TO EXT-OP30-BAL2.                       sbank30p
048900     MOVE BANK-SCR30-SRV2 TO EXT-OP30-SRV2.                       sbank30p
049000     MOVE BANK-SCR30-DTE2 TO EXT-OP30-DTE2.                       sbank30p
049100     MOVE BANK-SCR30-TXN2 TO EXT-OP30-TXN2.                       sbank30p
049200     MOVE BANK-SCR30-DET3 TO EXT-OP30-DET3.                       sbank30p
049300     MOVE BANK-SCR30-ACC3 TO EXT-OP30-ACC3.                       sbank30p
049400     MOVE BANK-SCR30-DSC3 TO EXT-OP30-DSC3.                       sbank30p
049500     MOVE BANK-SCR30-BAL3 TO EXT-OP30-BAL3.                       sbank30p
049600     MOVE BANK-SCR30-SRV3 TO EXT-OP30-SRV3.                       sbank30p
049700     MOVE BANK-SCR30-DTE3 TO EXT-OP30-DTE3.                       sbank30p
049800     MOVE BANK-SCR30-TXN3 TO EXT-OP30-TXN3.                       sbank30p
049900     MOVE BANK-SCR30-DET4 TO EXT-OP30-DET4.                       sbank30p
050000     MOVE BANK-SCR30-ACC4 TO EXT-OP30-ACC4.                       sbank30p
050100     MOVE BANK-SCR30-DSC4 TO EXT-OP30-DSC4.                       sbank30p
050200     MOVE BANK-SCR30-BAL4 TO EXT-OP30-BAL4.                       sbank30p
050300     MOVE BANK-SCR30-SRV4 TO EXT-OP30-SRV4.                       sbank30p
050400     MOVE BANK-SCR30-DTE4 TO EXT-OP30-DTE4.                       sbank30p
050500     MOVE BANK-SCR30-TXN4 TO EXT-OP30-TXN4.                       sbank30p
050600     MOVE BANK-SCR30-DET5 TO EXT-OP30-DET5.                       sbank30p
050700     MOVE BANK-SCR30-ACC5 TO EXT-OP30-ACC5.                       sbank30p
050800     MOVE BANK-SCR30-DSC5 TO EXT-OP30-DSC5.                       sbank30p
050900     MOVE BANK-SCR30-BAL5 TO EXT-OP30-BAL5.                       sbank30p
051000     MOVE BANK-SCR30-SRV5 TO EXT-OP30-SRV5.                       sbank30p
051100     MOVE BANK-SCR30-DTE5 TO EXT-OP30-DTE5.                       sbank30p
051200     MOVE BANK-SCR30-TXN5 TO EXT-OP30-TXN5.                       sbank30p
051300     MOVE BANK-SCR30-DET6 TO EXT-OP30-DET6.                       sbank30p
051400     MOVE BANK-SCR30-ACC6 TO EXT-OP30-ACC6.                       sbank30p
051500     MOVE BANK-SCR30-DSC6 TO EXT-OP30-DSC6.                       sbank30p
051600     MOVE BANK-SCR30-BAL6 TO EXT-OP30-BAL6.                       sbank30p
051700     MOVE BANK-SCR30-SRV6 TO EXT-OP30-SRV6.                       sbank30p
051800     MOVE BANK-SCR30-DTE6 TO EXT-OP30-DTE6.                       sbank30p
051900     MOVE BANK-SCR30-TXN6 TO EXT-OP30-TXN6.                       sbank30p
052000                                                                  sbank30p
052100 SCREEN30-BUILD-AND-SEND-EXIT.                                    sbank30p
052200     EXIT.                                                        sbank30p
052300                                                                  sbank30p
052400***************************************************************** sbank30p
052500* Call common routine to perform date conversions               * sbank30p
052600***************************************************************** sbank30p
052700 CALL-DATECONV.                                                   sbank30p
052800     MOVE BANK-ENV TO DD-ENV.                                     sbank30p
052900     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           sbank30p
053000     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            sbank30p
053100 CALL-DATECONV-EXIT.                                              sbank30p
053200     EXIT.                                                        sbank30p
053300                                                                  sbank30p
053400* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     sbank30p
