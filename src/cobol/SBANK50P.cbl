000100***************************************************************** sbank50p
000200*                                                               * sbank50p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * sbank50p
000400*   This demonstration program is provided for use by users     * sbank50p
000500*   of Micro Focus products and may be used, modified and       * sbank50p
000600*   distributed as part of your application provided that       * sbank50p
000700*   you properly acknowledge the copyright of Micro Focus       * sbank50p
000800*   in this material.                                           * sbank50p
000900*                                                               * sbank50p
001000***************************************************************** sbank50p
001100                                                                  sbank50p
001200***************************************************************** sbank50p
001300* Program:     SBANK50P.CBL (CICS Version)                      * sbank50p
001400* Layer:       Screen handling                                  * sbank50p
001500* Function:    Transfer funds between accounts                  * sbank50p
001600***************************************************************** sbank50p
001700                                                                  sbank50p
001800 IDENTIFICATION DIVISION.                                         sbank50p
001900 PROGRAM-ID.                                                      sbank50p
002000     SBANK50P.                                                    sbank50p
002100 DATE-WRITTEN.                                                    sbank50p
002200     September 2002.                                              sbank50p
002300 DATE-COMPILED.                                                   sbank50p
002400     Today.                                                       sbank50p
002500                                                                  sbank50p
002600 ENVIRONMENT DIVISION.                                            sbank50p
002700                                                                  sbank50p
002800 DATA DIVISION.                                                   sbank50p
002900 WORKING-STORAGE SECTION.                                         sbank50p
003000 01  WS-MISC-STORAGE.                                             sbank50p
003100   05  WS-PROGRAM-ID                         PIC X(8)             sbank50p
003200       VALUE 'SBANK50P'.                                          sbank50p
003300   05  WS-TRAN-ID                            PIC X(4).            sbank50p
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             sbank50p
003500       VALUE SPACES.                                              sbank50p
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             sbank50p
003700       VALUE 'UNKNOWN'.                                           sbank50p
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      sbank50p
003900   05  WS-WORK1                              PIC X(1).            sbank50p
004000   05  WS-SUB1                               PIC S9(4) COMP.      sbank50p
         05  WS-VERSION                            PIC X(7).
                                                                        sbank50p
004100                                                                  
004200 01  MAPAREA                                 PIC X(2048).         sbank50p
004300 COPY MBANK50.                                                    sbank50p
004400                                                                  sbank50p
004500 01  WS-TIME-DATE-WORK-AREA.                                      sbank50p
004600 COPY CDATED.                                                     sbank50p
004700                                                                  sbank50p
004800 01  WS-BANK-DATA-AREAS.                                          sbank50p
004900   05  WS-BANK-DATA.                                              sbank50p
005000 COPY CBANKDAT.                                                   sbank50p
005100   05  WS-BANK-EXT-DATA.                                          sbank50p
005200 COPY CBANKEXT.                                                   sbank50p
005300                                                                  sbank50p
005400 COPY CSCRNHDD.                                                   sbank50p
005500                                                                  sbank50p
005600 COPY CVERSND.                                                    sbank50p
005700                                                                  sbank50p
005800 COPY DFHAID.                                                     sbank50p
005900                                                                  sbank50p
006000 COPY DFHBMSCA.                                                   sbank50p
006100                                                                  sbank50p
006200 COPY CABENDD.                                                    sbank50p
006300                                                                  sbank50p
006400 LINKAGE SECTION.                                                 sbank50p
006500 01  DFHCOMMAREA.                                                 sbank50p
006600   05  FILLER                                PIC X(1)             sbank50p
006700       OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.             sbank50p
006800                                                                  sbank50p
006900 PROCEDURE DIVISION.                                              sbank50p
007000***************************************************************** sbank50p
007100* Write entry to log to show we have been invoked               * sbank50p
007200***************************************************************** sbank50p
007300     COPY CTRACE.                                                 sbank50p
007400                                                                  sbank50p
007500***************************************************************** sbank50p
007600* Store our transaction-id                                      * sbank50p
007700***************************************************************** sbank50p
007800     MOVE EIBTRNID TO WS-TRAN-ID.                                 sbank50p
007900                                                                  sbank50p
008000***************************************************************** sbank50p
008100* Store passed data or abend if there wasn't any                * sbank50p
008200***************************************************************** sbank50p
008300     IF EIBCALEN IS EQUAL TO 0                                    sbank50p
008400        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       sbank50p
008500        MOVE '0001' TO ABEND-CODE                                 sbank50p
008600        MOVE SPACES TO ABEND-REASON                               sbank50p
008700        COPY CABENDPO.                                            sbank50p
008800     ELSE                                                         sbank50p
008900        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        sbank50p
009000        MOVE LOW-VALUES TO WS-BANK-DATA                           sbank50p
009100        MOVE DFHCOMMAREA (1:EIBCALEN)                             sbank50p
009200          TO WS-BANK-DATA-AREAS (1:LENGTH OF WS-BANK-DATA-AREAS)  sbank50p
009300     END-IF.                                                      sbank50p
009400                                                                  sbank50p
009500***************************************************************** sbank50p
009600* This is the main process                                      * sbank50p
009700***************************************************************** sbank50p
009800                                                                  sbank50p
009900***************************************************************** sbank50p
010000* Determine what we have to do (read from or send to screen)    * sbank50p
010100***************************************************************** sbank50p
010200     MOVE LOW-VALUE TO MAPAREA.                                   sbank50p
010300     EVALUATE TRUE                                                sbank50p
010400       WHEN BANK-MAP-FUNCTION-GET                                 sbank50p
010500         PERFORM SCREEN50-READ THRU                               sbank50p
010600                 SCREEN50-READ-EXIT                               sbank50p
010700       WHEN BANK-MAP-FUNCTION-PUT                                 sbank50p
010800         PERFORM SCREEN50-BUILD-AND-SEND THRU                     sbank50p
010900                 SCREEN50-BUILD-AND-SEND-EXIT                     sbank50p
011000       WHEN OTHER                                                 sbank50p
011100         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      sbank50p
011200         MOVE '0002' TO ABEND-CODE                                sbank50p
011300         MOVE SPACES TO ABEND-REASON                              sbank50p
011400         COPY CABENDPO.                                           sbank50p
011500     END-EVALUATE.                                                sbank50p
011600                                                                  sbank50p
011700* Call the appropriate routine to handle the business logic       sbank50p
011800     IF BANK-MAP-FUNCTION-GET                                     sbank50p
011900        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             sbank50p
012000                       COMMAREA(WS-BANK-DATA)                     sbank50p
012100                       LENGTH(LENGTH OF WS-BANK-DATA)             sbank50p
012200        END-EXEC                                                  sbank50p
012300     END-IF.                                                      sbank50p
012400                                                                  sbank50p
012500***************************************************************** sbank50p
012600* Now we have to have finished and can return to our invoker.   * sbank50p
012700***************************************************************** sbank50p
012800* Now return to CICS                                              sbank50p
012900     MOVE WS-BANK-DATA-AREAS (1:LENGTH OF WS-BANK-DATA-AREAS)     sbank50p
013000       TO DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      sbank50p
013100     EXEC CICS                                                    sbank50p
013200          RETURN                                                  sbank50p
013300     END-EXEC.                                                    sbank50p
013400     GOBACK.                                                      sbank50p
013500                                                                  sbank50p
013600***************************************************************** sbank50p
013700* Screen processing for MBANK50                                 * sbank50p
013800*---------------------------------------------------------------* sbank50p
013900* Retrieve data from screen and format it                       * sbank50p
014000***************************************************************** sbank50p
014100 SCREEN50-READ.                                                   sbank50p
014200     MOVE 'BBANK50P' TO WS-BUSINESS-LOGIC-PGM.                    sbank50p
014300     IF BANK-AID-CLEAR                                            sbank50p
014400        SET BANK-AID-PFK03 TO TRUE                                sbank50p
014500        GO TO SCREEN50-READ-EXIT                                  sbank50p
014600     END-IF.                                                      sbank50p
014700     IF BANK-ENV-CICS                                             sbank50p
014800        GO TO SCREEN50-READ-CICS                                  sbank50p
014900     ELSE                                                         sbank50p
015000        GO TO SCREEN50-READ-INET                                  sbank50p
015100     END-IF.                                                      sbank50p
015200                                                                  sbank50p
015300 SCREEN50-READ-CICS.                                              sbank50p
015400     IF BANK-HELP-INACTIVE                                        sbank50p
015500        EXEC CICS RECEIVE MAP('BANK50A')                          sbank50p
015600                          MAPSET('MBANK50')                       sbank50p
015700        END-EXEC                                                  sbank50p
015800     ELSE                                                         sbank50p
015900        EXEC CICS RECEIVE MAP('HELP50A')                          sbank50p
016000                          MAPSET('MBANK50')                       sbank50p
016100        END-EXEC                                                  sbank50p
016200        GO TO SCREEN50-READ-EXIT                                  sbank50p
016300     END-IF.                                                      sbank50p
016400                                                                  sbank50p
016500     IF XFERAMTL IN BANK50AI IS EQUAL TO 0                        sbank50p
016600           MOVE LOW-VALUES TO BANK-SCR50-XFER                     sbank50p
016700     ELSE                                                         sbank50p
016800        MOVE XFERAMTI IN BANK50AI TO BANK-SCR50-XFER              sbank50p
016900        IF BANK-SCR50-XFER IS EQUAL TO SPACES                     sbank50p
017000           MOVE LOW-VALUES TO BANK-SCR50-XFER                     sbank50p
017100     END-IF.                                                      sbank50p
017200                                                                  sbank50p
017300     IF FROM1L IN BANK50AI IS EQUAL TO 0                          sbank50p
017400           MOVE LOW-VALUES TO BANK-SCR50-FRM1                     sbank50p
017500     ELSE                                                         sbank50p
017600        MOVE FROM1I IN BANK50AI TO BANK-SCR50-FRM1                sbank50p
017700        IF BANK-SCR50-FRM1 IS EQUAL TO SPACES OR                  sbank50p
017800           BANK-SCR50-FRM1 IS EQUAL TO ALL '_'                    sbank50p
017900           MOVE LOW-VALUES TO BANK-SCR50-FRM1                     sbank50p
018000     END-IF.                                                      sbank50p
018100                                                                  sbank50p
018200     IF FROM2L IN BANK50AI IS EQUAL TO 0                          sbank50p
018300           MOVE LOW-VALUES TO BANK-SCR50-FRM2                     sbank50p
018400     ELSE                                                         sbank50p
018500        MOVE FROM2I IN BANK50AI TO BANK-SCR50-FRM2                sbank50p
018600        IF BANK-SCR50-FRM2 IS EQUAL TO SPACES OR                  sbank50p
018700           BANK-SCR50-FRM2 IS EQUAL TO ALL '_'                    sbank50p
018800           MOVE LOW-VALUES TO BANK-SCR50-FRM2                     sbank50p
018900     END-IF.                                                      sbank50p
019000                                                                  sbank50p
019100     IF FROM3L IN BANK50AI IS EQUAL TO 0                          sbank50p
019200           MOVE LOW-VALUES TO BANK-SCR50-FRM3                     sbank50p
019300     ELSE                                                         sbank50p
019400        MOVE FROM3I IN BANK50AI TO BANK-SCR50-FRM3                sbank50p
019500        IF BANK-SCR50-FRM3 IS EQUAL TO SPACES OR                  sbank50p
019600           BANK-SCR50-FRM3 IS EQUAL TO ALL '_'                    sbank50p
019700           MOVE LOW-VALUES TO BANK-SCR50-FRM3                     sbank50p
019800     END-IF.                                                      sbank50p
019900                                                                  sbank50p
020000     IF FROM4L IN BANK50AI IS EQUAL TO 0                          sbank50p
020100           MOVE LOW-VALUES TO BANK-SCR50-FRM4                     sbank50p
020200     ELSE                                                         sbank50p
020300        MOVE FROM4I IN BANK50AI TO BANK-SCR50-FRM4                sbank50p
020400        IF BANK-SCR50-FRM4 IS EQUAL TO SPACES OR                  sbank50p
020500           BANK-SCR50-FRM4 IS EQUAL TO ALL '_'                    sbank50p
020600           MOVE LOW-VALUES TO BANK-SCR50-FRM4                     sbank50p
020700     END-IF.                                                      sbank50p
020800                                                                  sbank50p
020900     IF FROM5L IN BANK50AI IS EQUAL TO 0                          sbank50p
021000           MOVE LOW-VALUES TO BANK-SCR50-FRM5                     sbank50p
021100     ELSE                                                         sbank50p
021200        MOVE FROM5I IN BANK50AI TO BANK-SCR50-FRM5                sbank50p
021300        IF BANK-SCR50-FRM5 IS EQUAL TO SPACES OR                  sbank50p
021400           BANK-SCR50-FRM5 IS EQUAL TO ALL '_'                    sbank50p
021500           MOVE LOW-VALUES TO BANK-SCR50-FRM5                     sbank50p
021600     END-IF.                                                      sbank50p
021700                                                                  sbank50p
021800     IF FROM6L IN BANK50AI IS EQUAL TO 0                          sbank50p
021900           MOVE LOW-VALUES TO BANK-SCR50-FRM6                     sbank50p
022000     ELSE                                                         sbank50p
022100        MOVE FROM6I IN BANK50AI TO BANK-SCR50-FRM6                sbank50p
022200        IF BANK-SCR50-FRM6 IS EQUAL TO SPACES OR                  sbank50p
022300           BANK-SCR50-FRM6 IS EQUAL TO ALL '_'                    sbank50p
022400           MOVE LOW-VALUES TO BANK-SCR50-FRM6                     sbank50p
022500     END-IF.                                                      sbank50p
022600                                                                  sbank50p
022700     IF TO1L IN BANK50AI IS EQUAL TO 0                            sbank50p
022800           MOVE LOW-VALUES TO BANK-SCR50-TO1                      sbank50p
022900     ELSE                                                         sbank50p
023000        MOVE TO1I IN BANK50AI TO BANK-SCR50-TO1                   sbank50p
023100        IF BANK-SCR50-TO1 IS EQUAL TO SPACES OR                   sbank50p
023200           BANK-SCR50-TO1 IS EQUAL TO ALL '_'                     sbank50p
023300           MOVE LOW-VALUES TO BANK-SCR50-TO1                      sbank50p
023400     END-IF.                                                      sbank50p
023500                                                                  sbank50p
023600     IF TO2L IN BANK50AI IS EQUAL TO 0                            sbank50p
023700           MOVE LOW-VALUES TO BANK-SCR50-TO2                      sbank50p
023800     ELSE                                                         sbank50p
023900        MOVE TO2I IN BANK50AI TO BANK-SCR50-TO2                   sbank50p
024000        IF BANK-SCR50-TO2 IS EQUAL TO SPACES OR                   sbank50p
024100           BANK-SCR50-TO2 IS EQUAL TO ALL '_'                     sbank50p
024200           MOVE LOW-VALUES TO BANK-SCR50-TO2                      sbank50p
024300     END-IF.                                                      sbank50p
024400                                                                  sbank50p
024500     IF TO3L IN BANK50AI IS EQUAL TO 0                            sbank50p
024600           MOVE LOW-VALUES TO BANK-SCR50-TO3                      sbank50p
024700     ELSE                                                         sbank50p
024800        MOVE TO3I IN BANK50AI TO BANK-SCR50-TO3                   sbank50p
024900        IF BANK-SCR50-TO3 IS EQUAL TO SPACES OR                   sbank50p
025000           BANK-SCR50-TO3 IS EQUAL TO ALL '_'                     sbank50p
025100           MOVE LOW-VALUES TO BANK-SCR50-TO3                      sbank50p
025200     END-IF.                                                      sbank50p
025300                                                                  sbank50p
025400     IF TO4L IN BANK50AI IS EQUAL TO 0                            sbank50p
025500           MOVE LOW-VALUES TO BANK-SCR50-TO4                      sbank50p
025600     ELSE                                                         sbank50p
025700        MOVE TO4I IN BANK50AI TO BANK-SCR50-TO4                   sbank50p
025800        IF BANK-SCR50-TO4 IS EQUAL TO SPACES OR                   sbank50p
025900           BANK-SCR50-TO4 IS EQUAL TO ALL '_'                     sbank50p
026000           MOVE LOW-VALUES TO BANK-SCR50-TO4                      sbank50p
026100     END-IF.                                                      sbank50p
026200                                                                  sbank50p
026300     IF TO5L IN BANK50AI IS EQUAL TO 0                            sbank50p
026400           MOVE LOW-VALUES TO BANK-SCR50-TO5                      sbank50p
026500     ELSE                                                         sbank50p
026600        MOVE TO5I IN BANK50AI TO BANK-SCR50-TO5                   sbank50p
026700        IF BANK-SCR50-TO5 IS EQUAL TO SPACES OR                   sbank50p
026800           BANK-SCR50-TO5 IS EQUAL TO ALL '_'                     sbank50p
026900           MOVE LOW-VALUES TO BANK-SCR50-TO5                      sbank50p
027000     END-IF.                                                      sbank50p
027100                                                                  sbank50p
027200     IF TO6L IN BANK50AI IS EQUAL TO 0                            sbank50p
027300           MOVE LOW-VALUES TO BANK-SCR50-TO6                      sbank50p
027400     ELSE                                                         sbank50p
027500        MOVE TO6I IN BANK50AI TO BANK-SCR50-TO6                   sbank50p
027600        IF BANK-SCR50-TO6 IS EQUAL TO SPACES OR                   sbank50p
027700           BANK-SCR50-TO6 IS EQUAL TO ALL '_'                     sbank50p
027800           MOVE LOW-VALUES TO BANK-SCR50-TO6                      sbank50p
027900     END-IF.                                                      sbank50p
028000                                                                  sbank50p
028100     GO TO SCREEN50-READ-EXIT.                                    sbank50p
028200                                                                  sbank50p
028300 SCREEN50-READ-INET.                                              sbank50p
028400     MOVE EXT-IP50-XFER TO BANK-SCR50-XFER.                       sbank50p
028500     MOVE EXT-IP50-FRM1 TO BANK-SCR50-FRM1.                       sbank50p
028600     MOVE EXT-IP50-TO1 TO BANK-SCR50-TO1.                         sbank50p
028700     MOVE EXT-IP50-FRM2 TO BANK-SCR50-FRM2.                       sbank50p
028800     MOVE EXT-IP50-TO2 TO BANK-SCR50-TO2.                         sbank50p
028900     MOVE EXT-IP50-FRM3 TO BANK-SCR50-FRM3.                       sbank50p
029000     MOVE EXT-IP50-TO3 TO BANK-SCR50-TO3.                         sbank50p
029100     MOVE EXT-IP50-FRM4 TO BANK-SCR50-FRM4.                       sbank50p
029200     MOVE EXT-IP50-TO4 TO BANK-SCR50-TO4.                         sbank50p
029300     MOVE EXT-IP50-FRM5 TO BANK-SCR50-FRM5.                       sbank50p
029400     MOVE EXT-IP50-TO5 TO BANK-SCR50-TO5.                         sbank50p
029500     MOVE EXT-IP50-FRM6 TO BANK-SCR50-FRM6.                       sbank50p
029600     MOVE EXT-IP50-TO6 TO BANK-SCR50-TO6.                         sbank50p
029700     GO TO SCREEN50-READ-EXIT.                                    sbank50p
029800                                                                  sbank50p
029900 SCREEN50-READ-EXIT.                                              sbank50p
030000     EXIT.                                                        sbank50p
030100                                                                  sbank50p
030200***************************************************************** sbank50p
030300* Screen processing for SCREEN50 (BANK50/HELP50)                * sbank50p
030400*---------------------------------------------------------------* sbank50p
030500* Build the output screen and send it                           * sbank50p
030600***************************************************************** sbank50p
030700 SCREEN50-BUILD-AND-SEND.                                         sbank50p
030800* Clear map area, get date & time and move to the map             sbank50p
030900     MOVE LOW-VALUES TO BANK50AO.                                 sbank50p
031000     MOVE EIBTIME TO DD-TIME-INPUT-N.                             sbank50p
031100     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      sbank50p
031200     SET DDI-YYDDD TO TRUE.                                       sbank50p
031300     SET DDO-DD-MMM-YYYY TO TRUE.                                 sbank50p
031400     PERFORM CALL-DATECONV THRU                                   sbank50p
031500             CALL-DATECONV-EXIT.                                  sbank50p
031600* Ensure the last map fields are correct                          sbank50p
031700     IF BANK-HELP-ACTIVE                                          sbank50p
031800        MOVE 'MBANK50' TO BANK-LAST-MAPSET                        sbank50p
031900        MOVE 'HELP50A' TO BANK-LAST-MAP                           sbank50p
032000     ELSE                                                         sbank50p
032100        MOVE 'MBANK50' TO BANK-LAST-MAPSET                        sbank50p
032200        MOVE 'BANK50A' TO BANK-LAST-MAP                           sbank50p
032300     END-IF.                                                      sbank50p
032400     IF BANK-ENV-CICS                                             sbank50p
032500        GO TO SCREEN50-BUILD-AND-SEND-CICS                        sbank50p
032600     ELSE                                                         sbank50p
032700        GO TO SCREEN50-BUILD-AND-SEND-INET                        sbank50p
032800     END-IF.                                                      sbank50p
032900                                                                  sbank50p
033000 SCREEN50-BUILD-AND-SEND-CICS.                                    sbank50p
033100     IF BANK-LAST-MAP IS EQUAL TO 'BANK50A'                       sbank50p
033200        GO TO BANK50-BUILD-AND-SEND-CICS                          sbank50p
033300     END-IF.                                                      sbank50p
033400     IF BANK-LAST-MAP IS EQUAL TO 'HELP50A'                       sbank50p
033500        GO TO HELP50-BUILD-AND-SEND-CICS                          sbank50p
033600     END-IF.                                                      sbank50p
033700     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                          sbank50p
033800     MOVE '0003' TO ABEND-CODE                                    sbank50p
033900     MOVE SPACES TO ABEND-REASON                                  sbank50p
034000     COPY CABENDPO.                                               sbank50p
034100     GOBACK.                                                      sbank50p
034200                                                                  sbank50p
034300 BANK50-BUILD-AND-SEND-CICS.                                      sbank50p
034400     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANK50AO==.        sbank50p
034500     COPY CVERSNP1 REPLACING ==<<SCRN>>== BY ==BANK50AO==.        sbank50p
034600     MOVE WS-TRAN-ID TO TRANO IN BANK50AO.                        sbank50p
034700     MOVE DD-TIME-OUTPUT TO TIMEO IN BANK50AO.                    sbank50p
034800     MOVE DDO-DATA TO DATEO IN BANK50AO.                          sbank50p
034900* Move in any error message                                       sbank50p
035000     MOVE BANK-ERROR-MSG TO ERRMSGO IN BANK50AO.                  sbank50p
035100* Move in screen specific fields                                  sbank50p
035200     MOVE BANK-USERID TO USERIDO IN BANK50AO.                     sbank50p
035300     MOVE BANK-USERID-NAME TO USERNMO IN BANK50AO.                sbank50p
035400                                                                  sbank50p
035500     MOVE BANK-SCR50-XFER TO XFERAMTO IN BANK50AO.                sbank50p
035600     IF BANK-SCR50-ACC1 IS NOT EQUAL TO SPACES                    sbank50p
035700        MOVE -1 TO FROM1L IN BANK50AI                             sbank50p
035800        MOVE -1 TO TO1L IN BANK50AI                               sbank50p
035900        MOVE BANK-SCR50-FRM1 TO FROM1O IN BANK50AO                sbank50p
036000        MOVE BANK-SCR50-TO1  TO TO1O  IN BANK50AO                 sbank50p
036100        MOVE BANK-SCR50-ACC1 TO ACC1O IN BANK50AO                 sbank50p
036200        MOVE BANK-SCR50-DSC1 TO DSC1O IN BANK50AO                 sbank50p
036300        MOVE BANK-SCR50-BAL1 TO BAL1O IN BANK50AO                 sbank50p
036400     ELSE                                                         sbank50p
036500        MOVE DFHBMASK TO FROM1A IN BANK50AI                       sbank50p
036600        MOVE DFHBMASK TO TO1A IN BANK50AI                         sbank50p
036700        MOVE SPACES TO FROM1O IN BANK50AO                         sbank50p
036800        MOVE SPACES TO TO1O IN BANK50AO                           sbank50p
036900        MOVE SPACES TO ACC1O IN BANK50AO                          sbank50p
037000        MOVE SPACES TO DSC1O IN BANK50AO                          sbank50p
037100        MOVE SPACES TO BAL1O IN BANK50AO                          sbank50p
037200     END-IF.                                                      sbank50p
037300     IF BANK-SCR50-ACC2 IS NOT EQUAL TO SPACES                    sbank50p
037400        MOVE -1 TO FROM2L IN BANK50AI                             sbank50p
037500        MOVE -1 TO TO2L IN BANK50AI                               sbank50p
037600        MOVE BANK-SCR50-FRM2 TO FROM2O IN BANK50AO                sbank50p
037700        MOVE BANK-SCR50-TO2  TO TO2O  IN BANK50AO                 sbank50p
037800        MOVE BANK-SCR50-ACC2 TO ACC2O IN BANK50AO                 sbank50p
037900        MOVE BANK-SCR50-DSC2 TO DSC2O IN BANK50AO                 sbank50p
038000        MOVE BANK-SCR50-BAL2 TO BAL2O IN BANK50AO                 sbank50p
038100     ELSE                                                         sbank50p
038200        MOVE DFHBMASK TO FROM2A IN BANK50AI                       sbank50p
038300        MOVE DFHBMASK TO TO2A IN BANK50AI                         sbank50p
038400        MOVE SPACE TO FROM2O IN BANK50AO                          sbank50p
038500        MOVE SPACE TO TO2O IN BANK50AO                            sbank50p
038600        MOVE SPACES TO ACC2O IN BANK50AO                          sbank50p
038700        MOVE SPACES TO DSC2O IN BANK50AO                          sbank50p
038800        MOVE SPACES TO BAL2O IN BANK50AO                          sbank50p
038900     END-IF.                                                      sbank50p
039000     IF BANK-SCR50-ACC3 IS NOT EQUAL TO SPACES                    sbank50p
039100        MOVE -1 TO FROM3L IN BANK50AI                             sbank50p
039200        MOVE -1 TO TO3L IN BANK50AI                               sbank50p
039300        MOVE BANK-SCR50-FRM3 TO FROM3O IN BANK50AO                sbank50p
039400        MOVE BANK-SCR50-TO3  TO TO3O  IN BANK50AO                 sbank50p
039500        MOVE BANK-SCR50-ACC3 TO ACC3O IN BANK50AO                 sbank50p
039600        MOVE BANK-SCR50-DSC3 TO DSC3O IN BANK50AO                 sbank50p
039700        MOVE BANK-SCR50-BAL3 TO BAL3O IN BANK50AO                 sbank50p
039800     ELSE                                                         sbank50p
039900        MOVE DFHBMASK TO FROM3A IN BANK50AI                       sbank50p
040000        MOVE DFHBMASK TO TO3A IN BANK50AI                         sbank50p
040100        MOVE SPACE TO FROM3O IN BANK50AO                          sbank50p
040200        MOVE SPACE TO TO3O IN BANK50AO                            sbank50p
040300        MOVE SPACES TO ACC3O IN BANK50AO                          sbank50p
040400        MOVE SPACES TO DSC3O IN BANK50AO                          sbank50p
040500        MOVE SPACES TO BAL3O IN BANK50AO                          sbank50p
040600     END-IF.                                                      sbank50p
040700     IF BANK-SCR50-ACC4 IS NOT EQUAL TO SPACES                    sbank50p
040800        MOVE -1 TO FROM4L IN BANK50AI                             sbank50p
040900        MOVE -1 TO TO4L IN BANK50AI                               sbank50p
041000        MOVE BANK-SCR50-FRM4 TO FROM4O IN BANK50AO                sbank50p
041100        MOVE BANK-SCR50-TO4  TO TO4O  IN BANK50AO                 sbank50p
041200        MOVE BANK-SCR50-ACC4 TO ACC4O IN BANK50AO                 sbank50p
041300        MOVE BANK-SCR50-DSC4 TO DSC4O IN BANK50AO                 sbank50p
041400        MOVE BANK-SCR50-BAL4 TO BAL4O IN BANK50AO                 sbank50p
041500     ELSE                                                         sbank50p
041600        MOVE DFHBMASK TO FROM4A IN BANK50AI                       sbank50p
041700        MOVE DFHBMASK TO TO4A IN BANK50AI                         sbank50p
041800        MOVE SPACE TO FROM4O IN BANK50AO                          sbank50p
041900        MOVE SPACE TO TO4O IN BANK50AO                            sbank50p
042000        MOVE SPACES TO ACC4O IN BANK50AO                          sbank50p
042100        MOVE SPACES TO DSC4O IN BANK50AO                          sbank50p
042200        MOVE SPACES TO BAL4O IN BANK50AO                          sbank50p
042300     END-IF.                                                      sbank50p
042400     IF BANK-SCR50-ACC5 IS NOT EQUAL TO SPACES                    sbank50p
042500        MOVE -1 TO FROM5L IN BANK50AI                             sbank50p
042600        MOVE -1 TO TO5L IN BANK50AI                               sbank50p
042700        MOVE BANK-SCR50-FRM5 TO FROM5O IN BANK50AO                sbank50p
042800        MOVE BANK-SCR50-TO5  TO TO5O  IN BANK50AO                 sbank50p
042900        MOVE BANK-SCR50-ACC5 TO ACC5O IN BANK50AO                 sbank50p
043000        MOVE BANK-SCR50-DSC5 TO DSC5O IN BANK50AO                 sbank50p
043100        MOVE BANK-SCR50-BAL5 TO BAL5O IN BANK50AO                 sbank50p
043200     ELSE                                                         sbank50p
043300        MOVE DFHBMASK TO FROM5A IN BANK50AI                       sbank50p
043400        MOVE DFHBMASK TO TO5A IN BANK50AI                         sbank50p
043500        MOVE SPACE TO FROM5O IN BANK50AO                          sbank50p
043600        MOVE SPACE TO TO5O IN BANK50AO                            sbank50p
043700        MOVE SPACES TO ACC5O IN BANK50AO                          sbank50p
043800        MOVE SPACES TO DSC5O IN BANK50AO                          sbank50p
043900        MOVE SPACES TO BAL5O IN BANK50AO                          sbank50p
044000     END-IF.                                                      sbank50p
044100     IF BANK-SCR50-ACC6 IS NOT EQUAL TO SPACES                    sbank50p
044200        MOVE -1 TO FROM6L IN BANK50AI                             sbank50p
044300        MOVE -1 TO TO6L IN BANK50AI                               sbank50p
044400        MOVE BANK-SCR50-FRM6 TO FROM6O IN BANK50AO                sbank50p
044500        MOVE BANK-SCR50-TO6  TO TO6O  IN BANK50AO                 sbank50p
044600        MOVE BANK-SCR50-ACC6 TO ACC6O IN BANK50AO                 sbank50p
044700        MOVE BANK-SCR50-DSC6 TO DSC6O IN BANK50AO                 sbank50p
044800        MOVE BANK-SCR50-BAL6 TO BAL6O IN BANK50AO                 sbank50p
044900     ELSE                                                         sbank50p
045000        MOVE DFHBMASK TO FROM6A IN BANK50AI                       sbank50p
045100        MOVE DFHBMASK TO TO6A IN BANK50AI                         sbank50p
045200        MOVE SPACE TO FROM6O IN BANK50AO                          sbank50p
045300        MOVE SPACE TO TO6O IN BANK50AO                            sbank50p
045400        MOVE SPACES TO ACC6O IN BANK50AO                          sbank50p
045500        MOVE SPACES TO DSC6O IN BANK50AO                          sbank50p
045600        MOVE SPACES TO BAL6O IN BANK50AO                          sbank50p
045700     END-IF.                                                      sbank50p
045800* Turn colour off if required                                     sbank50p
045900     IF COLOUR-OFF                                                sbank50p
046000        MOVE DFHGREEN TO TXT01C IN BANK50AO                       sbank50p
046100        MOVE DFHGREEN TO SCRNC IN BANK50AO                        sbank50p
046200        MOVE DFHGREEN TO HEAD1C IN BANK50AO                       sbank50p
046300        MOVE DFHGREEN TO DATEC IN BANK50AO                        sbank50p
046400        MOVE DFHGREEN TO TXT02C IN BANK50AO                       sbank50p
046500        MOVE DFHGREEN TO TRANC IN BANK50AO                        sbank50p
046600        MOVE DFHGREEN TO HEAD2C IN BANK50AO                       sbank50p
046700        MOVE DFHGREEN TO TIMEC IN BANK50AO                        sbank50p
046800        MOVE DFHGREEN TO TXT03C IN BANK50AO                       sbank50p
046900        MOVE DFHGREEN TO USERIDC IN BANK50AO                      sbank50p
047000        MOVE DFHGREEN TO TXT04C IN BANK50AO                       sbank50p
047100        MOVE DFHGREEN TO USERNMC IN BANK50AO                      sbank50p
047200        MOVE DFHGREEN TO TXT05C IN BANK50AO                       sbank50p
047300        MOVE DFHGREEN TO TXT06C IN BANK50AO                       sbank50p
047400        MOVE DFHGREEN TO XFERAMTC IN BANK50AO                     sbank50p
047500        MOVE DFHGREEN TO TXT07C IN BANK50AO                       sbank50p
047600        MOVE DFHGREEN TO TXT08C IN BANK50AO                       sbank50p
047700        MOVE DFHGREEN TO TXT09C IN BANK50AO                       sbank50p
047800        MOVE DFHGREEN TO TXT10C IN BANK50AO                       sbank50p
047900        MOVE DFHGREEN TO FROM1C IN BANK50AO                       sbank50p
048000        MOVE DFHGREEN TO TO1C IN BANK50AO                         sbank50p
048100        MOVE DFHGREEN TO ACC1C IN BANK50AO                        sbank50p
048200        MOVE DFHGREEN TO DSC1C IN BANK50AO                        sbank50p
048300        MOVE DFHGREEN TO BAL1C IN BANK50AO                        sbank50p
048400        MOVE DFHGREEN TO FROM2C IN BANK50AO                       sbank50p
048500        MOVE DFHGREEN TO TO2C IN BANK50AO                         sbank50p
048600        MOVE DFHGREEN TO ACC2C IN BANK50AO                        sbank50p
048700        MOVE DFHGREEN TO DSC2C IN BANK50AO                        sbank50p
048800        MOVE DFHGREEN TO BAL2C IN BANK50AO                        sbank50p
048900        MOVE DFHGREEN TO FROM3C IN BANK50AO                       sbank50p
049000        MOVE DFHGREEN TO TO3C IN BANK50AO                         sbank50p
049100        MOVE DFHGREEN TO ACC3C IN BANK50AO                        sbank50p
049200        MOVE DFHGREEN TO DSC3C IN BANK50AO                        sbank50p
049300        MOVE DFHGREEN TO BAL3C IN BANK50AO                        sbank50p
049400        MOVE DFHGREEN TO FROM4C IN BANK50AO                       sbank50p
049500        MOVE DFHGREEN TO TO4C IN BANK50AO                         sbank50p
049600        MOVE DFHGREEN TO ACC4C IN BANK50AO                        sbank50p
049700        MOVE DFHGREEN TO DSC4C IN BANK50AO                        sbank50p
049800        MOVE DFHGREEN TO BAL4C IN BANK50AO                        sbank50p
049900        MOVE DFHGREEN TO FROM5C IN BANK50AO                       sbank50p
050000        MOVE DFHGREEN TO TO5C IN BANK50AO                         sbank50p
050100        MOVE DFHGREEN TO ACC5C IN BANK50AO                        sbank50p
050200        MOVE DFHGREEN TO DSC5C IN BANK50AO                        sbank50p
050300        MOVE DFHGREEN TO BAL5C IN BANK50AO                        sbank50p
050400        MOVE DFHGREEN TO FROM6C IN BANK50AO                       sbank50p
050500        MOVE DFHGREEN TO TO6C IN BANK50AO                         sbank50p
050600        MOVE DFHGREEN TO ACC6C IN BANK50AO                        sbank50p
050700        MOVE DFHGREEN TO DSC6C IN BANK50AO                        sbank50p
050800        MOVE DFHGREEN TO BAL6C IN BANK50AO                        sbank50p
050900        MOVE DFHGREEN TO ERRMSGC IN BANK50AO                      sbank50p
051000        MOVE DFHGREEN TO TXT11C IN BANK50AO                       sbank50p
051100        MOVE DFHGREEN TO VERC IN BANK50AO                         sbank50p
051200     END-IF.                                                      sbank50p
051300                                                                  sbank50p
051400     EXEC CICS SEND MAP('BANK50A')                                sbank50p
051500                    MAPSET('MBANK50')                             sbank50p
051600                    ERASE                                         sbank50p
051700                    FREEKB                                        sbank50p
051800     END-EXEC.                                                    sbank50p
051900     GO TO SCREEN50-BUILD-AND-SEND-EXIT.                          sbank50p
052000                                                                  sbank50p
052100 HELP50-BUILD-AND-SEND-CICS.                                      sbank50p
052200     COPY CSCRNHP2 REPLACING ==:OPTN:== BY ==BANK==               sbank50p
052300                             ==<<SCRN>>== BY ==HELP50AO==.        sbank50p
052400                                                                  sbank50p
052500     EXEC CICS SEND MAP('HELP50A')                                sbank50p
052600                    MAPSET('MBANK50')                             sbank50p
052700                    ERASE                                         sbank50p
052800                    FREEKB                                        sbank50p
052900     END-EXEC.                                                    sbank50p
053000     GO TO SCREEN50-BUILD-AND-SEND-EXIT.                          sbank50p
053100                                                                  sbank50p
053200 SCREEN50-BUILD-AND-SEND-INET.                                    sbank50p
053300     MOVE SPACES TO EXT-OP-DATA.                                  sbank50p
053400     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              sbank50p
053500     MOVE DDO-DATA TO EXT-OP-DATE.                                sbank50p
053600     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          sbank50p
053700     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         sbank50p
053800     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          sbank50p
053900     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          sbank50p
054000     CALL 'SVERSONP' USING VERSION.                               sbank50p
054100     MOVE VERSION TO EXT-OP-VERSION.                              sbank50p
054200* Move in screen name                                             sbank50p
054300     MOVE 'BANK50' TO EXT-OP-SCREEN.                              sbank50p
054400* Move in any error message                                       sbank50p
054500     MOVE BANK-ERROR-MSG TO EXT-OP-ERR-MSG.                       sbank50p
054600     MOVE BANK-USERID TO EXT-OP-USERID.                           sbank50p
054700     MOVE BANK-USERID-NAME TO EXT-OP-NAME.                        sbank50p
054800* Move in screen specific fields                                  sbank50p
054900     MOVE BANK-SCR50-XFER TO EXT-OP50-XFER.                       sbank50p
055000     MOVE BANK-SCR50-FRM1 TO EXT-OP50-FRM1.                       sbank50p
055100     MOVE BANK-SCR50-TO1  TO EXT-OP50-TO1.                        sbank50p
055200     MOVE BANK-SCR50-ACC1 TO EXT-OP50-ACC1.                       sbank50p
055300     MOVE BANK-SCR50-DSC1 TO EXT-OP50-DSC1.                       sbank50p
055400     MOVE BANK-SCR50-BAL1 TO EXT-OP50-BAL1.                       sbank50p
055500     MOVE BANK-SCR50-FRM2 TO EXT-OP50-FRM2.                       sbank50p
055600     MOVE BANK-SCR50-TO2  TO EXT-OP50-TO2.                        sbank50p
055700     MOVE BANK-SCR50-ACC2 TO EXT-OP50-ACC2.                       sbank50p
055800     MOVE BANK-SCR50-DSC2 TO EXT-OP50-DSC2.                       sbank50p
055900     MOVE BANK-SCR50-BAL2 TO EXT-OP50-BAL2.                       sbank50p
056000     MOVE BANK-SCR50-FRM3 TO EXT-OP50-FRM3.                       sbank50p
056100     MOVE BANK-SCR50-TO3  TO EXT-OP50-TO3.                        sbank50p
056200     MOVE BANK-SCR50-ACC3 TO EXT-OP50-ACC3.                       sbank50p
056300     MOVE BANK-SCR50-DSC3 TO EXT-OP50-DSC3.                       sbank50p
056400     MOVE BANK-SCR50-BAL3 TO EXT-OP50-BAL3.                       sbank50p
056500     MOVE BANK-SCR50-FRM4 TO EXT-OP50-FRM4.                       sbank50p
056600     MOVE BANK-SCR50-TO4  TO EXT-OP50-TO4.                        sbank50p
056700     MOVE BANK-SCR50-ACC4 TO EXT-OP50-ACC4.                       sbank50p
056800     MOVE BANK-SCR50-DSC4 TO EXT-OP50-DSC4.                       sbank50p
056900     MOVE BANK-SCR50-BAL4 TO EXT-OP50-BAL4.                       sbank50p
057000     MOVE BANK-SCR50-FRM5 TO EXT-OP50-FRM5.                       sbank50p
057100     MOVE BANK-SCR50-TO5  TO EXT-OP50-TO5.                        sbank50p
057200     MOVE BANK-SCR50-ACC5 TO EXT-OP50-ACC5.                       sbank50p
057300     MOVE BANK-SCR50-DSC5 TO EXT-OP50-DSC5.                       sbank50p
057400     MOVE BANK-SCR50-BAL5 TO EXT-OP50-BAL5.                       sbank50p
057500     MOVE BANK-SCR50-FRM6 TO EXT-OP50-FRM6.                       sbank50p
057600     MOVE BANK-SCR50-TO6  TO EXT-OP50-TO6.                        sbank50p
057700     MOVE BANK-SCR50-ACC6 TO EXT-OP50-ACC6.                       sbank50p
057800     MOVE BANK-SCR50-DSC6 TO EXT-OP50-DSC6.                       sbank50p
057900     MOVE BANK-SCR50-BAL6 TO EXT-OP50-BAL6.                       sbank50p
058000                                                                  sbank50p
058100 SCREEN50-BUILD-AND-SEND-EXIT.                                    sbank50p
058200     EXIT.                                                        sbank50p
058300                                                                  sbank50p
058400***************************************************************** sbank50p
058500* Call common routine to perform date conversions               * sbank50p
058600***************************************************************** sbank50p
058700 CALL-DATECONV.                                                   sbank50p
058800     MOVE BANK-ENV TO DD-ENV.                                     sbank50p
058900     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           sbank50p
059000     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            sbank50p
059100 CALL-DATECONV-EXIT.                                              sbank50p
059200     EXIT.                                                        sbank50p
059300                                                                  sbank50p
059400* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     sbank50p
