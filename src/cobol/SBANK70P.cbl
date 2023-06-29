000100***************************************************************** sbank70p
000200*                                                               * sbank70p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * sbank70p
000400*   This demonstration program is provided for use by users     * sbank70p
000500*   of Micro Focus products and may be used, modified and       * sbank70p
000600*   distributed as part of your application provided that       * sbank70p
000700*   you properly acknowledge the copyright of Micro Focus       * sbank70p
000800*   in this material.                                           * sbank70p
000900*                                                               * sbank70p
001000***************************************************************** sbank70p
001100                                                                  sbank70p
001200***************************************************************** sbank70p
001300* Program:     SBANK70P.CBL (CICS Version)                      * sbank70p
001400* Layer:       Screen handling                                  * sbank70p
001500* Function:    Calculate cost of loan                           * sbank70p
001600***************************************************************** sbank70p
001700                                                                  sbank70p
001800 IDENTIFICATION DIVISION.                                         sbank70p
001900 PROGRAM-ID.                                                      sbank70p
002000     SBANK70P.                                                    sbank70p
002100 DATE-WRITTEN.                                                    sbank70p
002200     September 2002.                                              sbank70p
002300 DATE-COMPILED.                                                   sbank70p
002400     Today.                                                       sbank70p
002500                                                                  sbank70p
002600 ENVIRONMENT DIVISION.                                            sbank70p
002700                                                                  sbank70p
002800 DATA DIVISION.                                                   sbank70p
002900 WORKING-STORAGE SECTION.                                         sbank70p
003000 01  WS-MISC-STORAGE.                                             sbank70p
003100   05  WS-PROGRAM-ID                         PIC X(8)             sbank70p
003200       VALUE 'SBANK70P'.                                          sbank70p
003300   05  WS-TRAN-ID                            PIC X(4).            sbank70p
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             sbank70p
003500       VALUE SPACES.                                              sbank70p
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             sbank70p
003700       VALUE 'UNKNOWN'.                                           sbank70p
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      sbank70p
003900   05  WS-WORK1                              PIC X(1).            sbank70p
004000   05  WS-SUB1                               PIC S9(4) COMP.      sbank70p
         05  WS-VERSION                            PIC X(7).
                                                                        sbank70p
004100                                                                  
004200 01  MAPAREA                                 PIC X(2048).         sbank70p
004300 COPY MBANK70.                                                    sbank70p
004400                                                                  sbank70p
004500 01  WS-TIME-DATE-WORK-AREA.                                      sbank70p
004600 COPY CDATED.                                                     sbank70p
004700                                                                  sbank70p
004800 01  WS-BANK-DATA-AREAS.                                          sbank70p
004900   05  WS-BANK-DATA.                                              sbank70p
005000 COPY CBANKDAT.                                                   sbank70p
005100   05  WS-BANK-EXT-DATA.                                          sbank70p
005200 COPY CBANKEXT.                                                   sbank70p
005300                                                                  sbank70p
005400 COPY CSCRNHDD.                                                   sbank70p
005500                                                                  sbank70p
005600 COPY CVERSND.                                                    sbank70p
005700                                                                  sbank70p
005800 COPY DFHAID.                                                     sbank70p
005900                                                                  sbank70p
006000 COPY DFHBMSCA.                                                   sbank70p
006100                                                                  sbank70p
006200 COPY CABENDD.                                                    sbank70p
006300                                                                  sbank70p
006400 LINKAGE SECTION.                                                 sbank70p
006500 01  DFHCOMMAREA.                                                 sbank70p
006600   05  FILLER                                PIC X(1)             sbank70p
006700       OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.             sbank70p
006800                                                                  sbank70p
006900 PROCEDURE DIVISION.                                              sbank70p
007000***************************************************************** sbank70p
007100* Write entry to log to show we have been invoked               * sbank70p
007200***************************************************************** sbank70p
007300     COPY CTRACE.                                                 sbank70p
007400                                                                  sbank70p
007500***************************************************************** sbank70p
007600* Store our transaction-id                                      * sbank70p
007700***************************************************************** sbank70p
007800     MOVE EIBTRNID TO WS-TRAN-ID.                                 sbank70p
007900                                                                  sbank70p
008000***************************************************************** sbank70p
008100* Store passed data or abend if there wasn't any                * sbank70p
008200***************************************************************** sbank70p
008300     IF EIBCALEN IS EQUAL TO 0                                    sbank70p
008400        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       sbank70p
008500        MOVE '0001' TO ABEND-CODE                                 sbank70p
008600        MOVE SPACES TO ABEND-REASON                               sbank70p
008700        COPY CABENDPO.                                            sbank70p
008800     ELSE                                                         sbank70p
008900        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        sbank70p
009000        MOVE LOW-VALUES TO WS-BANK-DATA                           sbank70p
009100        MOVE DFHCOMMAREA (1:EIBCALEN)                             sbank70p
009200          TO WS-BANK-DATA-AREAS (1:LENGTH OF WS-BANK-DATA-AREAS)  sbank70p
009300     END-IF.                                                      sbank70p
009400                                                                  sbank70p
009500***************************************************************** sbank70p
009600* This is the main process                                      * sbank70p
009700***************************************************************** sbank70p
009800                                                                  sbank70p
009900***************************************************************** sbank70p
010000* Determine what we have to do (read from or send to screen)    * sbank70p
010100***************************************************************** sbank70p
010200     MOVE LOW-VALUE TO MAPAREA.                                   sbank70p
010300     EVALUATE TRUE                                                sbank70p
010400       WHEN BANK-MAP-FUNCTION-GET                                 sbank70p
010500         PERFORM SCREEN70-READ THRU                               sbank70p
010600                 SCREEN70-READ-EXIT                               sbank70p
010700       WHEN BANK-MAP-FUNCTION-PUT                                 sbank70p
010800         PERFORM SCREEN70-BUILD-AND-SEND THRU                     sbank70p
010900                 SCREEN70-BUILD-AND-SEND-EXIT                     sbank70p
011000       WHEN OTHER                                                 sbank70p
011100         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      sbank70p
011200         MOVE '0002' TO ABEND-CODE                                sbank70p
011300         MOVE SPACES TO ABEND-REASON                              sbank70p
011400         COPY CABENDPO.                                           sbank70p
011500     END-EVALUATE.                                                sbank70p
011600                                                                  sbank70p
011700* Call the appropriate routine to handle the business logic       sbank70p
011800     IF BANK-MAP-FUNCTION-GET                                     sbank70p
011900        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             sbank70p
012000                       COMMAREA(WS-BANK-DATA)                     sbank70p
012100                       LENGTH(LENGTH OF WS-BANK-DATA)             sbank70p
012200        END-EXEC                                                  sbank70p
012300     END-IF.                                                      sbank70p
012400                                                                  sbank70p
012500***************************************************************** sbank70p
012600* Now we have to have finished and can return to our invoker.   * sbank70p
012700***************************************************************** sbank70p
012800* Now return to CICS                                              sbank70p
012900     MOVE WS-BANK-DATA-AREAS (1:LENGTH OF WS-BANK-DATA-AREAS)     sbank70p
013000       TO DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      sbank70p
013100     EXEC CICS                                                    sbank70p
013200          RETURN                                                  sbank70p
013300     END-EXEC.                                                    sbank70p
013400     GOBACK.                                                      sbank70p
013500                                                                  sbank70p
013600***************************************************************** sbank70p
013700* Screen processing for MBANK70                                 * sbank70p
013800*---------------------------------------------------------------* sbank70p
013900* Retrieve data from screen and format it                       * sbank70p
014000***************************************************************** sbank70p
014100 SCREEN70-READ.                                                   sbank70p
014200     MOVE 'BBANK70P' TO WS-BUSINESS-LOGIC-PGM.                    sbank70p
014300     IF BANK-AID-CLEAR                                            sbank70p
014400        SET BANK-AID-PFK03 TO TRUE                                sbank70p
014500        GO TO SCREEN70-READ-EXIT                                  sbank70p
014600     END-IF.                                                      sbank70p
014700     IF BANK-ENV-CICS                                             sbank70p
014800        GO TO SCREEN70-READ-CICS                                  sbank70p
014900     ELSE                                                         sbank70p
015000        GO TO SCREEN70-READ-INET                                  sbank70p
015100     END-IF.                                                      sbank70p
015200                                                                  sbank70p
015300 SCREEN70-READ-CICS.                                              sbank70p
015400     IF BANK-HELP-INACTIVE                                        sbank70p
015500        EXEC CICS RECEIVE MAP('BANK70A')                          sbank70p
015600                          MAPSET('MBANK70')                       sbank70p
015700        END-EXEC                                                  sbank70p
015800     ELSE                                                         sbank70p
015900        EXEC CICS RECEIVE MAP('HELP70A')                          sbank70p
016000                          MAPSET('MBANK70')                       sbank70p
016100        END-EXEC                                                  sbank70p
016200        GO TO SCREEN70-READ-EXIT                                  sbank70p
016300     END-IF.                                                      sbank70p
016400                                                                  sbank70p
016500     IF AMOUNTL IN BANK70AI IS EQUAL TO 0                         sbank70p
016600           MOVE LOW-VALUES TO BANK-SCR70-AMOUNT                   sbank70p
016700     ELSE                                                         sbank70p
016800        MOVE AMOUNTI IN BANK70AI TO BANK-SCR70-AMOUNT             sbank70p
016900        IF BANK-SCR70-AMOUNT IS EQUAL TO SPACES                   sbank70p
017000           MOVE LOW-VALUES TO BANK-SCR70-AMOUNT                   sbank70p
017100     END-IF.                                                      sbank70p
017200                                                                  sbank70p
017300     IF RATEL IN BANK70AI IS EQUAL TO 0                           sbank70p
017400           MOVE LOW-VALUES TO BANK-SCR70-RATE                     sbank70p
017500     ELSE                                                         sbank70p
017600        MOVE RATEI IN BANK70AI TO BANK-SCR70-RATE                 sbank70p
017700        IF BANK-SCR70-RATE IS EQUAL TO SPACES                     sbank70p
017800           MOVE LOW-VALUES TO BANK-SCR70-RATE                     sbank70p
017900     END-IF.                                                      sbank70p
018000                                                                  sbank70p
018100     IF TERML IN BANK70AI IS EQUAL TO 0                           sbank70p
018200           MOVE LOW-VALUES TO BANK-SCR70-TERM                     sbank70p
018300     ELSE                                                         sbank70p
018400        MOVE TERMI IN BANK70AI TO BANK-SCR70-TERM                 sbank70p
018500        IF BANK-SCR70-TERM IS EQUAL TO SPACES                     sbank70p
018600           MOVE LOW-VALUES TO BANK-SCR70-TERM                     sbank70p
018700     END-IF.                                                      sbank70p
018800                                                                  sbank70p
018900     GO TO SCREEN70-READ-EXIT.                                    sbank70p
019000                                                                  sbank70p
019100 SCREEN70-READ-INET.                                              sbank70p
019200     MOVE EXT-IP70-AMOUNT TO BANK-SCR70-AMOUNT.                   sbank70p
019300     MOVE EXT-IP70-RATE TO BANK-SCR70-RATE.                       sbank70p
019400     MOVE EXT-IP70-TERM TO BANK-SCR70-TERM.                       sbank70p
019500     GO TO SCREEN70-READ-EXIT.                                    sbank70p
019600                                                                  sbank70p
019700 SCREEN70-READ-EXIT.                                              sbank70p
019800     EXIT.                                                        sbank70p
019900                                                                  sbank70p
020000***************************************************************** sbank70p
020100* Screen processing for SCREEN70 (BANK70/HELP70)                * sbank70p
020200*---------------------------------------------------------------* sbank70p
020300* Build the output screen and send it                           * sbank70p
020400***************************************************************** sbank70p
020500 SCREEN70-BUILD-AND-SEND.                                         sbank70p
020600* Clear map area, get date & time and move to the map             sbank70p
020700     MOVE LOW-VALUES TO BANK70AO.                                 sbank70p
020800     MOVE EIBTIME TO DD-TIME-INPUT-N.                             sbank70p
020900     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      sbank70p
021000     SET DDI-YYDDD TO TRUE.                                       sbank70p
021100     SET DDO-DD-MMM-YYYY TO TRUE.                                 sbank70p
021200     PERFORM CALL-DATECONV THRU                                   sbank70p
021300             CALL-DATECONV-EXIT.                                  sbank70p
021400* Ensure the last map fields are correct                          sbank70p
021500     IF BANK-HELP-ACTIVE                                          sbank70p
021600        MOVE 'MBANK70' TO BANK-LAST-MAPSET                        sbank70p
021700        MOVE 'HELP70A' TO BANK-LAST-MAP                           sbank70p
021800     ELSE                                                         sbank70p
021900        MOVE 'MBANK70' TO BANK-LAST-MAPSET                        sbank70p
022000        MOVE 'BANK70A' TO BANK-LAST-MAP                           sbank70p
022100     END-IF.                                                      sbank70p
022200     IF BANK-ENV-CICS                                             sbank70p
022300        GO TO SCREEN70-BUILD-AND-SEND-CICS                        sbank70p
022400     ELSE                                                         sbank70p
022500        GO TO SCREEN70-BUILD-AND-SEND-INET                        sbank70p
022600     END-IF.                                                      sbank70p
022700                                                                  sbank70p
022800 SCREEN70-BUILD-AND-SEND-CICS.                                    sbank70p
022900     IF BANK-LAST-MAP IS EQUAL TO 'BANK70A'                       sbank70p
023000        GO TO BANK70-BUILD-AND-SEND-CICS                          sbank70p
023100     END-IF.                                                      sbank70p
023200     IF BANK-LAST-MAP IS EQUAL TO 'HELP70A'                       sbank70p
023300        GO TO HELP70-BUILD-AND-SEND-CICS                          sbank70p
023400     END-IF.                                                      sbank70p
023500     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                          sbank70p
023600     MOVE '0003' TO ABEND-CODE                                    sbank70p
023700     MOVE SPACES TO ABEND-REASON                                  sbank70p
023800     COPY CABENDPO.                                               sbank70p
023900     GOBACK.                                                      sbank70p
024000                                                                  sbank70p
024100 BANK70-BUILD-AND-SEND-CICS.                                      sbank70p
024200     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANK70AO==.        sbank70p
024300     COPY CVERSNP1 REPLACING ==<<SCRN>>== BY ==BANK70AO==.        sbank70p
024400     MOVE WS-TRAN-ID TO TRANO IN BANK70AO.                        sbank70p
024500     MOVE DD-TIME-OUTPUT TO TIMEO IN BANK70AO.                    sbank70p
024600     MOVE DDO-DATA TO DATEO IN BANK70AO.                          sbank70p
024700* Move in any error message                                       sbank70p
024800     MOVE BANK-ERROR-MSG TO ERRMSGO IN BANK70AO.                  sbank70p
024900* Move in screen specific fields                                  sbank70p
025000     MOVE BANK-SCR70-AMOUNT TO AMOUNTO IN BANK70AO.               sbank70p
025100     MOVE BANK-SCR70-RATE TO RATEO IN BANK70AO.                   sbank70p
025200     MOVE BANK-SCR70-TERM TO TERMO IN BANK70AO.                   sbank70p
025300     MOVE BANK-SCR70-PAYMENT TO PAYMENTO IN BANK70AO.             sbank70p
025400* Turn colour off if required                                     sbank70p
025500     IF COLOUR-OFF                                                sbank70p
025600        MOVE DFHGREEN TO TXT01C IN BANK70AO                       sbank70p
025700        MOVE DFHGREEN TO SCRNC IN BANK70AO                        sbank70p
025800        MOVE DFHGREEN TO HEAD1C IN BANK70AO                       sbank70p
025900        MOVE DFHGREEN TO DATEC IN BANK70AO                        sbank70p
026000        MOVE DFHGREEN TO TXT02C IN BANK70AO                       sbank70p
026100        MOVE DFHGREEN TO TRANC IN BANK70AO                        sbank70p
026200        MOVE DFHGREEN TO HEAD2C IN BANK70AO                       sbank70p
026300        MOVE DFHGREEN TO TIMEC IN BANK70AO                        sbank70p
026400        MOVE DFHGREEN TO TXT03C IN BANK70AO                       sbank70p
026500        MOVE DFHGREEN TO TXT04C IN BANK70AO                       sbank70p
026600        MOVE DFHGREEN TO TXT05C IN BANK70AO                       sbank70p
026700        MOVE DFHGREEN TO AMOUNTC IN BANK70AO                      sbank70p
026800        MOVE DFHGREEN TO TXT06C IN BANK70AO                       sbank70p
026900        MOVE DFHGREEN TO RATEC IN BANK70AO                        sbank70p
027000        MOVE DFHGREEN TO TXT07C IN BANK70AO                       sbank70p
027100        MOVE DFHGREEN TO TERMC IN BANK70AO                        sbank70p
027200        MOVE DFHGREEN TO TXT08C IN BANK70AO                       sbank70p
027300        MOVE DFHGREEN TO PAYMENTC IN BANK70AO                     sbank70p
027400        MOVE DFHGREEN TO ERRMSGC IN BANK70AO                      sbank70p
027500        MOVE DFHGREEN TO TXT10C IN BANK70AO                       sbank70p
027600        MOVE DFHGREEN TO VERC IN BANK70AO                         sbank70p
027700     END-IF.                                                      sbank70p
027800* Hide line if no payment                                         sbank70p
027900     IF BANK-SCR70-PAYMENT IS EQUAL TO SPACES                     sbank70p
028000        MOVE SPACES TO TXT08O IN BANK70AO                         sbank70p
028100        MOVE SPACES TO PAYMENTO IN BANK70AO                       sbank70p
028200     END-IF.                                                      sbank70p
028300     EXEC CICS SEND MAP('BANK70A')                                sbank70p
028400                    MAPSET('MBANK70')                             sbank70p
028500                    ERASE                                         sbank70p
028600                    FREEKB                                        sbank70p
028700     END-EXEC.                                                    sbank70p
028800                                                                  sbank70p
028900     GO TO SCREEN70-BUILD-AND-SEND-EXIT.                          sbank70p
029000                                                                  sbank70p
029100 HELP70-BUILD-AND-SEND-CICS.                                      sbank70p
029200     COPY CSCRNHP2 REPLACING ==:OPTN:== BY ==BANK==               sbank70p
029300                             ==<<SCRN>>== BY ==HELP70AO==.        sbank70p
029400                                                                  sbank70p
029500     EXEC CICS SEND MAP('HELP70A')                                sbank70p
029600                    MAPSET('MBANK70')                             sbank70p
029700                    ERASE                                         sbank70p
029800                    FREEKB                                        sbank70p
029900     END-EXEC.                                                    sbank70p
030000     GO TO SCREEN70-BUILD-AND-SEND-EXIT.                          sbank70p
030100                                                                  sbank70p
030200 SCREEN70-BUILD-AND-SEND-INET.                                    sbank70p
030300     MOVE SPACES TO EXT-OP-DATA.                                  sbank70p
030400     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              sbank70p
030500     MOVE DDO-DATA TO EXT-OP-DATE.                                sbank70p
030600     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          sbank70p
030700     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         sbank70p
030800     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          sbank70p
030900     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          sbank70p
031000     CALL 'SVERSONP' USING SCREEN-TITLES.                         sbank70p
031100     MOVE VERSION TO EXT-OP-VERSION.                              sbank70p
031200* Move in screen name                                             sbank70p
031300     MOVE 'BANK70' TO EXT-OP-SCREEN.                              sbank70p
031400* Move in any error message                                       sbank70p
031500     MOVE BANK-ERROR-MSG TO EXT-OP-ERR-MSG.                       sbank70p
031600     MOVE BANK-USERID TO EXT-OP-USERID.                           sbank70p
031700     MOVE BANK-USERID-NAME TO EXT-OP-NAME.                        sbank70p
031800* Move in screen specific fields                                  sbank70p
031900     MOVE BANK-SCR70-AMOUNT TO EXT-OP70-AMOUNT.                   sbank70p
032000     MOVE BANK-SCR70-RATE TO EXT-OP70-RATE.                       sbank70p
032100     MOVE BANK-SCR70-TERM TO EXT-OP70-TERM.                       sbank70p
032200     MOVE BANK-SCR70-PAYMENT TO EXT-OP70-PAYMENT.                 sbank70p
032300                                                                  sbank70p
032400 SCREEN70-BUILD-AND-SEND-EXIT.                                    sbank70p
032500     EXIT.                                                        sbank70p
032600                                                                  sbank70p
032700***************************************************************** sbank70p
032800* Call common routine to perform date conversions               * sbank70p
032900***************************************************************** sbank70p
033000 CALL-DATECONV.                                                   sbank70p
033100     MOVE BANK-ENV TO DD-ENV.                                     sbank70p
033200     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           sbank70p
033300     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            sbank70p
033400 CALL-DATECONV-EXIT.                                              sbank70p
033500     EXIT.                                                        sbank70p
033600                                                                  sbank70p
033700* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     sbank70p
