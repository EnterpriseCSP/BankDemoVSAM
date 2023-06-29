000100***************************************************************** sbank80p
000200*                                                               * sbank80p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * sbank80p
000400*   This demonstration program is provided for use by users     * sbank80p
000500*   of Micro Focus products and may be used, modified and       * sbank80p
000600*   distributed as part of your application provided that       * sbank80p
000700*   you properly acknowledge the copyright of Micro Focus       * sbank80p
000800*   in this material.                                           * sbank80p
000900*                                                               * sbank80p
001000***************************************************************** sbank80p
001100                                                                  sbank80p
001200***************************************************************** sbank80p
001300* Program:     SBANK80P.CBL (CICS Version)                      * sbank80p
001400* Layer:       Screen handling                                  * sbank80p
001500* Function:    Request printing of statements                   * sbank80p
001600***************************************************************** sbank80p
001700                                                                  sbank80p
001800 IDENTIFICATION DIVISION.                                         sbank80p
001900 PROGRAM-ID.                                                      sbank80p
002000     SBANK80P.                                                    sbank80p
002100 DATE-WRITTEN.                                                    sbank80p
002200     September 2002.                                              sbank80p
002300 DATE-COMPILED.                                                   sbank80p
002400     Today.                                                       sbank80p
002500                                                                  sbank80p
002600 ENVIRONMENT DIVISION.                                            sbank80p
002700                                                                  sbank80p
002800 DATA DIVISION.                                                   sbank80p
002900 WORKING-STORAGE SECTION.                                         sbank80p
003000 01  WS-MISC-STORAGE.                                             sbank80p
003100   05  WS-PROGRAM-ID                         PIC X(8)             sbank80p
003200       VALUE 'SBANK80P'.                                          sbank80p
003300   05  WS-TRAN-ID                            PIC X(4).            sbank80p
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             sbank80p
003500       VALUE SPACES.                                              sbank80p
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             sbank80p
003700       VALUE 'UNKNOWN'.                                           sbank80p
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      sbank80p
003900   05  WS-WORK1                              PIC X(1).            sbank80p
004000   05  WS-SUB1                               PIC S9(4) COMP.      sbank80p
         05  WS-VERSION                            PIC X(7).
                                                                        sbank80p
004100                                                                  
004200 01  MAPAREA                                 PIC X(2048).         sbank80p
004300 COPY MBANK80.                                                    sbank80p
004400                                                                  sbank80p
004500 01  WS-TIME-DATE-WORK-AREA.                                      sbank80p
004600 COPY CDATED.                                                     sbank80p
004700                                                                  sbank80p
004800 01  WS-BANK-DATA-AREAS.                                          sbank80p
004900   05  WS-BANK-DATA.                                              sbank80p
005000 COPY CBANKDAT.                                                   sbank80p
005100   05  WS-BANK-EXT-DATA.                                          sbank80p
005200 COPY CBANKEXT.                                                   sbank80p
005300                                                                  sbank80p
005400 COPY CSCRNHDD.                                                   sbank80p
005500                                                                  sbank80p
005600 COPY CVERSND.                                                    sbank80p
005700                                                                  sbank80p
005800 COPY DFHAID.                                                     sbank80p
005900                                                                  sbank80p
006000 COPY DFHBMSCA.                                                   sbank80p
006100                                                                  sbank80p
006200 COPY CABENDD.                                                    sbank80p
006300                                                                  sbank80p
006400 LINKAGE SECTION.                                                 sbank80p
006500 01  DFHCOMMAREA.                                                 sbank80p
006600   05  FILLER                                PIC X(1)             sbank80p
006700       OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.             sbank80p
006800                                                                  sbank80p
006900 PROCEDURE DIVISION.                                              sbank80p
007000***************************************************************** sbank80p
007100* Write entry to log to show we have been invoked               * sbank80p
007200***************************************************************** sbank80p
007300     COPY CTRACE.                                                 sbank80p
007400                                                                  sbank80p
007500***************************************************************** sbank80p
007600* Store our transaction-id                                      * sbank80p
007700***************************************************************** sbank80p
007800     MOVE EIBTRNID TO WS-TRAN-ID.                                 sbank80p
007900                                                                  sbank80p
008000***************************************************************** sbank80p
008100* Store passed data or abend if there wasn't any                * sbank80p
008200***************************************************************** sbank80p
008300     IF EIBCALEN IS EQUAL TO 0                                    sbank80p
008400        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       sbank80p
008500        MOVE '0001' TO ABEND-CODE                                 sbank80p
008600        MOVE SPACES TO ABEND-REASON                               sbank80p
008700        COPY CABENDPO.                                            sbank80p
008800     ELSE                                                         sbank80p
008900        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        sbank80p
009000        MOVE LOW-VALUES TO WS-BANK-DATA                           sbank80p
009100        MOVE DFHCOMMAREA (1:EIBCALEN)                             sbank80p
009200          TO WS-BANK-DATA-AREAS (1:LENGTH OF WS-BANK-DATA-AREAS)  sbank80p
009300     END-IF.                                                      sbank80p
009400                                                                  sbank80p
009500***************************************************************** sbank80p
009600* This is the main process                                      * sbank80p
009700***************************************************************** sbank80p
009800                                                                  sbank80p
009900***************************************************************** sbank80p
010000* Determine what we have to do (read from or send to screen)    * sbank80p
010100***************************************************************** sbank80p
010200     MOVE LOW-VALUE TO MAPAREA.                                   sbank80p
010300     EVALUATE TRUE                                                sbank80p
010400       WHEN BANK-MAP-FUNCTION-GET                                 sbank80p
010500         PERFORM SCREEN80-READ THRU                               sbank80p
010600                 SCREEN80-READ-EXIT                               sbank80p
010700       WHEN BANK-MAP-FUNCTION-PUT                                 sbank80p
010800         PERFORM SCREEN80-BUILD-AND-SEND THRU                     sbank80p
010900                 SCREEN80-BUILD-AND-SEND-EXIT                     sbank80p
011000       WHEN OTHER                                                 sbank80p
011100         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      sbank80p
011200         MOVE '0002' TO ABEND-CODE                                sbank80p
011300         MOVE SPACES TO ABEND-REASON                              sbank80p
011400         COPY CABENDPO.                                           sbank80p
011500     END-EVALUATE.                                                sbank80p
011600                                                                  sbank80p
011700* Call the appropriate routine to handle the business logic       sbank80p
011800     IF BANK-MAP-FUNCTION-GET                                     sbank80p
011900        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             sbank80p
012000                       COMMAREA(WS-BANK-DATA)                     sbank80p
012100                       LENGTH(LENGTH OF WS-BANK-DATA)             sbank80p
012200        END-EXEC                                                  sbank80p
012300     END-IF.                                                      sbank80p
012400                                                                  sbank80p
012500***************************************************************** sbank80p
012600* Now we have to have finished and can return to our invoker.   * sbank80p
012700***************************************************************** sbank80p
012800* Now return to CICS                                              sbank80p
012900     MOVE WS-BANK-DATA-AREAS (1:LENGTH OF WS-BANK-DATA-AREAS)     sbank80p
013000       TO DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      sbank80p
013100     EXEC CICS                                                    sbank80p
013200          RETURN                                                  sbank80p
013300     END-EXEC.                                                    sbank80p
013400     GOBACK.                                                      sbank80p
013500                                                                  sbank80p
013600***************************************************************** sbank80p
013700* Screen processing for MBANK80                                 * sbank80p
013800*---------------------------------------------------------------* sbank80p
013900* Retrieve data from screen and format it                       * sbank80p
014000***************************************************************** sbank80p
014100 SCREEN80-READ.                                                   sbank80p
014200     MOVE 'BBANK80P' TO WS-BUSINESS-LOGIC-PGM.                    sbank80p
014300     IF BANK-AID-CLEAR                                            sbank80p
014400        SET BANK-AID-PFK03 TO TRUE                                sbank80p
014500        GO TO SCREEN80-READ-EXIT                                  sbank80p
014600     END-IF.                                                      sbank80p
014700     IF BANK-ENV-CICS                                             sbank80p
014800        GO TO SCREEN80-READ-CICS                                  sbank80p
014900     ELSE                                                         sbank80p
015000        GO TO SCREEN80-READ-INET                                  sbank80p
015100     END-IF.                                                      sbank80p
015200                                                                  sbank80p
015300 SCREEN80-READ-CICS.                                              sbank80p
015400     IF BANK-HELP-INACTIVE                                        sbank80p
015500        EXEC CICS RECEIVE MAP('BANK80A')                          sbank80p
015600                          MAPSET('MBANK80')                       sbank80p
015700        END-EXEC                                                  sbank80p
015800     ELSE                                                         sbank80p
015900        EXEC CICS RECEIVE MAP('HELP80A')                          sbank80p
016000                          MAPSET('MBANK80')                       sbank80p
016100        END-EXEC                                                  sbank80p
016200        GO TO SCREEN80-READ-EXIT                                  sbank80p
016300     END-IF.                                                      sbank80p
016400                                                                  sbank80p
016500     IF OPT1L IN BANK80AI IS EQUAL TO 0                           sbank80p
016600        MOVE LOW-VALUES TO BANK-SCR80-OPT1                        sbank80p
016700     ELSE                                                         sbank80p
016800        MOVE OPT1I IN BANK80AI TO BANK-SCR80-OPT1                 sbank80p
016900        IF BANK-SCR80-OPT1 IS EQUAL TO SPACES OR                  sbank80p
017000           BANK-SCR80-OPT1 IS EQUAL TO '_'                        sbank80p
017100           MOVE LOW-VALUES TO BANK-SCR80-OPT1                     sbank80p
017200        END-IF                                                    sbank80p
017300     END-IF.                                                      sbank80p
017400                                                                  sbank80p
017500     IF OPT2L IN BANK80AI IS EQUAL TO 0                           sbank80p
017600        MOVE LOW-VALUES TO BANK-SCR80-OPT2                        sbank80p
017700     ELSE                                                         sbank80p
017800        MOVE OPT2I IN BANK80AI TO BANK-SCR80-OPT2                 sbank80p
017900        IF BANK-SCR80-OPT2 IS EQUAL TO SPACES OR                  sbank80p
018000           BANK-SCR80-OPT2 IS EQUAL TO '_'                        sbank80p
018100           MOVE LOW-VALUES TO BANK-SCR80-OPT2                     sbank80p
018200        END-IF                                                    sbank80p
018300     END-IF.                                                      sbank80p
018400                                                                  sbank80p
018500     GO TO SCREEN80-READ-EXIT.                                    sbank80p
018600                                                                  sbank80p
018700 SCREEN80-READ-INET.                                              sbank80p
018800     MOVE EXT-IP80-OPT1 TO BANK-SCR80-OPT1.                       sbank80p
018900     MOVE EXT-IP80-OPT2 TO BANK-SCR80-OPT2.                       sbank80p
019000     IF BANK-AID-PFK10                                            sbank80p
019100        SET PRINT-CONFIRM TO TRUE                                 sbank80p
019200     END-IF.                                                      sbank80p
019300     GO TO SCREEN80-READ-EXIT.                                    sbank80p
019400                                                                  sbank80p
019500 SCREEN80-READ-EXIT.                                              sbank80p
019600     EXIT.                                                        sbank80p
019700                                                                  sbank80p
019800***************************************************************** sbank80p
019900* Screen processing for SCREEN80 (BANK80/HELP80)                * sbank80p
020000*---------------------------------------------------------------* sbank80p
020100* Build the output screen and send it                           * sbank80p
020200***************************************************************** sbank80p
020300 SCREEN80-BUILD-AND-SEND.                                         sbank80p
020400* Clear map area, get date & time and move to the map             sbank80p
020500     MOVE LOW-VALUES TO BANK80AO.                                 sbank80p
020600     MOVE EIBTIME TO DD-TIME-INPUT-N.                             sbank80p
020700     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      sbank80p
020800     SET DDI-YYDDD TO TRUE.                                       sbank80p
020900     SET DDO-DD-MMM-YYYY TO TRUE.                                 sbank80p
021000     PERFORM CALL-DATECONV THRU                                   sbank80p
021100             CALL-DATECONV-EXIT.                                  sbank80p
021200* Ensure the last map fields are correct                          sbank80p
021300     IF BANK-HELP-ACTIVE                                          sbank80p
021400        MOVE 'MBANK80' TO BANK-LAST-MAPSET                        sbank80p
021500        MOVE 'HELP80A' TO BANK-LAST-MAP                           sbank80p
021600     ELSE                                                         sbank80p
021700        MOVE 'MBANK80' TO BANK-LAST-MAPSET                        sbank80p
021800        MOVE 'BANK80A' TO BANK-LAST-MAP                           sbank80p
021900     END-IF.                                                      sbank80p
022000     IF BANK-ENV-CICS                                             sbank80p
022100        GO TO SCREEN80-BUILD-AND-SEND-CICS                        sbank80p
022200     ELSE                                                         sbank80p
022300        GO TO SCREEN80-BUILD-AND-SEND-INET                        sbank80p
022400     END-IF.                                                      sbank80p
022500                                                                  sbank80p
022600 SCREEN80-BUILD-AND-SEND-CICS.                                    sbank80p
022700     IF BANK-LAST-MAP IS EQUAL TO 'BANK80A'                       sbank80p
022800        GO TO BANK80-BUILD-AND-SEND-CICS                          sbank80p
022900     END-IF.                                                      sbank80p
023000     IF BANK-LAST-MAP IS EQUAL TO 'HELP80A'                       sbank80p
023100        GO TO HELP80-BUILD-AND-SEND-CICS                          sbank80p
023200     END-IF.                                                      sbank80p
023300     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                          sbank80p
023400     MOVE '0003' TO ABEND-CODE                                    sbank80p
023500     MOVE SPACES TO ABEND-REASON                                  sbank80p
023600     COPY CABENDPO.                                               sbank80p
023700     GOBACK.                                                      sbank80p
023800                                                                  sbank80p
023900 BANK80-BUILD-AND-SEND-CICS.                                      sbank80p
024000     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANK80AO==.        sbank80p
024100     COPY CVERSNP1 REPLACING ==<<SCRN>>== BY ==BANK80AO==.        sbank80p
024200     MOVE WS-TRAN-ID TO TRANO IN BANK80AO.                        sbank80p
024300     MOVE DD-TIME-OUTPUT TO TIMEO IN BANK80AO.                    sbank80p
024400     MOVE DDO-DATA TO DATEO IN BANK80AO.                          sbank80p
024500* Move in any error message                                       sbank80p
024600     MOVE BANK-ERROR-MSG TO ERRMSGO IN BANK80AO.                  sbank80p
024700* Move in screen specific fields                                  sbank80p
024800     MOVE BANK-SCR80-CONTACT-ID TO USERIDO IN BANK80AO.           sbank80p
024900     MOVE BANK-SCR80-CONTACT-NAME TO USERNMO IN BANK80AO.         sbank80p
025000                                                                  sbank80p
025100     MOVE BANK-SCR80-ADDR1 TO MADDR1O IN BANK80AO.                sbank80p
025200     MOVE BANK-SCR80-ADDR2 TO MADDR2O IN BANK80AO.                sbank80p
025300     MOVE BANK-SCR80-STATE TO MSTATEO IN BANK80AO.                sbank80p
025400     MOVE BANK-SCR80-CNTRY TO MCNTRYO IN BANK80AO.                sbank80p
025500     MOVE BANK-SCR80-PSTCDE TO MPSTCDEO IN BANK80AO.              sbank80p
025600     MOVE BANK-SCR80-EMAIL TO MEMAILO IN BANK80AO.                sbank80p
025700                                                                  sbank80p
025800     IF BANK-SCR80-EMAIL IS NOT EQUAL TO SPACES                   sbank80p
025900        MOVE -1 TO OPT1L IN BANK80AI                              sbank80p
026000        IF BANK-SCR80-OPT1 IS EQUAL TO LOW-VALUES                 sbank80p
026100           MOVE ALL '_' TO OPT1O IN BANK80AO                      sbank80p
026200        ELSE                                                      sbank80p
026300           MOVE BANK-SCR80-OPT1 TO OPT1O IN BANK80AO              sbank80p
026400        END-IF                                                    sbank80p
026500        MOVE -1 TO OPT2L IN BANK80AI                              sbank80p
026600        IF BANK-SCR80-OPT2 IS EQUAL TO LOW-VALUES                 sbank80p
026700           MOVE ALL '_' TO OPT2O IN BANK80AO                      sbank80p
026800        ELSE                                                      sbank80p
026900           MOVE BANK-SCR80-OPT2 TO OPT2O IN BANK80AO              sbank80p
027000        END-IF                                                    sbank80p
027100        IF BANK-SCR80-OPT2 IS EQUAL TO HIGH-VALUES                sbank80p
027200           MOVE LOW-VALUES TO BANK-SCR80-OPT2                     sbank80p
027300           MOVE ALL '_' TO OPT2O IN BANK80AO                      sbank80p
027400           MOVE DFHBMPRF TO OPT2A IN BANK80AI                     sbank80p
027500        END-IF                                                    sbank80p
027600     ELSE                                                         sbank80p
027700        MOVE -1 TO TXT01L IN BANK80AI                             sbank80p
027800        MOVE '(not available)' to MEMAILO IN BANK80AO             sbank80p
027900        MOVE SPACES TO TXT07O IN BANK80AO                         sbank80p
028000        MOVE SPACES TO TXT08O IN BANK80AO                         sbank80p
028100        MOVE SPACES TO OPT1O IN BANK80AO                          sbank80p
028200        MOVE SPACES TO TXT09O IN BANK80AO                         sbank80p
028300        MOVE SPACES TO OPT2O IN BANK80AO                          sbank80p
028400        MOVE DFHBMPRF TO OPT1A IN BANK80AI                        sbank80p
028500        MOVE DFHBMPRF TO OPT2A IN BANK80AI                        sbank80p
028600     END-IF.                                                      sbank80p
028700                                                                  sbank80p
028800* Turn colour off if required                                     sbank80p
028900     IF COLOUR-OFF                                                sbank80p
029000        MOVE DFHGREEN TO TXT01C IN BANK80AO                       sbank80p
029100        MOVE DFHGREEN TO SCRNC IN BANK80AO                        sbank80p
029200        MOVE DFHGREEN TO HEAD1C IN BANK80AO                       sbank80p
029300        MOVE DFHGREEN TO DATEC IN BANK80AO                        sbank80p
029400        MOVE DFHGREEN TO TXT02C IN BANK80AO                       sbank80p
029500        MOVE DFHGREEN TO TRANC IN BANK80AO                        sbank80p
029600        MOVE DFHGREEN TO HEAD2C IN BANK80AO                       sbank80p
029700        MOVE DFHGREEN TO TIMEC IN BANK80AO                        sbank80p
029800        MOVE DFHGREEN TO TXT03C IN BANK80AO                       sbank80p
029900        MOVE DFHGREEN TO USERIDC IN BANK80AO                      sbank80p
030000        MOVE DFHGREEN TO TXT04C IN BANK80AO                       sbank80p
030100        MOVE DFHGREEN TO USERNMC IN BANK80AO                      sbank80p
030200        MOVE DFHGREEN TO TXT05C IN BANK80AO                       sbank80p
030300        MOVE DFHGREEN TO MADDR1C IN BANK80AO                      sbank80p
030400        MOVE DFHGREEN TO MADDR2C IN BANK80AO                      sbank80p
030500        MOVE DFHGREEN TO MSTATEC IN BANK80AO                      sbank80p
030600        MOVE DFHGREEN TO MCNTRYC IN BANK80AO                      sbank80p
030700        MOVE DFHGREEN TO MPSTCDEC IN BANK80AO                     sbank80p
030800        MOVE DFHGREEN TO TXT06C IN BANK80AO                       sbank80p
030900        MOVE DFHGREEN TO MEMAILC IN BANK80AO                      sbank80p
031000        MOVE DFHGREEN TO TXT07C IN BANK80AO                       sbank80p
031100        MOVE DFHGREEN TO TXT08C IN BANK80AO                       sbank80p
031200        MOVE DFHGREEN TO OPT1C IN BANK80AO                        sbank80p
031300        MOVE DFHGREEN TO TXT09C IN BANK80AO                       sbank80p
031400        MOVE DFHGREEN TO OPT2C IN BANK80AO                        sbank80p
031500        MOVE DFHGREEN TO ERRMSGC IN BANK80AO                      sbank80p
031600        MOVE DFHGREEN TO TXT17C IN BANK80AO                       sbank80p
031700        MOVE DFHGREEN TO VERC IN BANK80AO                         sbank80p
031800     END-IF.                                                      sbank80p
031900                                                                  sbank80p
032000     EXEC CICS SEND MAP('BANK80A')                                sbank80p
032100                    MAPSET('MBANK80')                             sbank80p
032200                    CURSOR                                        sbank80p
032300                    ERASE                                         sbank80p
032400                    FREEKB                                        sbank80p
032500     END-EXEC.                                                    sbank80p
032600     GO TO SCREEN80-BUILD-AND-SEND-EXIT.                          sbank80p
032700                                                                  sbank80p
032800 HELP80-BUILD-AND-SEND-CICS.                                      sbank80p
032900     COPY CSCRNHP2 REPLACING ==:OPTN:== BY ==BANK==               sbank80p
033000                             ==<<SCRN>>== BY ==HELP80AO==.        sbank80p
033100                                                                  sbank80p
033200     EXEC CICS SEND MAP('HELP80A')                                sbank80p
033300                    MAPSET('MBANK80')                             sbank80p
033400                    ERASE                                         sbank80p
033500                    FREEKB                                        sbank80p
033600     END-EXEC.                                                    sbank80p
033700     GO TO SCREEN80-BUILD-AND-SEND-EXIT.                          sbank80p
033800                                                                  sbank80p
033900 SCREEN80-BUILD-AND-SEND-INET.                                    sbank80p
034000     MOVE SPACES TO EXT-OP-DATA.                                  sbank80p
034100     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              sbank80p
034200     MOVE DDO-DATA TO EXT-OP-DATE.                                sbank80p
034300     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          sbank80p
034400     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         sbank80p
034500     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          sbank80p
034600     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          sbank80p
034700     CALL 'SVERSONP' USING SCREEN-TITLES.                         sbank80p
034800     MOVE VERSION TO EXT-OP-VERSION.                              sbank80p
034900* Move in screen name                                             sbank80p
035000     MOVE 'BANK80' TO EXT-OP-SCREEN.                              sbank80p
035100* Move in userid and any error message                            sbank80p
035200     MOVE BANK-ERROR-MSG TO EXT-OP-ERR-MSG.                       sbank80p
035300     MOVE BANK-USERID TO EXT-OP-USERID.                           sbank80p
035400     MOVE BANK-USERID-NAME TO EXT-OP-NAME.                        sbank80p
035500* Move in screen specific fields                                  sbank80p
035600     MOVE BANK-SCR80-ADDR1 TO EXT-OP80-ADDR1.                     sbank80p
035700     MOVE BANK-SCR80-ADDR2 TO EXT-OP80-ADDR2.                     sbank80p
035800     MOVE BANK-SCR80-STATE TO EXT-OP80-STATE.                     sbank80p
035900     MOVE BANK-SCR80-CNTRY TO EXT-OP80-CNTRY.                     sbank80p
036000     MOVE BANK-SCR80-PSTCDE TO EXT-OP80-PSTCDE.                   sbank80p
036100     MOVE BANK-SCR80-EMAIL TO EXT-OP80-EMAIL.                     sbank80p
036200     MOVE BANK-SCR80-OPT1 TO EXT-OP80-OPT1.                       sbank80p
036300     MOVE BANK-SCR80-OPT2 TO EXT-OP80-OPT2.                       sbank80p
036400                                                                  sbank80p
036500 SCREEN80-BUILD-AND-SEND-EXIT.                                    sbank80p
036600     EXIT.                                                        sbank80p
036700                                                                  sbank80p
036800***************************************************************** sbank80p
036900* Call common routine to perform date conversions               * sbank80p
037000***************************************************************** sbank80p
037100 CALL-DATECONV.                                                   sbank80p
037200     MOVE BANK-ENV TO DD-ENV.                                     sbank80p
037300     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           sbank80p
037400     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            sbank80p
037500 CALL-DATECONV-EXIT.                                              sbank80p
037600     EXIT.                                                        sbank80p
037700                                                                  sbank80p
037800* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     sbank80p
