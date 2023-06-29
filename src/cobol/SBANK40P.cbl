000100***************************************************************** sbank40p
000200*                                                               * sbank40p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * sbank40p
000400*   This demonstration program is provided for use by users     * sbank40p
000500*   of Micro Focus products and may be used, modified and       * sbank40p
000600*   distributed as part of your application provided that       * sbank40p
000700*   you properly acknowledge the copyright of Micro Focus       * sbank40p
000800*   in this material.                                           * sbank40p
000900*                                                               * sbank40p
001000***************************************************************** sbank40p
001100                                                                  sbank40p
001200***************************************************************** sbank40p
001300* Program:     SBANK40P.CBL (CICS Version)                      * sbank40p
001400* Layer:       Screen handling                                  * sbank40p
001500* Function:    Display transaction details                      * sbank40p
001600***************************************************************** sbank40p
001700                                                                  sbank40p
001800 IDENTIFICATION DIVISION.                                         sbank40p
001900 PROGRAM-ID.                                                      sbank40p
002000     SBANK40P.                                                    sbank40p
002100 DATE-WRITTEN.                                                    sbank40p
002200     September 2002.                                              sbank40p
002300 DATE-COMPILED.                                                   sbank40p
002400     Today.                                                       sbank40p
002500                                                                  sbank40p
002600 ENVIRONMENT DIVISION.                                            sbank40p
002700                                                                  sbank40p
002800 DATA DIVISION.                                                   sbank40p
002900 WORKING-STORAGE SECTION.                                         sbank40p
003000 01  WS-MISC-STORAGE.                                             sbank40p
003100   05  WS-PROGRAM-ID                         PIC X(8)             sbank40p
003200       VALUE 'SBANK40P'.                                          sbank40p
003300   05  WS-TRAN-ID                            PIC X(4).            sbank40p
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             sbank40p
003500       VALUE SPACES.                                              sbank40p
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             sbank40p
003700       VALUE 'UNKNOWN'.                                           sbank40p
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      sbank40p
003900   05  WS-WORK1                              PIC X(1).            sbank40p
004000   05  WS-SUB1                               PIC S9(4) COMP.      sbank40p
         05  WS-VERSION                            PIC X(7).
                                                                        sbank40p
004100                                                                  
004200 01  MAPAREA                                 PIC X(2048).         sbank40p
004300 COPY MBANK40.                                                    sbank40p
004400                                                                  sbank40p
004500 01  WS-TIME-DATE-WORK-AREA.                                      sbank40p
004600 COPY CDATED.                                                     sbank40p
004700                                                                  sbank40p
004800 01  WS-BANK-DATA-AREAS.                                          sbank40p
004900   05  WS-BANK-DATA.                                              sbank40p
005000 COPY CBANKDAT.                                                   sbank40p
005100   05  WS-BANK-EXT-DATA.                                          sbank40p
005200 COPY CBANKEXT.                                                   sbank40p
005300                                                                  sbank40p
005400 COPY CSCRNHDD.                                                   sbank40p
005500                                                                  sbank40p
005600 COPY CVERSND.                                                    sbank40p
005700                                                                  sbank40p
005800 COPY DFHAID.                                                     sbank40p
005900                                                                  sbank40p
006000 COPY DFHBMSCA.                                                   sbank40p
006100                                                                  sbank40p
006200 COPY CABENDD.                                                    sbank40p
006300                                                                  sbank40p
006400 LINKAGE SECTION.                                                 sbank40p
006500 01  DFHCOMMAREA.                                                 sbank40p
006600   05  FILLER                                PIC X(1)             sbank40p
006700       OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.             sbank40p
006800                                                                  sbank40p
006900 PROCEDURE DIVISION.                                              sbank40p
007000***************************************************************** sbank40p
007100* Write entry to log to show we have been invoked               * sbank40p
007200***************************************************************** sbank40p
007300     COPY CTRACE.                                                 sbank40p
007400                                                                  sbank40p
007500***************************************************************** sbank40p
007600* Store our transaction-id                                      * sbank40p
007700***************************************************************** sbank40p
007800     MOVE EIBTRNID TO WS-TRAN-ID.                                 sbank40p
007900                                                                  sbank40p
008000***************************************************************** sbank40p
008100* Store passed data or abend if there wasn't any                * sbank40p
008200***************************************************************** sbank40p
008300     IF EIBCALEN IS EQUAL TO 0                                    sbank40p
008400        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       sbank40p
008500        MOVE '0001' TO ABEND-CODE                                 sbank40p
008600        MOVE SPACES TO ABEND-REASON                               sbank40p
008700        COPY CABENDPO.                                            sbank40p
008800     ELSE                                                         sbank40p
008900        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        sbank40p
009000        MOVE LOW-VALUES TO WS-BANK-DATA                           sbank40p
009100        MOVE DFHCOMMAREA (1:EIBCALEN)                             sbank40p
009200          TO WS-BANK-DATA-AREAS (1:LENGTH OF WS-BANK-DATA-AREAS)  sbank40p
009300     END-IF.                                                      sbank40p
009400                                                                  sbank40p
009500***************************************************************** sbank40p
009600* This is the main process                                      * sbank40p
009700***************************************************************** sbank40p
009800                                                                  sbank40p
009900***************************************************************** sbank40p
010000* Determine what we have to do (read from or send to screen)    * sbank40p
010100***************************************************************** sbank40p
010200     MOVE LOW-VALUE TO MAPAREA.                                   sbank40p
010300     EVALUATE TRUE                                                sbank40p
010400       WHEN BANK-MAP-FUNCTION-GET                                 sbank40p
010500         PERFORM SCREEN40-READ THRU                               sbank40p
010600                 SCREEN40-READ-EXIT                               sbank40p
010700       WHEN BANK-MAP-FUNCTION-PUT                                 sbank40p
010800         PERFORM SCREEN40-BUILD-AND-SEND THRU                     sbank40p
010900                 SCREEN40-BUILD-AND-SEND-EXIT                     sbank40p
011000       WHEN OTHER                                                 sbank40p
011100         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      sbank40p
011200         MOVE '0002' TO ABEND-CODE                                sbank40p
011300         MOVE SPACES TO ABEND-REASON                              sbank40p
011400         COPY CABENDPO.                                           sbank40p
011500     END-EVALUATE.                                                sbank40p
011600                                                                  sbank40p
011700* Call the appropriate routine to handle the business logic       sbank40p
011800     IF BANK-MAP-FUNCTION-GET                                     sbank40p
011900        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             sbank40p
012000                       COMMAREA(WS-BANK-DATA)                     sbank40p
012100                       LENGTH(LENGTH OF WS-BANK-DATA)             sbank40p
012200        END-EXEC                                                  sbank40p
012300     END-IF.                                                      sbank40p
012400                                                                  sbank40p
012500***************************************************************** sbank40p
012600* Now we have to have finished and can return to our invoker.   * sbank40p
012700***************************************************************** sbank40p
012800* Now return to CICS                                              sbank40p
012900     MOVE WS-BANK-DATA-AREAS (1:LENGTH OF WS-BANK-DATA-AREAS)     sbank40p
013000       TO DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      sbank40p
013100     EXEC CICS                                                    sbank40p
013200          RETURN                                                  sbank40p
013300     END-EXEC.                                                    sbank40p
013400     GOBACK.                                                      sbank40p
013500                                                                  sbank40p
013600***************************************************************** sbank40p
013700* Screen processing for MBANK40                                 * sbank40p
013800*---------------------------------------------------------------* sbank40p
013900* Retrieve data from screen and format it                       * sbank40p
014000***************************************************************** sbank40p
014100 SCREEN40-READ.                                                   sbank40p
014200     MOVE 'BBANK40P' TO WS-BUSINESS-LOGIC-PGM.                    sbank40p
014300     IF BANK-AID-CLEAR                                            sbank40p
014400        SET BANK-AID-PFK03 TO TRUE                                sbank40p
014500        GO TO SCREEN40-READ-EXIT                                  sbank40p
014600     END-IF.                                                      sbank40p
014700     IF BANK-ENV-CICS                                             sbank40p
014800        GO TO SCREEN40-READ-CICS                                  sbank40p
014900     ELSE                                                         sbank40p
015000        GO TO SCREEN40-READ-INET                                  sbank40p
015100     END-IF.                                                      sbank40p
015200                                                                  sbank40p
015300 SCREEN40-READ-CICS.                                              sbank40p
015400     IF BANK-HELP-INACTIVE                                        sbank40p
015500        EXEC CICS RECEIVE MAP('BANK40A')                          sbank40p
015600                          MAPSET('MBANK40')                       sbank40p
015700        END-EXEC                                                  sbank40p
015800     ELSE                                                         sbank40p
015900        EXEC CICS RECEIVE MAP('HELP40A')                          sbank40p
016000                          MAPSET('MBANK40')                       sbank40p
016100        END-EXEC                                                  sbank40p
016200        GO TO SCREEN40-READ-EXIT                                  sbank40p
016300     END-IF.                                                      sbank40p
016400                                                                  sbank40p
016500     GO TO SCREEN40-READ-EXIT.                                    sbank40p
016600                                                                  sbank40p
016700 SCREEN40-READ-INET.                                              sbank40p
016800     GO TO SCREEN40-READ-EXIT.                                    sbank40p
016900                                                                  sbank40p
017000 SCREEN40-READ-EXIT.                                              sbank40p
017100     EXIT.                                                        sbank40p
017200                                                                  sbank40p
017300***************************************************************** sbank40p
017400* Screen processing for SCREEN40 (BANK40/HELP40)                * sbank40p
017500*---------------------------------------------------------------* sbank40p
017600* Build the output screen and send it                           * sbank40p
017700***************************************************************** sbank40p
017800 SCREEN40-BUILD-AND-SEND.                                         sbank40p
017900* Clear map area, get date & time and move to the map             sbank40p
018000     MOVE LOW-VALUES TO BANK40AO.                                 sbank40p
018100     MOVE EIBTIME TO DD-TIME-INPUT-N.                             sbank40p
018200     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      sbank40p
018300     SET DDI-YYDDD TO TRUE.                                       sbank40p
018400     SET DDO-DD-MMM-YYYY TO TRUE.                                 sbank40p
018500     PERFORM CALL-DATECONV THRU                                   sbank40p
018600             CALL-DATECONV-EXIT.                                  sbank40p
018700* Ensure the last map fields are correct                          sbank40p
018800     IF BANK-HELP-ACTIVE                                          sbank40p
018900        MOVE 'MBANK40' TO BANK-LAST-MAPSET                        sbank40p
019000        MOVE 'HELP40A' TO BANK-LAST-MAP                           sbank40p
019100     ELSE                                                         sbank40p
019200        MOVE 'MBANK40' TO BANK-LAST-MAPSET                        sbank40p
019300        MOVE 'BANK40A' TO BANK-LAST-MAP                           sbank40p
019400     END-IF.                                                      sbank40p
019500     IF BANK-ENV-CICS                                             sbank40p
019600        GO TO SCREEN40-BUILD-AND-SEND-CICS                        sbank40p
019700     ELSE                                                         sbank40p
019800        GO TO SCREEN40-BUILD-AND-SEND-INET                        sbank40p
019900     END-IF.                                                      sbank40p
020000                                                                  sbank40p
020100 SCREEN40-BUILD-AND-SEND-CICS.                                    sbank40p
020200     IF BANK-LAST-MAP IS EQUAL TO 'BANK40A'                       sbank40p
020300        GO TO BANK40-BUILD-AND-SEND-CICS                          sbank40p
020400     END-IF.                                                      sbank40p
020500     IF BANK-LAST-MAP IS EQUAL TO 'HELP40A'                       sbank40p
020600        GO TO HELP40-BUILD-AND-SEND-CICS                          sbank40p
020700     END-IF.                                                      sbank40p
020800     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                          sbank40p
020900     MOVE '0003' TO ABEND-CODE                                    sbank40p
021000     MOVE SPACES TO ABEND-REASON                                  sbank40p
021100     COPY CABENDPO.                                               sbank40p
021200     GOBACK.                                                      sbank40p
021300                                                                  sbank40p
021400 BANK40-BUILD-AND-SEND-CICS.                                      sbank40p
021500     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANK40AO==.        sbank40p
021600     COPY CVERSNP1 REPLACING ==<<SCRN>>== BY ==BANK40AO==.        sbank40p
021700     MOVE WS-TRAN-ID TO TRANO IN BANK40AO.                        sbank40p
021800     MOVE DD-TIME-OUTPUT TO TIMEO IN BANK40AO.                    sbank40p
021900     MOVE DDO-DATA TO DATEO IN BANK40AO.                          sbank40p
022000* Move in any error message                                       sbank40p
022100     MOVE BANK-ERROR-MSG TO ERRMSGO IN BANK40AO.                  sbank40p
022200* Move in screen specific fields                                  sbank40p
022300     MOVE BANK-SCR40-ACC TO ACCNOO IN BANK40AO.                   sbank40p
022400     MOVE BANK-SCR40-ACCTYPE TO ACCTYPEO IN BANK40AO.             sbank40p
022500                                                                  sbank40p
022600     EVALUATE TRUE                                                sbank40p
022700       WHEN BANK-PAGING-OFF                                       sbank40p
022800         MOVE DFHBMDAR TO TXT09A IN BANK40AI                      sbank40p
022900         MOVE SPACES TO MOREO IN BANK40AO                         sbank40p
023000         MOVE SPACES TO PAGINGO IN BANK40AO                       sbank40p
023100       WHEN BANK-PAGING-FIRST                                     sbank40p
023200         MOVE ' /+' TO MOREO IN BANK40AO                          sbank40p
023300         MOVE '        F8=Forward' TO PAGINGO IN BANK40AO         sbank40p
023400       WHEN BANK-PAGING-MIDDLE                                    sbank40p
023500         MOVE '-/+' TO MOREO IN BANK40AO                          sbank40p
023600         MOVE 'F7=Back F8=Forward' TO PAGINGO IN BANK40AO         sbank40p
023700       WHEN BANK-PAGING-LAST                                      sbank40p
023800         MOVE '-/ ' TO MOREO IN BANK40AO                          sbank40p
023900         MOVE 'F7=Back           ' TO PAGINGO IN BANK40AO         sbank40p
024000       WHEN OTHER                                                 sbank40p
024100         MOVE DFHBMDAR TO TXT09A IN BANK40AI                      sbank40p
024200         MOVE SPACES TO MOREO IN BANK40AO                         sbank40p
024300         MOVE SPACES TO PAGINGO IN BANK40AO                       sbank40p
024400     END-EVALUATE.                                                sbank40p
024500                                                                  sbank40p
024600     MOVE BANK-SCR40-DAT1 TO DAT1O IN BANK40AO.                   sbank40p
024700     MOVE BANK-SCR40-TIM1 TO TIM1O IN BANK40AO.                   sbank40p
024800     MOVE BANK-SCR40-AMT1 TO AMT1O IN BANK40AO.                   sbank40p
024900     MOVE BANK-SCR40-DSC1 TO DSC1O IN BANK40AO.                   sbank40p
025000     MOVE BANK-SCR40-DAT2 TO DAT2O IN BANK40AO.                   sbank40p
025100     MOVE BANK-SCR40-TIM2 TO TIM2O IN BANK40AO.                   sbank40p
025200     MOVE BANK-SCR40-AMT2 TO AMT2O IN BANK40AO.                   sbank40p
025300     MOVE BANK-SCR40-DSC2 TO DSC2O IN BANK40AO.                   sbank40p
025400     MOVE BANK-SCR40-DAT3 TO DAT3O IN BANK40AO.                   sbank40p
025500     MOVE BANK-SCR40-TIM3 TO TIM3O IN BANK40AO.                   sbank40p
025600     MOVE BANK-SCR40-AMT3 TO AMT3O IN BANK40AO.                   sbank40p
025700     MOVE BANK-SCR40-DSC3 TO DSC3O IN BANK40AO.                   sbank40p
025800     MOVE BANK-SCR40-DAT4 TO DAT4O IN BANK40AO.                   sbank40p
025900     MOVE BANK-SCR40-TIM4 TO TIM4O IN BANK40AO.                   sbank40p
026000     MOVE BANK-SCR40-AMT4 TO AMT4O IN BANK40AO.                   sbank40p
026100     MOVE BANK-SCR40-DSC4 TO DSC4O IN BANK40AO.                   sbank40p
026200     MOVE BANK-SCR40-DAT5 TO DAT5O IN BANK40AO.                   sbank40p
026300     MOVE BANK-SCR40-TIM5 TO TIM5O IN BANK40AO.                   sbank40p
026400     MOVE BANK-SCR40-AMT5 TO AMT5O IN BANK40AO.                   sbank40p
026500     MOVE BANK-SCR40-DSC5 TO DSC5O IN BANK40AO.                   sbank40p
026600     MOVE BANK-SCR40-DAT6 TO DAT6O IN BANK40AO.                   sbank40p
026700     MOVE BANK-SCR40-TIM6 TO TIM6O IN BANK40AO.                   sbank40p
026800     MOVE BANK-SCR40-AMT6 TO AMT6O IN BANK40AO.                   sbank40p
026900     MOVE BANK-SCR40-DSC6 TO DSC6O IN BANK40AO.                   sbank40p
027000     MOVE BANK-SCR40-DAT7 TO DAT7O IN BANK40AO.                   sbank40p
027100     MOVE BANK-SCR40-TIM7 TO TIM7O IN BANK40AO.                   sbank40p
027200     MOVE BANK-SCR40-AMT7 TO AMT7O IN BANK40AO.                   sbank40p
027300     MOVE BANK-SCR40-DSC7 TO DSC7O IN BANK40AO.                   sbank40p
027400     MOVE BANK-SCR40-DAT8 TO DAT8O IN BANK40AO.                   sbank40p
027500     MOVE BANK-SCR40-TIM8 TO TIM8O IN BANK40AO.                   sbank40p
027600     MOVE BANK-SCR40-AMT8 TO AMT8O IN BANK40AO.                   sbank40p
027700     MOVE BANK-SCR40-DSC8 TO DSC8O IN BANK40AO.                   sbank40p
027800* Turn colour off if requ8red                                     sbank40p
027900     IF COLOUR-OFF                                                sbank40p
028000        MOVE DFHGREEN TO TXT01C IN BANK40AO                       sbank40p
028100        MOVE DFHGREEN TO SCRNC IN BANK40AO                        sbank40p
028200        MOVE DFHGREEN TO HEAD1C IN BANK40AO                       sbank40p
028300        MOVE DFHGREEN TO DATEC IN BANK40AO                        sbank40p
028400        MOVE DFHGREEN TO TXT02C IN BANK40AO                       sbank40p
028500        MOVE DFHGREEN TO TRANC IN BANK40AO                        sbank40p
028600        MOVE DFHGREEN TO HEAD2C IN BANK40AO                       sbank40p
028700        MOVE DFHGREEN TO TIMEC IN BANK40AO                        sbank40p
028800        MOVE DFHGREEN TO TXT03C IN BANK40AO                       sbank40p
028900        MOVE DFHGREEN TO ACCNOC IN BANK40AO                       sbank40p
029000        MOVE DFHGREEN TO TXT04C IN BANK40AO                       sbank40p
029100        MOVE DFHGREEN TO ACCTYPEC IN BANK40AO                     sbank40p
029200        MOVE DFHGREEN TO TXT05C IN BANK40AO                       sbank40p
029300        MOVE DFHGREEN TO TXT06C IN BANK40AO                       sbank40p
029400        MOVE DFHGREEN TO TXT07C IN BANK40AO                       sbank40p
029500        MOVE DFHGREEN TO TXT08C IN BANK40AO                       sbank40p
029600        MOVE DFHGREEN TO TXT09C IN BANK40AO                       sbank40p
029700        MOVE DFHGREEN TO MOREC IN BANK40AO                        sbank40p
029800        MOVE DFHGREEN TO DAT1C IN BANK40AO                        sbank40p
029900        MOVE DFHGREEN TO TIM1C IN BANK40AO                        sbank40p
030000        MOVE DFHGREEN TO AMT1C IN BANK40AO                        sbank40p
030100        MOVE DFHGREEN TO DSC1C IN BANK40AO                        sbank40p
030200        MOVE DFHGREEN TO DAT2C IN BANK40AO                        sbank40p
030300        MOVE DFHGREEN TO TIM2C IN BANK40AO                        sbank40p
030400        MOVE DFHGREEN TO AMT2C IN BANK40AO                        sbank40p
030500        MOVE DFHGREEN TO DSC2C IN BANK40AO                        sbank40p
030600        MOVE DFHGREEN TO DAT3C IN BANK40AO                        sbank40p
030700        MOVE DFHGREEN TO TIM3C IN BANK40AO                        sbank40p
030800        MOVE DFHGREEN TO AMT3C IN BANK40AO                        sbank40p
030900        MOVE DFHGREEN TO DSC3C IN BANK40AO                        sbank40p
031000        MOVE DFHGREEN TO DAT4C IN BANK40AO                        sbank40p
031100        MOVE DFHGREEN TO TIM4C IN BANK40AO                        sbank40p
031200        MOVE DFHGREEN TO AMT4C IN BANK40AO                        sbank40p
031300        MOVE DFHGREEN TO DSC4C IN BANK40AO                        sbank40p
031400        MOVE DFHGREEN TO DAT5C IN BANK40AO                        sbank40p
031500        MOVE DFHGREEN TO TIM5C IN BANK40AO                        sbank40p
031600        MOVE DFHGREEN TO AMT5C IN BANK40AO                        sbank40p
031700        MOVE DFHGREEN TO DSC5C IN BANK40AO                        sbank40p
031800        MOVE DFHGREEN TO DAT6C IN BANK40AO                        sbank40p
031900        MOVE DFHGREEN TO TIM6C IN BANK40AO                        sbank40p
032000        MOVE DFHGREEN TO AMT6C IN BANK40AO                        sbank40p
032100        MOVE DFHGREEN TO DSC6C IN BANK40AO                        sbank40p
032200        MOVE DFHGREEN TO DAT7C IN BANK40AO                        sbank40p
032300        MOVE DFHGREEN TO TIM7C IN BANK40AO                        sbank40p
032400        MOVE DFHGREEN TO AMT7C IN BANK40AO                        sbank40p
032500        MOVE DFHGREEN TO DSC7C IN BANK40AO                        sbank40p
032600        MOVE DFHGREEN TO DAT8C IN BANK40AO                        sbank40p
032700        MOVE DFHGREEN TO TIM8C IN BANK40AO                        sbank40p
032800        MOVE DFHGREEN TO AMT8C IN BANK40AO                        sbank40p
032900        MOVE DFHGREEN TO DSC8C IN BANK40AO                        sbank40p
033000        MOVE DFHGREEN TO ERRMSGC IN BANK40AO                      sbank40p
033100        MOVE DFHGREEN TO TXT10C IN BANK40AO                       sbank40p
033200        MOVE DFHGREEN TO PAGINGC IN BANK40AO                      sbank40p
033300        MOVE DFHGREEN TO VERC IN BANK40AO                         sbank40p
033400     END-IF.                                                      sbank40p
033500                                                                  sbank40p
033600     EXEC CICS SEND MAP('BANK40A')                                sbank40p
033700                    MAPSET('MBANK40')                             sbank40p
033800                    ERASE                                         sbank40p
033900                    FREEKB                                        sbank40p
034000     END-EXEC.                                                    sbank40p
034100     GO TO SCREEN40-BUILD-AND-SEND-EXIT.                          sbank40p
034200                                                                  sbank40p
034300 HELP40-BUILD-AND-SEND-CICS.                                      sbank40p
034400     COPY CSCRNHP2 REPLACING ==:OPTN:== BY ==BANK==               sbank40p
034500                             ==<<SCRN>>== BY ==HELP40AO==.        sbank40p
034600                                                                  sbank40p
034700     EXEC CICS SEND MAP('HELP40A')                                sbank40p
034800                    MAPSET('MBANK40')                             sbank40p
034900                    ERASE                                         sbank40p
035000                    FREEKB                                        sbank40p
035100     END-EXEC.                                                    sbank40p
035200     GO TO SCREEN40-BUILD-AND-SEND-EXIT.                          sbank40p
035300                                                                  sbank40p
035400 SCREEN40-BUILD-AND-SEND-INET.                                    sbank40p
035500     MOVE SPACES TO EXT-OP-DATA.                                  sbank40p
035600     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              sbank40p
035700     MOVE DDO-DATA TO EXT-OP-DATE.                                sbank40p
035800     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          sbank40p
035900     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         sbank40p
036000     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          sbank40p
036100     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          sbank40p
036200     CALL 'SVERSONP' USING VERSION.                               sbank40p
036300     MOVE VERSION TO EXT-OP-VERSION.                              sbank40p
036400* Move in screen name                                             sbank40p
036500     MOVE 'BANK40' TO EXT-OP-SCREEN.                              sbank40p
036600* Move in userid and any error message                            sbank40p
036700     MOVE BANK-ERROR-MSG TO EXT-OP-ERR-MSG.                       sbank40p
036800     MOVE BANK-USERID TO EXT-OP-USERID.                           sbank40p
036900     MOVE BANK-USERID-NAME TO EXT-OP-NAME.                        sbank40p
037000* Move in screen specific fields                                  sbank40p
037100     MOVE BANK-SCR40-ACC TO EXT-OP40-ACCNO.                       sbank40p
037200     MOVE BANK-SCR40-ACCTYPE TO EXT-OP40-ACCTYPE.                 sbank40p
037300     MOVE BANK-PAGING-STATUS TO EXT-OP40-PAGING-STATUS.           sbank40p
037400     MOVE 0 TO WS-SUB1.                                           sbank40p
037500     PERFORM SCREEN40-BUILD-AND-SEND-INET1 8 TIMES.               sbank40p
037600     GO TO SCREEN40-BUILD-AND-SEND-EXIT.                          sbank40p
037700 SCREEN40-BUILD-AND-SEND-INET1.                                   sbank40p
037800     ADD 1 TO WS-SUB1.                                            sbank40p
037900     MOVE BANK-SCR40-DATE (WS-SUB1) TO EXT-OP40-DATE (WS-SUB1).   sbank40p
038000     MOVE BANK-SCR40-TIME (WS-SUB1) TO EXT-OP40-TIME (WS-SUB1).   sbank40p
038100     MOVE BANK-SCR40-AMNT (WS-SUB1) TO EXT-OP40-AMNT (WS-SUB1).   sbank40p
038200     MOVE BANK-SCR40-DESC (WS-SUB1) TO EXT-OP40-DESC (WS-SUB1).   sbank40p
038300                                                                  sbank40p
038400                                                                  sbank40p
038500 SCREEN40-BUILD-AND-SEND-EXIT.                                    sbank40p
038600     EXIT.                                                        sbank40p
038700                                                                  sbank40p
038800***************************************************************** sbank40p
038900* Call common routine to perform date conversions               * sbank40p
039000***************************************************************** sbank40p
039100 CALL-DATECONV.                                                   sbank40p
039200     MOVE BANK-ENV TO DD-ENV.                                     sbank40p
039300     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           sbank40p
039400     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            sbank40p
039500 CALL-DATECONV-EXIT.                                              sbank40p
039600     EXIT.                                                        sbank40p
039700                                                                  sbank40p
039800* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     sbank40p
