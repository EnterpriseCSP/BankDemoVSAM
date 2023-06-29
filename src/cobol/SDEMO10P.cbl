000100***************************************************************** SDEMO10P
000200*                                                               * SDEMO10P
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * SDEMO10P
000400*   This demonstration program is provided for use by users     * SDEMO10P
000500*   of Micro Focus products and may be used, modified and       * SDEMO10P
000600*   distributed as part of your application provided that       * SDEMO10P
000700*   you properly acknowledge the copyright of Micro Focus       * SDEMO10P
000800*   in this material.                                           * SDEMO10P
000900*                                                               * SDEMO10P
001000***************************************************************** SDEMO10P
001100                                                                  SDEMO10P
001200***************************************************************** SDEMO10P
001300* Program:     SDEMO10P.CBL (CICS Version)                      * SDEMO10P
001400* Layer:       Screen handling                                  * SDEMO10P
001500* Function:    Application selection menu                       * SDEMO10P
001600***************************************************************** SDEMO10P
001700                                                                  SDEMO10P
001800 IDENTIFICATION DIVISION.                                         SDEMO10P
001900 PROGRAM-ID.                                                      SDEMO10P
002000     SDEMO10P.                                                    SDEMO10P
002100 DATE-WRITTEN.                                                    SDEMO10P
002200     September 2002.                                              SDEMO10P
002300 DATE-COMPILED.                                                   SDEMO10P
002400     Today.                                                       SDEMO10P
002500                                                                  SDEMO10P
002600 ENVIRONMENT DIVISION.                                            SDEMO10P
002700                                                                  SDEMO10P
002800 DATA DIVISION.                                                   SDEMO10P
002900 WORKING-STORAGE SECTION.                                         SDEMO10P
003000 01  WS-MISC-STORAGE.                                             SDEMO10P
003100   05  WS-PROGRAM-ID                         PIC X(8)             SDEMO10P
003200       VALUE 'SDEMO10P'.                                          SDEMO10P
003300   05  WS-TRAN-ID                            PIC X(4).            SDEMO10P
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             SDEMO10P
003500       VALUE SPACES.                                              SDEMO10P
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             SDEMO10P
003700       VALUE 'UNKNOWN'.                                           SDEMO10P
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      SDEMO10P
         05  WS-VERSION                            PIC X(7).            
003900                                                                  SDEMO10P
004000 01  MAPAREA                                 PIC X(2048).         SDEMO10P
004100 COPY MDEMO10.                                                    SDEMO10P
004200                                                                  SDEMO10P
004300 01  WS-TIME-DATE-WORK-AREA.                                      SDEMO10P
004400 COPY CDATED.                                                     SDEMO10P
004500                                                                  SDEMO10P
004600 01  WS-DEMO-DATA.                                                SDEMO10P
004700 COPY CDEMODAT.                                                   SDEMO10P
004800                                                                  SDEMO10P
004900 01  WS-DEMO-OPTIONS.                                             SDEMO10P
005000 COPY COPTIONS.                                                   SDEMO10P
005100                                                                  SDEMO10P
005200 COPY CSCRNHDD.                                                   SDEMO10P
005300                                                                  SDEMO10P
005400 COPY DFHAID.                                                     SDEMO10P
005500                                                                  SDEMO10P
005600 COPY DFHBMSCA.                                                   SDEMO10P
005700                                                                  SDEMO10P
005800 COPY CABENDD.                                                    SDEMO10P
005900                                                                  SDEMO10P
006000 LINKAGE SECTION.                                                 SDEMO10P
006100 01  DFHCOMMAREA.                                                 SDEMO10P
006200 COPY CDEMOEXT.                                                   SDEMO10P
006300                                                                  SDEMO10P
006400 PROCEDURE DIVISION.                                              SDEMO10P
006500***************************************************************** SDEMO10P
006600* Store our transaction-id                                      * SDEMO10P
006700***************************************************************** SDEMO10P
006800     MOVE EIBTRNID TO WS-TRAN-ID.                                 SDEMO10P
006900                                                                  SDEMO10P
007000***************************************************************** SDEMO10P
007100* Store passed data or abend if there wasn't any                * SDEMO10P
007200***************************************************************** SDEMO10P
007300     IF EIBCALEN IS EQUAL TO 0                                    SDEMO10P
007400        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       SDEMO10P
007500        MOVE '0001' TO ABEND-CODE                                 SDEMO10P
007600        MOVE SPACES TO ABEND-REASON                               SDEMO10P
007700        COPY CABENDPO.                                            SDEMO10P
007800     ELSE                                                         SDEMO10P
007900        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        SDEMO10P
008000        MOVE LOW-VALUES TO WS-DEMO-DATA                           SDEMO10P
008100        MOVE DFHCOMMAREA (1:EIBCALEN) TO WS-DEMO-DATA (1:EIBCALEN)SDEMO10P
008200     END-IF.                                                      SDEMO10P
008300                                                                  SDEMO10P
008400***************************************************************** SDEMO10P
008500* This is the main process                                      * SDEMO10P
008600***************************************************************** SDEMO10P
008700                                                                  SDEMO10P
008800***************************************************************** SDEMO10P
008900* Determine what we have to do (read from or send to screen)    * SDEMO10P
009000***************************************************************** SDEMO10P
009100     MOVE LOW-VALUE TO MAPAREA.                                   SDEMO10P
009200     EVALUATE TRUE                                                SDEMO10P
009300       WHEN DEMO-MAP-FUNCTION-GET                                 SDEMO10P
009400         PERFORM SCREEN10-READ THRU                               SDEMO10P
009500                 SCREEN10-READ-EXIT                               SDEMO10P
009600       WHEN DEMO-MAP-FUNCTION-PUT                                 SDEMO10P
009700         PERFORM SCREEN10-BUILD-AND-SEND THRU                     SDEMO10P
009800                 SCREEN10-BUILD-AND-SEND-EXIT                     SDEMO10P
009900       WHEN OTHER                                                 SDEMO10P
010000         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      SDEMO10P
010100         MOVE '0002' TO ABEND-CODE                                SDEMO10P
010200         MOVE SPACES TO ABEND-REASON                              SDEMO10P
010300         COPY CABENDPO.                                           SDEMO10P
010400     END-EVALUATE.                                                SDEMO10P
010500                                                                  SDEMO10P
010600* Call the appropriate routine to handle the business logic       SDEMO10P
010700     IF DEMO-MAP-FUNCTION-GET                                     SDEMO10P
010800        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             SDEMO10P
010900                       COMMAREA(WS-DEMO-DATA)                     SDEMO10P
011000                       LENGTH(LENGTH OF WS-DEMO-DATA)             SDEMO10P
011100        END-EXEC                                                  SDEMO10P
011200     END-IF.                                                      SDEMO10P
011300                                                                  SDEMO10P
011400***************************************************************** SDEMO10P
011500* Now we have to have finished and can return to our invoker.   * SDEMO10P
011600***************************************************************** SDEMO10P
011700* Now return to CICS                                              SDEMO10P
011800     MOVE WS-DEMO-DATA (1:WS-SAVED-EIBCALEN) TO                   SDEMO10P
011900          DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      SDEMO10P
012000     EXEC CICS                                                    SDEMO10P
012100          RETURN                                                  SDEMO10P
012200     END-EXEC.                                                    SDEMO10P
012300     GOBACK.                                                      SDEMO10P
012400                                                                  SDEMO10P
012500***************************************************************** SDEMO10P
012600* Screen processing for MDEMO10                                 * SDEMO10P
012700*---------------------------------------------------------------* SDEMO10P
012800* Retrieve data from screen and format it                       * SDEMO10P
012900***************************************************************** SDEMO10P
013000 SCREEN10-READ.                                                   SDEMO10P
013100     MOVE 'BDEMO10P' TO WS-BUSINESS-LOGIC-PGM.                    SDEMO10P
013200     IF DEMO-AID-CLEAR                                            SDEMO10P
013300        SET DEMO-AID-PFK03 TO TRUE                                SDEMO10P
013400        GO TO SCREEN10-READ-EXIT                                  SDEMO10P
013500     END-IF.                                                      SDEMO10P
013600     IF DEMO-LAST-MAPSET IS EQUAL TO SPACES                       SDEMO10P
013700        GO TO SCREEN10-READ-EXIT                                  SDEMO10P
013800     END-IF.                                                      SDEMO10P
013900     IF DEMO-ENV-CICS                                             SDEMO10P
014000        GO TO SCREEN10-READ-CICS                                  SDEMO10P
014100     ELSE                                                         SDEMO10P
014200        GO TO SCREEN10-READ-INET                                  SDEMO10P
014300     END-IF.                                                      SDEMO10P
014400                                                                  SDEMO10P
014500 SCREEN10-READ-CICS.                                              SDEMO10P
014600     IF DEMO-HELP-INACTIVE                                        SDEMO10P
014700        EXEC CICS RECEIVE MAP('DEMO10A')                          SDEMO10P
014800                          MAPSET('MDEMO10')                       SDEMO10P
014900        END-EXEC                                                  SDEMO10P
015000     ELSE                                                         SDEMO10P
015100        EXEC CICS RECEIVE MAP('HELP10A')                          SDEMO10P
015200                          MAPSET('MDEMO10')                       SDEMO10P
015300        END-EXEC                                                  SDEMO10P
015400        GO TO SCREEN10-READ-EXIT                                  SDEMO10P
015500     END-IF.                                                      SDEMO10P
015600                                                                  SDEMO10P
015700     IF SEL1L IN DEMO10AI IS EQUAL TO 0                           SDEMO10P
015800        MOVE LOW-VALUES TO DEMO-SCR10-SEL1                        SDEMO10P
015900     ELSE                                                         SDEMO10P
016000        MOVE SEL1I IN DEMO10AI TO DEMO-SCR10-SEL1                 SDEMO10P
016100        IF DEMO-SCR10-SEL1 IS EQUAL TO SPACES OR                  SDEMO10P
016200           DEMO-SCR10-SEL1 IS EQUAL TO ALL '_'                    SDEMO10P
016300           MOVE LOW-VALUES TO DEMO-SCR10-SEL1                     SDEMO10P
016400        END-IF                                                    SDEMO10P
016500     END-IF.                                                      SDEMO10P
016600                                                                  SDEMO10P
016700     IF SEL2L IN DEMO10AI IS EQUAL TO 0                           SDEMO10P
016800        MOVE LOW-VALUES TO DEMO-SCR10-SEL2                        SDEMO10P
016900     ELSE                                                         SDEMO10P
017000        MOVE SEL2I IN DEMO10AI TO DEMO-SCR10-SEL2                 SDEMO10P
017100        IF DEMO-SCR10-SEL2 IS EQUAL TO SPACES OR                  SDEMO10P
017200           DEMO-SCR10-SEL2 IS EQUAL TO ALL '_'                    SDEMO10P
017300           MOVE LOW-VALUES TO DEMO-SCR10-SEL2                     SDEMO10P
017400        END-IF                                                    SDEMO10P
017500     END-IF.                                                      SDEMO10P
017600                                                                  SDEMO10P
017700     IF SEL3L IN DEMO10AI IS EQUAL TO 0                           SDEMO10P
017800        MOVE LOW-VALUES TO DEMO-SCR10-SEL3                        SDEMO10P
017900     ELSE                                                         SDEMO10P
018000        MOVE SEL3I IN DEMO10AI TO DEMO-SCR10-SEL3                 SDEMO10P
018100        IF DEMO-SCR10-SEL3 IS EQUAL TO SPACES OR                  SDEMO10P
018200           DEMO-SCR10-SEL3 IS EQUAL TO ALL '_'                    SDEMO10P
018300           MOVE LOW-VALUES TO DEMO-SCR10-SEL3                     SDEMO10P
018400        END-IF                                                    SDEMO10P
018500     END-IF.                                                      SDEMO10P
018600                                                                  SDEMO10P
018700     IF SEL4L IN DEMO10AI IS EQUAL TO 0                           SDEMO10P
018800        MOVE LOW-VALUES TO DEMO-SCR10-SEL4                        SDEMO10P
018900     ELSE                                                         SDEMO10P
019000        MOVE SEL4I IN DEMO10AI TO DEMO-SCR10-SEL4                 SDEMO10P
019100        IF DEMO-SCR10-SEL4 IS EQUAL TO SPACES OR                  SDEMO10P
019200           DEMO-SCR10-SEL4 IS EQUAL TO ALL '_'                    SDEMO10P
019300           MOVE LOW-VALUES TO DEMO-SCR10-SEL4                     SDEMO10P
019400        END-IF                                                    SDEMO10P
019500     END-IF.                                                      SDEMO10P
019600                                                                  SDEMO10P
019700     GO TO SCREEN10-READ-EXIT.                                    SDEMO10P
019800                                                                  SDEMO10P
019900 SCREEN10-READ-INET.                                              SDEMO10P
020000     MOVE EXT-IP10-SEL1 TO DEMO-SCR10-SEL1.                       SDEMO10P
020100     MOVE EXT-IP10-SEL2 TO DEMO-SCR10-SEL2.                       SDEMO10P
020200     MOVE EXT-IP10-SEL3 TO DEMO-SCR10-SEL3.                       SDEMO10P
020300     MOVE EXT-IP10-SEL4 TO DEMO-SCR10-SEL4.                       SDEMO10P
020400     GO TO SCREEN10-READ-EXIT.                                    SDEMO10P
020500                                                                  SDEMO10P
020600 SCREEN10-READ-EXIT.                                              SDEMO10P
020700     EXIT.                                                        SDEMO10P
020800                                                                  SDEMO10P
020900***************************************************************** SDEMO10P
021000* Screen processing for SCREEN10 (DEMO10/HELP10)                * SDEMO10P
021100*---------------------------------------------------------------* SDEMO10P
021200* Build the output screen and send it                           * SDEMO10P
021300***************************************************************** SDEMO10P
021400 SCREEN10-BUILD-AND-SEND.                                         SDEMO10P
021500* Clear map area, get date & time and move to the map             SDEMO10P
021600     MOVE LOW-VALUES TO DEMO10AO.                                 SDEMO10P
021700     MOVE EIBTIME TO DD-TIME-INPUT-N.                             SDEMO10P
021800     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      SDEMO10P
021900     SET DDI-YYDDD TO TRUE.                                       SDEMO10P
022000     SET DDO-DD-MMM-YYYY TO TRUE.                                 SDEMO10P
022100     PERFORM CALL-DATECONV THRU                                   SDEMO10P
022200             CALL-DATECONV-EXIT.                                  SDEMO10P
022300* Ensure the last map fields are correct                          SDEMO10P
022400     IF DEMO-HELP-ACTIVE                                          SDEMO10P
022500        MOVE 'MDEMO10' TO DEMO-LAST-MAPSET                        SDEMO10P
022600        MOVE 'HELP10A' TO DEMO-LAST-MAP                           SDEMO10P
022700     ELSE                                                         SDEMO10P
022800        MOVE 'MDEMO10' TO DEMO-LAST-MAPSET                        SDEMO10P
022900        MOVE 'DEMO10A' TO DEMO-LAST-MAP                           SDEMO10P
023000     END-IF.                                                      SDEMO10P
023100     IF DEMO-ENV-CICS                                             SDEMO10P
023200        GO TO SCREEN10-BUILD-AND-SEND-CICS                        SDEMO10P
023300     ELSE                                                         SDEMO10P
023400        GO TO SCREEN10-BUILD-AND-SEND-INET                        SDEMO10P
023500     END-IF.                                                      SDEMO10P
023600                                                                  SDEMO10P
023700 SCREEN10-BUILD-AND-SEND-CICS.                                    SDEMO10P
023800     IF DEMO-LAST-MAP IS EQUAL TO 'DEMO10A'                       SDEMO10P
023900        GO TO DEMO10-BUILD-AND-SEND-CICS                          SDEMO10P
024000     END-IF.                                                      SDEMO10P
024100     IF DEMO-LAST-MAP IS EQUAL TO 'HELP10A'                       SDEMO10P
024200        GO TO HELP10-BUILD-AND-SEND-CICS                          SDEMO10P
024300     END-IF.                                                      SDEMO10P
024400     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                          SDEMO10P
024500     MOVE '0003' TO ABEND-CODE                                    SDEMO10P
024600     MOVE SPACES TO ABEND-REASON                                  SDEMO10P
024700     COPY CABENDPO.                                               SDEMO10P
024800     GOBACK.                                                      SDEMO10P
024900                                                                  SDEMO10P
025000 DEMO10-BUILD-AND-SEND-CICS.                                      SDEMO10P
025100     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==DEMO10AO==.        SDEMO10P
025200     MOVE WS-TRAN-ID TO TRANO IN DEMO10AO.                        SDEMO10P
025300     MOVE DD-TIME-OUTPUT TO TIMEO IN DEMO10AO.                    SDEMO10P
025400     MOVE DDO-DATA TO DATEO IN DEMO10AO.                          SDEMO10P
025500* Move in any error message                                       SDEMO10P
025600     MOVE DEMO-ERROR-MSG TO ERRMSGO IN DEMO10AO.                  SDEMO10P
025700* Move in screen specific fields                                  SDEMO10P
025800     MOVE -1 TO SEL1L IN DEMO10AI.                                SDEMO10P
025900     MOVE DEMO-SCR10-SEL1 TO SEL1O IN DEMO10AO.                   SDEMO10P
026000     MOVE DEMO-OPTN-DESC (1) TO OPT1O IN DEMO10AO.                SDEMO10P
026100     MOVE -1 TO SEL2L IN DEMO10AI.                                SDEMO10P
026200     MOVE DEMO-SCR10-SEL2 TO SEL2O IN DEMO10AO.                   SDEMO10P
026300     MOVE DEMO-OPTN-DESC (2) TO OPT2O IN DEMO10AO.                SDEMO10P
026400     MOVE -1 TO SEL3L IN DEMO10AI.                                SDEMO10P
026500     MOVE DEMO-SCR10-SEL3 TO SEL3O IN DEMO10AO.                   SDEMO10P
026600     MOVE DEMO-OPTN-DESC (3) TO OPT3O IN DEMO10AO.                SDEMO10P
026700     MOVE -1 TO SEL4L IN DEMO10AI.                                SDEMO10P
026800     MOVE DEMO-SCR10-SEL4 TO SEL4O IN DEMO10AO.                   SDEMO10P
026900     MOVE DEMO-OPTN-DESC (4) TO OPT4O IN DEMO10AO.                SDEMO10P
027000* Turn colour off if required                                     SDEMO10P
027100     IF COLOUR-OFF                                                SDEMO10P
027200        MOVE DFHGREEN TO TXT01C IN DEMO10AO                       SDEMO10P
027300        MOVE DFHGREEN TO SCRNC IN DEMO10AO                        SDEMO10P
027400        MOVE DFHGREEN TO HEAD1C IN DEMO10AO                       SDEMO10P
027500        MOVE DFHGREEN TO DATEC IN DEMO10AO                        SDEMO10P
027600        MOVE DFHGREEN TO TXT02C IN DEMO10AO                       SDEMO10P
027700        MOVE DFHGREEN TO TRANC IN DEMO10AO                        SDEMO10P
027800        MOVE DFHGREEN TO HEAD2C IN DEMO10AO                       SDEMO10P
027900        MOVE DFHGREEN TO TIMEC IN DEMO10AO                        SDEMO10P
028000        MOVE DFHGREEN TO TXT03C IN DEMO10AO                       SDEMO10P
028100        MOVE DFHGREEN TO TXT04C IN DEMO10AO                       SDEMO10P
028200        MOVE DFHGREEN TO SEL1C IN DEMO10AO                        SDEMO10P
028300        MOVE DFHGREEN TO OPT1C IN DEMO10AO                        SDEMO10P
028400        MOVE DFHGREEN TO SEL2C IN DEMO10AO                        SDEMO10P
028500        MOVE DFHGREEN TO OPT2C IN DEMO10AO                        SDEMO10P
028600        MOVE DFHGREEN TO SEL3C IN DEMO10AO                        SDEMO10P
028700        MOVE DFHGREEN TO OPT3C IN DEMO10AO                        SDEMO10P
028800        MOVE DFHGREEN TO SEL4C IN DEMO10AO                        SDEMO10P
028900        MOVE DFHGREEN TO OPT4C IN DEMO10AO                        SDEMO10P
029000        MOVE DFHGREEN TO TXT05C IN DEMO10AO                       SDEMO10P
029100        MOVE DFHGREEN TO TXT06C IN DEMO10AO                       SDEMO10P
029200        MOVE DFHGREEN TO TXT07C IN DEMO10AO                       SDEMO10P
029300        MOVE DFHGREEN TO TXT08C IN DEMO10AO                       SDEMO10P
029400        MOVE DFHGREEN TO TXT09C IN DEMO10AO                       SDEMO10P
029500        MOVE DFHGREEN TO TXT10C IN DEMO10AO                       SDEMO10P
029600        MOVE DFHGREEN TO TXT11C IN DEMO10AO                       SDEMO10P
029700        MOVE DFHGREEN TO ERRMSGC IN DEMO10AO                      SDEMO10P
029800        MOVE DFHGREEN TO TXT12C IN DEMO10AO                       SDEMO10P
029900     END-IF.                                                      SDEMO10P
030000                                                                  SDEMO10P
030100     EXEC CICS SEND MAP('DEMO10A')                                SDEMO10P
030200                    MAPSET('MDEMO10')                             SDEMO10P
030300                    ERASE                                         SDEMO10P
030400                    FREEKB                                        SDEMO10P
030500     END-EXEC.                                                    SDEMO10P
030600     GO TO SCREEN10-BUILD-AND-SEND-EXIT.                          SDEMO10P
030700                                                                  SDEMO10P
030800 HELP10-BUILD-AND-SEND-CICS.                                      SDEMO10P
030900     COPY CSCRNHP2 REPLACING ==:OPTN:== BY ==DEMO==               SDEMO10P
031000                             ==<<SCRN>>== BY ==HELP10AO==.        SDEMO10P
031100                                                                  SDEMO10P
031200     EXEC CICS SEND MAP('HELP10A')                                SDEMO10P
031300                    MAPSET('MDEMO10')                             SDEMO10P
031400                    ERASE                                         SDEMO10P
031500                    FREEKB                                        SDEMO10P
031600     END-EXEC.                                                    SDEMO10P
031700     GO TO SCREEN10-BUILD-AND-SEND-EXIT.                          SDEMO10P
031800                                                                  SDEMO10P
031900                                                                  SDEMO10P
032000 SCREEN10-BUILD-AND-SEND-INET.                                    SDEMO10P
032100     MOVE SPACES TO EXT-OP-DATA.                                  SDEMO10P
032200     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              SDEMO10P
032300     MOVE DDO-DATA TO EXT-OP-DATE.                                SDEMO10P
032400     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          SDEMO10P
032500     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         SDEMO10P
032600     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          SDEMO10P
032700     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          SDEMO10P
032800* Move in screen name                                             SDEMO10P
032900     MOVE 'DEMO10' TO EXT-OP-SCREEN.                              SDEMO10P
033000* Move in any error message                                       SDEMO10P
033100     MOVE DEMO-ERROR-MSG TO EXT-OP-ERR-MSG.                       SDEMO10P
033200* Move in screen specific fields                                  SDEMO10P
033300     MOVE DEMO-SCR10-SEL1 TO EXT-OP10-SEL1.                       SDEMO10P
033400     MOVE DEMO-OPTN-DESC (1) TO EXT-OP10-TXT1.                    SDEMO10P
033500     MOVE DEMO-SCR10-SEL2 TO EXT-OP10-SEL2.                       SDEMO10P
033600     MOVE DEMO-OPTN-DESC (2) TO EXT-OP10-TXT2.                    SDEMO10P
033700     MOVE DEMO-SCR10-SEL3 TO EXT-OP10-SEL3.                       SDEMO10P
033800     MOVE DEMO-OPTN-DESC (3) TO EXT-OP10-TXT3.                    SDEMO10P
033900     MOVE DEMO-SCR10-SEL3 TO EXT-OP10-SEL4.                       SDEMO10P
034000     MOVE DEMO-OPTN-DESC (4) TO EXT-OP10-TXT4.                    SDEMO10P
034100     GO TO SCREEN10-BUILD-AND-SEND-EXIT.                          SDEMO10P
034200                                                                  SDEMO10P
034300 SCREEN10-BUILD-AND-SEND-EXIT.                                    SDEMO10P
034400     EXIT.                                                        SDEMO10P
034500                                                                  SDEMO10P
034600***************************************************************** SDEMO10P
034700* Call common routine to perform date conversions               * SDEMO10P
034800***************************************************************** SDEMO10P
034900 CALL-DATECONV.                                                   SDEMO10P
035000     MOVE DEMO-ENV TO DD-ENV.                                     SDEMO10P
035100     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           SDEMO10P
035200     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            SDEMO10P
035300 CALL-DATECONV-EXIT.                                              SDEMO10P
035400     EXIT.                                                        SDEMO10P
035500                                                                  SDEMO10P
