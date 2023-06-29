000100***************************************************************** SDEMO99P
000200*                                                               * SDEMO99P
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * SDEMO99P
000400*   This demonstration program is provided for use by users     * SDEMO99P
000500*   of Micro Focus products and may be used, modified and       * SDEMO99P
000600*   distributed as part of your application provided that       * SDEMO99P
000700*   you properly acknowledge the copyright of Micro Focus       * SDEMO99P
000800*   in this material.                                           * SDEMO99P
000900*                                                               * SDEMO99P
001000***************************************************************** SDEMO99P
001100                                                                  SDEMO99P
001200***************************************************************** SDEMO99P
001300* Program:     SDEMO99P.CBL (CICS Version)                      * SDEMO99P
001400* Layer:       Screen handling                                  * SDEMO99P
001500* Function:    Terminate the pseudo conversation                * SDEMO99P
001600***************************************************************** SDEMO99P
001700                                                                  SDEMO99P
001800 IDENTIFICATION DIVISION.                                         SDEMO99P
001900 PROGRAM-ID.                                                      SDEMO99P
002000     SDEMO99P.                                                    SDEMO99P
002100 DATE-WRITTEN.                                                    SDEMO99P
002200     September 2002.                                              SDEMO99P
002300 DATE-COMPILED.                                                   SDEMO99P
002400     Today.                                                       SDEMO99P
002500                                                                  SDEMO99P
002600 ENVIRONMENT DIVISION.                                            SDEMO99P
002700                                                                  SDEMO99P
002800 DATA DIVISION.                                                   SDEMO99P
002900 WORKING-STORAGE SECTION.                                         SDEMO99P
003000 01  WS-MISC-STORAGE.                                             SDEMO99P
003100   05  WS-PROGRAM-ID                         PIC X(8)             SDEMO99P
003200       VALUE 'SDEMO99P'.                                          SDEMO99P
003300   05  WS-TRAN-ID                            PIC X(4).            SDEMO99P
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             SDEMO99P
003500       VALUE SPACES.                                              SDEMO99P
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             SDEMO99P
003700       VALUE 'UNKNOWN'.                                           SDEMO99P
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      SDEMO99P
003900                                                                  SDEMO99P
004000 01  MAPAREA                                 PIC X(2048).         SDEMO99P
004100 COPY MDEMO99.                                                    SDEMO99P
004200                                                                  SDEMO99P
004300 01  WS-TIME-DATE-WORK-AREA.                                      SDEMO99P
004400 COPY CDATED.                                                     SDEMO99P
004500                                                                  SDEMO99P
004600 01  WS-PRD-DATA.                                                 SDEMO99P
004700 COPY CDEMODAT.                                                   SDEMO99P
004800                                                                  SDEMO99P
004900 COPY CSCRNHDD.                                                   SDEMO99P
005000                                                                  SDEMO99P
005100 COPY DFHAID.                                                     SDEMO99P
005200                                                                  SDEMO99P
005300 COPY DFHBMSCA.                                                   SDEMO99P
005400                                                                  SDEMO99P
005500 COPY CABENDD.                                                    SDEMO99P
005600                                                                  SDEMO99P
005700 LINKAGE SECTION.                                                 SDEMO99P
005800 01  DFHCOMMAREA.                                                 SDEMO99P
005900 COPY CDEMOEXT.                                                   SDEMO99P
006000                                                                  SDEMO99P
006100 PROCEDURE DIVISION.                                              SDEMO99P
006200***************************************************************** SDEMO99P
006300* Store our transaction-id                                      * SDEMO99P
006400***************************************************************** SDEMO99P
006500     MOVE EIBTRNID TO WS-TRAN-ID.                                 SDEMO99P
006600                                                                  SDEMO99P
006700***************************************************************** SDEMO99P
006800* Store passed data or abend if there wasn't any                * SDEMO99P
006900***************************************************************** SDEMO99P
007000     IF EIBCALEN IS EQUAL TO 0                                    SDEMO99P
007100        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       SDEMO99P
007200        MOVE '0001' TO ABEND-CODE                                 SDEMO99P
007300        MOVE SPACES TO ABEND-REASON                               SDEMO99P
007400        COPY CABENDPO.                                            SDEMO99P
007500     ELSE                                                         SDEMO99P
007600        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        SDEMO99P
007700        MOVE LOW-VALUES TO WS-PRD-DATA                            SDEMO99P
007800        MOVE DFHCOMMAREA (1:EIBCALEN) TO WS-PRD-DATA (1:EIBCALEN) SDEMO99P
007900     END-IF.                                                      SDEMO99P
008000                                                                  SDEMO99P
008100***************************************************************** SDEMO99P
008200* This is the main process                                      * SDEMO99P
008300***************************************************************** SDEMO99P
008400                                                                  SDEMO99P
008500***************************************************************** SDEMO99P
008600* Determine what we have to do (read from or send to screen)    * SDEMO99P
008700***************************************************************** SDEMO99P
008800     MOVE LOW-VALUE TO MAPAREA.                                   SDEMO99P
008900     EVALUATE TRUE                                                SDEMO99P
009000       WHEN DEMO-MAP-FUNCTION-GET                                 SDEMO99P
009100         PERFORM DEMO99-READ THRU                                 SDEMO99P
009200                 DEMO99-READ-EXIT                                 SDEMO99P
009300       WHEN DEMO-MAP-FUNCTION-PUT                                 SDEMO99P
009400         PERFORM DEMO99-BUILD-AND-SEND THRU                       SDEMO99P
009500                 DEMO99-BUILD-AND-SEND-EXIT                       SDEMO99P
009600       WHEN OTHER                                                 SDEMO99P
009700         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      SDEMO99P
009800         MOVE '0002' TO ABEND-CODE                                SDEMO99P
009900         MOVE SPACES TO ABEND-REASON                              SDEMO99P
010000         COPY CABENDPO.                                           SDEMO99P
010100     END-EVALUATE.                                                SDEMO99P
010200                                                                  SDEMO99P
010300* Call the appropriate routine to handle the business logic       SDEMO99P
010400     IF DEMO-MAP-FUNCTION-GET                                     SDEMO99P
010500        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             SDEMO99P
010600                       COMMAREA(WS-PRD-DATA)                      SDEMO99P
010700                       LENGTH(LENGTH OF WS-PRD-DATA)              SDEMO99P
010800        END-EXEC                                                  SDEMO99P
010900     END-IF.                                                      SDEMO99P
011000                                                                  SDEMO99P
011100***************************************************************** SDEMO99P
011200* Now we have to have finished and can return to our invoker.   * SDEMO99P
011300***************************************************************** SDEMO99P
011400* Now return to CICS                                              SDEMO99P
011500     MOVE WS-PRD-DATA (1:WS-SAVED-EIBCALEN) TO                    SDEMO99P
011600          DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      SDEMO99P
011700     EXEC CICS                                                    SDEMO99P
011800          RETURN                                                  SDEMO99P
011900     END-EXEC.                                                    SDEMO99P
012000     GOBACK.                                                      SDEMO99P
012100                                                                  SDEMO99P
012200***************************************************************** SDEMO99P
012300* Screen processing for MDEMO99                                 * SDEMO99P
012400*---------------------------------------------------------------* SDEMO99P
012500* Retrieve data from screen and format it                       * SDEMO99P
012600***************************************************************** SDEMO99P
012700 DEMO99-READ.                                                     SDEMO99P
012800     MOVE 'BDEMO99P' TO WS-BUSINESS-LOGIC-PGM.                    SDEMO99P
012900     IF DEMO-AID-CLEAR                                            SDEMO99P
013000        SET DEMO-AID-PFK03 TO TRUE                                SDEMO99P
013100        GO TO DEMO99-READ-EXIT                                    SDEMO99P
013200     END-IF.                                                      SDEMO99P
013300     IF DEMO-LAST-MAPSET IS EQUAL TO SPACES                       SDEMO99P
013400        GO TO DEMO99-READ-EXIT                                    SDEMO99P
013500     END-IF.                                                      SDEMO99P
013600     IF DEMO-ENV-CICS                                             SDEMO99P
013700        GO TO DEMO99-READ-CICS                                    SDEMO99P
013800     ELSE                                                         SDEMO99P
013900        GO TO DEMO99-READ-INET                                    SDEMO99P
014000     END-IF.                                                      SDEMO99P
014100                                                                  SDEMO99P
014200 DEMO99-READ-CICS.                                                SDEMO99P
014300     GO TO DEMO99-READ-EXIT.                                      SDEMO99P
014400                                                                  SDEMO99P
014500 DEMO99-READ-INET.                                                SDEMO99P
014600     GO TO DEMO99-READ-EXIT.                                      SDEMO99P
014700                                                                  SDEMO99P
014800 DEMO99-READ-EXIT.                                                SDEMO99P
014900     EXIT.                                                        SDEMO99P
015000                                                                  SDEMO99P
015100***************************************************************** SDEMO99P
015200* Screen processing for MDEMO99                                 * SDEMO99P
015300*---------------------------------------------------------------* SDEMO99P
015400* Build the output screen and send it                           * SDEMO99P
015500***************************************************************** SDEMO99P
015600 DEMO99-BUILD-AND-SEND.                                           SDEMO99P
015700* Clear map area, get date & time and move to the map             SDEMO99P
015800     MOVE LOW-VALUES TO DEMO99AO.                                 SDEMO99P
015900     MOVE EIBTIME TO DD-TIME-INPUT-N.                             SDEMO99P
016000     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      SDEMO99P
016100     SET DDI-YYDDD TO TRUE.                                       SDEMO99P
016200     SET DDO-DD-MMM-YYYY TO TRUE.                                 SDEMO99P
016300     PERFORM CALL-DATECONV THRU                                   SDEMO99P
016400             CALL-DATECONV-EXIT.                                  SDEMO99P
016500* Ensure the last map fields are correct                          SDEMO99P
016600     MOVE 'MDEMO99' TO DEMO-LAST-MAPSET.                          SDEMO99P
016700     MOVE 'DEMO99A' TO DEMO-LAST-MAP.                             SDEMO99P
016800     IF DEMO-ENV-CICS                                             SDEMO99P
016900        GO TO DEMO99-BUILD-AND-SEND-CICS                          SDEMO99P
017000     ELSE                                                         SDEMO99P
017100        GO TO DEMO99-BUILD-AND-SEND-INET                          SDEMO99P
017200     END-IF.                                                      SDEMO99P
017300                                                                  SDEMO99P
017400 DEMO99-BUILD-AND-SEND-CICS.                                      SDEMO99P
017500     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==DEMO99AO==.        SDEMO99P
017600     MOVE WS-TRAN-ID TO TRANO IN DEMO99AO.                        SDEMO99P
017700     MOVE DD-TIME-OUTPUT TO TIMEO IN DEMO99AO.                    SDEMO99P
017800     MOVE DDO-DATA TO DATEO IN DEMO99AO.                          SDEMO99P
017900* Move in any error message                                       SDEMO99P
018000* Move in screen specific fields                                  SDEMO99P
018100     MOVE 'MDEMO99' TO DEMO-LAST-MAPSET.                          SDEMO99P
018200     MOVE 'DEMO99A' TO DEMO-LAST-MAP.                             SDEMO99P
018300* Turn colour off if required                                     SDEMO99P
018400     IF COLOUR-OFF                                                SDEMO99P
018500        MOVE DFHGREEN TO TXT01C IN DEMO99AO                       SDEMO99P
018600        MOVE DFHGREEN TO SCRNC IN DEMO99AO                        SDEMO99P
018700        MOVE DFHGREEN TO HEAD1C IN DEMO99AO                       SDEMO99P
018800        MOVE DFHGREEN TO DATEC IN DEMO99AO                        SDEMO99P
018900        MOVE DFHGREEN TO TXT02C IN DEMO99AO                       SDEMO99P
019000        MOVE DFHGREEN TO TRANC IN DEMO99AO                        SDEMO99P
019100        MOVE DFHGREEN TO HEAD2C IN DEMO99AO                       SDEMO99P
019200        MOVE DFHGREEN TO TIMEC IN DEMO99AO                        SDEMO99P
019300        MOVE DFHGREEN TO TXT03C IN DEMO99AO                       SDEMO99P
019400        MOVE DFHGREEN TO TXT04C IN DEMO99AO                       SDEMO99P
019500     END-IF.                                                      SDEMO99P
019600     EXEC CICS SEND MAP('DEMO99A')                                SDEMO99P
019700                    MAPSET('MDEMO99')                             SDEMO99P
019800                    ERASE                                         SDEMO99P
019900                    FREEKB                                        SDEMO99P
020000     END-EXEC.                                                    SDEMO99P
020100     GO TO DEMO99-BUILD-AND-SEND-EXIT.                            SDEMO99P
020200                                                                  SDEMO99P
020300 DEMO99-BUILD-AND-SEND-INET.                                      SDEMO99P
020400     MOVE SPACES TO EXT-OP-DATA.                                  SDEMO99P
020500     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              SDEMO99P
020600     MOVE DDO-DATA TO EXT-OP-DATE.                                SDEMO99P
020700     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          SDEMO99P
020800     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         SDEMO99P
020900     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          SDEMO99P
021000     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          SDEMO99P
021100* Move in userid and any error message                            SDEMO99P
021200* Move in screen specific fields                                  SDEMO99P
021300* Move in screen name                                             SDEMO99P
021400     MOVE 'DEMO99' TO EXT-OP-SCREEN.                              SDEMO99P
021500     GO TO DEMO99-BUILD-AND-SEND-EXIT.                            SDEMO99P
021600                                                                  SDEMO99P
021700 DEMO99-BUILD-AND-SEND-EXIT.                                      SDEMO99P
021800     EXIT.                                                        SDEMO99P
021900                                                                  SDEMO99P
022000***************************************************************** SDEMO99P
022100* Call common routine to perform date conversions               * SDEMO99P
022200***************************************************************** SDEMO99P
022300 CALL-DATECONV.                                                   SDEMO99P
022400     MOVE DEMO-ENV TO DD-ENV.                                     SDEMO99P
022500     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           SDEMO99P
022600     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            SDEMO99P
022700 CALL-DATECONV-EXIT.                                              SDEMO99P
022800     EXIT.                                                        SDEMO99P
022900                                                                  SDEMO99P
