000100***************************************************************** sbank99p
000200*                                                               * sbank99p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * sbank99p
000400*   This demonstration program is provided for use by users     * sbank99p
000500*   of Micro Focus products and may be used, modified and       * sbank99p
000600*   distributed as part of your application provided that       * sbank99p
000700*   you properly acknowledge the copyright of Micro Focus       * sbank99p
000800*   in this material.                                           * sbank99p
000900*                                                               * sbank99p
001000***************************************************************** sbank99p
001100                                                                  sbank99p
001200***************************************************************** sbank99p
001300* Program:     SBANK99P.CBL (CICS Version)                      * sbank99p
001400* Layer:       Screen handling                                  * sbank99p
001500* Function:    Terminate the pseudo conversation                * sbank99p
001600***************************************************************** sbank99p
001700                                                                  sbank99p
001800 IDENTIFICATION DIVISION.                                         sbank99p
001900 PROGRAM-ID.                                                      sbank99p
002000     SBANK99P.                                                    sbank99p
002100 DATE-WRITTEN.                                                    sbank99p
002200     September 2002.                                              sbank99p
002300 DATE-COMPILED.                                                   sbank99p
002400     Today.                                                       sbank99p
002500                                                                  sbank99p
002600 ENVIRONMENT DIVISION.                                            sbank99p
002700                                                                  sbank99p
002800 DATA DIVISION.                                                   sbank99p
002900 WORKING-STORAGE SECTION.                                         sbank99p
003000 01  WS-MISC-STORAGE.                                             sbank99p
003100   05  WS-PROGRAM-ID                         PIC X(8)             sbank99p
003200       VALUE 'SBANK99P'.                                          sbank99p
003300   05  WS-TRAN-ID                            PIC X(4).            sbank99p
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             sbank99p
003500       VALUE SPACES.                                              sbank99p
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             sbank99p
003700       VALUE 'UNKNOWN'.                                           sbank99p
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      sbank99p
003900                                                                  sbank99p
004000 01  MAPAREA                                 PIC X(2048).         sbank99p
004100 COPY MBANK99.                                                    sbank99p
004200                                                                  sbank99p
004300 01  WS-TIME-DATE-WORK-AREA.                                      sbank99p
004400 COPY CDATED.                                                     sbank99p
004500                                                                  sbank99p
004600 01  WS-BANK-DATA-AREAS.                                          sbank99p
004700   05 WS-BANK-DATA.                                               sbank99p
004800 COPY CBANKDAT.                                                   sbank99p
004900   05  WS-BANK-EXT-DATA.                                          sbank99p
005000 COPY CBANKEXT.                                                   sbank99p
005100                                                                  sbank99p
005200 COPY CSCRNHDD.                                                   sbank99p
005300                                                                  sbank99p
005400 COPY CVERSND.                                                    sbank99p
005500                                                                  sbank99p
005600 COPY DFHAID.                                                     sbank99p
005700                                                                  sbank99p
005800 COPY DFHBMSCA.                                                   sbank99p
005900                                                                  sbank99p
006000 COPY CABENDD.                                                    sbank99p
006100                                                                  sbank99p
006200 LINKAGE SECTION.                                                 sbank99p
006300 01  DFHCOMMAREA.                                                 sbank99p
006400   05  FILLER                                PIC X(1)             sbank99p
006500       OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.             sbank99p
006600                                                                  sbank99p
006700 PROCEDURE DIVISION.                                              sbank99p
006800***************************************************************** sbank99p
006900* Write entry to log to show we have been invoked               * sbank99p
007000***************************************************************** sbank99p
007100     COPY CTRACE.                                                 sbank99p
007200                                                                  sbank99p
007300***************************************************************** sbank99p
007400* Store our transaction-id                                      * sbank99p
007500***************************************************************** sbank99p
007600     MOVE EIBTRNID TO WS-TRAN-ID.                                 sbank99p
007700                                                                  sbank99p
007800***************************************************************** sbank99p
007900* Store passed data or abend if there wasn't any                * sbank99p
008000***************************************************************** sbank99p
008100     IF EIBCALEN IS EQUAL TO 0                                    sbank99p
008200        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       sbank99p
008300        MOVE '0001' TO ABEND-CODE                                 sbank99p
008400        MOVE SPACES TO ABEND-REASON                               sbank99p
008500        COPY CABENDPO.                                            sbank99p
008600     ELSE                                                         sbank99p
008700        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        sbank99p
008800        MOVE LOW-VALUES TO WS-BANK-DATA                           sbank99p
008900        MOVE DFHCOMMAREA (1:EIBCALEN)                             sbank99p
009000          TO WS-BANK-DATA-AREAS (1:LENGTH OF WS-BANK-DATA-AREAS)  sbank99p
009100     END-IF.                                                      sbank99p
009200                                                                  sbank99p
009300***************************************************************** sbank99p
009400* This is the main process                                      * sbank99p
009500***************************************************************** sbank99p
009600                                                                  sbank99p
009700***************************************************************** sbank99p
009800* Determine what we have to do (read from or send to screen)    * sbank99p
009900***************************************************************** sbank99p
010000     MOVE LOW-VALUE TO MAPAREA.                                   sbank99p
010100     EVALUATE TRUE                                                sbank99p
010200       WHEN BANK-MAP-FUNCTION-GET                                 sbank99p
010300         PERFORM BANK99-READ THRU                                 sbank99p
010400                 BANK99-READ-EXIT                                 sbank99p
010500       WHEN BANK-MAP-FUNCTION-PUT                                 sbank99p
010600         PERFORM BANK99-BUILD-AND-SEND THRU                       sbank99p
010700                 BANK99-BUILD-AND-SEND-EXIT                       sbank99p
010800       WHEN OTHER                                                 sbank99p
010900         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      sbank99p
011000         MOVE '0002' TO ABEND-CODE                                sbank99p
011100         MOVE SPACES TO ABEND-REASON                              sbank99p
011200         COPY CABENDPO.                                           sbank99p
011300     END-EVALUATE.                                                sbank99p
011400                                                                  sbank99p
011500* Call the appropriate routine to handle the business logic       sbank99p
011600     IF BANK-MAP-FUNCTION-GET                                     sbank99p
011700        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             sbank99p
011800                       COMMAREA(WS-BANK-DATA)                     sbank99p
011900                       LENGTH(LENGTH OF WS-BANK-DATA)             sbank99p
012000        END-EXEC                                                  sbank99p
012100     END-IF.                                                      sbank99p
012200                                                                  sbank99p
012300***************************************************************** sbank99p
012400* Now we have to have finished and can return to our invoker.   * sbank99p
012500***************************************************************** sbank99p
012600* Now return to CICS                                              sbank99p
012700     MOVE WS-BANK-DATA-AREAS (1:LENGTH OF WS-BANK-DATA-AREAS)     sbank99p
012800       TO DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      sbank99p
012900     EXEC CICS                                                    sbank99p
013000          RETURN                                                  sbank99p
013100     END-EXEC.                                                    sbank99p
013200     GOBACK.                                                      sbank99p
013300                                                                  sbank99p
013400***************************************************************** sbank99p
013500* Screen processing for MBANK99                                 * sbank99p
013600*---------------------------------------------------------------* sbank99p
013700* Retrieve data from screen and format it                       * sbank99p
013800***************************************************************** sbank99p
013900 BANK99-READ.                                                     sbank99p
014000     MOVE 'BBANK99P' TO WS-BUSINESS-LOGIC-PGM.                    sbank99p
014100     IF BANK-AID-CLEAR                                            sbank99p
014200        SET BANK-AID-PFK03 TO TRUE                                sbank99p
014300        GO TO BANK99-READ-EXIT                                    sbank99p
014400     END-IF.                                                      sbank99p
014500     IF BANK-LAST-MAPSET IS EQUAL TO SPACES                       sbank99p
014600        GO TO BANK99-READ-EXIT                                    sbank99p
014700     END-IF.                                                      sbank99p
014800     IF BANK-ENV-CICS                                             sbank99p
014900        GO TO BANK99-READ-CICS                                    sbank99p
015000     ELSE                                                         sbank99p
015100        GO TO BANK99-READ-INET                                    sbank99p
015200     END-IF.                                                      sbank99p
015300                                                                  sbank99p
015400 BANK99-READ-CICS.                                                sbank99p
015500     GO TO BANK99-READ-EXIT.                                      sbank99p
015600                                                                  sbank99p
015700 BANK99-READ-INET.                                                sbank99p
015800     GO TO BANK99-READ-EXIT.                                      sbank99p
015900                                                                  sbank99p
016000 BANK99-READ-EXIT.                                                sbank99p
016100     EXIT.                                                        sbank99p
016200                                                                  sbank99p
016300***************************************************************** sbank99p
016400* Screen processing for MBANK99                                 * sbank99p
016500*---------------------------------------------------------------* sbank99p
016600* Build the output screen and send it                           * sbank99p
016700***************************************************************** sbank99p
016800 BANK99-BUILD-AND-SEND.                                           sbank99p
016900* Clear map area, get date & time and move to the map             sbank99p
017000     MOVE LOW-VALUES TO BANK99AO.                                 sbank99p
017100     MOVE EIBTIME TO DD-TIME-INPUT-N.                             sbank99p
017200     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      sbank99p
017300     SET DDI-YYDDD TO TRUE.                                       sbank99p
017400     SET DDO-DD-MMM-YYYY TO TRUE.                                 sbank99p
017500     PERFORM CALL-DATECONV THRU                                   sbank99p
017600             CALL-DATECONV-EXIT.                                  sbank99p
017700* Ensure the last map fields are correct                          sbank99p
017800     MOVE 'MBANK99' TO BANK-LAST-MAPSET.                          sbank99p
017900     MOVE 'BANK99A' TO BANK-LAST-MAP.                             sbank99p
018000     IF BANK-ENV-CICS                                             sbank99p
018100        GO TO BANK99-BUILD-AND-SEND-CICS                          sbank99p
018200     ELSE                                                         sbank99p
018300        GO TO BANK99-BUILD-AND-SEND-INET                          sbank99p
018400     END-IF.                                                      sbank99p
018500                                                                  sbank99p
018600 BANK99-BUILD-AND-SEND-CICS.                                      sbank99p
018700     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANK99AO==.        sbank99p
018800     COPY CVERSNP1 REPLACING ==<<SCRN>>== BY ==BANK99AO==.        sbank99p
018900     MOVE WS-TRAN-ID TO TRANO IN BANK99AO.                        sbank99p
019000     MOVE DD-TIME-OUTPUT TO TIMEO IN BANK99AO.                    sbank99p
019100     MOVE DDO-DATA TO DATEO IN BANK99AO.                          sbank99p
019200* Move in any error message                                       sbank99p
019300* Move in screen specific fields                                  sbank99p
019400     MOVE 'MBANK99' TO BANK-LAST-MAPSET.                          sbank99p
019500     MOVE 'BANK99A' TO BANK-LAST-MAP.                             sbank99p
019600* Turn colour off if required                                     sbank99p
019700     IF COLOUR-OFF                                                sbank99p
019800        MOVE DFHGREEN TO TXT01C IN BANK99AO                       sbank99p
019900        MOVE DFHGREEN TO SCRNC IN BANK99AO                        sbank99p
020000        MOVE DFHGREEN TO HEAD1C IN BANK99AO                       sbank99p
020100        MOVE DFHGREEN TO DATEC IN BANK99AO                        sbank99p
020200        MOVE DFHGREEN TO TXT02C IN BANK99AO                       sbank99p
020300        MOVE DFHGREEN TO TRANC IN BANK99AO                        sbank99p
020400        MOVE DFHGREEN TO HEAD2C IN BANK99AO                       sbank99p
020500        MOVE DFHGREEN TO TIMEC IN BANK99AO                        sbank99p
020600        MOVE DFHGREEN TO TXT03C IN BANK99AO                       sbank99p
020700        MOVE DFHGREEN TO TXT04C IN BANK99AO                       sbank99p
020800        MOVE DFHGREEN TO VERC IN BANK99AO                         sbank99p
020900     END-IF.                                                      sbank99p
021000     EXEC CICS SEND MAP('BANK99A')                                sbank99p
021100                    MAPSET('MBANK99')                             sbank99p
021200                    ERASE                                         sbank99p
021300                    FREEKB                                        sbank99p
021400     END-EXEC.                                                    sbank99p
021500     GO TO BANK99-BUILD-AND-SEND-EXIT.                            sbank99p
021600                                                                  sbank99p
021700 BANK99-BUILD-AND-SEND-INET.                                      sbank99p
021800     MOVE SPACES TO EXT-OP-DATA.                                  sbank99p
021900     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              sbank99p
022000     MOVE DDO-DATA TO EXT-OP-DATE.                                sbank99p
022100     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          sbank99p
022200     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         sbank99p
022300     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          sbank99p
022400     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          sbank99p
022500     CALL 'SVERSONP' USING SCREEN-TITLES.                         sbank99p
022600     MOVE VERSION TO EXT-OP-VERSION.                              sbank99p
022700* Move in userid and any error message                            sbank99p
022800* Move in screen specific fields                                  sbank99p
022900* Move in screen name                                             sbank99p
023000     MOVE 'BANK99' TO EXT-OP-SCREEN.                              sbank99p
023100     GO TO BANK99-BUILD-AND-SEND-EXIT.                            sbank99p
023200                                                                  sbank99p
023300 BANK99-BUILD-AND-SEND-EXIT.                                      sbank99p
023400     EXIT.                                                        sbank99p
023500                                                                  sbank99p
023600***************************************************************** sbank99p
023700* Call common routine to perform date conversions               * sbank99p
023800***************************************************************** sbank99p
023900 CALL-DATECONV.                                                   sbank99p
024000     MOVE BANK-ENV TO DD-ENV.                                     sbank99p
024100     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           sbank99p
024200     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            sbank99p
024300 CALL-DATECONV-EXIT.                                              sbank99p
024400     EXIT.                                                        sbank99p
024500                                                                  sbank99p
024600* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     sbank99p
