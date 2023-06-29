000100***************************************************************** sbank10p
000200*                                                               * sbank10p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * sbank10p
000400*   This demonstration program is provided for use by users     * sbank10p
000500*   of Micro Focus products and may be used, modified and       * sbank10p
000600*   distributed as part of your application provided that       * sbank10p
000700*   you properly acknowledge the copyright of Micro Focus       * sbank10p
000800*   in this material.                                           * sbank10p
000900*                                                               * sbank10p
001000***************************************************************** sbank10p
001100                                                                  sbank10p
001200***************************************************************** sbank10p
001300* Program:     SBANK10P.CBL (CICS Version)                      * sbank10p
001400* Layer:       Screen handling                                  * sbank10p
001500* Function:    Signon to system to identify user                * sbank10p
001600***************************************************************** sbank10p
001700                                                                  sbank10p
001800 IDENTIFICATION DIVISION.                                         sbank10p
001900 PROGRAM-ID.                                                      sbank10p
002000     SBANK10P.                                                    sbank10p
002100 DATE-WRITTEN.                                                    sbank10p
002200     September 2002.                                              sbank10p
002300 DATE-COMPILED.                                                   sbank10p
002400     Today.                                                       sbank10p
002500                                                                  sbank10p
002600 ENVIRONMENT DIVISION.                                            sbank10p
002700                                                                  sbank10p
002800 DATA DIVISION.                                                   sbank10p
002900 WORKING-STORAGE SECTION.                                         sbank10p
003000 01  WS-MISC-STORAGE.                                             sbank10p
003100   05  WS-PROGRAM-ID                         PIC X(8)             sbank10p
003200       VALUE 'SBANK10P'.                                          sbank10p
003300   05  WS-TRAN-ID                            PIC X(4).            sbank10p
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             sbank10p
003500       VALUE SPACES.                                              sbank10p
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             sbank10p
003700       VALUE 'UNKNOWN'.                                           sbank10p
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      sbank10p
         05  WS-VERSION                            PIC X(7).
003900                                                                  sbank10p
004000 01  MAPAREA                                 PIC X(2048).         sbank10p
004100 COPY MBANK10.                                                    sbank10p
004200                                                                  sbank10p
004300 01  WS-TIME-DATE-WORK-AREA.                                      sbank10p
004400 COPY CDATED.                                                     sbank10p
004500                                                                  sbank10p
004600 01  WS-BANK-DATA-AREAS.                                          sbank10p
004700   05  WS-BANK-DATA.                                              sbank10p
004800 COPY CBANKDAT.                                                   sbank10p
004900   05  WS-BANK-EXT-DATA.                                          sbank10p
005000 COPY CBANKEXT.                                                   sbank10p
005100                                                                  sbank10p
005200 COPY CSCRNHDD.                                                   sbank10p
005300                                                                  sbank10p
005400 COPY CVERSND.                                                    sbank10p
005500                                                                  sbank10p
005600 COPY DFHAID.                                                     sbank10p
005700                                                                  sbank10p
005800 COPY DFHBMSCA.                                                   sbank10p
005900                                                                  sbank10p
006000 COPY CABENDD.                                                    sbank10p
006100                                                                  sbank10p
006200 LINKAGE SECTION.                                                 sbank10p
006300 01  DFHCOMMAREA.                                                 sbank10p
006400   05  FILLER                                PIC X(1)             sbank10p
006500       OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.             sbank10p
006600                                                                  sbank10p
006700 PROCEDURE DIVISION.                                              sbank10p
006800***************************************************************** sbank10p
006900* Write entry to log to show we have been invoked               * sbank10p
007000***************************************************************** sbank10p
007100     COPY CTRACE.                                                 sbank10p
007200                                                                  sbank10p
007300***************************************************************** sbank10p
007400* Store our transaction-id                                      * sbank10p
007500***************************************************************** sbank10p
007600     MOVE EIBTRNID TO WS-TRAN-ID.                                 sbank10p
007700                                                                  sbank10p
007800***************************************************************** sbank10p
007900* Store passed data or abend if there wasn't any                * sbank10p
008000***************************************************************** sbank10p
008100     IF EIBCALEN IS EQUAL TO 0                                    sbank10p
008200        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       sbank10p
008300        MOVE '0001' TO ABEND-CODE                                 sbank10p
008400        MOVE SPACES TO ABEND-REASON                               sbank10p
008500        COPY CABENDPO.                                            sbank10p
008600     ELSE                                                         sbank10p
008700        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        sbank10p
008800        MOVE LOW-VALUES TO WS-BANK-DATA                           sbank10p
008900        MOVE DFHCOMMAREA (1:EIBCALEN)                             sbank10p
009000          TO WS-BANK-DATA-AREAS (1:LENGTH OF WS-BANK-DATA-AREAS)  sbank10p
009100     END-IF.                                                      sbank10p
009200                                                                  sbank10p
009300***************************************************************** sbank10p
009400* This is the main process                                      * sbank10p
009500***************************************************************** sbank10p
009600                                                                  sbank10p
009700***************************************************************** sbank10p
009800* Determine what we have to do (read from or send to screen)    * sbank10p
009900***************************************************************** sbank10p
010000     MOVE LOW-VALUE TO MAPAREA.                                   sbank10p
010100     EVALUATE TRUE                                                sbank10p
010200       WHEN BANK-MAP-FUNCTION-GET                                 sbank10p
010300         PERFORM SCREEN10-READ THRU                               sbank10p
010400                 SCREEN10-READ-EXIT                               sbank10p
010500       WHEN BANK-MAP-FUNCTION-PUT                                 sbank10p
010600         PERFORM SCREEN10-BUILD-AND-SEND THRU                     sbank10p
010700                 SCREEN10-BUILD-AND-SEND-EXIT                     sbank10p
010800       WHEN OTHER                                                 sbank10p
010900         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      sbank10p
011000         MOVE '0002' TO ABEND-CODE                                sbank10p
011100         MOVE SPACES TO ABEND-REASON                              sbank10p
011200         COPY CABENDPO.                                           sbank10p
011300     END-EVALUATE.                                                sbank10p
011400                                                                  sbank10p
011500* Call the appropriate routine to handle the business logic       sbank10p
011600     IF BANK-MAP-FUNCTION-GET                                     sbank10p
011700        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             sbank10p
011800                       COMMAREA(WS-BANK-DATA)                     sbank10p
011900                       LENGTH(LENGTH OF WS-BANK-DATA)             sbank10p
012000        END-EXEC                                                  sbank10p
012100     END-IF.                                                      sbank10p
012200                                                                  sbank10p
012300***************************************************************** sbank10p
012400* Now we have to have finished and can return to our invoker.   * sbank10p
012500***************************************************************** sbank10p
012600* Now return to CICS                                              sbank10p
012700     MOVE WS-BANK-DATA-AREAS (1:LENGTH OF WS-BANK-DATA-AREAS)     sbank10p
012800       TO DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      sbank10p
012900     EXEC CICS                                                    sbank10p
013000          RETURN                                                  sbank10p
013100     END-EXEC.                                                    sbank10p
013200     GOBACK.                                                      sbank10p
013300                                                                  sbank10p
013400***************************************************************** sbank10p
013500* Screen processing for MBANK10                                 * sbank10p
013600*---------------------------------------------------------------* sbank10p
013700* Retrieve data from screen and format it                       * sbank10p
013800***************************************************************** sbank10p
013900 SCREEN10-READ.                                                   sbank10p
014000     MOVE 'BBANK10P' TO WS-BUSINESS-LOGIC-PGM.                    sbank10p
014100     IF BANK-AID-CLEAR                                            sbank10p
014200        SET BANK-AID-PFK03 TO TRUE                                sbank10p
014300        GO TO SCREEN10-READ-EXIT                                  sbank10p
014400     END-IF.                                                      sbank10p
014500     IF BANK-LAST-MAPSET IS EQUAL TO SPACES                       sbank10p
014600        GO TO SCREEN10-READ-EXIT                                  sbank10p
014700     END-IF.                                                      sbank10p
014800     IF BANK-ENV-CICS                                             sbank10p
014900        GO TO SCREEN10-READ-CICS                                  sbank10p
015000     ELSE                                                         sbank10p
015100        GO TO SCREEN10-READ-INET                                  sbank10p
015200     END-IF.                                                      sbank10p
015300                                                                  sbank10p
015400 SCREEN10-READ-CICS.                                              sbank10p
015500     IF BANK-HELP-INACTIVE                                        sbank10p
015600        EXEC CICS RECEIVE MAP('BANK10A')                          sbank10p
015700                          MAPSET('MBANK10')                       sbank10p
015800        END-EXEC                                                  sbank10p
015900     ELSE                                                         sbank10p
016000        EXEC CICS RECEIVE MAP('HELP10A')                          sbank10p
016100                          MAPSET('MBANK10')                       sbank10p
016200        END-EXEC                                                  sbank10p
016300        GO TO SCREEN10-READ-EXIT                                  sbank10p
016400     END-IF.                                                      sbank10p
016500                                                                  sbank10p
016600     IF USERIDL IN BANK10AI IS EQUAL TO 0                         sbank10p
016700           MOVE LOW-VALUES TO BANK-SIGNON-ID                      sbank10p
016800     ELSE                                                         sbank10p
016900        MOVE USERIDI IN BANK10AI                                  sbank10p
017000          TO BANK-SIGNON-ID (1:USERIDL IN BANK10AI)               sbank10p
017100     END-IF.                                                      sbank10p
017200                                                                  sbank10p
017300     IF PSWDL IN BANK10AI IS EQUAL TO 0                           sbank10p
017400        MOVE LOW-VALUES TO BANK-PSWD                              sbank10p
017500     ELSE                                                         sbank10p
017600        MOVE PSWDI IN BANK10AI                                    sbank10p
017700          TO BANK-PSWD (1:PSWDL IN BANK10AI)                      sbank10p
017800     END-IF.                                                      sbank10p
017900                                                                  sbank10p
018000     GO TO SCREEN10-READ-EXIT.                                    sbank10p
018100                                                                  sbank10p
018200 SCREEN10-READ-INET.                                              sbank10p
018300     MOVE EXT-IP10-USERID TO BANK-SIGNON-ID.                      sbank10p
018400     MOVE EXT-IP10-PSWD TO BANK-PSWD.                             sbank10p
018500     GO TO SCREEN10-READ-EXIT.                                    sbank10p
018600                                                                  sbank10p
018700 SCREEN10-READ-EXIT.                                              sbank10p
018800     EXIT.                                                        sbank10p
018900                                                                  sbank10p
019000***************************************************************** sbank10p
019100* Screen processing for SCREEN10 (BANK10/HELP10)                * sbank10p
019200*---------------------------------------------------------------* sbank10p
019300* Build the output screen and send it                           * sbank10p
019400***************************************************************** sbank10p
019500 SCREEN10-BUILD-AND-SEND.                                         sbank10p
019600* Clear map area, get date & time and move to the map             sbank10p
019700     MOVE LOW-VALUES TO BANK10AO.                                 sbank10p
019800     MOVE EIBTIME TO DD-TIME-INPUT-N.                             sbank10p
019900     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      sbank10p
020000     SET DDI-YYDDD TO TRUE.                                       sbank10p
020100     SET DDO-DD-MMM-YYYY TO TRUE.                                 sbank10p
020200     PERFORM CALL-DATECONV THRU                                   sbank10p
020300             CALL-DATECONV-EXIT.                                  sbank10p
020400* Ensure the last map fields are correct                          sbank10p
020500     IF BANK-HELP-ACTIVE                                          sbank10p
020600        MOVE 'MBANK10' TO BANK-LAST-MAPSET                        sbank10p
020700        MOVE 'HELP10A' TO BANK-LAST-MAP                           sbank10p
020800     ELSE                                                         sbank10p
020900        MOVE 'MBANK10' TO BANK-LAST-MAPSET                        sbank10p
021000        MOVE 'BANK10A' TO BANK-LAST-MAP                           sbank10p
021100     END-IF.                                                      sbank10p
021200     IF BANK-ENV-CICS                                             sbank10p
021300        GO TO SCREEN10-BUILD-AND-SEND-CICS                        sbank10p
021400     ELSE                                                         sbank10p
021500        GO TO SCREEN10-BUILD-AND-SEND-INET                        sbank10p
021600     END-IF.                                                      sbank10p
021700                                                                  sbank10p
021800 SCREEN10-BUILD-AND-SEND-CICS.                                    sbank10p
021900     IF BANK-LAST-MAP IS EQUAL TO 'BANK10A'                       sbank10p
022000        GO TO BANK10-BUILD-AND-SEND-CICS                          sbank10p
022100     END-IF.                                                      sbank10p
022200     IF BANK-LAST-MAP IS EQUAL TO 'HELP10A'                       sbank10p
022300        GO TO HELP10-BUILD-AND-SEND-CICS                          sbank10p
022400     END-IF.                                                      sbank10p
022500     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                          sbank10p
022600     MOVE '0003' TO ABEND-CODE                                    sbank10p
022700     MOVE SPACES TO ABEND-REASON                                  sbank10p
022800     COPY CABENDPO.                                               sbank10p
022900     GOBACK.                                                      sbank10p
023000                                                                  sbank10p
023100 BANK10-BUILD-AND-SEND-CICS.                                      sbank10p
023200     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANK10AO==.        sbank10p
023300     COPY CVERSNP1 REPLACING ==<<SCRN>>== BY ==BANK10AO==.        sbank10p
023400     MOVE WS-TRAN-ID TO TRANO IN BANK10AO.                        sbank10p
023500     MOVE DD-TIME-OUTPUT TO TIMEO IN BANK10AO.                    sbank10p
023600     MOVE DDO-DATA TO DATEO IN BANK10AO.                          sbank10p
023700* Move in any error message                                       sbank10p
023800     MOVE BANK-ERROR-MSG TO ERRMSGO IN BANK10AO.                  sbank10p
023900* Move in screen specific fields                                  sbank10p
024000     MOVE -1 TO USERIDL IN BANK10AI.                              sbank10p
024100     MOVE BANK-SIGNON-ID TO USERIDO IN BANK10AO.                  sbank10p
024200     MOVE -1 TO PSWDL IN BANK10AI.                                sbank10p
024300     MOVE BANK-PSWD TO PSWDO IN BANK10AO.                         sbank10p
024400* Turn colour off if required                                     sbank10p
024500     IF COLOUR-OFF                                                sbank10p
024600        MOVE DFHGREEN TO TXT01C IN BANK10AO                       sbank10p
024700        MOVE DFHGREEN TO SCRNC IN BANK10AO                        sbank10p
024800        MOVE DFHGREEN TO HEAD1C IN BANK10AO                       sbank10p
024900        MOVE DFHGREEN TO DATEC IN BANK10AO                        sbank10p
025000        MOVE DFHGREEN TO TXT02C IN BANK10AO                       sbank10p
025100        MOVE DFHGREEN TO TRANC IN BANK10AO                        sbank10p
025200        MOVE DFHGREEN TO HEAD2C IN BANK10AO                       sbank10p
025300        MOVE DFHGREEN TO TIMEC IN BANK10AO                        sbank10p
025400        MOVE DFHGREEN TO TXT03C IN BANK10AO                       sbank10p
025500        MOVE DFHGREEN TO TXT04C IN BANK10AO                       sbank10p
025600        MOVE DFHGREEN TO TXT05C IN BANK10AO                       sbank10p
025700        MOVE DFHGREEN TO TXT06C IN BANK10AO                       sbank10p
025800        MOVE DFHGREEN TO USERIDC IN BANK10AO                      sbank10p
025900        MOVE DFHGREEN TO TXT07C IN BANK10AO                       sbank10p
026000        MOVE DFHGREEN TO PSWDC IN BANK10AO                        sbank10p
026100        MOVE DFHGREEN TO ERRMSGC IN BANK10AO                      sbank10p
026200        MOVE DFHGREEN TO TXT08C IN BANK10AO                       sbank10p
026300        MOVE DFHGREEN TO VERC IN BANK10AO                         sbank10p
026400     END-IF.                                                      sbank10p
026500                                                                  sbank10p
026600     EXEC CICS SEND MAP('BANK10A')                                sbank10p
026700                    MAPSET('MBANK10')                             sbank10p
026800                    ERASE                                         sbank10p
026900                    FREEKB                                        sbank10p
027000     END-EXEC.                                                    sbank10p
027100     GO TO SCREEN10-BUILD-AND-SEND-EXIT.                          sbank10p
027200                                                                  sbank10p
027300 HELP10-BUILD-AND-SEND-CICS.                                      sbank10p
027400     COPY CSCRNHP2 REPLACING ==:OPTN:== BY ==BANK==               sbank10p
027500                             ==<<SCRN>>== BY ==HELP10AO==.        sbank10p
027600                                                                  sbank10p
027700     EXEC CICS SEND MAP('HELP10A')                                sbank10p
027800                    MAPSET('MBANK10')                             sbank10p
027900                    ERASE                                         sbank10p
028000                    FREEKB                                        sbank10p
028100     END-EXEC.                                                    sbank10p
028200     GO TO SCREEN10-BUILD-AND-SEND-EXIT.                          sbank10p
028300                                                                  sbank10p
028400                                                                  sbank10p
028500 SCREEN10-BUILD-AND-SEND-INET.                                    sbank10p
028600     MOVE SPACES TO EXT-OP-DATA.                                  sbank10p
028700     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              sbank10p
028800     MOVE DDO-DATA TO EXT-OP-DATE.                                sbank10p
028900     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          sbank10p
029000     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         sbank10p
029100     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          sbank10p
029200     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          sbank10p
029300     CALL 'SVERSONP' USING SCREEN-TITLES.                         sbank10p
029400     MOVE VERSION TO EXT-OP-VERSION.                              sbank10p
029500* Move in screen name                                             sbank10p
029600     MOVE 'BANK10' TO EXT-OP-SCREEN.                              sbank10p
029700* Move in userid and any error message                            sbank10p
029800     MOVE BANK-ERROR-MSG TO EXT-OP-ERR-MSG.                       sbank10p
029900     MOVE BANK-SIGNON-ID TO EXT-OP-USERID.                        sbank10p
030000     MOVE BANK-USERID-NAME TO EXT-OP-NAME.                        sbank10p
030100* Move in screen specific fields                                  sbank10p
030200     MOVE BANK-PSWD TO EXT-OP10-PSWD.                             sbank10p
030300     GO TO SCREEN10-BUILD-AND-SEND-EXIT.                          sbank10p
030400                                                                  sbank10p
030500 SCREEN10-BUILD-AND-SEND-EXIT.                                    sbank10p
030600     EXIT.                                                        sbank10p
030700                                                                  sbank10p
030800***************************************************************** sbank10p
030900* Call common routine to perform date conversions               * sbank10p
031000***************************************************************** sbank10p
031100 CALL-DATECONV.                                                   sbank10p
031200     MOVE BANK-ENV TO DD-ENV.                                     sbank10p
031300     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           sbank10p
031400     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            sbank10p
031500 CALL-DATECONV-EXIT.                                              sbank10p
031600     EXIT.                                                        sbank10p
031700                                                                  sbank10p
031800* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     sbank10p
