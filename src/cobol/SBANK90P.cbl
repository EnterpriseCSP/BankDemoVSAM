000100***************************************************************** sbank90p
000200*                                                               * sbank90p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * sbank90p
000400*   This demonstration program is provided for use by users     * sbank90p
000500*   of Micro Focus products and may be used, modified and       * sbank90p
000600*   distributed as part of your application provided that       * sbank90p
000700*   you properly acknowledge the copyright of Micro Focus       * sbank90p
000800*   in this material.                                           * sbank90p
000900*                                                               * sbank90p
001000***************************************************************** sbank90p
001100                                                                  sbank90p
001200***************************************************************** sbank90p
001300* Program:     SBANK90P.CBL (CICS Version)                      * sbank90p
001400* Layer:       Screen handling                                  * sbank90p
001500* Function:    Display request for informtion                   * sbank90p
001600***************************************************************** sbank90p
001700                                                                  sbank90p
001800 IDENTIFICATION DIVISION.                                         sbank90p
001900 PROGRAM-ID.                                                      sbank90p
002000     SBANK90P.                                                    sbank90p
002100 DATE-WRITTEN.                                                    sbank90p
002200     September 2002.                                              sbank90p
002300 DATE-COMPILED.                                                   sbank90p
002400     Today.                                                       sbank90p
002500                                                                  sbank90p
002600 ENVIRONMENT DIVISION.                                            sbank90p
002700                                                                  sbank90p
002800 DATA DIVISION.                                                   sbank90p
002900 WORKING-STORAGE SECTION.                                         sbank90p
003000 01  WS-MISC-STORAGE.                                             sbank90p
003100   05  WS-PROGRAM-ID                         PIC X(8)             sbank90p
003200       VALUE 'SBANK90P'.                                          sbank90p
003300   05  WS-TRAN-ID                            PIC X(4).            sbank90p
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             sbank90p
003500       VALUE SPACES.                                              sbank90p
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             sbank90p
003700       VALUE 'UNKNOWN'.                                           sbank90p
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      sbank90p
         05  WS-VERSION                            PIC X(7).
                                                                        sbank90p
003900                                                                  
004000 01  MAPAREA                                 PIC X(2048).         sbank90p
004100 COPY MBANK90.                                                    sbank90p
004200                                                                  sbank90p
004300 01  WS-TIME-DATE-WORK-AREA.                                      sbank90p
004400 COPY CDATED.                                                     sbank90p
004500                                                                  sbank90p
004600 01  WS-BANK-DATA-AREAS.                                          sbank90p
004700   05  WS-BANK-DATA.                                              sbank90p
004800 COPY CBANKDAT.                                                   sbank90p
004900   05  WS-BANK-EXT-DATA.                                          sbank90p
005000 COPY CBANKEXT.                                                   sbank90p
005100                                                                  sbank90p
005200 COPY CSCRNHDD.                                                   sbank90p
005300                                                                  sbank90p
005400 COPY CVERSND.                                                    sbank90p
005500                                                                  sbank90p
005600 COPY DFHAID.                                                     sbank90p
005700                                                                  sbank90p
005800 COPY DFHBMSCA.                                                   sbank90p
005900                                                                  sbank90p
006000 COPY CABENDD.                                                    sbank90p
006100                                                                  sbank90p
006200 LINKAGE SECTION.                                                 sbank90p
006300 01  DFHCOMMAREA.                                                 sbank90p
006400   05  FILLER                                PIC X(1)             sbank90p
006500       OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.             sbank90p
006600                                                                  sbank90p
006700 PROCEDURE DIVISION.                                              sbank90p
006800***************************************************************** sbank90p
006900* Write entry to log to show we have been invoked               * sbank90p
007000***************************************************************** sbank90p
007100     COPY CTRACE.                                                 sbank90p
007200                                                                  sbank90p
007300***************************************************************** sbank90p
007400* Store our transaction-id                                      * sbank90p
007500***************************************************************** sbank90p
007600     MOVE EIBTRNID TO WS-TRAN-ID.                                 sbank90p
007700                                                                  sbank90p
007800***************************************************************** sbank90p
007900* Store passed data or abend if there wasn't any                * sbank90p
008000***************************************************************** sbank90p
008100     IF EIBCALEN IS EQUAL TO 0                                    sbank90p
008200        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       sbank90p
008300        MOVE '0001' TO ABEND-CODE                                 sbank90p
008400        MOVE SPACES TO ABEND-REASON                               sbank90p
008500        COPY CABENDPO.                                            sbank90p
008600     ELSE                                                         sbank90p
008700        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        sbank90p
008800        MOVE LOW-VALUES TO WS-BANK-DATA                           sbank90p
008900        MOVE DFHCOMMAREA (1:EIBCALEN)                             sbank90p
009000          TO WS-BANK-DATA-AREAS (1:LENGTH OF WS-BANK-DATA-AREAS)  sbank90p
009100     END-IF.                                                      sbank90p
009200                                                                  sbank90p
009300***************************************************************** sbank90p
009400* This is the main process                                      * sbank90p
009500***************************************************************** sbank90p
009600                                                                  sbank90p
009700***************************************************************** sbank90p
009800* Determine what we have to do (read from or send to screen)    * sbank90p
009900***************************************************************** sbank90p
010000     MOVE LOW-VALUE TO MAPAREA.                                   sbank90p
010100     EVALUATE TRUE                                                sbank90p
010200       WHEN BANK-MAP-FUNCTION-GET                                 sbank90p
010300         PERFORM SCREEN90-READ THRU                               sbank90p
010400                 SCREEN90-READ-EXIT                               sbank90p
010500       WHEN BANK-MAP-FUNCTION-PUT                                 sbank90p
010600         PERFORM SCREEN90-BUILD-AND-SEND THRU                     sbank90p
010700                 SCREEN90-BUILD-AND-SEND-EXIT                     sbank90p
010800       WHEN OTHER                                                 sbank90p
010900         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      sbank90p
011000         MOVE '0001' TO ABEND-CODE                                sbank90p
011100         MOVE SPACES TO ABEND-REASON                              sbank90p
011200         COPY CABENDPO.                                           sbank90p
011300     END-EVALUATE.                                                sbank90p
011400                                                                  sbank90p
011500* Call the appropriate routine to handle the business logic       sbank90p
011600     IF BANK-MAP-FUNCTION-GET                                     sbank90p
011700        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             sbank90p
011800                       COMMAREA(WS-BANK-DATA)                     sbank90p
011900                       LENGTH(LENGTH OF WS-BANK-DATA)             sbank90p
012000        END-EXEC                                                  sbank90p
012100     END-IF.                                                      sbank90p
012200                                                                  sbank90p
012300***************************************************************** sbank90p
012400* Now we have to have finished and can return to our invoker.   * sbank90p
012500***************************************************************** sbank90p
012600* Now return to CICS                                              sbank90p
012700     MOVE WS-BANK-DATA-AREAS (1:LENGTH OF WS-BANK-DATA-AREAS)     sbank90p
012800       TO DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      sbank90p
012900     EXEC CICS                                                    sbank90p
013000          RETURN                                                  sbank90p
013100     END-EXEC.                                                    sbank90p
013200     GOBACK.                                                      sbank90p
013300                                                                  sbank90p
013400***************************************************************** sbank90p
013500* Screen processing for MBANK90                                 * sbank90p
013600*---------------------------------------------------------------* sbank90p
013700* Retrieve data from screen and format it                       * sbank90p
013800***************************************************************** sbank90p
013900 SCREEN90-READ.                                                   sbank90p
014000     MOVE 'BBANK90P' TO WS-BUSINESS-LOGIC-PGM.                    sbank90p
014100     IF BANK-AID-CLEAR                                            sbank90p
014200        SET BANK-AID-PFK03 TO TRUE                                sbank90p
014300        GO TO SCREEN90-READ-EXIT                                  sbank90p
014400     END-IF.                                                      sbank90p
014500     IF BANK-ENV-CICS                                             sbank90p
014600        GO TO SCREEN90-READ-CICS                                  sbank90p
014700     ELSE                                                         sbank90p
014800        GO TO SCREEN90-READ-INET                                  sbank90p
014900     END-IF.                                                      sbank90p
015000                                                                  sbank90p
015100 SCREEN90-READ-CICS.                                              sbank90p
015200     IF BANK-HELP-INACTIVE                                        sbank90p
015300        EXEC CICS RECEIVE MAP('BANK90A')                          sbank90p
015400                          MAPSET('MBANK90')                       sbank90p
015500        END-EXEC                                                  sbank90p
015600     ELSE                                                         sbank90p
015700        EXEC CICS RECEIVE MAP('HELP90A')                          sbank90p
015800                          MAPSET('MBANK90')                       sbank90p
015900        END-EXEC                                                  sbank90p
016000        GO TO SCREEN90-READ-EXIT                                  sbank90p
016100     END-IF.                                                      sbank90p
016200                                                                  sbank90p
016300     GO TO SCREEN90-READ-EXIT.                                    sbank90p
016400                                                                  sbank90p
016500 SCREEN90-READ-INET.                                              sbank90p
016600     GO TO SCREEN90-READ-EXIT.                                    sbank90p
016700                                                                  sbank90p
016800 SCREEN90-READ-EXIT.                                              sbank90p
016900     EXIT.                                                        sbank90p
017000                                                                  sbank90p
017100***************************************************************** sbank90p
017200* Screen processing for SCREEN90 (BANK90/HELP90)                * sbank90p
017300*---------------------------------------------------------------* sbank90p
017400* Build the output screen and send it                           * sbank90p
017500***************************************************************** sbank90p
017600 SCREEN90-BUILD-AND-SEND.                                         sbank90p
017700* Clear map area, get date & time and move to the map             sbank90p
017800     MOVE LOW-VALUES TO BANK90AO.                                 sbank90p
017900     MOVE EIBTIME TO DD-TIME-INPUT-N.                             sbank90p
018000     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      sbank90p
018100     SET DDI-YYDDD TO TRUE.                                       sbank90p
018200     SET DDO-DD-MMM-YYYY TO TRUE.                                 sbank90p
018300     PERFORM CALL-DATECONV THRU                                   sbank90p
018400             CALL-DATECONV-EXIT.                                  sbank90p
018500* Ensure the last map fields are correct                          sbank90p
018600     IF BANK-HELP-ACTIVE                                          sbank90p
018700        MOVE 'MHELP90' TO BANK-LAST-MAPSET                        sbank90p
018800        MOVE 'HELP90A' TO BANK-LAST-MAP                           sbank90p
018900     ELSE                                                         sbank90p
019000        MOVE 'MBANK90' TO BANK-LAST-MAPSET                        sbank90p
019100        MOVE 'BANK90A' TO BANK-LAST-MAP                           sbank90p
019200     END-IF.                                                      sbank90p
019300     IF BANK-ENV-CICS                                             sbank90p
019400        GO TO SCREEN90-BUILD-AND-SEND-CICS                        sbank90p
019500     ELSE                                                         sbank90p
019600        GO TO SCREEN90-BUILD-AND-SEND-INET                        sbank90p
019700     END-IF.                                                      sbank90p
019800                                                                  sbank90p
019900 SCREEN90-BUILD-AND-SEND-CICS.                                    sbank90p
020000     IF BANK-LAST-MAP IS EQUAL TO 'BANK90A'                       sbank90p
020100        GO TO BANK90-BUILD-AND-SEND-CICS                          sbank90p
020200     END-IF.                                                      sbank90p
020300     IF BANK-LAST-MAP IS EQUAL TO 'HELP90A'                       sbank90p
020400        GO TO HELP90-BUILD-AND-SEND-CICS                          sbank90p
020500     END-IF.                                                      sbank90p
020600     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                          sbank90p
020700     MOVE '0003' TO ABEND-CODE                                    sbank90p
020800     MOVE SPACES TO ABEND-REASON                                  sbank90p
020900     COPY CABENDPO.                                               sbank90p
021000     GOBACK.                                                      sbank90p
021100                                                                  sbank90p
021200 BANK90-BUILD-AND-SEND-CICS.                                      sbank90p
021300     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANK90AO==.        sbank90p
021400     COPY CVERSNP1 REPLACING ==<<SCRN>>== BY ==BANK90AO==.        sbank90p
021500     MOVE WS-TRAN-ID TO TRANO IN BANK90AO.                        sbank90p
021600     MOVE DD-TIME-OUTPUT TO TIMEO IN BANK90AO.                    sbank90p
021700     MOVE DDO-DATA TO DATEO IN BANK90AO.                          sbank90p
021800* Move in any error message                                       sbank90p
021900     MOVE BANK-ERROR-MSG TO ERRMSGO IN BANK90AO.                  sbank90p
022000* Move in screen specific fields                                  sbank90p
022100* Move in screen specific fields                                  sbank90p
022200        MOVE BANK-SCR90-LINE (01) TO INF01O IN BANK90AO.          sbank90p
022300        MOVE BANK-SCR90-LINE (02) TO INF02O IN BANK90AO.          sbank90p
022400        MOVE BANK-SCR90-LINE (03) TO INF03O IN BANK90AO.          sbank90p
022500        MOVE BANK-SCR90-LINE (04) TO INF04O IN BANK90AO.          sbank90p
022600        MOVE BANK-SCR90-LINE (05) TO INF05O IN BANK90AO.          sbank90p
022700        MOVE BANK-SCR90-LINE (06) TO INF06O IN BANK90AO.          sbank90p
022800        MOVE BANK-SCR90-LINE (07) TO INF07O IN BANK90AO.          sbank90p
022900        MOVE BANK-SCR90-LINE (08) TO INF08O IN BANK90AO.          sbank90p
023000        MOVE BANK-SCR90-LINE (09) TO INF09O IN BANK90AO.          sbank90p
023100        MOVE BANK-SCR90-LINE (10) TO INF10O IN BANK90AO.          sbank90p
023200        MOVE BANK-SCR90-LINE (11) TO INF11O IN BANK90AO.          sbank90p
023300        MOVE BANK-SCR90-LINE (12) TO INF12O IN BANK90AO.          sbank90p
023400        MOVE BANK-SCR90-LINE (13) TO INF13O IN BANK90AO.          sbank90p
023500        MOVE BANK-SCR90-LINE (14) TO INF14O IN BANK90AO.          sbank90p
023600        MOVE BANK-SCR90-LINE (15) TO INF15O IN BANK90AO.          sbank90p
023700        MOVE BANK-SCR90-LINE (16) TO INF16O IN BANK90AO.          sbank90p
023800        MOVE BANK-SCR90-LINE (17) TO INF17O IN BANK90AO.          sbank90p
023900        MOVE BANK-SCR90-LINE (18) TO INF18O IN BANK90AO.          sbank90p
024000        MOVE BANK-SCR90-LINE (19) TO INF19O IN BANK90AO.          sbank90p
024100* Turn colour off if required                                     sbank90p
024200     IF COLOUR-OFF                                                sbank90p
024300        MOVE DFHGREEN TO TXT01C IN BANK90AO                       sbank90p
024400        MOVE DFHGREEN TO SCRNC IN BANK90AO                        sbank90p
024500        MOVE DFHGREEN TO HEAD1C IN BANK90AO                       sbank90p
024600        MOVE DFHGREEN TO DATEC IN BANK90AO                        sbank90p
024700        MOVE DFHGREEN TO TXT02C IN BANK90AO                       sbank90p
024800        MOVE DFHGREEN TO TRANC IN BANK90AO                        sbank90p
024900        MOVE DFHGREEN TO HEAD2C IN BANK90AO                       sbank90p
025000        MOVE DFHGREEN TO TIMEC IN BANK90AO                        sbank90p
025100        MOVE DFHGREEN TO INF01C IN BANK90AO                       sbank90p
025200        MOVE DFHGREEN TO INF02C IN BANK90AO                       sbank90p
025300        MOVE DFHGREEN TO INF03C IN BANK90AO                       sbank90p
025400        MOVE DFHGREEN TO INF04C IN BANK90AO                       sbank90p
025500        MOVE DFHGREEN TO INF05C IN BANK90AO                       sbank90p
025600        MOVE DFHGREEN TO INF06C IN BANK90AO                       sbank90p
025700        MOVE DFHGREEN TO INF07C IN BANK90AO                       sbank90p
025800        MOVE DFHGREEN TO INF08C IN BANK90AO                       sbank90p
025900        MOVE DFHGREEN TO INF09C IN BANK90AO                       sbank90p
026000        MOVE DFHGREEN TO INF10C IN BANK90AO                       sbank90p
026100        MOVE DFHGREEN TO INF11C IN BANK90AO                       sbank90p
026200        MOVE DFHGREEN TO INF12C IN BANK90AO                       sbank90p
026300        MOVE DFHGREEN TO INF13C IN BANK90AO                       sbank90p
026400        MOVE DFHGREEN TO INF14C IN BANK90AO                       sbank90p
026500        MOVE DFHGREEN TO INF15C IN BANK90AO                       sbank90p
026600        MOVE DFHGREEN TO INF16C IN BANK90AO                       sbank90p
026700        MOVE DFHGREEN TO INF17C IN BANK90AO                       sbank90p
026800        MOVE DFHGREEN TO INF18C IN BANK90AO                       sbank90p
026900        MOVE DFHGREEN TO INF19C IN BANK90AO                       sbank90p
027000        MOVE DFHGREEN TO TXT03C IN BANK90AO                       sbank90p
027100        MOVE DFHGREEN TO ERRMSGC IN BANK90AO                      sbank90p
027200        MOVE DFHGREEN TO VERC IN BANK90AO                         sbank90p
027300     END-IF.                                                      sbank90p
027400                                                                  sbank90p
027500     EXEC CICS SEND MAP('BANK90A')                                sbank90p
027600                    MAPSET('MBANK90')                             sbank90p
027700                    ERASE                                         sbank90p
027800                    FREEKB                                        sbank90p
027900     END-EXEC.                                                    sbank90p
028000     GO TO SCREEN90-BUILD-AND-SEND-EXIT.                          sbank90p
028100                                                                  sbank90p
028200 HELP90-BUILD-AND-SEND-CICS.                                      sbank90p
028300     COPY CSCRNHP2 REPLACING ==:OPTN:== BY ==BANK==               sbank90p
028400                             ==<<SCRN>>== BY ==HELP90AO==.        sbank90p
028500                                                                  sbank90p
028600     EXEC CICS SEND MAP('HELP90A')                                sbank90p
028700                    MAPSET('MBANK90')                             sbank90p
028800                    ERASE                                         sbank90p
028900                    FREEKB                                        sbank90p
029000     END-EXEC.                                                    sbank90p
029100     GO TO SCREEN90-BUILD-AND-SEND-EXIT.                          sbank90p
029200                                                                  sbank90p
029300 SCREEN90-BUILD-AND-SEND-INET.                                    sbank90p
029400     MOVE SPACES TO EXT-OP-DATA.                                  sbank90p
029500     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              sbank90p
029600     MOVE DDO-DATA TO EXT-OP-DATE.                                sbank90p
029700     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          sbank90p
029800     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         sbank90p
029900     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          sbank90p
030000     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          sbank90p
030100     CALL 'SVERSONP' USING SCREEN-TITLES.                         sbank90p
030200     MOVE VERSION TO EXT-OP-VERSION.                              sbank90p
030300* Move in screen name                                             sbank90p
030400     MOVE 'BANK90' TO EXT-OP-SCREEN.                              sbank90p
030500* Move in userid and any error message                            sbank90p
030600     MOVE BANK-ERROR-MSG TO EXT-OP-ERR-MSG.                       sbank90p
030700     MOVE BANK-USERID TO EXT-OP-USERID.                           sbank90p
030800     MOVE BANK-USERID-NAME TO EXT-OP-NAME.                        sbank90p
030900* Move in screen specific fields                                  sbank90p
031000*    MOVE BANK-SCR20-SEL5TX TO EXT-OP20-SEL5TX.                   sbank90p
031100     GO TO SCREEN90-BUILD-AND-SEND-EXIT.                          sbank90p
031200                                                                  sbank90p
031300 SCREEN90-BUILD-AND-SEND-EXIT.                                    sbank90p
031400     EXIT.                                                        sbank90p
031500                                                                  sbank90p
031600***************************************************************** sbank90p
031700* Call common routine to perform date conversions               * sbank90p
031800***************************************************************** sbank90p
031900 CALL-DATECONV.                                                   sbank90p
032000     MOVE BANK-ENV TO DD-ENV.                                     sbank90p
032100     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           sbank90p
032200     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            sbank90p
032300 CALL-DATECONV-EXIT.                                              sbank90p
032400     EXIT.                                                        sbank90p
032500                                                                  sbank90p
032600* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     sbank90p
