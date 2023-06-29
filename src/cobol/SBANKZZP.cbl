000100***************************************************************** SBANKZZP
000200*                                                               * SBANKZZP
000300*   Copyright (C) 1998-2008 Micro Focus. All Rights Reserved.   * SBANKZZP
000400*   This demonstration program is provided for use by users     * SBANKZZP
000500*   of Micro Focus products and may be used, modified and       * SBANKZZP
000600*   distributed as part of your application provided that       * SBANKZZP
000700*   you properly acknowledge the copyright of Micro Focus       * SBANKZZP
000800*   in this material.                                           * SBANKZZP
000900*                                                               * SBANKZZP
001000***************************************************************** SBANKZZP
001100                                                                  SBANKZZP
001200***************************************************************** SBANKZZP
001300* Program:     SBANKZZP.CBL (CICS Version)                      * SBANKZZP
001400* Layer:       Screen handling                                  * SBANKZZP
001500* Function:    Determine problem / error to create              * SBANKZZP
001600***************************************************************** SBANKZZP
001700                                                                  SBANKZZP
001800 IDENTIFICATION DIVISION.                                         SBANKZZP
001900 PROGRAM-ID.                                                      SBANKZZP
002000     SBANKZZP.                                                    SBANKZZP
002100 DATE-WRITTEN.                                                    SBANKZZP
002200     September 2002.                                              SBANKZZP
002300 DATE-COMPILED.                                                   SBANKZZP
002400     Today.                                                       SBANKZZP
002500                                                                  SBANKZZP
002600 ENVIRONMENT DIVISION.                                            SBANKZZP
002700                                                                  SBANKZZP
002800 DATA DIVISION.                                                   SBANKZZP
002900 WORKING-STORAGE SECTION.                                         SBANKZZP
003000 01  WS-MISC-STORAGE.                                             SBANKZZP
003100   05  WS-PROGRAM-ID                         PIC X(8)             SBANKZZP
003200       VALUE 'SBANKZZP'.                                          SBANKZZP
003300   05  WS-TRAN-ID                            PIC X(4).            SBANKZZP
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             SBANKZZP
003500       VALUE SPACES.                                              SBANKZZP
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             SBANKZZP
003700       VALUE 'UNKNOWN'.                                           SBANKZZP
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      SBANKZZP
         05  WS-VERSION                            PIC X(7).
                                                                        SBANKZZP
003900                                                                  
004000 01  MAPAREA                                 PIC X(2048).         SBANKZZP
004100 COPY MBANKZZ.                                                    SBANKZZP
004200                                                                  SBANKZZP
004300 01  WS-TIME-DATE-WORK-AREA.                                      SBANKZZP
004400 COPY CDATED.                                                     SBANKZZP
004500                                                                  SBANKZZP
004600 01  WS-BANK-DATA-AREAS.                                          SBANKZZP
004700   05  WS-BANK-DATA.                                              SBANKZZP
004800 COPY CBANKDAT.                                                   SBANKZZP
004900   05  WS-BANK-EXT-DATA.                                          SBANKZZP
005000 COPY CBANKEXT.                                                   SBANKZZP
005100                                                                  SBANKZZP
005200 COPY CSCRNHDD.                                                   SBANKZZP
005300                                                                  SBANKZZP
005400 COPY CVERSND.                                                    SBANKZZP
005500                                                                  SBANKZZP
005600 COPY DFHAID.                                                     SBANKZZP
005700                                                                  SBANKZZP
005800 COPY DFHBMSCA.                                                   SBANKZZP
005900                                                                  SBANKZZP
006000 COPY CABENDD.                                                    SBANKZZP
006100                                                                  SBANKZZP
006200 LINKAGE SECTION.                                                 SBANKZZP
006300 01  DFHCOMMAREA.                                                 SBANKZZP
006400   05  FILLER                                PIC X(7168).         SBANKZZP
006500                                                                  SBANKZZP
006600 PROCEDURE DIVISION.                                              SBANKZZP
006700***************************************************************** SBANKZZP
006800* Write entry to log to show we have been invoked               * SBANKZZP
006900***************************************************************** SBANKZZP
007000     COPY CTRACE.                                                 SBANKZZP
007100                                                                  SBANKZZP
007200***************************************************************** SBANKZZP
007300* Store our transaction-id                                      * SBANKZZP
007400***************************************************************** SBANKZZP
007500     MOVE EIBTRNID TO WS-TRAN-ID.                                 SBANKZZP
007600                                                                  SBANKZZP
007700***************************************************************** SBANKZZP
007800* Store passed data or abend if there wasn't any                * SBANKZZP
007900***************************************************************** SBANKZZP
008000     IF EIBCALEN IS EQUAL TO 0                                    SBANKZZP
008100        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       SBANKZZP
008200        MOVE '0001' TO ABEND-CODE                                 SBANKZZP
008300        MOVE SPACES TO ABEND-REASON                               SBANKZZP
008400        COPY CABENDPO.                                            SBANKZZP
008500     ELSE                                                         SBANKZZP
008600        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        SBANKZZP
008700        MOVE LOW-VALUES TO WS-BANK-DATA                           SBANKZZP
008800        MOVE DFHCOMMAREA (1:EIBCALEN) TO WS-BANK-DATA (1:EIBCALEN)SBANKZZP
008900     END-IF.                                                      SBANKZZP
009000                                                                  SBANKZZP
009100***************************************************************** SBANKZZP
009200* This is the main process                                      * SBANKZZP
009300***************************************************************** SBANKZZP
009400                                                                  SBANKZZP
009500***************************************************************** SBANKZZP
009600* Determine what we have to do (read from or send to screen)    * SBANKZZP
009700***************************************************************** SBANKZZP
009800     MOVE LOW-VALUE TO MAPAREA.                                   SBANKZZP
009900     EVALUATE TRUE                                                SBANKZZP
010000       WHEN BANK-MAP-FUNCTION-GET                                 SBANKZZP
010100         PERFORM SCREENZZ-READ THRU                               SBANKZZP
010200                 SCREENZZ-READ-EXIT                               SBANKZZP
010300       WHEN BANK-MAP-FUNCTION-PUT                                 SBANKZZP
010400         PERFORM SCREENZZ-BUILD-AND-SEND THRU                     SBANKZZP
010500                 SCREENZZ-BUILD-AND-SEND-EXIT                     SBANKZZP
010600       WHEN OTHER                                                 SBANKZZP
010700         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      SBANKZZP
010800         MOVE '0001' TO ABEND-CODE                                SBANKZZP
010900         MOVE SPACES TO ABEND-REASON                              SBANKZZP
011000         COPY CABENDPO.                                           SBANKZZP
011100     END-EVALUATE.                                                SBANKZZP
011200                                                                  SBANKZZP
011300* Call the appropriate routine to handle the business logic       SBANKZZP
011400     IF BANK-MAP-FUNCTION-GET                                     SBANKZZP
011500        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             SBANKZZP
011600                       COMMAREA(WS-BANK-DATA)                     SBANKZZP
011700                       LENGTH(LENGTH OF WS-BANK-DATA)             SBANKZZP
011800        END-EXEC                                                  SBANKZZP
011900     END-IF.                                                      SBANKZZP
012000                                                                  SBANKZZP
012100***************************************************************** SBANKZZP
012200* Now we have to have finished and can return to our invoker.   * SBANKZZP
012300***************************************************************** SBANKZZP
012400* Now return to CICS                                              SBANKZZP
012500     MOVE WS-BANK-DATA (1:WS-SAVED-EIBCALEN) TO                   SBANKZZP
012600          DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      SBANKZZP
012700     EXEC CICS                                                    SBANKZZP
012800          RETURN                                                  SBANKZZP
012900     END-EXEC.                                                    SBANKZZP
013000     GOBACK.                                                      SBANKZZP
013100                                                                  SBANKZZP
013200***************************************************************** SBANKZZP
013300* Screen processing for MBANKZZ                                 * SBANKZZP
013400*---------------------------------------------------------------* SBANKZZP
013500* Retrieve data from screen and format it                       * SBANKZZP
013600***************************************************************** SBANKZZP
013700 SCREENZZ-READ.                                                   SBANKZZP
013800     MOVE 'BBANKZZP' TO WS-BUSINESS-LOGIC-PGM.                    SBANKZZP
013900     IF BANK-AID-CLEAR                                            SBANKZZP
014000        SET BANK-AID-PFK03 TO TRUE                                SBANKZZP
014100        GO TO SCREENZZ-READ-EXIT                                  SBANKZZP
014200     END-IF.                                                      SBANKZZP
014300     IF BANK-ENV-CICS                                             SBANKZZP
014400        GO TO SCREENZZ-READ-CICS                                  SBANKZZP
014500     ELSE                                                         SBANKZZP
014600        GO TO SCREENZZ-READ-INET                                  SBANKZZP
014700     END-IF.                                                      SBANKZZP
014800                                                                  SBANKZZP
014900 SCREENZZ-READ-CICS.                                              SBANKZZP
015000     IF BANK-HELP-INACTIVE                                        SBANKZZP
015100        EXEC CICS RECEIVE MAP('BANKZZA')                          SBANKZZP
015200                          MAPSET('MBANKZZ')                       SBANKZZP
015300        END-EXEC                                                  SBANKZZP
015400     ELSE                                                         SBANKZZP
015500        EXEC CICS RECEIVE MAP('HELPZZA')                          SBANKZZP
015600                          MAPSET('MBANKZZ')                       SBANKZZP
015700        END-EXEC                                                  SBANKZZP
015800        GO TO SCREENZZ-READ-EXIT                                  SBANKZZP
015900     END-IF.                                                      SBANKZZP
016000                                                                  SBANKZZP
016100     IF SEL1IDL IN BANKZZAI IS EQUAL TO 0                         SBANKZZP
016200        MOVE SPACES TO BANK-SCRZZ-SEL1ID                          SBANKZZP
016300     ELSE                                                         SBANKZZP
016400        MOVE SEL1IDI IN BANKZZAI TO BANK-SCRZZ-SEL1ID             SBANKZZP
016500        IF BANK-SCRZZ-SEL1ID IS EQUAL TO SPACES OR                SBANKZZP
016600           BANK-SCRZZ-SEL1ID IS EQUAL TO ALL '_'                  SBANKZZP
016700           MOVE LOW-VALUES TO BANK-SCRZZ-SEL1ID                   SBANKZZP
016800     END-IF.                                                      SBANKZZP
016900     IF SEL1IPL IN BANKZZAI IS EQUAL TO 0                         SBANKZZP
017000        MOVE LOW-VALUES TO BANK-SCRZZ-SEL1IP                      SBANKZZP
017100     ELSE                                                         SBANKZZP
017200        MOVE SEL1IPI IN BANKZZAI TO BANK-SCRZZ-SEL1IP             SBANKZZP
017300        IF BANK-SCRZZ-SEL1IP IS EQUAL TO SPACES OR                SBANKZZP
017400           BANK-SCRZZ-SEL1IP IS EQUAL TO ALL '_'                  SBANKZZP
017500           MOVE LOW-VALUES TO BANK-SCRZZ-SEL1IP                   SBANKZZP
017600     END-IF.                                                      SBANKZZP
017700                                                                  SBANKZZP
017800     IF SEL2IDL IN BANKZZAI IS EQUAL TO 0                         SBANKZZP
017900           MOVE SPACES TO BANK-SCRZZ-SEL2ID                       SBANKZZP
018000     ELSE                                                         SBANKZZP
018100        MOVE SEL2IDI IN BANKZZAI TO BANK-SCRZZ-SEL2ID             SBANKZZP
018200        IF BANK-SCRZZ-SEL2ID IS EQUAL TO SPACES OR                SBANKZZP
018300           BANK-SCRZZ-SEL2ID IS EQUAL TO ALL '_'                  SBANKZZP
018400           MOVE LOW-VALUES TO BANK-SCRZZ-SEL2ID                   SBANKZZP
018500     END-IF.                                                      SBANKZZP
018600     IF SEL2IPL IN BANKZZAI IS EQUAL TO 0                         SBANKZZP
018700        MOVE LOW-VALUES TO BANK-SCRZZ-SEL2IP                      SBANKZZP
018800     ELSE                                                         SBANKZZP
018900        MOVE SEL2IPI IN BANKZZAI TO BANK-SCRZZ-SEL2IP             SBANKZZP
019000        IF BANK-SCRZZ-SEL2IP IS EQUAL TO SPACES OR                SBANKZZP
019100           BANK-SCRZZ-SEL2IP IS EQUAL TO ALL '_'                  SBANKZZP
019200           MOVE LOW-VALUES TO BANK-SCRZZ-SEL2IP                   SBANKZZP
019300     END-IF.                                                      SBANKZZP
019400                                                                  SBANKZZP
019500     IF SEL3IDL IN BANKZZAI IS EQUAL TO 0                         SBANKZZP
019600        MOVE SPACES TO BANK-SCRZZ-SEL3ID                          SBANKZZP
019700     ELSE                                                         SBANKZZP
019800        MOVE SEL3IDI IN BANKZZAI TO BANK-SCRZZ-SEL3ID             SBANKZZP
019900        IF BANK-SCRZZ-SEL3ID IS EQUAL TO SPACES OR                SBANKZZP
020000           BANK-SCRZZ-SEL3ID IS EQUAL TO ALL '_'                  SBANKZZP
020100           MOVE LOW-VALUES TO BANK-SCRZZ-SEL3ID                   SBANKZZP
020200     END-IF.                                                      SBANKZZP
020300     IF SEL3IPL IN BANKZZAI IS EQUAL TO 0                         SBANKZZP
020400           MOVE LOW-VALUES TO BANK-SCRZZ-SEL3IP                   SBANKZZP
020500     ELSE                                                         SBANKZZP
020600        MOVE SEL3IPI IN BANKZZAI TO BANK-SCRZZ-SEL3IP             SBANKZZP
020700        IF BANK-SCRZZ-SEL3IP IS EQUAL TO SPACES OR                SBANKZZP
020800           BANK-SCRZZ-SEL3IP IS EQUAL TO ALL '_'                  SBANKZZP
020900           MOVE LOW-VALUES TO BANK-SCRZZ-SEL3IP                   SBANKZZP
021000     END-IF.                                                      SBANKZZP
021100                                                                  SBANKZZP
021200     IF SEL4IDL IN BANKZZAI IS EQUAL TO 0                         SBANKZZP
021300        MOVE SPACES TO BANK-SCRZZ-SEL4ID                          SBANKZZP
021400     ELSE                                                         SBANKZZP
021500        MOVE SEL4IDI IN BANKZZAI TO BANK-SCRZZ-SEL4ID             SBANKZZP
021600        IF BANK-SCRZZ-SEL4ID IS EQUAL TO SPACES OR                SBANKZZP
021700           BANK-SCRZZ-SEL4ID IS EQUAL TO ALL '_'                  SBANKZZP
021800           MOVE LOW-VALUES TO BANK-SCRZZ-SEL4ID                   SBANKZZP
021900     END-IF.                                                      SBANKZZP
022000     IF SEL4IPL IN BANKZZAI IS EQUAL TO 0                         SBANKZZP
022100        MOVE LOW-VALUES TO BANK-SCRZZ-SEL4IP                      SBANKZZP
022200     ELSE                                                         SBANKZZP
022300        MOVE SEL4IPI IN BANKZZAI TO BANK-SCRZZ-SEL4IP             SBANKZZP
022400        IF BANK-SCRZZ-SEL4IP IS EQUAL TO SPACES OR                SBANKZZP
022500           BANK-SCRZZ-SEL4IP IS EQUAL TO ALL '_'                  SBANKZZP
022600           MOVE LOW-VALUES TO BANK-SCRZZ-SEL4IP                   SBANKZZP
022700     END-IF.                                                      SBANKZZP
022800                                                                  SBANKZZP
022900     IF SEL5IDL IN BANKZZAI IS EQUAL TO 0                         SBANKZZP
023000        MOVE SPACES TO BANK-SCRZZ-SEL5ID                          SBANKZZP
023100     ELSE                                                         SBANKZZP
023200        MOVE SEL5IDI IN BANKZZAI TO BANK-SCRZZ-SEL5ID             SBANKZZP
023300        IF BANK-SCRZZ-SEL5ID IS EQUAL TO SPACES OR                SBANKZZP
023400           BANK-SCRZZ-SEL5ID IS EQUAL TO ALL '_'                  SBANKZZP
023500           MOVE LOW-VALUES TO BANK-SCRZZ-SEL5ID                   SBANKZZP
023600     END-IF.                                                      SBANKZZP
023700     IF SEL5IPL IN BANKZZAI IS EQUAL TO 0                         SBANKZZP
023800           MOVE LOW-VALUES TO BANK-SCRZZ-SEL5IP                   SBANKZZP
023900     ELSE                                                         SBANKZZP
024000        MOVE SEL5IPI IN BANKZZAI TO BANK-SCRZZ-SEL5IP             SBANKZZP
024100        IF BANK-SCRZZ-SEL5IP IS EQUAL TO SPACES OR                SBANKZZP
024200           BANK-SCRZZ-SEL5IP IS EQUAL TO ALL '_'                  SBANKZZP
024300           MOVE LOW-VALUES TO BANK-SCRZZ-SEL5IP                   SBANKZZP
024400     END-IF.                                                      SBANKZZP
024500                                                                  SBANKZZP
024600     IF SEL6IDL IN BANKZZAI IS EQUAL TO 0                         SBANKZZP
024700        MOVE SPACES TO BANK-SCRZZ-SEL6ID                          SBANKZZP
024800     ELSE                                                         SBANKZZP
024900        MOVE SEL6IDI IN BANKZZAI TO BANK-SCRZZ-SEL6ID             SBANKZZP
025000        IF BANK-SCRZZ-SEL6ID IS EQUAL TO SPACES OR                SBANKZZP
025100           BANK-SCRZZ-SEL6ID IS EQUAL TO ALL '_'                  SBANKZZP
025200           MOVE LOW-VALUES TO BANK-SCRZZ-SEL6ID                   SBANKZZP
025300     END-IF.                                                      SBANKZZP
025400     IF SEL6IPL IN BANKZZAI IS EQUAL TO 0                         SBANKZZP
025500           MOVE LOW-VALUES TO BANK-SCRZZ-SEL6IP                   SBANKZZP
025600     ELSE                                                         SBANKZZP
025700        MOVE SEL6IPI IN BANKZZAI TO BANK-SCRZZ-SEL6IP             SBANKZZP
025800        IF BANK-SCRZZ-SEL6IP IS EQUAL TO SPACES OR                SBANKZZP
025900           BANK-SCRZZ-SEL6IP IS EQUAL TO ALL '_'                  SBANKZZP
026000           MOVE LOW-VALUES TO BANK-SCRZZ-SEL6IP                   SBANKZZP
026100     END-IF.                                                      SBANKZZP
026200                                                                  SBANKZZP
026300     IF SEL7IDL IN BANKZZAI IS EQUAL TO 0                         SBANKZZP
026400        MOVE SPACES TO BANK-SCRZZ-SEL7ID                          SBANKZZP
026500     ELSE                                                         SBANKZZP
026600        MOVE SEL7IDI IN BANKZZAI TO BANK-SCRZZ-SEL7ID             SBANKZZP
026700        IF BANK-SCRZZ-SEL7ID IS EQUAL TO SPACES OR                SBANKZZP
026800           BANK-SCRZZ-SEL7ID IS EQUAL TO ALL '_'                  SBANKZZP
026900           MOVE LOW-VALUES TO BANK-SCRZZ-SEL7ID                   SBANKZZP
027000     END-IF.                                                      SBANKZZP
027100     IF SEL7IPL IN BANKZZAI IS EQUAL TO 0                         SBANKZZP
027200           MOVE LOW-VALUES TO BANK-SCRZZ-SEL7IP                   SBANKZZP
027300     ELSE                                                         SBANKZZP
027400        MOVE SEL7IPI IN BANKZZAI TO BANK-SCRZZ-SEL7IP             SBANKZZP
027500        IF BANK-SCRZZ-SEL7IP IS EQUAL TO SPACES OR                SBANKZZP
027600           BANK-SCRZZ-SEL7IP IS EQUAL TO ALL '_'                  SBANKZZP
027700           MOVE LOW-VALUES TO BANK-SCRZZ-SEL7IP                   SBANKZZP
027800     END-IF.                                                      SBANKZZP
027900                                                                  SBANKZZP
028000     IF SEL8IDL IN BANKZZAI IS EQUAL TO 0                         SBANKZZP
028100        MOVE SPACES TO BANK-SCRZZ-SEL8ID                          SBANKZZP
028200     ELSE                                                         SBANKZZP
028300        MOVE SEL8IDI IN BANKZZAI TO BANK-SCRZZ-SEL8ID             SBANKZZP
028400        IF BANK-SCRZZ-SEL8ID IS EQUAL TO SPACES OR                SBANKZZP
028500           BANK-SCRZZ-SEL8ID IS EQUAL TO ALL '_'                  SBANKZZP
028600           MOVE LOW-VALUES TO BANK-SCRZZ-SEL8ID                   SBANKZZP
028700     END-IF.                                                      SBANKZZP
028800     IF SEL8IPL IN BANKZZAI IS EQUAL TO 0                         SBANKZZP
028900           MOVE LOW-VALUES TO BANK-SCRZZ-SEL8IP                   SBANKZZP
029000     ELSE                                                         SBANKZZP
029100        MOVE SEL8IPI IN BANKZZAI TO BANK-SCRZZ-SEL8IP             SBANKZZP
029200        IF BANK-SCRZZ-SEL8IP IS EQUAL TO SPACES OR                SBANKZZP
029300           BANK-SCRZZ-SEL8IP IS EQUAL TO ALL '_'                  SBANKZZP
029400           MOVE LOW-VALUES TO BANK-SCRZZ-SEL8IP                   SBANKZZP
029500     END-IF.                                                      SBANKZZP
029600                                                                  SBANKZZP
029700     GO TO SCREENZZ-READ-EXIT.                                    SBANKZZP
029800                                                                  SBANKZZP
029900 SCREENZZ-READ-INET.                                              SBANKZZP
030000     MOVE EXT-IPZZ-SEL1ID TO BANK-SCRZZ-SEL1ID.                   SBANKZZP
030100     MOVE EXT-IPZZ-SEL1IP TO BANK-SCRZZ-SEL1IP.                   SBANKZZP
030200     MOVE EXT-IPZZ-SEL2ID TO BANK-SCRZZ-SEL2ID.                   SBANKZZP
030300     MOVE EXT-IPZZ-SEL2IP TO BANK-SCRZZ-SEL2IP.                   SBANKZZP
030400     MOVE EXT-IPZZ-SEL3ID TO BANK-SCRZZ-SEL3ID.                   SBANKZZP
030500     MOVE EXT-IPZZ-SEL3IP TO BANK-SCRZZ-SEL3IP.                   SBANKZZP
030600     MOVE EXT-IPZZ-SEL4ID TO BANK-SCRZZ-SEL4ID.                   SBANKZZP
030700     MOVE EXT-IPZZ-SEL4IP TO BANK-SCRZZ-SEL4IP.                   SBANKZZP
030800     MOVE EXT-IPZZ-SEL5ID TO BANK-SCRZZ-SEL5ID.                   SBANKZZP
030900     MOVE EXT-IPZZ-SEL5IP TO BANK-SCRZZ-SEL5IP.                   SBANKZZP
031000     MOVE EXT-IPZZ-SEL6ID TO BANK-SCRZZ-SEL6ID.                   SBANKZZP
031100     MOVE EXT-IPZZ-SEL6IP TO BANK-SCRZZ-SEL6IP.                   SBANKZZP
031200     MOVE EXT-IPZZ-SEL7ID TO BANK-SCRZZ-SEL7ID.                   SBANKZZP
031300     MOVE EXT-IPZZ-SEL7IP TO BANK-SCRZZ-SEL7IP.                   SBANKZZP
031400     MOVE EXT-IPZZ-SEL8ID TO BANK-SCRZZ-SEL8ID.                   SBANKZZP
031500     MOVE EXT-IPZZ-SEL8IP TO BANK-SCRZZ-SEL8IP.                   SBANKZZP
031600     GO TO SCREENZZ-READ-EXIT.                                    SBANKZZP
031700                                                                  SBANKZZP
031800 SCREENZZ-READ-EXIT.                                              SBANKZZP
031900     EXIT.                                                        SBANKZZP
032000                                                                  SBANKZZP
032100***************************************************************** SBANKZZP
032200* Screen processing for SCREENZZ (BANKZZ/HELPZZ)                * SBANKZZP
032300*---------------------------------------------------------------* SBANKZZP
032400* Build the output screen and send it                           * SBANKZZP
032500***************************************************************** SBANKZZP
032600 SCREENZZ-BUILD-AND-SEND.                                         SBANKZZP
032700* Clear map area, get date & time and move to the map             SBANKZZP
032800     MOVE LOW-VALUES TO BANKZZAO.                                 SBANKZZP
032900     MOVE EIBTIME TO DD-TIME-INPUT-N.                             SBANKZZP
033000     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      SBANKZZP
033100     SET DDI-YYDDD TO TRUE.                                       SBANKZZP
033200     SET DDO-DD-MMM-YYYY TO TRUE.                                 SBANKZZP
033300     PERFORM CALL-DATECONV THRU                                   SBANKZZP
033400             CALL-DATECONV-EXIT.                                  SBANKZZP
033500* Ensure the last map fields are correct                          SBANKZZP
033600     IF BANK-HELP-ACTIVE                                          SBANKZZP
033700        MOVE 'MHELPZZ' TO BANK-LAST-MAPSET                        SBANKZZP
033800        MOVE 'HELPZZA' TO BANK-LAST-MAP                           SBANKZZP
033900     ELSE                                                         SBANKZZP
034000        MOVE 'MBANKZZ' TO BANK-LAST-MAPSET                        SBANKZZP
034100        MOVE 'BANKZZA' TO BANK-LAST-MAP                           SBANKZZP
034200     END-IF.                                                      SBANKZZP
034300     IF BANK-ENV-CICS                                             SBANKZZP
034400        GO TO SCREENZZ-BUILD-AND-SEND-CICS                        SBANKZZP
034500     ELSE                                                         SBANKZZP
034600        GO TO SCREENZZ-BUILD-AND-SEND-INET                        SBANKZZP
034700     END-IF.                                                      SBANKZZP
034800                                                                  SBANKZZP
034900 SCREENZZ-BUILD-AND-SEND-CICS.                                    SBANKZZP
035000     IF BANK-LAST-MAP IS EQUAL TO 'BANKZZA'                       SBANKZZP
035100        GO TO BANKZZ-BUILD-AND-SEND-CICS                          SBANKZZP
035200     END-IF.                                                      SBANKZZP
035300     IF BANK-LAST-MAP IS EQUAL TO 'HELPZZA'                       SBANKZZP
035400        GO TO HELPZZ-BUILD-AND-SEND-CICS                          SBANKZZP
035500     END-IF.                                                      SBANKZZP
035600     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                          SBANKZZP
035700     MOVE '0003' TO ABEND-CODE                                    SBANKZZP
035800     MOVE SPACES TO ABEND-REASON                                  SBANKZZP
035900     COPY CABENDPO.                                               SBANKZZP
036000     GOBACK.                                                      SBANKZZP
036100                                                                  SBANKZZP
036200 BANKZZ-BUILD-AND-SEND-CICS.                                      SBANKZZP
036300     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANKZZAO==.        SBANKZZP
036400     COPY CVERSNP1 REPLACING ==<<SCRN>>== BY ==BANKZZAO==.        SBANKZZP
036500     MOVE WS-TRAN-ID TO TRANO IN BANKZZAO.                        SBANKZZP
036600     MOVE DD-TIME-OUTPUT TO TIMEO IN BANKZZAO.                    SBANKZZP
036700     MOVE DDO-DATA TO DATEO IN BANKZZAO.                          SBANKZZP
036800* Move in any error message                                       SBANKZZP
036900     MOVE BANK-ERROR-MSG TO ERRMSGO IN BANKZZAO.                  SBANKZZP
037000* Move in screen specific fields                                  SBANKZZP
037100     IF BANK-SCRZZ-SEL1ID IS NOT EQUAL TO SPACES                  SBANKZZP
037200        MOVE -1 TO SEL1IDL IN BANKZZAI                            SBANKZZP
037300        MOVE BANK-SCRZZ-SEL1ID TO SEL1IDO IN BANKZZAO             SBANKZZP
037400        MOVE -1 TO SEL1IPL IN BANKZZAI                            SBANKZZP
037500        IF BANK-SCRZZ-SEL1IP IS EQUAL TO LOW-VALUES               SBANKZZP
037600           MOVE ALL '_' TO SEL1IPO IN BANKZZAO                    SBANKZZP
037700        ELSE                                                      SBANKZZP
037800           MOVE BANK-SCRZZ-SEL1IP TO SEL1IPO IN BANKZZAO          SBANKZZP
037900        END-IF                                                    SBANKZZP
038000        MOVE BANK-SCRZZ-SEL1TX TO SEL1TXO IN BANKZZAO             SBANKZZP
038100     ELSE                                                         SBANKZZP
038200        MOVE DFHBMASK TO SEL1IDA IN BANKZZAI                      SBANKZZP
038300        MOVE DFHBMASK TO SEL1IPA IN BANKZZAI                      SBANKZZP
038400        MOVE SPACES TO SEL1IDO IN BANKZZAO                        SBANKZZP
038500        MOVE SPACES TO SEL1IPO IN BANKZZAO                        SBANKZZP
038600        MOVE SPACES TO SEL1TXO IN BANKZZAO                        SBANKZZP
038700     END-IF.                                                      SBANKZZP
038800     IF BANK-SCRZZ-SEL2ID IS NOT EQUAL TO SPACES                  SBANKZZP
038900        MOVE -1 TO SEL2IDL IN BANKZZAI                            SBANKZZP
039000        MOVE BANK-SCRZZ-SEL2ID TO SEL2IDO IN BANKZZAO             SBANKZZP
039100        MOVE -1 TO SEL2IPL IN BANKZZAI                            SBANKZZP
039200        IF BANK-SCRZZ-SEL2IP IS EQUAL TO LOW-VALUES               SBANKZZP
039300           MOVE ALL '_' TO SEL2IPO IN BANKZZAO                    SBANKZZP
039400        ELSE                                                      SBANKZZP
039500           MOVE BANK-SCRZZ-SEL2IP TO SEL2IPO IN BANKZZAO          SBANKZZP
039600        END-IF                                                    SBANKZZP
039700        MOVE BANK-SCRZZ-SEL2TX TO SEL2TXO IN BANKZZAO             SBANKZZP
039800     ELSE                                                         SBANKZZP
039900        MOVE DFHBMASK TO SEL2IDA IN BANKZZAI                      SBANKZZP
040000        MOVE DFHBMASK TO SEL2IPA IN BANKZZAI                      SBANKZZP
040100        MOVE SPACES TO SEL2IDO IN BANKZZAO                        SBANKZZP
040200        MOVE SPACES TO SEL2IPO IN BANKZZAO                        SBANKZZP
040300        MOVE SPACES TO SEL2TXO IN BANKZZAO                        SBANKZZP
040400     END-IF.                                                      SBANKZZP
040500     IF BANK-SCRZZ-SEL3ID IS NOT EQUAL TO SPACES                  SBANKZZP
040600        MOVE -1 TO SEL3IDL IN BANKZZAI                            SBANKZZP
040700        MOVE BANK-SCRZZ-SEL3ID TO SEL3IDO IN BANKZZAO             SBANKZZP
040800        MOVE -1 TO SEL3IPL IN BANKZZAI                            SBANKZZP
040900        IF BANK-SCRZZ-SEL3IP IS EQUAL TO LOW-VALUES               SBANKZZP
041000           MOVE ALL '_' TO SEL3IPO IN BANKZZAO                    SBANKZZP
041100        ELSE                                                      SBANKZZP
041200           MOVE BANK-SCRZZ-SEL3IP TO SEL3IPO IN BANKZZAO          SBANKZZP
041300        END-IF                                                    SBANKZZP
041400        MOVE BANK-SCRZZ-SEL3TX TO SEL3TXO IN BANKZZAO             SBANKZZP
041500     ELSE                                                         SBANKZZP
041600        MOVE DFHBMASK TO SEL3IDA IN BANKZZAI                      SBANKZZP
041700        MOVE DFHBMASK TO SEL3IPA IN BANKZZAI                      SBANKZZP
041800        MOVE SPACES TO SEL3IDO IN BANKZZAO                        SBANKZZP
041900        MOVE SPACES TO SEL3IPO IN BANKZZAO                        SBANKZZP
042000        MOVE SPACES TO SEL3TXO IN BANKZZAO                        SBANKZZP
042100     END-IF.                                                      SBANKZZP
042200     IF BANK-SCRZZ-SEL4ID IS NOT EQUAL TO SPACES                  SBANKZZP
042300        MOVE -1 TO SEL4IDL IN BANKZZAI                            SBANKZZP
042400        MOVE BANK-SCRZZ-SEL4ID TO SEL4IDO IN BANKZZAO             SBANKZZP
042500        MOVE -1 TO SEL4IPL IN BANKZZAI                            SBANKZZP
042600        IF BANK-SCRZZ-SEL4IP IS EQUAL TO LOW-VALUES               SBANKZZP
042700           MOVE ALL '_' TO SEL4IPO IN BANKZZAO                    SBANKZZP
042800        ELSE                                                      SBANKZZP
042900           MOVE BANK-SCRZZ-SEL4IP TO SEL4IPO IN BANKZZAO          SBANKZZP
043000        END-IF                                                    SBANKZZP
043100        MOVE BANK-SCRZZ-SEL4TX TO SEL4TXO IN BANKZZAO             SBANKZZP
043200     ELSE                                                         SBANKZZP
043300        MOVE DFHBMASK TO SEL4IDA IN BANKZZAI                      SBANKZZP
043400        MOVE DFHBMASK TO SEL4IPA IN BANKZZAI                      SBANKZZP
043500        MOVE SPACES TO SEL4IDO IN BANKZZAO                        SBANKZZP
043600        MOVE SPACES TO SEL4IPO IN BANKZZAO                        SBANKZZP
043700        MOVE SPACES TO SEL4TXO IN BANKZZAO                        SBANKZZP
043800     END-IF.                                                      SBANKZZP
043900     IF BANK-SCRZZ-SEL5ID IS NOT EQUAL TO SPACES                  SBANKZZP
044000        MOVE -1 TO SEL5IDL IN BANKZZAI                            SBANKZZP
044100        MOVE BANK-SCRZZ-SEL5ID TO SEL5IDO IN BANKZZAO             SBANKZZP
044200        MOVE -1 TO SEL5IPL IN BANKZZAI                            SBANKZZP
044300        IF BANK-SCRZZ-SEL5IP IS EQUAL TO LOW-VALUES               SBANKZZP
044400           MOVE ALL '_' TO SEL5IPO IN BANKZZAO                    SBANKZZP
044500        ELSE                                                      SBANKZZP
044600           MOVE BANK-SCRZZ-SEL5IP TO SEL5IPO IN BANKZZAO          SBANKZZP
044700        END-IF                                                    SBANKZZP
044800        MOVE BANK-SCRZZ-SEL5TX TO SEL5TXO IN BANKZZAO             SBANKZZP
044900     ELSE                                                         SBANKZZP
045000        MOVE DFHBMASK TO SEL5IDA IN BANKZZAI                      SBANKZZP
045100        MOVE DFHBMASK TO SEL5IPA IN BANKZZAI                      SBANKZZP
045200        MOVE SPACES TO SEL5IDO IN BANKZZAO                        SBANKZZP
045300        MOVE SPACES TO SEL5IPO IN BANKZZAO                        SBANKZZP
045400        MOVE SPACES TO SEL5TXO IN BANKZZAO                        SBANKZZP
045500     END-IF.                                                      SBANKZZP
045600     IF BANK-SCRZZ-SEL6ID IS NOT EQUAL TO SPACES                  SBANKZZP
045700        MOVE -1 TO SEL6IDL IN BANKZZAI                            SBANKZZP
045800        MOVE BANK-SCRZZ-SEL6ID TO SEL6IDO IN BANKZZAO             SBANKZZP
045900        MOVE -1 TO SEL6IPL IN BANKZZAI                            SBANKZZP
046000        IF BANK-SCRZZ-SEL6IP IS EQUAL TO LOW-VALUES               SBANKZZP
046100           MOVE ALL '_' TO SEL6IPO IN BANKZZAO                    SBANKZZP
046200        ELSE                                                      SBANKZZP
046300           MOVE BANK-SCRZZ-SEL6IP TO SEL6IPO IN BANKZZAO          SBANKZZP
046400        END-IF                                                    SBANKZZP
046500        MOVE BANK-SCRZZ-SEL6TX TO SEL6TXO IN BANKZZAO             SBANKZZP
046600     ELSE                                                         SBANKZZP
046700        MOVE DFHBMASK TO SEL6IDA IN BANKZZAI                      SBANKZZP
046800        MOVE DFHBMASK TO SEL6IPA IN BANKZZAI                      SBANKZZP
046900        MOVE SPACES TO SEL6IDO IN BANKZZAO                        SBANKZZP
047000        MOVE SPACES TO SEL6IPO IN BANKZZAO                        SBANKZZP
047100        MOVE SPACES TO SEL6TXO IN BANKZZAO                        SBANKZZP
047200     END-IF.                                                      SBANKZZP
047300     IF BANK-SCRZZ-SEL7ID IS NOT EQUAL TO SPACES                  SBANKZZP
047400        MOVE -1 TO SEL7IDL IN BANKZZAI                            SBANKZZP
047500        MOVE BANK-SCRZZ-SEL7ID TO SEL7IDO IN BANKZZAO             SBANKZZP
047600        MOVE -1 TO SEL7IPL IN BANKZZAI                            SBANKZZP
047700        IF BANK-SCRZZ-SEL7IP IS EQUAL TO LOW-VALUES               SBANKZZP
047800           MOVE ALL '_' TO SEL7IPO IN BANKZZAO                    SBANKZZP
047900        ELSE                                                      SBANKZZP
048000           MOVE BANK-SCRZZ-SEL7IP TO SEL7IPO IN BANKZZAO          SBANKZZP
048100        END-IF                                                    SBANKZZP
048200        MOVE BANK-SCRZZ-SEL7TX TO SEL7TXO IN BANKZZAO             SBANKZZP
048300     ELSE                                                         SBANKZZP
048400        MOVE DFHBMASK TO SEL7IDA IN BANKZZAI                      SBANKZZP
048500        MOVE DFHBMASK TO SEL7IPA IN BANKZZAI                      SBANKZZP
048600        MOVE SPACES TO SEL7IDO IN BANKZZAO                        SBANKZZP
048700        MOVE SPACES TO SEL7IPO IN BANKZZAO                        SBANKZZP
048800        MOVE SPACES TO SEL7TXO IN BANKZZAO                        SBANKZZP
048900     END-IF.                                                      SBANKZZP
049000     IF BANK-SCRZZ-SEL8ID IS NOT EQUAL TO SPACES                  SBANKZZP
049100        MOVE -1 TO SEL8IDL IN BANKZZAI                            SBANKZZP
049200        MOVE BANK-SCRZZ-SEL8ID TO SEL8IDO IN BANKZZAO             SBANKZZP
049300        MOVE -1 TO SEL8IPL IN BANKZZAI                            SBANKZZP
049400        IF BANK-SCRZZ-SEL8IP IS EQUAL TO LOW-VALUES               SBANKZZP
049500           MOVE ALL '_' TO SEL8IPO IN BANKZZAO                    SBANKZZP
049600        ELSE                                                      SBANKZZP
049700           MOVE BANK-SCRZZ-SEL8IP TO SEL8IPO IN BANKZZAO          SBANKZZP
049800        END-IF                                                    SBANKZZP
049900        MOVE BANK-SCRZZ-SEL8TX TO SEL8TXO IN BANKZZAO             SBANKZZP
050000     ELSE                                                         SBANKZZP
050100        MOVE DFHBMASK TO SEL8IDA IN BANKZZAI                      SBANKZZP
050200        MOVE DFHBMASK TO SEL8IPA IN BANKZZAI                      SBANKZZP
050300        MOVE SPACES TO SEL8IDO IN BANKZZAO                        SBANKZZP
050400        MOVE SPACES TO SEL8IPO IN BANKZZAO                        SBANKZZP
050500        MOVE SPACES TO SEL8TXO IN BANKZZAO                        SBANKZZP
050600     END-IF.                                                      SBANKZZP
050700* Turn colour off if required                                     SBANKZZP
050800     IF COLOUR-OFF                                                SBANKZZP
050900        MOVE DFHGREEN TO TXT01C IN BANKZZAO                       SBANKZZP
051000        MOVE DFHGREEN TO SCRNC IN BANKZZAO                        SBANKZZP
051100        MOVE DFHGREEN TO HEAD1C IN BANKZZAO                       SBANKZZP
051200        MOVE DFHGREEN TO DATEC IN BANKZZAO                        SBANKZZP
051300        MOVE DFHGREEN TO TXT02C IN BANKZZAO                       SBANKZZP
051400        MOVE DFHGREEN TO TRANC IN BANKZZAO                        SBANKZZP
051500        MOVE DFHGREEN TO HEAD2C IN BANKZZAO                       SBANKZZP
051600        MOVE DFHGREEN TO TIMEC IN BANKZZAO                        SBANKZZP
051700        MOVE DFHGREEN TO TXT03C IN BANKZZAO                       SBANKZZP
051800        MOVE DFHGREEN TO TXT04C IN BANKZZAO                       SBANKZZP
051900        MOVE DFHGREEN TO SEL1IDC IN BANKZZAO                      SBANKZZP
052000        MOVE DFHGREEN TO SEL1IPC IN BANKZZAO                      SBANKZZP
052100        MOVE DFHGREEN TO SEL1TXC IN BANKZZAO                      SBANKZZP
052200        MOVE DFHGREEN TO SEL2IDC IN BANKZZAO                      SBANKZZP
052300        MOVE DFHGREEN TO SEL2IPC IN BANKZZAO                      SBANKZZP
052400        MOVE DFHGREEN TO SEL2TXC IN BANKZZAO                      SBANKZZP
052500        MOVE DFHGREEN TO SEL3IDC IN BANKZZAO                      SBANKZZP
052600        MOVE DFHGREEN TO SEL3IPC IN BANKZZAO                      SBANKZZP
052700        MOVE DFHGREEN TO SEL3TXC IN BANKZZAO                      SBANKZZP
052800        MOVE DFHGREEN TO SEL4IDC IN BANKZZAO                      SBANKZZP
052900        MOVE DFHGREEN TO SEL4IPC IN BANKZZAO                      SBANKZZP
053000        MOVE DFHGREEN TO SEL4TXC IN BANKZZAO                      SBANKZZP
053100        MOVE DFHGREEN TO SEL5IDC IN BANKZZAO                      SBANKZZP
053200        MOVE DFHGREEN TO SEL5IPC IN BANKZZAO                      SBANKZZP
053300        MOVE DFHGREEN TO SEL5TXC IN BANKZZAO                      SBANKZZP
053400        MOVE DFHGREEN TO SEL6IDC IN BANKZZAO                      SBANKZZP
053500        MOVE DFHGREEN TO SEL6IPC IN BANKZZAO                      SBANKZZP
053600        MOVE DFHGREEN TO SEL6TXC IN BANKZZAO                      SBANKZZP
053700        MOVE DFHGREEN TO SEL7IDC IN BANKZZAO                      SBANKZZP
053800        MOVE DFHGREEN TO SEL7IPC IN BANKZZAO                      SBANKZZP
053900        MOVE DFHGREEN TO SEL7TXC IN BANKZZAO                      SBANKZZP
054000        MOVE DFHGREEN TO SEL8IDC IN BANKZZAO                      SBANKZZP
054100        MOVE DFHGREEN TO SEL8IPC IN BANKZZAO                      SBANKZZP
054200        MOVE DFHGREEN TO SEL8TXC IN BANKZZAO                      SBANKZZP
054300        MOVE DFHGREEN TO ERRMSGC IN BANKZZAO                      SBANKZZP
054400        MOVE DFHGREEN TO VERC IN BANKZZAO                         SBANKZZP
054500     END-IF.                                                      SBANKZZP
054600                                                                  SBANKZZP
054700     EXEC CICS SEND MAP('BANKZZA')                                SBANKZZP
054800                    MAPSET('MBANKZZ')                             SBANKZZP
054900                    ERASE                                         SBANKZZP
055000                    FREEKB                                        SBANKZZP
055100     END-EXEC.                                                    SBANKZZP
055200     GO TO SCREENZZ-BUILD-AND-SEND-EXIT.                          SBANKZZP
055300                                                                  SBANKZZP
055400 HELPZZ-BUILD-AND-SEND-CICS.                                      SBANKZZP
055500     COPY CSCRNHP2 REPLACING ==:OPTN:== BY ==BANK==               SBANKZZP
055600                             ==<<SCRN>>== BY ==HELPZZAO==.        SBANKZZP
055700                                                                  SBANKZZP
055800     EXEC CICS SEND MAP('HELPZZA')                                SBANKZZP
055900                    MAPSET('MBANKZZ')                             SBANKZZP
056000                    ERASE                                         SBANKZZP
056100                    FREEKB                                        SBANKZZP
056200     END-EXEC.                                                    SBANKZZP
056300     GO TO SCREENZZ-BUILD-AND-SEND-EXIT.                          SBANKZZP
056400                                                                  SBANKZZP
056500 SCREENZZ-BUILD-AND-SEND-INET.                                    SBANKZZP
056600     MOVE SPACES TO EXT-OP-DATA.                                  SBANKZZP
056700     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              SBANKZZP
056800     MOVE DDO-DATA TO EXT-OP-DATE.                                SBANKZZP
056900     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          SBANKZZP
057000     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         SBANKZZP
057100     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          SBANKZZP
057200     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          SBANKZZP
057300     CALL 'SVERSONP' USING SCREEN-TITLES.                         SBANKZZP
057400     MOVE VERSION TO EXT-OP-VERSION.                              SBANKZZP
057500* Move in screen name                                             SBANKZZP
057600     MOVE 'BANKZZ' TO EXT-OP-SCREEN.                              SBANKZZP
057700* Move in userid and any error message                            SBANKZZP
057800     MOVE BANK-ERROR-MSG TO EXT-OP-ERR-MSG.                       SBANKZZP
057900     MOVE BANK-USERID TO EXT-OP-USERID.                           SBANKZZP
058000     MOVE BANK-USERID-NAME TO EXT-OP-NAME.                        SBANKZZP
058100* Move in screen specific fields                                  SBANKZZP
058200     MOVE BANK-SCRZZ-SEL1ID TO EXT-OPZZ-SEL1ID.                   SBANKZZP
058300     MOVE BANK-SCRZZ-SEL1IP TO EXT-OPZZ-SEL1IP.                   SBANKZZP
058400     MOVE BANK-SCRZZ-SEL1TX TO EXT-OPZZ-SEL1TX.                   SBANKZZP
058500     MOVE BANK-SCRZZ-SEL2ID TO EXT-OPZZ-SEL2ID.                   SBANKZZP
058600     MOVE BANK-SCRZZ-SEL2IP TO EXT-OPZZ-SEL2IP.                   SBANKZZP
058700     MOVE BANK-SCRZZ-SEL2TX TO EXT-OPZZ-SEL2TX.                   SBANKZZP
058800     MOVE BANK-SCRZZ-SEL3ID TO EXT-OPZZ-SEL3ID.                   SBANKZZP
058900     MOVE BANK-SCRZZ-SEL3IP TO EXT-OPZZ-SEL3IP.                   SBANKZZP
059000     MOVE BANK-SCRZZ-SEL3TX TO EXT-OPZZ-SEL3TX.                   SBANKZZP
059100     MOVE BANK-SCRZZ-SEL4ID TO EXT-OPZZ-SEL4ID.                   SBANKZZP
059200     MOVE BANK-SCRZZ-SEL4IP TO EXT-OPZZ-SEL4IP.                   SBANKZZP
059300     MOVE BANK-SCRZZ-SEL4TX TO EXT-OPZZ-SEL4TX.                   SBANKZZP
059400     MOVE BANK-SCRZZ-SEL5ID TO EXT-OPZZ-SEL5ID.                   SBANKZZP
059500     MOVE BANK-SCRZZ-SEL5IP TO EXT-OPZZ-SEL5IP.                   SBANKZZP
059600     MOVE BANK-SCRZZ-SEL5TX TO EXT-OPZZ-SEL5TX.                   SBANKZZP
059700     MOVE BANK-SCRZZ-SEL6ID TO EXT-OPZZ-SEL6ID.                   SBANKZZP
059800     MOVE BANK-SCRZZ-SEL6IP TO EXT-OPZZ-SEL6IP.                   SBANKZZP
059900     MOVE BANK-SCRZZ-SEL6TX TO EXT-OPZZ-SEL6TX.                   SBANKZZP
060000     MOVE BANK-SCRZZ-SEL7ID TO EXT-OPZZ-SEL7ID.                   SBANKZZP
060100     MOVE BANK-SCRZZ-SEL7IP TO EXT-OPZZ-SEL7IP.                   SBANKZZP
060200     MOVE BANK-SCRZZ-SEL7TX TO EXT-OPZZ-SEL7TX.                   SBANKZZP
060300     MOVE BANK-SCRZZ-SEL8ID TO EXT-OPZZ-SEL8ID.                   SBANKZZP
060400     MOVE BANK-SCRZZ-SEL8IP TO EXT-OPZZ-SEL8IP.                   SBANKZZP
060500     MOVE BANK-SCRZZ-SEL8TX TO EXT-OPZZ-SEL8TX.                   SBANKZZP
060600     GO TO SCREENZZ-BUILD-AND-SEND-EXIT.                          SBANKZZP
060700                                                                  SBANKZZP
060800 SCREENZZ-BUILD-AND-SEND-EXIT.                                    SBANKZZP
060900     EXIT.                                                        SBANKZZP
061000                                                                  SBANKZZP
061100***************************************************************** SBANKZZP
061200* Call common routine to perform date conversions               * SBANKZZP
061300***************************************************************** SBANKZZP
061400 CALL-DATECONV.                                                   SBANKZZP
061500     MOVE BANK-ENV TO DD-ENV.                                     SBANKZZP
061600     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           SBANKZZP
061700     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            SBANKZZP
061800 CALL-DATECONV-EXIT.                                              SBANKZZP
061900     EXIT.                                                        SBANKZZP
062000                                                                  SBANKZZP
062100* $ Version 5.98b sequenced on Thursday 5 Feb 2009 at 11:00pm     SBANKZZP
