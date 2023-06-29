000100***************************************************************** SBANK20P
000200*                                                               * SBANK20P
000300*   Copyright (C) 1998-2005 Micro Focus. All Rights Reserved.   * SBANK20P
000400*   This demonstration program is provided for use by users     * SBANK20P
000500*   of Micro Focus products and may be used, modified and       * SBANK20P
000600*   distributed as part of your application provided that       * SBANK20P
000700*   you properly acknowledge the copyright of Micro Focus       * SBANK20P
000800*   in this material.                                           * SBANK20P
000900*                                                               * SBANK20P
001000***************************************************************** SBANK20P
001100                                                                  SBANK20P
001200***************************************************************** SBANK20P
001300* Program:     SBANK20P.CBL (CICS Version)                      * SBANK20P
001400* Layer:       Screen handling                                  * SBANK20P
001500* Function:    Determine user options                           * SBANK20P
001600***************************************************************** SBANK20P
001700                                                                  SBANK20P
001800 IDENTIFICATION DIVISION.                                         SBANK20P
001900 PROGRAM-ID.                                                      SBANK20P
002000     SBANK20P.                                                    SBANK20P
002100 DATE-WRITTEN.                                                    SBANK20P
002200     September 2002.                                              SBANK20P
002300 DATE-COMPILED.                                                   SBANK20P
002400     Today.                                                       SBANK20P
002500                                                                  SBANK20P
002600 ENVIRONMENT DIVISION.                                            SBANK20P
002700                                                                  SBANK20P
002800 DATA DIVISION.                                                   SBANK20P
002900 WORKING-STORAGE SECTION.                                         SBANK20P
003000 01  WS-MISC-STORAGE.                                             SBANK20P
003100   05  WS-PROGRAM-ID                         PIC X(8)             SBANK20P
003200       VALUE 'SBANK20P'.                                          SBANK20P
003300   05  WS-TRAN-ID                            PIC X(4).            SBANK20P
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             SBANK20P
003500       VALUE SPACES.                                              SBANK20P
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             SBANK20P
003700       VALUE 'UNKNOWN'.                                           SBANK20P
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      SBANK20P
         05  WS-VERSION                            PIC X(7).
                                                                        SBANK20P
003900                                                                  
004000 01  MAPAREA                                 PIC X(2048).         SBANK20P
004100 COPY MBANK20.                                                    SBANK20P
004200                                                                  SBANK20P
004300 01  WS-TIME-DATE-WORK-AREA.                                      SBANK20P
004400 COPY CDATED.                                                     SBANK20P
004500                                                                  SBANK20P
004600 01  WS-BANK-DATA-AREAS.                                          SBANK20P
004700   05  WS-BANK-DATA.                                              SBANK20P
004800 COPY CBANKDAT.                                                   SBANK20P
004900   05  WS-BANK-EXT-DATA.                                          SBANK20P
005000 COPY CBANKEXT.                                                   SBANK20P
005100                                                                  SBANK20P
005200 COPY CSCRNHDD.                                                   SBANK20P
005300                                                                  SBANK20P
       COPY CVERSND.

005400 COPY DFHAID.                                                     SBANK20P
005500                                                                  SBANK20P
005600 COPY DFHBMSCA.                                                   SBANK20P
005700                                                                  SBANK20P
005800 COPY CABENDD.                                                    SBANK20P
005900                                                                  SBANK20P
006000 LINKAGE SECTION.                                                 SBANK20P
006100 01  DFHCOMMAREA.                                                 SBANK20P
006200   05  FILLER                                PIC X(7168).         SBANK20P
006300                                                                  SBANK20P
006400 PROCEDURE DIVISION.                                              SBANK20P
006500***************************************************************** SBANK20P
006600* Write entry to log to show we have been invoked               * SBANK20P
006700***************************************************************** SBANK20P
006800     COPY CTRACE.                                                 SBANK20P
006900                                                                  SBANK20P
007000***************************************************************** SBANK20P
007100* Store our transaction-id                                      * SBANK20P
007200***************************************************************** SBANK20P
007300     MOVE EIBTRNID TO WS-TRAN-ID.                                 SBANK20P
007400                                                                  SBANK20P
007500***************************************************************** SBANK20P
007600* Store passed data or abend if there wasn't any                * SBANK20P
007700***************************************************************** SBANK20P
007800     IF EIBCALEN IS EQUAL TO 0                                    SBANK20P
007900        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       SBANK20P
008000        MOVE '0001' TO ABEND-CODE                                 SBANK20P
008100        MOVE SPACES TO ABEND-REASON                               SBANK20P
008200        COPY CABENDPO.                                            SBANK20P
008300     ELSE                                                         SBANK20P
008400        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        SBANK20P
008500        MOVE LOW-VALUES TO WS-BANK-DATA                           SBANK20P
008600        MOVE DFHCOMMAREA (1:EIBCALEN) TO WS-BANK-DATA (1:EIBCALEN)SBANK20P
008700     END-IF.                                                      SBANK20P
008800                                                                  SBANK20P
008900***************************************************************** SBANK20P
009000* This is the main process                                      * SBANK20P
009100***************************************************************** SBANK20P
009200                                                                  SBANK20P
009300***************************************************************** SBANK20P
009400* Determine what we have to do (read from or send to screen)    * SBANK20P
009500***************************************************************** SBANK20P
009600     MOVE LOW-VALUE TO MAPAREA.                                   SBANK20P
009700     EVALUATE TRUE                                                SBANK20P
009800       WHEN BANK-MAP-FUNCTION-GET                                 SBANK20P
009900         PERFORM SCREEN20-READ THRU                               SBANK20P
010000                 SCREEN20-READ-EXIT                               SBANK20P
010100       WHEN BANK-MAP-FUNCTION-PUT                                 SBANK20P
010200         PERFORM SCREEN20-BUILD-AND-SEND THRU                     SBANK20P
010300                 SCREEN20-BUILD-AND-SEND-EXIT                     SBANK20P
010400       WHEN OTHER                                                 SBANK20P
010500         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      SBANK20P
010600         MOVE '0001' TO ABEND-CODE                                SBANK20P
010700         MOVE SPACES TO ABEND-REASON                              SBANK20P
010800         COPY CABENDPO.                                           SBANK20P
010900     END-EVALUATE.                                                SBANK20P
011000                                                                  SBANK20P
011100* Call the appropriate routine to handle the business logic       SBANK20P
011200     IF BANK-MAP-FUNCTION-GET                                     SBANK20P
011300        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             SBANK20P
011400                       COMMAREA(WS-BANK-DATA)                     SBANK20P
011500                       LENGTH(LENGTH OF WS-BANK-DATA)             SBANK20P
011600        END-EXEC                                                  SBANK20P
011700     END-IF.                                                      SBANK20P
011800                                                                  SBANK20P
011900***************************************************************** SBANK20P
012000* Now we have to have finished and can return to our invoker.   * SBANK20P
012100***************************************************************** SBANK20P
012200* Now return to CICS                                              SBANK20P
012300     MOVE WS-BANK-DATA (1:WS-SAVED-EIBCALEN) TO                   SBANK20P
012400          DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      SBANK20P
012500     EXEC CICS                                                    SBANK20P
012600          RETURN                                                  SBANK20P
012700     END-EXEC.                                                    SBANK20P
012800     GOBACK.                                                      SBANK20P
012900                                                                  SBANK20P
013000***************************************************************** SBANK20P
013100* Screen processing for MBANK20                                 * SBANK20P
013200*---------------------------------------------------------------* SBANK20P
013300* Retrieve data from screen and format it                       * SBANK20P
013400***************************************************************** SBANK20P
013500 SCREEN20-READ.                                                   SBANK20P
013600     MOVE 'BBANK20P' TO WS-BUSINESS-LOGIC-PGM.                    SBANK20P
013700     IF BANK-AID-CLEAR                                            SBANK20P
013800        SET BANK-AID-PFK03 TO TRUE                                SBANK20P
013900        GO TO SCREEN20-READ-EXIT                                  SBANK20P
014000     END-IF.                                                      SBANK20P
014100     IF BANK-ENV-CICS                                             SBANK20P
014200        GO TO SCREEN20-READ-CICS                                  SBANK20P
014300     ELSE                                                         SBANK20P
014400        GO TO SCREEN20-READ-INET                                  SBANK20P
014500     END-IF.                                                      SBANK20P
014600                                                                  SBANK20P
014700 SCREEN20-READ-CICS.                                              SBANK20P
014800     IF BANK-HELP-INACTIVE                                        SBANK20P
014900        EXEC CICS RECEIVE MAP('BANK20A')                          SBANK20P
015000                          MAPSET('MBANK20')                       SBANK20P
015100        END-EXEC                                                  SBANK20P
015200     ELSE                                                         SBANK20P
015300        EXEC CICS RECEIVE MAP('HELP20A')                          SBANK20P
015400                          MAPSET('MBANK20')                       SBANK20P
015500        END-EXEC                                                  SBANK20P
015600        GO TO SCREEN20-READ-EXIT                                  SBANK20P
015700     END-IF.                                                      SBANK20P
015800                                                                  SBANK20P
015900     IF SEL1IDL IN BANK20AI IS EQUAL TO 0                         SBANK20P
016000           MOVE SPACES TO BANK-SCR20-SEL1ID                       SBANK20P
016100     ELSE                                                         SBANK20P
016200        MOVE SEL1IDI IN BANK20AI TO BANK-SCR20-SEL1ID             SBANK20P
016300        IF BANK-SCR20-SEL1ID IS EQUAL TO SPACES OR                SBANK20P
016400           BANK-SCR20-SEL1ID IS EQUAL TO ALL '_'                  SBANK20P
016500           MOVE LOW-VALUES TO BANK-SCR20-SEL1ID                   SBANK20P
016600     END-IF.                                                      SBANK20P
016700     IF SEL1IPL IN BANK20AI IS EQUAL TO 0                         SBANK20P
016800           MOVE LOW-VALUES TO BANK-SCR20-SEL1IP                   SBANK20P
016900     ELSE                                                         SBANK20P
017000        MOVE SEL1IPI IN BANK20AI TO BANK-SCR20-SEL1IP             SBANK20P
017100        IF BANK-SCR20-SEL1IP IS EQUAL TO SPACES OR                SBANK20P
017200           BANK-SCR20-SEL1IP IS EQUAL TO ALL '_'                  SBANK20P
017300           MOVE LOW-VALUES TO BANK-SCR20-SEL1IP                   SBANK20P
017400     END-IF.                                                      SBANK20P
017500                                                                  SBANK20P
017600     IF SEL2IDL IN BANK20AI IS EQUAL TO 0                         SBANK20P
017700           MOVE SPACES TO BANK-SCR20-SEL2ID                       SBANK20P
017800     ELSE                                                         SBANK20P
017900        MOVE SEL2IDI IN BANK20AI TO BANK-SCR20-SEL2ID             SBANK20P
018000        IF BANK-SCR20-SEL2ID IS EQUAL TO SPACES OR                SBANK20P
018100           BANK-SCR20-SEL2ID IS EQUAL TO ALL '_'                  SBANK20P
018200           MOVE LOW-VALUES TO BANK-SCR20-SEL2ID                   SBANK20P
018300     END-IF.                                                      SBANK20P
018400     IF SEL2IPL IN BANK20AI IS EQUAL TO 0                         SBANK20P
018500           MOVE LOW-VALUES TO BANK-SCR20-SEL2IP                   SBANK20P
018600     ELSE                                                         SBANK20P
018700        MOVE SEL2IPI IN BANK20AI TO BANK-SCR20-SEL2IP             SBANK20P
018800        IF BANK-SCR20-SEL2IP IS EQUAL TO SPACES OR                SBANK20P
018900           BANK-SCR20-SEL2IP IS EQUAL TO ALL '_'                  SBANK20P
019000           MOVE LOW-VALUES TO BANK-SCR20-SEL2IP                   SBANK20P
019100     END-IF.                                                      SBANK20P
019200                                                                  SBANK20P
019300     IF SEL3IDL IN BANK20AI IS EQUAL TO 0                         SBANK20P
019400           MOVE SPACES TO BANK-SCR20-SEL3ID                       SBANK20P
019500     ELSE                                                         SBANK20P
019600        MOVE SEL3IDI IN BANK20AI TO BANK-SCR20-SEL3ID             SBANK20P
019700        IF BANK-SCR20-SEL3ID IS EQUAL TO SPACES OR                SBANK20P
019800           BANK-SCR20-SEL3ID IS EQUAL TO ALL '_'                  SBANK20P
019900           MOVE LOW-VALUES TO BANK-SCR20-SEL3ID                   SBANK20P
020000     END-IF.                                                      SBANK20P
020100     IF SEL3IPL IN BANK20AI IS EQUAL TO 0                         SBANK20P
020200           MOVE LOW-VALUES TO BANK-SCR20-SEL3IP                   SBANK20P
020300     ELSE                                                         SBANK20P
020400        MOVE SEL3IPI IN BANK20AI TO BANK-SCR20-SEL3IP             SBANK20P
020500        IF BANK-SCR20-SEL3IP IS EQUAL TO SPACES OR                SBANK20P
020600           BANK-SCR20-SEL3IP IS EQUAL TO ALL '_'                  SBANK20P
020700           MOVE LOW-VALUES TO BANK-SCR20-SEL3IP                   SBANK20P
020800     END-IF.                                                      SBANK20P
020900                                                                  SBANK20P
021000     IF SEL4IDL IN BANK20AI IS EQUAL TO 0                         SBANK20P
021100           MOVE SPACES TO BANK-SCR20-SEL4ID                       SBANK20P
021200     ELSE                                                         SBANK20P
021300        MOVE SEL4IDI IN BANK20AI TO BANK-SCR20-SEL4ID             SBANK20P
021400        IF BANK-SCR20-SEL4ID IS EQUAL TO SPACES OR                SBANK20P
021500           BANK-SCR20-SEL4ID IS EQUAL TO ALL '_'                  SBANK20P
021600           MOVE LOW-VALUES TO BANK-SCR20-SEL4ID                   SBANK20P
021700     END-IF.                                                      SBANK20P
021800     IF SEL4IPL IN BANK20AI IS EQUAL TO 0                         SBANK20P
021900           MOVE LOW-VALUES TO BANK-SCR20-SEL4IP                   SBANK20P
022000     ELSE                                                         SBANK20P
022100        MOVE SEL4IPI IN BANK20AI TO BANK-SCR20-SEL4IP             SBANK20P
022200        IF BANK-SCR20-SEL4IP IS EQUAL TO SPACES OR                SBANK20P
022300           BANK-SCR20-SEL4IP IS EQUAL TO ALL '_'                  SBANK20P
022400           MOVE LOW-VALUES TO BANK-SCR20-SEL4IP                   SBANK20P
022500     END-IF.                                                      SBANK20P
022600                                                                  SBANK20P
022700     IF SEL5IDL IN BANK20AI IS EQUAL TO 0                         SBANK20P
022800           MOVE SPACES TO BANK-SCR20-SEL5ID                       SBANK20P
022900     ELSE                                                         SBANK20P
023000        MOVE SEL5IDI IN BANK20AI TO BANK-SCR20-SEL5ID             SBANK20P
023100        IF BANK-SCR20-SEL5ID IS EQUAL TO SPACES OR                SBANK20P
023200           BANK-SCR20-SEL5ID IS EQUAL TO ALL '_'                  SBANK20P
023300           MOVE LOW-VALUES TO BANK-SCR20-SEL5ID                   SBANK20P
023400     END-IF.                                                      SBANK20P
023500     IF SEL5IPL IN BANK20AI IS EQUAL TO 0                         SBANK20P
023600           MOVE LOW-VALUES TO BANK-SCR20-SEL5IP                   SBANK20P
023700     ELSE                                                         SBANK20P
023800        MOVE SEL5IPI IN BANK20AI TO BANK-SCR20-SEL5IP             SBANK20P
023900        IF BANK-SCR20-SEL5IP IS EQUAL TO SPACES OR                SBANK20P
024000           BANK-SCR20-SEL5IP IS EQUAL TO ALL '_'                  SBANK20P
024100           MOVE LOW-VALUES TO BANK-SCR20-SEL5IP                   SBANK20P
024200     END-IF.                                                      SBANK20P
024300                                                                  SBANK20P
024400     IF SEL6IDL IN BANK20AI IS EQUAL TO 0                         SBANK20P
024500           MOVE SPACES TO BANK-SCR20-SEL6ID                       SBANK20P
024600     ELSE                                                         SBANK20P
024700        MOVE SEL6IDI IN BANK20AI TO BANK-SCR20-SEL6ID             SBANK20P
024800        IF BANK-SCR20-SEL6ID IS EQUAL TO SPACES OR                SBANK20P
024900           BANK-SCR20-SEL6ID IS EQUAL TO ALL '_'                  SBANK20P
025000           MOVE LOW-VALUES TO BANK-SCR20-SEL6ID                   SBANK20P
025100     END-IF.                                                      SBANK20P
025200     IF SEL6IPL IN BANK20AI IS EQUAL TO 0                         SBANK20P
025300           MOVE LOW-VALUES TO BANK-SCR20-SEL6IP                   SBANK20P
025400     ELSE                                                         SBANK20P
025500        MOVE SEL6IPI IN BANK20AI TO BANK-SCR20-SEL6IP             SBANK20P
025600        IF BANK-SCR20-SEL6IP IS EQUAL TO SPACES OR                SBANK20P
025700           BANK-SCR20-SEL6IP IS EQUAL TO ALL '_'                  SBANK20P
025800           MOVE LOW-VALUES TO BANK-SCR20-SEL6IP                   SBANK20P
025900     END-IF.                                                      SBANK20P
026000                                                                  SBANK20P
026100     IF SEL7IDL IN BANK20AI IS EQUAL TO 0                         SBANK20P
026200           MOVE SPACES TO BANK-SCR20-SEL7ID                       SBANK20P
026300     ELSE                                                         SBANK20P
026400        MOVE SEL7IDI IN BANK20AI TO BANK-SCR20-SEL7ID             SBANK20P
026500        IF BANK-SCR20-SEL7ID IS EQUAL TO SPACES OR                SBANK20P
026600           BANK-SCR20-SEL7ID IS EQUAL TO ALL '_'                  SBANK20P
026700           MOVE LOW-VALUES TO BANK-SCR20-SEL7ID                   SBANK20P
026800     END-IF.                                                      SBANK20P
026900     IF SEL7IPL IN BANK20AI IS EQUAL TO 0                         SBANK20P
027000           MOVE LOW-VALUES TO BANK-SCR20-SEL7IP                   SBANK20P
027100     ELSE                                                         SBANK20P
027200        MOVE SEL7IPI IN BANK20AI TO BANK-SCR20-SEL7IP             SBANK20P
027300        IF BANK-SCR20-SEL7IP IS EQUAL TO SPACES OR                SBANK20P
027400           BANK-SCR20-SEL7IP IS EQUAL TO ALL '_'                  SBANK20P
027500           MOVE LOW-VALUES TO BANK-SCR20-SEL7IP                   SBANK20P
027600     END-IF.                                                      SBANK20P
027700                                                                  SBANK20P
027800     GO TO SCREEN20-READ-EXIT.                                    SBANK20P
027900                                                                  SBANK20P
028000 SCREEN20-READ-INET.                                              SBANK20P
028100     MOVE EXT-IP20-SEL1ID TO BANK-SCR20-SEL1ID.                   SBANK20P
028200     MOVE EXT-IP20-SEL1IP TO BANK-SCR20-SEL1IP.                   SBANK20P
028300     MOVE EXT-IP20-SEL2ID TO BANK-SCR20-SEL2ID.                   SBANK20P
028400     MOVE EXT-IP20-SEL2IP TO BANK-SCR20-SEL2IP.                   SBANK20P
028500     MOVE EXT-IP20-SEL3ID TO BANK-SCR20-SEL3ID.                   SBANK20P
028600     MOVE EXT-IP20-SEL3IP TO BANK-SCR20-SEL3IP.                   SBANK20P
028700     MOVE EXT-IP20-SEL4ID TO BANK-SCR20-SEL4ID.                   SBANK20P
028800     MOVE EXT-IP20-SEL4IP TO BANK-SCR20-SEL4IP.                   SBANK20P
028900     MOVE EXT-IP20-SEL5ID TO BANK-SCR20-SEL5ID.                   SBANK20P
029000     MOVE EXT-IP20-SEL5IP TO BANK-SCR20-SEL5IP.                   SBANK20P
029100     MOVE EXT-IP20-SEL6ID TO BANK-SCR20-SEL6ID.                   SBANK20P
029200     MOVE EXT-IP20-SEL6IP TO BANK-SCR20-SEL6IP.                   SBANK20P
029300     MOVE EXT-IP20-SEL7ID TO BANK-SCR20-SEL7ID.                   SBANK20P
029400     MOVE EXT-IP20-SEL7IP TO BANK-SCR20-SEL7IP.                   SBANK20P
029500     GO TO SCREEN20-READ-EXIT.                                    SBANK20P
029600                                                                  SBANK20P
029700 SCREEN20-READ-EXIT.                                              SBANK20P
029800     EXIT.                                                        SBANK20P
029900                                                                  SBANK20P
030000***************************************************************** SBANK20P
030100* Screen processing for SCREEN20 (BANK20/HELP20)                * SBANK20P
030200*---------------------------------------------------------------* SBANK20P
030300* Build the output screen and send it                           * SBANK20P
030400***************************************************************** SBANK20P
030500 SCREEN20-BUILD-AND-SEND.                                         SBANK20P
030600* Clear map area, get date & time and move to the map             SBANK20P
030700     MOVE LOW-VALUES TO BANK20AO.                                 SBANK20P
030800     MOVE EIBTIME TO DD-TIME-INPUT-N.                             SBANK20P
030900     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      SBANK20P
031000     SET DDI-YYDDD TO TRUE.                                       SBANK20P
031100     SET DDO-DD-MMM-YYYY TO TRUE.                                 SBANK20P
031200     PERFORM CALL-DATECONV THRU                                   SBANK20P
031300             CALL-DATECONV-EXIT.                                  SBANK20P
031400* Ensure the last map fields are correct                          SBANK20P
031500     IF BANK-HELP-ACTIVE                                          SBANK20P
031600        MOVE 'MHELP20' TO BANK-LAST-MAPSET                        SBANK20P
031700        MOVE 'HELP20A' TO BANK-LAST-MAP                           SBANK20P
031800     ELSE                                                         SBANK20P
031900        MOVE 'MBANK20' TO BANK-LAST-MAPSET                        SBANK20P
032000        MOVE 'BANK20A' TO BANK-LAST-MAP                           SBANK20P
032100     END-IF.                                                      SBANK20P
032200     IF BANK-ENV-CICS                                             SBANK20P
032300        GO TO SCREEN20-BUILD-AND-SEND-CICS                        SBANK20P
032400     ELSE                                                         SBANK20P
032500        GO TO SCREEN20-BUILD-AND-SEND-INET                        SBANK20P
032600     END-IF.                                                      SBANK20P
032700                                                                  SBANK20P
032800 SCREEN20-BUILD-AND-SEND-CICS.                                    SBANK20P
032900     IF BANK-LAST-MAP IS EQUAL TO 'BANK20A'                       SBANK20P
033000        GO TO BANK20-BUILD-AND-SEND-CICS                          SBANK20P
033100     END-IF.                                                      SBANK20P
033200     IF BANK-LAST-MAP IS EQUAL TO 'HELP20A'                       SBANK20P
033300        GO TO HELP20-BUILD-AND-SEND-CICS                          SBANK20P
033400     END-IF.                                                      SBANK20P
033500     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                          SBANK20P
033600     MOVE '0003' TO ABEND-CODE                                    SBANK20P
033700     MOVE SPACES TO ABEND-REASON                                  SBANK20P
033800     COPY CABENDPO.                                               SBANK20P
033900     GOBACK.                                                      SBANK20P
034000                                                                  SBANK20P
034100 BANK20-BUILD-AND-SEND-CICS.                                      SBANK20P
034200     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANK20AO==.        SBANK20P
034300     MOVE WS-TRAN-ID TO TRANO IN BANK20AO.                        SBANK20P
034400     MOVE DD-TIME-OUTPUT TO TIMEO IN BANK20AO.                    SBANK20P
034500     MOVE DDO-DATA TO DATEO IN BANK20AO.                          SBANK20P
034600* Move in any error message                                       SBANK20P
034700     MOVE BANK-ERROR-MSG TO ERRMSGO IN BANK20AO.                  SBANK20P
034800* Move in screen specific fields                                  SBANK20P
034900     IF BANK-SCR20-SEL1ID IS NOT EQUAL TO SPACES                  SBANK20P
035000        MOVE -1 TO SEL1IDL IN BANK20AI                            SBANK20P
035100        MOVE BANK-SCR20-SEL1ID TO SEL1IDO IN BANK20AO             SBANK20P
035200        MOVE -1 TO SEL1IPL IN BANK20AI                            SBANK20P
035300        IF BANK-SCR20-SEL1IP IS EQUAL TO LOW-VALUES               SBANK20P
035400           MOVE ALL '_' TO SEL1IPO IN BANK20AO                    SBANK20P
035500        ELSE                                                      SBANK20P
035600           MOVE BANK-SCR20-SEL1IP TO SEL1IPO IN BANK20AO          SBANK20P
035700        END-IF                                                    SBANK20P
035800        MOVE BANK-SCR20-SEL1TX TO SEL1TXO IN BANK20AO             SBANK20P
035900     ELSE                                                         SBANK20P
036000        MOVE DFHBMASK TO SEL1IDA IN BANK20AI                      SBANK20P
036100        MOVE DFHBMASK TO SEL1IPA IN BANK20AI                      SBANK20P
036200        MOVE SPACES TO SEL1IDO IN BANK20AO                        SBANK20P
036300        MOVE SPACES TO SEL1IPO IN BANK20AO                        SBANK20P
036400        MOVE SPACES TO SEL1TXO IN BANK20AO                        SBANK20P
036500     END-IF.                                                      SBANK20P
036600     IF BANK-SCR20-SEL2ID IS NOT EQUAL TO SPACES                  SBANK20P
036700        MOVE -1 TO SEL2IDL IN BANK20AI                            SBANK20P
036800        MOVE BANK-SCR20-SEL2ID TO SEL2IDO IN BANK20AO             SBANK20P
036900        MOVE -1 TO SEL2IPL IN BANK20AI                            SBANK20P
037000        IF BANK-SCR20-SEL2IP IS EQUAL TO LOW-VALUES               SBANK20P
037100           MOVE ALL '_' TO SEL2IPO IN BANK20AO                    SBANK20P
037200        ELSE                                                      SBANK20P
037300           MOVE BANK-SCR20-SEL2IP TO SEL2IPO IN BANK20AO          SBANK20P
037400        END-IF                                                    SBANK20P
037500        MOVE BANK-SCR20-SEL2TX TO SEL2TXO IN BANK20AO             SBANK20P
037600     ELSE                                                         SBANK20P
037700        MOVE DFHBMASK TO SEL2IDA IN BANK20AI                      SBANK20P
037800        MOVE DFHBMASK TO SEL2IPA IN BANK20AI                      SBANK20P
037900        MOVE SPACES TO SEL2IDO IN BANK20AO                        SBANK20P
038000        MOVE SPACES TO SEL2IPO IN BANK20AO                        SBANK20P
038100        MOVE SPACES TO SEL2TXO IN BANK20AO                        SBANK20P
038200     END-IF.                                                      SBANK20P
038300     IF BANK-SCR20-SEL3ID IS NOT EQUAL TO SPACES                  SBANK20P
038400        MOVE -1 TO SEL3IDL IN BANK20AI                            SBANK20P
038500        MOVE BANK-SCR20-SEL3ID TO SEL3IDO IN BANK20AO             SBANK20P
038600        MOVE -1 TO SEL3IPL IN BANK20AI                            SBANK20P
038700        IF BANK-SCR20-SEL3IP IS EQUAL TO LOW-VALUES               SBANK20P
038800           MOVE ALL '_' TO SEL3IPO IN BANK20AO                    SBANK20P
038900        ELSE                                                      SBANK20P
039000           MOVE BANK-SCR20-SEL3IP TO SEL3IPO IN BANK20AO          SBANK20P
039100        END-IF                                                    SBANK20P
039200        MOVE BANK-SCR20-SEL3TX TO SEL3TXO IN BANK20AO             SBANK20P
039300     ELSE                                                         SBANK20P
039400        MOVE DFHBMASK TO SEL3IDA IN BANK20AI                      SBANK20P
039500        MOVE DFHBMASK TO SEL3IPA IN BANK20AI                      SBANK20P
039600        MOVE SPACES TO SEL3IDO IN BANK20AO                        SBANK20P
039700        MOVE SPACES TO SEL3IPO IN BANK20AO                        SBANK20P
039800        MOVE SPACES TO SEL3TXO IN BANK20AO                        SBANK20P
039900     END-IF.                                                      SBANK20P
040000     IF BANK-SCR20-SEL4ID IS NOT EQUAL TO SPACES                  SBANK20P
040100        MOVE -1 TO SEL4IDL IN BANK20AI                            SBANK20P
040200        MOVE BANK-SCR20-SEL4ID TO SEL4IDO IN BANK20AO             SBANK20P
040300        MOVE -1 TO SEL4IPL IN BANK20AI                            SBANK20P
040400        IF BANK-SCR20-SEL4IP IS EQUAL TO LOW-VALUES               SBANK20P
040500           MOVE ALL '_' TO SEL4IPO IN BANK20AO                    SBANK20P
040600        ELSE                                                      SBANK20P
040700           MOVE BANK-SCR20-SEL4IP TO SEL4IPO IN BANK20AO          SBANK20P
040800        END-IF                                                    SBANK20P
040900        MOVE BANK-SCR20-SEL4TX TO SEL4TXO IN BANK20AO             SBANK20P
041000     ELSE                                                         SBANK20P
041100        MOVE DFHBMASK TO SEL4IDA IN BANK20AI                      SBANK20P
041200        MOVE DFHBMASK TO SEL4IPA IN BANK20AI                      SBANK20P
041300        MOVE SPACES TO SEL4IDO IN BANK20AO                        SBANK20P
041400        MOVE SPACES TO SEL4IPO IN BANK20AO                        SBANK20P
041500        MOVE SPACES TO SEL4TXO IN BANK20AO                        SBANK20P
041600     END-IF.                                                      SBANK20P
041700     IF BANK-SCR20-SEL5ID IS NOT EQUAL TO SPACES                  SBANK20P
041800        MOVE -1 TO SEL5IDL IN BANK20AI                            SBANK20P
041900        MOVE BANK-SCR20-SEL5ID TO SEL5IDO IN BANK20AO             SBANK20P
042000        MOVE -1 TO SEL5IPL IN BANK20AI                            SBANK20P
042100        IF BANK-SCR20-SEL5IP IS EQUAL TO LOW-VALUES               SBANK20P
042200           MOVE ALL '_' TO SEL5IPO IN BANK20AO                    SBANK20P
042300        ELSE                                                      SBANK20P
042400           MOVE BANK-SCR20-SEL5IP TO SEL5IPO IN BANK20AO          SBANK20P
042500        END-IF                                                    SBANK20P
042600        MOVE BANK-SCR20-SEL5TX TO SEL5TXO IN BANK20AO             SBANK20P
042700     ELSE                                                         SBANK20P
042800        MOVE DFHBMASK TO SEL5IDA IN BANK20AI                      SBANK20P
042900        MOVE DFHBMASK TO SEL5IPA IN BANK20AI                      SBANK20P
043000        MOVE SPACES TO SEL5IDO IN BANK20AO                        SBANK20P
043100        MOVE SPACES TO SEL5IPO IN BANK20AO                        SBANK20P
043200        MOVE SPACES TO SEL5TXO IN BANK20AO                        SBANK20P
043300     END-IF.                                                      SBANK20P
043400     IF BANK-SCR20-SEL6ID IS NOT EQUAL TO SPACES                  SBANK20P
043500        MOVE -1 TO SEL6IDL IN BANK20AI                            SBANK20P
043600        MOVE BANK-SCR20-SEL6ID TO SEL6IDO IN BANK20AO             SBANK20P
043700        MOVE -1 TO SEL6IPL IN BANK20AI                            SBANK20P
043800        IF BANK-SCR20-SEL6IP IS EQUAL TO LOW-VALUES               SBANK20P
043900           MOVE ALL '_' TO SEL6IPO IN BANK20AO                    SBANK20P
044000        ELSE                                                      SBANK20P
044100           MOVE BANK-SCR20-SEL6IP TO SEL6IPO IN BANK20AO          SBANK20P
044200        END-IF                                                    SBANK20P
044300        MOVE BANK-SCR20-SEL6TX TO SEL6TXO IN BANK20AO             SBANK20P
044400     ELSE                                                         SBANK20P
044500        MOVE DFHBMASK TO SEL6IDA IN BANK20AI                      SBANK20P
044600        MOVE DFHBMASK TO SEL6IPA IN BANK20AI                      SBANK20P
044700        MOVE SPACES TO SEL6IDO IN BANK20AO                        SBANK20P
044800        MOVE SPACES TO SEL6IPO IN BANK20AO                        SBANK20P
044900        MOVE SPACES TO SEL6TXO IN BANK20AO                        SBANK20P
045000     END-IF.                                                      SBANK20P
045100     IF BANK-SCR20-SEL7ID IS NOT EQUAL TO SPACES                  SBANK20P
045200        MOVE -1 TO SEL7IDL IN BANK20AI                            SBANK20P
045300        MOVE BANK-SCR20-SEL7ID TO SEL7IDO IN BANK20AO             SBANK20P
045400        MOVE -1 TO SEL7IPL IN BANK20AI                            SBANK20P
045500        IF BANK-SCR20-SEL7IP IS EQUAL TO LOW-VALUES               SBANK20P
045600           MOVE ALL '_' TO SEL7IPO IN BANK20AO                    SBANK20P
045700        ELSE                                                      SBANK20P
045800           MOVE BANK-SCR20-SEL7IP TO SEL7IPO IN BANK20AO          SBANK20P
045900        END-IF                                                    SBANK20P
046000        MOVE BANK-SCR20-SEL7TX TO SEL7TXO IN BANK20AO             SBANK20P
046100     ELSE                                                         SBANK20P
046200        MOVE DFHBMASK TO SEL7IDA IN BANK20AI                      SBANK20P
046300        MOVE DFHBMASK TO SEL7IPA IN BANK20AI                      SBANK20P
046400        MOVE SPACES TO SEL7IDO IN BANK20AO                        SBANK20P
046500        MOVE SPACES TO SEL7IPO IN BANK20AO                        SBANK20P
046600        MOVE SPACES TO SEL7TXO IN BANK20AO                        SBANK20P
046700     END-IF.                                                      SBANK20P
046800* Turn colour off if required                                     SBANK20P
046900     IF COLOUR-OFF                                                SBANK20P
047000        MOVE DFHGREEN TO TXT01C IN BANK20AO                       SBANK20P
047100        MOVE DFHGREEN TO SCRNC IN BANK20AO                        SBANK20P
047200        MOVE DFHGREEN TO HEAD1C IN BANK20AO                       SBANK20P
047300        MOVE DFHGREEN TO DATEC IN BANK20AO                        SBANK20P
047400        MOVE DFHGREEN TO TXT02C IN BANK20AO                       SBANK20P
047500        MOVE DFHGREEN TO TRANC IN BANK20AO                        SBANK20P
047600        MOVE DFHGREEN TO HEAD2C IN BANK20AO                       SBANK20P
047700        MOVE DFHGREEN TO TIMEC IN BANK20AO                        SBANK20P
047800        MOVE DFHGREEN TO TXT03C IN BANK20AO                       SBANK20P
047900        MOVE DFHGREEN TO TXT04C IN BANK20AO                       SBANK20P
048000        MOVE DFHGREEN TO SEL1IDC IN BANK20AO                      SBANK20P
048100        MOVE DFHGREEN TO SEL1IPC IN BANK20AO                      SBANK20P
048200        MOVE DFHGREEN TO SEL1TXC IN BANK20AO                      SBANK20P
048300        MOVE DFHGREEN TO SEL2IDC IN BANK20AO                      SBANK20P
048400        MOVE DFHGREEN TO SEL2IPC IN BANK20AO                      SBANK20P
048500        MOVE DFHGREEN TO SEL2TXC IN BANK20AO                      SBANK20P
048600        MOVE DFHGREEN TO SEL3IDC IN BANK20AO                      SBANK20P
048700        MOVE DFHGREEN TO SEL3IPC IN BANK20AO                      SBANK20P
048800        MOVE DFHGREEN TO SEL3TXC IN BANK20AO                      SBANK20P
048900        MOVE DFHGREEN TO SEL4IDC IN BANK20AO                      SBANK20P
049000        MOVE DFHGREEN TO SEL4IPC IN BANK20AO                      SBANK20P
049100        MOVE DFHGREEN TO SEL4TXC IN BANK20AO                      SBANK20P
049200        MOVE DFHGREEN TO SEL5IDC IN BANK20AO                      SBANK20P
049300        MOVE DFHGREEN TO SEL5IPC IN BANK20AO                      SBANK20P
049400        MOVE DFHGREEN TO SEL5TXC IN BANK20AO                      SBANK20P
049500        MOVE DFHGREEN TO SEL6IDC IN BANK20AO                      SBANK20P
049600        MOVE DFHGREEN TO SEL6IPC IN BANK20AO                      SBANK20P
049700        MOVE DFHGREEN TO SEL6TXC IN BANK20AO                      SBANK20P
049800        MOVE DFHGREEN TO SEL7IDC IN BANK20AO                      SBANK20P
049900        MOVE DFHGREEN TO SEL7IPC IN BANK20AO                      SBANK20P
050000        MOVE DFHGREEN TO SEL7TXC IN BANK20AO                      SBANK20P
050100        MOVE DFHGREEN TO TXT05C IN BANK20AO                       SBANK20P
050200        MOVE DFHGREEN TO ERRMSGC IN BANK20AO                      SBANK20P
050300     END-IF.                                                      SBANK20P
050400                                                                  SBANK20P
050500     EXEC CICS SEND MAP('BANK20A')                                SBANK20P
050600                    MAPSET('MBANK20')                             SBANK20P
050700                    ERASE                                         SBANK20P
050800                    FREEKB                                        SBANK20P
050900     END-EXEC.                                                    SBANK20P
051000     GO TO SCREEN20-BUILD-AND-SEND-EXIT.                          SBANK20P
051100                                                                  SBANK20P
051200 HELP20-BUILD-AND-SEND-CICS.                                      SBANK20P
051300     COPY CSCRNHP2 REPLACING ==:OPTN:== BY ==BANK==               SBANK20P
051400                             ==<<SCRN>>== BY ==HELP20AO==.        SBANK20P
051500                                                                  SBANK20P
051600     EXEC CICS SEND MAP('HELP20A')                                SBANK20P
051700                    MAPSET('MBANK20')                             SBANK20P
051800                    ERASE                                         SBANK20P
051900                    FREEKB                                        SBANK20P
052000     END-EXEC.                                                    SBANK20P
052100     GO TO SCREEN20-BUILD-AND-SEND-EXIT.                          SBANK20P
052200                                                                  SBANK20P
052300 SCREEN20-BUILD-AND-SEND-INET.                                    SBANK20P
052400     MOVE SPACES TO EXT-OP-DATA.                                  SBANK20P
052500     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              SBANK20P
052600     MOVE DDO-DATA TO EXT-OP-DATE.                                SBANK20P
052700     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          SBANK20P
052800     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         SBANK20P
052900     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          SBANK20P
053000     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          SBANK20P
053100* Move in screen name                                             SBANK20P
053200     MOVE 'BANK20' TO EXT-OP-SCREEN.                              SBANK20P
053300* Move in userid and any error message                            SBANK20P
053400     MOVE BANK-ERROR-MSG TO EXT-OP-ERR-MSG.                       SBANK20P
053500     MOVE BANK-USERID TO EXT-OP-USERID.                           SBANK20P
053600     MOVE BANK-USERID-NAME TO EXT-OP-NAME.                        SBANK20P
053700* Move in screen specific fields                                  SBANK20P
053800     MOVE BANK-SCR20-SEL1ID TO EXT-OP20-SEL1ID.                   SBANK20P
053900     MOVE BANK-SCR20-SEL1IP TO EXT-OP20-SEL1IP.                   SBANK20P
054000     MOVE BANK-SCR20-SEL1TX TO EXT-OP20-SEL1TX.                   SBANK20P
054100     MOVE BANK-SCR20-SEL2ID TO EXT-OP20-SEL2ID.                   SBANK20P
054200     MOVE BANK-SCR20-SEL2IP TO EXT-OP20-SEL2IP.                   SBANK20P
054300     MOVE BANK-SCR20-SEL2TX TO EXT-OP20-SEL2TX.                   SBANK20P
054400     MOVE BANK-SCR20-SEL3ID TO EXT-OP20-SEL3ID.                   SBANK20P
054500     MOVE BANK-SCR20-SEL3IP TO EXT-OP20-SEL3IP.                   SBANK20P
054600     MOVE BANK-SCR20-SEL3TX TO EXT-OP20-SEL3TX.                   SBANK20P
054700     MOVE BANK-SCR20-SEL4ID TO EXT-OP20-SEL4ID.                   SBANK20P
054800     MOVE BANK-SCR20-SEL4IP TO EXT-OP20-SEL4IP.                   SBANK20P
054900     MOVE BANK-SCR20-SEL4TX TO EXT-OP20-SEL4TX.                   SBANK20P
055000     MOVE BANK-SCR20-SEL5ID TO EXT-OP20-SEL5ID.                   SBANK20P
055100     MOVE BANK-SCR20-SEL5IP TO EXT-OP20-SEL5IP.                   SBANK20P
055200     MOVE BANK-SCR20-SEL5TX TO EXT-OP20-SEL5TX.                   SBANK20P
055300     MOVE BANK-SCR20-SEL6ID TO EXT-OP20-SEL6ID.                   SBANK20P
055400     MOVE BANK-SCR20-SEL6IP TO EXT-OP20-SEL6IP.                   SBANK20P
055500     MOVE BANK-SCR20-SEL6TX TO EXT-OP20-SEL6TX.                   SBANK20P
055600     MOVE BANK-SCR20-SEL7ID TO EXT-OP20-SEL7ID.                   SBANK20P
055700     MOVE BANK-SCR20-SEL7IP TO EXT-OP20-SEL7IP.                   SBANK20P
055800     MOVE BANK-SCR20-SEL7TX TO EXT-OP20-SEL7TX.                   SBANK20P
055900     GO TO SCREEN20-BUILD-AND-SEND-EXIT.                          SBANK20P
056000                                                                  SBANK20P
056100 SCREEN20-BUILD-AND-SEND-EXIT.                                    SBANK20P
056200     EXIT.                                                        SBANK20P
056300                                                                  SBANK20P
056400***************************************************************** SBANK20P
056500* Call common routine to perform date conversions               * SBANK20P
056600***************************************************************** SBANK20P
056700 CALL-DATECONV.                                                   SBANK20P
056800     MOVE BANK-ENV TO DD-ENV.                                     SBANK20P
056900     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           SBANK20P
057000     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            SBANK20P
057100 CALL-DATECONV-EXIT.                                              SBANK20P
057200     EXIT.                                                        SBANK20P
