000100***************************************************************** sbank35p
000200*                                                               * sbank35p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * sbank35p
000400*   This demonstration program is provided for use by users     * sbank35p
000500*   of Micro Focus products and may be used, modified and       * sbank35p
000600*   distributed as part of your application provided that       * sbank35p
000700*   you properly acknowledge the copyright of Micro Focus       * sbank35p
000800*   in this material.                                           * sbank35p
000900*                                                               * sbank35p
001000***************************************************************** sbank35p
001100                                                                  sbank35p
001200***************************************************************** sbank35p
001300* Program:     SBANK35P.CBL (CICS Version)                      * sbank35p
001400* Layer:       Screen handling                                  * sbank35p
001500* Function:    Display account details (extended)               * sbank35p
001600***************************************************************** sbank35p
001700                                                                  sbank35p
001800 IDENTIFICATION DIVISION.                                         sbank35p
001900 PROGRAM-ID.                                                      sbank35p
002000     SBANK35P.                                                    sbank35p
002100 DATE-WRITTEN.                                                    sbank35p
002200     September 2002.                                              sbank35p
002300 DATE-COMPILED.                                                   sbank35p
002400     Today.                                                       sbank35p
002500                                                                  sbank35p
002600 ENVIRONMENT DIVISION.                                            sbank35p
002700                                                                  sbank35p
002800 DATA DIVISION.                                                   sbank35p
002900 WORKING-STORAGE SECTION.                                         sbank35p
003000 01  WS-MISC-STORAGE.                                             sbank35p
003100   05  WS-PROGRAM-ID                         PIC X(8)             sbank35p
003200       VALUE 'SBANK35P'.                                          sbank35p
003300   05  WS-TRAN-ID                            PIC X(4).            sbank35p
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             sbank35p
003500       VALUE SPACES.                                              sbank35p
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             sbank35p
003700       VALUE 'UNKNOWN'.                                           sbank35p
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      sbank35p
003900   05  WS-WORK1                              PIC X(1).            sbank35p
         05  WS-VERSION                            PIC X(7).
                                                                        sbank35p
004000                                                                  
004100 01  MAPAREA                                 PIC X(2048).         sbank35p
004200 COPY MBANK35.                                                    sbank35p
004300                                                                  sbank35p
004400 01  WS-TIME-DATE-WORK-AREA.                                      sbank35p
004500 COPY CDATED.                                                     sbank35p
004600                                                                  sbank35p
004700 01  WS-BANK-DATA-AREAS.                                          sbank35p
004800   05  WS-BANK-DATA.                                              sbank35p
004900 COPY CBANKDAT.                                                   sbank35p
005000   05  WS-BANK-EXT-DATA.                                          sbank35p
005100 COPY CBANKEXT.                                                   sbank35p
005200                                                                  sbank35p
005300 COPY CSCRNHDD.                                                   sbank35p
005400                                                                  sbank35p
005500 COPY CVERSND.                                                    sbank35p
005600                                                                  sbank35p
005700 COPY DFHAID.                                                     sbank35p
005800                                                                  sbank35p
005900 COPY DFHBMSCA.                                                   sbank35p
006000                                                                  sbank35p
006100 COPY CABENDD.                                                    sbank35p
006200                                                                  sbank35p
006300 LINKAGE SECTION.                                                 sbank35p
006400 01  DFHCOMMAREA.                                                 sbank35p
006500   05  FILLER                                PIC X(1)             sbank35p
006600       OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.             sbank35p
006700                                                                  sbank35p
006800 PROCEDURE DIVISION.                                              sbank35p
006900***************************************************************** sbank35p
007000* Write entry to log to show we have been invoked               * sbank35p
007100***************************************************************** sbank35p
007200     COPY CTRACE.                                                 sbank35p
007300                                                                  sbank35p
007400***************************************************************** sbank35p
007500* Store our transaction-id                                      * sbank35p
007600***************************************************************** sbank35p
007700     MOVE EIBTRNID TO WS-TRAN-ID.                                 sbank35p
007800                                                                  sbank35p
007900***************************************************************** sbank35p
008000* Store passed data or abend if there wasn't any                * sbank35p
008100***************************************************************** sbank35p
008200     IF EIBCALEN IS EQUAL TO 0                                    sbank35p
008300        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       sbank35p
008400        MOVE '0001' TO ABEND-CODE                                 sbank35p
008500        MOVE SPACES TO ABEND-REASON                               sbank35p
008600        COPY CABENDPO.                                            sbank35p
008700     ELSE                                                         sbank35p
008800        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        sbank35p
008900        MOVE LOW-VALUES TO WS-BANK-DATA                           sbank35p
009000        MOVE DFHCOMMAREA (1:EIBCALEN)                             sbank35p
009100          TO WS-BANK-DATA-AREAS (1:LENGTH OF WS-BANK-DATA-AREAS)  sbank35p
009200     END-IF.                                                      sbank35p
009300                                                                  sbank35p
009400***************************************************************** sbank35p
009500* This is the main process                                      * sbank35p
009600***************************************************************** sbank35p
009700                                                                  sbank35p
009800***************************************************************** sbank35p
009900* Determine what we have to do (read from or send to screen)    * sbank35p
010000***************************************************************** sbank35p
010100     MOVE LOW-VALUE TO MAPAREA.                                   sbank35p
010200     EVALUATE TRUE                                                sbank35p
010300       WHEN BANK-MAP-FUNCTION-GET                                 sbank35p
010400         PERFORM SCREEN35-READ THRU                               sbank35p
010500                 SCREEN35-READ-EXIT                               sbank35p
010600       WHEN BANK-MAP-FUNCTION-PUT                                 sbank35p
010700         PERFORM SCREEN35-BUILD-AND-SEND THRU                     sbank35p
010800                 SCREEN35-BUILD-AND-SEND-EXIT                     sbank35p
010900       WHEN OTHER                                                 sbank35p
011000         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      sbank35p
011100         MOVE '0002' TO ABEND-CODE                                sbank35p
011200         MOVE SPACES TO ABEND-REASON                              sbank35p
011300         COPY CABENDPO.                                           sbank35p
011400     END-EVALUATE.                                                sbank35p
011500                                                                  sbank35p
011600* Call the appropriate routine to handle the business logic       sbank35p
011700     IF BANK-MAP-FUNCTION-GET                                     sbank35p
011800        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             sbank35p
011900                       COMMAREA(WS-BANK-DATA)                     sbank35p
012000                       LENGTH(LENGTH OF WS-BANK-DATA)             sbank35p
012100        END-EXEC                                                  sbank35p
012200     END-IF.                                                      sbank35p
012300                                                                  sbank35p
012400***************************************************************** sbank35p
012500* Now we have to have finished and can return to our invoker.   * sbank35p
012600***************************************************************** sbank35p
012700* Now return to CICS                                              sbank35p
012800     MOVE WS-BANK-DATA-AREAS (1:LENGTH OF WS-BANK-DATA-AREAS)     sbank35p
012900       TO DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      sbank35p
013000     EXEC CICS                                                    sbank35p
013100          RETURN                                                  sbank35p
013200     END-EXEC.                                                    sbank35p
013300     GOBACK.                                                      sbank35p
013400                                                                  sbank35p
013500***************************************************************** sbank35p
013600* Screen processing for MBANK35                                 * sbank35p
013700*---------------------------------------------------------------* sbank35p
013800* Retrieve data from screen and format it                       * sbank35p
013900***************************************************************** sbank35p
014000 SCREEN35-READ.                                                   sbank35p
014100     MOVE 'BBANK35P' TO WS-BUSINESS-LOGIC-PGM.                    sbank35p
014200     IF BANK-AID-CLEAR                                            sbank35p
014300        SET BANK-AID-PFK03 TO TRUE                                sbank35p
014400        GO TO SCREEN35-READ-EXIT                                  sbank35p
014500     END-IF.                                                      sbank35p
014600     IF BANK-ENV-CICS                                             sbank35p
014700        GO TO SCREEN35-READ-CICS                                  sbank35p
014800     ELSE                                                         sbank35p
014900        GO TO SCREEN35-READ-INET                                  sbank35p
015000     END-IF.                                                      sbank35p
015100                                                                  sbank35p
015200 SCREEN35-READ-CICS.                                              sbank35p
015300     IF BANK-HELP-INACTIVE                                        sbank35p
015400        EXEC CICS RECEIVE MAP('BANK35A')                          sbank35p
015500                          MAPSET('MBANK35')                       sbank35p
015600        END-EXEC                                                  sbank35p
015700     ELSE                                                         sbank35p
015800        EXEC CICS RECEIVE MAP('HELP35A')                          sbank35p
015900                          MAPSET('MBANK35')                       sbank35p
016000        END-EXEC                                                  sbank35p
016100        GO TO SCREEN35-READ-EXIT                                  sbank35p
016200     END-IF.                                                      sbank35p
016300                                                                  sbank35p
016400     GO TO SCREEN35-READ-EXIT.                                    sbank35p
016500                                                                  sbank35p
016600 SCREEN35-READ-INET.                                              sbank35p
016700     GO TO SCREEN35-READ-EXIT.                                    sbank35p
016800                                                                  sbank35p
016900 SCREEN35-READ-EXIT.                                              sbank35p
017000     EXIT.                                                        sbank35p
017100                                                                  sbank35p
017200***************************************************************** sbank35p
017300* Screen processing for SCREEN35 (BANK35/HELP35)                * sbank35p
017400*---------------------------------------------------------------* sbank35p
017500* Build the output screen and send it                           * sbank35p
017600***************************************************************** sbank35p
017700 SCREEN35-BUILD-AND-SEND.                                         sbank35p
017800* Clear map area, get date & time and move to the map             sbank35p
017900     MOVE LOW-VALUES TO BANK35AO.                                 sbank35p
018000     MOVE EIBTIME TO DD-TIME-INPUT-N.                             sbank35p
018100     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      sbank35p
018200     SET DDI-YYDDD TO TRUE.                                       sbank35p
018300     SET DDO-DD-MMM-YYYY TO TRUE.                                 sbank35p
018400     PERFORM CALL-DATECONV THRU                                   sbank35p
018500             CALL-DATECONV-EXIT.                                  sbank35p
018600* Ensure the last map fields are correct                          sbank35p
018700     IF BANK-HELP-ACTIVE                                          sbank35p
018800        MOVE 'MBANK35' TO BANK-LAST-MAPSET                        sbank35p
018900        MOVE 'HELP35A' TO BANK-LAST-MAP                           sbank35p
019000     ELSE                                                         sbank35p
019100        MOVE 'MBANK35' TO BANK-LAST-MAPSET                        sbank35p
019200        MOVE 'BANK35A' TO BANK-LAST-MAP                           sbank35p
019300     END-IF.                                                      sbank35p
019400     IF BANK-ENV-CICS                                             sbank35p
019500        GO TO SCREEN35-BUILD-AND-SEND-CICS                        sbank35p
019600     ELSE                                                         sbank35p
019700        GO TO SCREEN35-BUILD-AND-SEND-INET                        sbank35p
019800     END-IF.                                                      sbank35p
019900                                                                  sbank35p
020000 SCREEN35-BUILD-AND-SEND-CICS.                                    sbank35p
020100     IF BANK-LAST-MAP IS EQUAL TO 'BANK35A'                       sbank35p
020200        GO TO BANK35-BUILD-AND-SEND-CICS                          sbank35p
020300     END-IF.                                                      sbank35p
020400     IF BANK-LAST-MAP IS EQUAL TO 'HELP35A'                       sbank35p
020500        GO TO HELP35-BUILD-AND-SEND-CICS                          sbank35p
020600     END-IF.                                                      sbank35p
020700     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                          sbank35p
020800     MOVE '0003' TO ABEND-CODE                                    sbank35p
020900     MOVE SPACES TO ABEND-REASON                                  sbank35p
021000     COPY CABENDPO.                                               sbank35p
021100     GOBACK.                                                      sbank35p
021200                                                                  sbank35p
021300 BANK35-BUILD-AND-SEND-CICS.                                      sbank35p
021400     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANK35AO==.        sbank35p
021500     COPY CVERSNP1 REPLACING ==<<SCRN>>== BY ==BANK35AO==.        sbank35p
021600     MOVE WS-TRAN-ID TO TRANO IN BANK35AO.                        sbank35p
021700     MOVE DD-TIME-OUTPUT TO TIMEO IN BANK35AO.                    sbank35p
021800     MOVE DDO-DATA TO DATEO IN BANK35AO.                          sbank35p
021900* Move in any error message                                       sbank35p
022000     MOVE BANK-ERROR-MSG TO ERRMSGO IN BANK35AO.                  sbank35p
022100* Move in screen specific fields                                  sbank35p
022200     MOVE BANK-USERID TO USERIDO IN BANK35AO.                     sbank35p
022300     MOVE BANK-USERID-NAME TO USERNMO IN BANK35AO.                sbank35p
022400                                                                  sbank35p
022500     MOVE BANK-SCR35-ACC TO ACCNOO IN BANK35AO.                   sbank35p
022600     MOVE BANK-SCR35-DSC TO ACCTYPEO IN BANK35AO.                 sbank35p
022700                                                                  sbank35p
022800     MOVE BANK-SCR35-BAL TO BALO IN BANK35AO.                     sbank35p
022900     MOVE BANK-SCR35-DTE TO DTEO IN BANK35AO.                     sbank35p
023000                                                                  sbank35p
023100     MOVE BANK-SCR35-TRANS TO TRANSO IN BANK35AO.                 sbank35p
023200                                                                  sbank35p
023300     IF BANK-SCR35-ATM-ENABLED IS EQUAL TO 'Y'                    sbank35p
023400        MOVE 'Yes' TO ATMVISO IN BANK35AO                         sbank35p
023500        MOVE BANK-SCR35-ATM-LIM TO ATMLIMO IN BANK35AO            sbank35p
023600        MOVE BANK-SCR35-ATM-LDTE TO ATMLDTEO IN BANK35AO          sbank35p
023700        MOVE BANK-SCR35-ATM-LAMT TO ATMLAMTO IN BANK35AO          sbank35p
023800     ELSE                                                         sbank35p
023900        MOVE 'No' TO ATMVISO IN BANK35AO                          sbank35p
024000        MOVE SPACES TO TXT11O IN BANK35AO                         sbank35p
024100        MOVE SPACES TO ATMLIMO IN BANK35AO                        sbank35p
024200        MOVE SPACES TO TXT12O IN BANK35AO                         sbank35p
024300        MOVE SPACES TO ATMLDTEO IN BANK35AO                       sbank35p
024400        MOVE SPACES TO TXT13O IN BANK35AO                         sbank35p
024500        MOVE SPACES TO ATMLAMTO IN BANK35AO                       sbank35p
024600     END-IF.                                                      sbank35p
024700     IF BANK-SCR35-RP1ACC IS NOT EQUAL TO SPACES                  sbank35p
024800        MOVE BANK-SCR35-RP1DAY TO RP1DAYO IN BANK35AO             sbank35p
024900        MOVE BANK-SCR35-RP1AMT TO RP1AMTO IN BANK35AO             sbank35p
025000        MOVE BANK-SCR35-RP1PID TO RP1PIDO IN BANK35AO             sbank35p
025100        MOVE '/' TO RP1SEPO IN BANK35AO                           sbank35p
025200        MOVE BANK-SCR35-RP1ACC TO RP1ACCO IN BANK35AO             sbank35p
025300        MOVE BANK-SCR35-RP1DTE TO RP1DTEO IN BANK35AO             sbank35p
025400     ELSE                                                         sbank35p
025500        MOVE SPACES TO RP1DAYO IN BANK35AO                        sbank35p
025600        MOVE SPACES TO RP1AMTO IN BANK35AO                        sbank35p
025700        MOVE SPACES TO RP1PIDO IN BANK35AO                        sbank35p
025800        MOVE SPACES TO RP1SEPO IN BANK35AO                        sbank35p
025900        MOVE SPACES TO RP1ACCO IN BANK35AO                        sbank35p
026000        MOVE SPACES TO RP1DTEO IN BANK35AO                        sbank35p
026100     END-IF.                                                      sbank35p
026200     IF BANK-SCR35-RP2ACC IS NOT EQUAL TO SPACES                  sbank35p
026300        MOVE BANK-SCR35-RP2DAY TO RP2DAYO IN BANK35AO             sbank35p
026400        MOVE BANK-SCR35-RP2AMT TO RP2AMTO IN BANK35AO             sbank35p
026500        MOVE BANK-SCR35-RP2PID TO RP2PIDO IN BANK35AO             sbank35p
026600        MOVE '/' TO RP2SEPO IN BANK35AO                           sbank35p
026700        MOVE BANK-SCR35-RP2ACC TO RP2ACCO IN BANK35AO             sbank35p
026800        MOVE BANK-SCR35-RP2DTE TO RP2DTEO IN BANK35AO             sbank35p
026900     ELSE                                                         sbank35p
027000        MOVE SPACES TO RP2DAYO IN BANK35AO                        sbank35p
027100        MOVE SPACES TO RP2AMTO IN BANK35AO                        sbank35p
027200        MOVE SPACES TO RP2PIDO IN BANK35AO                        sbank35p
027300        MOVE SPACES TO RP2SEPO IN BANK35AO                        sbank35p
027400        MOVE SPACES TO RP2ACCO IN BANK35AO                        sbank35p
027500        MOVE SPACES TO RP2DTEO IN BANK35AO                        sbank35p
027600     END-IF.                                                      sbank35p
027700     IF BANK-SCR35-RP3ACC IS NOT EQUAL TO SPACES                  sbank35p
027800        MOVE BANK-SCR35-RP3DAY TO RP3DAYO IN BANK35AO             sbank35p
027900        MOVE BANK-SCR35-RP3AMT TO RP3AMTO IN BANK35AO             sbank35p
028000        MOVE BANK-SCR35-RP3PID TO RP3PIDO IN BANK35AO             sbank35p
028100        MOVE '/' TO RP3SEPO IN BANK35AO                           sbank35p
028200        MOVE BANK-SCR35-RP3ACC TO RP3ACCO IN BANK35AO             sbank35p
028300        MOVE BANK-SCR35-RP3DTE TO RP3DTEO IN BANK35AO             sbank35p
028400     ELSE                                                         sbank35p
028500        MOVE SPACES TO RP3DAYO IN BANK35AO                        sbank35p
028600        MOVE SPACES TO RP3AMTO IN BANK35AO                        sbank35p
028700        MOVE SPACES TO RP3PIDO IN BANK35AO                        sbank35p
028800        MOVE SPACES TO RP3SEPO IN BANK35AO                        sbank35p
028900        MOVE SPACES TO RP3ACCO IN BANK35AO                        sbank35p
029000        MOVE SPACES TO RP3DTEO IN BANK35AO                        sbank35p
029100     END-IF.                                                      sbank35p
029200                                                                  sbank35p
029300     IF BANK-SCR35-TRANS(1:1) IS NOT NUMERIC                      sbank35p
029400        MOVE SPACES TO TXNPFKO IN BANK35AO                        sbank35p
029500     END-IF.                                                      sbank35p
029600                                                                  sbank35p
029700* Turn colour off if requ8red                                     sbank35p
029800     IF COLOUR-OFF                                                sbank35p
029900        MOVE DFHGREEN TO TXT01C IN BANK35AO                       sbank35p
030000        MOVE DFHGREEN TO SCRNC IN BANK35AO                        sbank35p
030100        MOVE DFHGREEN TO HEAD1C IN BANK35AO                       sbank35p
030200        MOVE DFHGREEN TO DATEC IN BANK35AO                        sbank35p
030300        MOVE DFHGREEN TO TXT02C IN BANK35AO                       sbank35p
030400        MOVE DFHGREEN TO TRANC IN BANK35AO                        sbank35p
030500        MOVE DFHGREEN TO HEAD2C IN BANK35AO                       sbank35p
030600        MOVE DFHGREEN TO TIMEC IN BANK35AO                        sbank35p
030700        MOVE DFHGREEN TO TXT03C IN BANK35AO                       sbank35p
030800        MOVE DFHGREEN TO USERIDC IN BANK35AO                      sbank35p
030900        MOVE DFHGREEN TO TXT13C IN BANK35AO                       sbank35p
031000        MOVE DFHGREEN TO TXT04C IN BANK35AO                       sbank35p
031100        MOVE DFHGREEN TO USERNMC IN BANK35AO                      sbank35p
031200        MOVE DFHGREEN TO TXT05C IN BANK35AO                       sbank35p
031300        MOVE DFHGREEN TO ACCNOC IN BANK35AO                       sbank35p
031400        MOVE DFHGREEN TO ACCTYPEC IN BANK35AO                     sbank35p
031500        MOVE DFHGREEN TO TXT06C IN BANK35AO                       sbank35p
031600        MOVE DFHGREEN TO BALC IN BANK35AO                         sbank35p
031700        MOVE DFHGREEN TO TXT07C IN BANK35AO                       sbank35p
031800        MOVE DFHGREEN TO DTEC IN BANK35AO                         sbank35p
031900        MOVE DFHGREEN TO TXT08C IN BANK35AO                       sbank35p
032000        MOVE DFHGREEN TO TRANSC IN BANK35AO                       sbank35p
032100        MOVE DFHGREEN TO TXT09C IN BANK35AO                       sbank35p
032200        MOVE DFHGREEN TO TXT10C IN BANK35AO                       sbank35p
032300        MOVE DFHGREEN TO ATMVISC IN BANK35AO                      sbank35p
032400        MOVE DFHGREEN TO TXT11C IN BANK35AO                       sbank35p
032500        MOVE DFHGREEN TO ATMLIMC IN BANK35AO                      sbank35p
032600        MOVE DFHGREEN TO TXT12C IN BANK35AO                       sbank35p
032700        MOVE DFHGREEN TO ATMLDTEC IN BANK35AO                     sbank35p
032800        MOVE DFHGREEN TO TXT13C IN BANK35AO                       sbank35p
032900        MOVE DFHGREEN TO ATMLAMTC IN BANK35AO                     sbank35p
033000        MOVE DFHGREEN TO TXT14C IN BANK35AO                       sbank35p
033100        MOVE DFHGREEN TO TXT15C IN BANK35AO                       sbank35p
033200        MOVE DFHGREEN TO TXT16C IN BANK35AO                       sbank35p
033300        MOVE DFHGREEN TO TXT17C IN BANK35AO                       sbank35p
033400        MOVE DFHGREEN TO TXT18C IN BANK35AO                       sbank35p
033500        MOVE DFHGREEN TO RP1DAYC IN BANK35AO                      sbank35p
033600        MOVE DFHGREEN TO RP1AMTC IN BANK35AO                      sbank35p
033700        MOVE DFHGREEN TO RP1PIDC IN BANK35AO                      sbank35p
033800        MOVE DFHGREEN TO RP1SEPC IN BANK35AO                      sbank35p
033900        MOVE DFHGREEN TO RP1ACCC IN BANK35AO                      sbank35p
034000        MOVE DFHGREEN TO RP1DTEC IN BANK35AO                      sbank35p
034100        MOVE DFHGREEN TO RP2DAYC IN BANK35AO                      sbank35p
034200        MOVE DFHGREEN TO RP2AMTC IN BANK35AO                      sbank35p
034300        MOVE DFHGREEN TO RP2PIDC IN BANK35AO                      sbank35p
034400        MOVE DFHGREEN TO RP2SEPC IN BANK35AO                      sbank35p
034500        MOVE DFHGREEN TO RP2ACCC IN BANK35AO                      sbank35p
034600        MOVE DFHGREEN TO RP2DTEC IN BANK35AO                      sbank35p
034700        MOVE DFHGREEN TO RP3DAYC IN BANK35AO                      sbank35p
034800        MOVE DFHGREEN TO RP3AMTC IN BANK35AO                      sbank35p
034900        MOVE DFHGREEN TO RP3PIDC IN BANK35AO                      sbank35p
035000        MOVE DFHGREEN TO RP3SEPC IN BANK35AO                      sbank35p
035100        MOVE DFHGREEN TO RP3ACCC IN BANK35AO                      sbank35p
035200        MOVE DFHGREEN TO RP3DTEC IN BANK35AO                      sbank35p
035300        MOVE DFHGREEN TO ERRMSGC IN BANK35AO                      sbank35p
035400        MOVE DFHGREEN TO TXT19C IN BANK35AO                       sbank35p
035500        MOVE DFHGREEN TO TXNPFKC IN BANK35AO                      sbank35p
035600        MOVE DFHGREEN TO VERC IN BANK35AO                         sbank35p
035700     END-IF.                                                      sbank35p
035800                                                                  sbank35p
035900     EXEC CICS SEND MAP('BANK35A')                                sbank35p
036000                    MAPSET('MBANK35')                             sbank35p
036100                    ERASE                                         sbank35p
036200                    FREEKB                                        sbank35p
036300     END-EXEC.                                                    sbank35p
036400     GO TO SCREEN35-BUILD-AND-SEND-EXIT.                          sbank35p
036500                                                                  sbank35p
036600 HELP35-BUILD-AND-SEND-CICS.                                      sbank35p
036700     COPY CSCRNHP2 REPLACING ==:OPTN:== BY ==BANK==               sbank35p
036800                             ==<<SCRN>>== BY ==HELP35AO==.        sbank35p
036900                                                                  sbank35p
037000     EXEC CICS SEND MAP('HELP35A')                                sbank35p
037100                    MAPSET('MBANK35')                             sbank35p
037200                    ERASE                                         sbank35p
037300                    FREEKB                                        sbank35p
037400     END-EXEC.                                                    sbank35p
037500     GO TO SCREEN35-BUILD-AND-SEND-EXIT.                          sbank35p
037600                                                                  sbank35p
037700 SCREEN35-BUILD-AND-SEND-INET.                                    sbank35p
037800     MOVE SPACES TO EXT-OP-DATA.                                  sbank35p
037900     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              sbank35p
038000     MOVE DDO-DATA TO EXT-OP-DATE.                                sbank35p
038100     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          sbank35p
038200     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         sbank35p
038300     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          sbank35p
038400     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          sbank35p
038500     CALL 'SVERSONP' USING VERSION.                               sbank35p
038600     MOVE VERSION TO EXT-OP-VERSION.                              sbank35p
038700* Move in screen name                                             sbank35p
038800     MOVE 'BANK35' TO EXT-OP-SCREEN.                              sbank35p
038900* Move in userid and any error message                            sbank35p
039000     MOVE BANK-ERROR-MSG TO EXT-OP-ERR-MSG.                       sbank35p
039100     MOVE BANK-USERID TO EXT-OP-USERID.                           sbank35p
039200     MOVE BANK-USERID-NAME TO EXT-OP-NAME.                        sbank35p
039300* Move in screen specific fields                                  sbank35p
039400     MOVE BANK-SCR35-ACC TO EXT-OP35-ACCNO.                       sbank35p
039500     MOVE BANK-SCR35-DSC TO EXT-OP35-ACCTYPE.                     sbank35p
039600     MOVE BANK-SCR35-BAL TO EXT-OP35-BALANCE.                     sbank35p
039700     MOVE BANK-SCR35-DTE TO EXT-OP35-STMT-DATE.                   sbank35p
039800     MOVE BANK-SCR35-ATM-ENABLED TO EXT-OP35-ATM-VIS.             sbank35p
039900     MOVE BANK-SCR35-ATM-LIM TO EXT-OP35-ATM-LIM                  sbank35p
040000     MOVE BANK-SCR35-ATM-LDTE TO EXT-OP35-ATM-LDTE.               sbank35p
040100     MOVE BANK-SCR35-ATM-LAMT TO EXT-OP35-ATM-LAMT.               sbank35p
040200     IF BANK-SCR35-RP1ACC IS NOT EQUAL TO SPACES                  sbank35p
040300        MOVE BANK-SCR35-RP1DAY TO EXT-OP35-RP-DAY(1)              sbank35p
040400        MOVE BANK-SCR35-RP1AMT TO EXT-OP35-RP-AMT(1)              sbank35p
040500        MOVE BANK-SCR35-RP1PID TO EXT-OP35-RP-PID(1)              sbank35p
040600        MOVE BANK-SCR35-RP1ACC TO EXT-OP35-RP-ACC(1)              sbank35p
040700        MOVE BANK-SCR35-RP1DTE TO EXT-OP35-RP-DTE(1)              sbank35p
040800     ELSE                                                         sbank35p
040900        MOVE SPACES TO EXT-OP35-RP-DETAILS(1)                     sbank35p
041000     END-IF.                                                      sbank35p
041100     IF BANK-SCR35-RP2ACC IS NOT EQUAL TO SPACES                  sbank35p
041200        MOVE BANK-SCR35-RP2DAY TO EXT-OP35-RP-DAY(2)              sbank35p
041300        MOVE BANK-SCR35-RP2AMT TO EXT-OP35-RP-AMT(2)              sbank35p
041400        MOVE BANK-SCR35-RP2PID TO EXT-OP35-RP-PID(2)              sbank35p
041500        MOVE BANK-SCR35-RP2ACC TO EXT-OP35-RP-ACC(2)              sbank35p
041600        MOVE BANK-SCR35-RP2DTE TO EXT-OP35-RP-DTE(2)              sbank35p
041700     ELSE                                                         sbank35p
041800        MOVE SPACES TO EXT-OP35-RP-DETAILS(2)                     sbank35p
041900     END-IF.                                                      sbank35p
042000     IF BANK-SCR35-RP3ACC IS NOT EQUAL TO SPACES                  sbank35p
042100        MOVE BANK-SCR35-RP3DAY TO EXT-OP35-RP-DAY(3)              sbank35p
042200        MOVE BANK-SCR35-RP3AMT TO EXT-OP35-RP-AMT(3)              sbank35p
042300        MOVE BANK-SCR35-RP3PID TO EXT-OP35-RP-PID(3)              sbank35p
042400        MOVE BANK-SCR35-RP3ACC TO EXT-OP35-RP-ACC(3)              sbank35p
042500        MOVE BANK-SCR35-RP3DTE TO EXT-OP35-RP-DTE(3)              sbank35p
042600     ELSE                                                         sbank35p
042700        MOVE SPACES TO EXT-OP35-RP-DETAILS(3)                     sbank35p
042800     END-IF.                                                      sbank35p
042900                                                                  sbank35p
043000 SCREEN35-BUILD-AND-SEND-EXIT.                                    sbank35p
043100     EXIT.                                                        sbank35p
043200                                                                  sbank35p
043300***************************************************************** sbank35p
043400* Call common routine to perform date conversions               * sbank35p
043500***************************************************************** sbank35p
043600 CALL-DATECONV.                                                   sbank35p
043700     MOVE BANK-ENV TO DD-ENV.                                     sbank35p
043800     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           sbank35p
043900     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            sbank35p
044000 CALL-DATECONV-EXIT.                                              sbank35p
044100     EXIT.                                                        sbank35p
044200                                                                  sbank35p
044300* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     sbank35p
