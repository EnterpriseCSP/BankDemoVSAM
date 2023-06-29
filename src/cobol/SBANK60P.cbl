000100***************************************************************** sbank60p
000200*                                                               * sbank60p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * sbank60p
000400*   This demonstration program is provided for use by users     * sbank60p
000500*   of Micro Focus products and may be used, modified and       * sbank60p
000600*   distributed as part of your application provided that       * sbank60p
000700*   you properly acknowledge the copyright of Micro Focus       * sbank60p
000800*   in this material.                                           * sbank60p
000900*                                                               * sbank60p
001000***************************************************************** sbank60p
001100                                                                  sbank60p
001200***************************************************************** sbank60p
001300* Program:     SBANK60P.CBL (CICS Version)                      * sbank60p
001400* Layer:       Screen handling                                  * sbank60p
001500* Function:    Update address & telno                           * sbank60p
001600***************************************************************** sbank60p
001700                                                                  sbank60p
001800 IDENTIFICATION DIVISION.                                         sbank60p
001900 PROGRAM-ID.                                                      sbank60p
002000     SBANK60P.                                                    sbank60p
002100 DATE-WRITTEN.                                                    sbank60p
002200     September 2002.                                              sbank60p
002300 DATE-COMPILED.                                                   sbank60p
002400     Today.                                                       sbank60p
002500                                                                  sbank60p
002600 ENVIRONMENT DIVISION.                                            sbank60p
002700                                                                  sbank60p
002800 DATA DIVISION.                                                   sbank60p
002900 WORKING-STORAGE SECTION.                                         sbank60p
003000 01  WS-MISC-STORAGE.                                             sbank60p
003100   05  WS-PROGRAM-ID                         PIC X(8)             sbank60p
003200       VALUE 'SBANK60P'.                                          sbank60p
003300   05  WS-TRAN-ID                            PIC X(4).            sbank60p
003400   05  WS-BUSINESS-LOGIC-PGM                 PIC X(8)             sbank60p
003500       VALUE SPACES.                                              sbank60p
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             sbank60p
003700       VALUE 'UNKNOWN'.                                           sbank60p
003800   05  WS-SAVED-EIBCALEN                     PIC S9(4) COMP.      sbank60p
003900   05  WS-WORK1                              PIC X(1).            sbank60p
004000   05  WS-SUB1                               PIC S9(4) COMP.      sbank60p
         05  WS-VERSION                            PIC X(7).
                                                                        sbank60p
004100                                                                  
004200 01  MAPAREA                                 PIC X(2048).         sbank60p
004300 COPY MBANK60.                                                    sbank60p
004400                                                                  sbank60p
004500 01  WS-TIME-DATE-WORK-AREA.                                      sbank60p
004600 COPY CDATED.                                                     sbank60p
004700                                                                  sbank60p
004800 01  WS-BANK-DATA-AREAS.                                          sbank60p
004900   05  WS-BANK-DATA.                                              sbank60p
005000 COPY CBANKDAT.                                                   sbank60p
005100   05  WS-BANK-EXT-DATA.                                          sbank60p
005200 COPY CBANKEXT.                                                   sbank60p
005300                                                                  sbank60p
005400 COPY CSCRNHDD.                                                   sbank60p
005500                                                                  sbank60p
005600 COPY CVERSND.                                                    sbank60p
005700                                                                  sbank60p
005800 COPY DFHAID.                                                     sbank60p
005900                                                                  sbank60p
006000 COPY DFHBMSCA.                                                   sbank60p
006100                                                                  sbank60p
006200 COPY CABENDD.                                                    sbank60p
006300                                                                  sbank60p
006400 LINKAGE SECTION.                                                 sbank60p
006500 01  DFHCOMMAREA.                                                 sbank60p
006600   05  FILLER                                PIC X(1)             sbank60p
006700       OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.             sbank60p
006800                                                                  sbank60p
006900 PROCEDURE DIVISION.                                              sbank60p
007000***************************************************************** sbank60p
007100* Write entry to log to show we have been invoked               * sbank60p
007200***************************************************************** sbank60p
007300     COPY CTRACE.                                                 sbank60p
007400                                                                  sbank60p
007500***************************************************************** sbank60p
007600* Store our transaction-id                                      * sbank60p
007700***************************************************************** sbank60p
007800     MOVE EIBTRNID TO WS-TRAN-ID.                                 sbank60p
007900                                                                  sbank60p
008000***************************************************************** sbank60p
008100* Store passed data or abend if there wasn't any                * sbank60p
008200***************************************************************** sbank60p
008300     IF EIBCALEN IS EQUAL TO 0                                    sbank60p
008400        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       sbank60p
008500        MOVE '0001' TO ABEND-CODE                                 sbank60p
008600         MOVE SPACES TO ABEND-REASON                              sbank60p
008700        COPY CABENDPO.                                            sbank60p
008800     ELSE                                                         sbank60p
008900        MOVE EIBCALEN TO WS-SAVED-EIBCALEN                        sbank60p
009000        MOVE LOW-VALUES TO WS-BANK-DATA                           sbank60p
009100        MOVE DFHCOMMAREA (1:EIBCALEN)                             sbank60p
009200          TO WS-BANK-DATA-AREAS (1:LENGTH OF WS-BANK-DATA-AREAS)  sbank60p
009300     END-IF.                                                      sbank60p
009400                                                                  sbank60p
009500***************************************************************** sbank60p
009600* This is the main process                                      * sbank60p
009700***************************************************************** sbank60p
009800                                                                  sbank60p
009900***************************************************************** sbank60p
010000* Determine what we have to do (read from or send to screen)    * sbank60p
010100***************************************************************** sbank60p
010200     MOVE LOW-VALUE TO MAPAREA.                                   sbank60p
010300     EVALUATE TRUE                                                sbank60p
010400       WHEN BANK-MAP-FUNCTION-GET                                 sbank60p
010500         PERFORM SCREEN60-READ THRU                               sbank60p
010600                 SCREEN60-READ-EXIT                               sbank60p
010700       WHEN BANK-MAP-FUNCTION-PUT                                 sbank60p
010800         PERFORM SCREEN60-BUILD-AND-SEND THRU                     sbank60p
010900                 SCREEN60-BUILD-AND-SEND-EXIT                     sbank60p
011000       WHEN OTHER                                                 sbank60p
011100         MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                      sbank60p
011200         MOVE '0002' TO ABEND-CODE                                sbank60p
011300         MOVE SPACES TO ABEND-REASON                              sbank60p
011400         COPY CABENDPO.                                           sbank60p
011500     END-EVALUATE.                                                sbank60p
011600                                                                  sbank60p
011700* Call the appropriate routine to handle the business logic       sbank60p
011800     IF BANK-MAP-FUNCTION-GET                                     sbank60p
011900        EXEC CICS LINK PROGRAM(WS-BUSINESS-LOGIC-PGM)             sbank60p
012000                       COMMAREA(WS-BANK-DATA)                     sbank60p
012100                       LENGTH(LENGTH OF WS-BANK-DATA)             sbank60p
012200        END-EXEC                                                  sbank60p
012300     END-IF.                                                      sbank60p
012400                                                                  sbank60p
012500***************************************************************** sbank60p
012600* Now we have to have finished and can return to our invoker.   * sbank60p
012700***************************************************************** sbank60p
012800* Now return to CICS                                              sbank60p
012900     MOVE WS-BANK-DATA-AREAS (1:LENGTH OF WS-BANK-DATA-AREAS)     sbank60p
013000       TO DFHCOMMAREA (1:WS-SAVED-EIBCALEN).                      sbank60p
013100     EXEC CICS                                                    sbank60p
013200          RETURN                                                  sbank60p
013300     END-EXEC.                                                    sbank60p
013400     GOBACK.                                                      sbank60p
013500                                                                  sbank60p
013600***************************************************************** sbank60p
013700* Screen processing for MBANK60                                 * sbank60p
013800*---------------------------------------------------------------* sbank60p
013900* Retrieve data from screen and format it                       * sbank60p
014000***************************************************************** sbank60p
014100 SCREEN60-READ.                                                   sbank60p
014200     MOVE 'BBANK60P' TO WS-BUSINESS-LOGIC-PGM.                    sbank60p
014300     IF BANK-AID-CLEAR                                            sbank60p
014400        SET BANK-AID-PFK03 TO TRUE                                sbank60p
014500        GO TO SCREEN60-READ-EXIT                                  sbank60p
014600     END-IF.                                                      sbank60p
014700     IF BANK-ENV-CICS                                             sbank60p
014800        GO TO SCREEN60-READ-CICS                                  sbank60p
014900     ELSE                                                         sbank60p
015000        GO TO SCREEN60-READ-INET                                  sbank60p
015100     END-IF.                                                      sbank60p
015200                                                                  sbank60p
015300 SCREEN60-READ-CICS.                                              sbank60p
015400     IF BANK-HELP-INACTIVE                                        sbank60p
015500        EXEC CICS RECEIVE MAP('BANK60A')                          sbank60p
015600                          MAPSET('MBANK60')                       sbank60p
015700        END-EXEC                                                  sbank60p
015800     ELSE                                                         sbank60p
015900        EXEC CICS RECEIVE MAP('HELP60A')                          sbank60p
016000                          MAPSET('MBANK60')                       sbank60p
016100        END-EXEC                                                  sbank60p
016200        GO TO SCREEN60-READ-EXIT                                  sbank60p
016300     END-IF.                                                      sbank60p
016400                                                                  sbank60p
016500     IF NADDR1L IN BANK60AI IS EQUAL TO 0                         sbank60p
016600        MOVE SPACES TO NADDR1I IN BANK60AI                        sbank60p
016700     END-IF.                                                      sbank60p
016800                                                                  sbank60p
016900     IF NADDR2L IN BANK60AI IS EQUAL TO 0                         sbank60p
017000        MOVE SPACES TO NADDR2I IN BANK60AI                        sbank60p
017100     END-IF.                                                      sbank60p
017200                                                                  sbank60p
017300     IF NSTATEL IN BANK60AI IS EQUAL TO 0                         sbank60p
017400        MOVE SPACES TO NSTATEI IN BANK60AI                        sbank60p
017500     END-IF.                                                      sbank60p
017600                                                                  sbank60p
017700     IF NCNTRYL IN BANK60AI IS EQUAL TO 0                         sbank60p
017800        MOVE SPACES TO NCNTRYI IN BANK60AI                        sbank60p
017900     END-IF.                                                      sbank60p
018000                                                                  sbank60p
018100     IF NPSTCDEL IN BANK60AI IS EQUAL TO 0                        sbank60p
018200        MOVE SPACES TO NPSTCDEI IN BANK60AI                       sbank60p
018300     END-IF.                                                      sbank60p
018400                                                                  sbank60p
018500     IF NTELNOL IN BANK60AI IS EQUAL TO 0                         sbank60p
018600        MOVE SPACES TO NTELNOI IN BANK60AI                        sbank60p
018700     END-IF.                                                      sbank60p
018800                                                                  sbank60p
018900     IF NEMAILL IN BANK60AI IS EQUAL TO 0                         sbank60p
019000        MOVE SPACES TO NEMAILI IN BANK60AI                        sbank60p
019100     END-IF.                                                      sbank60p
019200                                                                  sbank60p
019300     IF NSMAILL IN BANK60AI IS EQUAL TO 0                         sbank60p
019400        MOVE SPACES TO NSMAILI IN BANK60AI                        sbank60p
019500     END-IF.                                                      sbank60p
019600                                                                  sbank60p
019700     IF NSEMAILL IN BANK60AI IS EQUAL TO 0                        sbank60p
019800        MOVE SPACES TO NSEMAILI IN BANK60AI                       sbank60p
019900     END-IF.                                                      sbank60p
020000                                                                  sbank60p
020100     MOVE NADDR1I IN BANK60AI TO BANK-SCR60-NEW-ADDR1.            sbank60p
020200     MOVE NADDR2I IN BANK60AI TO BANK-SCR60-NEW-ADDR2.            sbank60p
020300     MOVE NSTATEI IN BANK60AI TO BANK-SCR60-NEW-STATE.            sbank60p
020400     MOVE NCNTRYI IN BANK60AI TO BANK-SCR60-NEW-CNTRY.            sbank60p
020500     MOVE NPSTCDEI IN BANK60AI TO BANK-SCR60-NEW-PSTCDE.          sbank60p
020600     MOVE NTELNOI IN BANK60AI TO BANK-SCR60-NEW-TELNO.            sbank60p
020700     MOVE NEMAILI IN BANK60AI TO BANK-SCR60-NEW-EMAIL.            sbank60p
020800     MOVE NSMAILI IN BANK60AI TO BANK-SCR60-NEW-SEND-MAIL.        sbank60p
020900     MOVE NSEMAILI IN BANK60AI TO BANK-SCR60-NEW-SEND-EMAIL.      sbank60p
021000                                                                  sbank60p
021100     GO TO SCREEN60-READ-EXIT.                                    sbank60p
021200                                                                  sbank60p
021300 SCREEN60-READ-INET.                                              sbank60p
021400     MOVE EXT-IP60-NADDR1 TO BANK-SCR60-NEW-ADDR1.                sbank60p
021500     MOVE EXT-IP60-NADDR2 TO BANK-SCR60-NEW-ADDR2.                sbank60p
021600     MOVE EXT-IP60-NSTATE TO BANK-SCR60-NEW-STATE.                sbank60p
021700     MOVE EXT-IP60-NCNTRY TO BANK-SCR60-NEW-CNTRY.                sbank60p
021800     MOVE EXT-IP60-NPSTCDE TO BANK-SCR60-NEW-PSTCDE.              sbank60p
021900     MOVE EXT-IP60-NTELNO TO BANK-SCR60-NEW-TELNO.                sbank60p
022000     MOVE EXT-IP60-NEMAIL TO BANK-SCR60-NEW-EMAIL.                sbank60p
022100     MOVE EXT-IP60-NSMAIL TO BANK-SCR60-NEW-SEND-MAIL.            sbank60p
022200     MOVE EXT-IP60-NSEMAIL TO BANK-SCR60-NEW-SEND-EMAIL.          sbank60p
022300     GO TO SCREEN60-READ-EXIT.                                    sbank60p
022400                                                                  sbank60p
022500 SCREEN60-READ-EXIT.                                              sbank60p
022600     EXIT.                                                        sbank60p
022700                                                                  sbank60p
022800***************************************************************** sbank60p
022900* Screen processing for SCREEN60 (BANK60/HELP60)                * sbank60p
023000*---------------------------------------------------------------* sbank60p
023100* Build the output screen and send it                           * sbank60p
023200***************************************************************** sbank60p
023300 SCREEN60-BUILD-AND-SEND.                                         sbank60p
023400* Clear map area, get date & time and move to the map             sbank60p
023500     MOVE LOW-VALUES TO BANK60AO.                                 sbank60p
023600     MOVE EIBTIME TO DD-TIME-INPUT-N.                             sbank60p
023700     MOVE EIBDATE TO DDI-DATA-YYDDD-YYDDD-N.                      sbank60p
023800     SET DDI-YYDDD TO TRUE.                                       sbank60p
023900     SET DDO-DD-MMM-YYYY TO TRUE.                                 sbank60p
024000     PERFORM CALL-DATECONV THRU                                   sbank60p
024100             CALL-DATECONV-EXIT.                                  sbank60p
024200* Ensure the last map fields are correct                          sbank60p
024300     IF BANK-HELP-ACTIVE                                          sbank60p
024400        MOVE 'MBANK60' TO BANK-LAST-MAPSET                        sbank60p
024500        MOVE 'HELP60A' TO BANK-LAST-MAP                           sbank60p
024600     ELSE                                                         sbank60p
024700        MOVE 'MBANK60' TO BANK-LAST-MAPSET                        sbank60p
024800        MOVE 'BANK60A' TO BANK-LAST-MAP                           sbank60p
024900     END-IF.                                                      sbank60p
025000     IF BANK-ENV-CICS                                             sbank60p
025100        GO TO SCREEN60-BUILD-AND-SEND-CICS                        sbank60p
025200     ELSE                                                         sbank60p
025300        GO TO SCREEN60-BUILD-AND-SEND-INET                        sbank60p
025400     END-IF.                                                      sbank60p
025500                                                                  sbank60p
025600 SCREEN60-BUILD-AND-SEND-CICS.                                    sbank60p
025700     IF BANK-LAST-MAP IS EQUAL TO 'BANK60A'                       sbank60p
025800        GO TO BANK60-BUILD-AND-SEND-CICS                          sbank60p
025900     END-IF.                                                      sbank60p
026000     IF BANK-LAST-MAP IS EQUAL TO 'HELP60A'                       sbank60p
026100        GO TO HELP60-BUILD-AND-SEND-CICS                          sbank60p
026200     END-IF.                                                      sbank60p
026300     MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                          sbank60p
026400     MOVE '0003' TO ABEND-CODE                                    sbank60p
026500     MOVE SPACES TO ABEND-REASON                                  sbank60p
026600     COPY CABENDPO.                                               sbank60p
026700     GOBACK.                                                      sbank60p
026800                                                                  sbank60p
026900 BANK60-BUILD-AND-SEND-CICS.                                      sbank60p
027000     COPY CSCRNHP1 REPLACING ==<<SCRN>>== BY ==BANK60AO==.        sbank60p
027100     COPY CVERSNP1 REPLACING ==<<SCRN>>== BY ==BANK60AO==.        sbank60p
027200     MOVE WS-TRAN-ID TO TRANO IN BANK60AO.                        sbank60p
027300     MOVE DD-TIME-OUTPUT TO TIMEO IN BANK60AO.                    sbank60p
027400     MOVE DDO-DATA TO DATEO IN BANK60AO.                          sbank60p
027500* Move in any error message                                       sbank60p
027600     MOVE BANK-ERROR-MSG TO ERRMSGO IN BANK60AO.                  sbank60p
027700* Move in screen specific fields                                  sbank60p
027800     MOVE BANK-SCR60-CONTACT-ID TO USERIDO IN BANK60AO.           sbank60p
027900     MOVE BANK-SCR60-CONTACT-NAME TO USERNMO IN BANK60AO.         sbank60p
028000                                                                  sbank60p
028100     MOVE BANK-SCR60-OLD-ADDR1 TO OADDR1O IN BANK60AO.            sbank60p
028200     MOVE BANK-SCR60-OLD-ADDR2 TO OADDR2O IN BANK60AO.            sbank60p
028300     MOVE BANK-SCR60-OLD-STATE TO OSTATEO IN BANK60AO.            sbank60p
028400     MOVE BANK-SCR60-OLD-CNTRY TO OCNTRYO IN BANK60AO.            sbank60p
028500     MOVE BANK-SCR60-OLD-PSTCDE TO OPSTCDEO IN BANK60AO.          sbank60p
028600     MOVE BANK-SCR60-OLD-TELNO TO OTELNOO IN BANK60AO.            sbank60p
028700     MOVE BANK-SCR60-OLD-EMAIL TO OEMAILO IN BANK60AO.            sbank60p
028800     MOVE BANK-SCR60-OLD-SEND-MAIL TO OSMAILO IN BANK60AO.        sbank60p
028900     MOVE BANK-SCR60-OLD-SEND-EMAIL TO OSEMAILO IN BANK60AO.      sbank60p
029000     MOVE BANK-SCR60-NEW-ADDR1 TO NADDR1O IN BANK60AO.            sbank60p
029100     MOVE BANK-SCR60-NEW-ADDR2 TO NADDR2O IN BANK60AO.            sbank60p
029200     MOVE BANK-SCR60-NEW-STATE TO NSTATEO IN BANK60AO.            sbank60p
029300     MOVE BANK-SCR60-NEW-CNTRY TO NCNTRYO IN BANK60AO.            sbank60p
029400     MOVE BANK-SCR60-NEW-PSTCDE TO NPSTCDEO IN BANK60AO.          sbank60p
029500     MOVE BANK-SCR60-NEW-TELNO TO NTELNOO IN BANK60AO.            sbank60p
029600     MOVE BANK-SCR60-NEW-EMAIL TO NEMAILO IN BANK60AO.            sbank60p
029700     MOVE BANK-SCR60-NEW-SEND-MAIL TO NSMAILO IN BANK60AO.        sbank60p
029800     MOVE BANK-SCR60-NEW-SEND-EMAIL TO NSEMAILO IN BANK60AO.      sbank60p
029900     IF ADDR-CHANGE-VERIFY                                        sbank60p
030000        MOVE DFHBMPRF TO NADDR1A IN BANK60AI                      sbank60p
030100        MOVE DFHBMPRF TO NADDR2A IN BANK60AI                      sbank60p
030200        MOVE DFHBMPRF TO NSTATEA IN BANK60AI                      sbank60p
030300        MOVE DFHBMPRF TO NCNTRYA IN BANK60AI                      sbank60p
030400        MOVE DFHBMPRF TO NADDR1A IN BANK60AI                      sbank60p
030500        MOVE DFHBMPRF TO NPSTCDEA IN BANK60AI                     sbank60p
030600        MOVE DFHBMPRF TO NTELNOA IN BANK60AI                      sbank60p
030700        MOVE DFHBMPRF TO NEMAILA IN BANK60AI                      sbank60p
030800        MOVE DFHBMPRF TO NSMAILA IN BANK60AI                      sbank60p
030900        MOVE DFHBMPRF TO NSEMAILA IN BANK60AI                     sbank60p
031000     END-IF.                                                      sbank60p
031100* Turn colour off if required                                     sbank60p
031200     IF COLOUR-OFF                                                sbank60p
031300        MOVE DFHGREEN TO TXT01C IN BANK60AO                       sbank60p
031400        MOVE DFHGREEN TO SCRNC IN BANK60AO                        sbank60p
031500        MOVE DFHGREEN TO HEAD1C IN BANK60AO                       sbank60p
031600        MOVE DFHGREEN TO DATEC IN BANK60AO                        sbank60p
031700        MOVE DFHGREEN TO TXT02C IN BANK60AO                       sbank60p
031800        MOVE DFHGREEN TO TRANC IN BANK60AO                        sbank60p
031900        MOVE DFHGREEN TO HEAD2C IN BANK60AO                       sbank60p
032000        MOVE DFHGREEN TO TIMEC IN BANK60AO                        sbank60p
032100        MOVE DFHGREEN TO TXT03C IN BANK60AO                       sbank60p
032200        MOVE DFHGREEN TO USERIDC IN BANK60AO                      sbank60p
032300        MOVE DFHGREEN TO TXT04C IN BANK60AO                       sbank60p
032400        MOVE DFHGREEN TO USERNMC IN BANK60AO                      sbank60p
032500        MOVE DFHGREEN TO TXT05C IN BANK60AO                       sbank60p
032600        MOVE DFHGREEN TO TXT06C IN BANK60AO                       sbank60p
032700        MOVE DFHGREEN TO TXT07C IN BANK60AO                       sbank60p
032800        MOVE DFHGREEN TO NADDR1C IN BANK60AO                      sbank60p
032900        MOVE DFHGREEN TO OADDR1C IN BANK60AO                      sbank60p
033000        MOVE DFHGREEN TO NADDR2C IN BANK60AO                      sbank60p
033100        MOVE DFHGREEN TO OADDR2C IN BANK60AO                      sbank60p
033200        MOVE DFHGREEN TO TXT08C IN BANK60AO                       sbank60p
033300        MOVE DFHGREEN TO NSTATEC IN BANK60AO                      sbank60p
033400        MOVE DFHGREEN TO OSTATEC IN BANK60AO                      sbank60p
033500        MOVE DFHGREEN TO TXT09C IN BANK60AO                       sbank60p
033600        MOVE DFHGREEN TO NCNTRYC IN BANK60AO                      sbank60p
033700        MOVE DFHGREEN TO OCNTRYC IN BANK60AO                      sbank60p
033800        MOVE DFHGREEN TO TXT10C IN BANK60AO                       sbank60p
033900        MOVE DFHGREEN TO NPSTCDEC IN BANK60AO                     sbank60p
034000        MOVE DFHGREEN TO OPSTCDEC IN BANK60AO                     sbank60p
034100        MOVE DFHGREEN TO TXT11C IN BANK60AO                       sbank60p
034200        MOVE DFHGREEN TO NTELNOC IN BANK60AO                      sbank60p
034300        MOVE DFHGREEN TO OTELNOC IN BANK60AO                      sbank60p
034400        MOVE DFHGREEN TO TXT12C IN BANK60AO                       sbank60p
034500        MOVE DFHGREEN TO NEMAILC IN BANK60AO                      sbank60p
034600        MOVE DFHGREEN TO OEMAILC IN BANK60AO                      sbank60p
034700        MOVE DFHGREEN TO TXT13C IN BANK60AO                       sbank60p
034800        MOVE DFHGREEN TO TXT14C IN BANK60AO                       sbank60p
034900        MOVE DFHGREEN TO NSMAILC IN BANK60AO                      sbank60p
035000        MOVE DFHGREEN TO TXT15C IN BANK60AO                       sbank60p
035100        MOVE DFHGREEN TO NSEMAILC IN BANK60AO                     sbank60p
035200        MOVE DFHGREEN TO TXT16C IN BANK60AO                       sbank60p
035300        MOVE DFHGREEN TO OSMAILC IN BANK60AO                      sbank60p
035400        MOVE DFHGREEN TO OSEMAILC IN BANK60AO                     sbank60p
035500        MOVE DFHGREEN TO ERRMSGC IN BANK60AO                      sbank60p
035600        MOVE DFHGREEN TO TXT17C IN BANK60AO                       sbank60p
035700        MOVE DFHGREEN TO VERC IN BANK60AO                         sbank60p
035800     END-IF.                                                      sbank60p
035900                                                                  sbank60p
036000     EXEC CICS SEND MAP('BANK60A')                                sbank60p
036100                    MAPSET('MBANK60')                             sbank60p
036200                    ERASE                                         sbank60p
036300                    FREEKB                                        sbank60p
036400     END-EXEC.                                                    sbank60p
036500     GO TO SCREEN60-BUILD-AND-SEND-EXIT.                          sbank60p
036600                                                                  sbank60p
036700 HELP60-BUILD-AND-SEND-CICS.                                      sbank60p
036800     COPY CSCRNHP2 REPLACING ==:OPTN:== BY ==BANK==               sbank60p
036900                             ==<<SCRN>>== BY ==HELP60AO==.        sbank60p
037000                                                                  sbank60p
037100     EXEC CICS SEND MAP('HELP60A')                                sbank60p
037200                    MAPSET('MBANK60')                             sbank60p
037300                    ERASE                                         sbank60p
037400                    FREEKB                                        sbank60p
037500     END-EXEC.                                                    sbank60p
037600     GO TO SCREEN60-BUILD-AND-SEND-EXIT.                          sbank60p
037700                                                                  sbank60p
037800 SCREEN60-BUILD-AND-SEND-INET.                                    sbank60p
037900     MOVE SPACES TO EXT-OP-DATA.                                  sbank60p
038000     MOVE WS-TRAN-ID TO EXT-OP-TRAN.                              sbank60p
038100     MOVE DDO-DATA TO EXT-OP-DATE.                                sbank60p
038200     MOVE DD-TIME-OUTPUT TO EXT-OP-TIME.                          sbank60p
038300     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         sbank60p
038400     MOVE SCREEN-TITLE1 TO EXT-OP-HEAD1.                          sbank60p
038500     MOVE SCREEN-TITLE2 TO EXT-OP-HEAD2.                          sbank60p
038600     CALL 'SVERSONP' USING SCREEN-TITLES.                         sbank60p
038700     MOVE VERSION TO EXT-OP-VERSION.                              sbank60p
038800* Move in screen name                                             sbank60p
038900     MOVE 'BANK60' TO EXT-OP-SCREEN.                              sbank60p
039000* Move in userid and any error message                            sbank60p
039100     MOVE BANK-ERROR-MSG TO EXT-OP-ERR-MSG.                       sbank60p
039200     MOVE BANK-USERID TO EXT-OP-USERID.                           sbank60p
039300     MOVE BANK-USERID-NAME TO EXT-OP-NAME.                        sbank60p
039400* Move in screen specific fields                                  sbank60p
039500     MOVE BANK-SCR60-OLD-ADDR1 TO EXT-OP60-OADDR1.                sbank60p
039600     MOVE BANK-SCR60-OLD-ADDR2 TO EXT-OP60-OADDR2.                sbank60p
039700     MOVE BANK-SCR60-OLD-STATE TO EXT-OP60-OSTATE.                sbank60p
039800     MOVE BANK-SCR60-OLD-CNTRY TO EXT-OP60-OCNTRY.                sbank60p
039900     MOVE BANK-SCR60-OLD-PSTCDE TO EXT-OP60-OPSTCDE.              sbank60p
040000     MOVE BANK-SCR60-OLD-TELNO TO EXT-OP60-OTELNO.                sbank60p
040100     MOVE BANK-SCR60-NEW-ADDR1 TO EXT-OP60-NADDR1.                sbank60p
040200     MOVE BANK-SCR60-NEW-ADDR2 TO EXT-OP60-NADDR2.                sbank60p
040300     MOVE BANK-SCR60-NEW-STATE TO EXT-OP60-NSTATE.                sbank60p
040400     MOVE BANK-SCR60-NEW-CNTRY TO EXT-OP60-NCNTRY.                sbank60p
040500     MOVE BANK-SCR60-NEW-PSTCDE TO EXT-OP60-NPSTCDE.              sbank60p
040600     MOVE BANK-SCR60-NEW-TELNO TO EXT-OP60-NTELNO.                sbank60p
040700     MOVE BANK-SCR60-NEW-EMAIL TO EXT-OP60-NEMAIL.                sbank60p
040800     MOVE BANK-SCR60-NEW-SEND-MAIL TO EXT-OP60-NSMAIL.            sbank60p
040900     MOVE BANK-SCR60-NEW-SEND-EMAIL TO EXT-OP60-NSEMAIL.          sbank60p
041000                                                                  sbank60p
041100 SCREEN60-BUILD-AND-SEND-EXIT.                                    sbank60p
041200     EXIT.                                                        sbank60p
041300                                                                  sbank60p
041400***************************************************************** sbank60p
041500* Call common routine to perform date conversions               * sbank60p
041600***************************************************************** sbank60p
041700 CALL-DATECONV.                                                   sbank60p
041800     MOVE BANK-ENV TO DD-ENV.                                     sbank60p
041900     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           sbank60p
042000     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            sbank60p
042100 CALL-DATECONV-EXIT.                                              sbank60p
042200     EXIT.                                                        sbank60p
042300                                                                  sbank60p
042400* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     sbank60p
