000100***************************************************************** bbank60p
000200*                                                               * bbank60p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * bbank60p
000400*   This demonstration program is provided for use by users     * bbank60p
000500*   of Micro Focus products and may be used, modified and       * bbank60p
000600*   distributed as part of your application provided that       * bbank60p
000700*   you properly acknowledge the copyright of Micro Focus       * bbank60p
000800*   in this material.                                           * bbank60p
000900*                                                               * bbank60p
001000***************************************************************** bbank60p
001100                                                                  bbank60p
001200***************************************************************** bbank60p
001300* Program:     BBANK60P.CBL                                     * bbank60p
001400* Layer:       Business logic                                   * bbank60p
001500* Function:    Update address information                       * bbank60p
001600***************************************************************** bbank60p
001700                                                                  bbank60p
001800 IDENTIFICATION DIVISION.                                         bbank60p
001900 PROGRAM-ID.                                                      bbank60p
002000     BBANK60P.                                                    bbank60p
002100 DATE-WRITTEN.                                                    bbank60p
002200     September 2002.                                              bbank60p
002300 DATE-COMPILED.                                                   bbank60p
002400     Today.                                                       bbank60p
002500                                                                  bbank60p
002600 ENVIRONMENT DIVISION.                                            bbank60p
002700                                                                  bbank60p
002800 DATA DIVISION.                                                   bbank60p
002900 WORKING-STORAGE SECTION.                                         bbank60p
003000 01  WS-MISC-STORAGE.                                             bbank60p
003100   05  WS-PROGRAM-ID                         PIC X(8)             bbank60p
003200       VALUE 'BBANK60P'.                                          bbank60p
003300   05  WS-INPUT-FLAG                         PIC X(1).            bbank60p
003400     88  INPUT-OK                            VALUE '0'.           bbank60p
003500     88  INPUT-ERROR                         VALUE '1'.           bbank60p
003600   05  WS-RETURN-FLAG                        PIC X(1).            bbank60p
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    bbank60p
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           bbank60p
003900   05  WS-RETURN-MSG                         PIC X(75).           bbank60p
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        bbank60p
004100   05  WS-PFK-FLAG                           PIC X(1).            bbank60p
004200     88  PFK-VALID                           VALUE '0'.           bbank60p
004300     88  PFK-INVALID                         VALUE '1'.           bbank60p
004400   05  WS-ERROR-MSG                          PIC X(75).           bbank60p
004500   05  WS-ADDR-CHANGE                        PIC X(1).            bbank60p
004600     88  ADDR-DATA-UNCHANGED                 VALUE '0'.           bbank60p
004700     88  ADDR-DATA-CHANGED                   VALUE '1'.           bbank60p
004800                                                                  bbank60p
004900 01  WS-BANK-DATA.                                                bbank60p
005000 COPY CBANKDAT.                                                   bbank60p
005100                                                                  bbank60p
005200 01  WS-HELP-DATA.                                                bbank60p
005300 COPY CHELPD01.                                                   bbank60p
005400                                                                  bbank60p
005500 01  WS-ADDR-DATA.                                                bbank60p
005600 COPY CBANKD02.                                                   bbank60p
005700                                                                  bbank60p
005800 COPY CBANKD07.                                                   bbank60p
005900                                                                  bbank60p
006000 COPY CSTATESD.                                                   bbank60p
006100                                                                  bbank60p
006200 COPY CABENDD.                                                    bbank60p
006300                                                                  bbank60p
006400 LINKAGE SECTION.                                                 bbank60p
006500 01  DFHCOMMAREA.                                                 bbank60p
006600   05  LK-COMMAREA                           PIC X(6144).         bbank60p
006700                                                                  bbank60p
006800 COPY CENTRY.                                                     bbank60p
006900***************************************************************** bbank60p
007000* Make ourselves re-entrant                                     * bbank60p
007100***************************************************************** bbank60p
007200     MOVE SPACES TO WS-ERROR-MSG.                                 bbank60p
007300                                                                  bbank60p
007400***************************************************************** bbank60p
007500* Move the passed area to our area                              * bbank60p
007600***************************************************************** bbank60p
007700     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. bbank60p
007800                                                                  bbank60p
007900***************************************************************** bbank60p
008000* Ensure error message is cleared                               * bbank60p
008100***************************************************************** bbank60p
008200     MOVE SPACES TO BANK-ERROR-MSG.                               bbank60p
008300                                                                  bbank60p
008400***************************************************************** bbank60p
008500* This is the main process                                      * bbank60p
008600***************************************************************** bbank60p
008700                                                                  bbank60p
008800***************************************************************** bbank60p
008900* Save the passed return flag and then turn it off              * bbank60p
009000***************************************************************** bbank60p
009100     MOVE BANK-RETURN-FLAG TO WS-RETURN-FLAG.                     bbank60p
009200     SET BANK-RETURN-FLAG-OFF TO TRUE.                            bbank60p
009300                                                                  bbank60p
009400***************************************************************** bbank60p
009500* Check the AID to see if its valid at this point               * bbank60p
009600***************************************************************** bbank60p
009700     SET PFK-INVALID TO TRUE.                                     bbank60p
009800     IF BANK-AID-ENTER OR                                         bbank60p
009900        BANK-AID-PFK03 OR                                         bbank60p
010000        BANK-AID-PFK04 OR                                         bbank60p
010100        BANK-AID-PFK10                                            bbank60p
010200        SET PFK-VALID TO TRUE                                     bbank60p
010300     END-IF.                                                      bbank60p
010400     IF BANK-AID-PFK01 AND                                        bbank60p
010500        BANK-HELP-INACTIVE                                        bbank60p
010600        SET BANK-HELP-ACTIVE TO TRUE                              bbank60p
010700        SET PFK-VALID TO TRUE                                     bbank60p
010800     END-IF.                                                      bbank60p
010900     IF PFK-INVALID                                               bbank60p
011000        SET BANK-AID-ENTER TO TRUE                                bbank60p
011100     END-IF.                                                      bbank60p
011200                                                                  bbank60p
011300***************************************************************** bbank60p
011400* Check the AID to see if we have to quit                       * bbank60p
011500***************************************************************** bbank60p
011600     IF BANK-AID-PFK03                                            bbank60p
011700        MOVE 'BBANK60P' TO BANK-LAST-PROG                         bbank60p
011800        MOVE 'BBANK99P' TO BANK-NEXT-PROG                         bbank60p
011900        MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        bbank60p
012000        MOVE 'BANK99A' TO BANK-NEXT-MAP                           bbank60p
012100        GO TO COMMON-RETURN                                       bbank60p
012200     END-IF.                                                      bbank60p
012300                                                                  bbank60p
012400***************************************************************** bbank60p
012500* Check the to see if user needs or has been using help         * bbank60p
012600***************************************************************** bbank60p
012700     IF BANK-HELP-ACTIVE                                          bbank60p
012800        IF BANK-AID-PFK04                                         bbank60p
012900           SET BANK-HELP-INACTIVE TO TRUE                         bbank60p
013000           MOVE 00 TO BANK-HELP-SCREEN                            bbank60p
013100           MOVE 'BBANK60P' TO BANK-LAST-PROG                      bbank60p
013200           MOVE 'BBANK60P' TO BANK-NEXT-PROG                      bbank60p
013300           MOVE 'MBANK60' TO BANK-LAST-MAPSET                     bbank60p
013400           MOVE 'HELP60A' TO BANK-LAST-MAP                        bbank60p
013500           MOVE 'MBANK60' TO BANK-NEXT-MAPSET                     bbank60p
013600           MOVE 'BANK60A' TO BANK-NEXT-MAP                        bbank60p
013700           GO TO COMMON-RETURN                                    bbank60p
013800        ELSE                                                      bbank60p
013900           MOVE 01 TO BANK-HELP-SCREEN                            bbank60p
014000           MOVE 'BBANK60P' TO BANK-LAST-PROG                      bbank60p
014100           MOVE 'BBANK60P' TO BANK-NEXT-PROG                      bbank60p
014200           MOVE 'MBANK60' TO BANK-LAST-MAPSET                     bbank60p
014300           MOVE 'BANK60A' TO BANK-LAST-MAP                        bbank60p
014400           MOVE 'MBANK60' TO BANK-NEXT-MAPSET                     bbank60p
014500           MOVE 'HELP60A' TO BANK-NEXT-MAP                        bbank60p
014600           MOVE 'BANK60' TO HELP01I-SCRN                          bbank60p
014700           COPY CHELPX01.                                         bbank60p
014800           MOVE HELP01O-DATA TO BANK-HELP-DATA                    bbank60p
014900           GO TO COMMON-RETURN                                    bbank60p
015000     END-IF.                                                      bbank60p
015100                                                                  bbank60p
015200***************************************************************** bbank60p
015300* Check the AID to see if we have to return to previous screen  * bbank60p
015400***************************************************************** bbank60p
015500     IF BANK-AID-PFK04                                            bbank60p
015600        MOVE 'BBANK60P' TO BANK-LAST-PROG                         bbank60p
015700        MOVE 'BBANK20P' TO BANK-NEXT-PROG                         bbank60p
015800        MOVE 'MBANK20' TO BANK-NEXT-MAPSET                        bbank60p
015900        MOVE 'BANK20A' TO BANK-NEXT-MAP                           bbank60p
016000        SET BANK-AID-ENTER TO TRUE                                bbank60p
016100        GO TO COMMON-RETURN                                       bbank60p
016200     END-IF.                                                      bbank60p
016300                                                                  bbank60p
016400* Check if we have set the screen up before or is this 1st time   bbank60p
016500     IF BANK-LAST-MAPSET IS NOT EQUAL TO 'MBANK60'                bbank60p
016600        MOVE WS-RETURN-MSG TO BANK-ERROR-MSG                      bbank60p
016700        MOVE 'BBANK60P' TO BANK-LAST-PROG                         bbank60p
016800        MOVE 'BBANK60P' TO BANK-NEXT-PROG                         bbank60p
016900        MOVE 'MBANK60' TO BANK-LAST-MAPSET                        bbank60p
017000        MOVE 'BANK60A' TO BANK-LAST-MAP                           bbank60p
017100        MOVE 'MBANK60' TO BANK-NEXT-MAPSET                        bbank60p
017200        MOVE 'BANK60A' TO BANK-NEXT-MAP                           bbank60p
017300        PERFORM POPULATE-SCREEN-DATA THRU                         bbank60p
017400                POPULATE-SCREEN-DATA-EXIT                         bbank60p
017500        GO TO COMMON-RETURN                                       bbank60p
017600     END-IF.                                                      bbank60p
017700                                                                  bbank60p
017800***************************************************************** bbank60p
017900* Check to see if the data changed                              * bbank60p
018000***************************************************************** bbank60p
018100     IF ADDR-CHANGE-REQUEST                                       bbank60p
018200        IF BANK-SCR60-OLD-ADDR1 IS NOT EQUAL TO                   bbank60p
018300             BANK-SCR60-NEW-ADDR1 OR                              bbank60p
018400           BANK-SCR60-OLD-ADDR2 IS NOT EQUAL TO                   bbank60p
018500             BANK-SCR60-NEW-ADDR2 OR                              bbank60p
018600           BANK-SCR60-OLD-STATE IS NOT EQUAL TO                   bbank60p
018700             BANK-SCR60-NEW-STATE OR                              bbank60p
018800           BANK-SCR60-OLD-CNTRY IS NOT EQUAL TO                   bbank60p
018900             BANK-SCR60-NEW-CNTRY OR                              bbank60p
019000           BANK-SCR60-OLD-PSTCDE IS NOT EQUAL TO                  bbank60p
019100             BANK-SCR60-NEW-PSTCDE OR                             bbank60p
019200           BANK-SCR60-OLD-TELNO IS NOT EQUAL TO                   bbank60p
019300             BANK-SCR60-NEW-TELNO OR                              bbank60p
019400           BANK-SCR60-OLD-EMAIL IS NOT EQUAL TO                   bbank60p
019500             BANK-SCR60-NEW-EMAIL OR                              bbank60p
019600           BANK-SCR60-OLD-SEND-MAIL IS NOT EQUAL TO               bbank60p
019700             BANK-SCR60-NEW-SEND-MAIL OR                          bbank60p
019800           BANK-SCR60-OLD-SEND-EMAIL IS NOT EQUAL TO              bbank60p
019900             BANK-SCR60-NEW-SEND-EMAIL                            bbank60p
020000          SET ADDR-DATA-CHANGED TO TRUE                           bbank60p
020100        ELSE                                                      bbank60p
020200          SET ADDR-DATA-UNCHANGED TO TRUE                         bbank60p
020300        END-IF                                                    bbank60p
020400     END-IF.                                                      bbank60p
020500* Data has changed, we need to validate changes                   bbank60p
020600     IF ADDR-DATA-CHANGED AND                                     bbank60p
020700        ADDR-CHANGE-REQUEST                                       bbank60p
020800        PERFORM VALIDATE-DATA THRU                                bbank60p
020900                VALIDATE-DATA-EXIT                                bbank60p
021000        IF INPUT-ERROR                                            bbank60p
021100           MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                    bbank60p
021200           MOVE 'BBANK60P' TO BANK-LAST-PROG                      bbank60p
021300           MOVE 'BBANK60P' TO BANK-NEXT-PROG                      bbank60p
021400           MOVE 'MBANK60' TO BANK-LAST-MAPSET                     bbank60p
021500           MOVE 'BANK60A' TO BANK-LAST-MAP                        bbank60p
021600           MOVE 'MBANK60' TO BANK-NEXT-MAPSET                     bbank60p
021700           MOVE 'BANK60A' TO BANK-NEXT-MAP                        bbank60p
021800           GO TO COMMON-RETURN                                    bbank60p
021900        END-IF                                                    bbank60p
022000     END-IF.                                                      bbank60p
022100                                                                  bbank60p
022200* Data has changed, we need to verify the change                  bbank60p
022300     IF ADDR-DATA-CHANGED AND                                     bbank60p
022400        ADDR-CHANGE-REQUEST                                       bbank60p
022500        MOVE 'Please use F10 to confirm changes' TO WS-ERROR-MSG  bbank60p
022600        SET ADDR-CHANGE-VERIFY TO TRUE                            bbank60p
022700        MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                       bbank60p
022800        MOVE 'BBANK60P' TO BANK-LAST-PROG                         bbank60p
022900        MOVE 'BBANK60P' TO BANK-NEXT-PROG                         bbank60p
023000        MOVE 'MBANK60' TO BANK-LAST-MAPSET                        bbank60p
023100        MOVE 'BANK60A' TO BANK-LAST-MAP                           bbank60p
023200        MOVE 'MBANK60' TO BANK-NEXT-MAPSET                        bbank60p
023300        MOVE 'BANK60A' TO BANK-NEXT-MAP                           bbank60p
023400        GO TO COMMON-RETURN                                       bbank60p
023500     END-IF.                                                      bbank60p
023600* Data was changed and verified                                   bbank60p
023700     IF ADDR-CHANGE-VERIFY AND                                    bbank60p
023800        BANK-AID-PFK10                                            bbank60p
023900        MOVE SPACES TO CD02-DATA                                  bbank60p
024000        MOVE BANK-SCR60-CONTACT-ID TO CD02I-CONTACT-ID            bbank60p
024100* Set criteria for I/O rotine                                     bbank60p
024200        SET CD02I-WRITE TO TRUE                                   bbank60p
024300* Move the new data                                               bbank60p
024400        MOVE BANK-SCR60-CONTACT-NAME TO CD02I-CONTACT-NAME        bbank60p
024500        MOVE BANK-SCR60-NEW-ADDR1 TO CD02I-CONTACT-ADDR1          bbank60p
024600        MOVE BANK-SCR60-NEW-ADDR2 TO CD02I-CONTACT-ADDR2          bbank60p
024700        MOVE BANK-SCR60-NEW-STATE TO CD02I-CONTACT-STATE          bbank60p
024800        MOVE BANK-SCR60-NEW-CNTRY TO CD02I-CONTACT-CNTRY          bbank60p
024900        MOVE BANK-SCR60-NEW-PSTCDE TO CD02I-CONTACT-PSTCDE        bbank60p
025000        MOVE BANK-SCR60-NEW-TELNO TO CD02I-CONTACT-TELNO          bbank60p
025100        MOVE BANK-SCR60-NEW-EMAIL TO CD02I-CONTACT-EMAIL          bbank60p
025200        MOVE BANK-SCR60-NEW-SEND-MAIL TO CD02I-CONTACT-SEND-MAIL  bbank60p
025300        MOVE BANK-SCR60-NEW-SEND-EMAIL TO CD02I-CONTACT-SEND-EMAILbbank60p
025400* Now go update the data                                          bbank60p
025500 COPY CBANKX02.                                                   bbank60p
025600        MOVE SPACES TO CD07-DATA                                  bbank60p
025700        MOVE BANK-SCR60-CONTACT-ID TO CD07I-PERSON-PID            bbank60p
025800        MOVE BANK-SCR60-OLD-ADDR1 TO CD07I-OLD-ADDR1              bbank60p
025900        MOVE BANK-SCR60-OLD-ADDR2 TO CD07I-OLD-ADDR2              bbank60p
026000        MOVE BANK-SCR60-OLD-STATE TO CD07I-OLD-STATE              bbank60p
026100        MOVE BANK-SCR60-OLD-CNTRY TO CD07I-OLD-CNTRY              bbank60p
026200        MOVE BANK-SCR60-OLD-PSTCDE TO CD07I-OLD-PSTCDE            bbank60p
026300        MOVE BANK-SCR60-OLD-TELNO TO CD07I-OLD-TELNO              bbank60p
026400        MOVE BANK-SCR60-OLD-EMAIL TO CD07I-OLD-EMAIL              bbank60p
026500        MOVE BANK-SCR60-OLD-SEND-MAIL TO CD07I-OLD-SEND-MAIL      bbank60p
026600        MOVE BANK-SCR60-OLD-SEND-EMAIL TO CD07I-OLD-SEND-EMAIL    bbank60p
026700        MOVE BANK-SCR60-NEW-ADDR1 TO CD07I-NEW-ADDR1              bbank60p
026800        MOVE BANK-SCR60-NEW-ADDR2 TO CD07I-NEW-ADDR2              bbank60p
026900        MOVE BANK-SCR60-NEW-STATE TO CD07I-NEW-STATE              bbank60p
027000        MOVE BANK-SCR60-NEW-CNTRY TO CD07I-NEW-CNTRY              bbank60p
027100        MOVE BANK-SCR60-NEW-PSTCDE TO CD07I-NEW-PSTCDE            bbank60p
027200        MOVE BANK-SCR60-NEW-TELNO TO CD07I-NEW-TELNO              bbank60p
027300        MOVE BANK-SCR60-NEW-EMAIL TO CD07I-NEW-EMAIL              bbank60p
027400        MOVE BANK-SCR60-NEW-SEND-MAIL TO CD07I-NEW-SEND-MAIL      bbank60p
027500        MOVE BANK-SCR60-NEW-SEND-EMAIL TO CD07I-NEW-SEND-EMAIL    bbank60p
027600 COPY CBANKX07.                                                   bbank60p
027700        MOVE 'Contact information updated' TO BANK-RETURN-MSG     bbank60p
027800        MOVE SPACES TO BANK-SCREEN60-DATA                         bbank60p
027900        MOVE 'BBANK60P' TO BANK-LAST-PROG                         bbank60p
028000        MOVE 'BBANK20P' TO BANK-NEXT-PROG                         bbank60p
028100        MOVE 'MBANK20' TO BANK-NEXT-MAPSET                        bbank60p
028200        MOVE 'BANK20A' TO BANK-NEXT-MAP                           bbank60p
028300        SET BANK-AID-ENTER TO TRUE                                bbank60p
028400        GO TO COMMON-RETURN                                       bbank60p
028500     END-IF.                                                      bbank60p
028600                                                                  bbank60p
028700* Turn off update flags and redisplay                             bbank60p
028800     SET ADDR-CHANGE-REQUEST TO TRUE.                             bbank60p
028900     MOVE 'BBANK60P' TO BANK-LAST-PROG                            bbank60p
029000     MOVE 'BBANK60P' TO BANK-NEXT-PROG                            bbank60p
029100     MOVE 'MBANK60' TO BANK-LAST-MAPSET                           bbank60p
029200     MOVE 'BANK60A' TO BANK-LAST-MAP                              bbank60p
029300     MOVE 'MBANK60' TO BANK-NEXT-MAPSET                           bbank60p
029400     MOVE 'BANK60A' TO BANK-NEXT-MAP                              bbank60p
029500     GO TO COMMON-RETURN.                                         bbank60p
029600                                                                  bbank60p
029700 COMMON-RETURN.                                                   bbank60p
029800     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). bbank60p
029900 COPY CRETURN.                                                    bbank60p
030000                                                                  bbank60p
030100 VALIDATE-DATA.                                                   bbank60p
030200     SET INPUT-OK TO TRUE.                                        bbank60p
030300     MOVE SPACES TO STATE-PROV-WK-CNTRY.                          bbank60p
030400     MOVE BANK-SCR60-NEW-CNTRY TO STATE-PROV-TMP-CNTRY            bbank60p
030500     INSPECT STATE-PROV-TMP-CNTRY                                 bbank60p
030600       CONVERTING 'abcdefghijklmnopqrstuvwxyz'                    bbank60p
030700               TO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.                   bbank60p
030800     INSPECT BANK-SCR60-NEW-STATE                                 bbank60p
030900       CONVERTING 'abcdefghijklmnopqrstuvwxyz'                    bbank60p
031000               TO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.                   bbank60p
031100     INSPECT BANK-SCR60-NEW-SEND-MAIL                             bbank60p
031200       CONVERTING 'abcdefghijklmnopqrstuvwxyz'                    bbank60p
031300               TO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.                   bbank60p
031400     INSPECT BANK-SCR60-NEW-SEND-EMAIL                            bbank60p
031500       CONVERTING 'abcdefghijklmnopqrstuvwxyz'                    bbank60p
031600               TO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.                   bbank60p
031700     IF STATE-PROV-TMP-CNTRY IS EQUAL TO 'USA'                    bbank60p
031800        MOVE 'USA' TO STATE-PROV-WK-CNTRY                         bbank60p
031900     END-IF.                                                      bbank60p
032000     IF STATE-PROV-TMP-CNTRY IS EQUAL TO 'CANADA'                 bbank60p
032100        MOVE 'CDN' TO STATE-PROV-WK-CNTRY                         bbank60p
032200     END-IF.                                                      bbank60p
032300     IF STATE-PROV-WK-CNTRY IS EQUAL TO SPACES                    bbank60p
032400        MOVE 'Country must be USA or CANADA' TO WS-ERROR-MSG      bbank60p
032500        GO TO VALIDATE-DATA-ERROR                                 bbank60p
032600     END-IF.                                                      bbank60p
032700                                                                  bbank60p
032800     MOVE 0 TO STATE-PROV-SUB.                                    bbank60p
032900     DIVIDE LENGTH OF STATE-PROV-DATA (1) INTO                    bbank60p
033000       LENGTH OF STATE-PROV-TABLE                                 bbank60p
033100         GIVING STATE-PROV-COUNT.                                 bbank60p
033200 VALIDATE-DATA-LOOP1.                                             bbank60p
033300     ADD 1 TO STATE-PROV-SUB.                                     bbank60p
033400     IF STATE-PROV-SUB IS GREATER THAN STATE-PROV-COUNT           bbank60p
033500        MOVE 'Invlaid State/Prov code' TO WS-ERROR-MSG            bbank60p
033600        GO TO VALIDATE-DATA-ERROR                                 bbank60p
033700     END-IF.                                                      bbank60p
033800     IF BANK-SCR60-NEW-STATE IS EQUAL TO                          bbank60p
033900          STATE-PROV-CODE (STATE-PROV-SUB)                        bbank60p
034000        GO TO VALIDATE-DATA-LOOP1-EXIT                            bbank60p
034100     END-IF.                                                      bbank60p
034200     GO TO VALIDATE-DATA-LOOP1.                                   bbank60p
034300 VALIDATE-DATA-LOOP1-EXIT.                                        bbank60p
034400     IF STATE-PROV-CNTRY (STATE-PROV-SUB) IS NOT EQUAL TO         bbank60p
034500        STATE-PROV-WK-CNTRY                                       bbank60p
034600        MOVE 'State/Prov not valid for Country' TO WS-ERROR-MSG   bbank60p
034700        GO TO VALIDATE-DATA-ERROR                                 bbank60p
034800     END-IF.                                                      bbank60p
034900     IF BANK-SCR60-NEW-EMAIL IS NOT EQUAL TO SPACES               bbank60p
035000        MOVE 0 TO STATE-PROV-SUB                                  bbank60p
035100        INSPECT BANK-SCR60-NEW-EMAIL TALLYING STATE-PROV-SUB      bbank60p
035200          FOR ALL '@'                                             bbank60p
035300        IF STATE-PROV-SUB IS NOT EQUAL TO 1                       bbank60p
035400           MOVE 'E-Mail address format invalid' TO WS-ERROR-MSG   bbank60p
035500           GO TO VALIDATE-DATA-ERROR                              bbank60p
035600        END-IF                                                    bbank60p
035700     END-IF.                                                      bbank60p
035800     IF BANK-SCR60-NEW-SEND-MAIL IS NOT EQUAL TO ' ' AND          bbank60p
035900        BANK-SCR60-NEW-SEND-MAIL IS NOT EQUAL TO 'N' AND          bbank60p
036000        BANK-SCR60-NEW-SEND-MAIL IS NOT EQUAL TO 'Y'              bbank60p
036100        MOVE 'Send mail must be blank, Y or N' TO WS-ERROR-MSG    bbank60p
036200        GO TO VALIDATE-DATA-ERROR                                 bbank60p
036300     END-IF.                                                      bbank60p
036400     IF BANK-SCR60-NEW-SEND-EMAIL IS NOT EQUAL TO ' ' AND         bbank60p
036500        BANK-SCR60-NEW-SEND-EMAIL IS NOT EQUAL TO 'N' AND         bbank60p
036600        BANK-SCR60-NEW-SEND-EMAIL IS NOT EQUAL TO 'Y'             bbank60p
036700        MOVE 'Send E-Mail must be blank, Y or N' TO WS-ERROR-MSG  bbank60p
036800        GO TO VALIDATE-DATA-ERROR                                 bbank60p
036900     END-IF.                                                      bbank60p
037000     IF BANK-SCR60-NEW-SEND-EMAIL IS EQUAL TO 'Y' AND             bbank60p
037100        BANK-SCR60-NEW-EMAIL IS EQUAL TO SPACES                   bbank60p
037200        MOVE 'Send E-Mail required E-Mail address' TO WS-ERROR-MSGbbank60p
037300        GO TO VALIDATE-DATA-ERROR                                 bbank60p
037400     END-IF.                                                      bbank60p
037500                                                                  bbank60p
037600     GO TO VALIDATE-DATA-EXIT.                                    bbank60p
037700                                                                  bbank60p
037800 VALIDATE-DATA-ERROR.                                             bbank60p
037900     SET INPUT-ERROR TO TRUE.                                     bbank60p
038000 VALIDATE-DATA-EXIT.                                              bbank60p
038100     EXIT.                                                        bbank60p
038200                                                                  bbank60p
038300 POPULATE-SCREEN-DATA.                                            bbank60p
038400     MOVE SPACES TO CD02-DATA.                                    bbank60p
038500     MOVE BANK-USERID TO BANK-SCR60-CONTACT-ID.                   bbank60p
038600     MOVE BANK-SCR60-CONTACT-ID TO CD02I-CONTACT-ID.              bbank60p
038700* Set criteria for I/O rotine                                     bbank60p
038800     SET CD02I-READ TO TRUE.                                      bbank60p
038900* Now go get the data                                             bbank60p
039000 COPY CBANKX02.                                                   bbank60p
039100     MOVE SPACES TO BANK-SCR60-OLD-DETS.                          bbank60p
039200     MOVE SPACES TO BANK-SCR60-NEW-DETS.                          bbank60p
039300     IF CD02O-CONTACT-ID IS EQUAL TO CD02I-CONTACT-ID             bbank60p
039400        MOVE CD02O-CONTACT-ID TO BANK-SCR60-CONTACT-ID            bbank60p
039500        MOVE CD02O-CONTACT-NAME TO BANK-SCR60-CONTACT-NAME        bbank60p
039600        MOVE CD02O-CONTACT-ADDR1 TO BANK-SCR60-OLD-ADDR1          bbank60p
039700        MOVE CD02O-CONTACT-ADDR2 TO BANK-SCR60-OLD-ADDR2          bbank60p
039800        MOVE CD02O-CONTACT-STATE TO BANK-SCR60-OLD-STATE          bbank60p
039900        MOVE CD02O-CONTACT-CNTRY TO BANK-SCR60-OLD-CNTRY          bbank60p
040000        MOVE CD02O-CONTACT-PSTCDE TO BANK-SCR60-OLD-PSTCDE        bbank60p
040100        MOVE CD02O-CONTACT-TELNO TO BANK-SCR60-OLD-TELNO          bbank60p
040200        MOVE CD02O-CONTACT-EMAIL TO BANK-SCR60-OLD-EMAIL          bbank60p
040300        MOVE CD02O-CONTACT-SEND-MAIL TO BANK-SCR60-OLD-SEND-MAIL  bbank60p
040400        MOVE CD02O-CONTACT-SEND-EMAIL TO BANK-SCR60-OLD-SEND-EMAILbbank60p
040500        MOVE CD02O-CONTACT-ADDR1 TO BANK-SCR60-NEW-ADDR1          bbank60p
040600        MOVE CD02O-CONTACT-ADDR2 TO BANK-SCR60-NEW-ADDR2          bbank60p
040700        MOVE CD02O-CONTACT-STATE TO BANK-SCR60-NEW-STATE          bbank60p
040800        MOVE CD02O-CONTACT-CNTRY TO BANK-SCR60-NEW-CNTRY          bbank60p
040900        MOVE CD02O-CONTACT-PSTCDE TO BANK-SCR60-NEW-PSTCDE        bbank60p
041000        MOVE CD02O-CONTACT-TELNO TO BANK-SCR60-NEW-TELNO          bbank60p
041100        MOVE CD02O-CONTACT-EMAIL TO BANK-SCR60-NEW-EMAIL          bbank60p
041200        MOVE CD02O-CONTACT-SEND-MAIL TO BANK-SCR60-NEW-SEND-MAIL  bbank60p
041300        MOVE CD02O-CONTACT-SEND-EMAIL TO BANK-SCR60-NEW-SEND-EMAILbbank60p
041400     ELSE                                                         bbank60p
041500        MOVE CD02O-CONTACT-NAME TO BANK-SCR60-CONTACT-NAME        bbank60p
041600     END-IF.                                                      bbank60p
041700 POPULATE-SCREEN-DATA-EXIT.                                       bbank60p
041800     EXIT.                                                        bbank60p
041900                                                                  bbank60p
042000* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     bbank60p
