000100***************************************************************** bbank80p
000200*                                                               * bbank80p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * bbank80p
000400*   This demonstration program is provided for use by users     * bbank80p
000500*   of Micro Focus products and may be used, modified and       * bbank80p
000600*   distributed as part of your application provided that       * bbank80p
000700*   you properly acknowledge the copyright of Micro Focus       * bbank80p
000800*   in this material.                                           * bbank80p
000900*                                                               * bbank80p
001000***************************************************************** bbank80p
001100                                                                  bbank80p
001200***************************************************************** bbank80p
001300* Program:     BBANK80P.CBL                                     * bbank80p
001400* Layer:       Business logic                                   * bbank80p
001500* Function:    Print statements                                 * bbank80p
001600***************************************************************** bbank80p
001700                                                                  bbank80p
001800 IDENTIFICATION DIVISION.                                         bbank80p
001900 PROGRAM-ID.                                                      bbank80p
002000     BBANK80P.                                                    bbank80p
002100 DATE-WRITTEN.                                                    bbank80p
002200     September 2002.                                              bbank80p
002300 DATE-COMPILED.                                                   bbank80p
002400     Today.                                                       bbank80p
002500                                                                  bbank80p
002600 ENVIRONMENT DIVISION.                                            bbank80p
002700                                                                  bbank80p
002800 DATA DIVISION.                                                   bbank80p
002900 WORKING-STORAGE SECTION.                                         bbank80p
003000 01  WS-MISC-STORAGE.                                             bbank80p
003100   05  WS-PROGRAM-ID                         PIC X(8)             bbank80p
003200       VALUE 'BBANK80P'.                                          bbank80p
003300   05  WS-INPUT-FLAG                         PIC X(1).            bbank80p
003400     88  INPUT-OK                            VALUE '0'.           bbank80p
003500     88  INPUT-ERROR                         VALUE '1'.           bbank80p
003600   05  WS-RETURN-FLAG                        PIC X(1).            bbank80p
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    bbank80p
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           bbank80p
003900   05  WS-RETURN-MSG                         PIC X(75).           bbank80p
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        bbank80p
004100   05  WS-PFK-FLAG                           PIC X(1).            bbank80p
004200     88  PFK-VALID                           VALUE '0'.           bbank80p
004300     88  PFK-INVALID                         VALUE '1'.           bbank80p
004400   05  WS-ERROR-MSG                          PIC X(75).           bbank80p
004500                                                                  bbank80p
004600 01  WS-BANK-DATA.                                                bbank80p
004700 COPY CBANKDAT.                                                   bbank80p
004800                                                                  bbank80p
004900 01  WS-HELP-DATA.                                                bbank80p
005000 COPY CHELPD01.                                                   bbank80p
005100                                                                  bbank80p
005200 01  WS-ADDR-DATA.                                                bbank80p
005300 COPY CBANKD09.                                                   bbank80p
005400                                                                  bbank80p
005500 01  WS-STMT-REQUEST-DATA.                                        bbank80p
005600 COPY CSTMTD01.                                                   bbank80p
005700                                                                  bbank80p
005800 COPY CSTATESD.                                                   bbank80p
005900                                                                  bbank80p
006000 COPY CABENDD.                                                    bbank80p
006100                                                                  bbank80p
006200 LINKAGE SECTION.                                                 bbank80p
006300 01  DFHCOMMAREA.                                                 bbank80p
006400   05  LK-COMMAREA                           PIC X(6144).         bbank80p
006500                                                                  bbank80p
006600 COPY CENTRY.                                                     bbank80p
006700***************************************************************** bbank80p
006800* Make ourselves re-entrant                                     * bbank80p
006900***************************************************************** bbank80p
007000     MOVE SPACES TO WS-ERROR-MSG.                                 bbank80p
007100                                                                  bbank80p
007200***************************************************************** bbank80p
007300* Move the passed area to our area                              * bbank80p
007400***************************************************************** bbank80p
007500     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. bbank80p
007600                                                                  bbank80p
007700***************************************************************** bbank80p
007800* Ensure error message is cleared                               * bbank80p
007900***************************************************************** bbank80p
008000     MOVE SPACES TO BANK-ERROR-MSG.                               bbank80p
008100                                                                  bbank80p
008200***************************************************************** bbank80p
008300* This is the main process                                      * bbank80p
008400***************************************************************** bbank80p
008500                                                                  bbank80p
008600***************************************************************** bbank80p
008700* Save the passed return flag and then turn it off              * bbank80p
008800***************************************************************** bbank80p
008900     MOVE BANK-RETURN-FLAG TO WS-RETURN-FLAG.                     bbank80p
009000     SET BANK-RETURN-FLAG-OFF TO TRUE.                            bbank80p
009100                                                                  bbank80p
009200***************************************************************** bbank80p
009300* Check the AID to see if its valid at this point               * bbank80p
009400***************************************************************** bbank80p
009500     SET PFK-INVALID TO TRUE.                                     bbank80p
009600     IF BANK-AID-ENTER OR                                         bbank80p
009700        BANK-AID-PFK03 OR                                         bbank80p
009800        BANK-AID-PFK04 OR                                         bbank80p
009900        BANK-AID-PFK10                                            bbank80p
010000        SET PFK-VALID TO TRUE                                     bbank80p
010100     END-IF.                                                      bbank80p
010200     IF BANK-AID-PFK01 AND                                        bbank80p
010300        BANK-HELP-INACTIVE                                        bbank80p
010400        SET BANK-HELP-ACTIVE TO TRUE                              bbank80p
010500        SET PFK-VALID TO TRUE                                     bbank80p
010600     END-IF.                                                      bbank80p
010700     IF PFK-INVALID                                               bbank80p
010800        SET BANK-AID-ENTER TO TRUE                                bbank80p
010900     END-IF.                                                      bbank80p
011000                                                                  bbank80p
011100***************************************************************** bbank80p
011200* Check the AID to see if we have to quit                       * bbank80p
011300***************************************************************** bbank80p
011400     IF BANK-AID-PFK03                                            bbank80p
011500        MOVE 'BBANK80P' TO BANK-LAST-PROG                         bbank80p
011600        MOVE 'BBANK99P' TO BANK-NEXT-PROG                         bbank80p
011700        MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        bbank80p
011800        MOVE 'BANK99A' TO BANK-NEXT-MAP                           bbank80p
011900        GO TO COMMON-RETURN                                       bbank80p
012000     END-IF.                                                      bbank80p
012100                                                                  bbank80p
012200***************************************************************** bbank80p
012300* Check the to see if user needs or has been using help         * bbank80p
012400***************************************************************** bbank80p
012500     IF BANK-HELP-ACTIVE                                          bbank80p
012600        IF BANK-AID-PFK04                                         bbank80p
012700           SET BANK-HELP-INACTIVE TO TRUE                         bbank80p
012800           MOVE 00 TO BANK-HELP-SCREEN                            bbank80p
012900           MOVE 'BBANK80P' TO BANK-LAST-PROG                      bbank80p
013000           MOVE 'BBANK80P' TO BANK-NEXT-PROG                      bbank80p
013100           MOVE 'MBANK80' TO BANK-LAST-MAPSET                     bbank80p
013200           MOVE 'HELP80A' TO BANK-LAST-MAP                        bbank80p
013300           MOVE 'MBANK80' TO BANK-NEXT-MAPSET                     bbank80p
013400           MOVE 'BANK80A' TO BANK-NEXT-MAP                        bbank80p
013500           GO TO COMMON-RETURN                                    bbank80p
013600        ELSE                                                      bbank80p
013700           MOVE 01 TO BANK-HELP-SCREEN                            bbank80p
013800           MOVE 'BBANK80P' TO BANK-LAST-PROG                      bbank80p
013900           MOVE 'BBANK80P' TO BANK-NEXT-PROG                      bbank80p
014000           MOVE 'MBANK80' TO BANK-LAST-MAPSET                     bbank80p
014100           MOVE 'BANK80A' TO BANK-LAST-MAP                        bbank80p
014200           MOVE 'MBANK80' TO BANK-NEXT-MAPSET                     bbank80p
014300           MOVE 'HELP80A' TO BANK-NEXT-MAP                        bbank80p
014400           MOVE 'BANK80' TO HELP01I-SCRN                          bbank80p
014500           COPY CHELPX01.                                         bbank80p
014600           MOVE HELP01O-DATA TO BANK-HELP-DATA                    bbank80p
014700           GO TO COMMON-RETURN                                    bbank80p
014800     END-IF.                                                      bbank80p
014900                                                                  bbank80p
015000***************************************************************** bbank80p
015100* Check the AID to see if we have to return to previous screen  * bbank80p
015200***************************************************************** bbank80p
015300     IF BANK-AID-PFK04                                            bbank80p
015400        MOVE 'BBANK80P' TO BANK-LAST-PROG                         bbank80p
015500        MOVE 'BBANK20P' TO BANK-NEXT-PROG                         bbank80p
015600        MOVE 'MBANK20' TO BANK-NEXT-MAPSET                        bbank80p
015700        MOVE 'BANK20A' TO BANK-NEXT-MAP                           bbank80p
015800        SET BANK-AID-ENTER TO TRUE                                bbank80p
015900        GO TO COMMON-RETURN                                       bbank80p
016000     END-IF.                                                      bbank80p
016100                                                                  bbank80p
016200* Check if we have set the screen up before or is this 1st time   bbank80p
016300     IF BANK-LAST-MAPSET IS NOT EQUAL TO 'MBANK80'                bbank80p
016400        MOVE WS-RETURN-MSG TO BANK-ERROR-MSG                      bbank80p
016500        MOVE 'BBANK80P' TO BANK-LAST-PROG                         bbank80p
016600        MOVE 'BBANK80P' TO BANK-NEXT-PROG                         bbank80p
016700        MOVE 'MBANK80' TO BANK-LAST-MAPSET                        bbank80p
016800        MOVE 'BANK80A' TO BANK-LAST-MAP                           bbank80p
016900        MOVE 'MBANK80' TO BANK-NEXT-MAPSET                        bbank80p
017000        MOVE 'BANK80A' TO BANK-NEXT-MAP                           bbank80p
017100        PERFORM POPULATE-SCREEN-DATA THRU                         bbank80p
017200                POPULATE-SCREEN-DATA-EXIT                         bbank80p
017300        IF BANK-SCR80-EMAIL IS EQUAL TO SPACES                    bbank80p
017400           MOVE 'Please use F10 to confirm request.'              bbank80p
017500             TO WS-ERROR-MSG                                      bbank80p
017600           SET PRINT-CONFIRM TO TRUE                              bbank80p
017700           MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                    bbank80p
017800        ELSE                                                      bbank80p
017900           SET PRINT-REQUEST TO TRUE                              bbank80p
018000        END-IF                                                    bbank80p
018100        GO TO COMMON-RETURN                                       bbank80p
018200     END-IF.                                                      bbank80p
018300                                                                  bbank80p
018400***************************************************************** bbank80p
018500* Check  the data                                                *bbank80p
018600***************************************************************** bbank80p
018700     PERFORM VALIDATE-DATA THRU                                   bbank80p
018800             VALIDATE-DATA-EXIT.                                  bbank80p
018900     IF INPUT-ERROR                                               bbank80p
019000        MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                       bbank80p
019100        MOVE 'BBANK80P' TO BANK-LAST-PROG                         bbank80p
019200        MOVE 'BBANK80P' TO BANK-NEXT-PROG                         bbank80p
019300        MOVE 'MBANK80' TO BANK-LAST-MAPSET                        bbank80p
019400        MOVE 'BANK80A' TO BANK-LAST-MAP                           bbank80p
019500        MOVE 'MBANK80' TO BANK-NEXT-MAPSET                        bbank80p
019600        MOVE 'BANK80A' TO BANK-NEXT-MAP                           bbank80p
019700        GO TO COMMON-RETURN                                       bbank80p
019800     END-IF                                                       bbank80p
019900                                                                  bbank80p
020000* We want to verify the request                                   bbank80p
020100     IF PRINT-REQUEST                                             bbank80p
020200        MOVE 'Please use F10 to confirm request' TO WS-ERROR-MSG  bbank80p
020300        SET PRINT-CONFIRM TO TRUE                                 bbank80p
020400        MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                       bbank80p
020500        MOVE 'BBANK80P' TO BANK-LAST-PROG                         bbank80p
020600        MOVE 'BBANK80P' TO BANK-NEXT-PROG                         bbank80p
020700        MOVE 'MBANK80' TO BANK-LAST-MAPSET                        bbank80p
020800        MOVE 'BANK80A' TO BANK-LAST-MAP                           bbank80p
020900        MOVE 'MBANK80' TO BANK-NEXT-MAPSET                        bbank80p
021000        MOVE 'BANK80A' TO BANK-NEXT-MAP                           bbank80p
021100        GO TO COMMON-RETURN                                       bbank80p
021200     END-IF.                                                      bbank80p
021300* Data was changed and verified                                   bbank80p
021400     IF PRINT-CONFIRM AND                                         bbank80p
021500        BANK-AID-PFK10                                            bbank80p
021600        MOVE SPACES TO CSTMTD01-DATA                              bbank80p
021700        MOVE BANK-SCR80-CONTACT-ID TO CSTMTD01I-CONTACT-ID        bbank80p
021800        IF BANK-SCR80-EMAIL IS EQUAL TO SPACES                    bbank80p
021900           SET CSTMTD01I-POST TO TRUE                             bbank80p
022000        END-IF                                                    bbank80p
022100        IF BANK-SCR80-OPT1 IS NOT EQUAL TO LOW-VALUES             bbank80p
022200           SET CSTMTD01I-POST TO TRUE                             bbank80p
022300        END-IF                                                    bbank80p
022400        IF BANK-SCR80-OPT2 IS NOT EQUAL TO LOW-VALUES             bbank80p
022500           SET CSTMTD01I-EMAIL TO TRUE                            bbank80p
022600        END-IF                                                    bbank80p
022700* all the routine that will invoke the print process              bbank80p
022800 COPY CSTMTX01.                                                   bbank80p
022900        IF CSTMTD01I-POST                                         bbank80p
023000           STRING 'Statement print has been requested'            bbank80p
023100                     DELIMITED BY SIZE                            bbank80p
023200                  ' and will be sent to your postal address'      bbank80p
023300                    DELIMITED BY SIZE                             bbank80p
023400             INTO BANK-RETURN-MSG                                 bbank80p
023500        ELSE                                                      bbank80p
023600           STRING 'Statement print has been requested'            bbank80p
023700                     DELIMITED BY SIZE                            bbank80p
023800                  ' and will be sent to your E-Mail address'      bbank80p
023900                    DELIMITED BY SIZE                             bbank80p
024000             INTO BANK-RETURN-MSG                                 bbank80p
024100        END-IF                                                    bbank80p
024200        MOVE SPACES TO BANK-SCREEN80-DATA                         bbank80p
024300        MOVE 'BBANK80P' TO BANK-LAST-PROG                         bbank80p
024400        MOVE 'BBANK20P' TO BANK-NEXT-PROG                         bbank80p
024500        MOVE 'MBANK20' TO BANK-NEXT-MAPSET                        bbank80p
024600        MOVE 'BANK20A' TO BANK-NEXT-MAP                           bbank80p
024700        SET BANK-AID-ENTER TO TRUE                                bbank80p
024800        GO TO COMMON-RETURN                                       bbank80p
024900     END-IF.                                                      bbank80p
025000                                                                  bbank80p
025100* Turn off update flags and redisplay                             bbank80p
025200     SET PRINT-REQUEST TO TRUE.                                   bbank80p
025300     MOVE 'BBANK80P' TO BANK-LAST-PROG                            bbank80p
025400     MOVE 'BBANK80P' TO BANK-NEXT-PROG                            bbank80p
025500     MOVE 'MBANK80' TO BANK-LAST-MAPSET                           bbank80p
025600     MOVE 'BANK80A' TO BANK-LAST-MAP                              bbank80p
025700     MOVE 'MBANK80' TO BANK-NEXT-MAPSET                           bbank80p
025800     MOVE 'BANK80A' TO BANK-NEXT-MAP                              bbank80p
025900     GO TO COMMON-RETURN.                                         bbank80p
026000                                                                  bbank80p
026100 COMMON-RETURN.                                                   bbank80p
026200     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). bbank80p
026300 COPY CRETURN.                                                    bbank80p
026400                                                                  bbank80p
026500 VALIDATE-DATA.                                                   bbank80p
026600     SET INPUT-OK TO TRUE.                                        bbank80p
026700     IF BANK-SCR80-EMAIL IS NOT EQUAL TO SPACES                   bbank80p
026800        IF BANK-SCR80-OPT1 IS EQUAL TO LOW-VALUES AND             bbank80p
026900           BANK-SCR80-OPT2 IS EQUAL TO LOW-VALUES                 bbank80p
027000           MOVE 'Must select an option' TO WS-ERROR-MSG           bbank80p
027100           GO TO VALIDATE-DATA-ERROR                              bbank80p
027200        END-IF                                                    bbank80p
027300        IF BANK-SCR80-OPT1 IS NOT EQUAL TO LOW-VALUES AND         bbank80p
027400           BANK-SCR80-OPT2 IS NOT EQUAL TO LOW-VALUES             bbank80p
027500           MOVE 'Select only one of mail or e-mail'               bbank80p
027600             TO WS-ERROR-MSG                                      bbank80p
027700           GO TO VALIDATE-DATA-ERROR                              bbank80p
027800        END-IF                                                    bbank80p
027900     END-IF.                                                      bbank80p
028000* Disallow email as we cant really send it                        bbank80p
028100*    IF BANK-SCR80-OPT2 IS NOT EQUAL TO LOW-VALUES                bbank80p
028200*       MOVE SPACES TO WS-ERROR-MSG                               bbank80p
028300*       STRING 'Could not validate email address. '               bbank80p
028400*                DELIMITED BY SIZE                                bbank80p
028500*              'Please select "mail" or Return'                   bbank80p
028600*                DELIMITED BY SIZE                                bbank80p
028700*         INTO WS-ERROR-MSG                                       bbank80p
028800*       MOVE HIGH-VALUES TO BANK-SCR80-OPT2                       bbank80p
028900*       GO TO VALIDATE-DATA-ERROR                                 bbank80p
029000*    END-IF.                                                      bbank80p
029100                                                                  bbank80p
029200     GO TO VALIDATE-DATA-EXIT.                                    bbank80p
029300                                                                  bbank80p
029400 VALIDATE-DATA-ERROR.                                             bbank80p
029500     SET INPUT-ERROR TO TRUE.                                     bbank80p
029600 VALIDATE-DATA-EXIT.                                              bbank80p
029700     EXIT.                                                        bbank80p
029800                                                                  bbank80p
029900 POPULATE-SCREEN-DATA.                                            bbank80p
030000     MOVE SPACES TO CD09-DATA.                                    bbank80p
030100     MOVE BANK-USERID TO BANK-SCR80-CONTACT-ID.                   bbank80p
030200     MOVE BANK-SCR80-CONTACT-ID TO CD09I-CONTACT-ID.              bbank80p
030300* Now go get the data                                             bbank80p
030400 COPY CBANKX09.                                                   bbank80p
030500     MOVE SPACES TO BANK-SCR80-DETS.                              bbank80p
030600     MOVE '_' TO BANK-SCR80-OPT1.                                 bbank80p
030700     MOVE '_' TO BANK-SCR80-OPT2.                                 bbank80p
030800     IF CD09O-CONTACT-ID IS EQUAL TO CD09I-CONTACT-ID             bbank80p
030900        MOVE CD09O-CONTACT-ID TO BANK-SCR80-CONTACT-ID            bbank80p
031000        MOVE CD09O-CONTACT-NAME TO BANK-SCR80-CONTACT-NAME        bbank80p
031100        MOVE CD09O-CONTACT-ADDR1 TO BANK-SCR80-ADDR1              bbank80p
031200        MOVE CD09O-CONTACT-ADDR2 TO BANK-SCR80-ADDR2              bbank80p
031300        MOVE CD09O-CONTACT-STATE TO BANK-SCR80-STATE              bbank80p
031400        MOVE CD09O-CONTACT-CNTRY TO BANK-SCR80-CNTRY              bbank80p
031500        MOVE CD09O-CONTACT-PSTCDE TO BANK-SCR80-PSTCDE            bbank80p
031600        MOVE CD09O-CONTACT-EMAIL TO BANK-SCR80-EMAIL              bbank80p
031700     ELSE                                                         bbank80p
031800        MOVE CD09O-CONTACT-NAME TO BANK-SCR80-CONTACT-NAME        bbank80p
031900     END-IF.                                                      bbank80p
032000 POPULATE-SCREEN-DATA-EXIT.                                       bbank80p
032100     EXIT.                                                        bbank80p
032200                                                                  bbank80p
032300* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     bbank80p
