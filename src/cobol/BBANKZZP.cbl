000100***************************************************************** bbankzzp
000200*                                                               * bbankzzp
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * bbankzzp
000400*   This demonstration program is provided for use by users     * bbankzzp
000500*   of Micro Focus products and may be used, modified and       * bbankzzp
000600*   distributed as part of your application provided that       * bbankzzp
000700*   you properly acknowledge the copyright of Micro Focus       * bbankzzp
000800*   in this material.                                           * bbankzzp
000900*                                                               * bbankzzp
001000***************************************************************** bbankzzp
001100                                                                  bbankzzp
001200***************************************************************** bbankzzp
001300* Program:     BBANKZZP.CBL                                     * bbankzzp
001400* Layer:       Business logic                                   * bbankzzp
001500* Function:    Create various problem / error conditions        * bbankzzp
001600***************************************************************** bbankzzp
001700                                                                  bbankzzp
001800 IDENTIFICATION DIVISION.                                         bbankzzp
001900 PROGRAM-ID.                                                      bbankzzp
002000     BBANKZZP.                                                    bbankzzp
002100 DATE-WRITTEN.                                                    bbankzzp
002200     September 2002.                                              bbankzzp
002300 DATE-COMPILED.                                                   bbankzzp
002400     Today.                                                       bbankzzp
002500                                                                  bbankzzp
002600 ENVIRONMENT DIVISION.                                            bbankzzp
002700                                                                  bbankzzp
002800 DATA DIVISION.                                                   bbankzzp
002900 WORKING-STORAGE SECTION.                                         bbankzzp
003000 01  WS-MISC-STORAGE.                                             bbankzzp
003100   05  WS-PROGRAM-ID                         PIC X(8)             bbankzzp
003200       VALUE 'BBANKZZP'.                                          bbankzzp
003300   05  WS-INPUT-FLAG                         PIC X(1).            bbankzzp
003400     88  INPUT-OK                            VALUE '0'.           bbankzzp
003500     88  INPUT-ERROR                         VALUE '1'.           bbankzzp
003600   05  WS-RETURN-FLAG                        PIC X(1).            bbankzzp
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    bbankzzp
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           bbankzzp
003900   05  WS-RETURN-MSG                         PIC X(75).           bbankzzp
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        bbankzzp
004100   05  WS-PFK-FLAG                           PIC X(1).            bbankzzp
004200     88  PFK-VALID                           VALUE '0'.           bbankzzp
004300     88  PFK-INVALID                         VALUE '1'.           bbankzzp
004400   05  WS-ERROR-MSG                          PIC X(75).           bbankzzp
004500   05  WS-SUB1                               PIC S9(4) COMP.      bbankzzp
004600   05  WS-SUB1-LIMIT                         PIC S9(4) COMP.      bbankzzp
004700   05  WS-SEL-COUNT                          PIC 9(1).            bbankzzp
004800   05  WS-SEL-OPTION                         PIC X(1).            bbankzzp
004900     88  WS-SEL-OPTION-NULL                  VALUES ' ', '.'.     bbankzzp
005000     88  WS-SEL-OPTION-1                     VALUE '1'.           bbankzzp
005100     88  WS-SEL-OPTION-2                     VALUE '2'.           bbankzzp
005200     88  WS-SEL-OPTION-3                     VALUE '3'.           bbankzzp
005300     88  WS-SEL-OPTION-4                     VALUE '4'.           bbankzzp
005400     88  WS-SEL-OPTION-5                     VALUE '5'.           bbankzzp
005500     88  WS-SEL-OPTION-6                     VALUE '6'.           bbankzzp
005600     88  WS-SEL-OPTION-7                     VALUE '7'.           bbankzzp
005700     88  WS-SEL-OPTION-8                     VALUE '8'.           bbankzzp
005800   05  WS-SEL-MATRIX                         PIC X(8).            bbankzzp
005900                                                                  bbankzzp
006000 01  WS-PROBLEM-DATA.                                             bbankzzp
006100   05  WS-PROB-SUB                           PIC 9(8) COMP.       bbankzzp
006200   05  WS-PROB-TABLE.                                             bbankzzp
006300     10  WS-PROB-TABLE-ITEM                  PIC 9(3)             bbankzzp
006400         OCCURS 10 TIMES.                                         bbankzzp
006500   05  WS-PROB-MODULE                        PIC X(8).            bbankzzp
006600   05  WS-FLD-A                              PIC X(9).            bbankzzp
006700   05  WS-FLD-A-NUM REDEFINES WS-FLD-A       PIC 9(5).99-.        bbankzzp
006800   05  WS-FLD-B                              PIC S9(5)V99.        bbankzzp
006900                                                                  bbankzzp
007000 01  WS-BANK-DATA.                                                bbankzzp
007100 COPY CBANKDAT.                                                   bbankzzp
007200                                                                  bbankzzp
007300 01  WS-HELP-DATA.                                                bbankzzp
007400 COPY CHELPD01.                                                   bbankzzp
007500                                                                  bbankzzp
007600 01  WS-ACCOUNT-DATA.                                             bbankzzp
007700 COPY CBANKD08.                                                   bbankzzp
007800                                                                  bbankzzp
007900 COPY CABENDD.                                                    bbankzzp
008000                                                                  bbankzzp
008100 LINKAGE SECTION.                                                 bbankzzp
008200 01  DFHCOMMAREA.                                                 bbankzzp
008300   05  LK-COMMAREA                           PIC X(6144).         bbankzzp
008400                                                                  bbankzzp
008500 01  LK-PROB-LINKAGE                         PIC X(1).            bbankzzp
008600                                                                  bbankzzp
008700 COPY CENTRY.                                                     bbankzzp
008800***************************************************************** bbankzzp
008900* Make ourselves re-entrant                                     * bbankzzp
009000***************************************************************** bbankzzp
009100     MOVE SPACES TO WS-ERROR-MSG.                                 bbankzzp
009200                                                                  bbankzzp
009300***************************************************************** bbankzzp
009400* Move the passed area to our area                              * bbankzzp
009500***************************************************************** bbankzzp
009600     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. bbankzzp
009700                                                                  bbankzzp
009800***************************************************************** bbankzzp
009900* Ensure error message is cleared                               * bbankzzp
010000***************************************************************** bbankzzp
010100     MOVE SPACES TO BANK-ERROR-MSG.                               bbankzzp
010200                                                                  bbankzzp
010300***************************************************************** bbankzzp
010400* This is the main process                                      * bbankzzp
010500***************************************************************** bbankzzp
010600                                                                  bbankzzp
010700***************************************************************** bbankzzp
010800* Initialize the list of options available                      * bbankzzp
010900***************************************************************** bbankzzp
011000     MOVE SPACES TO WS-SEL-MATRIX.                                bbankzzp
011100                                                                  bbankzzp
011200***************************************************************** bbankzzp
011300* Save the passed return message and then turn it off           * bbankzzp
011400***************************************************************** bbankzzp
011500     MOVE BANK-RETURN-MSG TO WS-RETURN-MSG.                       bbankzzp
011600     SET BANK-RETURN-MSG-OFF TO TRUE.                             bbankzzp
011700                                                                  bbankzzp
011800     MOVE WS-RETURN-MSG TO WS-ERROR-MSG.                          bbankzzp
011900                                                                  bbankzzp
012000***************************************************************** bbankzzp
012100* Check the AID to see if its valid at this point               * bbankzzp
012200***************************************************************** bbankzzp
012300     SET PFK-INVALID TO TRUE.                                     bbankzzp
012400     IF BANK-AID-ENTER OR                                         bbankzzp
012500        BANK-AID-PFK03 OR                                         bbankzzp
012600        BANK-AID-PFK04                                            bbankzzp
012700        SET PFK-VALID TO TRUE                                     bbankzzp
012800     END-IF.                                                      bbankzzp
012900     IF BANK-AID-PFK01 AND                                        bbankzzp
013000        BANK-HELP-INACTIVE                                        bbankzzp
013100        SET BANK-HELP-ACTIVE TO TRUE                              bbankzzp
013200        SET PFK-VALID TO TRUE                                     bbankzzp
013300     END-IF.                                                      bbankzzp
013400     IF PFK-INVALID                                               bbankzzp
013500        SET BANK-AID-ENTER TO TRUE                                bbankzzp
013600     END-IF.                                                      bbankzzp
013700                                                                  bbankzzp
013800***************************************************************** bbankzzp
013900* Check the AID to see if we have to quit                       * bbankzzp
014000***************************************************************** bbankzzp
014100     IF BANK-AID-PFK03                                            bbankzzp
014200        MOVE 'BBANKZZP' TO BANK-LAST-PROG                         bbankzzp
014300        MOVE 'BBANK99P' TO BANK-NEXT-PROG                         bbankzzp
014400        MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        bbankzzp
014500        MOVE 'BANK99A' TO BANK-NEXT-MAP                           bbankzzp
014600        GO TO COMMON-RETURN                                       bbankzzp
014700     END-IF.                                                      bbankzzp
014800                                                                  bbankzzp
014900***************************************************************** bbankzzp
015000* Check the to see if user needs or has been using help         * bbankzzp
015100***************************************************************** bbankzzp
015200     IF BANK-HELP-ACTIVE                                          bbankzzp
015300        IF BANK-AID-PFK04                                         bbankzzp
015400           SET BANK-HELP-INACTIVE TO TRUE                         bbankzzp
015500           MOVE 00 TO BANK-HELP-SCREEN                            bbankzzp
015600           MOVE 'BBANKZZP' TO BANK-LAST-PROG                      bbankzzp
015700           MOVE 'BBANKZZP' TO BANK-NEXT-PROG                      bbankzzp
015800           MOVE 'MBANKZZ' TO BANK-LAST-MAPSET                     bbankzzp
015900           MOVE 'HELP20A' TO BANK-LAST-MAP                        bbankzzp
016000           MOVE 'MBANKZZ' TO BANK-NEXT-MAPSET                     bbankzzp
016100           MOVE 'BANKZZA' TO BANK-NEXT-MAP                        bbankzzp
016200           PERFORM POPULATE-OPTIONS THRU                          bbankzzp
016300                   POPULATE-OPTIONS-EXIT                          bbankzzp
016400           GO TO COMMON-RETURN                                    bbankzzp
016500        ELSE                                                      bbankzzp
016600           MOVE 01 TO BANK-HELP-SCREEN                            bbankzzp
016700           MOVE 'BBANKZZP' TO BANK-LAST-PROG                      bbankzzp
016800           MOVE 'BBANKZZP' TO BANK-NEXT-PROG                      bbankzzp
016900           MOVE 'MBANKZZ' TO BANK-LAST-MAPSET                     bbankzzp
017000           MOVE 'BANKZZA' TO BANK-LAST-MAP                        bbankzzp
017100           MOVE 'MBANKZZ' TO BANK-NEXT-MAPSET                     bbankzzp
017200           MOVE 'HELP20A' TO BANK-NEXT-MAP                        bbankzzp
017300           MOVE 'BANKZZ' TO HELP01I-SCRN                          bbankzzp
017400           COPY CHELPX01.                                         bbankzzp
017500           MOVE HELP01O-DATA TO BANK-HELP-DATA                    bbankzzp
017600           GO TO COMMON-RETURN                                    bbankzzp
017700     END-IF.                                                      bbankzzp
017800                                                                  bbankzzp
017900***************************************************************** bbankzzp
018000* Check the AID to see if we have to return to previous screen  * bbankzzp
018100***************************************************************** bbankzzp
018200     IF BANK-AID-PFK04                                            bbankzzp
018300        MOVE 'BBANKZZP' TO BANK-LAST-PROG                         bbankzzp
018400        MOVE 'BBANK10P' TO BANK-NEXT-PROG                         bbankzzp
018500        MOVE 'MBANK10' TO BANK-NEXT-MAPSET                        bbankzzp
018600        MOVE 'BANK10A' TO BANK-NEXT-MAP                           bbankzzp
018700        SET BANK-AID-ENTER TO TRUE                                bbankzzp
018800        SET BANK-NO-CONV-IN-PROGRESS TO TRUE                      bbankzzp
018900        GO TO COMMON-RETURN                                       bbankzzp
019000     END-IF.                                                      bbankzzp
019100                                                                  bbankzzp
019200* Check if we have set the screen up before or is this 1st time   bbankzzp
019300     IF BANK-LAST-MAPSET IS NOT EQUAL TO 'MBANKZZ'                bbankzzp
019400        MOVE WS-RETURN-MSG TO BANK-ERROR-MSG                      bbankzzp
019500        MOVE 'BBANKZZP' TO BANK-LAST-PROG                         bbankzzp
019600        MOVE 'BBANKZZP' TO BANK-NEXT-PROG                         bbankzzp
019700        MOVE 'MBANKZZ' TO BANK-LAST-MAPSET                        bbankzzp
019800        MOVE 'BANKZZA' TO BANK-LAST-MAP                           bbankzzp
019900        MOVE 'MBANKZZ' TO BANK-NEXT-MAPSET                        bbankzzp
020000        MOVE 'BANKZZA' TO BANK-NEXT-MAP                           bbankzzp
020100        MOVE LOW-VALUES TO BANK-SCRZZ-SEL1IP                      bbankzzp
020200        MOVE LOW-VALUES TO BANK-SCRZZ-SEL2IP                      bbankzzp
020300        MOVE LOW-VALUES TO BANK-SCRZZ-SEL3IP                      bbankzzp
020400        MOVE LOW-VALUES TO BANK-SCRZZ-SEL4IP                      bbankzzp
020500        MOVE LOW-VALUES TO BANK-SCRZZ-SEL5IP                      bbankzzp
020600        MOVE LOW-VALUES TO BANK-SCRZZ-SEL6IP                      bbankzzp
020700        MOVE LOW-VALUES TO BANK-SCRZZ-SEL7IP                      bbankzzp
020800        MOVE LOW-VALUES TO BANK-SCRZZ-SEL8IP                      bbankzzp
020900        IF GUEST                                                  bbankzzp
021000           MOVE 'LI      ' TO WS-SEL-MATRIX                       bbankzzp
021100        ELSE                                                      bbankzzp
021200          MOVE SPACES TO CD08-DATA                                bbankzzp
021300          MOVE BANK-USERID TO CD08I-CONTACT-ID                    bbankzzp
021400* Now go get the data                                             bbankzzp
021500          COPY CBANKX08.                                          bbankzzp
021600          IF CD08O-COUNT IS EQUAL TO 0                            bbankzzp
021700             IF PROBLEM-USER                                      bbankzzp
021800                MOVE '123     ' TO WS-SEL-MATRIX                  bbankzzp
021900             ELSE                                                 bbankzzp
022000                MOVE '12      ' TO WS-SEL-MATRIX                  bbankzzp
022100             END-IF                                               bbankzzp
022200          END-IF                                                  bbankzzp
022300          IF CD08O-COUNT IS EQUAL TO 1                            bbankzzp
022400             IF PROBLEM-USER                                      bbankzzp
022500                MOVE '123456  ' TO WS-SEL-MATRIX                  bbankzzp
022600             ELSE                                                 bbankzzp
022700                MOVE '12345   ' TO WS-SEL-MATRIX                  bbankzzp
022800             END-IF                                               bbankzzp
022900          END-IF                                                  bbankzzp
023000          IF CD08O-COUNT IS GREATER THAN 1                        bbankzzp
023100             IF PROBLEM-USER                                      bbankzzp
023200                MOVE '12345678' TO WS-SEL-MATRIX                  bbankzzp
023300             ELSE                                                 bbankzzp
023400                MOVE '1234567 ' TO WS-SEL-MATRIX                  bbankzzp
023500             END-IF                                               bbankzzp
023600          END-IF                                                  bbankzzp
023700        END-IF                                                    bbankzzp
023800        PERFORM POPULATE-OPTIONS THRU                             bbankzzp
023900                POPULATE-OPTIONS-EXIT                             bbankzzp
024000        GO TO COMMON-RETURN                                       bbankzzp
024100     END-IF.                                                      bbankzzp
024200                                                                  bbankzzp
024300     PERFORM VALIDATE-DATA THRU                                   bbankzzp
024400             VALIDATE-DATA-EXIT.                                  bbankzzp
024500                                                                  bbankzzp
024600* If we had an error display error and return                     bbankzzp
024700     IF INPUT-ERROR                                               bbankzzp
024800        MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                       bbankzzp
024900        MOVE 'BBANKZZP' TO BANK-LAST-PROG                         bbankzzp
025000        MOVE 'BBANKZZP' TO BANK-NEXT-PROG                         bbankzzp
025100        MOVE 'MBANKZZ' TO BANK-LAST-MAPSET                        bbankzzp
025200        MOVE 'BANKZZA' TO BANK-LAST-MAP                           bbankzzp
025300        MOVE 'MBANKZZ' TO BANK-NEXT-MAPSET                        bbankzzp
025400        MOVE 'BANKZZA' TO BANK-NEXT-MAP                           bbankzzp
025500        PERFORM POPULATE-OPTIONS THRU                             bbankzzp
025600                POPULATE-OPTIONS-EXIT                             bbankzzp
025700        GO TO COMMON-RETURN                                       bbankzzp
025800     END-IF.                                                      bbankzzp
025900                                                                  bbankzzp
026000     IF BANK-SCRZZ-SEL1IP IS NOT EQUAL TO LOW-VALUES              bbankzzp
026100        MOVE BANK-SCRZZ-SEL1ID TO WS-SEL-OPTION                   bbankzzp
026200     END-IF.                                                      bbankzzp
026300     IF BANK-SCRZZ-SEL2IP IS NOT EQUAL TO LOW-VALUES              bbankzzp
026400        MOVE BANK-SCRZZ-SEL2ID TO WS-SEL-OPTION                   bbankzzp
026500     END-IF.                                                      bbankzzp
026600     IF BANK-SCRZZ-SEL3IP IS NOT EQUAL TO LOW-VALUES              bbankzzp
026700        MOVE BANK-SCRZZ-SEL3ID TO WS-SEL-OPTION                   bbankzzp
026800     END-IF.                                                      bbankzzp
026900     IF BANK-SCRZZ-SEL4IP IS NOT EQUAL TO LOW-VALUES              bbankzzp
027000        MOVE BANK-SCRZZ-SEL4ID TO WS-SEL-OPTION                   bbankzzp
027100     END-IF.                                                      bbankzzp
027200     IF BANK-SCRZZ-SEL5IP IS NOT EQUAL TO LOW-VALUES              bbankzzp
027300        MOVE BANK-SCRZZ-SEL5ID TO WS-SEL-OPTION                   bbankzzp
027400     END-IF.                                                      bbankzzp
027500     IF BANK-SCRZZ-SEL6IP IS NOT EQUAL TO LOW-VALUES              bbankzzp
027600        MOVE BANK-SCRZZ-SEL6ID TO WS-SEL-OPTION                   bbankzzp
027700     END-IF.                                                      bbankzzp
027800     IF BANK-SCRZZ-SEL7IP IS NOT EQUAL TO LOW-VALUES              bbankzzp
027900        MOVE BANK-SCRZZ-SEL7ID TO WS-SEL-OPTION                   bbankzzp
028000     END-IF.                                                      bbankzzp
028100     IF BANK-SCRZZ-SEL8IP IS NOT EQUAL TO LOW-VALUES              bbankzzp
028200        MOVE BANK-SCRZZ-SEL8ID TO WS-SEL-OPTION                   bbankzzp
028300     END-IF.                                                      bbankzzp
028400                                                                  bbankzzp
028500     MOVE ALL '*' TO WS-PROBLEM-DATA.                             bbankzzp
028600                                                                  bbankzzp
028700     EVALUATE TRUE                                                bbankzzp
028800        WHEN WS-SEL-OPTION IS EQUAL TO '1'                        bbankzzp
028900          PERFORM SCENARIO-1 THRU                                 bbankzzp
029000                  SCENARIO-1-EXIT                                 bbankzzp
029100        WHEN WS-SEL-OPTION IS EQUAL TO '2'                        bbankzzp
029200          PERFORM SCENARIO-2 THRU                                 bbankzzp
029300                  SCENARIO-2-EXIT                                 bbankzzp
029400        WHEN WS-SEL-OPTION IS EQUAL TO '3'                        bbankzzp
029500          PERFORM SCENARIO-3 THRU                                 bbankzzp
029600                  SCENARIO-3-EXIT                                 bbankzzp
029700        WHEN WS-SEL-OPTION IS EQUAL TO '4'                        bbankzzp
029800          PERFORM SCENARIO-4 THRU                                 bbankzzp
029900                  SCENARIO-4-EXIT                                 bbankzzp
030000        WHEN WS-SEL-OPTION IS EQUAL TO '5'                        bbankzzp
030100          PERFORM SCENARIO-5 THRU                                 bbankzzp
030200                  SCENARIO-5-EXIT                                 bbankzzp
030300        WHEN WS-SEL-OPTION IS EQUAL TO '6'                        bbankzzp
030400          PERFORM SCENARIO-6 THRU                                 bbankzzp
030500                  SCENARIO-6-EXIT                                 bbankzzp
030600        WHEN WS-SEL-OPTION IS EQUAL TO '7'                        bbankzzp
030700          PERFORM SCENARIO-7 THRU                                 bbankzzp
030800                  SCENARIO-7-EXIT                                 bbankzzp
030900        WHEN WS-SEL-OPTION IS EQUAL TO '8'                        bbankzzp
031000          PERFORM SCENARIO-8 THRU                                 bbankzzp
031100                  SCENARIO-8-EXIT                                 bbankzzp
031200        WHEN OTHER                                                bbankzzp
031300          PERFORM SCENARIO-9 THRU                                 bbankzzp
031400                  SCENARIO-9-EXIT                                 bbankzzp
031500     END-EVALUATE.                                                bbankzzp
031600                                                                  bbankzzp
031700     GO TO COMMON-RETURN.                                         bbankzzp
031800                                                                  bbankzzp
031900 COMMON-RETURN.                                                   bbankzzp
032000     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). bbankzzp
032100 COPY CRETURN.                                                    bbankzzp
032200                                                                  bbankzzp
032300 VALIDATE-DATA.                                                   bbankzzp
032400     SET INPUT-OK TO TRUE.                                        bbankzzp
032500     MOVE ZERO TO WS-SEL-COUNT.                                   bbankzzp
032600                                                                  bbankzzp
032700     IF BANK-SCRZZ-SEL1IP IS NOT EQUAL TO LOW-VALUES              bbankzzp
032800        ADD 1 TO WS-SEL-COUNT                                     bbankzzp
032900     END-IF.                                                      bbankzzp
033000     IF BANK-SCRZZ-SEL2IP IS NOT EQUAL TO LOW-VALUES              bbankzzp
033100        ADD 1 TO WS-SEL-COUNT                                     bbankzzp
033200     END-IF.                                                      bbankzzp
033300     IF BANK-SCRZZ-SEL3IP IS NOT EQUAL TO LOW-VALUES              bbankzzp
033400        ADD 1 TO WS-SEL-COUNT                                     bbankzzp
033500     END-IF.                                                      bbankzzp
033600     IF BANK-SCRZZ-SEL4IP IS NOT EQUAL TO LOW-VALUES              bbankzzp
033700        ADD 1 TO WS-SEL-COUNT                                     bbankzzp
033800     END-IF.                                                      bbankzzp
033900     IF BANK-SCRZZ-SEL5IP IS NOT EQUAL TO LOW-VALUES              bbankzzp
034000        ADD 1 TO WS-SEL-COUNT                                     bbankzzp
034100     END-IF.                                                      bbankzzp
034200     IF BANK-SCRZZ-SEL6IP IS NOT EQUAL TO LOW-VALUES              bbankzzp
034300        ADD 1 TO WS-SEL-COUNT                                     bbankzzp
034400     END-IF.                                                      bbankzzp
034500     IF BANK-SCRZZ-SEL7IP IS NOT EQUAL TO LOW-VALUES              bbankzzp
034600        ADD 1 TO WS-SEL-COUNT                                     bbankzzp
034700     END-IF.                                                      bbankzzp
034800     IF BANK-SCRZZ-SEL8IP IS NOT EQUAL TO LOW-VALUES              bbankzzp
034900        ADD 1 TO WS-SEL-COUNT                                     bbankzzp
035000     END-IF.                                                      bbankzzp
035100                                                                  bbankzzp
035200     IF WS-SEL-COUNT IS EQUAL TO ZERO                             bbankzzp
035300        MOVE 'Please select an option' TO WS-ERROR-MSG            bbankzzp
035400        GO TO VALIDATE-DATA-ERROR                                 bbankzzp
035500     END-IF.                                                      bbankzzp
035600                                                                  bbankzzp
035700     IF WS-SEL-COUNT IS GREATER THAN 1                            bbankzzp
035800        MOVE 'Please select a single option' TO WS-ERROR-MSG      bbankzzp
035900        GO TO VALIDATE-DATA-ERROR                                 bbankzzp
036000     END-IF.                                                      bbankzzp
036100                                                                  bbankzzp
036200     GO TO VALIDATE-DATA-EXIT.                                    bbankzzp
036300 VALIDATE-DATA-ERROR.                                             bbankzzp
036400     SET INPUT-ERROR TO TRUE.                                     bbankzzp
036500 VALIDATE-DATA-EXIT.                                              bbankzzp
036600     EXIT.                                                        bbankzzp
036700                                                                  bbankzzp
036800 POPULATE-OPTIONS.                                                bbankzzp
036900     MOVE 0 TO WS-SUB1.                                           bbankzzp
037000     DIVIDE LENGTH BANK-SCREENZZ-FIELD                            bbankzzp
037100       INTO LENGTH OF BANK-SCREENZZ-DATA-R                        bbankzzp
037200         GIVING WS-SUB1-LIMIT.                                    bbankzzp
037300 POPULATE-OPTIONS-LOOP.                                           bbankzzp
037400     ADD 1 TO WS-SUB1.                                            bbankzzp
037500     IF WS-SUB1 IS GREATER THAN WS-SUB1-LIMIT                     bbankzzp
037600        GO TO POPULATE-OPTIONS-EXIT                               bbankzzp
037700     END-IF.                                                      bbankzzp
037800     IF WS-SEL-MATRIX IS NOT EQUAL TO SPACES                      bbankzzp
037900        MOVE WS-SEL-MATRIX (WS-SUB1:1) TO BANK-SCRZZ-ID (WS-SUB1) bbankzzp
038000     END-IF.                                                      bbankzzp
038100     MOVE SPACES TO BANK-SCRZZ-TX (WS-SUB1).                      bbankzzp
038200     IF BANK-SCRZZ-ID (WS-SUB1) IS EQUAL TO '1'                   bbankzzp
038300        MOVE 'Problem scenario 1'                                 bbankzzp
038400          TO BANK-SCRZZ-TX (WS-SUB1)                              bbankzzp
038500     END-IF.                                                      bbankzzp
038600     IF BANK-SCRZZ-ID (WS-SUB1) IS EQUAL TO '2'                   bbankzzp
038700        MOVE 'Problem scenario 2'                                 bbankzzp
038800          TO BANK-SCRZZ-TX (WS-SUB1)                              bbankzzp
038900     END-IF.                                                      bbankzzp
039000     IF BANK-SCRZZ-ID (WS-SUB1) IS EQUAL TO '3'                   bbankzzp
039100        MOVE 'Problem scenario 3'                                 bbankzzp
039200          TO BANK-SCRZZ-TX (WS-SUB1)                              bbankzzp
039300     END-IF.                                                      bbankzzp
039400     IF BANK-SCRZZ-ID (WS-SUB1) IS EQUAL TO '4'                   bbankzzp
039500        MOVE 'Problem scenario 4'                                 bbankzzp
039600          TO BANK-SCRZZ-TX (WS-SUB1)                              bbankzzp
039700     END-IF.                                                      bbankzzp
039800     IF BANK-SCRZZ-ID (WS-SUB1) IS EQUAL TO '5'                   bbankzzp
039900        MOVE 'Problem scenario 5'                                 bbankzzp
040000          TO BANK-SCRZZ-TX (WS-SUB1)                              bbankzzp
040100     END-IF.                                                      bbankzzp
040200     IF BANK-SCRZZ-ID (WS-SUB1) IS EQUAL TO '6'                   bbankzzp
040300        MOVE 'Problem scenario 6'                                 bbankzzp
040400          TO BANK-SCRZZ-TX (WS-SUB1)                              bbankzzp
040500     END-IF.                                                      bbankzzp
040600     IF BANK-SCRZZ-ID (WS-SUB1) IS EQUAL TO '7'                   bbankzzp
040700        MOVE 'Problem scenario 7'                                 bbankzzp
040800          TO BANK-SCRZZ-TX (WS-SUB1)                              bbankzzp
040900     END-IF.                                                      bbankzzp
041000     IF BANK-SCRZZ-ID (WS-SUB1) IS EQUAL TO '8'                   bbankzzp
041100        MOVE 'Problem scenario 8'                                 bbankzzp
041200          TO BANK-SCRZZ-TX (WS-SUB1)                              bbankzzp
041300     END-IF.                                                      bbankzzp
041400     GO TO POPULATE-OPTIONS-LOOP.                                 bbankzzp
041500 POPULATE-OPTIONS-EXIT.                                           bbankzzp
041600     EXIT.                                                        bbankzzp
041700                                                                  bbankzzp
041800***************************************************************** bbankzzp
041900* Problem scenario 1                                            * bbankzzp
042000***************************************************************** bbankzzp
042100 SCENARIO-1.                                                      bbankzzp
042200     MOVE 99 TO WS-PROB-SUB                                       bbankzzp
042300     MOVE WS-PROB-TABLE-ITEM(1)                                   bbankzzp
042400       TO WS-PROB-TABLE-ITEM(WS-PROB-SUB).                        bbankzzp
042500 SCENARIO-1-EXIT.                                                 bbankzzp
042600     EXIT.                                                        bbankzzp
042700                                                                  bbankzzp
042800***************************************************************** bbankzzp
042900* Problem scenario 2                                            * bbankzzp
043000***************************************************************** bbankzzp
043100 SCENARIO-2.                                                      bbankzzp
043200     ADD 1 TO WS-PROB-TABLE-ITEM(1).                              bbankzzp
043300 SCENARIO-2-EXIT.                                                 bbankzzp
043400     EXIT.                                                        bbankzzp
043500                                                                  bbankzzp
043600***************************************************************** bbankzzp
043700* Problem scenario 3                                            * bbankzzp
043800***************************************************************** bbankzzp
043900 SCENARIO-3.                                                      bbankzzp
044000     CALL WS-PROB-MODULE.                                         bbankzzp
044100 SCENARIO-3-EXIT.                                                 bbankzzp
044200     EXIT.                                                        bbankzzp
044300                                                                  bbankzzp
044400***************************************************************** bbankzzp
044500* Problem scenario 4                                            * bbankzzp
044600***************************************************************** bbankzzp
044700 SCENARIO-4.                                                      bbankzzp
044800     MOVE 40960 TO WS-PROB-SUB.                                   bbankzzp
044900     MOVE LOW-VALUES TO WS-PROBLEM-DATA(1:WS-PROB-SUB).           bbankzzp
045000 SCENARIO-4-EXIT.                                                 bbankzzp
045100     EXIT.                                                        bbankzzp
045200                                                                  bbankzzp
045300***************************************************************** bbankzzp
045400* Problem scenario 5                                            * bbankzzp
045500***************************************************************** bbankzzp
045600 SCENARIO-5.                                                      bbankzzp
045700     MOVE ALL '*' TO LK-PROB-LINKAGE.                             bbankzzp
045800 SCENARIO-5-EXIT.                                                 bbankzzp
045900     EXIT.                                                        bbankzzp
046000                                                                  bbankzzp
046100***************************************************************** bbankzzp
046200* Problem scenario 6                                            * bbankzzp
046300***************************************************************** bbankzzp
046400 SCENARIO-6.                                                      bbankzzp
046500     GO TO NOWHERE.                                               bbankzzp
046600 SCENARIO-6-EXIT.                                                 bbankzzp
046700     EXIT.                                                        bbankzzp
046800                                                                  bbankzzp
046900***************************************************************** bbankzzp
047000* Problem scenario 7                                            * bbankzzp
047100***************************************************************** bbankzzp
047200 SCENARIO-7.                                                      bbankzzp
047300*    PERFORM SCENARIO-7.                                          bbankzzp
047400      CONTINUE.                                                   bbankzzp
047500 SCENARIO-7-EXIT.                                                 bbankzzp
047600     EXIT.                                                        bbankzzp
047700                                                                  bbankzzp
047800***************************************************************** bbankzzp
047900* Problem scenario 8                                            * bbankzzp
048000***************************************************************** bbankzzp
048100 SCENARIO-8.                                                      bbankzzp
048200*    call 'CBL_DEBUGBREAK'.                                       bbankzzp
048300     MOVE '  123.45 ' TO WS-FLD-A.                                bbankzzp
048400     MOVE 99999.99 TO WS-FLD-B.                                   bbankzzp
048500     MOVE WS-FLD-A-NUM TO WS-FLD-B .                              bbankzzp
048600 SCENARIO-8-EXIT.                                                 bbankzzp
048700     EXIT.                                                        bbankzzp
048800                                                                  bbankzzp
048900***************************************************************** bbankzzp
049000* Problem scenario 9                                            * bbankzzp
049100***************************************************************** bbankzzp
049200 SCENARIO-9.                                                      bbankzzp
049300      CONTINUE.                                                   bbankzzp
049400 SCENARIO-9-EXIT.                                                 bbankzzp
049500     EXIT.                                                        bbankzzp
049600                                                                  bbankzzp
049700 NOWHERE.                                                         bbankzzp
049800                                                                  bbankzzp
049900* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     bbankzzp
