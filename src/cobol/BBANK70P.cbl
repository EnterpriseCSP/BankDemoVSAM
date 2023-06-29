000100***************************************************************** bbank70p
000200*                                                               * bbank70p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * bbank70p
000400*   This demonstration program is provided for use by users     * bbank70p
000500*   of Micro Focus products and may be used, modified and       * bbank70p
000600*   distributed as part of your application provided that       * bbank70p
000700*   you properly acknowledge the copyright of Micro Focus       * bbank70p
000800*   in this material.                                           * bbank70p
000900*                                                               * bbank70p
001000***************************************************************** bbank70p
001100                                                                  bbank70p
001200***************************************************************** bbank70p
001300* Program:     BBANK70P.CBL                                     * bbank70p
001400* Layer:       Business logic                                   * bbank70p
001500* Function:    Calculate cost of loan                           * bbank70p
001600***************************************************************** bbank70p
001700                                                                  bbank70p
001800 IDENTIFICATION DIVISION.                                         bbank70p
001900 PROGRAM-ID.                                                      bbank70p
002000     BBANK70P.                                                    bbank70p
002100 DATE-WRITTEN.                                                    bbank70p
002200     September 2002.                                              bbank70p
002300 DATE-COMPILED.                                                   bbank70p
002400     Today.                                                       bbank70p
002500                                                                  bbank70p
002600 ENVIRONMENT DIVISION.                                            bbank70p
002700                                                                  bbank70p
002800 DATA DIVISION.                                                   bbank70p
002900 WORKING-STORAGE SECTION.                                         bbank70p
003000 01  WS-MISC-STORAGE.                                             bbank70p
003100   05  WS-PROGRAM-ID                         PIC X(8)             bbank70p
003200       VALUE 'BBANK70P'.                                          bbank70p
003300   05  WS-INPUT-FLAG                         PIC X(1).            bbank70p
003400     88  INPUT-OK                            VALUE '0'.           bbank70p
003500     88  INPUT-ERROR                         VALUE '1'.           bbank70p
003600   05  WS-RETURN-FLAG                        PIC X(1).            bbank70p
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    bbank70p
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           bbank70p
003900   05  WS-RETURN-MSG                         PIC X(75).           bbank70p
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        bbank70p
004100   05  WS-PFK-FLAG                           PIC X(1).            bbank70p
004200     88  PFK-VALID                           VALUE '0'.           bbank70p
004300     88  PFK-INVALID                         VALUE '1'.           bbank70p
004400   05  WS-ERROR-MSG                          PIC X(75).           bbank70p
004500*                                                                 bbank70p
004600   05  WS-CALC-WORK-AREAS.                                        bbank70p
004700* Used to count no of periods in rate                             bbank70p
004800     10  WS-CALC-WORK-RATE-PERIOD            PIC 9(1).            bbank70p
004900* Generate work area                                              bbank70p
005000     10  WS-CALC-WORK-TEMP                   PIC X(7).            bbank70p
005100* Work area for AMOUNT                                            bbank70p
005200     10  WS-CALC-WORK-AMOUNT                 PIC X(7).            bbank70p
005300     10  WS-CALC-WORK-AMOUNT-N REDEFINES WS-CALC-WORK-AMOUNT      bbank70p
005400                                             PIC 9(7).            bbank70p
005500* Work area for RATE                                              bbank70p
005600     10  WS-CALC-WORK-RATE                   PIC X(7).            bbank70p
005700* Used to hold first part of rate (before the period)             bbank70p
005800     10  WS-CALC-WORK-RATE-P1                PIC X(6).            bbank70p
005900     10  WS-CALC-WORK-RATE-P1-N REDEFINES WS-CALC-WORK-RATE-P1    bbank70p
006000                                             PIC 9(6).            bbank70p
006100* Used to hold second part of rate (before the period)            bbank70p
006200     10  WS-CALC-WORK-RATE-P2                PIC X(6).            bbank70p
006300     10  WS-CALC-WORK-RATE-P2-N REDEFINES WS-CALC-WORK-RATE-P2    bbank70p
006400                                             PIC 9(6).            bbank70p
006500* Used to hold rate as percentage (xxxvxxx)                       bbank70p
006600     10  WS-CALC-WORK-PERC                   PIC X(6).            bbank70p
006700     10  WS-CALC-WORK-PERC-N REDEFINES WS-CALC-WORK-PERC          bbank70p
006800                                             PIC 9(3)V9(3).       bbank70p
006900* Work area for TERM                                              bbank70p
007000     10  WS-CALC-WORK-TERM                   PIC X(5).            bbank70p
007100     10  WS-CALC-WORK-TERM-N REDEFINES WS-CALC-WORK-TERM          bbank70p
007200                                             PIC 9(5).            bbank70p
007300* Work area for PAYMENT                                           bbank70p
007400     10  WS-CALC-WORK-PAYMENT                PIC X(9).            bbank70p
007500     10  WS-CALC-WORK-PAYMENT-N REDEFINES WS-CALC-WORK-PAYMENT    bbank70p
007600                                             PIC $$$$$9.99.       bbank70p
007700                                                                  bbank70p
007800   05  WS-LOAN-AREAS.                                             bbank70p
007900     10  WS-LOAN-PRINCIPAL                   PIC S9(7).           bbank70p
008000     10  WS-LOAN-INTEREST                    PIC SV9(8).          bbank70p
008100     10  WS-LOAN-TERM                        PIC S9(5).           bbank70p
008200     10  WS-LOAN-MONTHLY-PAYMENT             PIC S9(6)V99.        bbank70p
008300                                                                  bbank70p
008400 01  WS-BANK-DATA.                                                bbank70p
008500 COPY CBANKDAT.                                                   bbank70p
008600                                                                  bbank70p
008700 01  WS-HELP-DATA.                                                bbank70p
008800 COPY CHELPD01.                                                   bbank70p
008900                                                                  bbank70p
009000 COPY CABENDD.                                                    bbank70p
009100                                                                  bbank70p
009200 LINKAGE SECTION.                                                 bbank70p
009300 01  DFHCOMMAREA.                                                 bbank70p
009400   05  LK-COMMAREA                           PIC X(6144).         bbank70p
009500                                                                  bbank70p
009600 COPY CENTRY.                                                     bbank70p
009700***************************************************************** bbank70p
009800* Make ourselves re-entrant                                     * bbank70p
009900***************************************************************** bbank70p
010000     MOVE SPACES TO WS-ERROR-MSG.                                 bbank70p
010100                                                                  bbank70p
010200***************************************************************** bbank70p
010300* Move the passed area to Bour area                              *bbank70p
010400***************************************************************** bbank70p
010500     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. bbank70p
010600                                                                  bbank70p
010700***************************************************************** bbank70p
010800* Ensure error message is cleared                               * bbank70p
010900***************************************************************** bbank70p
011000     MOVE SPACES TO BANK-ERROR-MSG.                               bbank70p
011100                                                                  bbank70p
011200***************************************************************** bbank70p
011300* This is the main process                                      * bbank70p
011400***************************************************************** bbank70p
011500                                                                  bbank70p
011600***************************************************************** bbank70p
011700* Save the passed return message and then turn it off           * bbank70p
011800***************************************************************** bbank70p
011900     MOVE BANK-RETURN-MSG TO WS-RETURN-MSG.                       bbank70p
012000     SET BANK-RETURN-MSG-OFF TO TRUE.                             bbank70p
012100                                                                  bbank70p
012200***************************************************************** bbank70p
012300* Check the AID to see if its valid at this point               * bbank70p
012400***************************************************************** bbank70p
012500     SET PFK-INVALID TO TRUE.                                     bbank70p
012600     IF BANK-AID-ENTER OR                                         bbank70p
012700        BANK-AID-PFK03 OR                                         bbank70p
012800        BANK-AID-PFK04                                            bbank70p
012900        SET PFK-VALID TO TRUE                                     bbank70p
013000     END-IF.                                                      bbank70p
013100     IF BANK-AID-PFK01 AND                                        bbank70p
013200        BANK-HELP-INACTIVE                                        bbank70p
013300        SET BANK-HELP-ACTIVE TO TRUE                              bbank70p
013400        SET PFK-VALID TO TRUE                                     bbank70p
013500     END-IF.                                                      bbank70p
013600     IF PFK-INVALID                                               bbank70p
013700        SET BANK-AID-ENTER TO TRUE                                bbank70p
013800     END-IF.                                                      bbank70p
013900                                                                  bbank70p
014000***************************************************************** bbank70p
014100* Check the AID to see if we have to quit                       * bbank70p
014200***************************************************************** bbank70p
014300     IF BANK-AID-PFK03                                            bbank70p
014400        MOVE 'BBANK70P' TO BANK-LAST-PROG                         bbank70p
014500        MOVE 'BBANK99P' TO BANK-NEXT-PROG                         bbank70p
014600        MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        bbank70p
014700        MOVE 'BANK99A' TO BANK-NEXT-MAP                           bbank70p
014800        GO TO COMMON-RETURN                                       bbank70p
014900     END-IF.                                                      bbank70p
015000                                                                  bbank70p
015100***************************************************************** bbank70p
015200* Check the to see if user needs or has been using help         * bbank70p
015300***************************************************************** bbank70p
015400     IF BANK-HELP-ACTIVE                                          bbank70p
015500        IF BANK-AID-PFK04                                         bbank70p
015600           SET BANK-HELP-INACTIVE TO TRUE                         bbank70p
015700           MOVE 00 TO BANK-HELP-SCREEN                            bbank70p
015800           MOVE 'BBANK70P' TO BANK-LAST-PROG                      bbank70p
015900           MOVE 'BBANK70P' TO BANK-NEXT-PROG                      bbank70p
016000           MOVE 'MBANK70' TO BANK-LAST-MAPSET                     bbank70p
016100           MOVE 'HELP70A' TO BANK-LAST-MAP                        bbank70p
016200           MOVE 'MBANK70' TO BANK-NEXT-MAPSET                     bbank70p
016300           MOVE 'BANK70A' TO BANK-NEXT-MAP                        bbank70p
016400           GO TO COMMON-RETURN                                    bbank70p
016500        ELSE                                                      bbank70p
016600           MOVE 01 TO BANK-HELP-SCREEN                            bbank70p
016700           MOVE 'BBANK70P' TO BANK-LAST-PROG                      bbank70p
016800           MOVE 'BBANK70P' TO BANK-NEXT-PROG                      bbank70p
016900           MOVE 'MBANK70' TO BANK-LAST-MAPSET                     bbank70p
017000           MOVE 'BANK70A' TO BANK-LAST-MAP                        bbank70p
017100           MOVE 'MBANK70' TO BANK-NEXT-MAPSET                     bbank70p
017200           MOVE 'HELP70A' TO BANK-NEXT-MAP                        bbank70p
017300           MOVE 'BANK70' TO HELP01I-SCRN                          bbank70p
017400           COPY CHELPX01.                                         bbank70p
017500           MOVE HELP01O-DATA TO BANK-HELP-DATA                    bbank70p
017600           GO TO COMMON-RETURN                                    bbank70p
017700     END-IF.                                                      bbank70p
017800                                                                  bbank70p
017900***************************************************************** bbank70p
018000* Check the AID to see if we have to return to previous screen  * bbank70p
018100***************************************************************** bbank70p
018200     IF BANK-AID-PFK04                                            bbank70p
018300        MOVE 'BBANK70P' TO BANK-LAST-PROG                         bbank70p
018400        MOVE 'BBANK20P' TO BANK-NEXT-PROG                         bbank70p
018500        MOVE 'MBANK20' TO BANK-NEXT-MAPSET                        bbank70p
018600        MOVE 'BANK20A' TO BANK-NEXT-MAP                           bbank70p
018700        SET BANK-AID-ENTER TO TRUE                                bbank70p
018800        GO TO COMMON-RETURN                                       bbank70p
018900     END-IF.                                                      bbank70p
019000                                                                  bbank70p
019100* Check if we have set the screen up before or is this 1st time   bbank70p
019200     IF BANK-LAST-MAPSET IS NOT EQUAL TO 'MBANK70'                bbank70p
019300        MOVE SPACES TO BANK-SCR70-AMOUNT                          bbank70p
019400        MOVE SPACES TO BANK-SCR70-RATE                            bbank70p
019500        MOVE SPACES TO BANK-SCR70-TERM                            bbank70p
019600        MOVE SPACES TO BANK-SCR70-PAYMENT                         bbank70p
019700        MOVE WS-RETURN-MSG TO BANK-ERROR-MSG                      bbank70p
019800        MOVE 'BBANK70P' TO BANK-LAST-PROG                         bbank70p
019900        MOVE 'BBANK70P' TO BANK-NEXT-PROG                         bbank70p
020000        MOVE 'MBANK70' TO BANK-LAST-MAPSET                        bbank70p
020100        MOVE 'BANK70A' TO BANK-LAST-MAP                           bbank70p
020200        MOVE 'MBANK70' TO BANK-NEXT-MAPSET                        bbank70p
020300        MOVE 'BANK70A' TO BANK-NEXT-MAP                           bbank70p
020400        GO TO COMMON-RETURN                                       bbank70p
020500     END-IF.                                                      bbank70p
020600                                                                  bbank70p
020700     PERFORM VALIDATE-DATA THRU                                   bbank70p
020800             VALIDATE-DATA-EXIT.                                  bbank70p
020900                                                                  bbank70p
021000* If we had an error display error and return                     bbank70p
021100     IF INPUT-ERROR                                               bbank70p
021200        MOVE SPACES TO BANK-SCR70-PAYMENT                         bbank70p
021300        MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                       bbank70p
021400        MOVE 'BBANK70P' TO BANK-LAST-PROG                         bbank70p
021500        MOVE 'BBANK70P' TO BANK-NEXT-PROG                         bbank70p
021600        MOVE 'MBANK70' TO BANK-LAST-MAPSET                        bbank70p
021700        MOVE 'BANK70A' TO BANK-LAST-MAP                           bbank70p
021800        MOVE 'MBANK70' TO BANK-NEXT-MAPSET                        bbank70p
021900        MOVE 'BANK70A' TO BANK-NEXT-MAP                           bbank70p
022000        GO TO COMMON-RETURN                                       bbank70p
022100     END-IF.                                                      bbank70p
022200                                                                  bbank70p
022300* Now calculate the monthly cost of the loan                      bbank70p
022400     MOVE WS-CALC-WORK-AMOUNT-N TO WS-LOAN-PRINCIPAL.             bbank70p
022500     DIVIDE 100 INTO WS-CALC-WORK-PERC-N                          bbank70p
022600       GIVING WS-LOAN-INTEREST.                                   bbank70p
022700     MOVE WS-CALC-WORK-TERM-N TO WS-LOAN-TERM.                    bbank70p
022800     MOVE ZERO TO WS-LOAN-MONTHLY-PAYMENT.                        bbank70p
022900                                                                  bbank70p
023000     DIVIDE WS-LOAN-INTEREST BY 12                                bbank70p
023100       GIVING WS-LOAN-INTEREST ROUNDED.                           bbank70p
023200     COMPUTE WS-LOAN-MONTHLY-PAYMENT ROUNDED =                    bbank70p
023300       ((WS-LOAN-INTEREST * ((1 + WS-LOAN-INTEREST)               bbank70p
023400           ** WS-LOAN-TERM)) /                                    bbank70p
023500       (((1 + WS-LOAN-INTEREST) ** WS-LOAN-TERM) - 1 ))           bbank70p
023600         * WS-LOAN-PRINCIPAL.                                     bbank70p
023700     MOVE WS-LOAN-MONTHLY-PAYMENT TO WS-CALC-WORK-PAYMENT-N.      bbank70p
023800     MOVE WS-CALC-WORK-PAYMENT TO BANK-SCR70-PAYMENT.             bbank70p
023900* Left justify the result                                         bbank70p
024000 LEFT-JUST-PAYMENT.                                               bbank70p
024100     IF BANK-SCR70-PAYMENT (1:1) IS EQUAL TO ' '                  bbank70p
024200        MOVE BANK-SCR70-PAYMENT (2:8) TO BANK-SCR70-PAYMENT (1:8) bbank70p
024300        MOVE ' ' TO BANK-SCR70-PAYMENT (9:1)                      bbank70p
024400        GO TO LEFT-JUST-PAYMENT                                   bbank70p
024500     END-IF                                                       bbank70p
024600                                                                  bbank70p
024700     GO TO COMMON-RETURN.                                         bbank70p
024800                                                                  bbank70p
024900 COMMON-RETURN.                                                   bbank70p
025000     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). bbank70p
025100 COPY CRETURN.                                                    bbank70p
025200                                                                  bbank70p
025300 VALIDATE-DATA.                                                   bbank70p
025400     SET INPUT-OK TO TRUE.                                        bbank70p
025500                                                                  bbank70p
025600     MOVE BANK-SCR70-AMOUNT TO WS-CALC-WORK-AMOUNT.               bbank70p
025700     PERFORM VALIDATE-AMOUNT THRU                                 bbank70p
025800             VALIDATE-AMOUNT-EXIT.                                bbank70p
025900     IF NOT INPUT-OK                                              bbank70p
026000        GO TO VALIDATE-DATA-ERROR                                 bbank70p
026100     END-IF.                                                      bbank70p
026200                                                                  bbank70p
026300     MOVE BANK-SCR70-RATE TO WS-CALC-WORK-RATE.                   bbank70p
026400     PERFORM VALIDATE-RATE THRU                                   bbank70p
026500             VALIDATE-RATE-EXIT.                                  bbank70p
026600     IF NOT INPUT-OK                                              bbank70p
026700        GO TO VALIDATE-DATA-ERROR                                 bbank70p
026800     END-IF.                                                      bbank70p
026900                                                                  bbank70p
027000     MOVE BANK-SCR70-TERM TO WS-CALC-WORK-TERM.                   bbank70p
027100     PERFORM VALIDATE-TERM THRU                                   bbank70p
027200             VALIDATE-TERM-EXIT.                                  bbank70p
027300     IF NOT INPUT-OK                                              bbank70p
027400        GO TO VALIDATE-DATA-ERROR                                 bbank70p
027500     END-IF.                                                      bbank70p
027600                                                                  bbank70p
027700     GO TO VALIDATE-DATA-EXIT.                                    bbank70p
027800                                                                  bbank70p
027900 VALIDATE-DATA-ERROR.                                             bbank70p
028000     SET INPUT-ERROR TO TRUE.                                     bbank70p
028100 VALIDATE-DATA-EXIT.                                              bbank70p
028200     EXIT.                                                        bbank70p
028300                                                                  bbank70p
028400 VALIDATE-AMOUNT.                                                 bbank70p
028500      CONTINUE.                                                   bbank70p
028600 VALIDATE-AMOUNT-RIGHT-JUSTIFY.                                   bbank70p
028700     IF WS-CALC-WORK-AMOUNT IS EQUAL TO SPACES OR                 bbank70p
028800        WS-CALC-WORK-AMOUNT IS EQUAL TO LOW-VALUES                bbank70p
028900        MOVE 'Please enter an amount'                             bbank70p
029000          TO WS-ERROR-MSG                                         bbank70p
029100        GO TO VALIDATE-AMOUNT-ERROR                               bbank70p
029200     END-IF.                                                      bbank70p
029300     IF WS-CALC-WORK-AMOUNT (7:1) IS EQUAL TO SPACES OR           bbank70p
029400        WS-CALC-WORK-AMOUNT (7:1) IS EQUAL TO LOW-VALUE           bbank70p
029500        MOVE WS-CALC-WORK-AMOUNT (1:6) TO WS-CALC-WORK-TEMP       bbank70p
029600        MOVE SPACES TO WS-CALC-WORK-AMOUNT                        bbank70p
029700        MOVE WS-CALC-WORK-TEMP (1:6) TO WS-CALC-WORK-AMOUNT (2:6) bbank70p
029800        GO TO VALIDATE-AMOUNT-RIGHT-JUSTIFY                       bbank70p
029900     END-IF.                                                      bbank70p
030000     INSPECT WS-CALC-WORK-AMOUNT                                  bbank70p
030100       REPLACING LEADING SPACES BY ZEROS.                         bbank70p
030200     IF WS-CALC-WORK-AMOUNT IS NOT NUMERIC                        bbank70p
030300        MOVE 'Amount is invalid (not numeric)'                    bbank70p
030400          TO WS-ERROR-MSG                                         bbank70p
030500        GO TO VALIDATE-AMOUNT-ERROR                               bbank70p
030600     END-IF.                                                      bbank70p
030700     IF WS-CALC-WORK-AMOUNT IS EQUAL TO ZERO                      bbank70p
030800        MOVE 'Please enter a non-zero amount'                     bbank70p
030900          TO WS-ERROR-MSG                                         bbank70p
031000        GO TO VALIDATE-AMOUNT-ERROR                               bbank70p
031100     END-IF.                                                      bbank70p
031200                                                                  bbank70p
031300     GO TO VALIDATE-AMOUNT-EXIT.                                  bbank70p
031400                                                                  bbank70p
031500 VALIDATE-AMOUNT-ERROR.                                           bbank70p
031600     SET INPUT-ERROR TO TRUE.                                     bbank70p
031700 VALIDATE-AMOUNT-EXIT.                                            bbank70p
031800     EXIT.                                                        bbank70p
031900                                                                  bbank70p
032000 VALIDATE-RATE.                                                   bbank70p
032100      CONTINUE.                                                   bbank70p
032200 VALIDATE-RATE-RIGHT-JUSTIFY.                                     bbank70p
032300     IF WS-CALC-WORK-RATE IS EQUAL TO SPACES OR                   bbank70p
032400        WS-CALC-WORK-RATE IS EQUAL TO LOW-VALUES                  bbank70p
032500        MOVE 'Please enter an interest rate in the form 999.999'  bbank70p
032600          TO WS-ERROR-MSG                                         bbank70p
032700        GO TO VALIDATE-RATE-ERROR                                 bbank70p
032800     END-IF.                                                      bbank70p
032900     IF WS-CALC-WORK-RATE (7:1) IS EQUAL TO SPACES OR             bbank70p
033000        WS-CALC-WORK-RATE (7:1) IS EQUAL TO LOW-VALUE             bbank70p
033100        MOVE WS-CALC-WORK-RATE (1:6) TO WS-CALC-WORK-TEMP         bbank70p
033200        MOVE SPACES TO WS-CALC-WORK-RATE                          bbank70p
033300        MOVE WS-CALC-WORK-TEMP (1:6) TO WS-CALC-WORK-RATE (2:6)   bbank70p
033400        GO TO VALIDATE-RATE-RIGHT-JUSTIFY                         bbank70p
033500     END-IF.                                                      bbank70p
033600     INSPECT WS-CALC-WORK-RATE REPLACING LEADING SPACES BY ZERO.  bbank70p
033700     MOVE ZERO TO WS-CALC-WORK-RATE-PERIOD.                       bbank70p
033800     MOVE ZEROS TO WS-CALC-WORK-RATE-P1.                          bbank70p
033900     MOVE ZEROS TO WS-CALC-WORK-RATE-P2.                          bbank70p
034000* Rate is in form .xxxxxx                                         bbank70p
034100     IF WS-CALC-WORK-RATE (1:1) IS EQUAL TO '.'                   bbank70p
034200        MOVE ZEROS                   TO WS-CALC-WORK-RATE-P1      bbank70p
034300        MOVE WS-CALC-WORK-RATE (2:6) TO WS-CALC-WORK-RATE-P2 (1:6)bbank70p
034400        ADD 1 TO WS-CALC-WORK-RATE-PERIOD                         bbank70p
034500     END-IF.                                                      bbank70p
034600* Rate is in form x.xxxxx                                         bbank70p
034700     IF WS-CALC-WORK-RATE (2:1) IS EQUAL TO '.'                   bbank70p
034800        MOVE WS-CALC-WORK-RATE (1:1) TO WS-CALC-WORK-RATE-P1 (6:1)bbank70p
034900        MOVE WS-CALC-WORK-RATE (3:5) TO WS-CALC-WORK-RATE-P2 (1:5)bbank70p
035000        ADD 1 TO WS-CALC-WORK-RATE-PERIOD                         bbank70p
035100     END-IF.                                                      bbank70p
035200* Rate is in form xx.xxxx                                         bbank70p
035300     IF WS-CALC-WORK-RATE (3:1) IS EQUAL TO '.'                   bbank70p
035400        MOVE WS-CALC-WORK-RATE (1:2) TO WS-CALC-WORK-RATE-P1 (5:2)bbank70p
035500        MOVE WS-CALC-WORK-RATE (4:4) TO WS-CALC-WORK-RATE-P2 (1:4)bbank70p
035600        ADD 1 TO WS-CALC-WORK-RATE-PERIOD                         bbank70p
035700     END-IF.                                                      bbank70p
035800* Rate is in form xxx.xxx                                         bbank70p
035900     IF WS-CALC-WORK-RATE (4:1) IS EQUAL TO '.'                   bbank70p
036000        MOVE WS-CALC-WORK-RATE (1:3) TO WS-CALC-WORK-RATE-P1 (4:3)bbank70p
036100        MOVE WS-CALC-WORK-RATE (5:3) TO WS-CALC-WORK-RATE-P2 (1:3)bbank70p
036200        ADD 1 TO WS-CALC-WORK-RATE-PERIOD                         bbank70p
036300     END-IF.                                                      bbank70p
036400* Rate is in form xxxx.xx                                         bbank70p
036500     IF WS-CALC-WORK-RATE (5:1) IS EQUAL TO '.'                   bbank70p
036600        MOVE WS-CALC-WORK-RATE (1:4) TO WS-CALC-WORK-RATE-P1 (3:4)bbank70p
036700        MOVE WS-CALC-WORK-RATE (6:2) TO WS-CALC-WORK-RATE-P2 (1:2)bbank70p
036800        ADD 1 TO WS-CALC-WORK-RATE-PERIOD                         bbank70p
036900     END-IF.                                                      bbank70p
037000* Rate is in form xxxxx.x                                         bbank70p
037100     IF WS-CALC-WORK-RATE (6:1) IS EQUAL TO '.'                   bbank70p
037200        MOVE WS-CALC-WORK-RATE (1:5) TO WS-CALC-WORK-RATE-P1 (2:5)bbank70p
037300        MOVE WS-CALC-WORK-RATE (7:1) TO WS-CALC-WORK-RATE-P2 (1:1)bbank70p
037400        ADD 1 TO WS-CALC-WORK-RATE-PERIOD                         bbank70p
037500     END-IF.                                                      bbank70p
037600* Rate is in form xxxxxx.                                         bbank70p
037700     IF WS-CALC-WORK-RATE (7:1) IS EQUAL TO '.'                   bbank70p
037800        MOVE WS-CALC-WORK-RATE (1:6) TO WS-CALC-WORK-RATE-P1 (1:6)bbank70p
037900        MOVE ZEROS                   TO WS-CALC-WORK-RATE-P2 (1:1)bbank70p
038000        ADD 1 TO WS-CALC-WORK-RATE-PERIOD                         bbank70p
038100     END-IF.                                                      bbank70p
038200     IF WS-CALC-WORK-RATE-PERIOD IS NOT EQUAL TO 1                bbank70p
038300        MOVE 'Decimal point missing/misplaced in interest rate'   bbank70p
038400          TO WS-ERROR-MSG                                         bbank70p
038500        GO TO VALIDATE-RATE-ERROR                                 bbank70p
038600     END-IF.                                                      bbank70p
038700     IF WS-CALC-WORK-RATE-P1 IS NOT NUMERIC OR                    bbank70p
038800        WS-CALC-WORK-RATE-P2 IS NOT NUMERIC                       bbank70p
038900        MOVE 'Rate is not numeric'                                bbank70p
039000          TO WS-ERROR-MSG                                         bbank70p
039100        GO TO VALIDATE-RATE-ERROR                                 bbank70p
039200     END-IF.                                                      bbank70p
039300     IF WS-CALC-WORK-RATE-P2 (4:3) IS NOT EQUAL TO '000'          bbank70p
039400        MOVE 'Rate has too many decimal places'                   bbank70p
039500          TO WS-ERROR-MSG                                         bbank70p
039600        GO TO VALIDATE-RATE-ERROR                                 bbank70p
039700     END-IF.                                                      bbank70p
039800* Bring parts of rate together with no physical decimal point     bbank70p
039900     MOVE WS-CALC-WORK-RATE-P1 (4:3) TO WS-CALC-WORK-PERC (1:3).  bbank70p
040000     MOVE WS-CALC-WORK-RATE-P2 (1:3) TO WS-CALC-WORK-PERC (4:3).  bbank70p
040100                                                                  bbank70p
040200     IF WS-CALC-WORK-PERC-N IS NOT GREATER THAN ZERO              bbank70p
040300        MOVE 'Nothing''s free. Rate must be greater than 0%'      bbank70p
040400          TO WS-ERROR-MSG                                         bbank70p
040500        GO TO VALIDATE-RATE-ERROR                                 bbank70p
040600     END-IF.                                                      bbank70p
040700     IF WS-CALC-WORK-PERC-N IS NOT LESS THAN 100.000              bbank70p
040800        MOVE 'Outrageous rate - 100% or more'                     bbank70p
040900          TO WS-ERROR-MSG                                         bbank70p
041000        GO TO VALIDATE-RATE-ERROR                                 bbank70p
041100     END-IF.                                                      bbank70p
041200                                                                  bbank70p
041300     GO TO VALIDATE-RATE-EXIT.                                    bbank70p
041400                                                                  bbank70p
041500 VALIDATE-RATE-ERROR.                                             bbank70p
041600     SET INPUT-ERROR TO TRUE.                                     bbank70p
041700 VALIDATE-RATE-EXIT.                                              bbank70p
041800     EXIT.                                                        bbank70p
041900                                                                  bbank70p
042000 VALIDATE-TERM.                                                   bbank70p
042100      CONTINUE.                                                   bbank70p
042200 VALIDATE-TERM-RIGHT-JUSTIFY.                                     bbank70p
042300     IF WS-CALC-WORK-TERM IS EQUAL TO SPACES OR                   bbank70p
042400        WS-CALC-WORK-TERM IS EQUAL TO LOW-VALUES                  bbank70p
042500        MOVE 'Please enter a term as a number of months'          bbank70p
042600          TO WS-ERROR-MSG                                         bbank70p
042700        GO TO VALIDATE-TERM-ERROR                                 bbank70p
042800     END-IF.                                                      bbank70p
042900     IF WS-CALC-WORK-TERM (5:1) IS EQUAL TO SPACES OR             bbank70p
043000        WS-CALC-WORK-TERM (5:1) IS EQUAL TO LOW-VALUE             bbank70p
043100        MOVE WS-CALC-WORK-TERM (1:4) TO WS-CALC-WORK-TEMP         bbank70p
043200        MOVE SPACES TO WS-CALC-WORK-TERM                          bbank70p
043300        MOVE WS-CALC-WORK-TEMP (1:4) TO WS-CALC-WORK-TERM (2:4)   bbank70p
043400        GO TO VALIDATE-TERM-RIGHT-JUSTIFY                         bbank70p
043500     END-IF.                                                      bbank70p
043600     INSPECT WS-CALC-WORK-TERM                                    bbank70p
043700       REPLACING LEADING SPACES BY ZEROS.                         bbank70p
043800     IF WS-CALC-WORK-TERM IS NOT NUMERIC                          bbank70p
043900        MOVE 'Term is invalid (not numeric)'                      bbank70p
044000          TO WS-ERROR-MSG                                         bbank70p
044100        GO TO VALIDATE-TERM-ERROR                                 bbank70p
044200     END-IF.                                                      bbank70p
044300     IF WS-CALC-WORK-TERM IS EQUAL TO ZERO                        bbank70p
044400        MOVE 'Please enter a non-zero term'                       bbank70p
044500          TO WS-ERROR-MSG                                         bbank70p
044600        GO TO VALIDATE-TERM-ERROR                                 bbank70p
044700     END-IF.                                                      bbank70p
044800     IF WS-CALC-WORK-TERM-N IS GREATER THAN 1200                  bbank70p
044900        MOVE 'Term exceeds 100 years!'                            bbank70p
045000          TO WS-ERROR-MSG                                         bbank70p
045100        GO TO VALIDATE-TERM-ERROR                                 bbank70p
045200     END-IF.                                                      bbank70p
045300                                                                  bbank70p
045400     GO TO VALIDATE-TERM-EXIT.                                    bbank70p
045500                                                                  bbank70p
045600 VALIDATE-TERM-ERROR.                                             bbank70p
045700     SET INPUT-ERROR TO TRUE.                                     bbank70p
045800 VALIDATE-TERM-EXIT.                                              bbank70p
045900     EXIT.                                                        bbank70p
046000                                                                  bbank70p
046100* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     bbank70p
