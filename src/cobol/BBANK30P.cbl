000100***************************************************************** bbank30p
000200*                                                               * bbank30p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * bbank30p
000400*   This demonstration program is provided for use by users     * bbank30p
000500*   of Micro Focus products and may be used, modified and       * bbank30p
000600*   distributed as part of your application provided that       * bbank30p
000700*   you properly acknowledge the copyright of Micro Focus       * bbank30p
000800*   in this material.                                           * bbank30p
000900*                                                               * bbank30p
001000***************************************************************** bbank30p
001100                                                                  bbank30p
001200***************************************************************** bbank30p
001300* Program:     BBANK30P.CBL                                     * bbank30p
001400* Layer:       Business logic                                   * bbank30p
001500* Function:    Display account balances                         * bbank30p
001600***************************************************************** bbank30p
001700                                                                  bbank30p
001800 IDENTIFICATION DIVISION.                                         bbank30p
001900 PROGRAM-ID.                                                      bbank30p
002000     BBANK30P.                                                    bbank30p
002100 DATE-WRITTEN.                                                    bbank30p
002200     September 2002.                                              bbank30p
002300 DATE-COMPILED.                                                   bbank30p
002400     Today.                                                       bbank30p
002500                                                                  bbank30p
002600 ENVIRONMENT DIVISION.                                            bbank30p
002700                                                                  bbank30p
002800 DATA DIVISION.                                                   bbank30p
002900 WORKING-STORAGE SECTION.                                         bbank30p
003000 01  WS-MISC-STORAGE.                                             bbank30p
003100   05  WS-PROGRAM-ID                         PIC X(8)             bbank30p
003200       VALUE 'BBANK30P'.                                          bbank30p
003300   05  WS-INPUT-FLAG                         PIC X(1).            bbank30p
003400     88  INPUT-OK                            VALUE '0'.           bbank30p
003500     88  INPUT-ERROR                         VALUE '1'.           bbank30p
003600   05  WS-RETURN-FLAG                        PIC X(1).            bbank30p
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    bbank30p
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           bbank30p
003900   05  WS-RETURN-MSG                         PIC X(75).           bbank30p
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        bbank30p
004100   05  WS-PFK-FLAG                           PIC X(1).            bbank30p
004200     88  PFK-VALID                           VALUE '0'.           bbank30p
004300     88  PFK-INVALID                         VALUE '1'.           bbank30p
004400   05  WS-ERROR-MSG                          PIC X(75).           bbank30p
004500   05  WS-SUB1                               PIC S9(4) COMP.      bbank30p
004600   05  WS-SUB-LIMIT                          PIC S9(4) COMP.      bbank30p
004700   05  WS-EDIT-BALANCE                       PIC Z,ZZZ,ZZ9.99-.   bbank30p
004800   05  WS-DYNAMIC-PGM                        PIC X(8)             bbank30p
004900       VALUE 'UNKNOWN'.                                           bbank30p
005000                                                                  bbank30p
005100 01  WS-SERVICE-CHARGES.                                          bbank30p
005200   05  WS-SRV-MSG.                                                bbank30p
005300     10  FILLER                              PIC X(43)            bbank30p
005400         VALUE 'Service charges are estimated based on your'.     bbank30p
005500     10  FILLER                              PIC X(19)            bbank30p
005600         VALUE ' existing balances '.                             bbank30p
005700   05  WS-SRV-BAL                            PIC X(9).            bbank30p
005800   05  WS-SRV-BAL-N REDEFINES WS-SRV-BAL     PIC S9(7)V99.        bbank30p
005900   05  WS-SRV-CHARGE                         PIC ZZ9.99           bbank30p
006000                                             BLANK ZERO.          bbank30p
006100   05  WS-SRV-AMT                            PIC 9(3)V99.         bbank30p
006200   05  WS-SRV-CHARGE-LIMITS.                                      bbank30p
006300     10  WS-SRV-BAND0.                                            bbank30p
006400       15  WS-SRV-BAL0     VALUE 9999999       PIC S9(7).         bbank30p
006500       15  WS-SRV-CHG0     VALUE 050.00        PIC 9(3)V99.       bbank30p
006600     10  WS-SRV-BAND1.                                            bbank30p
006700       15  WS-SRV-BAL1     VALUE 0000000       PIC S9(7).         bbank30p
006800       15  WS-SRV-CHG1     VALUE 025.00        PIC 9(3)V99.       bbank30p
006900     10  WS-SRV-BAND2.                                            bbank30p
007000       15  WS-SRV-BAL2     VALUE 0001000       PIC S9(7).         bbank30p
007100       15  WS-SRV-CHG2     VALUE 020.00        PIC 9(3)V99.       bbank30p
007200     10  WS-SRV-BAND3.                                            bbank30p
007300       15  WS-SRV-BAL3     VALUE 0005000       PIC S9(7).         bbank30p
007400       15  WS-SRV-CHG3     VALUE 015.00        PIC 9(3)V99.       bbank30p
007500     10  WS-SRV-BAND4.                                            bbank30p
007600       15  WS-SRV-BAL4     VALUE 0010000       PIC S9(7).         bbank30p
007700       15  WS-SRV-CHG4     VALUE 010.00        PIC 9(3)V99.       bbank30p
007800     10  WS-SRV-BAND5.                                            bbank30p
007900       15  WS-SRV-BAL5     VALUE 0100000       PIC S9(7).         bbank30p
008000       15  WS-SRV-CHG5     VALUE 000.00        PIC 9(3)V99.       bbank30p
008100                                                                  bbank30p
008200 01  WS-TIME-DATE-WORK-AREA.                                      bbank30p
008300 COPY CDATED.                                                     bbank30p
008400                                                                  bbank30p
008500 01  WS-BANK-DATA.                                                bbank30p
008600 COPY CBANKDAT.                                                   bbank30p
008700                                                                  bbank30p
008800 01  WS-HELP-DATA.                                                bbank30p
008900 COPY CHELPD01.                                                   bbank30p
009000                                                                  bbank30p
009100 01  WS-ACCOUNT-DATA.                                             bbank30p
009200 COPY CBANKD03.                                                   bbank30p
009300                                                                  bbank30p
009400 COPY CABENDD.                                                    bbank30p
009500                                                                  bbank30p
009600 LINKAGE SECTION.                                                 bbank30p
009700 01  DFHCOMMAREA.                                                 bbank30p
009800   05  LK-COMMAREA                           PIC X(6144).         bbank30p
009900                                                                  bbank30p
010000 COPY CENTRY.                                                     bbank30p
010100***************************************************************** bbank30p
010200* Make ourselves re-entrant                                     * bbank30p
010300***************************************************************** bbank30p
010400     MOVE SPACES TO WS-ERROR-MSG.                                 bbank30p
010500                                                                  bbank30p
010600***************************************************************** bbank30p
010700* Move the passed area to our area                              * bbank30p
010800***************************************************************** bbank30p
010900     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. bbank30p
011000                                                                  bbank30p
011100***************************************************************** bbank30p
011200* Ensure error message is cleared                               * bbank30p
011300***************************************************************** bbank30p
011400     MOVE SPACES TO BANK-ERROR-MSG.                               bbank30p
011500                                                                  bbank30p
011600***************************************************************** bbank30p
011700* This is the main process                                      * bbank30p
011800***************************************************************** bbank30p
011900                                                                  bbank30p
012000***************************************************************** bbank30p
012100* Save the passed return flag and then turn it off              * bbank30p
012200***************************************************************** bbank30p
012300     MOVE BANK-RETURN-FLAG TO WS-RETURN-FLAG.                     bbank30p
012400     SET BANK-RETURN-FLAG-OFF TO TRUE.                            bbank30p
012500                                                                  bbank30p
012600***************************************************************** bbank30p
012700* Check the AID to see if its valid at this point               * bbank30p
012800***************************************************************** bbank30p
012900     SET PFK-INVALID TO TRUE.                                     bbank30p
013000     IF BANK-AID-ENTER OR                                         bbank30p
013100        BANK-AID-PFK03 OR                                         bbank30p
013200        BANK-AID-PFK04 OR                                         bbank30p
013300        BANK-AID-PFK06                                            bbank30p
013400        SET PFK-VALID TO TRUE                                     bbank30p
013500     END-IF.                                                      bbank30p
013600     IF BANK-AID-PFK01 AND                                        bbank30p
013700        BANK-HELP-INACTIVE                                        bbank30p
013800        SET BANK-HELP-ACTIVE TO TRUE                              bbank30p
013900        SET PFK-VALID TO TRUE                                     bbank30p
014000     END-IF.                                                      bbank30p
014100     IF PFK-INVALID                                               bbank30p
014200        SET BANK-AID-ENTER TO TRUE                                bbank30p
014300     END-IF.                                                      bbank30p
014400                                                                  bbank30p
014500***************************************************************** bbank30p
014600* Check the AID to see if we have to quit                       * bbank30p
014700***************************************************************** bbank30p
014800     IF BANK-AID-PFK03                                            bbank30p
014900        MOVE 'BBANK30P' TO BANK-LAST-PROG                         bbank30p
015000        MOVE 'BBANK99P' TO BANK-NEXT-PROG                         bbank30p
015100        MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        bbank30p
015200        MOVE 'BANK99A' TO BANK-NEXT-MAP                           bbank30p
015300        GO TO COMMON-RETURN                                       bbank30p
015400     END-IF.                                                      bbank30p
015500                                                                  bbank30p
015600***************************************************************** bbank30p
015700* Check the to see if user needs or has been using help         * bbank30p
015800***************************************************************** bbank30p
015900     IF BANK-HELP-ACTIVE                                          bbank30p
016000        IF BANK-AID-PFK04                                         bbank30p
016100           SET BANK-HELP-INACTIVE TO TRUE                         bbank30p
016200           MOVE 00 TO BANK-HELP-SCREEN                            bbank30p
016300           MOVE 'BBANK30P' TO BANK-LAST-PROG                      bbank30p
016400           MOVE 'BBANK30P' TO BANK-NEXT-PROG                      bbank30p
016500           MOVE 'MBANK30' TO BANK-LAST-MAPSET                     bbank30p
016600           MOVE 'HELP30A' TO BANK-LAST-MAP                        bbank30p
016700           MOVE 'MBANK30' TO BANK-NEXT-MAPSET                     bbank30p
016800           MOVE 'BANK30A' TO BANK-NEXT-MAP                        bbank30p
016900           GO TO COMMON-RETURN                                    bbank30p
017000        ELSE                                                      bbank30p
017100           MOVE 01 TO BANK-HELP-SCREEN                            bbank30p
017200           MOVE 'BBANK30P' TO BANK-LAST-PROG                      bbank30p
017300           MOVE 'BBANK30P' TO BANK-NEXT-PROG                      bbank30p
017400           MOVE 'MBANK30' TO BANK-LAST-MAPSET                     bbank30p
017500           MOVE 'BANK30A' TO BANK-LAST-MAP                        bbank30p
017600           MOVE 'MBANK30' TO BANK-NEXT-MAPSET                     bbank30p
017700           MOVE 'HELP30A' TO BANK-NEXT-MAP                        bbank30p
017800           MOVE 'BANK30' TO HELP01I-SCRN                          bbank30p
017900           COPY CHELPX01.                                         bbank30p
018000           MOVE HELP01O-DATA TO BANK-HELP-DATA                    bbank30p
018100           GO TO COMMON-RETURN                                    bbank30p
018200     END-IF.                                                      bbank30p
018300                                                                  bbank30p
018400***************************************************************** bbank30p
018500* Check the AID to see if we have to return to previous screen  * bbank30p
018600***************************************************************** bbank30p
018700     IF BANK-AID-PFK04                                            bbank30p
018800        MOVE 'BBANK30P' TO BANK-LAST-PROG                         bbank30p
018900        MOVE 'BBANK20P' TO BANK-NEXT-PROG                         bbank30p
019000        MOVE 'MBANK20' TO BANK-NEXT-MAPSET                        bbank30p
019100        MOVE 'BANK20A' TO BANK-NEXT-MAP                           bbank30p
019200        SET BANK-AID-ENTER TO TRUE                                bbank30p
019300        GO TO COMMON-RETURN                                       bbank30p
019400     END-IF.                                                      bbank30p
019500                                                                  bbank30p
019600* Check if we have set the screen up before or is this 1st time   bbank30p
019700     IF BANK-LAST-MAPSET IS NOT EQUAL TO 'MBANK30'                bbank30p
019800        MOVE WS-RETURN-MSG TO BANK-ERROR-MSG                      bbank30p
019900        MOVE 'BBANK30P' TO BANK-LAST-PROG                         bbank30p
020000        MOVE 'BBANK30P' TO BANK-NEXT-PROG                         bbank30p
020100        MOVE 'MBANK30' TO BANK-LAST-MAPSET                        bbank30p
020200        MOVE 'BANK30A' TO BANK-LAST-MAP                           bbank30p
020300        MOVE 'MBANK30' TO BANK-NEXT-MAPSET                        bbank30p
020400        MOVE 'BANK30A' TO BANK-NEXT-MAP                           bbank30p
020500        PERFORM POPULATE-SCREEN-DATA THRU                         bbank30p
020600                POPULATE-SCREEN-DATA-EXIT                         bbank30p
020700        GO TO COMMON-RETURN                                       bbank30p
020800     END-IF.                                                      bbank30p
020900                                                                  bbank30p
021000     MOVE 0 TO WS-SUB1.                                           bbank30p
021100     MOVE SPACES TO BANK-SCR40-ACC.                               bbank30p
021200* Calculate no of entries in table                                bbank30p
021300     DIVIDE LENGTH OF BANK-SCREEN30-INPUT (1)                     bbank30p
021400       INTO LENGTH OF BANK-SCREEN30-INPUT-DATA                    bbank30p
021500         GIVING WS-SUB-LIMIT.                                     bbank30p
021600 SCAN-INPUT-LOOP.                                                 bbank30p
021700     ADD 1 TO WS-SUB1.                                            bbank30p
021800     IF BANK-SCR30-DET (WS-SUB1) IS NOT = LOW-VALUES              bbank30p
021900        MOVE LOW-VALUES TO BANK-SCR30-DET (WS-SUB1)               bbank30p
022000        IF BANK-AID-ENTER                                         bbank30p
022100           MOVE BANK-SCR30-ACC (WS-SUB1) TO BANK-SCR35-ACC        bbank30p
022200           MOVE BANK-SCR30-DSC (WS-SUB1) TO BANK-SCR35-DSC        bbank30p
022300           MOVE BANK-SCR30-BAL (WS-SUB1) TO BANK-SCR35-BAL        bbank30p
022400           MOVE BANK-SCR30-DTE (WS-SUB1) TO BANK-SCR35-DTE        bbank30p
022500           MOVE BANK-SCR30-TXN (WS-SUB1) TO BANK-SCR35-TRANS      bbank30p
022600           MOVE 'BBANK30P' TO BANK-LAST-PROG                      bbank30p
022700           MOVE 'BBANK35P' TO BANK-NEXT-PROG                      bbank30p
022800           MOVE 'BBANK30P' TO BANK-RETURN-TO-PROG                 bbank30p
022900           GO TO COMMON-RETURN                                    bbank30p
023000        END-IF                                                    bbank30p
023100        IF BANK-AID-PFK06                                         bbank30p
023200           IF BANK-SCR30-TXN (WS-SUB1) IS NOT EQUAL TO SPACES     bbank30p
023300              MOVE BANK-SCR30-ACC (WS-SUB1) TO BANK-SCR40-ACC     bbank30p
023400              MOVE BANK-SCR30-DSC (WS-SUB1) TO BANK-SCR40-ACCTYPE bbank30p
023500              MOVE 'BBANK30P' TO BANK-LAST-PROG                   bbank30p
023600              MOVE 'BBANK40P' TO BANK-NEXT-PROG                   bbank30p
023700              MOVE 'BBANK30P' TO BANK-RETURN-TO-PROG              bbank30p
023800              GO TO COMMON-RETURN                                 bbank30p
023900           ELSE                                                   bbank30p
024000              MOVE 'No transactions to show' TO WS-ERROR-MSG      bbank30p
024100              MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                 bbank30p
024200              MOVE 'BBANK30P' TO BANK-LAST-PROG                   bbank30p
024300              MOVE 'BBANK30P' TO BANK-NEXT-PROG                   bbank30p
024400              MOVE 'MBANK30' TO BANK-LAST-MAPSET                  bbank30p
024500              MOVE 'BANK30A' TO BANK-LAST-MAP                     bbank30p
024600              MOVE 'MBANK30' TO BANK-NEXT-MAPSET                  bbank30p
024700              MOVE 'BANK30A' TO BANK-NEXT-MAP                     bbank30p
024800              GO TO COMMON-RETURN                                 bbank30p
024900           END-IF                                                 bbank30p
025000        END-IF                                                    bbank30p
025100     END-IF.                                                      bbank30p
025200                                                                  bbank30p
025300     IF WS-SUB1 IS LESS THAN WS-SUB-LIMIT                         bbank30p
025400        GO TO SCAN-INPUT-LOOP                                     bbank30p
025500     END-IF.                                                      bbank30p
025600                                                                  bbank30p
025700* As this is a display screen we need PFK03/4 to get out so we    bbank30p
025800* will just redisplay the data                                    bbank30p
025900     PERFORM POPULATE-SCREEN-DATA THRU                            bbank30p
026000             POPULATE-SCREEN-DATA-EXIT.                           bbank30p
026100                                                                  bbank30p
026200     GO TO COMMON-RETURN.                                         bbank30p
026300                                                                  bbank30p
026400 COMMON-RETURN.                                                   bbank30p
026500     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). bbank30p
026600 COPY CRETURN.                                                    bbank30p
026700                                                                  bbank30p
026800 VALIDATE-DATA.                                                   bbank30p
026900     SET INPUT-OK TO TRUE.                                        bbank30p
027000     GO TO VALIDATE-DATA-EXIT.                                    bbank30p
027100                                                                  bbank30p
027200 VALIDATE-DATA-ERROR.                                             bbank30p
027300     SET INPUT-ERROR TO TRUE.                                     bbank30p
027400 VALIDATE-DATA-EXIT.                                              bbank30p
027500     EXIT.                                                        bbank30p
027600                                                                  bbank30p
027700 POPULATE-SCREEN-DATA.                                            bbank30p
027800     MOVE SPACES TO CD03-DATA.                                    bbank30p
027900     MOVE BANK-USERID TO CD03I-CONTACT-ID.                        bbank30p
028000* Now go get the data                                             bbank30p
028100 COPY CBANKX03.                                                   bbank30p
028200     MOVE CD03O-ACC1 TO BANK-SCR30-ACC1.                          bbank30p
028300     MOVE CD03O-DSC1 TO BANK-SCR30-DSC1.                          bbank30p
028400     IF CD03O-BAL1 IS EQUAL TO SPACES                             bbank30p
028500        MOVE CD03O-BAL1 TO BANK-SCR30-BAL1                        bbank30p
028600     ELSE                                                         bbank30p
028700        MOVE CD03O-BAL1N TO WS-EDIT-BALANCE                       bbank30p
028800        MOVE WS-EDIT-BALANCE TO BANK-SCR30-BAL1                   bbank30p
028900     END-IF.                                                      bbank30p
029000     MOVE CD03O-BAL1 TO WS-SRV-BAL.                               bbank30p
029100     PERFORM CALC-SERVICE-CHARGE THRU                             bbank30p
029200             CALC-SERVICE-CHARGE-EXIT.                            bbank30p
029300     MOVE WS-SRV-CHARGE TO BANK-SCR30-SRV1.                       bbank30p
029400     IF CD03O-DTE1 IS EQUAL TO SPACES                             bbank30p
029500        MOVE CD03O-DTE1 TO BANK-SCR30-DTE1                        bbank30p
029600     ELSE                                                         bbank30p
029700        MOVE CD03O-DTE1 TO DDI-DATA                               bbank30p
029800        SET DDI-ISO TO TRUE                                       bbank30p
029900        SET DDO-DD-MMM-YYYY TO TRUE                               bbank30p
030000        PERFORM CALL-DATECONV THRU                                bbank30p
030100               CALL-DATECONV-EXIT                                 bbank30p
030200        MOVE DDO-DATA TO BANK-SCR30-DTE1                          bbank30p
030300     END-IF.                                                      bbank30p
030400     MOVE CD03O-TXN1 TO BANK-SCR30-TXN1.                          bbank30p
030500                                                                  bbank30p
030600     MOVE CD03O-ACC2 TO BANK-SCR30-ACC2.                          bbank30p
030700     MOVE CD03O-DSC2 TO BANK-SCR30-DSC2.                          bbank30p
030800     IF CD03O-BAL2 IS EQUAL TO SPACES                             bbank30p
030900        MOVE CD03O-BAL2 TO BANK-SCR30-BAL2                        bbank30p
031000     ELSE                                                         bbank30p
031100        MOVE CD03O-BAL2N TO WS-EDIT-BALANCE                       bbank30p
031200        MOVE WS-EDIT-BALANCE TO BANK-SCR30-BAL2                   bbank30p
031300     END-IF.                                                      bbank30p
031400     MOVE CD03O-BAL2 TO WS-SRV-BAL                                bbank30p
031500     PERFORM CALC-SERVICE-CHARGE THRU                             bbank30p
031600             CALC-SERVICE-CHARGE-EXIT.                            bbank30p
031700     MOVE WS-SRV-CHARGE TO BANK-SCR30-SRV2.                       bbank30p
031800     IF CD03O-DTE2 IS EQUAL TO SPACES                             bbank30p
031900        MOVE CD03O-DTE2 TO BANK-SCR30-DTE2                        bbank30p
032000     ELSE                                                         bbank30p
032100        MOVE CD03O-DTE2 TO DDI-DATA                               bbank30p
032200        SET DDI-ISO TO TRUE                                       bbank30p
032300        SET DDO-DD-MMM-YYYY TO TRUE                               bbank30p
032400        PERFORM CALL-DATECONV THRU                                bbank30p
032500                CALL-DATECONV-EXIT                                bbank30p
032600        MOVE DDO-DATA TO BANK-SCR30-DTE2                          bbank30p
032700     END-IF.                                                      bbank30p
032800     MOVE CD03O-TXN2 TO BANK-SCR30-TXN2.                          bbank30p
032900                                                                  bbank30p
033000     MOVE CD03O-ACC3 TO BANK-SCR30-ACC3.                          bbank30p
033100     MOVE CD03O-DSC3 TO BANK-SCR30-DSC3.                          bbank30p
033200     IF CD03O-BAL3 IS EQUAL TO SPACES                             bbank30p
033300        MOVE CD03O-BAL3 TO BANK-SCR30-BAL3                        bbank30p
033400     ELSE                                                         bbank30p
033500        MOVE CD03O-BAL3N TO WS-EDIT-BALANCE                       bbank30p
033600        MOVE WS-EDIT-BALANCE TO BANK-SCR30-BAL3                   bbank30p
033700     END-IF.                                                      bbank30p
033800     MOVE CD03O-BAL3 TO WS-SRV-BAL.                               bbank30p
033900     PERFORM CALC-SERVICE-CHARGE THRU                             bbank30p
034000             CALC-SERVICE-CHARGE-EXIT.                            bbank30p
034100     MOVE WS-SRV-CHARGE TO BANK-SCR30-SRV3.                       bbank30p
034200     IF CD03O-DTE3 IS EQUAL TO SPACES                             bbank30p
034300        MOVE CD03O-DTE3 TO BANK-SCR30-DTE3                        bbank30p
034400     ELSE                                                         bbank30p
034500        MOVE CD03O-DTE3 TO DDI-DATA                               bbank30p
034600        SET DDI-ISO TO TRUE                                       bbank30p
034700        SET DDO-DD-MMM-YYYY TO TRUE                               bbank30p
034800        PERFORM CALL-DATECONV THRU                                bbank30p
034900                CALL-DATECONV-EXIT                                bbank30p
035000        MOVE DDO-DATA TO BANK-SCR30-DTE3                          bbank30p
035100     END-IF.                                                      bbank30p
035200     MOVE CD03O-TXN3 TO BANK-SCR30-TXN3.                          bbank30p
035300                                                                  bbank30p
035400     MOVE CD03O-ACC4 TO BANK-SCR30-ACC4.                          bbank30p
035500     MOVE CD03O-DSC4 TO BANK-SCR30-DSC4.                          bbank30p
035600     IF CD03O-BAL4 IS EQUAL TO SPACES                             bbank30p
035700        MOVE CD03O-BAL4 TO BANK-SCR30-BAL4                        bbank30p
035800     ELSE                                                         bbank30p
035900        MOVE CD03O-BAL4N TO WS-EDIT-BALANCE                       bbank30p
036000        MOVE WS-EDIT-BALANCE TO BANK-SCR30-BAL4                   bbank30p
036100     END-IF.                                                      bbank30p
036200     MOVE CD03O-BAL4 TO WS-SRV-BAL.                               bbank30p
036300     PERFORM CALC-SERVICE-CHARGE THRU                             bbank30p
036400             CALC-SERVICE-CHARGE-EXIT.                            bbank30p
036500     MOVE WS-SRV-CHARGE TO BANK-SCR30-SRV4.                       bbank30p
036600     IF CD03O-DTE4 IS EQUAL TO SPACES                             bbank30p
036700        MOVE CD03O-DTE4 TO BANK-SCR30-DTE4                        bbank30p
036800     ELSE                                                         bbank30p
036900        MOVE CD03O-DTE4 TO DDI-DATA                               bbank30p
037000        SET DDI-ISO TO TRUE                                       bbank30p
037100        SET DDO-DD-MMM-YYYY TO TRUE                               bbank30p
037200        PERFORM CALL-DATECONV THRU                                bbank30p
037300                CALL-DATECONV-EXIT                                bbank30p
037400        MOVE DDO-DATA TO BANK-SCR30-DTE4                          bbank30p
037500     END-IF.                                                      bbank30p
037600     MOVE CD03O-TXN4 TO BANK-SCR30-TXN4.                          bbank30p
037700                                                                  bbank30p
037800     MOVE CD03O-ACC5 TO BANK-SCR30-ACC5.                          bbank30p
037900     MOVE CD03O-DSC5 TO BANK-SCR30-DSC5.                          bbank30p
038000     IF CD03O-BAL5 IS EQUAL TO SPACES                             bbank30p
038100        MOVE CD03O-BAL5 TO BANK-SCR30-BAL5                        bbank30p
038200     ELSE                                                         bbank30p
038300        MOVE CD03O-BAL5N TO WS-EDIT-BALANCE                       bbank30p
038400        MOVE WS-EDIT-BALANCE TO BANK-SCR30-BAL5                   bbank30p
038500     END-IF.                                                      bbank30p
038600                                                                  bbank30p
038700     MOVE CD03O-BAL5 TO WS-SRV-BAL.                               bbank30p
038800     PERFORM CALC-SERVICE-CHARGE THRU                             bbank30p
038900             CALC-SERVICE-CHARGE-EXIT.                            bbank30p
039000     MOVE WS-SRV-CHARGE TO BANK-SCR30-SRV5.                       bbank30p
039100     IF CD03O-DTE5 IS EQUAL TO SPACES                             bbank30p
039200        MOVE CD03O-DTE5 TO BANK-SCR30-DTE5                        bbank30p
039300     ELSE                                                         bbank30p
039400        MOVE CD03O-DTE5 TO DDI-DATA                               bbank30p
039500        SET DDI-ISO TO TRUE                                       bbank30p
039600        SET DDO-DD-MMM-YYYY TO TRUE                               bbank30p
039700        PERFORM CALL-DATECONV THRU                                bbank30p
039800                CALL-DATECONV-EXIT                                bbank30p
039900        MOVE DDO-DATA TO BANK-SCR30-DTE5                          bbank30p
040000     END-IF.                                                      bbank30p
040100     MOVE CD03O-TXN5 TO BANK-SCR30-TXN5.                          bbank30p
040200                                                                  bbank30p
040300     MOVE CD03O-ACC6 TO BANK-SCR30-ACC6.                          bbank30p
040400     MOVE CD03O-DSC6 TO BANK-SCR30-DSC6.                          bbank30p
040500     IF CD03O-BAL6 IS EQUAL TO SPACES                             bbank30p
040600        MOVE CD03O-BAL6 TO BANK-SCR30-BAL6                        bbank30p
040700     ELSE                                                         bbank30p
040800        MOVE CD03O-BAL6N TO WS-EDIT-BALANCE                       bbank30p
040900        MOVE WS-EDIT-BALANCE TO BANK-SCR30-BAL6                   bbank30p
041000     END-IF.                                                      bbank30p
041100                                                                  bbank30p
041200     MOVE CD03O-BAL6 TO WS-SRV-BAL.                               bbank30p
041300     PERFORM CALC-SERVICE-CHARGE THRU                             bbank30p
041400             CALC-SERVICE-CHARGE-EXIT.                            bbank30p
041500     MOVE WS-SRV-CHARGE TO BANK-SCR30-SRV6.                       bbank30p
041600     IF CD03O-DTE6 IS EQUAL TO SPACES                             bbank30p
041700        MOVE CD03O-DTE6 TO BANK-SCR30-DTE6                        bbank30p
041800     ELSE                                                         bbank30p
041900        MOVE CD03O-DTE6 TO DDI-DATA                               bbank30p
042000        SET DDI-ISO TO TRUE                                       bbank30p
042100        SET DDO-DD-MMM-YYYY TO TRUE                               bbank30p
042200        PERFORM CALL-DATECONV THRU                                bbank30p
042300                CALL-DATECONV-EXIT                                bbank30p
042400        MOVE DDO-DATA TO BANK-SCR30-DTE6                          bbank30p
042500     END-IF.                                                      bbank30p
042600     MOVE CD03O-TXN6 TO BANK-SCR30-TXN6.                          bbank30p
042700                                                                  bbank30p
042800     IF BANK-SCR30-SRV1 IS NOT EQUAL TO SPACES OR                 bbank30p
042900        BANK-SCR30-SRV2 IS NOT EQUAL TO SPACES OR                 bbank30p
043000        BANK-SCR30-SRV3 IS NOT EQUAL TO SPACES OR                 bbank30p
043100        BANK-SCR30-SRV4 IS NOT EQUAL TO SPACES OR                 bbank30p
043200        BANK-SCR30-SRV5 IS NOT EQUAL TO SPACES OR                 bbank30p
043300        BANK-SCR30-SRV6 IS NOT EQUAL TO SPACES                    bbank30p
043400        MOVE WS-SRV-MSG TO BANK-SCR30-SRVMSG                      bbank30p
043500     ELSE                                                         bbank30p
043600        MOVE SPACES TO BANK-SCR30-SRVMSG                          bbank30p
043700     END-IF.                                                      bbank30p
043800                                                                  bbank30p
043900 POPULATE-SCREEN-DATA-EXIT.                                       bbank30p
044000     EXIT.                                                        bbank30p
044100                                                                  bbank30p
044200                                                                  bbank30p
044300***************************************************************** bbank30p
044400* Calculate any service charges based on provided balance       * bbank30p
044500***************************************************************** bbank30p
044600 CALC-SERVICE-CHARGE.                                             bbank30p
044700     IF WS-SRV-BAL IS EQUAL TO SPACES                             bbank30p
044800        MOVE 0 TO WS-SRV-AMT                                      bbank30p
044900        GO TO CALC-SERVICE-CHARGE-EDIT                            bbank30p
045000     END-IF.                                                      bbank30p
045100     IF WS-SRV-BAL-N IS GREATER THAN WS-SRV-BAL5                  bbank30p
045200        MOVE WS-SRV-CHG5 TO WS-SRV-AMT                            bbank30p
045300        GO TO CALC-SERVICE-CHARGE-EDIT                            bbank30p
045400     END-IF.                                                      bbank30p
045500     IF WS-SRV-BAL-N IS GREATER THAN WS-SRV-BAL4                  bbank30p
045600        MOVE WS-SRV-CHG4 TO WS-SRV-AMT                            bbank30p
045700        GO TO CALC-SERVICE-CHARGE-EDIT                            bbank30p
045800     END-IF.                                                      bbank30p
045900     IF WS-SRV-BAL-N IS GREATER THAN WS-SRV-BAL3                  bbank30p
046000        MOVE WS-SRV-CHG3 TO WS-SRV-AMT                            bbank30p
046100        GO TO CALC-SERVICE-CHARGE-EDIT                            bbank30p
046200     END-IF.                                                      bbank30p
046300     IF WS-SRV-BAL-N IS GREATER THAN WS-SRV-BAL2                  bbank30p
046400        MOVE WS-SRV-CHG2 TO WS-SRV-AMT                            bbank30p
046500        GO TO CALC-SERVICE-CHARGE-EDIT                            bbank30p
046600     END-IF.                                                      bbank30p
046700     IF WS-SRV-BAL-N IS GREATER THAN WS-SRV-BAL1                  bbank30p
046800        MOVE WS-SRV-CHG1 TO WS-SRV-AMT                            bbank30p
046900        GO TO CALC-SERVICE-CHARGE-EDIT                            bbank30p
047000     ELSE                                                         bbank30p
047100        MOVE WS-SRV-CHG0 TO WS-SRV-AMT                            bbank30p
047200        GO TO CALC-SERVICE-CHARGE-EDIT                            bbank30p
047300     END-IF.                                                      bbank30p
047400 CALC-SERVICE-CHARGE-EDIT.                                        bbank30p
047500     MOVE WS-SRV-AMT TO WS-SRV-CHARGE.                            bbank30p
047600 CALC-SERVICE-CHARGE-EXIT.                                        bbank30p
047700     EXIT.                                                        bbank30p
047800                                                                  bbank30p
047900***************************************************************** bbank30p
048000* Call common routine to perform date conversions               * bbank30p
048100***************************************************************** bbank30p
048200 CALL-DATECONV.                                                   bbank30p
048300     MOVE BANK-ENV TO DD-ENV.                                     bbank30p
048400     MOVE 'UDATECNV' TO WS-DYNAMIC-PGM.                           bbank30p
048500     CALL WS-DYNAMIC-PGM USING WS-TIME-DATE-WORK-AREA.            bbank30p
048600 CALL-DATECONV-EXIT.                                              bbank30p
048700     EXIT.                                                        bbank30p
048800                                                                  bbank30p
048900* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     bbank30p
