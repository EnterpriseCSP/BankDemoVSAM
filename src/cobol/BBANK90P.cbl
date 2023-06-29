000100***************************************************************** bbank90p
000200*                                                               * bbank90p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * bbank90p
000400*   This demonstration program is provided for use by users     * bbank90p
000500*   of Micro Focus products and may be used, modified and       * bbank90p
000600*   distributed as part of your application provided that       * bbank90p
000700*   you properly acknowledge the copyright of Micro Focus       * bbank90p
000800*   in this material.                                           * bbank90p
000900*                                                               * bbank90p
001000***************************************************************** bbank90p
001100                                                                  bbank90p
001200***************************************************************** bbank90p
001300* Program:     BBANK90P.CBL                                     * bbank90p
001400* Layer:       Business logic                                   * bbank90p
001500* Function:    Obtain data for "more information"               * bbank90p
001600***************************************************************** bbank90p
001700                                                                  bbank90p
001800 IDENTIFICATION DIVISION.                                         bbank90p
001900 PROGRAM-ID.                                                      bbank90p
002000     BBANK90P.                                                    bbank90p
002100 DATE-WRITTEN.                                                    bbank90p
002200     September 2002.                                              bbank90p
002300 DATE-COMPILED.                                                   bbank90p
002400     Today.                                                       bbank90p
002500                                                                  bbank90p
002600 ENVIRONMENT DIVISION.                                            bbank90p
002700                                                                  bbank90p
002800 DATA DIVISION.                                                   bbank90p
002900 WORKING-STORAGE SECTION.                                         bbank90p
003000 01  WS-MISC-STORAGE.                                             bbank90p
003100   05  WS-PROGRAM-ID                         PIC X(8)             bbank90p
003200       VALUE 'BBANK90P'.                                          bbank90p
003300   05  WS-INPUT-FLAG                         PIC X(1).            bbank90p
003400     88  INPUT-OK                            VALUE '0'.           bbank90p
003500     88  INPUT-ERROR                         VALUE '1'.           bbank90p
003600   05  WS-RETURN-FLAG                        PIC X(1).            bbank90p
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    bbank90p
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           bbank90p
003900   05  WS-RETURN-MSG                         PIC X(75).           bbank90p
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        bbank90p
004100   05  WS-PFK-FLAG                           PIC X(1).            bbank90p
004200     88  PFK-VALID                           VALUE '0'.           bbank90p
004300     88  PFK-INVALID                         VALUE '1'.           bbank90p
004400   05  WS-ERROR-MSG                          PIC X(75).           bbank90p
004500   05  WS-SUB1                               PIC S9(4) COMP.      bbank90p
004600   05  WS-SUB1-LIMIT                         PIC S9(4) COMP.      bbank90p
004700                                                                  bbank90p
004800 01  WS-INF-DATA.                                                 bbank90p
004900   05  WS-INF-DATA01.                                             bbank90p
005000     10  FILLER                              PIC X(40)            bbank90p
005100         VALUE 'Sorry. The information you requested is '.        bbank90p
005200     10  FILLER                              PIC X(35)            bbank90p
005300         VALUE 'not available at this time.        '.             bbank90p
005400   05  WS-INF-DATA03.                                             bbank90p
005500     10  FILLER                              PIC X(40)            bbank90p
005600         VALUE 'Please try our web site at:             '.        bbank90p
005700     10  FILLER                              PIC X(35)            bbank90p
005800         VALUE '                                   '.             bbank90p
005900   05  WS-INF-DATA05.                                             bbank90p
006000     10  FILLER                              PIC X(40)            bbank90p
006100         VALUE '     http://www.microfocus.com          '.        bbank90p
006200     10  FILLER                              PIC X(35)            bbank90p
006300         VALUE '                                   '.             bbank90p
006400   05  WS-INF-DATA07.                                             bbank90p
006500     10  FILLER                              PIC X(40)            bbank90p
006600         VALUE 'or call our office at 1-800-VS-COBOL    '.        bbank90p
006700     10  FILLER                              PIC X(35)            bbank90p
006800         VALUE '                                   '.             bbank90p
006900   05  WS-INF-DATA08.                                             bbank90p
007000     10  FILLER                              PIC X(40)            bbank90p
007100         VALUE '                     (1-800-872-6265)   '.        bbank90p
007200     10  FILLER                              PIC X(35)            bbank90p
007300         VALUE '                                   '.             bbank90p
007400   05  WS-INF-DATA10.                                             bbank90p
007500     10  FILLER                              PIC X(40)            bbank90p
007600         VALUE 'Thank you for your interest.            '.        bbank90p
007700     10  FILLER                              PIC X(35)            bbank90p
007800         VALUE '                                   '.             bbank90p
007900                                                                  bbank90p
008000 01  WS-BANK-DATA.                                                bbank90p
008100 COPY CBANKDAT.                                                   bbank90p
008200                                                                  bbank90p
008300 01  WS-HELP-DATA.                                                bbank90p
008400 COPY CHELPD01.                                                   bbank90p
008500                                                                  bbank90p
008600 COPY CABENDD.                                                    bbank90p
008700                                                                  bbank90p
008800 LINKAGE SECTION.                                                 bbank90p
008900 01  DFHCOMMAREA.                                                 bbank90p
009000   05  LK-COMMAREA                           PIC X(6144).         bbank90p
009100                                                                  bbank90p
009200 COPY CENTRY.                                                     bbank90p
009300***************************************************************** bbank90p
009400* Make ourselves re-entrant                                     * bbank90p
009500***************************************************************** bbank90p
009600     MOVE SPACES TO WS-ERROR-MSG.                                 bbank90p
009700                                                                  bbank90p
009800***************************************************************** bbank90p
009900* Move the passed area to our area                              * bbank90p
010000***************************************************************** bbank90p
010100     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. bbank90p
010200                                                                  bbank90p
010300***************************************************************** bbank90p
010400* Ensure error message is cleared                               * bbank90p
010500***************************************************************** bbank90p
010600     MOVE SPACES TO BANK-ERROR-MSG.                               bbank90p
010700                                                                  bbank90p
010800***************************************************************** bbank90p
010900* This is the main process                                      * bbank90p
011000***************************************************************** bbank90p
011100                                                                  bbank90p
011200***************************************************************** bbank90p
011300* Save the passed return message and then turn it off           * bbank90p
011400***************************************************************** bbank90p
011500     MOVE BANK-RETURN-MSG TO WS-RETURN-MSG.                       bbank90p
011600     SET BANK-RETURN-MSG-OFF TO TRUE.                             bbank90p
011700                                                                  bbank90p
011800     MOVE WS-RETURN-MSG TO WS-ERROR-MSG.                          bbank90p
011900                                                                  bbank90p
012000***************************************************************** bbank90p
012100* Check the AID to see if its valid at this point               * bbank90p
012200***************************************************************** bbank90p
012300     SET PFK-INVALID TO TRUE.                                     bbank90p
012400     IF BANK-AID-ENTER OR                                         bbank90p
012500        BANK-AID-PFK03 OR                                         bbank90p
012600        BANK-AID-PFK04                                            bbank90p
012700        SET PFK-VALID TO TRUE                                     bbank90p
012800     END-IF.                                                      bbank90p
012900     IF BANK-AID-PFK01 AND                                        bbank90p
013000        BANK-HELP-INACTIVE                                        bbank90p
013100        SET BANK-HELP-ACTIVE TO TRUE                              bbank90p
013200        SET PFK-VALID TO TRUE                                     bbank90p
013300     END-IF.                                                      bbank90p
013400     IF PFK-INVALID                                               bbank90p
013500        SET BANK-AID-ENTER TO TRUE                                bbank90p
013600     END-IF.                                                      bbank90p
013700                                                                  bbank90p
013800***************************************************************** bbank90p
013900* Check the AID to see if we have to quit                       * bbank90p
014000***************************************************************** bbank90p
014100     IF BANK-AID-PFK03                                            bbank90p
014200        MOVE 'BBANK90P' TO BANK-LAST-PROG                         bbank90p
014300        MOVE 'BBANK99P' TO BANK-NEXT-PROG                         bbank90p
014400        MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        bbank90p
014500        MOVE 'BANK99A' TO BANK-NEXT-MAP                           bbank90p
014600        GO TO COMMON-RETURN                                       bbank90p
014700     END-IF.                                                      bbank90p
014800                                                                  bbank90p
014900***************************************************************** bbank90p
015000* Check the to see if user needs or has been using help         * bbank90p
015100***************************************************************** bbank90p
015200     IF BANK-HELP-ACTIVE                                          bbank90p
015300        IF BANK-AID-PFK04                                         bbank90p
015400           SET BANK-HELP-INACTIVE TO TRUE                         bbank90p
015500           MOVE 00 TO BANK-HELP-SCREEN                            bbank90p
015600           MOVE 'BBANK90P' TO BANK-LAST-PROG                      bbank90p
015700           MOVE 'BBANK90P' TO BANK-NEXT-PROG                      bbank90p
015800           MOVE 'MBANK90' TO BANK-LAST-MAPSET                     bbank90p
015900           MOVE 'HELP90A' TO BANK-LAST-MAP                        bbank90p
016000           MOVE 'MBANK90' TO BANK-NEXT-MAPSET                     bbank90p
016100           MOVE 'BANK90A' TO BANK-NEXT-MAP                        bbank90p
016200           PERFORM POPULATE-SCR90-DATA THRU                       bbank90p
016300                   POPULATE-SCR90-DATA-EXIT                       bbank90p
016400           GO TO COMMON-RETURN                                    bbank90p
016500        ELSE                                                      bbank90p
016600           MOVE 01 TO BANK-HELP-SCREEN                            bbank90p
016700           MOVE 'BBANK90P' TO BANK-LAST-PROG                      bbank90p
016800           MOVE 'BBANK90P' TO BANK-NEXT-PROG                      bbank90p
016900           MOVE 'MBANK90' TO BANK-LAST-MAPSET                     bbank90p
017000           MOVE 'BANK90A' TO BANK-LAST-MAP                        bbank90p
017100           MOVE 'MBANK90' TO BANK-NEXT-MAPSET                     bbank90p
017200           MOVE 'HELP90A' TO BANK-NEXT-MAP                        bbank90p
017300           MOVE 'BANK90' TO HELP01I-SCRN                          bbank90p
017400           COPY CHELPX01.                                         bbank90p
017500           MOVE HELP01O-DATA TO BANK-HELP-DATA                    bbank90p
017600           GO TO COMMON-RETURN                                    bbank90p
017700     END-IF.                                                      bbank90p
017800                                                                  bbank90p
017900***************************************************************** bbank90p
018000* Check the AID to see if we have to return to previous screen  * bbank90p
018100***************************************************************** bbank90p
018200     IF BANK-AID-PFK04                                            bbank90p
018300        MOVE 'BBANK90P' TO BANK-LAST-PROG                         bbank90p
018400        MOVE 'BBANK20P' TO BANK-NEXT-PROG                         bbank90p
018500        MOVE 'MBANK20' TO BANK-NEXT-MAPSET                        bbank90p
018600        MOVE 'BANK20A' TO BANK-NEXT-MAP                           bbank90p
018700        SET BANK-AID-ENTER TO TRUE                                bbank90p
018800        SET BANK-NO-CONV-IN-PROGRESS TO TRUE                      bbank90p
018900        GO TO COMMON-RETURN                                       bbank90p
019000     END-IF.                                                      bbank90p
019100                                                                  bbank90p
019200* Check if we have set the screen up before or is this 1st time   bbank90p
019300     IF BANK-LAST-MAPSET IS NOT EQUAL TO 'MBANK90'                bbank90p
019400        MOVE WS-RETURN-MSG TO BANK-ERROR-MSG                      bbank90p
019500        MOVE 'BBANK90P' TO BANK-LAST-PROG                         bbank90p
019600        MOVE 'BBANK90P' TO BANK-NEXT-PROG                         bbank90p
019700        MOVE 'MBANK90' TO BANK-LAST-MAPSET                        bbank90p
019800        MOVE 'BANK90A' TO BANK-LAST-MAP                           bbank90p
019900        MOVE 'MBANK90' TO BANK-NEXT-MAPSET                        bbank90p
020000        MOVE 'BANK90A' TO BANK-NEXT-MAP                           bbank90p
020100        PERFORM POPULATE-SCR90-DATA THRU                          bbank90p
020200                POPULATE-SCR90-DATA-EXIT                          bbank90p
020300        GO TO COMMON-RETURN                                       bbank90p
020400     END-IF.                                                      bbank90p
020500                                                                  bbank90p
020600     PERFORM VALIDATE-DATA THRU                                   bbank90p
020700             VALIDATE-DATA-EXIT.                                  bbank90p
020800                                                                  bbank90p
020900* If we had an error display error and return                     bbank90p
021000     IF INPUT-ERROR                                               bbank90p
021100        MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                       bbank90p
021200        MOVE 'BBANK90P' TO BANK-LAST-PROG                         bbank90p
021300        MOVE 'BBANK90P' TO BANK-NEXT-PROG                         bbank90p
021400        MOVE 'MBANK90' TO BANK-LAST-MAPSET                        bbank90p
021500        MOVE 'BANK90A' TO BANK-LAST-MAP                           bbank90p
021600        MOVE 'MBANK90' TO BANK-NEXT-MAPSET                        bbank90p
021700        MOVE 'BANK90A' TO BANK-NEXT-MAP                           bbank90p
021800        GO TO COMMON-RETURN                                       bbank90p
021900     END-IF.                                                      bbank90p
022000                                                                  bbank90p
022100     PERFORM POPULATE-SCR90-DATA THRU                             bbank90p
022200             POPULATE-SCR90-DATA-EXIT.                            bbank90p
022300     GO TO COMMON-RETURN.                                         bbank90p
022400                                                                  bbank90p
022500 COMMON-RETURN.                                                   bbank90p
022600     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). bbank90p
022700 COPY CRETURN.                                                    bbank90p
022800                                                                  bbank90p
022900 VALIDATE-DATA.                                                   bbank90p
023000     SET INPUT-OK TO TRUE.                                        bbank90p
023100                                                                  bbank90p
023200     GO TO VALIDATE-DATA-EXIT.                                    bbank90p
023300 VALIDATE-DATA-ERROR.                                             bbank90p
023400     SET INPUT-ERROR TO TRUE.                                     bbank90p
023500 VALIDATE-DATA-EXIT.                                              bbank90p
023600     EXIT.                                                        bbank90p
023700                                                                  bbank90p
023800 POPULATE-SCR90-DATA.                                             bbank90p
023900     MOVE 'INFO90' TO HELP01I-SCRN.                               bbank90p
024000     COPY CHELPX01.                                               bbank90p
024100     IF HELP-NOT-FOUND                                            bbank90p
024200        MOVE SPACES TO HELP01O-INDIVIDUAL-LINES                   bbank90p
024300        MOVE WS-INF-DATA01 TO HELP01O-L01                         bbank90p
024400        MOVE WS-INF-DATA03 TO HELP01O-L03                         bbank90p
024500        MOVE WS-INF-DATA05 TO HELP01O-L05                         bbank90p
024600        MOVE WS-INF-DATA07 TO HELP01O-L07                         bbank90p
024700        MOVE WS-INF-DATA08 TO HELP01O-L08                         bbank90p
024800        MOVE WS-INF-DATA10 TO HELP01O-L10                         bbank90p
024900     END-IF.                                                      bbank90p
025000     MOVE HELP01O-DATA TO BANK-SCREEN90-DATA.                     bbank90p
025100 POPULATE-SCR90-DATA-EXIT.                                        bbank90p
025200     EXIT.                                                        bbank90p
025300                                                                  bbank90p
025400* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     bbank90p
