000100***************************************************************** bcash01p
000200*                                                               * bcash01p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * bcash01p
000400*   This demonstration program is provided for use by users     * bcash01p
000500*   of Micro Focus products and may be used, modified and       * bcash01p
000600*   distributed as part of your application provided that       * bcash01p
000700*   you properly acknowledge the copyright of Micro Focus       * bcash01p
000800*   in this material.                                           * bcash01p
000900*                                                               * bcash01p
001000***************************************************************** bcash01p
001100                                                                  bcash01p
001200***************************************************************** bcash01p
001300* Program:     BCASH01P.CBL                                     * bcash01p
001400* Layer:       Business logic                                   * bcash01p
001500* Function:    ATM - obtain list of enabled accounts            * bcash01p
001600***************************************************************** bcash01p
001700                                                                  bcash01p
001800 IDENTIFICATION DIVISION.                                         bcash01p
001900 PROGRAM-ID.                                                      bcash01p
002000     BCASH01P.                                                    bcash01p
002100 DATE-WRITTEN.                                                    bcash01p
002200     September 2002.                                              bcash01p
002300 DATE-COMPILED.                                                   bcash01p
002400     Today.                                                       bcash01p
002500                                                                  bcash01p
002600 ENVIRONMENT DIVISION.                                            bcash01p
002700                                                                  bcash01p
002800 DATA DIVISION.                                                   bcash01p
002900 WORKING-STORAGE SECTION.                                         bcash01p
003000 01  WS-MISC-STORAGE.                                             bcash01p
003100   05  WS-PROGRAM-ID                         PIC X(8)             bcash01p
003200       VALUE 'BCASH01P'.                                          bcash01p
003300   05  WS-COMMAREA-LENGTH                    PIC 9(5).            bcash01p
003400   05  WS-SUB                                PIC 9(3).            bcash01p
003500 01  WS-CASH-DATA.                                                bcash01p
003600 COPY CCASHDAT.                                                   bcash01p
003700                                                                  bcash01p
003800 01  WS-ACCOUNT-DATA.                                             bcash01p
003900 COPY CCASHD02.                                                   bcash01p
004000                                                                  bcash01p
004100 COPY CABENDD.                                                    bcash01p
004200                                                                  bcash01p
004300 LINKAGE SECTION.                                                 bcash01p
004400 01  DFHCOMMAREA.                                                 bcash01p
004500   05  LK-COMMAREA                           PIC X(6144).         bcash01p
004600                                                                  bcash01p
004700 COPY CENTRY.                                                     bcash01p
004800***************************************************************** bcash01p
004900* Make ourselves re-entrant                                     * bcash01p
005000***************************************************************** bcash01p
005100                                                                  bcash01p
005200***************************************************************** bcash01p
005300* Move the passed area to our area                              * bcash01p
005400***************************************************************** bcash01p
005500     MOVE DFHCOMMAREA (1:LENGTH OF WS-CASH-DATA) TO WS-CASH-DATA. bcash01p
005600                                                                  bcash01p
005700***************************************************************** bcash01p
005800* Ensure error message is cleared                               * bcash01p
005900***************************************************************** bcash01p
006000     MOVE SPACES TO CASH-ERROR-MSG.                               bcash01p
006100                                                                  bcash01p
006200***************************************************************** bcash01p
006300* This is the main process                                      * bcash01p
006400***************************************************************** bcash01p
006500     MOVE SPACES TO CD02-DATA.                                    bcash01p
006600     MOVE CASH-USERID TO CD02I-CONTACT-ID.                        bcash01p
006700* Now go get the data                                             bcash01p
006800 COPY CCASHX02.                                                   bcash01p
006900     MOVE 0 TO WS-SUB.                                            bcash01p
007000     PERFORM 5 TIMES                                              bcash01p
007100     ADD 1 TO WS-SUB                                              bcash01p
007200     MOVE CD02O-ACC-NO(WS-SUB)                                    bcash01p
007300       TO CASH-ATM1-ACC(WS-SUB)                                   bcash01p
007400     MOVE CD02O-ACC-DESC(WS-SUB)                                  bcash01p
007500       TO CASH-ATM1-DSC(WS-SUB)                                   bcash01p
007600     MOVE CD02O-ACC-BAL(WS-SUB)                                   bcash01p
007700       TO CASH-ATM1-BAL(WS-SUB)                                   bcash01p
007800     MOVE CD02O-ACC-DAY-LIMIT(WS-SUB)                             bcash01p
007900       TO CASH-ATM1-DAY-LIMIT(WS-SUB)                             bcash01p
008000     MOVE CD02O-ACC-DATE-USED(WS-SUB)                             bcash01p
008100       TO CASH-ATM1-DATE-USED(WS-SUB)                             bcash01p
008200     MOVE CD02O-ACC-DATE-AMT(WS-SUB)                              bcash01p
008300       TO CASH-ATM1-DATE-AMT(WS-SUB)                              bcash01p
008400     END-PERFORM.                                                 bcash01p
008500                                                                  bcash01p
008600 COMMON-RETURN.                                                   bcash01p
008700     MOVE WS-CASH-DATA TO DFHCOMMAREA (1:LENGTH OF WS-CASH-DATA). bcash01p
008800 COPY CRETURN.                                                    bcash01p
008900                                                                  bcash01p
009000* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     bcash01p
