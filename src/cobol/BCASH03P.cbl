000100***************************************************************** bcash03p
000200*                                                               * bcash03p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * bcash03p
000400*   This demonstration program is provided for use by users     * bcash03p
000500*   of Micro Focus products and may be used, modified and       * bcash03p
000600*   distributed as part of your application provided that       * bcash03p
000700*   you properly acknowledge the copyright of Micro Focus       * bcash03p
000800*   in this material.                                           * bcash03p
000900*                                                               * bcash03p
001000***************************************************************** bcash03p
001100                                                                  bcash03p
001200***************************************************************** bcash03p
001300* Program:     BCASH03P.CBL                                     * bcash03p
001400* Layer:       Business logic                                   * bcash03p
001500* Function:    ATM - cash withdrawal. This is the same as funds * bcash03p
001600*              transfer but to a different person (the bank)    * bcash03p
001700***************************************************************** bcash03p
001800                                                                  bcash03p
001900 IDENTIFICATION DIVISION.                                         bcash03p
002000 PROGRAM-ID.                                                      bcash03p
002100     BCASH03P.                                                    bcash03p
002200 DATE-WRITTEN.                                                    bcash03p
002300     September 2002.                                              bcash03p
002400 DATE-COMPILED.                                                   bcash03p
002500     Today.                                                       bcash03p
002600                                                                  bcash03p
002700 ENVIRONMENT DIVISION.                                            bcash03p
002800                                                                  bcash03p
002900 DATA DIVISION.                                                   bcash03p
003000 WORKING-STORAGE SECTION.                                         bcash03p
003100 01  WS-MISC-STORAGE.                                             bcash03p
003200   05  WS-PROGRAM-ID                         PIC X(8)             bcash03p
003300       VALUE 'BCASH03P'.                                          bcash03p
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            bcash03p
003500   05  WS-SUB                                PIC 9(3).            bcash03p
003600 01  WS-CASH-DATA.                                                bcash03p
003700 COPY CCASHDAT.                                                   bcash03p
003800                                                                  bcash03p
003900 01  WS-BANK-DATA.                                                bcash03p
004000 COPY CBANKDAT.                                                   bcash03p
004100*                                                                 bcash03p
004200*01  WS-ACCOUNT-DATA.                                             bcash03p
004300*COPY CCASHD02.                                                   bcash03p
004400                                                                  bcash03p
004500 COPY CABENDD.                                                    bcash03p
004600                                                                  bcash03p
004700 LINKAGE SECTION.                                                 bcash03p
004800 01  DFHCOMMAREA.                                                 bcash03p
004900   05  LK-COMMAREA                           PIC X(6144).         bcash03p
005000                                                                  bcash03p
005100 COPY CENTRY.                                                     bcash03p
005200***************************************************************** bcash03p
005300* Make ourselves re-entrant                                     * bcash03p
005400***************************************************************** bcash03p
005500                                                                  bcash03p
005600***************************************************************** bcash03p
005700* Move the passed area to our area                              * bcash03p
005800***************************************************************** bcash03p
005900     MOVE DFHCOMMAREA (1:LENGTH OF WS-CASH-DATA) TO WS-CASH-DATA. bcash03p
006000                                                                  bcash03p
006100***************************************************************** bcash03p
006200* Ensure error message is cleared                               * bcash03p
006300***************************************************************** bcash03p
006400     MOVE SPACES TO CASH-ERROR-MSG.                               bcash03p
006500                                                                  bcash03p
006600***************************************************************** bcash03p
006700* This is the main process                                      * bcash03p
006800***************************************************************** bcash03p
006900     MOVE LOW-VALUES TO WS-BANK-DATA.                             bcash03p
007000     SET BANK-AID-ENTER TO TRUE.                                  bcash03p
007100     MOVE 'MBANK50' TO BANK-LAST-MAPSET.                          bcash03p
007200     MOVE CASH-USERID TO BANK-USERID.                             bcash03p
007300     MOVE CASH-ATM3-CASH-AMT TO BANK-SCR50-XFER                   bcash03p
007400     MOVE 'X' TO BANK-SCR50-FRM1.                                 bcash03p
007500     MOVE CASH-ATM3-FROM-ACC TO BANK-SCR50-ACC1.                  bcash03p
007600     MOVE CASH-ATM3-FROM-BAL TO BANK-SCR50-BAL1.                  bcash03p
007700     MOVE 'X' TO BANK-SCR50-TO2.                                  bcash03p
007800     MOVE '999999996' TO BANK-SCR50-ACC2.                         bcash03p
007900     MOVE '0,000,000,00 ' TO BANK-SCR50-BAL2.                     bcash03p
008000                                                                  bcash03p
008100     EXEC CICS LINK PROGRAM('BBANK50P')                           bcash03p
008200                    COMMAREA(WS-BANK-DATA)                        bcash03p
008300                    LENGTH(LENGTH OF WS-BANK-DATA)                bcash03p
008400     END-EXEC.                                                    bcash03p
008500                                                                  bcash03p
008600     MOVE BANK-ERROR-MSG TO CASH-ERROR-MSG.                       bcash03p
008700                                                                  bcash03p
008800 COMMON-RETURN.                                                   bcash03p
008900     MOVE WS-CASH-DATA TO DFHCOMMAREA (1:LENGTH OF WS-CASH-DATA). bcash03p
009000 COPY CRETURN.                                                    bcash03p
009100                                                                  bcash03p
009200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     bcash03p
