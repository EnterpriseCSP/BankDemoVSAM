000100***************************************************************** bbank99p
000200*                                                               * bbank99p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * bbank99p
000400*   This demonstration program is provided for use by users     * bbank99p
000500*   of Micro Focus products and may be used, modified and       * bbank99p
000600*   distributed as part of your application provided that       * bbank99p
000700*   you properly acknowledge the copyright of Micro Focus       * bbank99p
000800*   in this material.                                           * bbank99p
000900*                                                               * bbank99p
001000***************************************************************** bbank99p
001100                                                                  bbank99p
001200***************************************************************** bbank99p
001300* Program:     BBANK99P.CBL                                     * bbank99p
001400* Layer:       Business logic                                   * bbank99p
001500* Function:    Terminate the pseudo converation                 * bbank99p
001600***************************************************************** bbank99p
001700                                                                  bbank99p
001800 IDENTIFICATION DIVISION.                                         bbank99p
001900 PROGRAM-ID.                                                      bbank99p
002000     BBANK99P.                                                    bbank99p
002100 DATE-WRITTEN.                                                    bbank99p
002200     September 2002.                                              bbank99p
002300 DATE-COMPILED.                                                   bbank99p
002400     Today.                                                       bbank99p
002500                                                                  bbank99p
002600 ENVIRONMENT DIVISION.                                            bbank99p
002700                                                                  bbank99p
002800 DATA DIVISION.                                                   bbank99p
002900 WORKING-STORAGE SECTION.                                         bbank99p
003000 01  WS-MISC-STORAGE.                                             bbank99p
003100   05  WS-PROGRAM-ID                         PIC X(8)             bbank99p
003200       VALUE 'BBANK99P'.                                          bbank99p
003300   05  WS-INPUT-FLAG                         PIC X(1).            bbank99p
003400     88  INPUT-OK                            VALUE '0'.           bbank99p
003500     88  INPUT-ERROR                         VALUE '1'.           bbank99p
003600   05  WS-ERROR-MSG                          PIC X(75).           bbank99p
003700                                                                  bbank99p
003800 01  WS-BANK-DATA.                                                bbank99p
003900 COPY CBANKDAT.                                                   bbank99p
004000                                                                  bbank99p
004100 01  WS-SECURITY.                                                 bbank99p
004200 COPY CPSWDD01.                                                   bbank99p
004300                                                                  bbank99p
004400 COPY CABENDD.                                                    bbank99p
004500                                                                  bbank99p
004600 LINKAGE SECTION.                                                 bbank99p
004700 01  DFHCOMMAREA.                                                 bbank99p
004800   05  LK-COMMAREA                           PIC X(6144).         bbank99p
004900                                                                  bbank99p
005000 COPY CENTRY.                                                     bbank99p
005100                                                                  bbank99p
005200***************************************************************** bbank99p
005300* Make ourselves re-entrant                                     * bbank99p
005400***************************************************************** bbank99p
005500     MOVE SPACES TO WS-ERROR-MSG.                                 bbank99p
005600                                                                  bbank99p
005700***************************************************************** bbank99p
005800* Move the passed area to our area                              * bbank99p
005900***************************************************************** bbank99p
006000     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. bbank99p
006100                                                                  bbank99p
006200***************************************************************** bbank99p
006300* Ensure error message is cleared                               * bbank99p
006400***************************************************************** bbank99p
006500     MOVE SPACES TO BANK-ERROR-MSG.                               bbank99p
006600                                                                  bbank99p
006700***************************************************************** bbank99p
006800* This is the main process                                      * bbank99p
006900***************************************************************** bbank99p
007000* We now make sure the user is logged off.......                  bbank99p
007100     MOVE SPACES TO CPSWDD01-DATA.                                bbank99p
007200     MOVE BANK-SIGNON-ID TO CPSWDD01I-USERID.                     bbank99p
007300     MOVE BANK-PSWD TO CPSWDD01I-PASSWORD                         bbank99p
007400* If user starts with "Z" then treat as "B"                       bbank99p
007500     IF CPSWDD01I-USERID(1:1) IS EQUAL TO 'Z'                     bbank99p
007600        MOVE 'B' TO  CPSWDD01I-USERID(1:1)                        bbank99p
007700     END-IF.                                                      bbank99p
007800                                                                  bbank99p
007900     SET PSWD-SIGNOFF TO TRUE                                     bbank99p
008000                                                                  bbank99p
008100 COPY CPSWDX01.                                                   bbank99p
008200     IF CPSWDD01O-MESSAGE IS NOT EQUAL TO SPACES                  bbank99p
008300        MOVE CPSWDD01O-MESSAGE TO WS-ERROR-MSG                    bbank99p
008400     END-IF.                                                      bbank99p
008500                                                                  bbank99p
008600     MOVE SPACES TO BANK-IMS-SPA-TRANCODE.                        bbank99p
008700     MOVE 'BBANK99P' TO BANK-LAST-PROG                            bbank99p
008800     MOVE 'BBANK99P' TO BANK-NEXT-PROG                            bbank99p
008900     MOVE 'MBANK99' TO BANK-LAST-MAPSET                           bbank99p
009000     MOVE 'BANK99A' TO BANK-LAST-MAP                              bbank99p
009100     MOVE 'MBANK99' TO BANK-NEXT-MAPSET                           bbank99p
009200     MOVE 'BANK99A' TO BANK-NEXT-MAP                              bbank99p
009300     GO TO COMMON-RETURN.                                         bbank99p
009400                                                                  bbank99p
009500 COMMON-RETURN.                                                   bbank99p
009600     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). bbank99p
009700 COPY CRETURN.                                                    bbank99p
009800                                                                  bbank99p
009900* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     bbank99p
