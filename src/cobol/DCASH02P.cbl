000100***************************************************************** dcash02p
000200*                                                               * dcash02p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * dcash02p
000400*   This demonstration program is provided for use by users     * dcash02p
000500*   of Micro Focus products and may be used, modified and       * dcash02p
000600*   distributed as part of your application provided that       * dcash02p
000700*   you properly acknowledge the copyright of Micro Focus       * dcash02p
000800*   in this material.                                           * dcash02p
000900*                                                               * dcash02p
001000***************************************************************** dcash02p
001100                                                                  dcash02p
001200***************************************************************** dcash02p
001300* Program:     DCASH02P.CBL                                     * dcash02p
001400* Function:    Obtain ATM enabled account details               * dcash02p
001500*              VSAM version                                     * dcash02p
001600***************************************************************** dcash02p
001700                                                                  dcash02p
001800 IDENTIFICATION DIVISION.                                         dcash02p
001900 PROGRAM-ID.                                                      dcash02p
002000     DCASH02P.                                                    dcash02p
002100 DATE-WRITTEN.                                                    dcash02p
002200     September 2002.                                              dcash02p
002300 DATE-COMPILED.                                                   dcash02p
002400     Today.                                                       dcash02p
002500                                                                  dcash02p
002600 ENVIRONMENT DIVISION.                                            dcash02p
002700                                                                  dcash02p
002800 DATA DIVISION.                                                   dcash02p
002900                                                                  dcash02p
003000 WORKING-STORAGE SECTION.                                         dcash02p
003100 01  WS-MISC-STORAGE.                                             dcash02p
003200   05  WS-PROGRAM-ID                         PIC X(8)             dcash02p
003300       VALUE 'DCASH02P'.                                          dcash02p
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            dcash02p
003500   05  WS-RESP                               PIC S9(8) COMP.      dcash02p
003600   05  WS-SUB1                               PIC S9(4) COMP.      dcash02p
003700   05  WS-BNKACCT-AIX1-RID                   PIC X(5).            dcash02p
003800   05  WS-BNKATYP-RID                        PIC X(1).            dcash02p
003900   05  WS-BNKTXN-AIX1-RID                    PIC X(31).           dcash02p
004000   05  WS-BNKTXN-AIX1-RID-LEN                PIC X(31).           dcash02p
004100   05  WS-ACC-BALANCE                        PIC Z,ZZZ,ZZ9.99-.   dcash02p
004200   05  WS-ACC-BALANCE-X REDEFINES WS-ACC-BALANCE                  dcash02p
004300                                             PIC X(13).           dcash02p
004400   05  WS-ATM-DAY-LIMIT-N                    PIC 9(3).            dcash02p
004500   05  WS-ATM-DAY-AMT-N                      PIC 9(3).            dcash02p
004600                                                                  dcash02p
004700 01  WS-BNKACCT-REC.                                              dcash02p
004800 COPY CBANKVAC.                                                   dcash02p
004900                                                                  dcash02p
005000 01  WS-BNKATYP-REC.                                              dcash02p
005100 COPY CBANKVAT.                                                   dcash02p
005200                                                                  dcash02p
005300 01  WS-COMMAREA.                                                 dcash02p
005400 COPY CCASHD02.                                                   dcash02p
005500                                                                  dcash02p
005600 COPY CABENDD.                                                    dcash02p
005700                                                                  dcash02p
005800 LINKAGE SECTION.                                                 dcash02p
005900 01  DFHCOMMAREA.                                                 dcash02p
006000   05  LK-COMMAREA                           PIC X(1)             dcash02p
006100       OCCURS 1 TO 4096 TIMES                                     dcash02p
006200         DEPENDING ON WS-COMMAREA-LENGTH.                         dcash02p
006300                                                                  dcash02p
006400 COPY CENTRY.                                                     dcash02p
006500***************************************************************** dcash02p
006600* Move the passed data to our area                              * dcash02p
006700***************************************************************** dcash02p
006800     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            dcash02p
006900     MOVE DFHCOMMAREA TO WS-COMMAREA.                             dcash02p
007000                                                                  dcash02p
007100***************************************************************** dcash02p
007200* Initialize our output area                                    * dcash02p
007300***************************************************************** dcash02p
007400     MOVE SPACES TO CD02O-DATA.                                   dcash02p
007500                                                                  dcash02p
007600***************************************************************** dcash02p
007700* Set up the start position for the browse                      * dcash02p
007800***************************************************************** dcash02p
007900     MOVE CD02I-CONTACT-ID TO WS-BNKACCT-AIX1-RID.                dcash02p
008000                                                                  dcash02p
008100***************************************************************** dcash02p
008200* Start browsing the file                                       * dcash02p
008300***************************************************************** dcash02p
008400     EXEC CICS STARTBR FILE('BNKACC1')                            dcash02p
008500                       RIDFLD(WS-BNKACCT-AIX1-RID)                dcash02p
008600                       GTEQ                                       dcash02p
008700     END-EXEC.                                                    dcash02p
008800                                                                  dcash02p
008900***************************************************************** dcash02p
009000* Now browse the selected recs and move up to 5 into our area   * dcash02p
009100***************************************************************** dcash02p
009200     MOVE 0 TO WS-SUB1.                                           dcash02p
009300 ACCOUNT-FETCH-LOOP.                                              dcash02p
009400     ADD 1 TO WS-SUB1.                                            dcash02p
009500     IF WS-SUB1 IS GREATER THAN 5                                 dcash02p
009600        GO TO ACCOUNT-FETCH-LOOP-EXIT                             dcash02p
009700     END-IF.                                                      dcash02p
009800     MOVE SPACES TO WS-BNKACCT-REC.                               dcash02p
009900     EXEC CICS READNEXT FILE('BNKACC1')                           dcash02p
010000                        INTO(WS-BNKACCT-REC)                      dcash02p
010100                        LENGTH(LENGTH OF WS-BNKACCT-REC)          dcash02p
010200                        RIDFLD(WS-BNKACCT-AIX1-RID)               dcash02p
010300                        RESP(WS-RESP)                             dcash02p
010400     END-EXEC.                                                    dcash02p
010500     IF (WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL) AND              dcash02p
010600         WS-RESP IS NOT EQUAL TO DFHRESP(DUPKEY)) OR              dcash02p
010700        BAC-REC-PID IS NOT EQUAL TO CD02I-CONTACT-ID OR           dcash02p
010800        BAC-REC-ATM-ENABLED IS NOT EQUAL TO 'Y'                   dcash02p
010900        GO TO ACCOUNT-FETCH-LOOP-EXIT                             dcash02p
011000     END-IF.                                                      dcash02p
011100                                                                  dcash02p
011200***************************************************************** dcash02p
011300* We got an account record ok, save no & bal, get description   * dcash02p
011400***************************************************************** dcash02p
011500     MOVE BAC-REC-ACCNO TO CD02O-ACC-NO (WS-SUB1).                dcash02p
011600     MOVE BAC-REC-TYPE TO WS-BNKATYP-RID.                         dcash02p
011700     EXEC CICS READ FILE('BNKATYPE')                              dcash02p
011800                        INTO(WS-BNKATYP-REC)                      dcash02p
011900                        LENGTH(LENGTH OF WS-BNKATYP-REC)          dcash02p
012000                        RIDFLD(WS-BNKATYP-RID)                    dcash02p
012100                        RESP(WS-RESP)                             dcash02p
012200     END-EXEC.                                                    dcash02p
012300     IF WS-RESP IS EQUAL TO DFHRESP(NORMAL)                       dcash02p
012400        MOVE BAT-REC-DESC TO CD02O-ACC-DESC (WS-SUB1)             dcash02p
012500     ELSE                                                         dcash02p
012600        MOVE 'Unkown' TO CD02O-ACC-DESC (WS-SUB1)                 dcash02p
012700     END-IF.                                                      dcash02p
012800     MOVE BAC-REC-BALANCE TO WS-ACC-BALANCE.                      dcash02p
012900     MOVE WS-ACC-BALANCE-X TO CD02O-ACC-BAL (WS-SUB1).            dcash02p
013000     MOVE BAC-REC-ATM-DAY-LIMIT TO WS-ATM-DAY-LIMIT-N.            dcash02p
013100     MOVE WS-ATM-DAY-LIMIT-N TO CD02O-ACC-DAY-LIMIT (WS-SUB1).    dcash02p
013200     MOVE BAC-REC-ATM-DAY-DTE TO CD02O-ACC-DATE-USED (WS-SUB1).   dcash02p
013300     MOVE BAC-REC-ATM-DAY-AMT TO WS-ATM-DAY-AMT-N.                dcash02p
013400     MOVE WS-ATM-DAY-AMT-N TO CD02O-ACC-DATE-AMT (WS-SUB1).       dcash02p
013500                                                                  dcash02p
013600     GO TO ACCOUNT-FETCH-LOOP.                                    dcash02p
013700                                                                  dcash02p
013800***************************************************************** dcash02p
013900* We quit the loop for some reason                              * dcash02p
014000***************************************************************** dcash02p
014100 ACCOUNT-FETCH-LOOP-EXIT.                                         dcash02p
014200     EXEC CICS ENDBR FILE('BNKACC1')                              dcash02p
014300     END-EXEC.                                                    dcash02p
014400                                                                  dcash02p
014500***************************************************************** dcash02p
014600* Move the result back to the callers area                      * dcash02p
014700***************************************************************** dcash02p
014800     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       dcash02p
014900                                                                  dcash02p
015000***************************************************************** dcash02p
015100* Return to our caller                                          * dcash02p
015200***************************************************************** dcash02p
015300 COPY CRETURN.                                                    dcash02p
015400                                                                  dcash02p
015500* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     dcash02p
