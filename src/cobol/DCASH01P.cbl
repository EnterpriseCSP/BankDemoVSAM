000100***************************************************************** dcash01p
000200*                                                               * dcash01p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * dcash01p
000400*   This demonstration program is provided for use by users     * dcash01p
000500*   of Micro Focus products and may be used, modified and       * dcash01p
000600*   distributed as part of your application provided that       * dcash01p
000700*   you properly acknowledge the copyright of Micro Focus       * dcash01p
000800*   in this material.                                           * dcash01p
000900*                                                               * dcash01p
001000***************************************************************** dcash01p
001100                                                                  dcash01p
001200***************************************************************** dcash01p
001300* Program:     DCASH01P.CBL                                     * dcash01p
001400* Function:    Obtain users PIN                                 * dcash01p
001500*              VSAM version                                     * dcash01p
001600***************************************************************** dcash01p
001700                                                                  dcash01p
001800 IDENTIFICATION DIVISION.                                         dcash01p
001900 PROGRAM-ID.                                                      dcash01p
002000     DCASH01P.                                                    dcash01p
002100 DATE-WRITTEN.                                                    dcash01p
002200     September 2002.                                              dcash01p
002300 DATE-COMPILED.                                                   dcash01p
002400     Today.                                                       dcash01p
002500                                                                  dcash01p
002600 ENVIRONMENT DIVISION.                                            dcash01p
002700                                                                  dcash01p
002800 DATA DIVISION.                                                   dcash01p
002900                                                                  dcash01p
003000 WORKING-STORAGE SECTION.                                         dcash01p
003100 01  WS-MISC-STORAGE.                                             dcash01p
003200   05  WS-PROGRAM-ID                         PIC X(8)             dcash01p
003300       VALUE 'DCASH01P'.                                          dcash01p
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            dcash01p
003500   05  WS-SUB1                               PIC S9(4) COMP.      dcash01p
003600   05  WS-RESP                               PIC S9(8) COMP.      dcash01p
003700   05  WS-BNKCUST-RID                        PIC X(5).            dcash01p
003800                                                                  dcash01p
003900 01 WS-BNKCUST-REC.                                               dcash01p
004000 COPY CBANKVCS.                                                   dcash01p
004100                                                                  dcash01p
004200 01  WS-COMMAREA.                                                 dcash01p
004300 COPY CCASHD01.                                                   dcash01p
004400                                                                  dcash01p
004500 COPY CABENDD.                                                    dcash01p
004600                                                                  dcash01p
004700 LINKAGE SECTION.                                                 dcash01p
004800 01  DFHCOMMAREA.                                                 dcash01p
004900   05  LK-COMMAREA                           PIC X(1)             dcash01p
005000       OCCURS 1 TO 4096 TIMES                                     dcash01p
005100         DEPENDING ON WS-COMMAREA-LENGTH.                         dcash01p
005200                                                                  dcash01p
005300 COPY CENTRY.                                                     dcash01p
005400***************************************************************** dcash01p
005500* Move the passed data to our area                              * dcash01p
005600***************************************************************** dcash01p
005700     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            dcash01p
005800     MOVE DFHCOMMAREA TO WS-COMMAREA.                             dcash01p
005900                                                                  dcash01p
006000***************************************************************** dcash01p
006100* Initialize our output area                                    * dcash01p
006200***************************************************************** dcash01p
006300     MOVE SPACES TO CD01O-DATA.                                   dcash01p
006400                                                                  dcash01p
006500***************************************************************** dcash01p
006600* Now attempt to get the requested record                       * dcash01p
006700***************************************************************** dcash01p
006800     MOVE CD01I-CONTACT-ID TO WS-BNKCUST-RID.                     dcash01p
006900     EXEC CICS READ FILE('BNKCUST')                               dcash01p
007000                    INTO(WS-BNKCUST-REC)                          dcash01p
007100                    LENGTH(LENGTH OF WS-BNKCUST-REC)              dcash01p
007200                    RIDFLD(WS-BNKCUST-RID)                        dcash01p
007300                    RESP(WS-RESP)                                 dcash01p
007400     END-EXEC.                                                    dcash01p
007500                                                                  dcash01p
007600***************************************************************** dcash01p
007700* Did we get the record OK                                      * dcash01p
007800***************************************************************** dcash01p
007900     IF WS-RESP IS EQUAL TO DFHRESP(NORMAL)                       dcash01p
008000        MOVE BCS-REC-ATM-PIN TO CD01O-PIN                         dcash01p
008100     END-IF.                                                      dcash01p
008200                                                                  dcash01p
008300***************************************************************** dcash01p
008400* Was the record not found?                                     * dcash01p
008500***************************************************************** dcash01p
008600     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   dcash01p
008700        MOVE '????' TO CD01O-PIN                                  dcash01p
008800     END-IF.                                                      dcash01p
008900                                                                  dcash01p
009000                                                                  dcash01p
009100***************************************************************** dcash01p
009200* Move the result back to the callers area                      * dcash01p
009300***************************************************************** dcash01p
009400     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       dcash01p
009500                                                                  dcash01p
009600***************************************************************** dcash01p
009700* Return to our caller                                          * dcash01p
009800***************************************************************** dcash01p
009900 COPY CRETURN.                                                    dcash01p
010000                                                                  dcash01p
010100* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     dcash01p
