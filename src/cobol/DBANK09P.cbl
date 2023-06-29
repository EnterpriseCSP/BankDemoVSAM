000100***************************************************************** dbank09p
000200*                                                               * dbank09p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * dbank09p
000400*   This demonstration program is provided for use by users     * dbank09p
000500*   of Micro Focus products and may be used, modified and       * dbank09p
000600*   distributed as part of your application provided that       * dbank09p
000700*   you properly acknowledge the copyright of Micro Focus       * dbank09p
000800*   in this material.                                           * dbank09p
000900*                                                               * dbank09p
001000***************************************************************** dbank09p
001100                                                                  dbank09p
001200***************************************************************** dbank09p
001300* Program:     DBANK09P.CBL                                     * dbank09p
001400* Function:    Obtain contact information for statements        * dbank09p
001500*              VSAM version                                     * dbank09p
001600***************************************************************** dbank09p
001700                                                                  dbank09p
001800 IDENTIFICATION DIVISION.                                         dbank09p
001900 PROGRAM-ID.                                                      dbank09p
002000     DBANK09P.                                                    dbank09p
002100 DATE-WRITTEN.                                                    dbank09p
002200     September 2002.                                              dbank09p
002300 DATE-COMPILED.                                                   dbank09p
002400     Today.                                                       dbank09p
002500                                                                  dbank09p
002600 ENVIRONMENT DIVISION.                                            dbank09p
002700                                                                  dbank09p
002800 DATA DIVISION.                                                   dbank09p
002900                                                                  dbank09p
003000 WORKING-STORAGE SECTION.                                         dbank09p
003100 01  WS-MISC-STORAGE.                                             dbank09p
003200   05  WS-PROGRAM-ID                         PIC X(8)             dbank09p
003300       VALUE 'DBANK09P'.                                          dbank09p
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            dbank09p
003500   05  WS-RESP                               PIC S9(8) COMP.      dbank09p
003600   05  WS-BNKCUST-RID                        PIC X(5).            dbank09p
003700                                                                  dbank09p
003800 01 WS-BNKCUST-REC.                                               dbank09p
003900 COPY CBANKVCS.                                                   dbank09p
004000                                                                  dbank09p
004100 01  WS-COMMAREA.                                                 dbank09p
004200 COPY CBANKD09.                                                   dbank09p
004300                                                                  dbank09p
004400 COPY CABENDD.                                                    dbank09p
004500                                                                  dbank09p
004600 LINKAGE SECTION.                                                 dbank09p
004700 01  DFHCOMMAREA.                                                 dbank09p
004800   05  LK-COMMAREA                           PIC X(1)             dbank09p
004900       OCCURS 1 TO 4096 TIMES                                     dbank09p
005000         DEPENDING ON WS-COMMAREA-LENGTH.                         dbank09p
005100                                                                  dbank09p
005200 COPY CENTRY.                                                     dbank09p
005300***************************************************************** dbank09p
005400* Move the passed data to our area                              * dbank09p
005500***************************************************************** dbank09p
005600     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            dbank09p
005700     MOVE DFHCOMMAREA TO WS-COMMAREA.                             dbank09p
005800                                                                  dbank09p
005900***************************************************************** dbank09p
006000* Initialize our output area                                    * dbank09p
006100***************************************************************** dbank09p
006200     MOVE SPACES TO CD09O-DATA.                                   dbank09p
006300                                                                  dbank09p
006400***************************************************************** dbank09p
006500* Now attempt to get the requested record                       * dbank09p
006600***************************************************************** dbank09p
006700     MOVE CD09I-CONTACT-ID TO WS-BNKCUST-RID.                     dbank09p
006800     EXEC CICS READ FILE('BNKCUST')                               dbank09p
006900                    INTO(WS-BNKCUST-REC)                          dbank09p
007000                    LENGTH(LENGTH OF WS-BNKCUST-REC)              dbank09p
007100                    RIDFLD(WS-BNKCUST-RID)                        dbank09p
007200                    RESP(WS-RESP)                                 dbank09p
007300     END-EXEC.                                                    dbank09p
007400                                                                  dbank09p
007500***************************************************************** dbank09p
007600* Did we get the record OK                                      * dbank09p
007700***************************************************************** dbank09p
007800     IF WS-RESP IS EQUAL TO DFHRESP(NORMAL)                       dbank09p
007900        MOVE BCS-REC-PID TO CD09O-CONTACT-ID                      dbank09p
008000        MOVE BCS-REC-NAME TO CD09O-CONTACT-NAME                   dbank09p
008100        MOVE BCS-REC-ADDR1 TO CD09O-CONTACT-ADDR1                 dbank09p
008200        MOVE BCS-REC-ADDR2 TO CD09O-CONTACT-ADDR2                 dbank09p
008300        MOVE BCS-REC-STATE TO CD09O-CONTACT-STATE                 dbank09p
008400        MOVE BCS-REC-CNTRY TO CD09O-CONTACT-CNTRY                 dbank09p
008500        MOVE BCS-REC-POST-CODE TO CD09O-CONTACT-PSTCDE            dbank09p
008600        MOVE BCS-REC-EMAIL TO CD09O-CONTACT-EMAIL                 dbank09p
008700     END-IF.                                                      dbank09p
008800                                                                  dbank09p
008900***************************************************************** dbank09p
009000* Was the record not found?                                     * dbank09p
009100***************************************************************** dbank09p
009200     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   dbank09p
009300        MOVE SPACES TO CD09O-DATA                                 dbank09p
009400        MOVE HIGH-VALUES TO CD09O-CONTACT-ID                      dbank09p
009500        MOVE 'Bad VSAM read' TO CD09O-CONTACT-NAME                dbank09p
009600     END-IF.                                                      dbank09p
009700                                                                  dbank09p
009800***************************************************************** dbank09p
009900* Move the result back to the callers area                      * dbank09p
010000***************************************************************** dbank09p
010100     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       dbank09p
010200                                                                  dbank09p
010300***************************************************************** dbank09p
010400* Return to our caller                                          * dbank09p
010500***************************************************************** dbank09p
010600 COPY CRETURN.                                                    dbank09p
010700                                                                  dbank09p
010800* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     dbank09p
