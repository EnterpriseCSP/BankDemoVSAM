000100***************************************************************** dbank02p
000200*                                                               * dbank02p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * dbank02p
000400*   This demonstration program is provided for use by users     * dbank02p
000500*   of Micro Focus products and may be used, modified and       * dbank02p
000600*   distributed as part of your application provided that       * dbank02p
000700*   you properly acknowledge the copyright of Micro Focus       * dbank02p
000800*   in this material.                                           * dbank02p
000900*                                                               * dbank02p
001000***************************************************************** dbank02p
001100                                                                  dbank02p
001200***************************************************************** dbank02p
001300* Program:     DBANK02P.CBL                                     * dbank02p
001400* Function:    Obtain/update address information                * dbank02p
001500*              VSAM version                                     * dbank02p
001600***************************************************************** dbank02p
001700                                                                  dbank02p
001800 IDENTIFICATION DIVISION.                                         dbank02p
001900 PROGRAM-ID.                                                      dbank02p
002000     DBANK02P.                                                    dbank02p
002100 DATE-WRITTEN.                                                    dbank02p
002200     September 2002.                                              dbank02p
002300 DATE-COMPILED.                                                   dbank02p
002400     Today.                                                       dbank02p
002500                                                                  dbank02p
002600 ENVIRONMENT DIVISION.                                            dbank02p
002700                                                                  dbank02p
002800 DATA DIVISION.                                                   dbank02p
002900                                                                  dbank02p
003000 WORKING-STORAGE SECTION.                                         dbank02p
003100 01  WS-MISC-STORAGE.                                             dbank02p
003200   05  WS-PROGRAM-ID                         PIC X(8)             dbank02p
003300       VALUE 'DBANK02P'.                                          dbank02p
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            dbank02p
003500   05  WS-RESP                               PIC S9(8) COMP.      dbank02p
003600   05  WS-BNKCUST-RID                        PIC X(5).            dbank02p
003700                                                                  dbank02p
003800 01 WS-BNKCUST-REC.                                               dbank02p
003900 COPY CBANKVCS.                                                   dbank02p
004000                                                                  dbank02p
004100 01  WS-COMMAREA.                                                 dbank02p
004200 COPY CBANKD02.                                                   dbank02p
004300                                                                  dbank02p
004400 COPY CABENDD.                                                    dbank02p
004500                                                                  dbank02p
004600 LINKAGE SECTION.                                                 dbank02p
004700 01  DFHCOMMAREA.                                                 dbank02p
004800   05  LK-COMMAREA                           PIC X(1)             dbank02p
004900       OCCURS 1 TO 4096 TIMES                                     dbank02p
005000         DEPENDING ON WS-COMMAREA-LENGTH.                         dbank02p
005100                                                                  dbank02p
005200 COPY CENTRY.                                                     dbank02p
005300***************************************************************** dbank02p
005400* Move the passed data to our area                              * dbank02p
005500***************************************************************** dbank02p
005600     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            dbank02p
005700     MOVE DFHCOMMAREA TO WS-COMMAREA.                             dbank02p
005800                                                                  dbank02p
005900***************************************************************** dbank02p
006000* Initialize our output area                                    * dbank02p
006100***************************************************************** dbank02p
006200     MOVE SPACES TO CD02O-DATA.                                   dbank02p
006300                                                                  dbank02p
006400***************************************************************** dbank02p
006500* See if we have a read or write request and react accordingly  * dbank02p
006600***************************************************************** dbank02p
006700     EVALUATE TRUE                                                dbank02p
006800       WHEN CD02I-READ                                            dbank02p
006900         PERFORM READ-PROCESSING THRU                             dbank02p
007000                 READ-PROCESSING-EXIT                             dbank02p
007100       WHEN CD02I-WRITE                                           dbank02p
007200         PERFORM WRITE-PROCESSING THRU                            dbank02p
007300                 WRITE-PROCESSING-EXIT                            dbank02p
007400       WHEN OTHER                                                 dbank02p
007500         MOVE HIGH-VALUES TO CD02O-CONTACT-ID                     dbank02p
007600         MOVE 'Bad request code' TO CD02O-CONTACT-NAME            dbank02p
007700     END-EVALUATE.                                                dbank02p
007800                                                                  dbank02p
007900***************************************************************** dbank02p
008000* Move the result back to the callers area                      * dbank02p
008100***************************************************************** dbank02p
008200     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       dbank02p
008300                                                                  dbank02p
008400***************************************************************** dbank02p
008500* Return to our caller                                          * dbank02p
008600***************************************************************** dbank02p
008700 COPY CRETURN.                                                    dbank02p
008800                                                                  dbank02p
008900***************************************************************** dbank02p
009000* Read request                                                  * dbank02p
009100***************************************************************** dbank02p
009200 READ-PROCESSING.                                                 dbank02p
009300                                                                  dbank02p
009400***************************************************************** dbank02p
009500* Now attempt to get the requested record                       * dbank02p
009600***************************************************************** dbank02p
009700     MOVE CD02I-CONTACT-ID TO WS-BNKCUST-RID.                     dbank02p
009800     EXEC CICS READ FILE('BNKCUST')                               dbank02p
009900                    INTO(WS-BNKCUST-REC)                          dbank02p
010000                    LENGTH(LENGTH OF WS-BNKCUST-REC)              dbank02p
010100                    RIDFLD(WS-BNKCUST-RID)                        dbank02p
010200                    RESP(WS-RESP)                                 dbank02p
010300     END-EXEC.                                                    dbank02p
010400                                                                  dbank02p
010500***************************************************************** dbank02p
010600* Did we get the record OK                                      * dbank02p
010700***************************************************************** dbank02p
010800     IF WS-RESP IS EQUAL TO DFHRESP(NORMAL)                       dbank02p
010900        MOVE BCS-REC-PID TO CD02O-CONTACT-ID                      dbank02p
011000        MOVE BCS-REC-NAME TO CD02O-CONTACT-NAME                   dbank02p
011100        MOVE BCS-REC-ADDR1 TO CD02O-CONTACT-ADDR1                 dbank02p
011200        MOVE BCS-REC-ADDR2 TO CD02O-CONTACT-ADDR2                 dbank02p
011300        MOVE BCS-REC-STATE TO CD02O-CONTACT-STATE                 dbank02p
011400        MOVE BCS-REC-CNTRY TO CD02O-CONTACT-CNTRY                 dbank02p
011500        MOVE BCS-REC-POST-CODE TO CD02O-CONTACT-PSTCDE            dbank02p
011600        MOVE BCS-REC-TEL TO CD02O-CONTACT-TELNO                   dbank02p
011700        MOVE BCS-REC-EMAIL TO CD02O-CONTACT-EMAIL                 dbank02p
011800        MOVE BCS-REC-SEND-MAIL TO CD02O-CONTACT-SEND-MAIL         dbank02p
011900        MOVE BCS-REC-SEND-EMAIL TO CD02O-CONTACT-SEND-EMAIL       dbank02p
012000     END-IF.                                                      dbank02p
012100                                                                  dbank02p
012200***************************************************************** dbank02p
012300* Was the record not found?                                     * dbank02p
012400***************************************************************** dbank02p
012500     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   dbank02p
012600        MOVE SPACES TO CD02O-DATA                                 dbank02p
012700        MOVE HIGH-VALUES TO CD02O-CONTACT-ID                      dbank02p
012800        MOVE 'Bad VSAM read' TO CD02O-CONTACT-NAME                dbank02p
012900     END-IF.                                                      dbank02p
013000                                                                  dbank02p
013100 READ-PROCESSING-EXIT.                                            dbank02p
013200     EXIT.                                                        dbank02p
013300                                                                  dbank02p
013400***************************************************************** dbank02p
013500* Write request                                                 * dbank02p
013600***************************************************************** dbank02p
013700 WRITE-PROCESSING.                                                dbank02p
013800                                                                  dbank02p
013900***************************************************************** dbank02p
014000* Now attempt to get the requested record for update            * dbank02p
014100***************************************************************** dbank02p
014200     MOVE CD02I-CONTACT-ID TO WS-BNKCUST-RID.                     dbank02p
014300     EXEC CICS READ FILE('BNKCUST')                               dbank02p
014400                    UPDATE                                        dbank02p
014500                    INTO(WS-BNKCUST-REC)                          dbank02p
014600                    LENGTH(LENGTH OF WS-BNKCUST-REC)              dbank02p
014700                    RIDFLD(WS-BNKCUST-RID)                        dbank02p
014800                    RESP(WS-RESP)                                 dbank02p
014900     END-EXEC.                                                    dbank02p
015000                                                                  dbank02p
015100***************************************************************** dbank02p
015200* Did we get the record for update                              * dbank02p
015300***************************************************************** dbank02p
015400     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   dbank02p
015500        MOVE SPACES TO CD02O-DATA                                 dbank02p
015600        MOVE HIGH-VALUES TO CD02O-CONTACT-ID                      dbank02p
015700        MOVE 'Unable to lock record' TO CD02O-CONTACT-NAME        dbank02p
015800        GO TO WRITE-PROCESSING-EXIT                               dbank02p
015900     END-IF.                                                      dbank02p
016000                                                                  dbank02p
016100     MOVE CD02I-CONTACT-ADDR1 TO BCS-REC-ADDR1.                   dbank02p
016200     MOVE CD02I-CONTACT-ADDR2 TO BCS-REC-ADDR2.                   dbank02p
016300     MOVE CD02I-CONTACT-STATE TO BCS-REC-STATE.                   dbank02p
016400     MOVE CD02I-CONTACT-CNTRY TO BCS-REC-CNTRY.                   dbank02p
016500     MOVE CD02I-CONTACT-PSTCDE TO BCS-REC-POST-CODE.              dbank02p
016600     MOVE CD02I-CONTACT-STATE TO BCS-REC-STATE.                   dbank02p
016700     MOVE CD02I-CONTACT-EMAIL TO BCS-REC-EMAIL.                   dbank02p
016800     MOVE CD02I-CONTACT-SEND-MAIL TO BCS-REC-SEND-MAIL.           dbank02p
016900     MOVE CD02I-CONTACT-SEND-EMAIL TO BCS-REC-SEND-EMAIL.         dbank02p
017000     EXEC CICS REWRITE FILE('BNKCUST')                            dbank02p
017100                       FROM(WS-BNKCUST-REC)                       dbank02p
017200                       LENGTH(LENGTH OF WS-BNKCUST-REC)           dbank02p
017300                       RESP(WS-RESP)                              dbank02p
017400     END-EXEC.                                                    dbank02p
017500                                                                  dbank02p
017600***************************************************************** dbank02p
017700* Did we update the record OK                                   * dbank02p
017800***************************************************************** dbank02p
017900     IF WS-RESP IS EQUAL TO DFHRESP(NORMAL)                       dbank02p
018000        MOVE HIGH-VALUES TO CD02O-CONTACT-ID                      dbank02p
018100        MOVE 'Update OK' TO CD02O-CONTACT-NAME                    dbank02p
018200     END-IF.                                                      dbank02p
018300                                                                  dbank02p
018400***************************************************************** dbank02p
018500* The record update failed                                      * dbank02p
018600***************************************************************** dbank02p
018700     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   dbank02p
018800        MOVE SPACES TO CD02O-DATA                                 dbank02p
018900        MOVE HIGH-VALUES TO CD02O-CONTACT-ID                      dbank02p
019000        MOVE 'Update failed' TO CD02O-CONTACT-NAME                dbank02p
019100     END-IF.                                                      dbank02p
019200                                                                  dbank02p
019300 WRITE-PROCESSING-EXIT.                                           dbank02p
019400     EXIT.                                                        dbank02p
019500                                                                  dbank02p
019600* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     dbank02p
