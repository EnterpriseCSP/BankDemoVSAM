000100***************************************************************** cbankd51
000200*                                                               * cbankd51
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankd51
000400*                                                               * cbankd51
000500***************************************************************** cbankd51
000600                                                                  cbankd51
000700****************************************************************  cbankd51
000800* CD51DATA.CPY                                                 *  cbankd51
000900*--------------------------------------------------------------*  cbankd51
001000* This area is used to pass data between ????????????????????  *  cbankd51
001100* display program and the I/O program (DBANK51P) which         *  cbankd51
001200* retrieves the data requested ????????????????????????????    *  cbankd51
001300****************************************************************  cbankd51
001400   05  CD51-DATA.                                                 cbankd51
001500     10  CD51I-DATA.                                              cbankd51
001600       15  CD51I-PID                         PIC X(5).            cbankd51
001700         88  CD51-REQUESTED-ALL              VALUE 'ALL  '.       cbankd51
001800     10  CD51O-DATA.                                              cbankd51
001900       15  CD51O-PID                         PIC X(5).            cbankd51
002000       15  CD51O-NAME                        PIC X(25).           cbankd51
002100       15  CD51O-ADDR1                       PIC X(25).           cbankd51
002200       15  CD51O-ADDR2                       PIC X(25).           cbankd51
002300       15  CD51O-STATE                       PIC X(2).            cbankd51
002400       15  CD51O-CNTRY                       PIC X(6).            cbankd51
002500       15  CD51O-POST-CODE                   PIC X(6).            cbankd51
002600       15  CD51O-EMAIL                       PIC X(30).           cbankd51
002700       15  CD51O-ACC-NO                      PIC X(9).            cbankd51
002800       15  CD51O-ACC-DESC                    PIC X(15).           cbankd51
002900       15  CD51O-ACC-CURR-BAL                PIC S9(7)V99 COMP-3. cbankd51
003000       15  CD51O-ACC-LAST-STMT-DTE           PIC X(10).           cbankd51
003100       15  CD51O-ACC-LAST-STMT-BAL           PIC S9(7)V99 COMP-3. cbankd51
003200                                                                  cbankd51
003300* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankd51
