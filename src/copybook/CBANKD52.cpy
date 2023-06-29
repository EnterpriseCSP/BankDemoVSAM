000100***************************************************************** cbankd52
000200*                                                               * cbankd52
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankd52
000400*                                                               * cbankd52
000500***************************************************************** cbankd52
000600                                                                  cbankd52
000700****************************************************************  cbankd52
000800* CD52DATA.CPY                                                 *  cbankd52
000900*--------------------------------------------------------------*  cbankd52
001000* This area is used to pass data between ????????????????????  *  cbankd52
001100* display program and the I/O program (DBANK52P) which         *  cbankd52
001200* retrieves the data requested ????????????????????????????    *  cbankd52
001300****************************************************************  cbankd52
001400   05  CD52-DATA.                                                 cbankd52
001500     10  CD52I-DATA.                                              cbankd52
001600       15  CD52I-PID                         PIC X(5).            cbankd52
001700         88  CD52-REQUESTED-ALL              VALUE 'ALL  '.       cbankd52
001800     10  CD52O-DATA.                                              cbankd52
001900       15  CD52O-PID                         PIC X(5).            cbankd52
002000       15  CD52O-ACC-NO                      PIC X(9).            cbankd52
002100       15  CD52O-TIMESTAMP                   PIC X(26).           cbankd52
002200       15  CD52O-AMOUNT                      PIC S9(7)V99 COMP-3. cbankd52
002300       15  CD52O-DESC                        PIC X(30).           cbankd52
002400                                                                  cbankd52
002500* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankd52
