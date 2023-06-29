000100***************************************************************** ctimerd
000200*                                                               * ctimerd
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * ctimerd
000400*                                                               * ctimerd
000500***************************************************************** ctimerd
000600                                                                  ctimerd
000700***************************************************************** ctimerd
000800* CTIMERD.CPY                                                   * ctimerd
000900*---------------------------------------------------------------* ctimerd
001000* Work areas for run timer                                      * ctimerd
001100***************************************************************** ctimerd
001200 01  TIMER-DATA.                                                  ctimerd
001300   05  TIMER-START                           PIC 9(8)             ctimerd
001400       VALUE ZERO.                                                ctimerd
001500   05  FILLER REDEFINES TIMER-START.                              ctimerd
001600     10  TIMER-START-HH                      PIC 9(2).            ctimerd
001700     10  TIMER-START-MM                      PIC 9(2).            ctimerd
001800     10  TIMER-START-SS                      PIC 9(2).            ctimerd
001900     10  TIMER-START-DD                      PIC 9(2).            ctimerd
002000   05  TIMER-END                             PIC 9(8)             ctimerd
002100       VALUE ZERO.                                                ctimerd
002200   05  FILLER REDEFINES TIMER-END.                                ctimerd
002300     10  TIMER-END-HH                        PIC 9(2).            ctimerd
002400     10  TIMER-END-MM                        PIC 9(2).            ctimerd
002500     10  TIMER-END-SS                        PIC 9(2).            ctimerd
002600     10  TIMER-END-DD                        PIC 9(2).            ctimerd
002700   05  TIMER-ELAPSED                         PIC 9(8).            ctimerd
002800   05  TIMER-ELAPSED-R REDEFINES TIMER-ELAPSED                    ctimerd
002900                                             PIC 9(6)V9(2).       ctimerd
003000   05  TIMER-RUN-TIME.                                            ctimerd
003100     10  FILLER                              PIC X(17)            ctimerd
003200         VALUE 'Elaped run time: '.                               ctimerd
003300     10  TIMER-RUN-TIME-ELAPSED              PIC Z(5)9.99.        ctimerd
003400     10  FILLER                              PIC X(8)             ctimerd
003500         VALUE ' seconds'.                                        ctimerd
003600                                                                  ctimerd
003700* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     ctimerd
