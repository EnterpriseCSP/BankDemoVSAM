000100***************************************************************** cbankd10
000200*                                                               * cbankd10
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankd10
000400*                                                               * cbankd10
000500***************************************************************** cbankd10
000600                                                                  cbankd10
000700***************************************************************** cbankd10
000800* CBANKD10.CPY                                                  * cbankd10
000900*---------------------------------------------------------------* cbankd10
001000* This area is used to pass data between a requesting program   * cbankd10
001100* and the I/O program (DBANK10P) which retrieves or updates     * cbankd10
001200* address information.                                          * cbankd10
001300***************************************************************** cbankd10
001400   05  CD10-DATA.                                                 cbankd10
001500     10  CD10I-DATA.                                              cbankd10
001600       15  CD10I-FUNCTION                    PIC X(1).            cbankd10
001700         88  CD10I-DELETE                    VALUE 'D'.           cbankd10
001800         88  CD10I-READ                      VALUE 'R'.           cbankd10
001900         88  CD10I-WRITE                     VALUE 'W'.           cbankd10
002000       15  CD10I-TERM                        PIC X(8).            cbankd10
002100       15  CD10I-SPA-DATA                    PIC X(6144).         cbankd10
002200     10  CD10O-DATA.                                              cbankd10
002300       15  CD10O-RESULT                      PIC X(1).            cbankd10
002400         88  CD10O-RESULT-OK                 VALUE '0'.           cbankd10
002500         88  CD10O-RESULT-NOT-FOUND          VALUE '1'.           cbankd10
002600         88  CD10O-RESULT-FAILED             VALUE '2'.           cbankd10
002700       15  CD10O-SPA-DATA                    PIC X(6144).         cbankd10
002800                                                                  cbankd10
002900* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankd10
