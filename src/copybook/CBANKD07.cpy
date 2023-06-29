000100***************************************************************** cbankd07
000200*                                                               * cbankd07
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankd07
000400*                                                               * cbankd07
000500***************************************************************** cbankd07
000600* hjsfhjsghjgsjg                                                  cbankd07
000700***************************************************************** cbankd07
000800* CBANKD07.CPY                                                  * cbankd07
000900*---------------------------------------------------------------* cbankd07
001000* This area is used to pass data between a requesting program   * cbankd07
001100* and the I/O program (DBANK07P) which retrieves or updates     * cbankd07
001200* address information.                                          * cbankd07
001300***************************************************************** cbankd07
001400   05  CD07-DATA.                                                 cbankd07
001500     10  CD07I-DATA.                                              cbankd07
001600       15  CD07I-PERSON-PID                  PIC X(5).            cbankd07
001700       15  CD07I-TIMESTAMP                   PIC X(26).           cbankd07
001800       15  CD07I-OLD-DATA                    PIC X(150).          cbankd07
001900       15  FILLER REDEFINES CD07I-OLD-DATA.                       cbankd07
002000         20  CD07I-OLD-ADDR1                 PIC X(25).           cbankd07
002100         20  CD07I-OLD-ADDR2                 PIC X(25).           cbankd07
002200         20  CD07I-OLD-STATE                 PIC X(2).            cbankd07
002300         20  CD07I-OLD-CNTRY                 PIC X(6).            cbankd07
002400         20  CD07I-OLD-PSTCDE                PIC X(6).            cbankd07
002500         20  CD07I-OLD-TELNO                 PIC X(12).           cbankd07
002600         20  CD07I-OLD-EMAIL                 PIC X(30).           cbankd07
002700         20  CD07I-OLD-SEND-MAIL             PIC X(1).            cbankd07
002800         20  CD07I-OLD-SEND-EMAIL            PIC X(1).            cbankd07
002900       15  CD07I-NEW-DATA                    PIC X(150).          cbankd07
003000       15  FILLER REDEFINES CD07I-NEW-DATA.                       cbankd07
003100         20  CD07I-NEW-ADDR1                 PIC X(25).           cbankd07
003200         20  CD07I-NEW-ADDR2                 PIC X(25).           cbankd07
003300         20  CD07I-NEW-STATE                 PIC X(2).            cbankd07
003400         20  CD07I-NEW-CNTRY                 PIC X(6).            cbankd07
003500         20  CD07I-NEW-PSTCDE                PIC X(6).            cbankd07
003600         20  CD07I-NEW-TELNO                 PIC X(12).           cbankd07
003700         20  CD07I-NEW-EMAIL                 PIC X(30).           cbankd07
003800         20  CD07I-NEW-SEND-MAIL             PIC X(1).            cbankd07
003900         20  CD07I-NEW-SEND-EMAIL            PIC X(1).            cbankd07
004000     10  CD07O-DATA.                                              cbankd07
004100       15  CD07O-RESULT                      PIC X(1).            cbankd07
004200         88  CD07O-UPDATE-OK                 VALUE '0'.           cbankd07
004300         88  CD07O-UPDATE-FAIL               VALUE '1'.           cbankd07
004400       15  CD07O-MSG                         PIC X(62).           cbankd07
004500                                                                  cbankd07
004600* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankd07
