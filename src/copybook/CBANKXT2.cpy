000100***************************************************************** cbankxt2
000200*                                                               * cbankxt2
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankxt2
000400*                                                               * cbankxt2
000500***************************************************************** cbankxt2
000600                                                                  cbankxt2
000700***************************************************************** cbankxt2
000800* CBANKXT2.CPY                                                  * cbankxt2
000900*---------------------------------------------------------------* cbankxt2
001000* This is the record file record layout used to extract data    * cbankxt2
001100* from the bank files to produce mailing addres labels.         * cbankxt2
001200***************************************************************** cbankxt2
001300   05  XTACT02-RECORD.                                            cbankxt2
001400     10  BANKXT02-NAME                       PIC X(25).           cbankxt2
001500     10  BANKXT02-ADDR1                      PIC X(25).           cbankxt2
001600     10  BANKXT02-ADDR2                      PIC X(25).           cbankxt2
001700     10  BANKXT02-ADDR3                      PIC X(25).           cbankxt2
001800     10  BANKXT02-ZIP                        PIC X(6).            cbankxt2
001900     10  BANKXT02-PID                        PIC X(5).            cbankxt2
002000     10  BANKXT02-POLICY-ID                  PIC X(8).            cbankxt2
002100                                                                  cbankxt2
002200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankxt2
