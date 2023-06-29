000100***************************************************************** cbankxt1
000200*                                                               * cbankxt1
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankxt1
000400*                                                               * cbankxt1
000500***************************************************************** cbankxt1
000600                                                                  cbankxt1
000700***************************************************************** cbankxt1
000800* CBANKXT1.CPY                                                  * cbankxt1
000900*---------------------------------------------------------------* cbankxt1
001000* This is the record file record layout used to extract data    * cbankxt1
001100* from the bank file to produce statements.                     * cbankxt1
001200***************************************************************** cbankxt1
001300 01  BANKXT01-REC0.                                               cbankxt1
001400   10  BANKXT01-0-TYPE                       PIC X(1).            cbankxt1
001500   10  BANKXT01-0-PID                        PIC X(5).            cbankxt1
001600   10  BANKXT01-0-NAME                       PIC X(25).           cbankxt1
001700   10  BANKXT01-0-EMAIL                      PIC X(30).           cbankxt1
001800   10  BANKXT01-0-FILLER                     PIC X(5).            cbankxt1
001900 01  BANKXT01-REC1.                                               cbankxt1
002000   10  BANKXT01-1-TYPE                       PIC X(1).            cbankxt1
002100   10  BANKXT01-1-PID                        PIC X(5).            cbankxt1
002200   10  BANKXT01-1-NAME                       PIC X(25).           cbankxt1
002300   10  BANKXT01-1-ADDR1                      PIC X(25).           cbankxt1
002400   10  BANKXT01-1-ADDR2                      PIC X(25).           cbankxt1
002500   10  BANKXT01-1-STATE                      PIC X(2).            cbankxt1
002600   10  BANKXT01-1-CNTRY                      PIC X(6).            cbankxt1
002700   10  BANKXT01-1-PST-CDE                    PIC X(6).            cbankxt1
002800 01  BANKXT01-REC2.                                               cbankxt1
002900   10  BANKXT01-2-TYPE                       PIC X(1).            cbankxt1
003000   10  BANKXT01-2-PID                        PIC X(5).            cbankxt1
003100   10  BANKXT01-2-ACC-NO                     PIC X(9).            cbankxt1
003200   10  BANKXT01-2-ACC-DESC                   PIC X(15).           cbankxt1
003300   10  BANKXT01-2-ACC-CURR-BAL               PIC S9(7)V99 COMP-3. cbankxt1
003400   10  BANKXT01-2-ACC-LAST-STMT-DTE          PIC X(26).           cbankxt1
003500   10  BANKXT01-2-ACC-LAST-STMT-BAL          PIC S9(7)V99 COMP-3. cbankxt1
003600 01  BANKXT01-REC3.                                               cbankxt1
003700   10  BANKXT01-3-TYPE                       PIC X(1).            cbankxt1
003800   10  BANKXT01-3-PID                        PIC X(5).            cbankxt1
003900   10  BANKXT01-3-ACC-NO                     PIC X(9).            cbankxt1
004000   10  BANKXT01-3-TIMESTAMP                  PIC X(26).           cbankxt1
004100   10  BANKXT01-3-AMOUNT                     PIC S9(7)V99 COMP-3. cbankxt1
004200   10  BANKXT01-3-DESC                       PIC X(30).           cbankxt1
004300                                                                  cbankxt1
004400* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankxt1
