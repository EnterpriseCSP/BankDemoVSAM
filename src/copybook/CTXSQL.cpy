000100***************************************************************** CTXSQL
000200*                                                               * CTXSQL
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CTXSQL
000400*                                                               * CTXSQL
000500***************************************************************** CTXSQL
000600                                                                  CTXSQL
000700***************************************************************** CTXSQL
000800* CTXSQL.CPY                                                    * CTXSQL
000900*---------------------------------------------------------------* CTXSQL
001000* Define SQL areas to access TeXt table                         * CTXSQL
001100***************************************************************** CTXSQL
001200     EXEC SQL DECLARE USERID.TTEXT TABLE                          CTXSQL
001300     (                                                            CTXSQL
001400        TX_PAGE                        CHAR (7)                   CTXSQL
001500                                       NOT NULL,                  CTXSQL
001600        TX_SUB_PAGE                    CHAR (1)                   CTXSQL
001700                                       NOT NULL                   CTXSQL
001800        TX_LINE                        CHAR (2)                   CTXSQL
001900                                       NOT NULL                   CTXSQL
002000        TX_TEXT                        CHAR (75)                  CTXSQL
002100                                       NOT NULL                   CTXSQL
002200     )                                                            CTXSQL
002300     END-EXEC.                                                    CTXSQL
002400                                                                  CTXSQL
002500 01  DCLTEXTT.                                                    CTXSQL
002600     03 DCL-TX-PAGE                    PIC X(7).                  CTXSQL
002700     03 DCL-TX-SUB-PAGE                PIC X(1).                  CTXSQL
002800     03 DCL-TX-LINE                    PIC X(2).                  CTXSQL
002900     03 DCL-TX-TEXT                    PIC X(75).                 CTXSQL
003000                                                                  CTXSQL
