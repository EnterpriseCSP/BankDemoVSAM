000100***************************************************************** CHELPSQL
000200*                                                               * CHELPSQL
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CHELPSQL
000400*                                                               * CHELPSQL
000500***************************************************************** CHELPSQL
000600                                                                  CHELPSQL
000700***************************************************************** CHELPSQL
000800* CHELPSQL.CPY                                                  * CHELPSQL
000900*---------------------------------------------------------------* CHELPSQL
001000* Define SQL areas to access HELP table                         * CHELPSQL
001100***************************************************************** CHELPSQL
001200     EXEC SQL DECLARE USERID.BNKHELP TABLE                        CHELPSQL
001300     (                                                            CHELPSQL
001400        BHP_SCRN                       CHAR (8)                   CHELPSQL
001500                                       NOT NULL,                  CHELPSQL
001600        BHP_LINE                       CHAR (2)                   CHELPSQL
001700                                       NOT NULL,                  CHELPSQL
001800        BHP_TEXT                       CHAR (75)                  CHELPSQL
001900                                       NOT NULL                   CHELPSQL
002000     )                                                            CHELPSQL
002100     END-EXEC.                                                    CHELPSQL
002200                                                                  CHELPSQL
002300 01  DCLHELP.                                                     CHELPSQL
002400     03 DCL-BHP-SCRN                   PIC X(6).                  CHELPSQL
002500     03 DCL-BHP-LINE                   PIC X(2).                  CHELPSQL
002600     03 DCL-BHP-TEXT                   PIC X(75).                 CHELPSQL
002700                                                                  CHELPSQL
