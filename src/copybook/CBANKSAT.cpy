000100***************************************************************** CBANKSAT
000200*                                                               * CBANKSAT
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKSAT
000400*                                                               * CBANKSAT
000500***************************************************************** CBANKSAT
000600                                                                  CBANKSAT
000700***************************************************************** CBANKSAT
000800* CBANKSAT.CPY                                                  * CBANKSAT
000900*---------------------------------------------------------------* CBANKSAT
001000* Define SQL areas to access Account Type (Descriptions)        * CBANKSAT
001100***************************************************************** CBANKSAT
001200     EXEC SQL DECLARE USERID.BNKATYP TABLE                        CBANKSAT
001300     (                                                            CBANKSAT
001400        BAT_TYPE                       CHAR (1)                   CBANKSAT
001500                                       NOT NULL,                  CBANKSAT
001600        BAT_DESC                       CHAR (15)                  CBANKSAT
001700                                       NOT NULL,                  CBANKSAT
001800        BAT_FILLER                     CHAR (84)                  CBANKSAT
001900                                       NOT NULL                   CBANKSAT
002000     )                                                            CBANKSAT
002100     END-EXEC.                                                    CBANKSAT
002200                                                                  CBANKSAT
002300 01  DCLATYP.                                                     CBANKSAT
002400     03 DCL-BAT-TYPE                   PIC X(1).                  CBANKSAT
002500     03 DCL-BAT-DESC                   PIC X(15).                 CBANKSAT
002600     03 DCL-BAT-FILLER                 PIC X(84).                 CBANKSAT
002700                                                                  CBANKSAT
