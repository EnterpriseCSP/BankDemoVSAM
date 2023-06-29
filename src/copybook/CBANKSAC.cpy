000100***************************************************************** CBANKSAC
000200*                                                               * CBANKSAC
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKSAC
000400*                                                               * CBANKSAC
000500***************************************************************** CBANKSAC
000600                                                                  CBANKSAC
000700***************************************************************** CBANKSAC
000800* CBANKSAC.CPY                                                  * CBANKSAC
000900*---------------------------------------------------------------* CBANKSAC
001000* Define SQL areas to access Bank ACcount table                 * CBANKSAC
001100***************************************************************** CBANKSAC
001200     EXEC SQL DECLARE USERID.BNKACC TABLE                         CBANKSAC
001300     (                                                            CBANKSAC
001400        BAC_PID                        CHAR (5)                   CBANKSAC
001500                                       NOT NULL,                  CBANKSAC
001600        BAC_ACCNO                      CHAR (9)                   CBANKSAC
001700                                       NOT NULL                   CBANKSAC
001800                                       WITH DEFAULT,              CBANKSAC
001900        BAC_ACCTYPE                    CHAR (1)                   CBANKSAC
002000                                       NOT NULL                   CBANKSAC
002100                                       WITH DEFAULT,              CBANKSAC
002200        BAC_BALANCE                    DECIMAL (9,2)              CBANKSAC
002300                                       NOT NULL                   CBANKSAC
002400                                       WITH DEFAULT,              CBANKSAC
002500        BAC_LAST_STMT_DTE              DATE                       CBANKSAC
002600                                       NOT NULL                   CBANKSAC
002700                                       WITH DEFAULT,              CBANKSAC
002800        BAC_LAST_STMT_BAL              DECIMAL (9,2)              CBANKSAC
002900                                       NOT NULL                   CBANKSAC
003000                                       WITH DEFAULT,              CBANKSAC
003100        BAC_FILLER                     CHAR (65)                  CBANKSAC
003200                                       NOT NULL                   CBANKSAC
003300                                       WITH DEFAULT               CBANKSAC
003400     )                                                            CBANKSAC
003500     END-EXEC.                                                    CBANKSAC
003600                                                                  CBANKSAC
003700 01  DCLACC.                                                      CBANKSAC
003800     03 DCL-BAC-PID                    PIC X(5).                  CBANKSAC
003900     03 DCL-BAC-ACCNO                  PIC X(9).                  CBANKSAC
004000     03 DCL-BAC-ACCTYPE                PIC X(1).                  CBANKSAC
004100     03 DCL-BAC-BALANCE                PIC S9(7)V99 COMP-3.       CBANKSAC
004200     03 DCL-BAC-LAST-STMT-DTE          PIC X(10).                 CBANKSAC
004300     03 DCL-BAC-LAST-STMT-BAL          PIC S9(7)V99 COMP-3.       CBANKSAC
004400     03 DCL-BAC-FILLER                 PIC X(65).                 CBANKSAC
004500                                                                  CBANKSAC
