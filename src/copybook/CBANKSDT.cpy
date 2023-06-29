000100***************************************************************** CBANKSDT
000200*                                                               * CBANKSDT
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKSDT
000400*                                                               * CBANKSDT
000500***************************************************************** CBANKSDT
000600                                                                  CBANKSDT
000700***************************************************************** CBANKSDT
000800* CBANKSDT.CPY                                                  * CBANKSDT
000900*---------------------------------------------------------------* CBANKSDT
001000* Define SQL areas to access bank DeTails view                  * CBANKSDT
001100***************************************************************** CBANKSDT
001200     EXEC SQL DECLARE USERID.VBNKDETS TABLE                       CBANKSDT
001300     (                                                            CBANKSDT
001400        VPID                           CHAR (5)                   CBANKSDT
001500                                       NOT NULL,                  CBANKSDT
001600        VNAME                          CHAR (25)                  CBANKSDT
001700                                       NOT NULL,                  CBANKSDT
001800        VADDR1                         CHAR (25)                  CBANKSDT
001900                                       NOT NULL                   CBANKSDT
002000                                       WITH DEFAULT,              CBANKSDT
002100        VADDR2                         CHAR (25)                  CBANKSDT
002200                                       NOT NULL                   CBANKSDT
002300                                       WITH DEFAULT,              CBANKSDT
002400        VSTATE                         CHAR (2)                   CBANKSDT
002500                                       NOT NULL                   CBANKSDT
002600                                       WITH DEFAULT,              CBANKSDT
002700        VCNTRY                         CHAR (6)                   CBANKSDT
002800                                       NOT NULL                   CBANKSDT
002900                                       WITH DEFAULT,              CBANKSDT
003000        VPSTCDE                        CHAR (6)                   CBANKSDT
003100                                       NOT NULL                   CBANKSDT
003200                                       WITH DEFAULT,              CBANKSDT
003300        VACCNO                         CHAR (9)                   CBANKSDT
003400                                       NOT NULL                   CBANKSDT
003500                                       WITH DEFAULT,              CBANKSDT
003600        VDESC                          CHAR (15)                  CBANKSDT
003700                                       NOT NULL,                  CBANKSDT
003800        VCURRBAL                       DECIMAL (9, 2)             CBANKSDT
003900                                       NOT NULL                   CBANKSDT
004000                                       WITH DEFAULT,              CBANKSDT
004100        VLASTSTMTDTE                   DATE                       CBANKSDT
004200                                       NOT NULL                   CBANKSDT
004300                                       WITH DEFAULT,              CBANKSDT
004400        VLASTSTMTBAL                   DECIMAL (9, 2)             CBANKSDT
004500                                       NOT NULL                   CBANKSDT
004600                                       WITH DEFAULT               CBANKSDT
004700     )                                                            CBANKSDT
004800     END-EXEC.                                                    CBANKSDT
004900                                                                  CBANKSDT
005000                                                                  CBANKSDT
005100 01  DCLVBNKDETS.                                                 CBANKSDT
005200     03 VPID                           PIC X(5).                  CBANKSDT
005300     03 VNAME                          PIC X(25).                 CBANKSDT
005400     03 VADDR1                         PIC X(25).                 CBANKSDT
005500     03 VADDR2                         PIC X(25).                 CBANKSDT
005600     03 VSTATE                         PIC X(2).                  CBANKSDT
005700     03 VCNTRY                         PIC X(6).                  CBANKSDT
005800     03 VPSTCDE                        PIC X(6).                  CBANKSDT
005900     03 VACCNO                         PIC X(9).                  CBANKSDT
006000     03 VDESC                          PIC X(15).                 CBANKSDT
006100     03 VCURRBAL                       PIC S9(7)V9(2) COMP-3.     CBANKSDT
006200     03 VLASTSTMTDTE                   PIC X(10).                 CBANKSDT
006300     03 VLASTSTMTBAL                   PIC S9(7)V9(2) COMP-3.     CBANKSDT
