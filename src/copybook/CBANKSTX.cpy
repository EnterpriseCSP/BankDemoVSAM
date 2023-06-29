000100***************************************************************** CBANKSTX
000200*                                                               * CBANKSTX
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKSTX
000400*                                                               * CBANKSTX
000500***************************************************************** CBANKSTX
000600                                                                  CBANKSTX
000700***************************************************************** CBANKSTX
000800* CBANKSTX.CPY                                                  * CBANKSTX
000900*---------------------------------------------------------------* CBANKSTX
001000* Define SQL areas to access bank Transaction table             * CBANKSTX
001100***************************************************************** CBANKSTX
001200     EXEC SQL DECLARE USERID.BNKTXN TABLE                         CBANKSTX
001300     (                                                            CBANKSTX
001400        BTX_PID                        CHAR (5)                   CBANKSTX
001500                                       NOT NULL,                  CBANKSTX
001600        BTX_TYPE                       CHAR (1)                   CBANKSTX
001700                                       NOT NULL,                  CBANKSTX
001800        BTX_SUB_TYPE                   CHAR (1)                   CBANKSTX
001900                                       NOT NULL,                  CBANKSTX
002000        BTX_ACCNO                      CHAR (9)                   CBANKSTX
002100                                       NOT NULL                   CBANKSTX
002200                                       WITH DEFAULT,              CBANKSTX
002300        BXT_TIMESTAMP                  TIMESTAMP                  CBANKSTX
002400                                       NOT NULL                   CBANKSTX
002500                                       WITH DEFAULT,              CBANKSTX
002600        BTX_TIMESTAMP_FF               CHAR (26)                  CBANKSTX
002700                                       NOT NULL                   CBANKSTX
002800                                       WITH DEFAULT,              CBANKSTX
002900        BTX_AMOUNT                     DECIMAL (9,2)              CBANKSTX
003000                                       NOT NULL                   CBANKSTX
003100                                       WITH DEFAULT,              CBANKSTX
003200        BTX_DATA_OLD                   CHAR (150)                 CBANKSTX
003300                                       NOT NULL                   CBANKSTX
003400                                       WITH DEFAULT,              CBANKSTX
003500        BTX_DATA_NEW                   CHAR (150)                 CBANKSTX
003600                                       NOT NULL                   CBANKSTX
003700                                       WITH DEFAULT,              CBANKSTX
003800        BTX_FILLER                     CHAR (27)                  CBANKSTX
003900                                       NOT NULL                   CBANKSTX
004000                                       WITH DEFAULT               CBANKSTX
004100     )                                                            CBANKSTX
004200     END-EXEC.                                                    CBANKSTX
004300                                                                  CBANKSTX
004400 01  DCLTXN.                                                      CBANKSTX
004500     03 DCL-BTX-PID                    PIC X(5).                  CBANKSTX
004600     03 DCL-BTX-TYPE                   PIC X(1).                  CBANKSTX
004700     03 DCL-BTX-SUB-TYPE               PIC X(1).                  CBANKSTX
004800     03 DCL-BTX-ACCNO                  PIC X(9).                  CBANKSTX
004900     03 DCL-BTX-TIMESTAMP              PIC X(26).                 CBANKSTX
005000     03 DCL-BTX-TIMESTAMP-FF           PIC X(26).                 CBANKSTX
005100     03 DCL-BTX-AMOUNT                 PIC S9(7)V99 COMP-3.       CBANKSTX
005200     03 DCL-BTX-DATA-OLD               PIC X(150).                CBANKSTX
005300     03 DCL-BTX-DATA-NEW               PIC X(150).                CBANKSTX
005400     03 DCL-BTX-FILLER                 PIC X(27).                 CBANKSTX
005500                                                                  CBANKSTX
005600 01  DCLTXN-NULL.                                                 CBANKSTX
005700     03 DCL-BTX-ACCNO-NULL             PIC S9(4) COMP.            CBANKSTX
005800                                                                  CBANKSTX
