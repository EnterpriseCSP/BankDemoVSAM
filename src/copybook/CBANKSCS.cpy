000100***************************************************************** CBANKSCS
000200*                                                               * CBANKSCS
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CBANKSCS
000400*                                                               * CBANKSCS
000500***************************************************************** CBANKSCS
000600                                                                  CBANKSCS
000700***************************************************************** CBANKSCS
000800* CBANKSCS.CPY                                                  * CBANKSCS
000900*---------------------------------------------------------------* CBANKSCS
001000* Define SQL areas to access CuStomer table                     * CBANKSCS
001100***************************************************************** CBANKSCS
001200     EXEC SQL DECLARE USERID.BNKCUST TABLE                        CBANKSCS
001300     (                                                            CBANKSCS
001400        BCS_PID                        CHAR (5)                   CBANKSCS
001500                                       NOT NULL,                  CBANKSCS
001600        BCS_NAME                       CHAR (25)                  CBANKSCS
001700                                       NOT NULL,                  CBANKSCS
001800        BCS_NAME_FF                    CHAR (25)                  CBANKSCS
001900                                       NOT NULL,                  CBANKSCS
002000        BCS_SIN                        CHAR (9)                   CBANKSCS
002100                                       NOT NULL                   CBANKSCS
002200                                       WITH DEFAULT,              CBANKSCS
002300        BCS_ADDR1                      CHAR (25)                  CBANKSCS
002400                                       NOT NULL                   CBANKSCS
002500                                       WITH DEFAULT,              CBANKSCS
002600        BCS_ADDR2                      CHAR (25)                  CBANKSCS
002700                                       NOT NULL                   CBANKSCS
002800                                       WITH DEFAULT,              CBANKSCS
002900        BCS_STATE                      CHAR (2)                   CBANKSCS
003000                                       NOT NULL                   CBANKSCS
003100                                       WITH DEFAULT,              CBANKSCS
003200        BCS_COUNTRY                    CHAR (6)                   CBANKSCS
003300                                       NOT NULL                   CBANKSCS
003400                                       WITH DEFAULT,              CBANKSCS
003500        BCS_POST_CODE                  CHAR (6)                   CBANKSCS
003600                                       NOT NULL                   CBANKSCS
003700                                       WITH DEFAULT,              CBANKSCS
003800        BCS_TEL                        CHAR (12)                  CBANKSCS
003900                                       NOT NULL                   CBANKSCS
004000                                       WITH DEFAULT,              CBANKSCS
004100        BCS_EMAIL                      CHAR (30)                  CBANKSCS
004200                                       NOT NULL                   CBANKSCS
004300                                       WITH DEFAULT,              CBANKSCS
004400        BCS_SEND_MAIL                  CHAR (1)                   CBANKSCS
004500                                       NOT NULL                   CBANKSCS
004600                                       WITH DEFAULT,              CBANKSCS
004700        BCS_SEND_EMAIL                 CHAR (1)                   CBANKSCS
004800                                       NOT NULL                   CBANKSCS
004900                                       WITH DEFAULT,              CBANKSCS
005000        BCS_FILLER                     CHAR (78)                  CBANKSCS
005100                                       NOT NULL                   CBANKSCS
005200                                       WITH DEFAULT               CBANKSCS
005300     )                                                            CBANKSCS
005400     END-EXEC.                                                    CBANKSCS
005500                                                                  CBANKSCS
005600 01  DCLCUST.                                                     CBANKSCS
005700     03 DCL-BCS-PID                    PIC X(5).                  CBANKSCS
005800     03 DCL-BCS-NAME                   PIC X(25).                 CBANKSCS
005900     03 DCL-BCS-NAME-FF                PIC X(25).                 CBANKSCS
006000     03 DCL-BCS-SIN                    PIC X(9).                  CBANKSCS
006100     03 DCL-BCS-ADDR1                  PIC X(25).                 CBANKSCS
006200     03 DCL-BCS-ADDR2                  PIC X(25).                 CBANKSCS
006300     03 DCL-BCS-STATE                  PIC X(2).                  CBANKSCS
006400     03 DCL-BCS-COUNTRY                PIC X(6).                  CBANKSCS
006500     03 DCL-BCS-POST-CODE              PIC X(6).                  CBANKSCS
006600     03 DCL-BCS-TEL                    PIC X(12).                 CBANKSCS
006700     03 DCL-BCS-EMAIL                  PIC X(30).                 CBANKSCS
006800     03 DCL-BCS-SEND-MAIL              PIC X(1).                  CBANKSCS
006900     03 DCL-BCS-SEND-EMAIL             PIC X(1).                  CBANKSCS
007000     03 DCL-BCS-FILLER                 PIC X(78).                 CBANKSCS
007100                                                                  CBANKSCS
