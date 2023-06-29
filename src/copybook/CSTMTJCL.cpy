000100***************************************************************** cstmtjcl
000200*                                                               * cstmtjcl
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cstmtjcl
000400*                                                               * cstmtjcl
000500***************************************************************** cstmtjcl
000600                                                                  cstmtjcl
000700***************************************************************** cstmtjcl
000800* CSTMTJCL.CPY                                                  * cstmtjcl
000900*---------------------------------------------------------------* cstmtjcl
001000* This is JCL to print statements from VSAM data                * cstmtjcl
001100***************************************************************** cstmtjcl
001200 01  WS-JCL-CARD-COUNT                       PIC 9(3).            cstmtjcl
001300 01  WS-JCL-CARDS.                                                cstmtjcl
001400   05  WS-JCL-CARD01                         PIC X(80)            cstmtjcl
001500       VALUE '//MFIDEMO  JOB MFIDEMO,MFIDEMO,CLASS=A,'.           cstmtjcl
001600   05  WS-JCL-CARD02                         PIC X(80)            cstmtjcl
001700       VALUE '//  MSGCLASS=A,MSGLEVEL=(1,1)          '.           cstmtjcl
001800   05  WS-JCL-CARD03                         PIC X(80)            cstmtjcl
001900       VALUE '//* USER=DUMMY,PASSWORD=DUMMY           '.          cstmtjcl
002000   05  WS-JCL-CARD04                         PIC X(80)            cstmtjcl
002100       VALUE '//* NOTIFY=DUMMY                        '.          cstmtjcl
002200   05  WS-JCL-CARD05                         PIC X(80)            cstmtjcl
002300       VALUE '//EXTRACT  EXEC YBNKEXTV,               '.          cstmtjcl
002400   05  WS-JCL-CARD-06                        PIC X(80)            cstmtjcl
002500       VALUE '//  REQUEST=%%%%%                       '.          cstmtjcl
002600   05  WS-JCL-CARD07                         PIC X(80)            cstmtjcl
002700       VALUE '//EXTRACT.SYSOUT DD DUMMY               '.          cstmtjcl
002800   05  WS-JCL-CARD08                         PIC X(80)            cstmtjcl
002900       VALUE '//SORT     EXEC YBNKSRT1,GEN=''+1''     '.          cstmtjcl
003000   05  WS-JCL-CARD09                         PIC X(80)            cstmtjcl
003100       VALUE '//SORT.SYSOUT DD DUMMY                  '.          cstmtjcl
003200   05  WS-JCL-CARD10                         PIC X(80)            cstmtjcl
003300       VALUE '//PRINT    EXEC YBNKPRT1,GEN=''+1''     '.          cstmtjcl
003400   05  WS-JCL-CARD11                         PIC X(80)            cstmtjcl
003500       VALUE '//                                      '.          cstmtjcl
003600   05  WS-JCL-CARD12                         PIC X(80)            cstmtjcl
003700       VALUE '/*EOF                                   '.          cstmtjcl
003800 01  WS-JCL-CARD-TABLE REDEFINES WS-JCL-CARDS.                    cstmtjcl
003900   05  WS-JCL-CARD                           PIC X(80)            cstmtjcl
004000       OCCURS 12 TIMES.                                           cstmtjcl
004100                                                                  cstmtjcl
004200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cstmtjcl
