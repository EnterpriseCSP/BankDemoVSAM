000100***************************************************************** coptions
000200*                                                               * coptions
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * coptions
000400*                                                               * coptions
000500***************************************************************** coptions
000600                                                                  coptions
000700***************************************************************** coptions
000800* COPTIONS.CPY                                                  * coptions
000900*---------------------------------------------------------------* coptions
001000* Definitions of demo options                                   * coptions
001100***************************************************************** coptions
001200   05  DEMO-OPTIONS.                                              coptions
001300*  Demonstration menu option 1                                    coptions
001400     10  DEMO-OPTION01.                                           coptions
001500       15  DEMO-OPTION01-TRAN                PIC X(4)             coptions
001600           VALUE 'BANK'.                                          coptions
001700       15  DEMO-OPTION01-PROG                PIC X(8)             coptions
001800           VALUE 'SBANK00P'.                                      coptions
001900       15  DEMO-OPTION01-DESC.                                    coptions
002000         20  FILLER                          PIC X(35)            coptions
002100             VALUE 'Micro Focus Banking Application    '.         coptions
002200         20  FILLER                          PIC X(35)            coptions
002300             VALUE '                                   '.         coptions
002400*  Demonstration menu option 2                                    coptions
002500     10  DEMO-OPTION02.                                           coptions
002600       15  DEMO-OPTION02-TRAN                PIC X(4)             coptions
002700           VALUE 'INSC'.                                          coptions
002800       15  DEMO-OPTION02-PROG                PIC X(8)             coptions
002900           VALUE 'SINSC00P'.                                      coptions
003000       15  DEMO-OPTION02-DESC.                                    coptions
003100         20  FILLER                          PIC X(35)            coptions
003200             VALUE 'Micro Focus Insurance Application  '.         coptions
003300         20  FILLER                          PIC X(35)            coptions
003400             VALUE '                                   '.         coptions
003500*  Demonstration menu option 3                                    coptions
003600     10  DEMO-OPTION03.                                           coptions
003700       15  DEMO-OPTION03-TRAN                PIC X(4)             coptions
003800           VALUE 'PRDA'.                                          coptions
003900       15  DEMO-OPTION03-PROG                PIC X(8)             coptions
004000           VALUE 'SPRDA00P'.                                      coptions
004100       15  DEMO-OPTION03-DESC.                                    coptions
004200         20  FILLER                          PIC X(35)            coptions
004300             VALUE 'Information on Mainframe Developmen'.         coptions
004400          20 FILLER                          PIC X(35)            coptions
004500             VALUE 't Products                         '.         coptions
004600*  Demonstration menu option 4                                    coptions
004700     10  DEMO-OPTION04.                                           coptions
004800       15  DEMO-OPTION04-TRAN                PIC X(4)             coptions
004900           VALUE 'PRDB'.                                          coptions
005000       15  DEMO-OPTION04-PROG                PIC X(8)             coptions
005100           VALUE 'SPRDB00P'.                                      coptions
005200       15  DEMO-OPTION04-DESC.                                    coptions
005300         20  FILLER                          PIC X(35)            coptions
005400             VALUE 'Information on Windows and UNIX dis'.         coptions
005500         20  FILLER                          PIC X(35)            coptions
005600             VALUE 'tributed computing products        '.         coptions
005700   05  DEMO-OPTIONS-R REDEFINES DEMO-OPTIONS.                     coptions
005800     10  DEMO-OPTN                           OCCURS 4 TIMES.      coptions
005900       15  DEMO-OPTN-TRAN                    PIC X(4).            coptions
006000       15  DEMO-OPTN-PROG                    PIC X(8).            coptions
006100       15  DEMO-OPTN-DESC                    PIC X(70).           coptions
006200                                                                  coptions
006300* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     coptions
