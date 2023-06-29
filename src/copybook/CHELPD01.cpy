000100***************************************************************** chelpd01
000200*                                                               * chelpd01
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * chelpd01
000400*                                                               * chelpd01
000500***************************************************************** chelpd01
000600                                                                  chelpd01
000700***************************************************************** chelpd01
000800* CHELPD01.CPY                                                  * chelpd01
000900*---------------------------------------------------------------* chelpd01
001000* This area is used to pass data between a requesting program   * chelpd01
001100* and the I/O program (DHELP01P) which retrieves screen help    * chelpd01
001200* information.                                                  * chelpd01
001300***************************************************************** chelpd01
001400   05  HELP01-DATA.                                               chelpd01
001500     10  HELPO1I-DATA.                                            chelpd01
001600       15  HELP01I-SCRN                      PIC X(6).            chelpd01
001700     10  HELP01O-DATA.                                            chelpd01
001800       15  HELP01O-SCRN                      PIC X(6).            chelpd01
001900       15  HELP01O-FOUND                     PIC X(1).            chelpd01
002000         88 HELP-FOUND                       VALUE 'Y'.           chelpd01
002100         88 HELP-NOT-FOUND                   VALUE 'N'.           chelpd01
002200       15  HELP01O-INDIVIDUAL-LINES.                              chelpd01
002300         20  HELP01O-L01                     PIC X(75).           chelpd01
002400         20  HELP01O-L02                     PIC X(75).           chelpd01
002500         20  HELP01O-L03                     PIC X(75).           chelpd01
002600         20  HELP01O-L04                     PIC X(75).           chelpd01
002700         20  HELP01O-L05                     PIC X(75).           chelpd01
002800         20  HELP01O-L06                     PIC X(75).           chelpd01
002900         20  HELP01O-L07                     PIC X(75).           chelpd01
003000         20  HELP01O-L08                     PIC X(75).           chelpd01
003100         20  HELP01O-L09                     PIC X(75).           chelpd01
003200         20  HELP01O-L10                     PIC X(75).           chelpd01
003300         20  HELP01O-L11                     PIC X(75).           chelpd01
003400         20  HELP01O-L12                     PIC X(75).           chelpd01
003500         20  HELP01O-L13                     PIC X(75).           chelpd01
003600         20  HELP01O-L14                     PIC X(75).           chelpd01
003700         20  HELP01O-L15                     PIC X(75).           chelpd01
003800         20  HELP01O-L16                     PIC X(75).           chelpd01
003900         20  HELP01O-L17                     PIC X(75).           chelpd01
004000         20  HELP01O-L18                     PIC X(75).           chelpd01
004100         20  HELP01O-L19                     PIC X(75).           chelpd01
004200       15  FILLER REDEFINES HELP01O-INDIVIDUAL-LINES.             chelpd01
004300         20  HELP01O-LINE                    PIC X(75)            chelpd01
004400             OCCURS 19 TIMES.                                     chelpd01
004500                                                                  chelpd01
004600* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     chelpd01
