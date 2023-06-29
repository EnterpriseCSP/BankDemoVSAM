000100***************************************************************** cimsaib
000200*                                                               * cimsaib
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cimsaib
000400*                                                               * cimsaib
000500***************************************************************** cimsaib
000600                                                                  cimsaib
000700***************************************************************** cimsaib
000800* CDFSAIB.CPY                                                   * cimsaib
000900*---------------------------------------------------------------* cimsaib
001000* IMS AIB Mask                                                  * cimsaib
001100***************************************************************** cimsaib
001200 01  DFSAIB.                                                      cimsaib
001300   05  AIBID                                 PIC X(8).            cimsaib
001400   05  AIBLEN                                PIC S9(8) COMP.      cimsaib
001500   05  AIBSFUNC                              PIC X(8).            cimsaib
001600   05  AIBRSNM1                              PIC X(8).            cimsaib
001700   05  AIBRESVD1                             PIC X(16).           cimsaib
001800   05  AIBAOLEN                              PIC S9(8) COMP.      cimsaib
001900   05  AIBOAUSE                              PIC S9(8) COMP.      cimsaib
002000   05  AIBRESVD2                             PIC X(12).           cimsaib
002100   05  AIBRETRN                              PIC S9(8) COMP.      cimsaib
002200   05  AIBREASN                              PIC S9(8) COMP.      cimsaib
002300   05  AIBERRXT                              PIC X(4).            cimsaib
002400   05  AIBRSA1                               POINTER.             cimsaib
002500   05  AIBRESVD4                             PIC X(48).           cimsaib
002600                                                                  cimsaib
002700* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cimsaib
