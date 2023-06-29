000100***************************************************************** cbankvat
000200*                                                               * cbankvat
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankvat
000400*                                                               * cbankvat
000500***************************************************************** cbankvat
000600                                                                  cbankvat
000700***************************************************************** cbankvat
000800* CBANKVAT.CPY                                                  * cbankvat
000900*---------------------------------------------------------------* cbankvat
001000* This is the record file record layout for bank account type   * cbankvat
001100***************************************************************** cbankvat
001200   05  BAT-RECORD                            PIC X(100).          cbankvat
001300   05  FILLER REDEFINES BAT-RECORD.                               cbankvat
001400     10  BAT-REC-TYPE                        PIC X(1).            cbankvat
001500     10  BAT-REC-DESC                        PIC X(15).           cbankvat
001600     10  BAT-REC-FILLER                      PIC X(84).           cbankvat
001700                                                                  cbankvat
001800* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankvat
