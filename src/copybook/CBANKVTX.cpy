000100***************************************************************** cbankvtx
000200*                                                               * cbankvtx
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankvtx
000400*                                                               * cbankvtx
000500***************************************************************** cbankvtx
000600                                                                  cbankvtx
000700***************************************************************** cbankvtx
000800* CBANKVTX.CPY                                                  * cbankvtx
000900*---------------------------------------------------------------* cbankvtx
001000* This is the record file record layout for bank transactions   * cbankvtx
001100***************************************************************** cbankvtx
001200   05  BTX-RECORD                            PIC X(400).          cbankvtx
001300   05  FILLER REDEFINES BTX-RECORD.                               cbankvtx
001400     10  BTX-REC-PID                         PIC X(5).            cbankvtx
001500     10  BTX-REC-TYPE                        PIC X(1).            cbankvtx
001600     10  BTX-REC-SUB-TYPE                    PIC X(1).            cbankvtx
001700     10  BTX-REC-ALTKEY1.                                         cbankvtx
001800       15  BTX-REC-ACCNO                     PIC X(9).            cbankvtx
001900       15  BTX-REC-TIMESTAMP                 PIC X(26).           cbankvtx
002000     10  BTX-REC-TIMESTAMP-FF                PIC X(26).           cbankvtx
002100     10  BTX-REC-AMOUNT                      PIC S9(7)V99 COMP-3. cbankvtx
002200     10  BTX-REC-DATA-OLD                    PIC X(150).          cbankvtx
002300     10  BTX-REC-DATA-NEW                    PIC X(150).          cbankvtx
002400     10  BTX-REC-FILLER                      PIC X(27).           cbankvtx
002500                                                                  cbankvtx
002600* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankvtx
