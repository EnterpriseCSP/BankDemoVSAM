000100***************************************************************** cbanktxd
000200*                                                               * cbanktxd
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbanktxd
000400*                                                               * cbanktxd
000500***************************************************************** cbanktxd
000600                                                                  cbanktxd
000700***************************************************************** cbanktxd
000800* CBANKTXD.CPY                                                  * cbanktxd
000900*---------------------------------------------------------------* cbanktxd
001000* This area is used to pass data between a requesting program   * cbanktxd
001100* and the I/O program (DBANK01P) which retrieves the customer   * cbanktxd
001200* information.                                                  * cbanktxd
001300***************************************************************** cbanktxd
001400 01  TXN-DATA.                                                    cbanktxd
001500   05  TXN-TYPE                              PIC X(1).            cbanktxd
001600     88  TXN-TRANSFER-MONEY                  VALUE '1'.           cbanktxd
001700     88  TXN-CHANGE-CONTACT-INFO             VALUE '2'.           cbanktxd
001800   05  TXN-SUB-TYPE                          PIC X(1).            cbanktxd
001900     88  TXN-TRANSFER-MONEY-FROM             VALUE '1'.           cbanktxd
002000     88  TXN-TRANSFER-MONEY-TO               VALUE '2'.           cbanktxd
002100   05  TXN-DATA-OLD                          PIC X(150).          cbanktxd
002200   05  TXN-T1-OLD REDEFINES TXN-DATA-OLD.                         cbanktxd
002300     15  TXN-T1-OLD-DESC                     PIC X(30).           cbanktxd
002400   05  TXN-T2-OLD REDEFINES TXN-DATA-OLD.                         cbanktxd
002500     15  TXN-T2-OLD-ADDR1                    PIC X(25).           cbanktxd
002600     15  TXN-T2-OLD-ADDR2                    PIC X(25).           cbanktxd
002700     15  TXN-T2-OLD-STATE                    PIC X(2).            cbanktxd
002800     15  TXN-T2-OLD-CNTRY                    PIC X(6).            cbanktxd
002900     15  TXN-T2-OLD-PSTCDE                   PIC X(6).            cbanktxd
003000     15  TXN-T2-OLD-TELNO                    PIC X(12).           cbanktxd
003100     15  TXN-T2-OLD-EMAIL                    PIC X(30).           cbanktxd
003200     15  TXN-T2-OLD-SEND-MAIL                PIC X(1).            cbanktxd
003300     15  TXN-T2-OLD-SEND-EMAIL               PIC X(1).            cbanktxd
003400   05  TXN-DATA-NEW                          PIC X(150).          cbanktxd
003500   05  TXN-T2-NEW REDEFINES TXN-DATA-NEW.                         cbanktxd
003600     15  TXN-T2-NEW-ADDR1                    PIC X(25).           cbanktxd
003700     15  TXN-T2-NEW-ADDR2                    PIC X(25).           cbanktxd
003800     15  TXN-T2-NEW-STATE                    PIC X(2).            cbanktxd
003900     15  TXN-T2-NEW-CNTRY                    PIC X(6).            cbanktxd
004000     15  TXN-T2-NEW-PSTCDE                   PIC X(6).            cbanktxd
004100     15  TXN-T2-NEW-TELNO                    PIC X(12).           cbanktxd
004200     15  TXN-T2-NEW-EMAIL                    PIC X(30).           cbanktxd
004300     15  TXN-T2-NEW-SEND-MAIL                PIC X(1).            cbanktxd
004400     15  TXN-T2-NEW-SEND-EMAIL               PIC X(1).            cbanktxd
004500                                                                  cbanktxd
004600* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbanktxd
