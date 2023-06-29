000100***************************************************************** cbankvac
000200*                                                               * cbankvac
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankvac
000400*                                                               * cbankvac
000500***************************************************************** cbankvac
000600                                                                  cbankvac
000700***************************************************************** cbankvac
000800* CBANKVAC.CPY       ACS-GE                                     * cbankvac
000900*---------------------------------------------------------------* cbankvac
001000* This is the record file record layout for bank account        * cbankvac
001100***************************************************************** cbankvac
001200   05  BAC-RECORD                            PIC X(200).          cbankvac
001300   05  FILLER REDEFINES BAC-RECORD.                               cbankvac
001400     10  BAC-REC-PID                         PIC X(5).            cbankvac
001500     10  BAC-REC-ACCNO                       PIC X(9).            cbankvac
001600     10  BAC-REC-TYPE                        PIC X(1).            cbankvac
001700     10  BAC-REC-BALANCE                     PIC S9(7)V99 COMP-3. cbankvac
001800     10  BAC-REC-LAST-STMT-DTE               PIC X(10).           cbankvac
001900     10  BAC-REC-LAST-STMT-BAL               PIC S9(7)V99 COMP-3. cbankvac
002000     10  BAC-REC-ATM-ENABLED                 PIC X(2).            cbankvac
002100     10  BAC-REC-ATM-DAY-LIMIT               PIC S9(3)V COMP-3.   cbankvac
002200     10  BAC-REC-ATM-DAY-DTE                 PIC X(10).           cbankvac
002300     10  BAC-REC-ATM-DAY-AMT                 PIC S9(3)V COMP-3.   cbankvac
002400     10  BAC-REC-RP1-DAY                     PIC X(2).            cbankvac
002500     10  BAC-REC-RP1-AMOUNT                  PIC S9(5)V99 COMP-3. cbankvac
002600     10  BAC-REC-RP1-PID                     PIC X(5).            cbankvac
002700     10  BAC-REC-RP1-ACCNO                   PIC X(9).            cbankvac
002800     10  BAC-REC-RP1-LAST-PAY                PIC X(10).           cbankvac
002900     10  BAC-REC-RP2-DAY                     PIC X(2).            cbankvac
003000     10  BAC-REC-RP2-AMOUNT                  PIC S9(5)V99 COMP-3. cbankvac
003100     10  BAC-REC-RP2-PID                     PIC X(5).            cbankvac
003200     10  BAC-REC-RP2-ACCNO                   PIC X(9).            cbankvac
003300     10  BAC-REC-RP2-LAST-PAY                PIC X(10).           cbankvac
003400     10  BAC-REC-RP3-DAY                     PIC X(2).            cbankvac
003500     10  BAC-REC-RP3-AMOUNT                  PIC S9(5)V99 COMP-3. cbankvac
003600     10  BAC-REC-RP3-PID                     PIC X(5).            cbankvac
003700     10  BAC-REC-RP3-ACCNO                   PIC X(9).            cbankvac
003800     10  BAC-REC-RP3-LAST-PAY                PIC X(10).           cbankvac
003900     10  BAC-REC-FILLER                      PIC X(59).           cbankvac
004000                                                                  cbankvac
004100* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankvac
