000100***************************************************************** ccashext
000200*                                                               * ccashext
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * ccashext
000400*                                                               * ccashext
000500***************************************************************** ccashext
000600                                                                  ccashext
000700***************************************************************** ccashext
000800* CBANKEXT.CPY                                                  * ccashext
000900*---------------------------------------------------------------* ccashext
001000***************************************************************** ccashext
001100     10  ATM-DATA                            PIC X(256).          ccashext
001200                                                                  ccashext
001300     10  ATM-IP-DATA REDEFINES ATM-DATA.                          ccashext
001400       15  ATM-IP-COMMON-AREA.                                    ccashext
001500         20  ATM-IP00-FUNCTION               PIC X(2).            ccashext
001600           88  ATM-FUNC-VALID                VALUE '10'           ccashext
001700                                                   '20'.          ccashext
001800           88  ATM-FUNC-VALIDATE-PIN         VALUE '10'.          ccashext
001900           88  ATM-FUNC-GET-ACCOUNTS         VALUE '20'.          ccashext
002000         20  ATM-IP00-USERID                 PIC X(5).            ccashext
002100         20  ATM-IP00-PIN                    PIC X(4).            ccashext
002200       15  ATM-IP-SHARED-AREA                PIC X(128).          ccashext
002300       15  ATM-IP10-DATA REDEFINES ATM-IP-SHARED-AREA.            ccashext
002400         20  ATM-IP10-USERID                 PIC X(5).            ccashext
002500         20  ATM-IP10-PIN                    PIC X(4).            ccashext
002600       15  ATM-IP20-DATA REDEFINES ATM-IP-SHARED-AREA.            ccashext
002700         20  ATM-IP20-USERID                 PIC X(5).            ccashext
002800       15  ATM-IP30-DATA REDEFINES ATM-IP-SHARED-AREA.            ccashext
002900         20  ATM-IP30-DET1                   PIC X(1).            ccashext
003000         20  ATM-IP30-TXS1                   PIC X(1).            ccashext
003100       15  ATM-IP50-DATA REDEFINES ATM-IP-SHARED-AREA.            ccashext
003200         20  ATM-IP50-XFER                   PIC X(8).            ccashext
003300         20  ATM-IP50-FRM1                   PIC X(1).            ccashext
003400         20  ATM-IP50-TO1                    PIC X(1).            ccashext
003500       15  ATM-IP60-DATA REDEFINES ATM-IP-SHARED-AREA.            ccashext
003600         20  ATM-IP60-NADDR1                 PIC X(25).           ccashext
003700         20  ATM-IP60-NADDR2                 PIC X(25).           ccashext
003800         20  ATM-IP60-NSTATE                 PIC X(2).            ccashext
003900         20  ATM-IP60-NCNTRY                 PIC X(6).            ccashext
004000         20  ATM-IP60-NPSTCDE                PIC X(6).            ccashext
004100         20  ATM-IP60-NTELNO                 PIC X(12).           ccashext
004200         20  ATM-IP60-NEMAIL                 PIC X(30).           ccashext
004300         20  ATM-IP60-NSMAIL                 PIC X(1).            ccashext
004400         20  ATM-IP60-NSEMAIL                PIC X(1).            ccashext
004500       15  ATM-IP70-DATA REDEFINES ATM-IP-SHARED-AREA.            ccashext
004600         20  ATM-IP70-AMOUNT                 PIC X(7).            ccashext
004700         20  ATM-IP70-RATE                   PIC X(5).            ccashext
004800         20  ATM-IP70-TERM                   PIC X(5).            ccashext
004900       15  ATM-IP80-DATA REDEFINES ATM-IP-SHARED-AREA.            ccashext
005000         20  ATM-IP80-OPT1                   PIC X(1).            ccashext
005100         20  ATM-IP80-OPT2                   PIC X(1).            ccashext
005200                                                                  ccashext
005300     10  ATM-OP-DATA REDEFINES ATM-DATA.                          ccashext
005400       15  ATM-OP-ERR-MSG                    PIC X(80).           ccashext
005500       15  ATM-OP-USERID                     PIC X(5).            ccashext
005600       15  ATM-OP-PIN-STATUS                 PIC X(2).            ccashext
005700         88  ATM-PIN-VALID                   VALUE '10'.          ccashext
005800         88  ATM-PIN-UNKNOWN-USER            VALUE '11'.          ccashext
005900         88  ATM-PIN-INVALID                 VALUE '12'.          ccashext
006000         88  ATM-PIN-MISSING                 VALUE '13'.          ccashext
006100       15  ATM-OP-SHARED-AREA                PIC X(169).          ccashext
006200       15  ATM-OP20-DATA REDEFINES ATM-OP-SHARED-AREA.            ccashext
006300         20  ATM-OP20-ACC1                   PIC X(9).            ccashext
006400         20  ATM-OP20-DSC1                   PIC X(15).           ccashext
006500         20  ATM-OP20-ACC2                   PIC X(9).            ccashext
006600         20  ATM-OP20-DSC2                   PIC X(15).           ccashext
006700         20  ATM-OP20-ACC3                   PIC X(9).            ccashext
006800         20  ATM-OP20-DSC3                   PIC X(15).           ccashext
006900         20  ATM-OP20-ACC4                   PIC X(9).            ccashext
007000         20  ATM-OP20-DSC4                   PIC X(15).           ccashext
007100         20  ATM-OP20-ACC5                   PIC X(9).            ccashext
007200         20  ATM-OP20-DSC5                   PIC X(15).           ccashext
007300*      15  ATM-OP35-DATA REDEFINES ATM-OP-SHARED-AREA.            ccashext
007400*        20  ATM-OP35-ACCNO                  PIC X(9).            ccashext
007500*        20  ATM-OP35-ACCTYPE                PIC X(15).           ccashext
007600*        20  ATM-OP35-BALANCE                PIC X(13).           ccashext
007700*        20  ATM-OP35-STMT-DATE              PIC X(11).           ccashext
007800*        20  ATM-OP35-ATM-DETAILS.                                ccashext
007900*          25  ATM-OP35-ATM-VIS              PIC X(1).            ccashext
008000*          25  ATM-OP35-ATM-LIM              PIC X(3).            ccashext
008100*          25  ATM-OP35-ATM-LDTE             PIC X(11).           ccashext
008200*          25  ATM-OP35-ATM-LAMT             PIC X(3).            ccashext
008300*        20  ATM-OP35-RP-DETAILS             OCCURS 3 TIMES.      ccashext
008400*          25  ATM-OP35-RP-DAY               PIC X(2).            ccashext
008500*          25  ATM-OP35-RP-AMT               PIC X(8).            ccashext
008600*          25  ATM-OP35-RP-PID               PIC X(5).            ccashext
008700*          25  ATM-OP35-RP-ACC               PIC X(9).            ccashext
008800*          25  ATM-OP35-RP-DTE               PIC X(11).           ccashext
008900       15  ATM-OP50-DATA REDEFINES ATM-OP-SHARED-AREA.            ccashext
009000         20  ATM-OP50-XFER                   PIC X(9).            ccashext
009100         20  ATM-OP50-FRM1                   PIC X(1).            ccashext
009200         20  ATM-OP50-TO1                    PIC X(1).            ccashext
009300         20  ATM-OP50-ACC1                   PIC X(9).            ccashext
009400         20  ATM-OP50-DSC1                   PIC X(15).           ccashext
009500         20  ATM-OP50-FRM2                   PIC X(1).            ccashext
009600         20  ATM-OP50-TO2                    PIC X(1).            ccashext
009700         20  ATM-OP50-ACC2                   PIC X(9).            ccashext
009800         20  ATM-OP50-DSC2                   PIC X(15).           ccashext
009900*      15  ATM-OP60-DATA REDEFINES ATM-OP-SHARED-AREA.            ccashext
010000*        20  ATM-OP60-OADDR1                 PIC X(25).           ccashext
010100*        20  ATM-OP60-OADDR2                 PIC X(25).           ccashext
010200*        20  ATM-OP60-OSTATE                 PIC X(2).            ccashext
010300*        20  ATM-OP60-OCNTRY                 PIC X(6).            ccashext
010400*        20  ATM-OP60-OPSTCDE                PIC X(6).            ccashext
010500*        20  ATM-OP60-OTELNO                 PIC X(12).           ccashext
010600*        20  ATM-OP60-NADDR1                 PIC X(25).           ccashext
010700*        20  ATM-OP60-NADDR2                 PIC X(25).           ccashext
010800*        20  ATM-OP60-NSTATE                 PIC X(2).            ccashext
010900*        20  ATM-OP60-NCNTRY                 PIC X(6).            ccashext
011000*        20  ATM-OP60-NPSTCDE                PIC X(6).            ccashext
011100*        20  ATM-OP60-NTELNO                 PIC X(12).           ccashext
011200*        20  ATM-OP60-NEMAIL                 PIC X(30).           ccashext
011300*        20  ATM-OP60-NSMAIL                 PIC X(1).            ccashext
011400*        20  ATM-OP60-NSEMAIL                PIC X(1).            ccashext
011500       15  ATM-OP70-DATA REDEFINES ATM-OP-SHARED-AREA.            ccashext
011600         20  ATM-OP70-AMOUNT                 PIC X(7).            ccashext
011700         20  ATM-OP70-RATE                   PIC X(7).            ccashext
011800         20  ATM-OP70-TERM                   PIC X(5).            ccashext
011900         20  ATM-OP70-PAYMENT                PIC X(9).            ccashext
012000       15  ATM-OP80-DATA REDEFINES ATM-OP-SHARED-AREA.            ccashext
012100         20  ATM-OP80-ADDR1                 PIC X(25).            ccashext
012200         20  ATM-OP80-ADDR2                 PIC X(25).            ccashext
012300         20  ATM-OP80-STATE                 PIC X(2).             ccashext
012400         20  ATM-OP80-CNTRY                 PIC X(6).             ccashext
012500         20  ATM-OP80-PSTCDE                PIC X(6).             ccashext
012600         20  ATM-OP80-EMAIL                 PIC X(30).            ccashext
012700         20  ATM-OP80-OPT1                  PIC X(1).             ccashext
012800         20  ATM-OP80-OPT2                  PIC X(1).             ccashext
012900                                                                  ccashext
013000* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     ccashext
