000100***************************************************************** cdated
000200*                                                               * cdated
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cdated
000400*                                                               * cdated
000500***************************************************************** cdated
000600                                                                  cdated
000700***************************************************************** cdated
000800* CDATED.CPY                                                    * cdated
000900*---------------------------------------------------------------* cdated
001000* Area used to pass data to/from date conversion routine        * cdated
001100***************************************************************** cdated
001200   05  DD-AREAS.                                                  cdated
001300     10  DD-ENV                              PIC X(4).            cdated
001400       88  DD-ENV-NULL                       VALUE LOW-VALUES.    cdated
001500       88  DD-ENV-CICS                       VALUE 'CICS'.        cdated
001600       88  DD-ENV-IMS                        VALUE 'IMS'.         cdated
001700       88  DD-ENV-INET                       VALUE 'INET'.        cdated
001800     10  DD-TIME-AREAS.                                           cdated
001900       15  DD-TIME-INPUT                     PIC X(7).            cdated
002000       15  DD-TIME-INPUT-N REDEFINES DD-TIME-INPUT                cdated
002100                                             PIC 9(7).            cdated
002200       15  DD-TIME-OUTPUT.                                        cdated
002300         20  DD-TIME-OUTPUT-HH               PIC X(2).            cdated
002400         20  DD-TIME-OUTPUT-SEP1             PIC X(1).            cdated
002500         20  DD-TIME-OUTPUT-MM               PIC X(2).            cdated
002600         20  DD-TIME-OUTPUT-SEP2             PIC X(1).            cdated
002700         20  DD-TIME-OUTPUT-SS               PIC X(2).            cdated
002800     10  DD-DATE-AREAS.                                           cdated
002900       15  DD-INPUT.                                              cdated
003000         20  DDI-TYPE                        PIC X(1).            cdated
003100           88  DDI-ISO                       VALUE '0'.           cdated
003200           88  DDI-YYYYMMDD                  VALUE '1'.           cdated
003300           88  DDI-YYMMDD                    VALUE '2'.           cdated
003400           88  DDI-YYDDD                     VALUE '3'.           cdated
003500         20  DDI-DATA                        PIC X(20).           cdated
003600         20  DDI-DATA-ISO REDEFINES DDI-DATA.                     cdated
003700           25  DDI-DATA-ISO-YYYY             PIC X(4).            cdated
003800           25  DDI-DATA-ISO-YYYY-N REDEFINES                      cdated
003900               DDI-DATA-ISO-YYYY             PIC 9(4).            cdated
004000           25  DDI-DATA-ISO-DASH1            PIC X(1).            cdated
004100           25  DDI-DATA-ISO-MM               PIC X(2).            cdated
004200           25  DDI-DATA-ISO-MM-N REDEFINES                        cdated
004300               DDI-DATA-ISO-MM               PIC 9(2).            cdated
004400           25  DDI-DATA-ISO-DASH2            PIC X(1).            cdated
004500           25  DDI-DATA-ISO-DD               PIC X(2).            cdated
004600           25  DDI-DATA-ISO-DD-N REDEFINES                        cdated
004700               DDI-DATA-ISO-DD               PIC 9(2).            cdated
004800         20  DDI-DATA-YYYYMMDD REDEFINES DDI-DATA.                cdated
004900           25  DDI-DATA-YYYYMMDD-YYYY        PIC X(4).            cdated
005000           25  DDI-DATA-YYYYMMDD-YYYY-N REDEFINES                 cdated
005100               DDI-DATA-YYYYMMDD-YYYY        PIC 9(4).            cdated
005200           25  DDI-DATA-YYYYMMDD-MM          PIC X(2).            cdated
005300           25  DDI-DATA-YYYYMMDD-MM-N REDEFINES                   cdated
005400               DDI-DATA-YYYYMMDD-MM          PIC 9(2).            cdated
005500           25  DDI-DATA-YYYYMMDD-DD          PIC X(2).            cdated
005600           25  DDI-DATA-YYYYMMDD-DD-N REDEFINES                   cdated
005700               DDI-DATA-YYYYMMDD-DD          PIC 9(2).            cdated
005800         20  DDI-DATA-YYMMDD REDEFINES DDI-DATA.                  cdated
005900           25  DDI-DATA-YYMMDD-YY            PIC X(2).            cdated
006000           25  DDI-DATA-YYMMDD-YY-N REDEFINES                     cdated
006100               DDI-DATA-YYMMDD-YY            PIC 9(2).            cdated
006200           25  DDI-DATA-YYMMDD-MM            PIC X(2).            cdated
006300           25  DDI-DATA-YYMMDD-MM-N REDEFINES                     cdated
006400               DDI-DATA-YYMMDD-MM            PIC 9(2).            cdated
006500           25  DDI-DATA-YYMMDD-DD            PIC X(2).            cdated
006600           25  DDI-DATA-YYMMDD-DD-N REDEFINES                     cdated
006700               DDI-DATA-YYMMDD-DD            PIC 9(2).            cdated
006800         20  DDI-DATA-YYDDD REDEFINES DDI-DATA.                   cdated
006900           25  DDI-DATA-YYDDD-YYDDD          PIC X(5).            cdated
007000           25  DDI-DATA-YYDDD-YYDDD-N REDEFINES                   cdated
007100               DDI-DATA-YYDDD-YYDDD          PIC 9(5).            cdated
007200           25  DDI-DATA-YYDDD-YYDDD-SPLIT REDEFINES               cdated
007300               DDI-DATA-YYDDD-YYDDD.                              cdated
007400             30  DDI-DATA-YYDDD-YY           PIC X(2).            cdated
007500             30  DDI-DATA-YYDDD-YY-N REDEFINES                    cdated
007600                 DDI-DATA-YYDDD-YY           PIC 9(2).            cdated
007700             30  DDI-DATA-YYDDD-DDD          PIC X(3).            cdated
007800             30  DDI-DATA-YYDDD-DDD-N REDEFINES                   cdated
007900                 DDI-DATA-YYDDD-DDD          PIC 9(3).            cdated
008000                                                                  cdated
008100       15  DD-OUTPUT.                                             cdated
008200         20  DDO-TYPE                        PIC X(1).            cdated
008300           88  DDO-DD-MMM-YY                 VALUE '1'.           cdated
008400           88  DDO-DD-MMM-YYYY               VALUE '2'.           cdated
008500         20  DDO-DATA                        PIC X(20).           cdated
008600         20  DDO-DATA-DD-MMM-YY REDEFINES DDO-DATA.               cdated
008700           25  DDO-DATA-DD-MMM-YY-DD         PIC X(2).            cdated
008800           25  DDO-DATA-DD-MMM-YY-DOT1       PIC X(1).            cdated
008900           25  DDO-DATA-DD-MMM-YY-MMM        PIC X(3).            cdated
009000           25  DDO-DATA-DD-MMM-YY-DOT2       PIC X(1).            cdated
009100           25  DDO-DATA-DD-MMM-YY-YY         PIC X(2).            cdated
009200         20  DDO-DATA-DD-MMM-YYYY REDEFINES DDO-DATA.             cdated
009300           25  DDO-DATA-DD-MMM-YYYY-DD       PIC X(2).            cdated
009400           25  DDO-DATA-DD-MMM-YYYY-DOT1     PIC X(1).            cdated
009500           25  DDO-DATA-DD-MMM-YYYY-MMM      PIC X(3).            cdated
009600           25  DDO-DATA-DD-MMM-YYYY-DOT2     PIC X(1).            cdated
009700           25  DDO-DATA-DD-MMM-YYYY-YYYY     PIC X(4).            cdated
009800                                                                  cdated
009900* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cdated
