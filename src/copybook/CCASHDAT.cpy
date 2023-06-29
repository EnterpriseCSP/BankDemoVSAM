000100***************************************************************** ccashdat
000200*                                                               * ccashdat
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * ccashdat
000400*                                                               * ccashdat
000500***************************************************************** ccashdat
000600*                                                                 ccashdat
000700***************************************************************** ccashdat
000800* CCASHDAT.CPY                                                  * ccashdat
000900*---------------------------------------------------------------* ccashdat
001000* Common data passed between the major components               * ccashdat
001100***************************************************************** ccashdat
001200*    10  CASH-EVERYTHING.                                         ccashdat
001300     10  CASH-EVERYTHING                     PIC X(1024).         ccashdat
001400     10  FILLER REDEFINES CASH-EVERYTHING.                        ccashdat
001500       15  CASH-REQUEST-CODE                 PIC X(1).            ccashdat
001600         88  CASH-REQUEST-DETAILS            VALUE '1'.           ccashdat
001700         88  CASH-REQUEST-XFER               VALUE '2'.           ccashdat
001800         88  CASH-REQUEST-CASH               VALUE '3'.           ccashdat
001900*      15  CASH-CONTROL-FIELDS.                                   ccashdat
002000*        20  CASH-ENV                        PIC X(4).            ccashdat
002100*          88  CASH-ENV-NULL                 VALUE LOW-VALUES.    ccashdat
002200*          88  CASH-ENV-CICS                 VALUE 'CICS'.        ccashdat
002300*          88  CASH-ENV-IMS                  VALUE 'IMS '.        ccashdat
002400*          88  CASH-ENV-INET                 VALUE 'INET'.        ccashdat
002500*        20  CASH-TS-QUEUE-NAME              PIC X(8).            ccashdat
002600*        20  CASH-RETURN-MSG                 PIC X(75).           ccashdat
002700*          88  CASH-RETURN-MSG-OFF           VALUE LOW-VALUES.    ccashdat
002800       15  CASH-USER-DETAILS.                                     ccashdat
002900*        20  CASH-SIGNON-ID                  PIC X(5).            ccashdat
003000*        20  FILLER REDEFINES CASH-SIGNON-ID.                     ccashdat
003100*          25  CASH-SIGNON-ID-1              PIC X(1).            ccashdat
003200*            88  PROBLEM-USER                VALUE 'Z'.           ccashdat
003300*          25  CASH-SIGNON-ID-2-5            PIC X(4).            ccashdat
003400         20  CASH-USERID                     PIC X(5).            ccashdat
003500           88  GUEST                         VALUE 'GUEST'.       ccashdat
003600         20  CASH-PIN                        PIC X(4).            ccashdat
003700         20  CASH-PIN-STATUS                 PIC X(2).            ccashdat
003800           88  CASH-PIN-STATUS-UNKNOWN       VALUE '  '.          ccashdat
003900           88  CASH-PIN-STATUS-OK            VALUE '10'.          ccashdat
004000           88  CASH-PIN-STATUS-NO-USER       VALUE '11'.          ccashdat
004100           88  CASH-PIN-STATUS-INVALID       VALUE '12'.          ccashdat
004200           88  CASH-PIN-STATUS-NO-PIN        VALUE '13'.          ccashdat
004300       15  CASH-ERROR-MSG                    PIC X(75).           ccashdat
004400       15  CASH-ATM-DATA.                                         ccashdat
004500         20  CASH-ATM1-DATA.                                      ccashdat
004600           25  CASH-ATM1-ACC-DET1.                                ccashdat
004700             30  CASH-ATM1-ACC1              PIC X(9).            ccashdat
004800             30  CASH-ATM1-DSC1              PIC X(15).           ccashdat
004900             30  CASH-ATM1-BAL1              PIC X(13).           ccashdat
005000             30  CASH-ATM1-DAY-LIMIT1        PIC X(3).            ccashdat
005100             30  CASH-ATM1-DATE-USED1        PIC X(10).           ccashdat
005200             30  CASH-ATM1-DATE-AMT1         PIC X(3).            ccashdat
005300           25  CASH-ATM1-ACC-DET2.                                ccashdat
005400             30  CASH-ATM1-ACC2              PIC X(9).            ccashdat
005500             30  CASH-ATM1-DSC2              PIC X(15).           ccashdat
005600             30  CASH-ATM1-BAL2              PIC X(13).           ccashdat
005700             30  CASH-ATM1-DAY-LIMIT2        PIC X(3).            ccashdat
005800             30  CASH-ATM1-DATE-USED2        PIC X(10).           ccashdat
005900             30  CASH-ATM1-DATE-AMT2         PIC X(3).            ccashdat
006000           25  CASH-ATM1-ACC-DET3.                                ccashdat
006100             30  CASH-ATM1-ACC3              PIC X(9).            ccashdat
006200             30  CASH-ATM1-DSC3              PIC X(15).           ccashdat
006300             30  CASH-ATM1-BAL3              PIC X(13).           ccashdat
006400             30  CASH-ATM1-DAY-LIMIT3        PIC X(3).            ccashdat
006500             30  CASH-ATM1-DATE-USED3        PIC X(10).           ccashdat
006600             30  CASH-ATM1-DATE-AMT3         PIC X(3).            ccashdat
006700           25  CASH-ATM1-ACC-DET4.                                ccashdat
006800             30  CASH-ATM1-ACC4              PIC X(9).            ccashdat
006900             30  CASH-ATM1-DSC4              PIC X(15).           ccashdat
007000             30  CASH-ATM1-BAL4              PIC X(13).           ccashdat
007100             30  CASH-ATM1-DAY-LIMIT4        PIC X(3).            ccashdat
007200             30  CASH-ATM1-DATE-USED4        PIC X(10).           ccashdat
007300             30  CASH-ATM1-DATE-AMT4         PIC X(3).            ccashdat
007400           25  CASH-ATM1-ACC-DET5.                                ccashdat
007500             30  CASH-ATM1-ACC5              PIC X(9).            ccashdat
007600             30  CASH-ATM1-DSC5              PIC X(15).           ccashdat
007700             30  CASH-ATM1-BAL5              PIC X(13).           ccashdat
007800             30  CASH-ATM1-DAY-LIMIT5        PIC X(3).            ccashdat
007900             30  CASH-ATM1-DATE-USED5        PIC X(10).           ccashdat
008000             30  CASH-ATM1-DATE-AMT5         PIC X(3).            ccashdat
008100         20  CASH-ATM1-DATA-R REDEFINES CASH-ATM1-DATA.           ccashdat
008200           25  CASH-ATM1-ACC-DET OCCURS 5 TIMES.                  ccashdat
008300             30  CASH-ATM1-ACC               PIC X(9).            ccashdat
008400             30  CASH-ATM1-DSC               PIC X(15).           ccashdat
008500             30  CASH-ATM1-BAL               PIC X(13).           ccashdat
008600             30  CASH-ATM1-DAY-LIMIT         PIC X(3).            ccashdat
008700             30  CASH-ATM1-DATE-USED         PIC X(10).           ccashdat
008800             30  CASH-ATM1-DATE-AMT          PIC X(3).            ccashdat
008900         20  CASH-ATM2-DATA-R REDEFINES CASH-ATM1-DATA.           ccashdat
009000           25  CASH-ATM2-XFER-AMT            PIC X(8).            ccashdat
009100           25  CASH-ATM2-FROM-ACC            PIC X(9).            ccashdat
009200           25  CASH-ATM2-FROM-BAL            PIC X(13).           ccashdat
009300           25  CASH-ATM2-TO-ACC              PIC X(9).            ccashdat
009400           25  CASH-ATM2-TO-BAL              PIC X(13).           ccashdat
009500         20  CASH-ATM3-DATA-R REDEFINES CASH-ATM1-DATA.           ccashdat
009600           25  CASH-ATM3-CASH-AMT            PIC X(8).            ccashdat
009700           25  CASH-ATM3-FROM-ACC            PIC X(9).            ccashdat
009800           25  CASH-ATM3-FROM-BAL            PIC X(13).           ccashdat
009900* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     ccashdat
