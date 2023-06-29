000100***************************************************************** cbankd05
000200*                                                               * cbankd05
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankd05
000400*                                                               * cbankd05
000500***************************************************************** cbankd05
000600                                                                  cbankd05
000700***************************************************************** cbankd05
000800* CD05DATA.CPY       *                                          * cbankd05
000900*---------------------------------------------------------------* cbankd05
001000* This area is used to pass data between the transaction list   * cbankd05
001100* display program and the I/O program (DBANK05P) which          * cbankd05
001200* retrieves the data requested (a series of transactions for a  * cbankd05
001300* specified account).                                           * cbankd05
001400***************************************************************** cbankd05
001500   05  CD05-DATA.                                                 cbankd05
001600     10  CD05I-DATA.                                              cbankd05
001700       15  CD05I-ACC                         PIC X(9).            cbankd05
001800       15  CD05I-START-ID                    PIC X(26).           cbankd05
001900       15  CD05I-START-ID-R REDEFINES CD05I-START-ID.             cbankd05
002000         20  CD05I-START-DATE                PIC X(10).           cbankd05
002100         20  CD05I-START-FILL1               PIC X(1).            cbankd05
002200         20  CD05I-START-TIME                PIC X(8).            cbankd05
002300         20  CD05I-START-FILL2               PIC X(1).            cbankd05
002400         20  CD05I-START-MICROSECS           PIC X(6).            cbankd05
002500       15  CD05I-SEARCH-CRITERIA             PIC X(5).            cbankd05
002600         88  CD05-START-EQUAL                VALUE 'ENTER'.       cbankd05
002700         88  CD05-START-LOW                  VALUE 'PFK07'.       cbankd05
002800         88  CD05-START-HIGH                 VALUE 'PFK08'.       cbankd05
002900     10  CD05O-DATA.                                              cbankd05
003000       15  CD05-DATA-STATUS                  PIC X(1).            cbankd05
003100         88  CD05-NO-DATA                    VALUE '0'.           cbankd05
003200         88  CD05-IS-DATA                    VALUE '1'.           cbankd05
003300         88  CD05-IS-MORE-DATA               VALUE '2'.           cbankd05
003400         88  CD05-NO-MORE-DATA               VALUE '3'.           cbankd05
003500       15  CD05O-TXN-DETAILS.                                     cbankd05
003600         20  CD05O-TXN1.                                          cbankd05
003700           25  CD05O-ID1.                                         cbankd05
003800             30  CD05O-DAT1                  PIC X(10).           cbankd05
003900             30  CD05O-FIL1A                 PIC X(1).            cbankd05
004000             30  CD05O-TIM1                  PIC X(8).            cbankd05
004100             30  CD05O-FIL1B                 PIC X(1).            cbankd05
004200             30  CD05O-FIL1C                 PIC X(6).            cbankd05
004300           25  CD05O-AMT1                    PIC X(9).            cbankd05
004400           25  CD05O-AMT1-N REDEFINES CD05O-AMT1                  cbankd05
004500                                             PIC S9(7)V99.        cbankd05
004600           25  CD05O-DSC1                    PIC X(30).           cbankd05
004700         20  CD05O-TXN2.                                          cbankd05
004800           25  CD05O-ID2.                                         cbankd05
004900             30  CD05O-DAT2                  PIC X(10).           cbankd05
005000             30  CD05O-FIL2A                 PIC X(1).            cbankd05
005100             30  CD05O-TIM2                  PIC X(8).            cbankd05
005200             30  CD05O-FIL2B                 PIC X(1).            cbankd05
005300             30  CD05O-FIL2C                 PIC X(6).            cbankd05
005400           25  CD05O-AMT2                    PIC X(9).            cbankd05
005500           25  CD05O-AMT2-N REDEFINES CD05O-AMT2                  cbankd05
005600                                             PIC S9(7)V99.        cbankd05
005700           25  CD05O-DSC2                    PIC X(30).           cbankd05
005800         20  CD05O-TXN3.                                          cbankd05
005900           25  CD05O-ID3.                                         cbankd05
006000             30  CD05O-DAT3                  PIC X(10).           cbankd05
006100             30  CD05O-FIL3A                 PIC X(1).            cbankd05
006200             30  CD05O-TIM3                  PIC X(8).            cbankd05
006300             30  CD05O-FIL3B                 PIC X(1).            cbankd05
006400             30  CD05O-FIL3C                 PIC X(6).            cbankd05
006500           25  CD05O-AMT3                    PIC X(9).            cbankd05
006600           25  CD05O-AMT3-N REDEFINES CD05O-AMT3                  cbankd05
006700                                             PIC S9(7)V99.        cbankd05
006800           25  CD05O-DSC3                    PIC X(30).           cbankd05
006900         20  CD05O-TXN4.                                          cbankd05
007000           25  CD05O-ID4.                                         cbankd05
007100             30  CD05O-DAT4                  PIC X(10).           cbankd05
007200             30  CD05O-FIL4A                 PIC X(1).            cbankd05
007300             30  CD05O-TIM4                  PIC X(8).            cbankd05
007400             30  CD05O-FIL4B                 PIC X(1).            cbankd05
007500             30  CD05O-FIL4C                 PIC X(6).            cbankd05
007600           25  CD05O-AMT4                    PIC X(9).            cbankd05
007700           25  CD05O-AMT4-N REDEFINES CD05O-AMT4                  cbankd05
007800                                             PIC S9(7)V99.        cbankd05
007900           25  CD05O-DSC4                    PIC X(30).           cbankd05
008000         20  CD05O-TXN5.                                          cbankd05
008100           25  CD05O-ID5.                                         cbankd05
008200             30  CD05O-DAT5                  PIC X(10).           cbankd05
008300             30  CD05O-FIL5A                 PIC X(1).            cbankd05
008400             30  CD05O-TIM5                  PIC X(8).            cbankd05
008500             30  CD05O-FIL5B                 PIC X(1).            cbankd05
008600             30  CD05O-FIL5C                 PIC X(6).            cbankd05
008700           25  CD05O-AMT5                    PIC X(9).            cbankd05
008800           25  CD05O-AMT5-N REDEFINES CD05O-AMT5                  cbankd05
008900                                             PIC S9(7)V99.        cbankd05
009000           25  CD05O-DSC5                    PIC X(30).           cbankd05
009100         20  CD05O-TXN6.                                          cbankd05
009200           25  CD05O-ID6.                                         cbankd05
009300             30  CD05O-DAT6                  PIC X(10).           cbankd05
009400             30  CD05O-FIL6A                 PIC X(1).            cbankd05
009500             30  CD05O-TIM6                  PIC X(8).            cbankd05
009600             30  CD05O-FIL6B                 PIC X(1).            cbankd05
009700             30  CD05O-FIL6C                 PIC X(6).            cbankd05
009800           25  CD05O-AMT6                    PIC X(9).            cbankd05
009900           25  CD05O-AMT6-N REDEFINES CD05O-AMT6                  cbankd05
010000                                             PIC S9(7)V99.        cbankd05
010100           25  CD05O-DSC6                    PIC X(30).           cbankd05
010200         20  CD05O-TXN7.                                          cbankd05
010300           25  CD05O-ID7.                                         cbankd05
010400             30  CD05O-DAT7                  PIC X(10).           cbankd05
010500             30  CD05O-FIL7A                 PIC X(1).            cbankd05
010600             30  CD05O-TIM7                  PIC X(8).            cbankd05
010700             30  CD05O-FIL7B                 PIC X(1).            cbankd05
010800             30  CD05O-FIL7C                 PIC X(6).            cbankd05
010900           25  CD05O-AMT7                    PIC X(9).            cbankd05
011000           25  CD05O-AMT7-N REDEFINES CD05O-AMT7                  cbankd05
011100                                             PIC S9(7)V99.        cbankd05
011200           25  CD05O-DSC7                    PIC X(30).           cbankd05
011300         20  CD05O-TXN8.                                          cbankd05
011400           25  CD05O-ID8.                                         cbankd05
011500             30  CD05O-DAT8                  PIC X(10).           cbankd05
011600             30  CD05O-FIL8A                 PIC X(1).            cbankd05
011700             30  CD05O-TIM8                  PIC X(8).            cbankd05
011800             30  CD05O-FIL8B                 PIC X(1).            cbankd05
011900             30  CD05O-FIL8C                 PIC X(6).            cbankd05
012000           25  CD05O-AMT8                    PIC X(9).            cbankd05
012100           25  CD05O-AMT8-N REDEFINES CD05O-AMT8                  cbankd05
012200                                             PIC S9(7)V99.        cbankd05
012300           25  CD05O-DSC8                    PIC X(30).           cbankd05
012400         20  CD05O-TXN9.                                          cbankd05
012500           25  CD05O-ID9.                                         cbankd05
012600             30  CD05O-DAT9                  PIC X(10).           cbankd05
012700             30  CD05O-FIL9A                 PIC X(1).            cbankd05
012800             30  CD05O-TIM9                  PIC X(8).            cbankd05
012900             30  CD05O-FIL9B                 PIC X(1).            cbankd05
013000             30  CD05O-FIL9C                 PIC X(6).            cbankd05
013100           25  CD05O-AMT9                    PIC X(9).            cbankd05
013200           25  CD05O-AMT9-N REDEFINES CD05O-AMT9                  cbankd05
013300                                             PIC S9(7)V99.        cbankd05
013400           25  CD05O-DSC9                    PIC X(30).           cbankd05
013500       15  CD05O-TXN-DETAILS-R REDEFINES CD05O-TXN-DETAILS.       cbankd05
013600         20  CD05O-TXN-DATA OCCURS 9 TIMES.                       cbankd05
013700           25  CD05O-ID.                                          cbankd05
013800             30  CD05O-DATE                  PIC X(10).           cbankd05
013900             30  CD05O-FILLER1               PIC X(1).            cbankd05
014000             30  CD05O-TIME                  PIC X(8).            cbankd05
014100             30  CD05O-FILLER2               PIC X(1).            cbankd05
014200             30  CD05O-MICROSEC              PIC X(6).            cbankd05
014300           25  CD05O-AMT                     PIC X(9).            cbankd05
014400           25  CD05O-AMT-N REDEFINES CD05O-AMT                    cbankd05
014500                                             PIC S9(7)V99.        cbankd05
014600           25  CD05O-DESC                    PIC X(30).           cbankd05
014700                                                                  cbankd05
014800* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankd05
