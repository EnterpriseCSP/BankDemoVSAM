000100***************************************************************** cstatesd
000200*                                                               * cstatesd
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cstatesd
000400*                                                               * cstatesd
000500***************************************************************** cstatesd
000600                                                                  cstatesd
000700***************************************************************** cstatesd
000800* CSTATESD.CPY                                                  * cstatesd
000900*---------------------------------------------------------------* cstatesd
001000* Look-up table of countr, state/provence codes & long form     * cstatesd
001100***************************************************************** cstatesd
001200 01  STATE-PROV-DATA-AREAS.                                       cstatesd
001300   05  STATE-PROV-TABLE.                                          cstatesd
001400     10  FILLER                              PIC X(28)            cstatesd
001500         VALUE 'USA AK Alaska               '.                    cstatesd
001600     10  FILLER                              PIC X(28)            cstatesd
001700         VALUE 'USA AL Alabama              '.                    cstatesd
001800     10  FILLER                              PIC X(28)            cstatesd
001900         VALUE 'USA AR Arkansas             '.                    cstatesd
002000     10  FILLER                              PIC X(28)            cstatesd
002100         VALUE 'USA AZ Arizona              '.                    cstatesd
002200     10  FILLER                              PIC X(28)            cstatesd
002300         VALUE 'USA CA California           '.                    cstatesd
002400     10  FILLER                              PIC X(28)            cstatesd
002500         VALUE 'USA CO Colorado             '.                    cstatesd
002600     10  FILLER                              PIC X(28)            cstatesd
002700         VALUE 'USA CT Connecticut          '.                    cstatesd
002800     10  FILLER                              PIC X(28)            cstatesd
002900         VALUE 'USA DC Washington D.C.      '.                    cstatesd
003000     10  FILLER                              PIC X(28)            cstatesd
003100         VALUE 'USA DE Delaware             '.                    cstatesd
003200     10  FILLER                              PIC X(28)            cstatesd
003300         VALUE 'USA FL Florida              '.                    cstatesd
003400     10  FILLER                              PIC X(28)            cstatesd
003500         VALUE 'USA GA Georgia              '.                    cstatesd
003600     10  FILLER                              PIC X(28)            cstatesd
003700         VALUE 'USA HI Hawaii               '.                    cstatesd
003800     10  FILLER                              PIC X(28)            cstatesd
003900         VALUE 'USA ID Idaho                '.                    cstatesd
004000     10  FILLER                              PIC X(28)            cstatesd
004100         VALUE 'USA IL Illinois             '.                    cstatesd
004200     10  FILLER                              PIC X(28)            cstatesd
004300         VALUE 'USA IN Indiana              '.                    cstatesd
004400     10  FILLER                              PIC X(28)            cstatesd
004500         VALUE 'USA IA Iowa                 '.                    cstatesd
004600     10  FILLER                              PIC X(28)            cstatesd
004700         VALUE 'USA KS Kansas               '.                    cstatesd
004800     10  FILLER                              PIC X(28)            cstatesd
004900         VALUE 'USA KY Kentucky             '.                    cstatesd
005000     10  FILLER                              PIC X(28)            cstatesd
005100         VALUE 'USA LA Louisiana            '.                    cstatesd
005200     10  FILLER                              PIC X(28)            cstatesd
005300         VALUE 'USA MA Massachusetts        '.                    cstatesd
005400     10  FILLER                              PIC X(28)            cstatesd
005500         VALUE 'USA MD Maryland             '.                    cstatesd
005600     10  FILLER                              PIC X(28)            cstatesd
005700         VALUE 'USA ME Maine                '.                    cstatesd
005800     10  FILLER                              PIC X(28)            cstatesd
005900         VALUE 'USA MI Michigan             '.                    cstatesd
006000     10  FILLER                              PIC X(28)            cstatesd
006100         VALUE 'USA MN Minnesota            '.                    cstatesd
006200     10  FILLER                              PIC X(28)            cstatesd
006300         VALUE 'USA MO Missouri             '.                    cstatesd
006400     10  FILLER                              PIC X(28)            cstatesd
006500         VALUE 'USA MS Mississippi          '.                    cstatesd
006600     10  FILLER                              PIC X(28)            cstatesd
006700         VALUE 'USA MT Montana              '.                    cstatesd
006800     10  FILLER                              PIC X(28)            cstatesd
006900         VALUE 'USA NC North Carolina       '.                    cstatesd
007000     10  FILLER                              PIC X(28)            cstatesd
007100         VALUE 'USA ND North Dakota         '.                    cstatesd
007200     10  FILLER                              PIC X(28)            cstatesd
007300         VALUE 'USA NE Nebraska             '.                    cstatesd
007400     10  FILLER                              PIC X(28)            cstatesd
007500         VALUE 'USA NH New Hampshire        '.                    cstatesd
007600     10  FILLER                              PIC X(28)            cstatesd
007700         VALUE 'USA NJ New Jersey           '.                    cstatesd
007800     10  FILLER                              PIC X(28)            cstatesd
007900         VALUE 'USA NM New Mexico           '.                    cstatesd
008000     10  FILLER                              PIC X(28)            cstatesd
008100         VALUE 'USA NV Nevada               '.                    cstatesd
008200     10  FILLER                              PIC X(28)            cstatesd
008300         VALUE 'USA NY New York             '.                    cstatesd
008400     10  FILLER                              PIC X(28)            cstatesd
008500         VALUE 'USA OH Ohio                 '.                    cstatesd
008600     10  FILLER                              PIC X(28)            cstatesd
008700         VALUE 'USA OK Oklahoma             '.                    cstatesd
008800     10  FILLER                              PIC X(28)            cstatesd
008900         VALUE 'USA OR Oregon               '.                    cstatesd
009000     10  FILLER                              PIC X(28)            cstatesd
009100         VALUE 'USA PA Pennsylvania         '.                    cstatesd
009200     10  FILLER                              PIC X(28)            cstatesd
009300         VALUE 'USA RI Rhode Island         '.                    cstatesd
009400     10  FILLER                              PIC X(28)            cstatesd
009500         VALUE 'USA SC South Carolina       '.                    cstatesd
009600     10  FILLER                              PIC X(28)            cstatesd
009700         VALUE 'USA SD South Dakota         '.                    cstatesd
009800     10  FILLER                              PIC X(28)            cstatesd
009900         VALUE 'USA TN Tennessee            '.                    cstatesd
010000     10  FILLER                              PIC X(28)            cstatesd
010100         VALUE 'USA TX Texas                '.                    cstatesd
010200     10  FILLER                              PIC X(28)            cstatesd
010300         VALUE 'USA UT Utah                 '.                    cstatesd
010400     10  FILLER                              PIC X(28)            cstatesd
010500         VALUE 'USA VA Virginia             '.                    cstatesd
010600     10  FILLER                              PIC X(28)            cstatesd
010700         VALUE 'USA VT Vermont              '.                    cstatesd
010800     10  FILLER                              PIC X(28)            cstatesd
010900         VALUE 'USA WA Washington           '.                    cstatesd
011000     10  FILLER                              PIC X(28)            cstatesd
011100         VALUE 'USA WI Wisconsin            '.                    cstatesd
011200     10  FILLER                              PIC X(28)            cstatesd
011300         VALUE 'USA WV West Virginia        '.                    cstatesd
011400     10  FILLER                              PIC X(28)            cstatesd
011500         VALUE 'USA WY Wyoming              '.                    cstatesd
011600     10  FILLER                              PIC X(28)            cstatesd
011700         VALUE 'USA PR Puerto Rico          '.                    cstatesd
011800     10  FILLER                              PIC X(28)            cstatesd
011900         VALUE 'CDN AB Alberta              '.                    cstatesd
012000     10  FILLER                              PIC X(28)            cstatesd
012100         VALUE 'CDN BC British Columbia     '.                    cstatesd
012200     10  FILLER                              PIC X(28)            cstatesd
012300         VALUE 'CDN MB Manitoba             '.                    cstatesd
012400     10  FILLER                              PIC X(28)            cstatesd
012500         VALUE 'CDN NB New Brunswick        '.                    cstatesd
012600     10  FILLER                              PIC X(28)            cstatesd
012700         VALUE 'CDN NF Newfoundland         '.                    cstatesd
012800     10  FILLER                              PIC X(28)            cstatesd
012900         VALUE 'CDN NS Nova Scotia          '.                    cstatesd
013000     10  FILLER                              PIC X(28)            cstatesd
013100         VALUE 'CDN NU Nunavut Territory    '.                    cstatesd
013200     10  FILLER                              PIC X(28)            cstatesd
013300         VALUE 'CDN NT Northwest Territories'.                    cstatesd
013400     10  FILLER                              PIC X(28)            cstatesd
013500         VALUE 'CDN ON Ontario              '.                    cstatesd
013600     10  FILLER                              PIC X(28)            cstatesd
013700         VALUE 'CDN PE Prince Edward Island '.                    cstatesd
013800     10  FILLER                              PIC X(28)            cstatesd
013900         VALUE 'CDN QC Quebec               '.                    cstatesd
014000     10  FILLER                              PIC X(28)            cstatesd
014100         VALUE 'CDN SK Saskatchewan         '.                    cstatesd
014200     10  FILLER                              PIC X(28)            cstatesd
014300         VALUE 'CDN YT Yukon Territory      '.                    cstatesd
014400   05  STATE-PROV-TABLE-R REDEFINES STATE-PROV-TABLE.             cstatesd
014500     10  STATE-PROV-DATA                     OCCURS 65 TIMES.     cstatesd
014600       15  STATE-PROV-CNTRY                  PIC X(3).            cstatesd
014700       15  FILLER                            PIC X(1).            cstatesd
014800       15  STATE-PROV-CODE                   PIC X(2).            cstatesd
014900       15  FILLER                            PIC X(1).            cstatesd
015000       15  STATE-PROV-NAME                   PIC X(21).           cstatesd
015100   05  STATE-PROV-COUNT                      PIC 9(2).            cstatesd
015200   05  STATE-PROV-SUB                        PIC 9(2).            cstatesd
015300   05  STATE-PROV-WK-CNTRY                   PIC X(3).            cstatesd
015400   05  STATE-PROV-TMP-CNTRY                  PIC X(6).            cstatesd
015500   05  STATE-PROV-WK-CODE                    PIC X(20).           cstatesd
015600   05  STATE-PROV-WK-NAME                    PIC X(20).           cstatesd
015700                                                                  cstatesd
015800* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cstatesd
