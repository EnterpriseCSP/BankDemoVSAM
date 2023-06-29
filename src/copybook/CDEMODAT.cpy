000100***************************************************************** CDEMODAT
000200*                                                               * CDEMODAT
000300*  Copyright(C) 1998-2002 Micro Focus. All Rights Reserved.     * CDEMODAT
000400*                                                               * CDEMODAT
000500***************************************************************** CDEMODAT
000600                                                                  CDEMODAT
000700***************************************************************** CDEMODAT
000800* CDEMODAT.CPY                                                  * CDEMODAT
000900*---------------------------------------------------------------* CDEMODAT
001000* Common data passed between the major components               * CDEMODAT
001100***************************************************************** CDEMODAT
001200   05  DEMO-EVERYTHING                       PIC X(4096).         CDEMODAT
001300   05  FILLER REDEFINES DEMO-EVERYTHING.                          CDEMODAT
001400     10  DEMO-PREFIX                         PIC X(22).           CDEMODAT
001500     10  DEMO-IMS-PREFIX REDEFINES DEMO-PREFIX.                   CDEMODAT
001600       15  DEMO-IMS-SPA-LL                   PIC S9(8) COMP.      CDEMODAT
001700       15  DEMO-IMS-SPA-ZZ                   PIC X(2).            CDEMODAT
001800       15  DEMO-IMS-SPA-TRANCODE             PIC X(8).            CDEMODAT
001900       15  DEMO-IMS-SPA-PASSED-DATA          PIC X(8).            CDEMODAT
002000       15  DEMO-IMS-SPA-PASSED-DATA-R1 REDEFINES                  CDEMODAT
002100           DEMO-IMS-SPA-PASSED-DATA.                              CDEMODAT
002200         20  DEMO-IMS-IO-PCB-DATE            PIC S9(7) COMP-3.    CDEMODAT
002300         20  DEMO-IMS-IO-PCB-TIME            PIC S9(7) COMP-3.    CDEMODAT
002400       15  DEMO-IMS-SPA-PASSED-DATA-R2 REDEFINES                  CDEMODAT
002500           DEMO-IMS-SPA-PASSED-DATA.                              CDEMODAT
002600         20  DEMO-IMS-PASSED-LITERAL         PIC X(7).            CDEMODAT
002700         20  DEMO-IMS-PASSED-COLOUR          PIC X(1).            CDEMODAT
002800     10  DEMO-CICS-PREFIX REDEFINES DEMO-PREFIX.                  CDEMODAT
002900       15  DEMO-CICS-LL                      PIC S9(4) COMP.      CDEMODAT
003000       15  FILLER                            PIC X(4).            CDEMODAT
003100       15  DEMO-CICS-TRANCODE-PLUS4          PIC X(8).            CDEMODAT
003200       15  FILLER REDEFINES DEMO-CICS-TRANCODE-PLUS4.             CDEMODAT
003300         20  DEMO-CICS-TRANCODE              PIC X(4).            CDEMODAT
003400         20  FILLER                          PIC X(4).            CDEMODAT
003500     10  DEMO-CONTROL-FIELDS.                                     CDEMODAT
003600       15  DEMO-ENV                          PIC X(4).            CDEMODAT
003700         88  DEMO-ENV-NULL                   VALUE LOW-VALUES.    CDEMODAT
003800         88  DEMO-ENV-CICS                   VALUE 'CICS'.        CDEMODAT
003900         88  DEMO-ENV-IMS                    VALUE 'IMS '.        CDEMODAT
004000         88  DEMO-ENV-INET                   VALUE 'INET'.        CDEMODAT
004100       15  DEMO-COLOUR-SETTING               PIC X(1).            CDEMODAT
004200         88  COLOUR-ON                       VALUE '1'.           CDEMODAT
004300         88  COLOUR-OFF                      VALUE '0'.           CDEMODAT
004400       15  DEMO-CONVERSATION                 PIC X(1).            CDEMODAT
004500         88  DEMO-NO-CONV-IN-PROGRESS        VALUE '0'.           CDEMODAT
004600         88  DEMO-CONV-IN-PROGRESS           VALUE '1'.           CDEMODAT
004700       15  DEMO-TS-QUEUE-NAME                PIC X(8).            CDEMODAT
004800       15  DEMO-AID                          PIC X(5).            CDEMODAT
004900         88  DEMO-AID-ENTER                  VALUE 'ENTER'.       CDEMODAT
005000         88  DEMO-AID-CLEAR                  VALUE 'CLEAR'.       CDEMODAT
005100         88  DEMO-AID-PA1                    VALUE 'PA1  '.       CDEMODAT
005200         88  DEMO-AID-PA2                    VALUE 'PA2  '.       CDEMODAT
005300         88  DEMO-AID-PFK01                  VALUE 'PFK01'.       CDEMODAT
005400         88  DEMO-AID-PFK02                  VALUE 'PFK02'.       CDEMODAT
005500         88  DEMO-AID-PFK03                  VALUE 'PFK03'.       CDEMODAT
005600         88  DEMO-AID-PFK04                  VALUE 'PFK04'.       CDEMODAT
005700         88  DEMO-AID-PFK05                  VALUE 'PFK05'.       CDEMODAT
005800         88  DEMO-AID-PFK06                  VALUE 'PFK06'.       CDEMODAT
005900         88  DEMO-AID-PFK07                  VALUE 'PFK07'.       CDEMODAT
006000         88  DEMO-AID-PFK08                  VALUE 'PFK08'.       CDEMODAT
006100         88  DEMO-AID-PFK09                  VALUE 'PFK09'.       CDEMODAT
006200         88  DEMO-AID-PFK10                  VALUE 'PFK10'.       CDEMODAT
006300         88  DEMO-AID-PFK11                  VALUE 'PFK11'.       CDEMODAT
006400         88  DEMO-AID-PFK12                  VALUE 'PFK12'.       CDEMODAT
006500       15  DEMO-LAST-PROG                    PIC X(8).            CDEMODAT
006600       15  DEMO-NEXT-PROG                    PIC X(8).            CDEMODAT
006700       15  DEMO-RETURN-TO-PROG               PIC X(8).            CDEMODAT
006800       15  DEMO-LAST-MAPSET                  PIC X(7).            CDEMODAT
006900       15  DEMO-LAST-MAP                     PIC X(7).            CDEMODAT
007000       15  DEMO-NEXT-MAPSET                  PIC X(7).            CDEMODAT
007100       15  DEMO-NEXT-MAP                     PIC X(7).            CDEMODAT
007200       15  DEMO-MAP-FUNCTION                 PIC X(3).            CDEMODAT
007300         88  DEMO-MAP-FUNCTION-GET           VALUE 'GET'.         CDEMODAT
007400         88  DEMO-MAP-FUNCTION-PUT           VALUE 'PUT'.         CDEMODAT
007500       15  DEMO-HELP-FIELDS.                                      CDEMODAT
007600         20  DEMO-HELP-FLAG                  PIC X(4).            CDEMODAT
007700           88  DEMO-HELP-ACTIVE              VALUE 'HELP'.        CDEMODAT
007800           88  DEMO-HELP-INACTIVE            VALUE LOW-VALUES.    CDEMODAT
007900         20  DEMO-HELP-SCREEN                PIC 9(2).            CDEMODAT
008000       15  DEMO-PAGING-FIELDS.                                    CDEMODAT
008100         20  DEMO-PAGING-STATUS              PIC X(1).            CDEMODAT
008200           88  DEMO-PAGING-OFF               VALUE LOW-VALUES.    CDEMODAT
008300           88  DEMO-PAGING-FIRST             VALUE '1'.           CDEMODAT
008400           88  DEMO-PAGING-MIDDLE            VALUE '2'.           CDEMODAT
008500           88  DEMO-PAGING-LAST              VALUE '3'.           CDEMODAT
008600         20  DEMO-PAGING-FIRST-ENTRY         PIC X(25).           CDEMODAT
008700         20  DEMO-PAGING-LAST-ENTRY          PIC X(25).           CDEMODAT
008800       15  DEMO-RETURN-FLAG                  PIC X(1).            CDEMODAT
008900         88  DEMO-RETURN-FLAG-OFF            VALUE LOW-VALUES.    CDEMODAT
009000         88  DEMO-RETURN-FLAG-ON             VALUE '1'.           CDEMODAT
009100       15  DEMO-NEXT-TRAN                    PIC X(4).            CDEMODAT
009200     10  DEMO-ERROR-MSG                      PIC X(75).           CDEMODAT
009300     10  DEMO-HELP-DATA.                                          CDEMODAT
009400       15  DEMO-HELP-SCRN                    PIC X(6).            CDEMODAT
009500       15  DEMO-HELP-LINE                    PIC X(75)            CDEMODAT
009600           OCCURS 19 TIMES.                                       CDEMODAT
009700     10  DEMO-SCREEN-DATA.                                        CDEMODAT
009800       15  DEMO-SCREEN10-DATA.                                    CDEMODAT
009900         20  DEMO-SCR10-SEL1                 PIC X(1).            CDEMODAT
010000         20  DEMO-SCR10-SEL2                 PIC X(1).            CDEMODAT
010100         20  DEMO-SCR10-SEL3                 PIC X(1).            CDEMODAT
010200         20  DEMO-SCR10-SEL4                 PIC X(1).            CDEMODAT
010300                                                                  CDEMODAT
