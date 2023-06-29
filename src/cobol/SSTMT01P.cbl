000100***************************************************************** SSTMT01P
000200*                                                               * SSTMT01P
000300*   Copyright (C) 1998-2008 Micro Focus. All Rights Reserved.   * SSTMT01P
000400*   This demonstration program is provided for use by users     * SSTMT01P
000500*   of Micro Focus products and may be used, modified and       * SSTMT01P
000600*   distributed as part of your application provided that       * SSTMT01P
000700*   you properly acknowledge the copyright of Micro Focus       * SSTMT01P
000800*   in this material.                                           * SSTMT01P
000900*                                                               * SSTMT01P
001000***************************************************************** SSTMT01P
001100***************************************************************** SSTMT01P
001200* Program:     SSTMT01P.CBL (CICS Version)                      * SSTMT01P
001300* Layer:       Transaction manager specific                     * SSTMT01P
001400* Function:    Create statement print request                   * SSTMT01P
001500***************************************************************** SSTMT01P
001600                                                                  SSTMT01P
001700 IDENTIFICATION DIVISION.                                         SSTMT01P
001800 PROGRAM-ID.                                                      SSTMT01P
001900     SSTMT01P.                                                    SSTMT01P
002000 DATE-WRITTEN.                                                    SSTMT01P
002100     September 2002.                                              SSTMT01P
002200 DATE-COMPILED.                                                   SSTMT01P
002300     Today.                                                       SSTMT01P
002400                                                                  SSTMT01P
002500 ENVIRONMENT DIVISION.                                            SSTMT01P
002600                                                                  SSTMT01P
002700 DATA DIVISION.                                                   SSTMT01P
002800                                                                  SSTMT01P
002900 WORKING-STORAGE SECTION.                                         SSTMT01P
003000 01  WS-MISC-STORAGE.                                             SSTMT01P
003100   05  WS-PROGRAM-ID                         PIC X(8)             SSTMT01P
003200       VALUE 'SSTMT01P'.                                          SSTMT01P
003300   05  WS-COMMAREA-LENGTH                    PIC 9(5).            SSTMT01P
003400   05  WS-RESP                               PIC S9(8) COMP.      SSTMT01P
003500                                                                  SSTMT01P
003600 01  WS-COMMAREA.                                                 SSTMT01P
003700 COPY CSTMTD01.                                                   SSTMT01P
003800                                                                  SSTMT01P
003900 01  WS-PRINT-MSG-AREA.                                           SSTMT01P
004000   05  FILLER                                PIC X(28)            SSTMT01P
004100       VALUE 'Accepted print request for: '.                      SSTMT01P
004200   05  WS-PRINT-MSG-UID                      PIC X(5).            SSTMT01P
004300   05  FILLER                                PIC X(10)            SSTMT01P
004400       VALUE '. Send by '.                                        SSTMT01P
004500   05  WS-PRINT-MSG-METHOD                   PIC X(13).           SSTMT01P
004600                                                                  SSTMT01P
004700 01  WS-INTRDR-QUEUE                         PIC X(4)             SSTMT01P
004800     VALUE 'IRDR'.                                                SSTMT01P
004900                                                                  SSTMT01P
005000 01  WS-PERFORM-COUNTER                      PIC 9(3).            SSTMT01P
005100                                                                  SSTMT01P
005200 COPY CSTMTJCL.                                                   SSTMT01P
005300                                                                  SSTMT01P
005400 COPY CABENDD.                                                    SSTMT01P
005500                                                                  SSTMT01P
005600 LINKAGE SECTION.                                                 SSTMT01P
005700 01  DFHCOMMAREA.                                                 SSTMT01P
005800   05  LK-COMMAREA                           PIC X(1)             SSTMT01P
005900       OCCURS 1 TO 4096 TIMES                                     SSTMT01P
006000         DEPENDING ON WS-COMMAREA-LENGTH.                         SSTMT01P
006100                                                                  SSTMT01P
006200 COPY CENTRY.                                                     SSTMT01P
006300***************************************************************** SSTMT01P
006400* Move the passed data to our area                              * SSTMT01P
006500***************************************************************** SSTMT01P
006600     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            SSTMT01P
006700     MOVE DFHCOMMAREA TO WS-COMMAREA.                             SSTMT01P
006800                                                                  SSTMT01P
006900***************************************************************** SSTMT01P
007000* Initialize our output area                                    * SSTMT01P
007100***************************************************************** SSTMT01P
007200     MOVE SPACES TO CSTMTD01O-DATA.                               SSTMT01P
007300                                                                  SSTMT01P
007400***************************************************************** SSTMT01P
007500* Set up message to go to log                                   * SSTMT01P
007600***************************************************************** SSTMT01P
007700     MOVE CSTMTD01I-CONTACT-ID TO WS-PRINT-MSG-UID.               SSTMT01P
007800     IF CSTMTD01I-POST                                            SSTMT01P
007900        MOVE Z'regular mail' TO WS-PRINT-MSG-METHOD               SSTMT01P
008000     END-IF.                                                      SSTMT01P
008100     IF CSTMTD01I-EMAIL                                           SSTMT01P
008200        MOVE Z'E-Mail' TO WS-PRINT-MSG-METHOD                     SSTMT01P
008300     END-IF.                                                      SSTMT01P
008400                                                                  SSTMT01P
008500***************************************************************** SSTMT01P
008600* Write the log message                                         * SSTMT01P
008700***************************************************************** SSTMT01P
008800     EXEC CICS WRITE                                              SSTMT01P
008900               OPERATOR                                           SSTMT01P
009000               TEXT(WS-PRINT-MSG-AREA)                            SSTMT01P
009100               TEXTLENGTH(LENGTH OF WS-PRINT-MSG-AREA)            SSTMT01P
009200     END-EXEC.                                                    SSTMT01P
009300                                                                  SSTMT01P
009400***************************************************************** SSTMT01P
009500* Set up the JCL to run the job                                 * SSTMT01P
009600***************************************************************** SSTMT01P
009700     INSPECT WS-JCL-CARD-TABLE                                    SSTMT01P
009800       REPLACING ALL '%%%%%' BY CSTMTD01I-CONTACT-ID.             SSTMT01P
009900                                                                  SSTMT01P
010000     DIVIDE LENGTH OF WS-JCL-CARD(1) INTO                         SSTMT01P
010100       LENGTH OF WS-JCL-CARD-TABLE GIVING WS-JCL-CARD-COUNT.      SSTMT01P
010200                                                                  SSTMT01P
010300***************************************************************** SSTMT01P
010400* Write the JCL to the internal reader TD queue                 * SSTMT01P
010500***************************************************************** SSTMT01P
010600     EXEC CICS ENQ                                                SSTMT01P
010700          RESOURCE(WS-INTRDR-QUEUE)                               SSTMT01P
010800          RESP(WS-RESP)                                           SSTMT01P
010900     END-EXEC.                                                    SSTMT01P
011000                                                                  SSTMT01P
011100     PERFORM VARYING WS-PERFORM-COUNTER FROM 1 BY 1               SSTMT01P
011200       UNTIL WS-PERFORM-COUNTER IS GREATER THAN WS-JCL-CARD-COUNT SSTMT01P
011300         EXEC CICS WRITEQ TD                                      SSTMT01P
011400              QUEUE(WS-INTRDR-QUEUE)                              SSTMT01P
011500              FROM(WS-JCL-CARD(WS-PERFORM-COUNTER))               SSTMT01P
011600              RESP(WS-RESP)                                       SSTMT01P
011700         END-EXEC                                                 SSTMT01P
011800     END-PERFORM.                                                 SSTMT01P
011900                                                                  SSTMT01P
012000***************************************************************** SSTMT01P
012100* Move the result back to the callers area                      * SSTMT01P
012200***************************************************************** SSTMT01P
012300     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       SSTMT01P
012400                                                                  SSTMT01P
012500***************************************************************** SSTMT01P
012600* Return to our caller                                          * SSTMT01P
012700***************************************************************** SSTMT01P
012800 COPY CRETURN.                                                    SSTMT01P
012900                                                                  SSTMT01P
013000* $ Version 5.98b sequenced on Thursday 5 Feb 2009 at 11:00pm     SSTMT01P
