000100***************************************************************** cabendpo
000200*                                                               * cabendpo
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cabendpo
000400*                                                               * cabendpo
000500***************************************************************** cabendpo
000600                                                                  cabendpo
000700***************************************************************** cabendpo
000800* CABENDP.CPY                                                   * cabendpo
000900*---------------------------------------------------------------* cabendpo
001000* Invoke abend processing                                       * cabendpo
001100* This copybook is used to provide an abend invocation routine  * cabendpo
001200* that is appropriate to the environment.                       * cabendpo
001300* There are different versions for BATCH, CICS and IMS.         * cabendpo
001400***************************************************************** cabendpo
001500     MOVE SPACES TO ABEND-MSG                                     cabendpo
001600     STRING ABEND-CULPRIT DELIMITED BY SIZE                       cabendpo
001700            ' Abend ' DELIMITED BY SIZE                           cabendpo
001800            ABEND-CODE DELIMITED BY SIZE                          cabendpo
001900            ' - ' DELIMITED BY SIZE                               cabendpo
002000            ABEND-REASON DELIMITED BY SIZE                        cabendpo
002100       INTO ABEND-MSG                                             cabendpo
002200     EXEC CICS WRITE                                              cabendpo
002300               OPERATOR                                           cabendpo
002400               TEXT(ABEND-MSG)                                    cabendpo
002500               TEXTLENGTH(LENGTH OF ABEND-MSG)                    cabendpo
002600     END-EXEC                                                     cabendpo
002700     EXEC CICS ABEND                                              cabendpo
002800               ABCODE(ABEND-CODE)                                 cabendpo
002900     END-EXEC                                                     cabendpo
003000     GOBACK                                                       cabendpo
003100                                                                  cabendpo
003200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cabendpo
