CREATE OR REPLACE PACKAGE BANINST1.Z_STU_ADVR_MESSAGE_LOG
--   AUTHID CURRENT_USER -- UVU MDS 9-DEC-2008 Commented out for wider use
IS
    /******************************************************************************
       NAME:         pkg_message_log
       PURPOSE:      To store information about a running process in a table for debugging
       DEPENDENCIES: MESSAGE_LOG table
    ******************************************************************************/

    /******************************************************************************
     * PROCEDURE:
     * NAME:       saveline
     * PURPOSE:    To store information about a running process in a table for debugging
     * PARAMETERS:
     * INPUT: pi_calling_proc, the procedure that is sending out the message
     *        pi_calling_section, the section of the calling procedure
     *        pi_message, the message to be stored
     *        pi_sqlcode, number datatype, to store the result code of the most recent
     *                    operation within the procedure
     *        pi_sqlerrm, the error message from oracle's environment
     * OUTPUT: Writes to MESSAGE_LOG table
     * RETURNED VALUE: None
     * CALLED BY:
     * CALLS: Private procedure putline() which uses SYS_CONTEXT to log environment info
     * EXAMPLE USE:    IF g_debug THEN   -- use global boolean variable to turn on/off messages
     *                    pkg_message_log.saveline(pi_calling_proc    => 'procedure name'
     *                                           , pi_calling_section => 'section name'
     *                                           , pi_message         => 'Message to be logged'
     *                                           , pi_sqlcode         => SQLCODE  -- results of previous SQL
     *                                           , pi_sqlerrm         => SQLERRM
     *                                          );
     *                 END IF;
     * ASSUMPTIONS: All messages are temporary and may be deleted from table at DBA's discretion.
     * LIMITATIONS: Programmer's will eliminate unnecessary messages when procedure rolled out
     *              to Production server.
     * ALGORITHM:
     * NOTES: Uses PRAGMA AUTONOMOUS_TRANSACTION to isolate commits/rollbacks of calling procedure
    ******************************************************************************/
    PROCEDURE saveline (pi_calling_proc      IN VARCHAR2,
                        pi_calling_section   IN VARCHAR2 DEFAULT NULL,
                        pi_message           IN VARCHAR2 DEFAULT NULL,
                        pi_sqlcode           IN NUMBER DEFAULT NULL,
                        pi_sqlerrm           IN VARCHAR2 DEFAULT NULL);
END Z_STU_ADVR_MESSAGE_LOG;
/