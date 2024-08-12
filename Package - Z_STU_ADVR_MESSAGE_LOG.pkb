CREATE OR REPLACE PACKAGE BODY BANINST1.Z_STU_ADVR_MESSAGE_LOG
AS
    /******************************************************************************
    PKG_MESSAGE_LOG - Message Log
    Ver      Date      Developer(s)          Description
    -------  --------  --------------------  ------------------------------------
    1.0      20040310  Chuck Colborne, UVU   Created this package.
    1.1      20041029  Chuck Colborne, UVU   Used SYS_CONTEXT() instead of V$SESSION
    2.0      20180306  Carl Ellsworth, USU   Refactor to USU Convention
    */

    /******************************************************************************
     * This package allows developers to log error or informational messages to the message log
     * The public procedure saveline() is used and SYS_CONTEXT values stored by putline()
     * It is meant to be used for debugging purposes during development and
     * to log useful information about users that are running procedures when in production
     * See package spec for more information
     ******************************************************************************/
    CURSOR env_cur
    IS
        SELECT SUBSTR (SYS_CONTEXT ('USERENV', 'SESSION_USER'), 1, 30)
                   username,
               SUBSTR (SYS_CONTEXT ('USERENV', 'EXTERNAL_NAME'), 1, 256)
                   external_name,
               SUBSTR (SYS_CONTEXT ('USERENV', 'OS_USER'), 1, 30)
                   os_user,
               SUBSTR (SYS_CONTEXT ('USERENV', 'SESSION_USER'), 1, 30)
                   session_user,
               SUBSTR (SYS_CONTEXT ('USERENV', 'CURRENT_USER'), 1, 30)
                   current_user,
               SUBSTR (SYS_CONTEXT ('USERENV', 'PROXY_USER'), 1, 30)
                   proxy_user,
               SUBSTR (SYS_CONTEXT ('USERENV', 'TERMINAL'), 1, 64)
                   terminal,
               SUBSTR (SYS_CONTEXT ('USERENV', 'HOST'), 1, 54)
                   HOST,
               SUBSTR (SYS_CONTEXT ('USERENV', 'IP_ADDRESS'), 1, 30)
                   ip_address
          FROM DUAL;

    env_rec   env_cur%ROWTYPE; -- place to keep environment variables scope = session

    /******************************************************************************
     * PRIVATE PROCEDURE:
     * NAME:       putline
     * PURPOSE:    To store environment information with messages
     * PARAMETERS:
     * INPUT:  Directly from saveline()
     * OUTPUT: Inserts to MESSAGE_LOG table
     * RETURNED VALUE: None
     * CALLED BY: saveline()
     * CALLS: None
     * EXAMPLE USE:
     * ASSUMPTIONS:
     * LIMITATIONS: Call only from saveline or will not be autonomous or committed
     * ALGORITHM:
     * NOTES:
     ******************************************************************************/
    PROCEDURE putline (pi_calling_proc      IN VARCHAR2,
                       pi_calling_section   IN VARCHAR2 DEFAULT NULL,
                       pi_message           IN VARCHAR2 DEFAULT NULL,
                       pi_sqlcode           IN NUMBER DEFAULT NULL,
                       pi_sqlerrm           IN VARCHAR2 DEFAULT NULL)
    IS
    BEGIN
        INSERT INTO baninst1.z_stu_advr_message_log_tb
             VALUES (z_stu_advr_message_log_seq.NEXTVAL -- This is used to ORDER later selects
                                            --    since date does not allow needed resolution
                                            ,
                     pi_calling_proc,
                     pi_calling_section,
                     pi_message,
                     pi_sqlcode,
                     pi_sqlerrm,
                     env_rec.username -- Environment variable values are captured
                                     ,
                     env_rec.external_name -- when session instantiates this package
                                          ,
                     env_rec.os_user,
                     env_rec.session_user,
                     env_rec.current_user,
                     env_rec.proxy_user,
                     env_rec.terminal,
                     env_rec.HOST,
                     env_rec.ip_address,
                     SYSDATE);
    END putline;                                              -- end putline()

    /******************************************************************************
     * PUBLIC PROCEDURE:
     * NAME:    saveline
     * PURPOSE: Uses putline to store environment information with messages
     * PARAMETERS:
     * INPUT: pi_calling_proc, the procedure that is sending out the message
     *        pi_calling_section, the section of the calling procedure
     *        pi_message, the message to be stored
     *        pi_sqlcode, number datatype, to store the result code of the most recent
     *                    operation within the procedure, OPTIONAL
     *        pi_sqlerrm, the error message from oracle's environment, OPTIONAL
     * OUTPUT: Writes to MESSAGE_LOG table
     * RETURNED VALUE: None
     * CALLED BY:
     * CALLS:   putline()
     * EXAMPLE USE: see package spec
     * ASSUMPTIONS:
     * LIMITATIONS:
     * ALGORITHM:
     * NOTES:
    ******************************************************************************/
    PROCEDURE saveline (pi_calling_proc      IN VARCHAR2,
                        pi_calling_section   IN VARCHAR2 DEFAULT NULL,
                        pi_message           IN VARCHAR2 DEFAULT NULL,
                        pi_sqlcode           IN NUMBER DEFAULT NULL,
                        pi_sqlerrm           IN VARCHAR2 DEFAULT NULL)
    IS
        PRAGMA AUTONOMOUS_TRANSACTION;
    BEGIN
        putline (pi_calling_proc,
                 pi_calling_section,
                 pi_message,
                 pi_sqlcode,
                 pi_sqlerrm);
        COMMIT;
    EXCEPTION
        WHEN OTHERS
        THEN
            ROLLBACK; -- Horrible programming style but this procedure shouldn't
    END; -- end saveline                   -- hold up the calling procedure. If nothing appears after
                 -- a call to saveline, try running the insert statement above
BEGIN -- this section is used to load the env_rec with useful environment information
    -- when package is instantiated. No need to rerun the cursor with each call
    OPEN env_cur;

    FETCH env_cur INTO env_rec;

    CLOSE env_cur;
END Z_STU_ADVR_MESSAGE_LOG;
/