CREATE OR REPLACE PACKAGE BANINST1.Z_STU_ADVR_SZADVR_LOGIC
AS
    gv_pidm          NUMBER := NULL;
    gv_term          VARCHAR2 (6) := NULL;
    gv_status        VARCHAR2 (2) := NULL;
    gv_delete_ind    VARCHAR2 (1) := NULL;
    gv_trigger       VARCHAR2 (100) := NULL;
    gv_changed_val   VARCHAR2 (100) := NULL;

    PROCEDURE P_ADVISOR_ASSIGNMENT;

    PROCEDURE P_ADVISOR_ASSIGNMENT (pi_term        VARCHAR2,
                                    --Should be Y for web display
                                    --Should be N for all others
                                    pi_web_ind     VARCHAR2,
                                    pi_jobsub      VARCHAR2 DEFAULT NULL,
                                    pi_school      VARCHAR2 DEFAULT NULL,
                                    pi_mode        VARCHAR2 DEFAULT 'A',
                                    pi_stu_pidm    NUMBER DEFAULT NULL);

    PROCEDURE P_PROCESS_ADVISORS (pi_stu_pidm    NUMBER,
                                  pi_term        VARCHAR2,
                                  pi_web_ind     VARCHAR2);
END Z_STU_ADVR_SZADVR_LOGIC;
/