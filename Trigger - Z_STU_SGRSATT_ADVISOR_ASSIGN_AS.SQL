DROP TRIGGER BANINST1.Z_STU_SGRSATT_ADVISOR_ASSIGN_AS;

CREATE OR REPLACE TRIGGER BANINST1.Z_STU_SGRSATT_ADVISOR_ASSIGN_AS 
    /*******************************************************************************
    Trigger: UVU_SGRSATT_ADVISOR_ASSIGN_AS

    Vers  Date        Developer(s)            Req #    Description
    ----  --------    -------------------     -----    --------------------------
    1.0   20100728    Jake Ellsworth, UVU     2662     Trigger to assign primary and secondary
                                                          advisers to students when changes are made
                                                          to a students record
    2.0   20180119    Carl Ellsworth, USU              Refactor to USU Convention

    *******************************************************************************/

    AFTER UPDATE OR INSERT OR DELETE
    ON SATURN.SGRSATT
BEGIN
    Z_STU_ADVR_SZADVR_LOGIC.P_ADVISOR_ASSIGNMENT;
EXCEPTION
    WHEN OTHERS
    THEN
        BANINST1.Z_STU_ADVR_MESSAGE_LOG.SAVELINE (
            pi_calling_proc   => 'UVU_SGRSATT_ADVISOR_ASSIGN_AS',
            pi_sqlerrm        => SQLERRM);
END UVU_SGRSATT_ADVISOR_ASSIGN_AS;
/
