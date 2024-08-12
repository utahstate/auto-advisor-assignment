DROP TRIGGER BANINST1.Z_STU_SGRSATT_ADVISOR_ASSIGN_AR;

CREATE OR REPLACE TRIGGER BANINST1.Z_STU_SGRSATT_ADVISOR_ASSIGN_AR 
    /*******************************************************************************
    Trigger: UVU_SGRSATT_ADVISOR_ASSIGN_AR

    Vers  Date        Developer(s)            Req #    Description
    ----  --------    -------------------     -----    --------------------------
    1.0   20100728    Jake Ellsworth, UVU     2662     Trigger to assign primary and secondary
                                                          advisers to students when changes are made
                                                          to a students record
    2.0   20180119    Carl Ellsworth, USU              Refactor to USU Convention

    *******************************************************************************/

    AFTER UPDATE OR INSERT OR DELETE
    ON SATURN.SGRSATT
    FOR EACH ROW
BEGIN
    IF DELETING
    THEN
        Z_STU_ADVR_SZADVR_LOGIC.gv_pidm := :old.sgrsatt_pidm;
        Z_STU_ADVR_SZADVR_LOGIC.gv_term := :old.sgrsatt_term_code_eff;
        Z_STU_ADVR_SZADVR_LOGIC.gv_changed_val := :old.sgrsatt_atts_code;
        Z_STU_ADVR_SZADVR_LOGIC.gv_status := 'AS';
        Z_STU_ADVR_SZADVR_LOGIC.gv_delete_ind := 'Y';
    ELSE
        Z_STU_ADVR_SZADVR_LOGIC.gv_pidm := :new.sgrsatt_pidm;
        Z_STU_ADVR_SZADVR_LOGIC.gv_term := :new.sgrsatt_term_code_eff;
        Z_STU_ADVR_SZADVR_LOGIC.gv_changed_val := :new.sgrsatt_atts_code;
        Z_STU_ADVR_SZADVR_LOGIC.gv_status := 'AS';
        Z_STU_ADVR_SZADVR_LOGIC.gv_delete_ind := 'N';
    END IF;

    Z_STU_ADVR_SZADVR_LOGIC.gv_trigger := 'SGRSATT';
EXCEPTION
    WHEN OTHERS
    THEN
        Z_STU_ADVR_MESSAGE_LOG.SAVELINE (
            pi_calling_proc   => 'Z_STU_SGRSATT_ADVISOR_ASSIGN_AR',
            pi_sqlerrm        => SQLERRM);
END Z_STU_SGRSATT_ADVISOR_ASSIGN_AR;
/
