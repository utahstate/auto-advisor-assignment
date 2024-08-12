DROP TRIGGER BANINST1.Z_STU_SGBSTDN_ADVISOR_ASSIGN_AR;

CREATE OR REPLACE TRIGGER BANINST1.Z_STU_SGBSTDN_ADVISOR_ASSIGN_AR 
   /*******************************************************************************
   Trigger: UVU_SGBSTDN_ADVISOR_ASSIGN_AR
   Vers  Date        Developer(s)            Req #    Description
   ----  --------    -------------------     -----    --------------------------
   1.0   20100728    Jake Ellsworth, UVU     2662     Trigger to assign primary and secondary
                                                         advisers to students when changes are made
                                                         to a students record
   2.0   20180119    Carl Ellsworth, USU              Refactor to USU Convention
   *******************************************************************************/
   AFTER UPDATE OR INSERT OR DELETE
   ON SATURN.SGBSTDN
   FOR EACH ROW
BEGIN
   IF DELETING
   THEN
       Z_STU_ADVR_SZADVR_LOGIC.gv_pidm := :old.sgbstdn_pidm;
       Z_STU_ADVR_SZADVR_LOGIC.gv_term := :old.sgbstdn_term_code_eff;
       Z_STU_ADVR_SZADVR_LOGIC.gv_changed_val := NULL;
       Z_STU_ADVR_SZADVR_LOGIC.gv_status := :old.sgbstdn_stst_code;
       Z_STU_ADVR_SZADVR_LOGIC.gv_delete_ind := 'Y';
   ELSE
       Z_STU_ADVR_SZADVR_LOGIC.gv_pidm := :new.sgbstdn_pidm;
       Z_STU_ADVR_SZADVR_LOGIC.gv_term := :new.sgbstdn_term_code_eff;
       Z_STU_ADVR_SZADVR_LOGIC.gv_changed_val := NULL;
       Z_STU_ADVR_SZADVR_LOGIC.gv_status := :new.sgbstdn_stst_code;
       Z_STU_ADVR_SZADVR_LOGIC.gv_delete_ind := 'N';
   END IF;
   Z_STU_ADVR_SZADVR_LOGIC.gv_trigger := 'SGBSTDN';
EXCEPTION
   WHEN OTHERS
   THEN
       Z_STU_ADVR_MESSAGE_LOG.SAVELINE (
           pi_calling_proc   => 'Z_STU_SGBSTDN_ADVISOR_ASSIGN_AR',
           pi_sqlerrm        => SQLERRM);
END Z_STU_SGBSTDN_ADVISOR_ASSIGN_AR;
/
