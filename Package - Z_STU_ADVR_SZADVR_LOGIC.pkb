CREATE OR REPLACE PACKAGE BODY BANINST1.Z_STU_ADVR_SZADVR_LOGIC
AS
    /*************************************************************************************************************
    SZADVR_LOGIC - Automatic Advisor Assignment Logic Package
    Vers  Date         Developer(s)            Req #    Description
    ----  --------     -------------------     -----    ---------------------------------------------------------
    1.0   20120109     GLA                              Added NULLS where an INSERT is being done on SGRADVR
    2.0   20180119     Carl Ellsworth, USU              Refactor to USU Convention
    2.1   20180313     Carl Ellsworth, USU              Added char_list dependency and explicitly listed table fields
    3.0   20231209     Trevor Bennett                   Refactored - removed UVU Schema Dependency
                                                                   -  Added campus to the advisor rules
                                                                   -  Fixed student attribute rule
                                                                   -  Defaulted secondary major to pull from previous 
                                                                      active advisor to the new active advisor
          20240301     Trevor Bennett                              - Fix for active secondary advisor inadvertently updating
                                                                     under the condition when the current primary advisor
                                                                     matches the the advisor rules, under that condition no
                                                                     updates/inserts should occur.
                                                                     
                                                                    
    */

    --Precision of the start and end alphabet rules
    gc_precision    INTEGER := 3;                                   --up to 10
    gc_alph_start   VARCHAR2 (1) := 'a';
    gc_alph_end     VARCHAR2 (1) := 'z';
    gv_primary_advisor_changed     VARCHAR2(1) := 'N';

    PROCEDURE P_ADVISOR_ASSIGNMENT
    --Called from triggers on tables SGBSTDN, SGRSATT, SGRVETN
    IS
    BEGIN
        Z_STU_ADVR_MESSAGE_LOG.SAVELINE (
            pi_calling_proc =>
                'SZADVR_LOGIC',
            pi_calling_section =>
                gv_trigger,
            pi_message =>
                   'Pidm:'
                || gv_pidm
                || ' ,'
                || 'Term:'
                || gv_term
                || ' ,'
                || 'Status:'
                || gv_status);

        --Called from the trigger
        IF gv_status = 'AS'
        THEN
            --If trigger indicates the action was to delete a record
            IF gv_delete_ind = 'Y'
            THEN
                IF gv_trigger = 'SGBSTDN'
                THEN
                    --Remove primary and secondary academic, minor, and veteran advisors
                    DELETE sgradvr
                     WHERE     sgradvr_term_code_eff = gv_term
                           AND sgradvr_pidm = gv_pidm
                           AND sgradvr_advr_code IN ('A', 'M', 'V');
                ELSIF gv_trigger = 'SGRSATT'
                THEN
                    --Remove any advisor associated with the deleted attribute
                    DECLARE
                        --Sorlcur records
                        v_sorlcur_ref   SB_CURRICULUM.curriculum_ref;
                        v_sorlcur_rec   SB_CURRICULUM.curriculum_rec;

                        --If student's last name is less than 3 characters, pad with an a
                        v_lname         VARCHAR2 (10)
                            := RPAD (
                                   LOWER (
                                       REPLACE (
                                           BANINST1.F_FORMAT_NAME (gv_pidm,
                                                                   'L30'),
                                           ' ',
                                           '')),
                                   3,
                                   'a');
                    BEGIN
                        --Get the current sorlcur record for the student
                        v_sorlcur_ref :=
                            SB_CURRICULUM.F_QUERY_CURRENT (
                                gv_pidm,
                                'LEARNER',
                                gv_term,
                                99,
                                p_active_ind   => 'Y');

                        FETCH v_sorlcur_ref INTO v_sorlcur_rec;

                        CLOSE v_sorlcur_ref;

                        IF v_sorlcur_rec.r_pidm IS NULL
                        THEN
                            v_sorlcur_ref :=
                                SB_CURRICULUM.F_QUERY_CURRENT (
                                    gv_pidm,
                                    'LEARNER',
                                    p_keyseqno     => 99,
                                    p_active_ind   => 'Y',
                                    p_eff_term     => gv_term);

                            FETCH v_sorlcur_ref INTO v_sorlcur_rec;

                            CLOSE v_sorlcur_ref;
                        END IF;

                        --Only remove secondary academic advisors
                        DELETE sgradvr
                         WHERE     sgradvr_term_code_eff = gv_term
                               AND sgradvr_pidm = gv_pidm
                               AND sgradvr_prim_ind = 'N'
                               AND (   sgradvr_advr_code = 'A'
                                    OR sgradvr_advr_code IS NULL)
                               AND sgradvr_advr_pidm IN
                                       (SELECT zsadvr_pidm
                                          FROM baninst1.z_stu_advr_zsadvr
                                         WHERE     zsadvr_term_code = gv_term
                                               AND zsadvr_role_code = 'S'
                                               AND zsadvr_school_code =
                                                   v_sorlcur_rec.r_coll_code
                                               AND zsadvr_attribute_code =
                                                   gv_changed_val
                                               AND (   v_lname BETWEEN RPAD (
                                                                           LOWER (
                                                                               REPLACE (
                                                                                   zsadvr_alphabet_start,
                                                                                   ' ',
                                                                                   '')),
                                                                           gc_precision,
                                                                           gc_alph_start)
                                                                   AND RPAD (
                                                                           LOWER (
                                                                               REPLACE (
                                                                                   zsadvr_alphabet_end,
                                                                                   ' ',
                                                                                   '')),
                                                                           gc_precision,
                                                                           gc_alph_end)
                                                    OR zsadvr_alphabet_start
                                                           IS NULL));
                    --                                                    AND      (v_lname BETWEEN LOWER(REPLACE(zsadvr_alphabet_start, ' ', '')) AND LOWER(REPLACE(zsadvr_alphabet_end, ' ', '')) OR zsadvr_alphabet_start IS NULL));
                    END;
                ELSIF gv_trigger = 'SGRVETN'
                THEN
                    --Only remove secondary veteran advisors
                    DELETE sgradvr
                     WHERE     sgradvr_term_code_eff = gv_term
                           AND sgradvr_pidm = gv_pidm
                           AND sgradvr_prim_ind = 'N'
                           AND sgradvr_advr_code = 'V';
                END IF;
            ELSE
                --Create new advisor assignments
                P_PROCESS_ADVISORS (gv_pidm, gv_term, 'N');
            END IF;
        ELSE
            IF gv_trigger <> 'SGBSTDN'
            THEN
                Z_STU_ADVR_MESSAGE_LOG.SAVELINE (
                    pi_calling_proc =>
                        'SZADVR_LOGIC',
                    pi_calling_section =>
                        'Trigger Error!',
                    pi_message =>
                        'No values sent from the fired Adviser Assignment trigger');
            END IF;
        END IF;
    END P_ADVISOR_ASSIGNMENT;

    PROCEDURE P_ADVISOR_ASSIGNMENT (pi_term        VARCHAR2,
                                    --Should be Y for web display
                                    --Should be N for all others
                                    pi_web_ind     VARCHAR2,
                                    pi_jobsub      VARCHAR2 DEFAULT NULL,
                                    pi_school      VARCHAR2 DEFAULT NULL,
                                    pi_mode        VARCHAR2 DEFAULT 'A',
                                    pi_stu_pidm    NUMBER DEFAULT NULL)
    /* P_ADVISOR_ASSIGNMENT can be called three different ways
       1. SZADVR web interface to display advisor assignments pi_web_ind = Y and pi_jobsub IS NULL
       2. toad to make assignments for a specific student pi_web_ind = N, pi_jobsub IS NULL, and pi_stu_pidm IS NOT NULL
       3. jobsub to do mass assignments pi_jobsub = JOBSUB, pi_stu_pidm IS NULL, and pi_web_ind = N */
    IS
        CURSOR get_active_students_c (
            term_in    VARCHAR2)
        --Selects all students considered to be active, excludes CE
        IS
            SELECT stu_pidm
              FROM (SELECT b.sgbstdn_pidm stu_pidm
                      FROM sgbstdn b
                     WHERE     b.sgbstdn_stst_code = 'AS'
                           AND b.sgbstdn_term_code_eff =
                               (SELECT MAX (c.sgbstdn_term_code_eff)
                                  FROM sgbstdn c
                                 WHERE     c.sgbstdn_pidm = b.sgbstdn_pidm
                                       AND SUBSTR (c.sgbstdn_term_code_eff,
                                                   6,
                                                   1) =
                                           '0'            --Remove CE students
                                       AND c.sgbstdn_term_code_eff BETWEEN   term_in
                                                                           - 100
                                                                       AND term_in) --go back 1 year
                    UNION
                    SELECT sfrstcr_pidm stu_pidm
                      FROM saturn.sfrstcr
                     WHERE     sfrstcr_term_code BETWEEN term_in - 100
                                                     AND term_in --go back 1 year
                           AND SUBSTR (sfrstcr_term_code, 6, 1) = '0' --Remove CE
                           AND sfrstcr_rsts_code IN ('RW', 'RE'));

        /* Old way
                 SELECT   DISTINCT(b.sgbstdn_pidm) stu_pidm
                 FROM     sgbstdn b
                 WHERE    b.sgbstdn_stst_code = 'AS'
                 AND      b.sgbstdn_term_code_eff = (SELECT   MAX(c.sgbstdn_term_code_eff)
                                                     FROM     sgbstdn c
                                                     WHERE    c.sgbstdn_pidm = b.sgbstdn_pidm
                                                     AND      SUBSTR(c.sgbstdn_term_code_eff, 6, 1) = '0' --Remove CE students
                                                     AND      c.sgbstdn_term_code_eff BETWEEN term_in - 100 AND term_in) --go back 1 year
                 OR EXISTS (SELECT   'X'
                            FROM     sfrstcr
                            WHERE    sfrstcr_term_code BETWEEN term_in - 100 AND term_in--go back 1 year
                            AND      sfrstcr_pidm = b.sgbstdn_pidm
                            AND      SUBSTR(sfrstcr_term_code, 6, 1) = '0' --Remove CE
                            AND      sfrstcr_rsts_code IN ('RW', 'RE'));
        */

        --Sorlcur records
        v_sorlcur_ref                     SB_CURRICULUM.curriculum_ref;
        v_sorlcur_rec                     SB_CURRICULUM.curriculum_rec;

        no_student_pidm                   EXCEPTION;
        v_counter                         INTEGER := 0;
        v_student_counter                 INTEGER := 0;

        c_student_output_count   CONSTANT INTEGER := 25;
        c_commit_count           CONSTANT INTEGER := 300;
    BEGIN
        --Procedure may have been called by a batch process
        IF pi_jobsub IS NULL
        THEN
            --If not called by a batch process and no student provided, raise error
            IF pi_stu_pidm IS NULL
            THEN
                RAISE no_student_pidm;
            ELSE
                --Process was not called by the advisor assignment interface
--                IF pi_web_ind = 'N'
--                THEN
                P_PROCESS_ADVISORS (pi_stu_pidm, pi_term, 'N');
                COMMIT;
                --If called through the advisor assignment interface, pi_stu_pidm is actually a A#
--                ELSE
--                    P_PROCESS_ADVISORS (TWBKSLIB.F_FETCHPIDM (pi_stu_pidm),
--                                        pi_term,
--                                        'Y');
--                END IF;
            END IF;
        --Called from a jobsub
        ELSIF pi_jobsub = 'JOBSUB'
        THEN
            DBMS_OUTPUT.PUT_LINE ('Student A-Numbers and Names');

            --Batch process, fetch and process active students
            FOR students_rec IN get_active_students_c (pi_term)
            LOOP
                --If no school code specified, make assignments for all active students
                IF pi_school IS NULL
                THEN
                    IF v_student_counter <= c_student_output_count
                    THEN
                        DBMS_OUTPUT.PUT_LINE (
                               BANINST1.F_GETSPRIDENID (
                                   students_rec.stu_pidm)
                            || ', '
                            || BANINST1.F_FORMAT_NAME (students_rec.stu_pidm,
                                                       'LF30'));
                    END IF;

                    --increment counters
                    v_counter := v_counter + 1;
                    v_student_counter := v_student_counter + 1;

                    --Only check for assignments if jobsub is in update mode
                    IF pi_mode = 'U'
                    THEN
                        P_PROCESS_ADVISORS (students_rec.stu_pidm,
                                            pi_term,
                                            'N');
                    END IF;
                ELSE
                    --Get the current sorlcur record for the student
                    v_sorlcur_ref :=
                        SB_CURRICULUM.F_QUERY_CURRENT (students_rec.stu_pidm,
                                                       'LEARNER',
                                                       pi_term,
                                                       99,
                                                       p_active_ind   => 'Y');

                    FETCH v_sorlcur_ref INTO v_sorlcur_rec;

                    CLOSE v_sorlcur_ref;

                    IF v_sorlcur_rec.r_pidm IS NULL
                    THEN
                        v_sorlcur_ref :=
                            SB_CURRICULUM.F_QUERY_CURRENT (
                                students_rec.stu_pidm,
                                'LEARNER',
                                p_keyseqno     => 99,
                                p_active_ind   => 'Y',
                                p_eff_term     => pi_term);

                        FETCH v_sorlcur_ref INTO v_sorlcur_rec;

                        CLOSE v_sorlcur_ref;
                    END IF;

                    --If pi_school has a value, make only assignments for students who are involved with that school
                    IF v_sorlcur_rec.r_coll_code = pi_school
                    THEN
                        IF v_student_counter <= c_student_output_count
                        THEN
                            DBMS_OUTPUT.PUT_LINE (
                                   BANINST1.F_GETSPRIDENID (
                                       students_rec.stu_pidm)
                                || ', '
                                || BANINST1.F_FORMAT_NAME (
                                       students_rec.stu_pidm,
                                       'LF30'));
                        END IF;

                        --increment counters
                        v_counter := v_counter + 1;
                        v_student_counter := v_student_counter + 1;

                        --Only check for assignments if jobsub is in update mode
                        IF pi_mode = 'U'
                        THEN
                            P_PROCESS_ADVISORS (students_rec.stu_pidm,
                                                pi_term,
                                                'N');
                        END IF;
                    END IF;
                END IF;

                --Commit every 300 records
                IF pi_mode = 'U' AND v_counter = c_commit_count
                THEN
                    v_counter := 0;
                    COMMIT;
                END IF;
            END LOOP;

            IF pi_mode = 'U'
            THEN
                COMMIT;
            END IF;

            DBMS_OUTPUT.PUT_LINE (
                'Total Students processed: ' || v_student_counter);
        END IF;
    EXCEPTION
        WHEN no_student_pidm
        THEN
            HTP.P ('A pidm or A-Number is required.');
            HTP.BR;
            HTP.BR;
        WHEN OTHERS
        THEN
            DBMS_OUTPUT.PUT_LINE (
                'Error: ' || SQLERRM || ' Rollback issued.');

            Z_STU_ADVR_MESSAGE_LOG.SAVELINE (pi_calling_proc      => 'SZADVR_LOGIC',
                                      pi_calling_section   => 'JobSub Error!',
                                      pi_message           => SQLERRM);
            ROLLBACK;
    END P_ADVISOR_ASSIGNMENT;

    PROCEDURE P_PROCESS_ADVISORS (pi_stu_pidm    NUMBER,
                                  pi_term        VARCHAR2,
                                  pi_web_ind     VARCHAR2)
    IS
        --Sorlcur records
        v_sorlcur_ref       SB_CURRICULUM.curriculum_ref;
        v_sorlcur_rec       SB_CURRICULUM.curriculum_rec;

        --Custom data types
        v_conc              z_stu_advr_char_list;
        v_minor             z_stu_advr_char_list;
        v_attr              z_stu_advr_char_list;
        v_sec_advr          z_stu_advr_char_list;

        --Regular data types
        --If student's last name is less than 3 characters, pad with an a
        v_lname             VARCHAR2 (10)
            := RPAD (
                   LOWER (
                       REPLACE (BANINST1.F_FORMAT_NAME (pi_stu_pidm, 'L30'),
                                ' ',
                                '')),
                   3,
                   'a');
        v_pidm              NUMBER := pi_stu_pidm;
        v_term              VARCHAR2 (6) := pi_term;
--        v_web_ind           VARCHAR2 (1) := pi_web_ind;
        v_prim_advr_count   INTEGER := 0;
        v_sec_advr_count    INTEGER := 0;
        v_ZSADVR_term       VARCHAR2 (6);
        v_camp_code         saturn.sgbstdn.sgbstdn_camp_code%TYPE;

        --Exceptions
        stop_assignment     EXCEPTION;


        --***Start 1st nested proc
        PROCEDURE P_PROCESS_PRIMARY_ADVISOR (
            pi_vet_ind      VARCHAR2 DEFAULT 'N',
            pi_force_ind    VARCHAR2 DEFAULT 'N')
        --Procedure to assign a primary advisor
        IS
--            primary_advisor_changed     VARCHAR2(1) := 'N';
                    
            CURSOR get_rules_c (
                term_in     VARCHAR2,
                coll_in     VARCHAR2,
                prog_in     VARCHAR2,
                conc_in     z_stu_advr_char_list,
                att_in      z_stu_advr_char_list,
                camp_in     VARCHAR2 DEFAULT NULL,
                lname_in    VARCHAR2 DEFAULT NULL)
            --Return advisor assignment rule(s). Sort ensure the most specific record is 1st in the list
            IS
                  SELECT *
                    FROM Z_STU_ADVR_ZSADVR
                   WHERE     zsadvr_term_code = term_in
                         AND zsadvr_role_code = 'P'                  --Primary
                         AND (zsadvr_school_code = coll_in
                              OR zsadvr_school_code IS NULL)
                         AND (zsadvr_program_code = prog_in
                              OR zsadvr_program_code IS NULL)
                         AND (   zsadvr_concentration_code MEMBER OF conc_in
                              OR zsadvr_concentration_code IS NULL)
                         AND (   zsadvr_attribute_code MEMBER OF att_in
                              OR zsadvr_attribute_code IS NULL)
                         AND (   zsadvr_campus_code = camp_in
                              OR zsadvr_campus_code IS NULL)
                         AND (   lname_in BETWEEN RPAD (
                                                      LOWER (
                                                          REPLACE (
                                                              zsadvr_alphabet_start,
                                                              ' ',
                                                              '')),
                                                      gc_precision,
                                                      gc_alph_start)
                                              AND RPAD (
                                                      LOWER (
                                                          REPLACE (
                                                              zsadvr_alphabet_end,
                                                              ' ',
                                                              '')),
                                                      gc_precision,
                                                      gc_alph_end)
                              OR zsadvr_alphabet_start IS NULL)
                ORDER BY zsadvr_school_code,
                         zsadvr_program_code,
                         zsadvr_concentration_code,
                         zsadvr_attribute_code,
                         zsadvr_campus_code,
                         zsadvr_alphabet_start
                FETCH FIRST ROW ONLY;

            --Cursor
            advisor_rules_rec   get_rules_c%ROWTYPE;
            no_advisor_record   EXCEPTION;
            v_current_advisor   sgradvr.sgradvr_advr_pidm%TYPE;
        BEGIN
            OPEN get_rules_c (v_ZSADVR_term,
                              v_sorlcur_rec.r_coll_code,
                              v_sorlcur_rec.r_program,
                              v_conc,
                              v_attr,
                              v_camp_code,
                              v_lname);

            FETCH get_rules_c INTO advisor_rules_rec;

            CLOSE get_rules_c;
--            insert into z_trevor_bennett.A_TESTING_LOG VALUES ( 'advisor_rules_rec.zsadvr_pidm = '||advisor_rules_rec.zsadvr_pidm, SYSDATE);           

            IF advisor_rules_rec.zsadvr_pidm IS NULL
            THEN
                RAISE no_advisor_record;
            ELSE
                --Get current primary advisor pidm
                BEGIN
                    SELECT s1.sgradvr_advr_pidm
                      INTO v_current_advisor
                      FROM sgradvr s1
                     WHERE     s1.sgradvr_pidm = v_pidm
                           AND s1.sgradvr_term_code_eff =
                               (SELECT MAX (s2.sgradvr_term_code_eff)
                                  FROM sgradvr s2
                                 WHERE     s2.sgradvr_pidm = s1.sgradvr_pidm
                                       AND s2.sgradvr_term_code_eff <= v_term --JGE 10/18/2010 Removed
                                                                             --                                                    AND     s2.sgradvr_prim_ind = 'Y')
                                                                             )
                           AND s1.sgradvr_prim_ind = 'Y';
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        v_current_advisor := NULL;
                END;

--****************************************
--                   insert into z_trevor_bennett.A_TESTING_LOG 
--                   VALUES ('pi_force_ind =  '||pi_force_ind||' pi_vet_ind = '||pi_vet_ind||' v_current_advisor = '||v_current_advisor||' advisor_rules_rec.zsadvr_pidm= '||advisor_rules_rec.zsadvr_pidm, SYSDATE);
--****************************************                   
                   
                    IF pi_force_ind = 'N'
                    THEN
                        --if no current advisor pidm found, or if the current advisor pidm is different
                        --than the rule found, delete old advisor record and insert new.
                        IF    v_current_advisor IS NULL
                           OR v_current_advisor <>
                              advisor_rules_rec.zsadvr_pidm
                        THEN
                            DELETE sgradvr
                             WHERE     sgradvr_pidm = v_pidm
                                   AND sgradvr_term_code_eff = v_term
                                   AND sgradvr_prim_ind = 'Y';

                            --Create new primary advisor record
                            INSERT INTO sgradvr (SGRADVR_PIDM,
                                                 SGRADVR_TERM_CODE_EFF,
                                                 SGRADVR_ADVR_PIDM,
                                                 SGRADVR_ADVR_CODE,
                                                 SGRADVR_PRIM_IND,
                                                 SGRADVR_ACTIVITY_DATE)
                                     --CARL 20180313 made table fields explicit
                                     VALUES (
                                                v_pidm,
                                                v_term,
                                                advisor_rules_rec.zsadvr_pidm,
                                                advisor_rules_rec.zsadvr_advisor_code,
                                                'Y',
                                                SYSDATE);
                            gv_primary_advisor_changed := 'Y';
                        END IF;
                    --Primary advisor being forced to be created. This is because secondary assignments have changed.
                    ELSIF pi_force_ind = 'Y'
                    THEN
                        DELETE sgradvr
                         WHERE     sgradvr_pidm = v_pidm
                               AND sgradvr_term_code_eff = v_term
                               AND sgradvr_prim_ind = 'Y';

                        --Create new primary advisor record
                        INSERT INTO sgradvr (SGRADVR_PIDM,
                                             SGRADVR_TERM_CODE_EFF,
                                             SGRADVR_ADVR_PIDM,
                                             SGRADVR_ADVR_CODE,
                                             SGRADVR_PRIM_IND,
                                             SGRADVR_ACTIVITY_DATE)
                             --CARL 20180313 made table fields explicit
                             VALUES (v_pidm,
                                     v_term,
                                     advisor_rules_rec.zsadvr_pidm,
                                     advisor_rules_rec.zsadvr_advisor_code,
                                     'Y',
                                     SYSDATE);
                        gv_primary_advisor_changed := 'Y';
                    END IF;
            END IF;
        EXCEPTION
            WHEN no_advisor_record
            THEN
                RAISE stop_assignment;
            WHEN OTHERS
            THEN
                    Z_STU_ADVR_MESSAGE_LOG.SAVELINE (
                        pi_calling_proc =>
                            'SZADVR_LOGIC',
                        pi_calling_section =>
                            'Primary Advisor',
                        pi_message =>
                               'Error for student: '
                            || BANINST1.F_GETSPRIDENID (v_pidm)
                            || ', for term: '
                            || v_term
                            || ' ROLLBACK ISSUED',
                        pi_sqlerrm =>
                            SQLERRM);

                    ROLLBACK;
        END P_PROCESS_PRIMARY_ADVISOR;

        --***End 1st nested proc

        --***Start 2nd nested proc
        PROCEDURE P_PROCESS_SECONDARY_ADVISOR
        --Procedure to assign a secondary advisor
        IS
        v_current_sec_advr NUMBER(8);
        BEGIN
                    Z_STU_ADVR_MESSAGE_LOG.SAVELINE (
                        pi_calling_proc =>
                            'SZADVR_LOGIC',
                        pi_calling_section =>
                            'Sec Advisor/Insert',
                        pi_sqlerrm =>
                            NULL,
                        pi_message =>
                            'Pidm:'
                            || v_pidm
                            || ' ,'
                            || 'Term:'
                            || v_term
                        );    
-- Check to see if secondary advisor already exists for term, if so then there is no need to roll secondary advisor from another term.
            SELECT count(sgradvr_advr_pidm)
                    INTO v_current_sec_advr
                      FROM sgradvr s1
                     WHERE     s1.sgradvr_pidm = v_pidm
                           AND s1.sgradvr_term_code_eff =
                               (SELECT MAX (s2.sgradvr_term_code_eff)
                                  FROM sgradvr s2
                                 WHERE     s2.sgradvr_pidm = s1.sgradvr_pidm
                                       AND s2.sgradvr_term_code_eff = v_term
                                       AND s2.sgradvr_prim_ind = 'N')
                           AND s1.sgradvr_prim_ind = 'N';                            

            IF v_current_sec_advr = 0 THEN
                FOR zs_rec
                       IN (SELECT s1.sgradvr_advr_pidm, s1.sgradvr_advr_code, sgradvr_term_code_eff
                          FROM sgradvr s1
                         WHERE     s1.sgradvr_pidm = v_pidm
                               AND s1.sgradvr_term_code_eff =
                                   (SELECT MAX (s2.sgradvr_term_code_eff)
                                      FROM sgradvr s2
                                     WHERE     s2.sgradvr_pidm =
                                               s1.sgradvr_pidm
                                           AND s2.sgradvr_term_code_eff <=
                                               v_term)
--                                           AND s2.sgradvr_prim_ind = 'N')
                               AND s1.sgradvr_prim_ind = 'N')
                                               
                 LOOP
                   
                       INSERT INTO sgradvr (sgradvr_pidm,
                                             sgradvr_term_code_eff,
                                             sgradvr_advr_pidm,
                                             sgradvr_advr_code,
                                             sgradvr_prim_ind,
                                             sgradvr_activity_date)
                             VALUES (v_pidm,
                                     v_term,
                                     zs_rec.sgradvr_advr_pidm,
                                     zs_rec.sgradvr_advr_code,
                                     'N',
                                     SYSDATE);  
  
--        IF zs_rec.zsadvr_advisor_code = 'V'
--        THEN
--            v_veteran_ind := TRUE;
--        END IF;
                END LOOP;  
            END IF;                                                 
                                    
--                   INSERT INTO sgradvr (sgradvr_pidm,
--                                         sgradvr_term_code_eff,
--                                         sgradvr_advr_pidm,
--                                         sgradvr_advr_code,
--                                         sgradvr_prim_ind,
--                                         sgradvr_activity_date)
--                         VALUES (1034941,
--                                 '202340',
--                                 2282612,
--                                 'RCSC',
--                                 'N',
--                                 SYSDATE);
--                    EXCEPTION
--                        WHEN OTHERS
--                        THEN
            Z_STU_ADVR_MESSAGE_LOG.SAVELINE (
                pi_calling_proc   => 'Secondary Advisor Roll After',
                pi_sqlerrm        => SQLERRM);        
----            CURSOR advisor_school_c (
----                pidm_in     NUMBER,
----                term_in     VARCHAR2,
----                coll_in     VARCHAR2,
----                prog_in     VARCHAR2,
----                conc_in     char_list,
----                attr_in     char_list,
----                lname_in    VARCHAR2)
----            --Get advisor assignment records based on school, program, concentration(s), attribute, and alphabet
----            IS
----                SELECT adv.*,
----                       LAG (adv.zsadvr_pidm) OVER (ORDER BY adv.zsadvr_pidm)
----                           prev_pidm
----                  FROM (SELECT zsadvr.*
----                          FROM zsadvr
----                         WHERE     zsadvr_term_code = term_in
----                               AND zsadvr_role_code = 'S'          --Secondary
----                               AND zsadvr_school_code = coll_in
----                               AND zsadvr_program_code = prog_in
----                               AND (   zsadvr_concentration_code
----                                            MEMBER OF conc_in
----                                    OR zsadvr_concentration_code IS NULL)
----                               AND (   lname_in BETWEEN RPAD (
----                                                            LOWER (
----                                                                REPLACE (
----                                                                    zsadvr_alphabet_start,
----                                                                    ' ',
----                                                                    '')),
----                                                            gc_precision,
----                                                            gc_alph_start)
----                                                    AND RPAD (
----                                                            LOWER (
----                                                                REPLACE (
----                                                                    zsadvr_alphabet_end,
----                                                                    ' ',
----                                                                    '')),
----                                                            gc_precision,
----                                                            gc_alph_end)
----                                    OR zsadvr_alphabet_start IS NULL)
----                        UNION
----                        SELECT *
----                          FROM zsadvr
----                         WHERE     zsadvr_term_code = term_in
----                               AND zsadvr_role_code = 'S'
----                               AND zsadvr_school_code = coll_in
----                               AND zsadvr_attribute_code MEMBER OF attr_in
----                               AND (   lname_in BETWEEN RPAD (
----                                                            LOWER (
----                                                                REPLACE (
----                                                                    zsadvr_alphabet_start,
----                                                                    ' ',
----                                                                    '')),
----                                                            gc_precision,
----                                                            gc_alph_start)
----                                                    AND RPAD (
----                                                            LOWER (
----                                                                REPLACE (
----                                                                    zsadvr_alphabet_end,
----                                                                    ' ',
----                                                                    '')),
----                                                            gc_precision,
----                                                            gc_alph_end)
----                                    OR zsadvr_alphabet_start IS NULL)) adv;
--
--            CURSOR advisor_minor_c (
--                pidm_in     NUMBER,
--                term_in     VARCHAR2,
--                minor_in    z_stu_advr_char_list,
--                lname_in    VARCHAR2)
--            IS
--                  --Get advisor assignment records based on minor(s) and alphabet
--                  SELECT z_stu_advr_zsadvr.*,
--                         LAG (zsadvr_pidm) OVER (ORDER BY zsadvr_pidm)
--                             prev_pidm
--                    FROM z_stu_advr_zsadvr
--                   WHERE     zsadvr_term_code = term_in
--                         AND zsadvr_role_code = 'S'
--                         AND zsadvr_advisor_code = 'M'
--                         AND zsadvr_minor_code MEMBER OF minor_in
--                         AND (   lname_in BETWEEN RPAD (
--                                                      LOWER (
--                                                          REPLACE (
--                                                              zsadvr_alphabet_start,
--                                                              ' ',
--                                                              '')),
--                                                      gc_precision,
--                                                      gc_alph_start)
--                                              AND RPAD (
--                                                      LOWER (
--                                                          REPLACE (
--                                                              zsadvr_alphabet_end,
--                                                              ' ',
--                                                              '')),
--                                                      gc_precision,
--                                                      gc_alph_end)
--                              OR zsadvr_alphabet_start IS NULL)
--                ORDER BY zsadvr_minor_code,
--                         zsadvr_alphabet_start,
--                         zsadvr_alphabet_end;
--
--            CURSOR advisor_vet_c (
--                pidm_in     NUMBER,
--                term_in     VARCHAR2,
--                lname_in    VARCHAR2)
--            --Get advisor assignment records based on veteran status, and alphabet
--            IS
--                  SELECT z_stu_advr_zsadvr.*,
--                         LAG (zsadvr_pidm) OVER (ORDER BY zsadvr_pidm)
--                             prev_pidm
--                    FROM z_stu_advr_zsadvr
--                   WHERE     zsadvr_term_code = term_in
--                         AND zsadvr_role_code = 'S'                --Secondary
--                         AND zsadvr_advisor_code = 'V'               --Veteran
--                         AND (   lname_in BETWEEN RPAD (
--                                                      LOWER (
--                                                          REPLACE (
--                                                              zsadvr_alphabet_start,
--                                                              ' ',
--                                                              '')),
--                                                      gc_precision,
--                                                      gc_alph_start)
--                                              AND RPAD (
--                                                      LOWER (
--                                                          REPLACE (
--                                                              zsadvr_alphabet_end,
--                                                              ' ',
--                                                              '')),
--                                                      gc_precision,
--                                                      gc_alph_end)
--                              OR zsadvr_alphabet_start IS NULL)
--                         AND EXISTS
--                                 (SELECT 'X'
--                                    FROM sgrvetn
--                                   WHERE     sgrvetn_term_code_va = term_in
--                                         --WHERE     sgrvetn_term_code_va = v_sorlcur_rec.r_term_code
--                                         AND sgrvetn_pidm = pidm_in)
--                --AND       v_sorlcur_rec.r_term_code = v_term)
--                ORDER BY zsadvr_school_code,
--                         zsadvr_program_code,
--                         zsadvr_concentration_code,
--                         zsadvr_attribute_code,
--                         zsadvr_alphabet_start,
--                         zsadvr_alphabet_end;
--
--            v_veteran_ind   BOOLEAN := FALSE;
--        BEGIN
----            IF v_web_ind = 'N'
----            THEN
--                --Removes excess records in sgradvr where there isn't a match in zsadvr (rules table)
--                DELETE sgradvr
--                 WHERE     sgradvr_advr_pidm || sgradvr_advr_code IN
--                               (SELECT sgradvr_advr_pidm || sgradvr_advr_code
--                                  FROM sgradvr s1
--                                 WHERE     s1.sgradvr_pidm = v_pidm
--                                       AND s1.sgradvr_term_code_eff =
--                                           (SELECT MAX (
--                                                       s2.sgradvr_term_code_eff)
--                                              FROM sgradvr s2
--                                             WHERE     s2.sgradvr_pidm =
--                                                       s1.sgradvr_pidm
--                                                   AND s2.sgradvr_term_code_eff <=
--                                                       v_term
--                                                   AND s2.sgradvr_prim_ind =
--                                                       'N')
--                                       AND sgradvr_prim_ind = 'N'
--                                MINUS
--                                (SELECT zsadvr_pidm || zsadvr_advisor_code
--                                   FROM z_stu_advr_zsadvr
--                                  WHERE        zsadvr_term_code =
--                                               v_ZSADVR_term
--                                           AND zsadvr_role_code = 'S'
--                                           AND zsadvr_school_code =
--                                               v_sorlcur_rec.r_coll_code
--                                           AND zsadvr_program_code =
--                                               v_sorlcur_rec.r_program
--                                           AND (   zsadvr_concentration_code
--                                                        MEMBER OF v_conc
--                                                OR zsadvr_concentration_code
--                                                       IS NULL)
--                                           AND (   v_lname BETWEEN RPAD (
--                                                                       LOWER (
--                                                                           REPLACE (
--                                                                               zsadvr_alphabet_start,
--                                                                               ' ',
--                                                                               '')),
--                                                                       gc_precision,
--                                                                       gc_alph_start)
--                                                               AND RPAD (
--                                                                       LOWER (
--                                                                           REPLACE (
--                                                                               zsadvr_alphabet_end,
--                                                                               ' ',
--                                                                               '')),
--                                                                       gc_precision,
--                                                                       gc_alph_end)
--                                                OR zsadvr_alphabet_start
--                                                       IS NULL)
--                                        OR (    zsadvr_minor_code
--                                                     MEMBER OF v_minor
--                                            AND zsadvr_term_code =
--                                                v_ZSADVR_term
--                                            AND zsadvr_role_code = 'S'
--                                            AND zsadvr_advisor_code = 'M'
--                                            AND (   v_lname BETWEEN RPAD (
--                                                                        LOWER (
--                                                                            REPLACE (
--                                                                                zsadvr_alphabet_start,
--                                                                                ' ',
--                                                                                '')),
--                                                                        gc_precision,
--                                                                        gc_alph_start)
--                                                                AND RPAD (
--                                                                        LOWER (
--                                                                            REPLACE (
--                                                                                zsadvr_alphabet_end,
--                                                                                ' ',
--                                                                                '')),
--                                                                        gc_precision,
--                                                                        gc_alph_end)
--                                                 OR zsadvr_alphabet_start
--                                                        IS NULL))
--                                 UNION
--                                 SELECT zsadvr_pidm || zsadvr_advisor_code
--                                   FROM z_stu_advr_zsadvr
--                                  WHERE        zsadvr_term_code =
--                                               v_ZSADVR_term
--                                           AND zsadvr_role_code = 'S'
--                                           AND zsadvr_school_code =
--                                               v_sorlcur_rec.r_coll_code
--                                           AND zsadvr_attribute_code
--                                                    MEMBER OF v_attr
--                                           AND (   v_lname BETWEEN RPAD (
--                                                                       LOWER (
--                                                                           REPLACE (
--                                                                               zsadvr_alphabet_start,
--                                                                               ' ',
--                                                                               '')),
--                                                                       gc_precision,
--                                                                       gc_alph_start)
--                                                               AND RPAD (
--                                                                       LOWER (
--                                                                           REPLACE (
--                                                                               zsadvr_alphabet_end,
--                                                                               ' ',
--                                                                               '')),
--                                                                       gc_precision,
--                                                                       gc_alph_end)
--                                                OR zsadvr_alphabet_start
--                                                       IS NULL)
--                                        OR (    zsadvr_minor_code
--                                                     MEMBER OF v_minor
--                                            AND zsadvr_term_code =
--                                                v_ZSADVR_term
--                                            AND zsadvr_role_code = 'S'
--                                            AND zsadvr_advisor_code = 'M' --Minor
--                                            AND (   v_lname BETWEEN RPAD (
--                                                                        LOWER (
--                                                                            REPLACE (
--                                                                                zsadvr_alphabet_start,
--                                                                                ' ',
--                                                                                '')),
--                                                                        gc_precision,
--                                                                        gc_alph_start)
--                                                                AND RPAD (
--                                                                        LOWER (
--                                                                            REPLACE (
--                                                                                zsadvr_alphabet_end,
--                                                                                ' ',
--                                                                                '')),
--                                                                        gc_precision,
--                                                                        gc_alph_end)
--                                                 OR zsadvr_alphabet_start
--                                                        IS NULL))
--                                 UNION
--                                 SELECT zsadvr_pidm || zsadvr_advisor_code
--                                   FROM z_stu_advr_zsadvr
--                                  --WHERE    zsadvr_term_code = v_ZSADVR_term
--                                  WHERE     zsadvr_term_code = v_term
--                                        AND zsadvr_advisor_code = 'V'
--                                        AND (   v_lname BETWEEN RPAD (
--                                                                    LOWER (
--                                                                        REPLACE (
--                                                                            zsadvr_alphabet_start,
--                                                                            ' ',
--                                                                            '')),
--                                                                    gc_precision,
--                                                                    gc_alph_start)
--                                                            AND RPAD (
--                                                                    LOWER (
--                                                                        REPLACE (
--                                                                            zsadvr_alphabet_end,
--                                                                            ' ',
--                                                                            '')),
--                                                                    gc_precision,
--                                                                    gc_alph_end)
--                                             OR zsadvr_alphabet_start IS NULL)
--                                        AND EXISTS
--                                                (SELECT 'X'
--                                                   FROM sgrvetn
--                                                  WHERE     sgrvetn_pidm =
--                                                            v_pidm
--                                                        AND sgrvetn_term_code_va =
--                                                            v_term) --AND    sgrvetn_term_code_va = v_sorlcur_rec.r_term_code
--                                                                   --AND    v_sorlcur_rec.r_term_code = v_term)
--                                                                   ))
--                       AND sgradvr_pidm = v_pidm
--                       AND sgradvr_term_code_eff = v_term
--                       AND sgradvr_prim_ind = 'N';
--
--                --Inserts new records into sgradvr where there is an additional record in zsadvr
--                FOR zs_rec
--                    IN ((SELECT zsadvr_pidm, zsadvr_advisor_code
--                           FROM z_stu_advr_zsadvr
--                          WHERE        zsadvr_term_code = v_ZSADVR_term
--                                   AND zsadvr_role_code = 'S'
--                                   AND zsadvr_school_code =
--                                       v_sorlcur_rec.r_coll_code
--                                   AND zsadvr_program_code =
--                                       v_sorlcur_rec.r_program
--                                   AND (   zsadvr_concentration_code
--                                                MEMBER OF v_conc
--                                        OR zsadvr_concentration_code IS NULL)
--                                   AND (   v_lname BETWEEN RPAD (
--                                                               LOWER (
--                                                                   REPLACE (
--                                                                       zsadvr_alphabet_start,
--                                                                       ' ',
--                                                                       '')),
--                                                               gc_precision,
--                                                               gc_alph_start)
--                                                       AND RPAD (
--                                                               LOWER (
--                                                                   REPLACE (
--                                                                       zsadvr_alphabet_end,
--                                                                       ' ',
--                                                                       '')),
--                                                               gc_precision,
--                                                               gc_alph_end)
--                                        OR zsadvr_alphabet_start IS NULL)
--                                OR (    zsadvr_minor_code MEMBER OF v_minor
--                                    AND zsadvr_term_code = v_ZSADVR_term
--                                    AND zsadvr_role_code = 'S'
--                                    AND zsadvr_advisor_code = 'M'
--                                    AND (   v_lname BETWEEN RPAD (
--                                                                LOWER (
--                                                                    REPLACE (
--                                                                        zsadvr_alphabet_start,
--                                                                        ' ',
--                                                                        '')),
--                                                                gc_precision,
--                                                                gc_alph_start)
--                                                        AND RPAD (
--                                                                LOWER (
--                                                                    REPLACE (
--                                                                        zsadvr_alphabet_end,
--                                                                        ' ',
--                                                                        '')),
--                                                                gc_precision,
--                                                                gc_alph_end)
--                                         OR zsadvr_alphabet_start IS NULL))
--                         UNION
--                         SELECT zsadvr_pidm, zsadvr_advisor_code
--                           FROM z_stu_advr_zsadvr
--                          WHERE        zsadvr_term_code = v_ZSADVR_term
--                                   AND zsadvr_role_code = 'S'
--                                   AND zsadvr_school_code =
--                                       v_sorlcur_rec.r_coll_code
--                                   AND zsadvr_attribute_code MEMBER OF v_attr
--                                   AND (   v_lname BETWEEN RPAD (
--                                                               LOWER (
--                                                                   REPLACE (
--                                                                       zsadvr_alphabet_start,
--                                                                       ' ',
--                                                                       '')),
--                                                               gc_precision,
--                                                               gc_alph_start)
--                                                       AND RPAD (
--                                                               LOWER (
--                                                                   REPLACE (
--                                                                       zsadvr_alphabet_end,
--                                                                       ' ',
--                                                                       '')),
--                                                               gc_precision,
--                                                               gc_alph_end)
--                                        OR zsadvr_alphabet_start IS NULL)
--                                OR (    zsadvr_minor_code MEMBER OF v_minor
--                                    AND zsadvr_term_code = v_ZSADVR_term
--                                    AND zsadvr_role_code = 'S'
--                                    AND zsadvr_advisor_code = 'M'
--                                    AND (   v_lname BETWEEN RPAD (
--                                                                LOWER (
--                                                                    REPLACE (
--                                                                        zsadvr_alphabet_start,
--                                                                        ' ',
--                                                                        '')),
--                                                                gc_precision,
--                                                                gc_alph_start)
--                                                        AND RPAD (
--                                                                LOWER (
--                                                                    REPLACE (
--                                                                        zsadvr_alphabet_end,
--                                                                        ' ',
--                                                                        '')),
--                                                                gc_precision,
--                                                                gc_alph_end)
--                                         OR zsadvr_alphabet_start IS NULL))
--                         UNION
--                         SELECT zsadvr_pidm, zsadvr_advisor_code
--                           FROM z_stu_advr_zsadvr
--                          --WHERE    zsadvr_term_code = v_ZSADVR_term
--                          WHERE     zsadvr_term_code = v_term
--                                AND zsadvr_advisor_code = 'V'
--                                AND (   v_lname BETWEEN RPAD (
--                                                            LOWER (
--                                                                REPLACE (
--                                                                    zsadvr_alphabet_start,
--                                                                    ' ',
--                                                                    '')),
--                                                            gc_precision,
--                                                            gc_alph_start)
--                                                    AND RPAD (
--                                                            LOWER (
--                                                                REPLACE (
--                                                                    zsadvr_alphabet_end,
--                                                                    ' ',
--                                                                    '')),
--                                                            gc_precision,
--                                                            gc_alph_end)
--                                     OR zsadvr_alphabet_start IS NULL)
--                                AND EXISTS
--                                        (SELECT 'X'
--                                           FROM sgrvetn
--                                          WHERE     sgrvetn_pidm = v_pidm
--                                                AND sgrvetn_term_code_va =
--                                                    v_term) --AND    sgrvetn_term_code_va = v_sorlcur_rec.r_term_code
--                                                           --AND    v_sorlcur_rec.r_term_code = v_term)
--                                                           )
--                        MINUS
--                        SELECT s1.sgradvr_advr_pidm, s1.sgradvr_advr_code
--                          FROM sgradvr s1
--                         WHERE     s1.sgradvr_pidm = v_pidm
--                               AND s1.sgradvr_term_code_eff =
--                                   (SELECT MAX (s2.sgradvr_term_code_eff)
--                                      FROM sgradvr s2
--                                     WHERE     s2.sgradvr_pidm =
--                                               s1.sgradvr_pidm
--                                           AND s2.sgradvr_term_code_eff <=
--                                               v_term
--                                           AND s2.sgradvr_prim_ind = 'N')
--                               AND s1.sgradvr_prim_ind = 'N')
--                LOOP
--                    INSERT INTO sgradvr (sgradvr_pidm,
--                                         sgradvr_term_code_eff,
--                                         sgradvr_advr_pidm,
--                                         sgradvr_advr_code,
--                                         sgradvr_prim_ind,
--                                         sgradvr_activity_date)
--                         VALUES (v_pidm,
--                                 v_term,
--                                 zs_rec.zsadvr_pidm,
--                                 zs_rec.zsadvr_advisor_code,
--                                 'N',
--                                 SYSDATE);
--
--                    --Set flag if veteran advisor is found
--                    IF zs_rec.zsadvr_advisor_code = 'V'
--                    THEN
--                        v_veteran_ind := TRUE;
--                    END IF;
--                END LOOP;
----            ELSIF v_web_ind = 'Y'
----            THEN
----                FOR school_rec
----                    IN advisor_school_c (v_pidm,
----                                         v_ZSADVR_term,
----                                         v_sorlcur_rec.r_coll_code,
----                                         v_sorlcur_rec.r_program,
----                                         v_conc,
----                                         v_attr,
----                                         v_lname)
----                LOOP
----                    IF    school_rec.prev_pidm IS NULL
----                       OR school_rec.zsadvr_pidm <> school_rec.prev_pidm
----                    THEN
----                        TWBKFRMT.P_TABLEOPEN ('DATAENTRY');
----                        HTP.P (
----                            '<caption><font size="+1"><b>Secondary Advisor(s) - School/Program/Concentration/Attribute</b></font></caption>');
----                        TWBKFRMT.P_TABLEROWOPEN;
----                        TWBKFRMT.P_TABLEHEADER ('Term');
----                        TWBKFRMT.P_TABLEHEADER ('Advisor A#');
----                        TWBKFRMT.P_TABLEHEADER ('Advisor PIDM');
----                        TWBKFRMT.P_TABLEHEADER ('Advisor name');
----                        TWBKFRMT.P_TABLEHEADER ('Advisor code');
----                        TWBKFRMT.P_TABLEHEADER ('Role code');
----                        TWBKFRMT.P_TABLEHEADER ('Attribute');
----                        TWBKFRMT.P_TABLEHEADER ('Minor');
----                        TWBKFRMT.P_TABLEHEADER ('School');
----                        TWBKFRMT.P_TABLEHEADER ('Program');
----                        TWBKFRMT.P_TABLEHEADER ('Concentration');
----                        TWBKFRMT.P_TABLEHEADER ('Alphabet start');
----                        TWBKFRMT.P_TABLEHEADER ('Alphabet end');
----                        TWBKFRMT.P_TABLEROWCLOSE;
----                        TWBKFRMT.P_TABLEROWOPEN;
----                        TWBKFRMT.P_TABLEDATALABEL (v_term);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            BANINST1.F_GETSPRIDENID (school_rec.zsadvr_pidm));
----                        TWBKFRMT.P_TABLEDATALABEL (school_rec.zsadvr_pidm);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            BANINST1.F_FORMAT_NAME (school_rec.zsadvr_pidm,
----                                                    'LF30'));
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            school_rec.zsadvr_advisor_code);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            school_rec.zsadvr_role_code);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            school_rec.zsadvr_attribute_code);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            school_rec.zsadvr_minor_code);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            school_rec.zsadvr_school_code);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            school_rec.zsadvr_program_code);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            school_rec.zsadvr_concentration_code);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            school_rec.zsadvr_alphabet_start);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            school_rec.zsadvr_alphabet_end);
----                        TWBKFRMT.P_TABLEROWCLOSE;
----                        TWBKFRMT.P_TABLECLOSE;
----                        HTP.BR;
----                    END IF;
----                END LOOP;
----
----                FOR minor_rec IN advisor_minor_c (v_pidm,
----                                                  v_ZSADVR_term,
----                                                  v_minor,
----                                                  v_lname)
----                LOOP
----                    IF    minor_rec.prev_pidm IS NULL
----                       OR minor_rec.zsadvr_pidm <> minor_rec.prev_pidm
----                    THEN
----                        TWBKFRMT.P_TABLEOPEN ('DATAENTRY');
----                        HTP.P (
----                            '<caption><font size="+1"><b>Secondary Advisor(s) - Minor</b></font></caption>');
----                        TWBKFRMT.P_TABLEROWOPEN;
----                        TWBKFRMT.P_TABLEHEADER ('Term');
----                        TWBKFRMT.P_TABLEHEADER ('Advisor A#');
----                        TWBKFRMT.P_TABLEHEADER ('Advisor PIDM');
----                        TWBKFRMT.P_TABLEHEADER ('Advisor name');
----                        TWBKFRMT.P_TABLEHEADER ('Advisor code');
----                        TWBKFRMT.P_TABLEHEADER ('Role code');
----                        TWBKFRMT.P_TABLEHEADER ('Attribute');
----                        TWBKFRMT.P_TABLEHEADER ('Minor');
----                        TWBKFRMT.P_TABLEHEADER ('School');
----                        TWBKFRMT.P_TABLEHEADER ('Program');
----                        TWBKFRMT.P_TABLEHEADER ('Concentration');
----                        TWBKFRMT.P_TABLEHEADER ('Alphabet start');
----                        TWBKFRMT.P_TABLEHEADER ('Alphabet end');
----                        TWBKFRMT.P_TABLEROWCLOSE;
----                        TWBKFRMT.P_TABLEROWOPEN;
----                        TWBKFRMT.P_TABLEDATALABEL (v_term);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            BANINST1.F_GETSPRIDENID (minor_rec.zsadvr_pidm));
----                        TWBKFRMT.P_TABLEDATALABEL (minor_rec.zsadvr_pidm);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            BANINST1.F_FORMAT_NAME (minor_rec.zsadvr_pidm,
----                                                    'LF30'));
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            minor_rec.zsadvr_advisor_code);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            minor_rec.zsadvr_role_code);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            minor_rec.zsadvr_attribute_code);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            minor_rec.zsadvr_minor_code);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            minor_rec.zsadvr_school_code);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            minor_rec.zsadvr_program_code);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            minor_rec.zsadvr_concentration_code);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            minor_rec.zsadvr_alphabet_start);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            minor_rec.zsadvr_alphabet_end);
----                        TWBKFRMT.P_TABLEROWCLOSE;
----                        TWBKFRMT.P_TABLECLOSE;
----                        HTP.BR;
----                    END IF;
----                END LOOP;
--
----                FOR vet_rec IN advisor_vet_c (v_pidm, v_term, v_lname)
----                LOOP
----                    IF    vet_rec.prev_pidm IS NULL
----                       OR vet_rec.zsadvr_pidm <> vet_rec.prev_pidm
----                    THEN
----                        TWBKFRMT.P_TABLEOPEN ('DATAENTRY');
----                        HTP.P (
----                            '<caption><font size="+1"><b>Secondary Advisor(s) - Veteran</b></font></caption>');
----                        TWBKFRMT.P_TABLEROWOPEN;
----                        TWBKFRMT.P_TABLEHEADER ('Term');
----                        TWBKFRMT.P_TABLEHEADER ('Advisor A#');
----                        TWBKFRMT.P_TABLEHEADER ('Advisor PIDM');
----                        TWBKFRMT.P_TABLEHEADER ('Advisor name');
----                        TWBKFRMT.P_TABLEHEADER ('Advisor code');
----                        TWBKFRMT.P_TABLEHEADER ('Role code');
----                        TWBKFRMT.P_TABLEHEADER ('Attribute');
----                        TWBKFRMT.P_TABLEHEADER ('Minor');
----                        TWBKFRMT.P_TABLEHEADER ('School');
----                        TWBKFRMT.P_TABLEHEADER ('Program');
----                        TWBKFRMT.P_TABLEHEADER ('Concentration');
----                        TWBKFRMT.P_TABLEHEADER ('Alphabet start');
----                        TWBKFRMT.P_TABLEHEADER ('Alphabet end');
----                        TWBKFRMT.P_TABLEROWCLOSE;
----                        TWBKFRMT.P_TABLEROWOPEN;
----                        TWBKFRMT.P_TABLEDATALABEL (v_term);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            BANINST1.F_GETSPRIDENID (vet_rec.zsadvr_pidm));
----                        TWBKFRMT.P_TABLEDATALABEL (vet_rec.zsadvr_pidm);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            BANINST1.F_FORMAT_NAME (vet_rec.zsadvr_pidm,
----                                                    'LF30'));
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            vet_rec.zsadvr_advisor_code);
----                        TWBKFRMT.P_TABLEDATALABEL (vet_rec.zsadvr_role_code);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            vet_rec.zsadvr_attribute_code);
----                        TWBKFRMT.P_TABLEDATALABEL (vet_rec.zsadvr_minor_code);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            vet_rec.zsadvr_school_code);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            vet_rec.zsadvr_program_code);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            vet_rec.zsadvr_concentration_code);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            vet_rec.zsadvr_alphabet_start);
----                        TWBKFRMT.P_TABLEDATALABEL (
----                            vet_rec.zsadvr_alphabet_end);
----                        TWBKFRMT.P_TABLEROWCLOSE;
----                        TWBKFRMT.P_TABLECLOSE;
----
----                        HTP.BR;
----                        HTP.BR;
----                    END IF;
----                END LOOP;
----            END IF;
--
--            IF v_veteran_ind
--            THEN
--                P_PROCESS_PRIMARY_ADVISOR ('Y');
--            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                    Z_STU_ADVR_MESSAGE_LOG.SAVELINE (
                        pi_calling_proc =>
                            'SZADVR_LOGIC',
                        pi_calling_section =>
                            'Secondary Advisor',
                        pi_message =>
                               'Error for student: '
                            || BANINST1.F_GETSPRIDENID (v_pidm)
                            || ', for term: '
                            || v_term
                            || ' ROLLBACK ISSUED',
                        pi_sqlerrm =>
                            SQLERRM);

                    ROLLBACK;
        END P_PROCESS_SECONDARY_ADVISOR;
    --***End 2nd nested procedure

    --Start of main procedure
    BEGIN
        --Get the most recent term for adviser rules, this keeps the rule from having to
        --be updated each term if no changes are needed
        SELECT DISTINCT zs1.zsadvr_term_code
          INTO v_ZSADVR_term
          FROM z_stu_advr_zsadvr zs1
         WHERE zs1.zsadvr_term_code = (SELECT MAX (zs2.zsadvr_term_code)
                                         FROM z_stu_advr_zsadvr zs2
                                        WHERE zs2.zsadvr_term_code <= v_term);

        --Get the current sorlcur record for the student
        v_sorlcur_ref :=
            SB_CURRICULUM.F_QUERY_CURRENT (v_pidm,
                                           'LEARNER',
                                           v_term,
                                           99,
                                           p_active_ind   => 'Y');

        FETCH v_sorlcur_ref INTO v_sorlcur_rec;

        CLOSE v_sorlcur_ref;

        IF v_sorlcur_rec.r_pidm IS NULL
        THEN
            v_sorlcur_ref :=
                SB_CURRICULUM.F_QUERY_CURRENT (v_pidm,
                                               'LEARNER',
                                               p_keyseqno     => 99,
                                               p_active_ind   => 'Y',
                                               p_eff_term     => v_term);

            FETCH v_sorlcur_ref INTO v_sorlcur_rec;

            CLOSE v_sorlcur_ref;
        END IF;

        BEGIN
            SELECT s1.sgbstdn_camp_code
              INTO v_camp_code
              FROM sgbstdn s1
             WHERE     s1.sgbstdn_pidm = v_pidm
                   AND s1.sgbstdn_term_code_eff =
                       (SELECT MAX (s2.sgbstdn_term_code_eff)
                          FROM sgbstdn s2
                         WHERE     s2.sgbstdn_pidm = v_pidm);        
--            SELECT s1.sgbstdn_camp_code
--              INTO v_camp_code
--              FROM sgbstdn s1
--             WHERE     s1.sgbstdn_pidm = v_pidm
--                   AND s1.sgbstdn_term_code_eff =
--                       (SELECT MAX (s2.sgbstdn_term_code_eff)
--                          FROM sgbstdn s2
--                         WHERE     s2.sgbstdn_pidm = v_pidm
--                               AND s2.sgbstdn_term_code_eff <= 
--                                                (SELECT MIN(term_code) FROM TABLE(casandra.f_list_activeterms)));  -- get current term.
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN                            --No campus code exists, set to null
                v_camp_code := NULL;
        END;

        --Get all sorlfos majr_codes for current concentration(s)
        SELECT sorlfos_majr_code
          BULK COLLECT INTO v_conc
          FROM sorlfos
         WHERE     sorlfos_pidm = v_pidm
               AND sorlfos_lcur_seqno = v_sorlcur_rec.r_seqno
               AND sorlfos_lfst_code = 'CONCENTRATION';

        --Get all sorlfos majr_codes for current minor(s)
        SELECT sorlfos_majr_code
          BULK COLLECT INTO v_minor
          FROM sorlfos
         WHERE     sorlfos_pidm = v_pidm
               AND sorlfos_lcur_seqno = v_sorlcur_rec.r_seqno
               AND sorlfos_lfst_code = 'MINOR';

        --Get all current attribute(s) for student
        SELECT s1.sgrsatt_atts_code att_code
          BULK COLLECT INTO v_attr
          FROM sgrsatt s1
         WHERE     s1.sgrsatt_pidm = v_pidm
               AND s1.sgrsatt_term_code_eff =
                   (SELECT MAX (s2.sgrsatt_term_code_eff)
                      FROM sgrsatt s2
                     WHERE     s2.sgrsatt_term_code_eff <= v_term
                           AND s2.sgrsatt_pidm = s1.sgrsatt_pidm);

        --Figure out if the advisor assignment rules for primary advisors is the same as the advisor already assigned for the student
--        insert into z_trevor_bennett.A_TESTING_LOG VALUES ( 'v_zsadvr_term = '||v_zsadvr_term||' v_sorlcur_rec.r_coll_code = '||v_sorlcur_rec.r_coll_code||
--                                    ' v_sorlcur_rec.r_program = '||v_sorlcur_rec.r_program||' v_pidm= '||v_pidm||' v_term = '||v_term||' v_camp_code= '||v_camp_code, SYSDATE); 
                
        SELECT COUNT (*)
          INTO v_prim_advr_count
          FROM ((SELECT *
                   FROM (  SELECT zsadvr_pidm
                             FROM z_stu_advr_zsadvr
                            WHERE     zsadvr_term_code = v_zsadvr_term
                                  AND zsadvr_role_code = 'P'         --Primary
                                  AND zsadvr_school_code =
                                      v_sorlcur_rec.r_coll_code
                                  AND zsadvr_program_code =
                                      v_sorlcur_rec.r_program
                                  AND (   zsadvr_concentration_code
                                               MEMBER OF v_conc
                                       OR zsadvr_concentration_code IS NULL)
                                  AND (   zsadvr_attribute_code
                                               MEMBER OF v_attr
                                       OR zsadvr_attribute_code IS NULL)
                                  AND (   zsadvr_campus_code = v_camp_code
                                       OR zsadvr_campus_code IS NULL)                                       
                                  AND (   v_lname BETWEEN RPAD (
                                                              LOWER (
                                                                  REPLACE (
                                                                      zsadvr_alphabet_start,
                                                                      ' ',
                                                                      '')),
                                                              gc_precision,
                                                              gc_alph_start)
                                                      AND RPAD (
                                                              LOWER (
                                                                  REPLACE (
                                                                      zsadvr_alphabet_end,
                                                                      ' ',
                                                                      '')),
                                                              gc_precision,
                                                              gc_alph_end)
                                       OR zsadvr_alphabet_start IS NULL)
                         ORDER BY zsadvr_school_code,
                                  zsadvr_program_code,
                                  zsadvr_concentration_code,
                                  zsadvr_attribute_code,
                                  zsadvr_campus_code,
                                  zsadvr_alphabet_start)
                  WHERE ROWNUM = 1));
--                MINUS
--                SELECT s1.sgradvr_advr_pidm
--                  FROM sgradvr s1
--                 WHERE     s1.sgradvr_pidm = v_pidm
--                       AND s1.sgradvr_term_code_eff =
--                           (SELECT MAX (s2.sgradvr_term_code_eff)
--                              FROM sgradvr s2
--                             WHERE     s2.sgradvr_pidm = s1.sgradvr_pidm
--                                   AND s2.sgradvr_term_code_eff <= v_term --JGE 10/18/2010 Removed
--                                                                         --                                           AND     s2.sgradvr_prim_ind = 'Y')
--                                                                         )
--                       AND s1.sgradvr_prim_ind = 'Y');

        --If count is zero then no primary advisor change is needed, now look at secondary advsior
        IF v_prim_advr_count = 0
        THEN
            --Secondary Check
            SELECT COUNT (*)
              INTO v_sec_advr_count
              FROM ((SELECT zsadvr_pidm
                       FROM z_stu_advr_zsadvr
                      WHERE        zsadvr_term_code = v_ZSADVR_term
                               AND zsadvr_role_code = 'S'
                               AND zsadvr_school_code =
                                   v_sorlcur_rec.r_coll_code
                               AND zsadvr_program_code =
                                   v_sorlcur_rec.r_program
                               AND (   zsadvr_concentration_code
                                            MEMBER OF v_conc
                                    OR zsadvr_concentration_code IS NULL)
                               AND (   v_lname BETWEEN RPAD (
                                                           LOWER (
                                                               REPLACE (
                                                                   zsadvr_alphabet_start,
                                                                   ' ',
                                                                   '')),
                                                           gc_precision,
                                                           gc_alph_start)
                                                   AND RPAD (
                                                           LOWER (
                                                               REPLACE (
                                                                   zsadvr_alphabet_end,
                                                                   ' ',
                                                                   '')),
                                                           gc_precision,
                                                           gc_alph_end)
                                    OR zsadvr_alphabet_start IS NULL)
                            OR (    zsadvr_minor_code MEMBER OF v_minor
                                AND zsadvr_term_code = v_ZSADVR_term
                                AND zsadvr_role_code = 'S'
                                AND zsadvr_advisor_code = 'M'
                                AND (   v_lname BETWEEN RPAD (
                                                            LOWER (
                                                                REPLACE (
                                                                    zsadvr_alphabet_start,
                                                                    ' ',
                                                                    '')),
                                                            gc_precision,
                                                            gc_alph_start)
                                                    AND RPAD (
                                                            LOWER (
                                                                REPLACE (
                                                                    zsadvr_alphabet_end,
                                                                    ' ',
                                                                    '')),
                                                            gc_precision,
                                                            gc_alph_end)
                                     OR zsadvr_alphabet_start IS NULL))
                     UNION
                     SELECT zsadvr_pidm
                       FROM z_stu_advr_zsadvr
                      WHERE        zsadvr_term_code = v_ZSADVR_term
                               AND zsadvr_role_code = 'S'
                               AND zsadvr_school_code =
                                   v_sorlcur_rec.r_coll_code
                               AND zsadvr_attribute_code MEMBER OF v_attr
                               AND (   v_lname BETWEEN RPAD (
                                                           LOWER (
                                                               REPLACE (
                                                                   zsadvr_alphabet_start,
                                                                   ' ',
                                                                   '')),
                                                           gc_precision,
                                                           gc_alph_start)
                                                   AND RPAD (
                                                           LOWER (
                                                               REPLACE (
                                                                   zsadvr_alphabet_end,
                                                                   ' ',
                                                                   '')),
                                                           gc_precision,
                                                           gc_alph_end)
                                    OR zsadvr_alphabet_start IS NULL)
                            OR (    zsadvr_minor_code MEMBER OF v_minor
                                AND zsadvr_term_code = v_ZSADVR_term
                                AND zsadvr_role_code = 'S'
                                AND zsadvr_advisor_code = 'M'
                                AND (   v_lname BETWEEN RPAD (
                                                            LOWER (
                                                                REPLACE (
                                                                    zsadvr_alphabet_start,
                                                                    ' ',
                                                                    '')),
                                                            gc_precision,
                                                            gc_alph_start)
                                                    AND RPAD (
                                                            LOWER (
                                                                REPLACE (
                                                                    zsadvr_alphabet_end,
                                                                    ' ',
                                                                    '')),
                                                            gc_precision,
                                                            gc_alph_end)
                                     OR zsadvr_alphabet_start IS NULL))
                     UNION
                     SELECT zsadvr_pidm
                       FROM z_stu_advr_zsadvr
                      --WHERE    zsadvr_term_code = v_ZSADVR_term
                      WHERE     zsadvr_term_code = v_term
                            AND zsadvr_advisor_code = 'V'
                            AND (   v_lname BETWEEN RPAD (
                                                        LOWER (
                                                            REPLACE (
                                                                zsadvr_alphabet_start,
                                                                ' ',
                                                                '')),
                                                        gc_precision,
                                                        gc_alph_start)
                                                AND RPAD (
                                                        LOWER (
                                                            REPLACE (
                                                                zsadvr_alphabet_end,
                                                                ' ',
                                                                '')),
                                                        gc_precision,
                                                        gc_alph_end)
                                 OR zsadvr_alphabet_start IS NULL)
                            AND EXISTS
                                    (SELECT 'X'
                                       FROM sgrvetn
                                      WHERE     sgrvetn_pidm = v_pidm
                                            AND sgrvetn_term_code_va <=
                                                v_term
                                            AND EXISTS
                                                    (SELECT 'X'
                                                       FROM sgrvetn
                                                      WHERE     sgrvetn_pidm =
                                                                v_pidm
                                                            AND sgrvetn_term_code_va =
                                                                v_term)) --AND    sgrvetn_term_code_va = v_sorlcur_rec.r_term_code
                                                                        --AND    v_sorlcur_rec.r_term_code = v_term)
                                                                        )
                    MINUS
                    SELECT s1.sgradvr_advr_pidm
                      FROM sgradvr s1
                     WHERE     s1.sgradvr_pidm = v_pidm
                           AND s1.sgradvr_term_code_eff =
                               (SELECT MAX (s2.sgradvr_term_code_eff)
                                  FROM sgradvr s2
                                 WHERE     s2.sgradvr_pidm = s1.sgradvr_pidm
                                       AND s2.sgradvr_term_code_eff <= v_term
                                       AND s2.sgradvr_prim_ind = 'N')
                           AND sgradvr_prim_ind = 'N');

            IF v_sec_advr_count = 0
            THEN
                SELECT COUNT (*)
                  INTO v_sec_advr_count
                  FROM (SELECT sgradvr_advr_pidm || sgradvr_advr_code
                          FROM sgradvr s1
                         WHERE     s1.sgradvr_pidm = v_pidm
                               AND s1.sgradvr_term_code_eff =
                                   (SELECT MAX (s2.sgradvr_term_code_eff)
                                      FROM sgradvr s2
                                     WHERE     s2.sgradvr_pidm =
                                               s1.sgradvr_pidm
                                           AND s2.sgradvr_term_code_eff <=
                                               v_term --JGE 10/18/2010 Removed
                                                     --                                                           AND       s2.sgradvr_prim_ind = 'N')
                                                     )
                               AND sgradvr_prim_ind = 'N'
                               AND (   sgradvr_advr_code <> 'V'
                                    OR sgradvr_advr_code IS NULL)
                        MINUS
                        (SELECT zsadvr_pidm || zsadvr_advisor_code
                           FROM z_stu_advr_zsadvr
                          WHERE        zsadvr_term_code = v_ZSADVR_term
                                   AND zsadvr_role_code = 'S'
                                   AND zsadvr_school_code =
                                       v_sorlcur_rec.r_coll_code
                                   AND zsadvr_program_code =
                                       v_sorlcur_rec.r_program
                                   AND (   zsadvr_concentration_code
                                                MEMBER OF v_conc
                                        OR zsadvr_concentration_code IS NULL)
                                   AND (   v_lname BETWEEN RPAD (
                                                               LOWER (
                                                                   REPLACE (
                                                                       zsadvr_alphabet_start,
                                                                       ' ',
                                                                       '')),
                                                               gc_precision,
                                                               gc_alph_start)
                                                       AND RPAD (
                                                               LOWER (
                                                                   REPLACE (
                                                                       zsadvr_alphabet_end,
                                                                       ' ',
                                                                       '')),
                                                               gc_precision,
                                                               gc_alph_end)
                                        OR zsadvr_alphabet_start IS NULL)
                                OR (    zsadvr_minor_code MEMBER OF v_minor
                                    AND zsadvr_term_code = v_ZSADVR_term
                                    AND zsadvr_role_code = 'S'
                                    AND zsadvr_advisor_code = 'M'
                                    AND (   v_lname BETWEEN RPAD (
                                                                LOWER (
                                                                    REPLACE (
                                                                        zsadvr_alphabet_start,
                                                                        ' ',
                                                                        '')),
                                                                gc_precision,
                                                                gc_alph_start)
                                                        AND RPAD (
                                                                LOWER (
                                                                    REPLACE (
                                                                        zsadvr_alphabet_end,
                                                                        ' ',
                                                                        '')),
                                                                gc_precision,
                                                                gc_alph_end)
                                         OR zsadvr_alphabet_start IS NULL))
                         UNION
                         SELECT zsadvr_pidm || zsadvr_advisor_code
                           FROM z_stu_advr_zsadvr
                          WHERE        zsadvr_term_code = v_ZSADVR_term
                                   AND zsadvr_role_code = 'S'
                                   AND zsadvr_school_code =
                                       v_sorlcur_rec.r_coll_code
                                   AND zsadvr_attribute_code MEMBER OF v_attr
                                   AND (   v_lname BETWEEN RPAD (
                                                               LOWER (
                                                                   REPLACE (
                                                                       zsadvr_alphabet_start,
                                                                       ' ',
                                                                       '')),
                                                               gc_precision,
                                                               gc_alph_start)
                                                       AND RPAD (
                                                               LOWER (
                                                                   REPLACE (
                                                                       zsadvr_alphabet_end,
                                                                       ' ',
                                                                       '')),
                                                               gc_precision,
                                                               gc_alph_end)
                                        OR zsadvr_alphabet_start IS NULL)
                                OR (    zsadvr_minor_code MEMBER OF v_minor
                                    AND zsadvr_term_code = v_ZSADVR_term
                                    AND zsadvr_role_code = 'S'
                                    AND zsadvr_advisor_code = 'M'      --Minor
                                    AND (   v_lname BETWEEN RPAD (
                                                                LOWER (
                                                                    REPLACE (
                                                                        zsadvr_alphabet_start,
                                                                        ' ',
                                                                        '')),
                                                                gc_precision,
                                                                gc_alph_start)
                                                        AND RPAD (
                                                                LOWER (
                                                                    REPLACE (
                                                                        zsadvr_alphabet_end,
                                                                        ' ',
                                                                        '')),
                                                                gc_precision,
                                                                gc_alph_end)
                                         OR zsadvr_alphabet_start IS NULL))
                         UNION
                         SELECT zsadvr_pidm || zsadvr_advisor_code
                           FROM z_stu_advr_zsadvr
                          --WHERE    zsadvr_term_code = v_ZSADVR_term
                          WHERE     zsadvr_term_code = v_term
                                AND zsadvr_advisor_code = 'V'
                                AND (   v_lname BETWEEN RPAD (
                                                            LOWER (
                                                                REPLACE (
                                                                    zsadvr_alphabet_start,
                                                                    ' ',
                                                                    '')),
                                                            gc_precision,
                                                            gc_alph_start)
                                                    AND RPAD (
                                                            LOWER (
                                                                REPLACE (
                                                                    zsadvr_alphabet_end,
                                                                    ' ',
                                                                    '')),
                                                            gc_precision,
                                                            gc_alph_end)
                                     OR zsadvr_alphabet_start IS NULL)
                                AND EXISTS
                                        (SELECT 'X'
                                           FROM sgrvetn
                                          WHERE     sgrvetn_pidm = v_pidm
                                                AND sgrvetn_term_code_va <=
                                                    v_term
                                                AND EXISTS
                                                        (SELECT 'X'
                                                           FROM sgrvetn
                                                          WHERE     sgrvetn_pidm =
                                                                    v_pidm
                                                                AND sgrvetn_term_code_va =
                                                                    v_term)) --AND    sgrvetn_term_code_va = v_sorlcur_rec.r_term_code
                                                                            --AND    v_sorlcur_rec.r_term_code = v_term)
                                                                            ));
            END IF;
        END IF;

        --Check if counts indicate either a primary or secondary adviser rule has changed,
        --if so get needed data for the student and proceed with the assignments
--        insert into z_trevor_bennett.A_TESTING_LOG VALUES ( 'v_prim_advr_count = '||v_prim_advr_count||' v_sec_advr_count = '||v_sec_advr_count, SYSDATE);        
        IF v_prim_advr_count <> 0 OR v_sec_advr_count <> 0
        THEN
            --Local nested procedures
            --All variables come from P_PROCESS_ADVISORS
            IF v_prim_advr_count <> 0
            THEN
                P_PROCESS_PRIMARY_ADVISOR;
                IF gv_primary_advisor_changed = 'Y'
                THEN
                    P_PROCESS_SECONDARY_ADVISOR;
                END IF;
            ELSIF v_sec_advr_count <> 0
            THEN
                P_PROCESS_PRIMARY_ADVISOR (pi_force_ind => 'Y');
                IF gv_primary_advisor_changed = 'Y'
                THEN
                    P_PROCESS_SECONDARY_ADVISOR;
                END IF;
            END IF;
        END IF;
    EXCEPTION
        WHEN stop_assignment
        THEN
--            IF v_web_ind = 'N'
--            THEN
                Z_STU_ADVR_MESSAGE_LOG.SAVELINE (
                pi_calling_proc =>
                    'SZADVR_LOGIC',
                pi_message =>
                       'No primary advisor rule found for student: '
                    || BANINST1.F_GETSPRIDENID (v_pidm)
                    || ', for term: '
                    || v_term);
        WHEN OTHERS
        THEN
            Z_STU_ADVR_MESSAGE_LOG.SAVELINE (
                pi_calling_proc =>
                    'SZADVR_LOGIC',
                pi_message =>
                       'Error for student: '
                    || BANINST1.F_GETSPRIDENID (v_pidm)
                    || ', for term: '
                    || v_term,
                pi_sqlerrm =>
                    SQLERRM);
    END P_PROCESS_ADVISORS;
END Z_STU_ADVR_SZADVR_LOGIC;
/