DROP TRIGGER BANINST1.Z_STU_ADVR_ZSADVR_SURROGATE_ID;

CREATE OR REPLACE TRIGGER BANINST1.Z_STU_ADVR_ZSADVR_SURROGATE_ID
    before insert or update on BANINST1.Z_STU_ADVR_ZSADVR
    for each row
begin
    if inserting then
        if :new.zsadvr_surrogate_id is null then
            :new.zsadvr_surrogate_id := Z_STU_ADVR_ZSADVR_SURROGATE_ID_SEQUENCE.nextVal();
        end if;
        :new.zsadvr_version := 0;
    else
        if :new.zsadvr_version = :old.zsadvr_version then
            :new.zsadvr_version := :old.zsadvr_version + 1;
        end if;
    end if;
end;
/
