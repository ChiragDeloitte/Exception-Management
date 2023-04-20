CLASS lhc_OutsortCheckGrp DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR OutsortCheckGrp RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE OutsortCheckGrp.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE OutsortCheckGrp.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE OutsortCheckGrp.

    METHODS read FOR READ
      IMPORTING keys FOR READ OutsortCheckGrp RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK OutsortCheckGrp.

    CLASS-DATA: mt_checkgrp_exclude TYPE STANDARD TABLE OF zexcp_outsort_io.
ENDCLASS.

CLASS lhc_OutsortCheckGrp IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD create.
    DATA: lt_checkgrp     TYPE STANDARD TABLE OF zexcp_outsort_io,
          lt_new_checkgrp TYPE STANDARD TABLE OF zexcp_outsort_io.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<lfs_entities>).
      lt_checkgrp = VALUE #( BASE lt_checkgrp
                                ( check_grp = <lfs_entities>-CheckGrp
                                  ctype     = <lfs_entities>-Ctype ) ).
    ENDLOOP.

    IF lt_checkgrp IS NOT INITIAL.
      SELECT check_grp,
             ctype
        FROM zexcp_outsort_io
         FOR ALL ENTRIES IN @lt_checkgrp
       WHERE check_grp EQ @lt_checkgrp-check_grp
         AND ctype     EQ @lt_checkgrp-ctype
        INTO TABLE @DATA(lt_checkgroup).
      IF sy-subrc EQ 0.
        LOOP AT lt_checkgroup ASSIGNING FIELD-SYMBOL(<lfs_checkgrp>).
          mt_checkgrp_exclude = VALUE #( BASE mt_checkgrp_exclude
                                           ( check_grp = <lfs_checkgrp>-check_grp
                                             ctype    = <lfs_checkgrp>-ctype ) ).

*          failed-outsortcheckgrp = VALUE #( BASE failed-outsortcheckgrp
*                                              ( checkgrp = <lfs_checkgrp>-check_grp
*                                                ctype    = <lfs_checkgrp>-ctype ) ).
*
*          reported-outsortcheckgrp = VALUE #( BASE reported-outsortcheckgrp
*                                              ( checkgrp = <lfs_checkgrp>-check_grp
*                                                ctype    = <lfs_checkgrp>-ctype
*                                                %msg     = new_message(
*                                                             id       = '00'
*                                                             number   = '00'
*                                                             v1       = 'Check group already exist'
*                                                             severity = if_abap_behv_message=>severity-error ) ) ).
        ENDLOOP.
      ENDIF.
    ENDIF.

    LOOP AT entities ASSIGNING <lfs_entities>.
      IF NOT line_exists( mt_checkgrp_exclude[ check_grp = <lfs_entities>-checkgrp
                                               ctype     = <lfs_entities>-ctype ] ).

        mapped-outsortcheckgrp = VALUE #( BASE mapped-outsortcheckgrp
                                            ( %cid     = <lfs_entities>-%cid
                                              checkgrp = <lfs_entities>-checkgrp
                                              ctype    = <lfs_entities>-ctype   ) ).
        lt_new_checkgrp = VALUE #( BASE lt_new_checkgrp ( VALUE #( BASE CORRESPONDING #( <lfs_entities> MAPPING FROM ENTITY USING CONTROL )
                                     check_grp  = <lfs_entities>-checkgrp
                                     ctype      = <lfs_entities>-ctype
                                     created_by = cl_abap_context_info=>get_user_technical_name( )
                                     created_on = cl_abap_context_info=>get_system_date( ) ) ) ).
      ENDIF.
    ENDLOOP.

    IF lt_new_checkgrp IS NOT INITIAL.
      MODIFY zexcp_outsort_io FROM TABLE @lt_new_checkgrp.
    ENDIF.
  ENDMETHOD.

  METHOD update.
  ENDMETHOD.

  METHOD delete.
    DATA: lt_checkgrp TYPE STANDARD TABLE OF zexcp_outsort_io.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<lfs_key>).
      lt_checkgrp = VALUE #( BASE lt_checkgrp (
                                check_grp = <lfs_key>-checkgrp
                                ctype     = <lfs_key>-ctype    ) ).
      mapped-outsortcheckgrp = VALUE #( BASE mapped-outsortcheckgrp (
                                            checkgrp = <lfs_key>-checkgrp
                                            ctype    = <lfs_key>-ctype    ) ).
    ENDLOOP.

    SELECT *
      FROM zexcp_outsort_io
       FOR ALL ENTRIES IN @lt_checkgrp
     WHERE check_grp EQ @lt_checkgrp-check_grp
      INTO TABLE @DATA(lt_checkgrp_del).
    IF sy-subrc EQ 0.
      DELETE zexcp_outsort_io FROM TABLE @lt_checkgrp_del.
    ENDIF.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZEXCP_I_OUTSORT_IO DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZEXCP_I_OUTSORT_IO IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.
  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
