CLASS lhc_HistCheckGrp DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR HistCheckGrp RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE HistCheckGrp.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE HistCheckGrp.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE HistCheckGrp.

    METHODS read FOR READ
      IMPORTING keys FOR READ HistCheckGrp RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK HistCheckGrp.

    CLASS-DATA: mt_checkgrp_exclude TYPE STANDARD TABLE OF zexcp_hist_io.

ENDCLASS.

CLASS lhc_HistCheckGrp IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD create.
    DATA: lt_checkgrp     TYPE STANDARD TABLE OF zexcp_hist_io,
          lt_new_checkgrp TYPE STANDARD TABLE OF zexcp_hist_io.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<lfs_entities>).
      lt_checkgrp = VALUE #( BASE lt_checkgrp
                                ( check_grp = <lfs_entities>-CheckGrp ) ).
    ENDLOOP.

    IF lt_checkgrp IS NOT INITIAL.
      SELECT check_grp
        FROM zexcp_hist_io
         FOR ALL ENTRIES IN @lt_checkgrp
       WHERE check_grp EQ @lt_checkgrp-check_grp
        INTO TABLE @DATA(lt_checkgroup).
      IF sy-subrc EQ 0.
        LOOP AT lt_checkgroup ASSIGNING FIELD-SYMBOL(<lfs_checkgrp>).
          mt_checkgrp_exclude = VALUE #( BASE mt_checkgrp_exclude
                                           ( check_grp = <lfs_checkgrp>-check_grp ) ).

*          failed-histcheckgrp = VALUE #( BASE failed-histcheckgrp
*                                              ( checkgrp = <lfs_checkgrp>-check_grp ) ).
*
*          reported-histcheckgrp = VALUE #( BASE reported-histcheckgrp
*                                              ( checkgrp = <lfs_checkgrp>-check_grp
*                                                %msg     = new_message(
*                                                             id       = '00'
*                                                             number   = '00'
*                                                             v1       = 'Check group already exist'
*                                                             severity = if_abap_behv_message=>severity-error ) ) ).
        ENDLOOP.
      ENDIF.
    ENDIF.

    LOOP AT entities ASSIGNING <lfs_entities>.
      IF NOT line_exists( mt_checkgrp_exclude[ check_grp = <lfs_entities>-checkgrp ] ).

        mapped-histcheckgrp = VALUE #( BASE mapped-histcheckgrp
                                            ( %cid     = <lfs_entities>-%cid
                                              checkgrp = <lfs_entities>-checkgrp ) ).
        lt_new_checkgrp = VALUE #( BASE lt_new_checkgrp ( VALUE #( BASE CORRESPONDING #( <lfs_entities> MAPPING FROM ENTITY USING CONTROL )
                                     check_grp  = <lfs_entities>-checkgrp
                                     created_by = cl_abap_context_info=>get_user_technical_name( )
                                     created_on = cl_abap_context_info=>get_system_date( ) ) ) ).

      ENDIF.
    ENDLOOP.

    IF lt_new_checkgrp IS NOT INITIAL.
      MODIFY zexcp_hist_io FROM TABLE @lt_new_checkgrp.
    ENDIF.
  ENDMETHOD.

  METHOD update.
  ENDMETHOD.

  METHOD delete.
    DATA: lt_checkgrp TYPE STANDARD TABLE OF zexcp_hist_io.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<lfs_key>).
      lt_checkgrp = VALUE #( BASE lt_checkgrp (
                                check_grp = <lfs_key>-checkgrp  ) ).
      mapped-histcheckgrp = VALUE #( BASE mapped-histcheckgrp (
                                          checkgrp = <lfs_key>-checkgrp   ) ).
    ENDLOOP.

    SELECT *
      FROM zexcp_hist_io
       FOR ALL ENTRIES IN @lt_checkgrp
     WHERE check_grp EQ @lt_checkgrp-check_grp
      INTO TABLE @DATA(lt_checkgrp_del).
    IF sy-subrc EQ 0.
      DELETE zexcp_hist_io FROM TABLE @lt_checkgrp_del.
    ENDIF.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZEXCP_I_HIST_IO DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZEXCP_I_HIST_IO IMPLEMENTATION.

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
