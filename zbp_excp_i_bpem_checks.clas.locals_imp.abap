CLASS lhc_BPEMChecks DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR BPEMChecks RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE BPEMChecks.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE BPEMChecks.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE BPEMChecks.

    METHODS read FOR READ
      IMPORTING keys FOR READ BPEMChecks RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK BPEMChecks.
    METHODS ValidateInvoiceOutsort FOR READ
      IMPORTING keys FOR FUNCTION BPEMChecks~ValidateInvoiceOutsort RESULT result.
    METHODS ValidateImplausibleMeterRead FOR READ
      IMPORTING keys FOR FUNCTION BPEMChecks~ValidateImplausibleMeterRead RESULT result.
    METHODS ValidateExceptions FOR READ
      IMPORTING keys FOR FUNCTION BPEMChecks~ValidateExceptions RESULT result.

    CLASS-DATA: mt_bpem_checks_exclude TYPE STANDARD TABLE OF zexcp_bpem_check.

ENDCLASS.

CLASS lhc_BPEMChecks IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD create.
    DATA: lt_bpem_checks     TYPE STANDARD TABLE OF zexcp_bpem_check,
          lt_new_bpem_checks TYPE STANDARD TABLE OF zexcp_bpem_check.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<lfs_entities>).
      lt_bpem_checks = VALUE #( BASE lt_bpem_checks
                                ( key_id = <lfs_entities>-keyid ) ).
    ENDLOOP.

    IF lt_bpem_checks IS NOT INITIAL.
      SELECT key_id
        FROM zexcp_bpem_check
         FOR ALL ENTRIES IN @lt_bpem_checks
       WHERE key_id EQ @lt_bpem_checks-key_id
        INTO TABLE @DATA(lt_bpem_chks).
      IF sy-subrc EQ 0.
        LOOP AT lt_bpem_chks ASSIGNING FIELD-SYMBOL(<lfs_bpem_chks>).

          mt_bpem_checks_exclude = VALUE #( BASE mt_bpem_checks_exclude
                                           ( key_id = <lfs_bpem_chks>-key_id ) ).

*          failed-BPEMChecks = VALUE #( BASE failed-BPEMChecks
*                                          ( KeyId = <lfs_bpem_chks>-key_id ) ).
*
*          reported-BPEMChecks = VALUE #( BASE reported-BPEMChecks
*                                              ( KeyId = <lfs_bpem_chks>-Key_id
*                                                %msg  = new_message(
*                                                          id       = '00'
*                                                          number   = '00'
*                                                          v1       = 'Check already exist'
*                                                          severity = if_abap_behv_message=>severity-error ) ) ).
        ENDLOOP.
      ENDIF.
    ENDIF.

    LOOP AT entities ASSIGNING <lfs_entities>.

      IF NOT line_exists( mt_bpem_checks_exclude[ Key_Id = <lfs_entities>-keyid ] ).

        mapped-BPEMChecks = VALUE #( BASE mapped-BPEMChecks
                                            ( %cid  = <lfs_entities>-%cid
                                              keyid = <lfs_entities>-keyid   ) ).

        lt_new_bpem_checks = VALUE #( BASE lt_new_bpem_checks ( VALUE #( BASE CORRESPONDING #( <lfs_entities> MAPPING FROM ENTITY USING CONTROL )
                                     key_id     = <lfs_entities>-keyid
                                     created_by = cl_abap_context_info=>get_user_technical_name( )
                                     created_on = cl_abap_context_info=>get_system_date( ) ) ) ).

      ENDIF.
    ENDLOOP.

    IF lt_new_bpem_checks IS NOT INITIAL.
      MODIFY zexcp_bpem_check FROM TABLE @lt_new_bpem_checks.
    ENDIF.
  ENDMETHOD.

  METHOD update.
    DATA(lv_uname) = cl_abap_context_info=>get_user_technical_name( ).
    DATA(lv_datum) = cl_abap_context_info=>get_system_date( ).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<lfs_entity>).

      UPDATE zexcp_bpem_check SET is_active     = @<lfs_entity>-isactive,
                                  is_maintained = @<lfs_entity>-ismaintained,
                                  changed_by    = @lv_uname,
                                  changed_on    = @lv_datum
                            WHERE key_id EQ @<lfs_entity>-keyid.
    ENDLOOP.
  ENDMETHOD.

  METHOD delete.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD ValidateInvoiceOutsort.
    DATA(lo_exceptions) = NEW zcl_uc_excp_exceptions_rpa( ).
    lo_exceptions->invoice_outsort( ).
  ENDMETHOD.

  METHOD ValidateImplausibleMeterRead.
    DATA(lo_exceptions) = NEW zcl_uc_excp_exceptions_rpa( ).
    lo_exceptions->implausible_meter( ).
  ENDMETHOD.

  METHOD ValidateExceptions.
    DATA(lo_exceptions) = NEW zcl_uc_excp_exceptions_rpa( ).
    lo_exceptions->exceptions_validation( ).
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZEXCP_I_BPEM_CHECKS DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZEXCP_I_BPEM_CHECKS IMPLEMENTATION.

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
