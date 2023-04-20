CLASS lhc_HistPercent DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR HistPercent RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE HistPercent.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE HistPercent.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE HistPercent.

    METHODS read FOR READ
      IMPORTING keys FOR READ HistPercent RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK HistPercent.

    CLASS-DATA: mt_percent_exclude TYPE STANDARD TABLE OF zexcp_hist_perc.

ENDCLASS.

CLASS lhc_HistPercent IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD create.
    DATA: lt_percent     TYPE STANDARD TABLE OF zexcp_hist_perc,
          lt_new_percent TYPE STANDARD TABLE OF zexcp_hist_perc.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<lfs_entities>).
      lt_percent = VALUE #( BASE lt_percent
                              ( client_id = <lfs_entities>-ClientId ) ).
    ENDLOOP.

    IF lt_percent IS NOT INITIAL.
        SELECT client_id
          FROM zexcp_hist_perc
           FOR ALL ENTRIES IN @lt_percent
         WHERE client_id EQ @lt_percent-client_id
          INTO TABLE @DATA(lt_perc).
        IF sy-subrc EQ 0.
          LOOP AT lt_perc ASSIGNING FIELD-SYMBOL(<lfs_percent>).
            mt_percent_exclude = VALUE #( BASE mt_percent_exclude
                                            ( client_id = <lfs_percent>-client_id ) ).

*            failed-histpercent = VALUE #( BASE failed-histpercent
*                                         ( clientid = <lfs_percent>-client_id ) ).
*
*            reported-histpercent = VALUE #( BASE reported-histpercent
*                                           ( clientid = <lfs_percent>-client_id
*                                             %msg     = new_message(
*                                                          id       = '00'
*                                                          number   = '00'
*                                                          v1       = 'Percentage already exist'
*                                                          severity = if_abap_behv_message=>severity-error ) ) ).
          ENDLOOP.
        ENDIF.
    ENDIF.

    LOOP AT entities ASSIGNING <lfs_entities>.
      IF NOT line_exists( mt_percent_exclude[ client_id = <lfs_entities>-clientid ] ).

        mapped-histpercent = VALUE #( BASE mapped-histpercent
                                     ( %cid     = <lfs_entities>-%cid
                                       clientid = <lfs_entities>-clientid   ) ).
        lt_new_percent = VALUE #( BASE lt_new_percent ( VALUE #( BASE CORRESPONDING #( <lfs_entities> MAPPING FROM ENTITY USING CONTROL )
                                    client_id  = '123'
                                    created_by = cl_abap_context_info=>get_user_technical_name( )
                                    created_on = cl_abap_context_info=>get_system_date( ) ) ) ).

      ENDIF.
    ENDLOOP.

    IF lt_new_percent IS NOT INITIAL.
      MODIFY zexcp_hist_perc FROM TABLE @lt_new_percent.
    ENDIF.
  ENDMETHOD.

  METHOD update.
    DATA(lv_uname) = cl_abap_context_info=>get_user_technical_name( ).
    DATA(lv_datum) = cl_abap_context_info=>get_system_date( ).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<lfs_entity>).

      UPDATE zexcp_hist_perc SET from_prcnt = @<lfs_entity>-fromprcnt,
                                 to_prcnt   = @<lfs_entity>-toprcnt,
                                 changed_by = @lv_uname,
                                 changed_on = @lv_datum
                           WHERE client_id EQ @<lfs_entity>-clientid.
    ENDLOOP.
  ENDMETHOD.

  METHOD delete.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZEXCP_I_HIST_PERC DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZEXCP_I_HIST_PERC IMPLEMENTATION.

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
