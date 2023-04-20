CLASS lhc_OutsortAmount DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR OutsortAmount RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE OutsortAmount.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE OutsortAmount.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE OutsortAmount.

    METHODS read FOR READ
      IMPORTING keys FOR READ OutsortAmount RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK OutsortAmount.

    CLASS-DATA: mt_amount_exclude TYPE STANDARD TABLE OF zexcp_outsortamt.

ENDCLASS.

CLASS lhc_OutsortAmount IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD create.
    DATA: lt_amount     TYPE STANDARD TABLE OF zexcp_outsortamt,
          lt_new_amount TYPE STANDARD TABLE OF zexcp_outsortamt.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<lfs_entities>).
      lt_amount = VALUE #( BASE lt_amount
                             ( ctype = <lfs_entities>-Ctype ) ).
    ENDLOOP.

    IF lt_amount IS NOT INITIAL.
      SELECT ctype
        FROM zexcp_outsortamt
         FOR ALL ENTRIES IN @lt_amount
       WHERE ctype EQ @lt_amount-ctype
        INTO TABLE @DATA(lt_amnt).
      IF sy-subrc EQ 0.
        LOOP AT lt_amnt ASSIGNING FIELD-SYMBOL(<lfs_amount>).
          mt_amount_exclude = VALUE #( BASE mt_amount_exclude
                                         ( ctype = <lfs_amount>-ctype ) ).

*          failed-outsortamount = VALUE #( BASE failed-outsortamount
*                                              ( ctype = <lfs_amount>-ctype ) ).
*
*          reported-outsortamount = VALUE #( BASE reported-outsortamount
*                                              ( ctype    = <lfs_amount>-ctype
*                                                %msg     = new_message(
*                                                             id       = '00'
*                                                             number   = '00'
*                                                             v1       = |{ <lfs_amount>-ctype } amount already exist|
*                                                             severity = if_abap_behv_message=>severity-error ) ) ).
        ENDLOOP.
      ENDIF.
    ENDIF.

    LOOP AT entities ASSIGNING <lfs_entities>.
      IF NOT line_exists( mt_amount_exclude[ ctype = <lfs_entities>-ctype ] ).

        mapped-outsortamount = VALUE #( BASE mapped-outsortamount
                                          ( %cid  = <lfs_entities>-%cid
                                            ctype = <lfs_entities>-ctype   ) ).
        lt_new_amount = VALUE #( BASE lt_new_amount ( VALUE #( BASE CORRESPONDING #( <lfs_entities> MAPPING FROM ENTITY USING CONTROL )
                                   ctype      = <lfs_entities>-ctype
                                   created_by = cl_abap_context_info=>get_user_technical_name( )
                                   created_on = cl_abap_context_info=>get_system_date( ) ) ) ).

      ENDIF.
    ENDLOOP.

    IF lt_new_amount IS NOT INITIAL.
      MODIFY zexcp_outsortamt FROM TABLE @lt_new_amount.
    ENDIF.
  ENDMETHOD.

  METHOD update.
    DATA(lv_uname) = cl_abap_context_info=>get_user_technical_name( ).
    DATA(lv_datum) = cl_abap_context_info=>get_system_date( ).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<lfs_entity>).
      UPDATE zexcp_outsortamt SET from_amnt  = @<lfs_entity>-fromamnt,
                                  to_amnt    = @<lfs_entity>-toamnt,
                                  changed_by = @lv_uname,
                                  changed_on = @lv_datum
                            WHERE ctype EQ @<lfs_entity>-ctype.
    ENDLOOP.
  ENDMETHOD.

  METHOD delete.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZEXCP_I_OUTSORT_AMNT DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZEXCP_I_OUTSORT_AMNT IMPLEMENTATION.

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
