CLASS lhc_IMRRate DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR IMRRate RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE IMRRate.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE IMRRate.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE IMRRate.

    METHODS read FOR READ
      IMPORTING keys FOR READ IMRRate RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK IMRRate.

    CLASS-DATA: mt_rates_exclude TYPE STANDARD TABLE OF zexcp_imr_rates.

ENDCLASS.

CLASS lhc_IMRRate IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD create.
  DATA: lt_rates     TYPE STANDARD TABLE OF zexcp_imr_rates,
        lt_new_rates TYPE STANDARD TABLE OF zexcp_imr_rates.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<lfs_entities>).
      lt_rates = VALUE #( BASE lt_rates
                            ( rate_category = <lfs_entities>-RateCategory
                              season        = <lfs_entities>-Season ) ).
    ENDLOOP.

    IF lt_rates IS NOT INITIAL.
        SELECT rate_category,
               season
          FROM zexcp_imr_rates
           FOR ALL ENTRIES IN @lt_rates
         WHERE rate_category EQ @lt_rates-rate_category
           AND season        EQ @lt_rates-season
          INTO TABLE @DATA(lt_ratecategory).
        IF sy-subrc EQ 0.
          LOOP AT lt_ratecategory ASSIGNING FIELD-SYMBOL(<lfs_rates>).
            mt_rates_exclude = VALUE #( BASE mt_rates_exclude
                                          ( rate_category = <lfs_rates>-rate_category
                                            season        = <lfs_rates>-season ) ).

*            failed-imrrate = VALUE #( BASE failed-imrrate
*                                         ( ratecategory = <lfs_rates>-rate_category
*                                           season       = <lfs_rates>-season ) ).
*
*            reported-imrrate = VALUE #( BASE reported-imrrate
*                                           ( ratecategory = <lfs_rates>-rate_category
*                                             season       = <lfs_rates>-season
*                                             %msg         = new_message(
*                                                              id       = '00'
*                                                              number   = '00'
*                                                              v1       = 'Rate category already exist'
*                                                              severity = if_abap_behv_message=>severity-error ) ) ).
          ENDLOOP.
        ENDIF.
    ENDIF.

    LOOP AT entities ASSIGNING <lfs_entities>.
      IF NOT line_exists( mt_rates_exclude[ rate_category = <lfs_entities>-ratecategory
                                            season        = <lfs_entities>-season ] ).

        mapped-imrrate = VALUE #( BASE mapped-imrrate
                                     ( %cid         = <lfs_entities>-%cid
                                       ratecategory = <lfs_entities>-ratecategory
                                       season       = <lfs_entities>-season ) ).
        lt_new_rates = VALUE #( BASE lt_new_rates ( VALUE #( BASE CORRESPONDING #( <lfs_entities> MAPPING FROM ENTITY USING CONTROL )
                                  rate_category = <lfs_entities>-ratecategory
                                  season        = <lfs_entities>-season
                                  created_by    = cl_abap_context_info=>get_user_technical_name( )
                                  created_on    = cl_abap_context_info=>get_system_date( ) ) ) ).

      ENDIF.
    ENDLOOP.

    IF lt_new_rates IS NOT INITIAL.
      MODIFY zexcp_imr_rates FROM TABLE @lt_new_rates.
    ENDIF.
  ENDMETHOD.

  METHOD update.
    DATA(lv_uname) = cl_abap_context_info=>get_user_technical_name( ).
    DATA(lv_datum) = cl_abap_context_info=>get_system_date( ).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<lfs_entity>).
      UPDATE zexcp_imr_rates SET monthly_ave  = @<lfs_entity>-monthlyave,
                                 tolerance_h  = @<lfs_entity>-toleranceh,
                                 tolerance_l  = @<lfs_entity>-tolerancel,
                                 winter_start = @<lfs_entity>-winterstart,
                                 winter_end   = @<lfs_entity>-winterend,
                                 summer_start = @<lfs_entity>-summerstart,
                                 summer_end   = @<lfs_entity>-summerend,
                                 changed_by   = @lv_uname,
                                 changed_on   = @lv_datum
                           WHERE rate_category EQ @<lfs_entity>-ratecategory
                             AND season        EQ @<lfs_entity>-season.
    ENDLOOP.
  ENDMETHOD.

  METHOD delete.
    DATA: lt_ratecategory TYPE STANDARD TABLE OF zexcp_imr_rates.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<lfs_key>).
      lt_ratecategory = VALUE #( BASE lt_ratecategory (
                                 rate_category = <lfs_key>-ratecategory
                                 season        = <lfs_key>-season ) ).
      mapped-imrrate = VALUE #( BASE mapped-imrrate (
                                ratecategory = <lfs_key>-ratecategory
                                season       = <lfs_key>-season ) ).
    ENDLOOP.

    SELECT *
      FROM zexcp_imr_rates
       FOR ALL ENTRIES IN @lt_ratecategory
     WHERE rate_category EQ @lt_ratecategory-rate_category
      INTO TABLE @DATA(lt_rate_del).
    IF sy-subrc EQ 0.
      DELETE zexcp_imr_rates FROM TABLE @lt_rate_del.
    ENDIF.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZEXCP_I_IMR_RATES DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZEXCP_I_IMR_RATES IMPLEMENTATION.

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
