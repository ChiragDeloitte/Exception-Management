CLASS lhc_IMRCategory DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR IMRCategory RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE IMRCategory.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE IMRCategory.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE IMRCategory.

    METHODS read FOR READ
      IMPORTING keys FOR READ IMRCategory RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK IMRCategory.

    CLASS-DATA: mt_category_exclude TYPE STANDARD TABLE OF zexcp_imr_cat.

ENDCLASS.

CLASS lhc_IMRCategory IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD create.
    DATA: lt_category     TYPE STANDARD TABLE OF zexcp_imr_cat,
          lt_new_category TYPE STANDARD TABLE OF zexcp_imr_cat.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<lfs_entities>).
      lt_category = VALUE #( BASE lt_category
                                ( category = <lfs_entities>-Category ) ).
    ENDLOOP.

    IF lt_category IS NOT INITIAL.
        SELECT category
          FROM zexcp_imr_cat
           FOR ALL ENTRIES IN @lt_category
         WHERE category EQ @lt_category-category
          INTO TABLE @DATA(lt_cat).
        IF sy-subrc EQ 0.
          LOOP AT lt_cat ASSIGNING FIELD-SYMBOL(<lfs_category>).
            mt_category_exclude = VALUE #( BASE mt_category_exclude
                                             ( category = <lfs_category>-category ) ).

*            failed-imrcategory = VALUE #( BASE failed-imrcategory
*                                                ( category = <lfs_category>-category ) ).
*
*            reported-imrcategory = VALUE #( BASE reported-imrcategory
*                                                ( category = <lfs_category>-category
*                                                  %msg     = new_message(
*                                                               id       = '00'
*                                                               number   = '00'
*                                                               v1       = 'Category already exist'
*                                                               severity = if_abap_behv_message=>severity-error ) ) ).
          ENDLOOP.
        ENDIF.
    ENDIF.

    LOOP AT entities ASSIGNING <lfs_entities>.
      IF NOT line_exists( mt_category_exclude[ category = <lfs_entities>-category ] ).

        mapped-imrcategory = VALUE #( BASE mapped-imrcategory
                                            ( %cid     = <lfs_entities>-%cid
                                              category = <lfs_entities>-category ) ).
        lt_new_category = VALUE #( BASE lt_new_category ( VALUE #( BASE CORRESPONDING #( <lfs_entities> MAPPING FROM ENTITY USING CONTROL )
                                     category   = <lfs_entities>-category
                                     created_by = cl_abap_context_info=>get_user_technical_name( )
                                     created_on = cl_abap_context_info=>get_system_date( ) ) ) ).

      ENDIF.
    ENDLOOP.

    IF lt_new_category IS NOT INITIAL.
      MODIFY zexcp_imr_cat FROM TABLE @lt_new_category.
    ENDIF.
  ENDMETHOD.

  METHOD update.
  ENDMETHOD.

  METHOD delete.
    DATA: lt_category TYPE STANDARD TABLE OF zexcp_imr_cat.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<lfs_key>).
      lt_category = VALUE #( BASE lt_category (
                                category = <lfs_key>-category ) ).
      mapped-imrcategory = VALUE #( BASE mapped-imrcategory (
                                      category = <lfs_key>-category ) ).
    ENDLOOP.

    SELECT *
      FROM zexcp_imr_cat
       FOR ALL ENTRIES IN @lt_category
     WHERE category EQ @lt_category-category
      INTO TABLE @DATA(lt_category_del).
    IF sy-subrc EQ 0.
      DELETE zexcp_imr_cat FROM TABLE @lt_category_del.
    ENDIF.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZEXCP_I_IMR_CAT DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZEXCP_I_IMR_CAT IMPLEMENTATION.

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
