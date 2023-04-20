CLASS zcl_uc_excp_exceptions_rpa DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_contractaccnt,
        vkont TYPE zzexcp_i_utilitiesinvoicingdoc-ContractAccount,
        budat TYPE zzexcp_i_utilitiesinvoicingdoc-PostingDate,
        betrw TYPE string,
      END OF ty_contractaccnt,

      BEGIN OF ty_discdoc,
        status     TYPE string,
        refobjtype TYPE string,
        refobjkey  TYPE string,
      END OF ty_discdoc,

      BEGIN OF ty_int_mtr_docid,
        meterreadingdocument TYPE zzexcp_c_meterreadingdocumenth-MeterReadingDocument,
      END OF ty_int_mtr_docid,

      BEGIN OF ty_imr_rates,
        rate_category TYPE zexcp_imr_rates-rate_category,
        season        TYPE zexcp_imr_rates-season,
        monthly_ave   TYPE zexcp_imr_rates-monthly_ave,
        tolerance_h   TYPE zexcp_imr_rates-tolerance_h,
        tolerance_l   TYPE zexcp_imr_rates-tolerance_l,
        winter_start  TYPE zexcp_imr_rates-winter_start,
        winter_end    TYPE zexcp_imr_rates-winter_end,
        summer_start  TYPE zexcp_imr_rates-summer_start,
        summer_end    TYPE zexcp_imr_rates-summer_end,
      END OF ty_imr_rates,

      BEGIN OF ty_imr_case,
        category TYPE zexcp_imr_cat-category,
      END OF ty_imr_case,

      BEGIN OF gs_contract,
        sign(1)   TYPE c,
        option(2) TYPE c,
        low       TYPE zzexcp_i_contractaccountpartne-contractaccount,
      END OF gs_contract,

      tt_contractaccnt TYPE STANDARD TABLE OF ty_contractaccnt,
      tt_discdoc       TYPE STANDARD TABLE OF ty_discdoc,
      tt_int_mtr_docid TYPE STANDARD TABLE OF ty_int_mtr_docid,
      tt_imr_rates     TYPE STANDARD TABLE OF ty_imr_rates,
      tt_imr_case      TYPE STANDARD TABLE OF ty_imr_case.

    CONSTANTS:
      BEGIN OF mc_invoice_outsort,
        printdoc  TYPE zcl_uc_isu_c4c_emma_case-main_object_type VALUE 'PRINTDOC',
        invoutchk TYPE zexcp_bpem_check-key_id VALUE 'INVOUTCHK',
        crdtinv   TYPE zexcp_bpem_check-key_id VALUE 'CRDTINV',
        impmr     TYPE zexcp_bpem_check-key_id VALUE 'IMPMR',
        monthsave TYPE zexcp_bpem_check-key_id VALUE '12MOS',
        hundred   TYPE zzexcp_i_utilitiesinvoicingdoc-totalamountintransactioncrcy VALUE '100.00',
      END OF mc_invoice_outsort,

      BEGIN OF mc_implausible,
        i(1)       TYPE c VALUE 'I',
        bw(2)      TYPE c VALUE 'BT',
        eq(2)      TYPE c VALUE 'EQ',
        dot(1)     TYPE c VALUE '.',
        one(1)     TYPE c VALUE '1',
        kwh(3)     TYPE c VALUE 'KWH',
        dash(1)    TYPE c VALUE '-',
        comma(1)   TYPE c VALUE ',',
        instln(6)  TYPE c VALUE 'INSTLN',
        Winter(6)  TYPE c VALUE 'Winter',
        Summer(6)  TYPE c VALUE 'Summer',
        ones       TYPE zzexcp_c_meterreadingdocumenth-MeterReadingReason VALUE '01',
        two        TYPE zzexcp_c_meterreadingdocumenth-MeterReadingReason VALUE '02',
        three      TYPE zzexcp_c_meterreadingdocumenth-MeterReadingReason VALUE '03',
        six        TYPE zzexcp_c_meterreadingdocumenth-MeterReadingReason VALUE '06',
        mtrreaddoc TYPE zcl_uc_isu_c4c_emma_case-main_object_type VALUE 'MTRREADDOC',
      END OF mc_implausible.

    METHODS exceptions_validation.
    METHODS implausible_meter.
    METHODS invoice_outsort.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS case_category_cred_io
      EXPORTING
        er_res_category  TYPE any
        er_nres_category TYPE any.
    METHODS check_group_cred_io
      EXPORTING
        er_res_checkgrp  TYPE any
        er_nres_checkgrp TYPE any.
    METHODS amount_range
      EXPORTING
        er_res_amount  TYPE any
        er_nres_amount TYPE any.
    METHODS percentage_range
      EXPORTING
        er_percentage TYPE any.
    METHODS case_category_hist
      EXPORTING
        er_category TYPE any.
    METHODS check_group_hist
      EXPORTING
        er_checkgrp TYPE any.
    METHODS util_invoice_doc
      IMPORTING
        ir_invoice TYPE any
      EXPORTING
        et_invoice TYPE any.
    METHODS get_invoice_details
      IMPORTING
        ir_isuaccount TYPE any OPTIONAL
        ir_printdoc   TYPE any OPTIONAL
        is_released   TYPE any OPTIONAL
      EXPORTING
        et_invoice    TYPE any.
    METHODS outsorted_invoice
      IMPORTING
        ir_invoice TYPE any
      EXPORTING
        er_invoice TYPE any.
    METHODS invoicing_check_group
      IMPORTING
        ir_contract          TYPE any
      EXPORTING
        er_contract_checkgrp TYPE any
        et_contract          TYPE any.
    METHODS bpem_checks
      EXPORTING
        et_checks TYPE any.
    METHODS get_bpem_cases
      IMPORTING
        iv_invoutsort  TYPE any OPTIONAL
        iv_implausible TYPE any OPTIONAL
      EXPORTING
        et_cases       TYPE any.
    METHODS io_complete_case
      IMPORTING
        es_casenumber TYPE any.
    METHODS io_release_invoice
      IMPORTING
        es_invoice TYPE any.
    METHODS get_mtrrdngdocument
      IMPORTING
        ir_meterreadingdoc TYPE any
      EXPORTING
        et_meterreadingdoc TYPE any.
    METHODS get_mr_doc_reason
      IMPORTING
        ir_mrdocument     TYPE any
      EXPORTING
        et_mrdocument_rsn TYPE any.
    METHODS get_all_mtrrdngdocument
      EXPORTING
        et_meterreadingdoc TYPE any
      RAISING
        cx_web_http_client_error
        cx_http_dest_provider_error.
    METHODS get_installation
      IMPORTING
        ir_installation TYPE any
      EXPORTING
        et_installation TYPE any.
    METHODS get_contract
      IMPORTING
        ir_installation TYPE any
      EXPORTING
        et_contract     TYPE any.
    METHODS release_mtrrdngdoc
      IMPORTING
        es_mtrrdngdoc TYPE any.
    METHODS call_rfc.
    METHODS call_read_table_rfc_cntr_accnt
      IMPORTING
        im_vkont TYPE any
      EXPORTING
        ex_data  TYPE tt_contractaccnt.

    METHODS call_read_table_rfc_discdoc
      IMPORTING
        im_REFOBJTYPE TYPE any
        im_REFOBJKEY  TYPE any
      EXPORTING
        ex_data       TYPE tt_discdoc.

    METHODS call_imr_tolerance_rates
      EXPORTING
        er_imr_rate_cat TYPE tt_imr_rates.

    METHODS call_read_category_case
      IMPORTING
        iv_outsort  TYPE c OPTIONAL
        iv_imr      TYPE c OPTIONAL
      EXPORTING
        er_cat_case TYPE tt_imr_case.
ENDCLASS.



CLASS zcl_uc_excp_exceptions_rpa IMPLEMENTATION.


  METHOD outsorted_invoice.

    DATA:
      lt_business_data    TYPE TABLE OF zzexcp_i_utilsinvcgdocoutsrtgd,
      lt_sorted_data      TYPE SORTED TABLE OF zzexcp_i_utilsinvcgdocoutsrtgd
                          WITH UNIQUE KEY utilitiesinvoicingdocument,

      lt_invoice_r        TYPE RANGE OF zzexcp_i_utilsinvcgdocoutsrtgd-utilitiesinvoicingdocument,
      lt_inv_filtered_r   TYPE RANGE OF zzexcp_i_utilsinvcgdocoutsrtgd-utilitiesinvoicingdocument,

      lo_http_client      TYPE REF TO if_web_http_client,
      lo_client_proxy     TYPE REF TO /iwbep/if_cp_client_proxy,
      lo_request          TYPE REF TO /iwbep/if_cp_request_read_list,
      lo_response         TYPE REF TO /iwbep/if_cp_response_read_lst,
      lo_filter_factory   TYPE REF TO /iwbep/if_cp_filter_factory,
      lo_filter_invoice   TYPE REF TO /iwbep/if_cp_filter_node,
      lo_filter_node_root TYPE REF TO /iwbep/if_cp_filter_node.


    TRY.
        "Create http client
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination(
                                  cl_http_destination_provider=>create_by_cloud_destination(
                                                 i_name       = 'ExceptionsConfigApp_usq_https_8001'
                                                 i_authn_mode = if_a4c_cp_service=>service_specific ) ).


        lo_client_proxy = cl_web_odata_client_factory=>create_v2_remote_proxy(
          EXPORTING
            iv_service_definition_name = 'ZUC_SCM_O_OUTSRTD_INVCG_DOC'
            io_http_client             = lo_http_client
            iv_relative_service_root   = '/sap/opu/odata/sap/ISU_IN_OUTSRTD_INVCG_DOC_SRV' ).

        lt_invoice_r = ir_invoice.

        " Navigate to the resource and create a request for the read operation
        lo_request = lo_client_proxy->create_resource_for_entity_set( 'I_UTILSINVCGDOCOUTSRTGDATACNTR' )->create_request_for_read( ).

        " Create the filter tree
        lo_filter_factory = lo_request->create_filter_factory( ).

        lo_filter_invoice = lo_filter_factory->create_by_range( iv_property_path = 'UTILITIESINVOICINGDOCUMENT'
                                                                it_range         = lt_invoice_r ).

        lo_filter_node_root = lo_filter_invoice.
        lo_request->set_filter( lo_filter_node_root ).

        " Execute the request and retrieve the business data
        lo_response = lo_request->execute( ).
        lo_response->get_business_data( IMPORTING et_business_data = lt_business_data ).

        IF lt_business_data IS NOT INITIAL.
          DATA(lt_business_data_temp) = lt_business_data.
          SORT lt_business_data_temp BY utilitiesinvoicingdocument.
          lt_sorted_data = lt_business_data.
          lt_inv_filtered_r = FILTER #( lt_invoice_r IN lt_sorted_data
                                   WHERE low = utilitiesinvoicingdocument ).
          IF lt_inv_filtered_r IS NOT INITIAL.
            er_invoice = lt_inv_filtered_r.
          ENDIF.
        ELSE.
          CLEAR er_invoice.
        ENDIF.

      CATCH /iwbep/cx_cp_remote INTO DATA(lv_remote).
      CATCH /iwbep/cx_gateway INTO DATA(lv_gateway).
      CATCH cx_web_http_client_error INTO DATA(lv_client).
      CATCH cx_http_dest_provider_error INTO DATA(lv_dest).

    ENDTRY.

  ENDMETHOD.


  METHOD call_read_table_rfc_cntr_accnt.

    DATA:
      dest                 TYPE REF TO if_rfc_dest,
      myobj                TYPE REF TO zcl_uc_excp_rfc_read_table,
      lt_contractaccnt     TYPE STANDARD TABLE OF ty_contractaccnt,
      ls_contractaccnt     TYPE ty_contractaccnt,
      lv_delimiter         TYPE zcl_uc_excp_rfc_read_table=>so_text001,
      get_sorted           TYPE zcl_uc_excp_rfc_read_table=>boole_d,
      no_data              TYPE zcl_uc_excp_rfc_read_table=>so_text001,
      query_table          TYPE zcl_uc_excp_rfc_read_table=>tabname,
      rowcount             TYPE zcl_uc_excp_rfc_read_table=>so_int,
      rowskips             TYPE zcl_uc_excp_rfc_read_table=>so_int,
      use_et_data_4_return TYPE zcl_uc_excp_rfc_read_table=>boole_d,
      et_data              TYPE zcl_uc_excp_rfc_read_table=>sdti_result_tab,
      data                 TYPE zcl_uc_excp_rfc_read_table=>_tab512,
      fields               TYPE zcl_uc_excp_rfc_read_table=>_rfc_db_fld,
      options              TYPE zcl_uc_excp_rfc_read_table=>_rfc_db_opt,
      lv_fld_str           TYPE string.

    lv_fld_str = | VKONT = | & | { im_vkont } |.

    TRY.
        dest = cl_rfc_destination_provider=>create_by_cloud_destination(
          i_name = 'USQ-RFC'
        ).

        CREATE OBJECT myobj
          EXPORTING
            destination = dest.

        query_table = 'DFKKOP'.
        fields      = VALUE #( ( fieldname = 'VKONT' )
                               ( fieldname = 'BUDAT' )
                               ( fieldname = 'BETRW' ) ).
        options     = VALUE #( ( text      = lv_fld_str ) ).

        myobj->rfc_read_table(
          EXPORTING
            delimiter            = lv_delimiter
            get_sorted           = get_sorted
            no_data              = no_data
            query_table          = query_table
            rowcount             = rowcount
            rowskips             = rowskips
            use_et_data_4_return = use_et_data_4_return
          IMPORTING
            et_data              = et_data
          CHANGING
            data                 = data
            fields               = fields
            options              = options
        ).
        CLEAR ex_data[].
        IF NOT data[] IS INITIAL AND sy-subrc EQ 0.
          LOOP AT data ASSIGNING FIELD-SYMBOL(<lfs_data>).
            ls_contractaccnt-vkont = <lfs_data>+0(12).
            ls_contractaccnt-budat = <lfs_data>+12(8).
            ls_contractaccnt-betrw = <lfs_data>+20(13).
            APPEND ls_contractaccnt TO ex_data.
            CLEAR ls_contractaccnt.
          ENDLOOP.
          UNASSIGN <lfs_data>.
          SORT ex_data BY budat DESCENDING.
        ENDIF.

      CATCH  cx_aco_communication_failure INTO DATA(lcx_comm).
        " handle CX_ACO_COMMUNICATION_FAILURE (sy-msg* in lcx_comm->IF_T100_MESSAGE~T100KEY)
      CATCH cx_aco_system_failure INTO DATA(lcx_sys).
        " handle CX_ACO_SYSTEM_FAILURE (sy-msg* in lcx_sys->IF_T100_MESSAGE~T100KEY)
      CATCH cx_aco_application_exception INTO DATA(lcx_appl).
        " handle APPLICATION_EXCEPTIONS (sy-msg* in lcx_appl->IF_T100_MESSAGE~T100KEY)
        CASE lcx_appl->get_exception_id( ).
          WHEN 'DATA_BUFFER_EXCEEDED'.
            "handle DATA_BUFFER_EXCEEDED.
          WHEN 'FIELD_NOT_VALID'.
            "handle FIELD_NOT_VALID.
          WHEN 'NOT_AUTHORIZED'.
            "handle NOT_AUTHORIZED.
          WHEN 'OPTION_NOT_VALID'.
            "handle OPTION_NOT_VALID.
          WHEN 'TABLE_NOT_AVAILABLE'.
            "handle TABLE_NOT_AVAILABLE.
          WHEN 'TABLE_WITHOUT_DATA'.
            "handle TABLE_WITHOUT_DATA.
        ENDCASE.
      CATCH cx_rfc_dest_provider_error.
        " handle CX_RFC_DEST_PROVIDER_ERROR
    ENDTRY.
  ENDMETHOD.


  METHOD call_read_table_rfc_discdoc.
    DATA:
      dest                 TYPE REF TO if_rfc_dest,
      myobj                TYPE REF TO zcl_uc_read_table,
      ls_discdoc           TYPE ty_discdoc,
      delimiter            TYPE zcl_uc_excp_rfc_read_table=>so_text001,
      get_sorted           TYPE zcl_uc_excp_rfc_read_table=>boole_d,
      no_data              TYPE zcl_uc_excp_rfc_read_table=>so_text001,
      query_table          TYPE zcl_uc_excp_rfc_read_table=>tabname,
      rowcount             TYPE zcl_uc_excp_rfc_read_table=>so_int,
      rowskips             TYPE zcl_uc_excp_rfc_read_table=>so_int,
      use_et_data_4_return TYPE zcl_uc_excp_rfc_read_table=>boole_d,
      et_data              TYPE zcl_uc_excp_rfc_read_table=>sdti_result_tab,
      data                 TYPE zcl_uc_excp_rfc_read_table=>_tab512,
      fields               TYPE zcl_uc_excp_rfc_read_table=>_rfc_db_fld,
      options              TYPE zcl_uc_excp_rfc_read_table=>_rfc_db_opt,
      lv_fld_str           TYPE string,
      lt_option            TYPE STANDARD TABLE OF zcl_uc_read_table=>rfc_db_opt,
      ls_option            TYPE  zcl_uc_read_table=>rfc_db_opt.

    CLEAR: lv_fld_str.
    lv_fld_str = |REFOBJKEY = '| & |{ im_refobjkey }| & |'|.

    TRY.
        dest = cl_rfc_destination_provider=>create_by_cloud_destination(
          i_name = 'USQ-RFC' ).

        CREATE OBJECT myobj
          EXPORTING
            destination = dest.

        query_table = 'EDISCDOC'.

        fields      = VALUE #( ( fieldname = 'STATUS' )
                               ( fieldname = 'REFOBJTYPE' )
                               ( fieldname = 'REFOBJKEY' ) ).

        options     = VALUE #( ( text = lv_fld_str ) ).

        myobj->rfc_read_table(
          EXPORTING
            delimiter            = delimiter
            get_sorted           = get_sorted
            no_data              = no_data
            query_table          = query_table
            rowcount             = rowcount
            rowskips             = rowskips
            use_et_data_4_return = use_et_data_4_return
          IMPORTING
            et_data              = et_data
          CHANGING
            data                 = data
            fields               = fields
            options              = options
        ).
        CLEAR ex_data[].
        IF NOT data[] IS INITIAL AND sy-subrc EQ 0.
          LOOP AT data ASSIGNING FIELD-SYMBOL(<lfs_data>).
            ls_discdoc-status     = <lfs_data>+0(2).
            ls_discdoc-refobjtype = <lfs_data>+2(10).
            ls_discdoc-refobjkey  = <lfs_data>+12(70).
            APPEND ls_discdoc TO ex_data.
            CLEAR ls_discdoc.
          ENDLOOP.
          UNASSIGN <lfs_data>.
          SORT ex_data BY refobjkey ASCENDING.
        ENDIF.

      CATCH  cx_aco_communication_failure INTO DATA(lcx_comm).
        " handle CX_ACO_COMMUNICATION_FAILURE (sy-msg* in lcx_comm->IF_T100_MESSAGE~T100KEY)
      CATCH cx_aco_system_failure INTO DATA(lcx_sys).
        " handle CX_ACO_SYSTEM_FAILURE (sy-msg* in lcx_sys->IF_T100_MESSAGE~T100KEY)
      CATCH cx_aco_application_exception INTO DATA(lcx_appl).
        " handle APPLICATION_EXCEPTIONS (sy-msg* in lcx_appl->IF_T100_MESSAGE~T100KEY)
        CASE lcx_appl->get_exception_id( ).
          WHEN 'DATA_BUFFER_EXCEEDED'.
            "handle DATA_BUFFER_EXCEEDED.
          WHEN 'FIELD_NOT_VALID'.
            "handle FIELD_NOT_VALID.
          WHEN 'NOT_AUTHORIZED'.
            "handle NOT_AUTHORIZED.
          WHEN 'OPTION_NOT_VALID'.
            "handle OPTION_NOT_VALID.
          WHEN 'TABLE_NOT_AVAILABLE'.
            "handle TABLE_NOT_AVAILABLE.
          WHEN 'TABLE_WITHOUT_DATA'.
            "handle TABLE_WITHOUT_DATA.
        ENDCASE.
      CATCH cx_rfc_dest_provider_error.
        " handle CX_RFC_DEST_PROVIDER_ERROR
    ENDTRY.
  ENDMETHOD.


  METHOD get_mr_doc_reason.

    DATA:
      lt_business_data    TYPE TABLE OF zzexcp_c_meterreadingdocumenth,

      lo_http_client      TYPE REF TO if_web_http_client,
      lo_client_proxy     TYPE REF TO /iwbep/if_cp_client_proxy,
      lo_request          TYPE REF TO /iwbep/if_cp_request_read_list,
      lo_response         TYPE REF TO /iwbep/if_cp_response_read_lst,
      lo_filter_factory   TYPE REF TO /iwbep/if_cp_filter_factory,
      lo_filter_mrdoc     TYPE REF TO /iwbep/if_cp_filter_node,
      lo_filter_node_root TYPE REF TO /iwbep/if_cp_filter_node,

      lt_mrdoc_r          TYPE RANGE OF zzexcp_c_meterreadingdocumenth-utilitiesinstallation.

    lt_mrdoc_r = ir_mrdocument.

    TRY.
        "Create http client
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination(
                                  cl_http_destination_provider=>create_by_cloud_destination(
                                                 i_name       = 'ExceptionsConfigApp_usq_https_8001'
                                                 i_authn_mode = if_a4c_cp_service=>service_specific ) ).


        lo_client_proxy = cl_web_odata_client_factory=>create_v2_remote_proxy(
          EXPORTING
            iv_service_definition_name = 'ZUC_SCM_O_IMPL_RESOLVE'
            io_http_client             = lo_http_client
            iv_relative_service_root   = '/sap/opu/odata/SAP/ISU_MR_IMPL_RESOLVE' ).


        " Navigate to the resource and create a request for the read operation
        lo_request = lo_client_proxy->create_resource_for_entity_set( 'C_METERREADINGDOCUMENTHISTORY' )->create_request_for_read( ).

        " Create the filter tree
        lo_filter_factory = lo_request->create_filter_factory( ).
        lo_filter_mrdoc = lo_filter_factory->create_by_range( iv_property_path = 'UTILITIESINSTALLATION'
                                                              it_range         = lt_mrdoc_r ).
        lo_filter_node_root = lo_filter_mrdoc.
        lo_request->set_filter( lo_filter_node_root ).

        " Execute the request and retrieve the business data
        lo_response = lo_request->execute( ).
        lo_response->get_business_data( IMPORTING et_business_data = lt_business_data ).

        IF lt_business_data IS NOT INITIAL.
          et_mrdocument_rsn = lt_business_data.
        ENDIF.

      CATCH /iwbep/cx_cp_remote INTO DATA(lv_remote).
        " Handle remote Exception
        " It contains details about the problems of your http(s) connection

      CATCH /iwbep/cx_gateway INTO DATA(lv_gateway).
        " Handle Exception
      CATCH cx_web_http_client_error INTO DATA(lv_client).
      CATCH cx_http_dest_provider_error INTO DATA(lv_dest).
    ENDTRY.

  ENDMETHOD.


  METHOD exceptions_validation.

    "Process Invoice Outsort validation
    invoice_outsort( ).

    "Process Implausible Meter Reads validation
    implausible_meter( ).

  ENDMETHOD.


  METHOD get_all_mtrrdngdocument.
    DATA:
      lt_business_data TYPE TABLE OF zzexcp_i_meterreadingdocument,

      lo_http_client   TYPE REF TO if_web_http_client,
      lo_client_proxy  TYPE REF TO /iwbep/if_cp_client_proxy,
      lo_request       TYPE REF TO /iwbep/if_cp_request_read_list,
      lo_response      TYPE REF TO /iwbep/if_cp_response_read_lst.

    TRY.
        " Create http client
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination(
                            cl_http_destination_provider=>create_by_cloud_destination(
                                           i_name       = 'ExceptionsConfigApp_usq_https_8001'
                                           i_authn_mode = if_a4c_cp_service=>service_specific ) ).

        lo_client_proxy = cl_web_odata_client_factory=>create_v2_remote_proxy(
          EXPORTING
            iv_service_definition_name = 'ZUC_SCM_O_IMPL_RESOLVE'
            io_http_client             = lo_http_client
            iv_relative_service_root   = '/sap/opu/odata/SAP/ISU_MR_IMPL_RESOLVE' ).


        " Navigate to the resource and create a request for the read operation
        lo_request = lo_client_proxy->create_resource_for_entity_set( 'I_METERREADINGDOCUMENT' )->create_request_for_read( ).

        " Execute the request and retrieve the business data
        lo_response = lo_request->execute( ).
        lo_response->get_business_data( IMPORTING et_business_data = lt_business_data ).

        IF lt_business_data IS NOT INITIAL.
          et_meterreadingdoc = lt_business_data.
        ENDIF.

      CATCH /iwbep/cx_cp_remote INTO DATA(lv_remote).
        " Handle remote Exception
        " It contains details about the problems of your http(s) connection

      CATCH /iwbep/cx_gateway INTO DATA(lv_gateway).
        " Handle Exception

    ENDTRY.

  ENDMETHOD.


  METHOD io_complete_case.

    DATA: ls_http_header_fields  TYPE if_web_http_request=>name_value_pairs  .

    TRY.
        ls_http_header_fields = VALUE #( ( name = if_web_http_header=>accept value = |application/json| )
                                         ( name = if_web_http_header=>content_type value =  |application/json|  )
                                         ( name = 'x-csrf-token' value = 'fetch' ) ).

        DATA(lv_service_relative_url) = '/sap/opu/odata/sap/ISU_IN_OUTSRTD_INVCG_DOC_SRV'.
        DATA(lv_relative_url) = |{ lv_service_relative_url }?sap-client=200|.

        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination(
                                  cl_http_destination_provider=>create_by_cloud_destination(
                                                 i_name       = 'ExceptionsConfigApp_usq_https_8001'
                                                 i_authn_mode = if_a4c_cp_service=>service_specific ) ).

        " GET CSRF token
        lo_http_client->get_http_request( )->set_uri_path( i_uri_path = lv_relative_url ).
        lo_http_client->get_http_request( )->set_header_fields( ls_http_header_fields ).

        DATA(lo_http_response) = lo_http_client->execute( if_web_http_client=>get ).
        DATA(lv_http_status_code) = lo_http_response->get_status( ).
        DATA(lv_x_csrf_token) =  lo_http_response->get_header_field( 'x-csrf-token' ).
        DATA(lv_http_response_body) =  lo_http_response->get_text( ).


        DATA(lv_relative_url2) = |{ lv_service_relative_url }/C_BPEMCaseTPComplete?BPEMCase='{ es_casenumber }'&Reason=''&Newprocessor=''|.
        lo_http_client->get_http_request( )->set_uri_path( i_uri_path = lv_relative_url2 ).
        DATA(lv_text) = lo_http_client->get_http_request( )->get_text( ).

        ls_http_header_fields = VALUE #( ( name = if_web_http_header=>accept value = |application/json| )
                                         ( name = 'x-csrf-token' value =  lv_x_csrf_token ) ).

        lo_http_client->get_http_request( )->set_header_fields( ls_http_header_fields ).

        lo_http_response = lo_http_client->execute( if_web_http_client=>post ).
        lv_http_status_code = lo_http_response->get_status( ).
        DATA(lv_response) =  lo_http_response->get_text( ).

      CATCH cx_web_http_client_error INTO DATA(lv_client).

    ENDTRY.
  ENDMETHOD.


  METHOD implausible_meter.
    DATA:
      lt_cases                   TYPE STANDARD TABLE OF zcl_uc_isu_c4c_emma_case,
      lt_meterreadingdoc         TYPE STANDARD TABLE OF zzexcp_c_mtrrdngdocumenttp,
      lt_all_meterreadingdoc     TYPE STANDARD TABLE OF zzexcp_i_meterreadingdocument,
      lt_installation            TYPE STANDARD TABLE OF zzexcp_i_utilitiesinstallation-utilitiesinstallation,
      lt_contract                TYPE STANDARD TABLE OF zzexcp_i_utilitiescontract,
      lt_mrdocument_rsn          TYPE STANDARD TABLE OF zzexcp_c_meterreadingdocumenth,
      lt_data_discdoc            TYPE tt_discdoc,
      lt_all_meterreadingdoc_tmp TYPE STANDARD TABLE OF zzexcp_i_meterreadingdocument,
      lt_meterreadingdoc_tmp     TYPE STANDARD TABLE OF zzexcp_c_meterreadingdocumenth,
      lt_mrdocument_rsn_tmp      TYPE STANDARD TABLE OF zzexcp_c_meterreadingdocumenth,
      lt_mrdocument_rsn_tmp1     TYPE STANDARD TABLE OF zzexcp_c_meterreadingdocumenth,
      lt_checks                  TYPE STANDARD TABLE OF zexcp_bpem_check,
      lt_rate_cat                TYPE STANDARD TABLE OF ty_imr_rates,

      lr_main_object_key         TYPE RANGE OF zcl_uc_isu_c4c_emma_case-main_object_key,
      lr_winter                  TYPE RANGE OF zexcp_imr_rates-winter_start,
      lr_summer                  TYPE RANGE OF zexcp_imr_rates-summer_start,
      lr_installation            TYPE RANGE OF zzexcp_c_meterreadingdocumenth-utilitiesinstallation,
      lr_int_mtr_docid           TYPE RANGE OF zzexcp_i_meterreadingdocument-MeterReadingDocument,
      lr_mrdoc                   TYPE RANGE OF zzexcp_c_meterreadingdocumenth-utilitiesinstallation,
      lr_ablbelnr                TYPE RANGE OF zzexcp_c_meterreadingdocumenth-meterreadingdocument,

      lv_date                    TYPE string,
      lv_moveoutdate             TYPE d,
      lv_index                   TYPE i,
      lv_zero_inc                TYPE i,
      lv_indx                    TYPE i,
      lv_meterreadingresult_tmp  TYPE string,
      lv_kwh                     TYPE i,
      lv_kwh_prev                TYPE i,
      lv_kwh2                    TYPE i,
      lv_mtrreadingdate          TYPE i,
      lv_consumption_total       TYPE i,
      lv_consumption_ave         TYPE i,
      lv_billqty                 TYPE f,
      lv_dats                    TYPE string,
      lv_base_date               TYPE d,
      lv_prev_year               TYPE zzexcp_c_meterreadingdocumenth-meterreadingdate,
      lv_tolerance_high          TYPE zexcp_imr_rates-tolerance_h,
      lv_tolerance_low           TYPE zexcp_imr_rates-tolerance_l,
      lv_season                  TYPE zexcp_imr_rates-season,
      lv_mnth_season             TYPE d,
      lwa_mrdocument_rsn_tmp1    TYPE zzexcp_c_meterreadingdocumenth.

    TYPES:
      lt_meterreadingreason_r TYPE RANGE OF  zzexcp_c_meterreadingdocumenth-MeterReadingReason,
      lr_historical_date      TYPE RANGE OF  d.

    "Get configured BPEM checks
    bpem_checks(
      IMPORTING
        et_checks = lt_checks ).

    IF line_exists( lt_checks[ key_id = mc_invoice_outsort-impmr
                               is_active = abap_true ] ).

      "Get BPEM Cases
      get_bpem_cases(
        EXPORTING
          iv_implausible = abap_true
        IMPORTING
          et_cases       = lt_cases ).

      IF lt_cases IS NOT INITIAL.
        SORT lt_cases BY main_object_type ASCENDING.
        DELETE lt_cases WHERE main_object_type NE mc_implausible-mtrreaddoc.

        SORT lt_cases BY main_object_type
                         main_object_key.
      ENDIF.

      lr_main_object_key = VALUE #( FOR lwa_cases IN lt_cases
                                    ( sign = mc_implausible-i option = mc_implausible-eq low = lwa_cases-main_object_key ) ).

      SORT lr_main_object_key BY low ASCENDING.
      DELETE ADJACENT DUPLICATES FROM lr_main_object_key COMPARING low.

      get_mtrrdngdocument(
        EXPORTING
          ir_meterreadingdoc = lr_main_object_key
        IMPORTING
          et_meterreadingdoc = lt_meterreadingdoc ).


      lr_mrdoc = VALUE #( FOR lwa_meterreadingdoc IN lt_meterreadingdoc
                        ( sign = mc_implausible-i option = mc_implausible-eq low = lwa_meterreadingdoc-utilitiesinstallation ) ).

      get_mr_doc_reason(
        EXPORTING
          ir_mrdocument     = lr_mrdoc
        IMPORTING
          et_mrdocument_rsn = lt_mrdocument_rsn ).

      SORT lt_mrdocument_rsn BY utilitiesinstallation meterreadingreason ASCENDING.

      DATA(lr_mtrreason) = VALUE lt_meterreadingreason_r( sign = mc_implausible-i
                                                          option = mc_implausible-eq
                                                        ( low = mc_implausible-ones )
                                                        ( low = mc_implausible-two )
                                                        ( low = mc_implausible-three )
                                                        ( low = mc_implausible-six ) ).
      "Deleting unnecessary Meter Reading Reason
      DELETE lt_mrdocument_rsn WHERE meterreadingreason NOT IN lr_mtrreason[].
      SORT lt_mrdocument_rsn BY meterreadingdate DESCENDING.

      IF lt_mrdocument_rsn IS NOT INITIAL.

        lr_installation[] = lr_mrdoc[].
        get_installation(
          EXPORTING
            ir_installation = lr_installation
          IMPORTING
            et_installation = lt_installation ).

        get_contract(
          EXPORTING
            ir_installation = lr_installation
          IMPORTING
            et_contract     = lt_contract ).

        get_all_mtrrdngdocument(
          IMPORTING
            et_meterreadingdoc = lt_all_meterreadingdoc ).

        IF lt_all_meterreadingdoc IS NOT INITIAL.
          SORT lt_all_meterreadingdoc
            BY meterreadingdate DESCENDING
               meterreadingtime DESCENDING.
        ENDIF.
      ENDIF.

      "Get IMR Rates
      me->call_imr_tolerance_rates( IMPORTING er_imr_rate_cat = lt_rate_cat ).
      SORT lt_rate_cat BY rate_category ASCENDING.
      DATA(lt_rate_cat_season) = lt_rate_cat[].

      LOOP AT lt_cases ASSIGNING FIELD-SYMBOL(<lwa_cases>).

        DATA(lv_meterreadingdoc) = |{ <lwa_cases>-main_object_key ALPHA = OUT }|.
        ASSIGN lt_meterreadingdoc[ meterreadingdocument = lv_meterreadingdoc ]
          TO FIELD-SYMBOL(<lwa_meterreadingdoc>).
        IF sy-subrc EQ 0.
          IF <lwa_meterreadingdoc>-meterreadingstatus NE 2. "Not Automatically Locked"

            "Mark the case as completed
            io_complete_case(
              EXPORTING
                es_casenumber = <lwa_cases>-casenumber ).

          ELSE.

            "Check for disconnection stat of installation in EDISCDOC
            me->call_read_table_rfc_discdoc( EXPORTING im_refobjtype = mc_implausible-instln
                                                       im_REFOBJKEY  = <lwa_meterreadingdoc>-utilitiesinstallation
                                             IMPORTING ex_data       = lt_data_discdoc ).

            READ TABLE lt_data_discdoc ASSIGNING FIELD-SYMBOL(<lfs_data>) INDEX 1 .
            IF sy-subrc EQ 0 AND <lfs_data> IS ASSIGNED.
              IF <lfs_data>-status  NE 20 OR
                 <lfs_data>-status  NE 22.

              ENDIF.
            ENDIF.

            "Checking of Move out date is Greater then System date/Current date.

            ASSIGN lt_contract[ utilitiesinstallation = <lwa_meterreadingdoc>-utilitiesinstallation ]
              TO FIELD-SYMBOL(<lwa_contract_tmp>).
            IF sy-subrc EQ 0.
              lv_date = CONV string( <lwa_contract_tmp>-utilitiesmoveoutdate ).
              lv_moveoutdate = lv_date(8).

              IF lv_moveoutdate GE syst-datum.

                "Deleting unnecessary records based on UtilitiesInstallation.
                lt_mrdocument_rsn_tmp[] = lt_mrdocument_rsn[].
                DELETE lt_mrdocument_rsn_tmp WHERE utilitiesinstallation NE <lwa_meterreadingdoc>-utilitiesinstallation.

                CLEAR: lv_indx,
                       lv_meterreadingresult_tmp.

                DO 5 TIMES.
                  lv_indx = lv_indx + 1.

                  READ TABLE lt_mrdocument_rsn_tmp ASSIGNING FIELD-SYMBOL(<lfs_mrdocument_rsn>) INDEX lv_indx.

                  "Getting Current Consumption
                  IF lv_indx EQ 1.
                    lv_meterreadingresult_tmp = <lfs_mrdocument_rsn>-meterreadingresultvalue.
                    SHIFT lv_meterreadingresult_tmp RIGHT DELETING TRAILING mc_implausible-kwh.
                    REPLACE ALL OCCURRENCES OF mc_implausible-comma IN lv_meterreadingresult_tmp WITH space.
                    REPLACE ALL OCCURRENCES OF mc_implausible-dot   IN lv_meterreadingresult_tmp WITH space.
                    lv_kwh = lv_meterreadingresult_tmp.
                    CLEAR lv_meterreadingresult_tmp.
                  ENDIF.
                  CLEAR: lv_kwh_prev,
                         lv_meterreadingresult_tmp.

                  "Getting 3 months consumption
                  IF lv_indx EQ 2.
                    lv_meterreadingresult_tmp = <lfs_mrdocument_rsn>-meterreadingresultvalue.
                    SHIFT lv_meterreadingresult_tmp RIGHT DELETING TRAILING mc_implausible-kwh.
                    REPLACE ALL OCCURRENCES OF mc_implausible-comma IN lv_meterreadingresult_tmp WITH space.
                    REPLACE ALL OCCURRENCES OF mc_implausible-dot   IN lv_meterreadingresult_tmp WITH space.
                    lv_kwh_prev = lv_meterreadingresult_tmp.
                    DATA(lv_current_consumption) = CONV string( lv_kwh - lv_kwh_prev ).
                    lv_kwh = lv_kwh_prev.
                  ENDIF.
                  CLEAR lv_meterreadingresult_tmp.

                  IF lv_indx GE 3.
                    lv_meterreadingresult_tmp = <lfs_mrdocument_rsn>-meterreadingresultvalue.
                    SHIFT lv_meterreadingresult_tmp RIGHT DELETING TRAILING mc_implausible-kwh.
                    REPLACE ALL OCCURRENCES OF mc_implausible-comma IN lv_meterreadingresult_tmp WITH space.
                    REPLACE ALL OCCURRENCES OF mc_implausible-dot   IN lv_meterreadingresult_tmp WITH space.
                    lv_kwh_prev = lv_meterreadingresult_tmp.

                    DATA(lv_comsumption_totl_tmp) = lv_kwh - lv_kwh_prev.
                    lv_consumption_total = lv_consumption_total + lv_comsumption_totl_tmp.
                    lv_kwh = lv_kwh_prev.

                    "Getting 3 months consumption
                    IF lv_indx EQ 5.
                      DATA(lv_3months_consumption) = lv_consumption_total.
                    ENDIF.

                    CLEAR: lv_kwh_prev.
                  ENDIF.
                ENDDO.
                CLEAR: lv_indx,
                       lv_kwh,
                       lv_kwh_prev,
                       lv_meterreadingresult_tmp.
                UNASSIGN <lfs_mrdocument_rsn>.

                "Check for the Bad rollover

                IF  lv_current_consumption IS NOT INITIAL AND
                    lv_current_consumption CA mc_implausible-dash.
                  CONTINUE.
                ELSE.
                  "Check for 3 months consumption EQ 0
                  IF lv_3months_consumption EQ 0.
                    CONTINUE.
                  ELSE.
                    "Check for 12 months consumption

                    READ TABLE lt_mrdocument_rsn_tmp ASSIGNING FIELD-SYMBOL(<lfs_date_range>) INDEX 1.
                    IF sy-subrc EQ 0.

                      lv_base_date = CONV string( <lfs_date_range>-meterreadingdate ).
                      lv_base_date = lv_base_date+0(8).
                      DATA(lv_current_date) = lv_base_date.

                      DATA(lv_previous_year) = NEW zcl_uc_excp_util_exceptions( )->calculate_date( EXPORTING
                                                                                                        iv_date   = lv_base_date
                                                                                                        iv_signum = mc_implausible-dash
                                                                                                        iv_years  = 1 ).
                      DATA(lr_12_months_dates) = VALUE lr_historical_date( sign   = mc_implausible-i
                                                                           option = mc_implausible-bw
                                                                         ( low    = lv_previous_year
                                                                           high   = lv_current_date ) ).
                    ENDIF.

                    CLEAR lv_base_date.

                    LOOP AT lt_mrdocument_rsn_tmp ASSIGNING FIELD-SYMBOL(<lfs_12mnths>).

                      lv_base_date = CONV string( <lfs_12mnths>-meterreadingdate ).
                      lv_base_date = lv_base_date+0(8).

                      CLEAR lwa_mrdocument_rsn_tmp1.

                      IF lv_base_date IN lr_12_months_dates[].

                        lwa_mrdocument_rsn_tmp1 = CORRESPONDING #( <lfs_12mnths> ).

                        APPEND lwa_mrdocument_rsn_tmp1 TO lt_mrdocument_rsn_tmp1.
                        CLEAR:lv_base_date,
                              lwa_mrdocument_rsn_tmp1.
                      ENDIF.
                    ENDLOOP.
                    UNASSIGN <lfs_12mnths>.

                    IF lt_mrdocument_rsn_tmp1 IS NOT INITIAL.
                      lt_mrdocument_rsn_tmp[] = lt_mrdocument_rsn_tmp1[].
                    ENDIF.

                    IF lt_mrdocument_rsn_tmp IS NOT INITIAL AND
                       lines( lt_mrdocument_rsn_tmp ) GE 12.

                      CLEAR: lv_indx,
                             lv_kwh,
                             lv_kwh_prev,
                             lv_meterreadingresult_tmp,
                             lv_consumption_ave,
                             lv_consumption_total.
                      DO 8 TIMES.
                        lv_indx = lv_indx + 1.
                        READ TABLE lt_mrdocument_rsn_tmp ASSIGNING FIELD-SYMBOL(<lfs_6mnths_consump>) INDEX lv_indx.
                        IF sy-subrc EQ 0.

                          "6 Months Consumption
                          IF lv_indx EQ 2.

                            lv_meterreadingresult_tmp = <lfs_6mnths_consump>-meterreadingresultvalue.
                            SHIFT lv_meterreadingresult_tmp RIGHT DELETING TRAILING mc_implausible-kwh.
                            REPLACE ALL OCCURRENCES OF mc_implausible-comma IN lv_meterreadingresult_tmp WITH space.
                            REPLACE ALL OCCURRENCES OF mc_implausible-dot   IN lv_meterreadingresult_tmp WITH space.
                            lv_kwh = lv_meterreadingresult_tmp.

                            CLEAR lv_meterreadingresult_tmp.
                          ENDIF.

                          CLEAR lv_meterreadingresult_tmp.

                          IF lv_indx GE 3.
                            lv_meterreadingresult_tmp = <lfs_6mnths_consump>-meterreadingresultvalue.
                            SHIFT lv_meterreadingresult_tmp RIGHT DELETING TRAILING mc_implausible-kwh.
                            REPLACE ALL OCCURRENCES OF mc_implausible-comma IN lv_meterreadingresult_tmp WITH space.
                            REPLACE ALL OCCURRENCES OF mc_implausible-dot   IN lv_meterreadingresult_tmp WITH space.
                            lv_kwh_prev = lv_meterreadingresult_tmp.

                            DATA(lv_consumption)     = lv_kwh - lv_kwh_prev.
                            DATA(lv_consumption_tmp) = lv_consumption.

                            lv_consumption_total = lv_consumption_total + lv_consumption_tmp.

                            lv_kwh = lv_meterreadingresult_tmp.

                          ENDIF.
                        ENDIF.
                      ENDDO.

                      "Check for the Average 6 months consumption
                      IF lv_consumption_total IS NOT INITIAL.
                        lv_consumption_ave = lv_consumption_total / 6.
                        IF lv_consumption_ave IS NOT INITIAL.

                          DATA(lv_low)  = lv_consumption_ave * '0.80'.
                          DATA(lv_high) = lv_consumption_ave * '1.20'.

                          IF lv_consumption_ave LE lv_high AND
                             lv_consumption_ave GE lv_low.

                            "Release the Implausible
                            release_mtrrdngdoc(
                              EXPORTING
                                es_mtrrdngdoc = <lwa_cases>-main_object_key ).

                            "Mark the Case Completed.
                            io_complete_case(
                              EXPORTING
                                es_casenumber = <lwa_cases>-casenumber ).
                          ELSE.
                            IF lt_rate_cat IS NOT INITIAL.
                              CLEAR: lv_mnth_season.
                              READ TABLE lt_rate_cat ASSIGNING FIELD-SYMBOL(<lfs_rate_category>)
                                                     WITH KEY rate_category = <lwa_meterreadingdoc>-utilitiesratecategory
                                                     BINARY SEARCH.
                              IF sy-subrc EQ 0 AND <lfs_rate_category> IS ASSIGNED.

                                lr_winter = VALUE #( sign = mc_implausible-i
                                                   option = mc_implausible-bw ( low  = <lfs_rate_category>-winter_start
                                                                                high = <lfs_rate_category>-winter_end ) ).

                                lr_summer = VALUE #( sign = mc_implausible-i
                                                   option = mc_implausible-bw ( low  = <lfs_rate_category>-summer_start
                                                                                high = <lfs_rate_category>-summer_end ) ).

                                IF <lwa_meterreadingdoc>-meterreadingdate IS NOT INITIAL.

                                  lv_mnth_season = CONV string( <lwa_meterreadingdoc>-meterreadingdate ).
                                  lv_mnth_season = lv_mnth_season+4(2).

                                  IF lv_mnth_season IN lr_winter.

                                    lv_season = mc_implausible-winter.
                                  ELSE.
                                    lv_season = mc_implausible-summer.
                                  ENDIF.

                                  READ TABLE lt_rate_cat_season ASSIGNING FIELD-SYMBOL(<lfs_rate_cat_season>)
                                                                          WITH KEY rate_category = <lwa_meterreadingdoc>-utilitiesratecategory
                                                                                   season        = lv_season
                                                                                   BINARY SEARCH.
                                  IF sy-subrc EQ 0 AND <lfs_rate_cat_season> IS ASSIGNED.
                                    lv_tolerance_high = <lfs_rate_cat_season>-monthly_ave * <lfs_rate_cat_season>-tolerance_h / 100.
                                    lv_tolerance_low  = <lfs_rate_cat_season>-monthly_ave * <lfs_rate_cat_season>-tolerance_l / 100.
                                  ENDIF.
                                ENDIF.
                                CLEAR lv_mnth_season.
                                IF lv_current_consumption LE lv_tolerance_high AND
                                   lv_current_consumption GE lv_tolerance_low.

                                  "Release the Implausible
                                  release_mtrrdngdoc(
                                    EXPORTING
                                      es_mtrrdngdoc = <lwa_cases>-main_object_key ).

                                  "Mark the Case Completed.
                                  io_complete_case(
                                    EXPORTING
                                      es_casenumber = <lwa_cases>-casenumber ).
                                ENDIF.

                              ELSE.
                                "Check for disconnection stat of installation in EDISCDOC
                                IF lv_current_consumption EQ 0.
                                  me->call_read_table_rfc_discdoc( EXPORTING im_refobjtype = mc_implausible-instln
                                                                             im_REFOBJKEY  = <lwa_meterreadingdoc>-utilitiesinstallation
                                                                   IMPORTING ex_data       = lt_data_discdoc ).

                                  READ TABLE lt_data_discdoc ASSIGNING FIELD-SYMBOL(<lfs_data_zero>) INDEX 1 .
                                  IF sy-subrc EQ 0.
                                    IF <lfs_data_zero>-status EQ 21.

                                      "Release the Implausible
                                      release_mtrrdngdoc(
                                        EXPORTING
                                          es_mtrrdngdoc = <lwa_cases>-main_object_key ).

                                      "Mark the Case Completed.
                                      io_complete_case(
                                        EXPORTING
                                          es_casenumber = <lwa_cases>-casenumber ).

                                    ENDIF.
                                  ENDIF.
                                ENDIF.
                              ENDIF.
                            ENDIF.

                          ENDIF.
                        ENDIF.
                      ENDIF.
                    ELSE.
                      "Check for Release meter read based on Rate Category (No History Available)
                      "Get IMR Rates
                      CLEAR lv_mnth_season.
                      IF lt_rate_cat IS NOT INITIAL.
                        READ TABLE lt_rate_cat ASSIGNING FIELD-SYMBOL(<lfs_rate_cat>)
                                               WITH KEY rate_category = <lwa_meterreadingdoc>-utilitiesratecategory
                                               BINARY SEARCH.
                        IF sy-subrc EQ 0 AND <lfs_rate_cat> IS ASSIGNED.

                          lr_winter = VALUE #( sign = mc_implausible-i
                                             option = mc_implausible-bw ( low  = <lfs_rate_cat>-winter_start
                                                                          high = <lfs_rate_cat>-winter_end ) ).

                          lr_summer = VALUE #( sign = mc_implausible-i
                                             option = mc_implausible-bw ( low  = <lfs_rate_cat>-summer_start
                                                                          high = <lfs_rate_cat>-summer_end ) ).

                          IF <lwa_meterreadingdoc>-meterreadingdate IS NOT INITIAL.

                            lv_mnth_season = CONV string( <lwa_meterreadingdoc>-meterreadingdate ).
                            lv_mnth_season = lv_mnth_season+4(2).

                            IF lv_mnth_season IN lr_winter.

                              lv_season = mc_implausible-winter.
                            ELSE.
                              lv_season = mc_implausible-summer.
                            ENDIF.

                            READ TABLE lt_rate_cat_season ASSIGNING FIELD-SYMBOL(<lfs_rate_cat_seas>)
                                                                    WITH KEY rate_category = <lwa_meterreadingdoc>-utilitiesratecategory
                                                                             season        = lv_season
                                                                             BINARY SEARCH.
                            IF sy-subrc EQ 0 AND <lfs_rate_cat_seas> IS ASSIGNED.
                              lv_tolerance_high = <lfs_rate_cat_seas>-monthly_ave * <lfs_rate_cat_seas>-tolerance_h / 100.
                              lv_tolerance_low  = <lfs_rate_cat_seas>-monthly_ave * <lfs_rate_cat_seas>-tolerance_l / 100.
                            ENDIF.
                            UNASSIGN: <lfs_rate_cat_seas>.
                          ENDIF.
                          CLEAR lv_mnth_season.

                          IF lv_current_consumption LE lv_tolerance_high AND
                             lv_current_consumption GE lv_tolerance_low.

                            "Release the Implausible
                            release_mtrrdngdoc(
                              EXPORTING
                                es_mtrrdngdoc = <lwa_cases>-main_object_key ).

                            "Mark the Case Completed.
                            io_complete_case(
                              EXPORTING
                                es_casenumber = <lwa_cases>-casenumber ).
                          ENDIF.

                        ELSE.
                          "Check for disconnection stat of installation in EDISCDOC
                          IF lv_current_consumption EQ 0.
                            me->call_read_table_rfc_discdoc( EXPORTING im_refobjtype = mc_implausible-instln
                                                                       im_REFOBJKEY  = <lwa_meterreadingdoc>-utilitiesinstallation
                                                             IMPORTING ex_data       = lt_data_discdoc ).

                            READ TABLE lt_data_discdoc ASSIGNING FIELD-SYMBOL(<lfs_data_zeroes>) INDEX 1 .
                            IF sy-subrc EQ 0.
                              IF <lfs_data_zeroes>-status EQ 21.

                                "Release the Implausible
                                release_mtrrdngdoc(
                                  EXPORTING
                                    es_mtrrdngdoc = <lwa_cases>-main_object_key ).

                                "Mark the Case Completed.
                                io_complete_case(
                                  EXPORTING
                                    es_casenumber = <lwa_cases>-casenumber ).

                              ENDIF.
                            ENDIF.
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ELSE.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_contract.
    DATA:
      lt_business_data    TYPE TABLE OF zzexcp_i_utilitiescontract,

      lo_http_client      TYPE REF TO if_web_http_client,
      lo_client_proxy     TYPE REF TO /iwbep/if_cp_client_proxy,
      lo_request          TYPE REF TO /iwbep/if_cp_request_read_list,
      lo_response         TYPE REF TO /iwbep/if_cp_response_read_lst,
      lo_filter_factory   TYPE REF TO /iwbep/if_cp_filter_factory,
      lo_filter_contract  TYPE REF TO /iwbep/if_cp_filter_node,
      lo_filter_node_root TYPE REF TO /iwbep/if_cp_filter_node,

      lt_installation_r   TYPE RANGE OF zzexcp_i_utilitiescontract-utilitiesinstallation.

    lt_installation_r = ir_installation.

    TRY.
        "Create http client
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination(
                            cl_http_destination_provider=>create_by_cloud_destination(
                                           i_name       = 'ExceptionsConfigApp_usq_https_8001'
                                           i_authn_mode = if_a4c_cp_service=>service_specific ) ).

        lo_client_proxy = cl_web_odata_client_factory=>create_v2_remote_proxy(
          EXPORTING
            iv_service_definition_name = 'ZUC_SCM_O_OPN_OVP_SRV'
            io_http_client             = lo_http_client
            iv_relative_service_root   = '/sap/opu/odata/SAP/ISU_BI_OPN_OVP_SRV' ).


        " Navigate to the resource and create a request for the read operation
        lo_request = lo_client_proxy->create_resource_for_entity_set( 'I_UTILITIESCONTRACT' )->create_request_for_read( ).

        " Create the filter tree
        lo_filter_factory = lo_request->create_filter_factory( ).
        lo_filter_contract = lo_filter_factory->create_by_range( iv_property_path = 'UTILITIESINSTALLATION'
                                                                 it_range         = lt_installation_r ).
        lo_filter_node_root = lo_filter_contract.
        lo_request->set_filter( lo_filter_node_root ).

        " Execute the request and retrieve the business data
        lo_response = lo_request->execute( ).
        lo_response->get_business_data( IMPORTING et_business_data = lt_business_data ).

        IF lt_business_data IS NOT INITIAL.
          et_contract = lt_business_data.
        ENDIF.

      CATCH /iwbep/cx_cp_remote INTO DATA(lv_remote).
        " Handle remote Exception
        " It contains details about the problems of your http(s) connection

      CATCH /iwbep/cx_gateway INTO DATA(lv_gateway).
        " Handle Exception

      CATCH cx_web_http_client_error INTO DATA(lv_client).
      CATCH cx_http_dest_provider_error INTO DATA(lv_dest).

    ENDTRY.

  ENDMETHOD.


  METHOD release_mtrrdngdoc.

    DATA ls_http_header_fields  TYPE if_web_http_request=>name_value_pairs  .

    ls_http_header_fields = VALUE #( ( name = if_web_http_header=>accept value = |application/json| )
                                     ( name = if_web_http_header=>content_type value =  |application/json|  )
                                     ( name = 'x-csrf-token' value = 'fetch' ) ).

    DATA(lv_service_relative_url) = '/sap/opu/odata/SAP/ISU_MR_IMPL_RESOLVE'.
    DATA(lv_relative_url) = |{ lv_service_relative_url }?sap-client=200|.

    DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination(
                              cl_http_destination_provider=>create_by_cloud_destination(
                                             i_name       = 'ExceptionsConfigApp_usq_https_8001'
                                             i_authn_mode = if_a4c_cp_service=>service_specific ) ).

    " GET CSRF token
    lo_http_client->get_http_request( )->set_uri_path( i_uri_path = lv_relative_url ).
    lo_http_client->get_http_request( )->set_header_fields( ls_http_header_fields ).

    DATA(lo_http_response) = lo_http_client->execute( if_web_http_client=>get ).
    DATA(lv_http_status_code) = lo_http_response->get_status( ).
    DATA(lv_x_csrf_token) =  lo_http_response->get_header_field( 'x-csrf-token' ).
    DATA(lv_http_response_body) =  lo_http_response->get_text( ).


    DATA(lv_relative_url2) = |{ lv_service_relative_url }/C_MtrRdngDocumentTPSingle_release?MeterReadingDocument='{ es_mtrrdngdoc }'|.
    lo_http_client->get_http_request( )->set_uri_path( i_uri_path = lv_relative_url2 ).
    DATA(lv_text) = lo_http_client->get_http_request( )->get_text( ).

    ls_http_header_fields = VALUE #( ( name = if_web_http_header=>accept value = |application/json| )
                                     ( name = 'x-csrf-token' value =  lv_x_csrf_token ) ).

    lo_http_client->get_http_request( )->set_header_fields( ls_http_header_fields ).

    lo_http_response = lo_http_client->execute( if_web_http_client=>post ).
    lv_http_status_code = lo_http_response->get_status( ).
    DATA(lv_response) =  lo_http_response->get_text( ).
  ENDMETHOD.


  METHOD call_imr_tolerance_rates.

    DATA:
      lt_imr_rates  TYPE STANDARD TABLE OF ty_imr_rates.

    "Get Rate Category and Tolerance Rates

    SELECT rate_category,
           season,
           monthly_ave,
           tolerance_h,
           tolerance_l,
           winter_start,
           winter_end,
           summer_start,
           summer_end
    FROM zexcp_imr_rates
    INTO TABLE @lt_imr_rates.
    IF sy-subrc EQ 0.

      er_imr_rate_cat[] = lt_imr_rates[].

    ENDIF.

  ENDMETHOD.


  METHOD check_group_cred_io.

    DATA:
      lt_res_checkgrp_r  TYPE RANGE OF zexcp_outsort_io-check_grp,
      lt_nres_checkgrp_r TYPE RANGE OF zexcp_outsort_io-check_grp.

    "Get check group from configured checks
    SELECT check_grp
           ctype
      FROM zexcp_outsort_io
      INTO TABLE @DATA(lt_checkgrp).
    IF sy-subrc EQ 0.
      lt_res_checkgrp_r = VALUE #( FOR ls_checkgrp IN lt_checkgrp
                                   WHERE ( ctype EQ 'Res' )
                                   ( sign   = 'I'
                                     option = 'EQ'
                                     low    = ls_checkgrp-check_grp ) ).


      lt_nres_checkgrp_r = VALUE #( FOR ls_checkgrp IN lt_checkgrp
                                    WHERE ( ctype EQ 'Non-Res' )
                                    ( sign   = 'I'
                                      option = 'EQ'
                                      low    = ls_checkgrp-check_grp ) ).
    ENDIF.

    IF lt_res_checkgrp_r[] IS NOT INITIAL.
      er_res_checkgrp = lt_res_checkgrp_r[].
    ENDIF.

    IF lt_nres_checkgrp_r[] IS NOT INITIAL.
      er_nres_checkgrp = lt_nres_checkgrp_r[].
    ENDIF.
  ENDMETHOD.


  METHOD call_rfc.

    DATA dest TYPE REF TO if_rfc_dest.


  ENDMETHOD.


  METHOD amount_range.

    DATA:
      lt_res_amount_r  TYPE RANGE OF zexcp_outsortamt-from_amnt,
      lt_nres_amount_r TYPE RANGE OF zexcp_outsortamt-from_amnt.

    "Get amount range from configured checks
    SELECT from_amnt
           to_amnt
           ctype
      FROM zexcp_outsortamt
      INTO TABLE @DATA(lt_amount).
    IF sy-subrc EQ 0.
      lt_res_amount_r = VALUE #( FOR ls_amount IN lt_amount
                                 WHERE ( ctype EQ 'Res' )
                                 ( sign   = 'I'
                                   option = 'BT'
                                   low    = ls_amount-to_amnt
                                   high   = ls_amount-from_amnt   ) ).


      lt_nres_amount_r = VALUE #( FOR ls_amount IN lt_amount
                                  WHERE ( ctype EQ 'Non-Res' )
                                  ( sign   = 'I'
                                    option = 'BT'
                                    low    = ls_amount-to_amnt
                                    high   = ls_amount-from_amnt   ) ).
    ENDIF.

    IF lt_res_amount_r[] IS NOT INITIAL.
      er_res_amount = lt_res_amount_r[].
    ENDIF.

    IF lt_nres_amount_r[] IS NOT INITIAL.
      er_nres_amount = lt_nres_amount_r[].
    ENDIF.

  ENDMETHOD.


  METHOD bpem_checks.

    SELECT *
      FROM zexcp_bpem_check
      INTO TABLE @DATA(lt_checks).
    IF sy-subrc EQ 0.
      et_checks = lt_checks.
    ENDIF.

  ENDMETHOD.


  METHOD case_category_cred_io.

    DATA:
      lt_res_category_r  TYPE RANGE OF zexcp_outsortbpm-category,
      lt_nres_category_r TYPE RANGE OF zexcp_outsortbpm-category.

    "Get categories from configured checks
    SELECT category
           ctype
      FROM zexcp_outsortbpm
      INTO TABLE @DATA(lt_category).
    IF sy-subrc EQ 0.
      lt_res_category_r[] = VALUE #( FOR ls_category IN lt_category
                                     WHERE ( ctype EQ 'Res' )
                                     ( sign   = 'I'
                                       option = 'EQ'
                                       low    = ls_category-category ) ).

      lt_nres_category_r[] = VALUE #( FOR ls_category IN lt_category
                                      WHERE ( ctype EQ 'Non-Res' )
                                      ( sign   = 'I'
                                        option = 'EQ'
                                        low    = ls_category-category ) ).
    ENDIF.

    IF lt_res_category_r[] IS NOT INITIAL.
      er_res_category = lt_res_category_r[].
    ENDIF.

    IF lt_nres_category_r IS NOT INITIAL.
      er_nres_category = lt_nres_category_r[].
    ENDIF.
  ENDMETHOD.


  METHOD case_category_hist.

    DATA:
      lt_category_r TYPE RANGE OF zexcp_hist_bpem-category.

    "Get categories from configured checks
    SELECT category
      FROM zexcp_hist_bpem
      INTO TABLE @DATA(lt_category).
    IF sy-subrc EQ 0.
      lt_category_r = VALUE #( FOR ls_category IN lt_category
                             ( sign   = 'I'
                               option = 'EQ'
                               low    = ls_category-category ) ).
      er_category = lt_category_r.
    ENDIF.

  ENDMETHOD.


  METHOD check_group_hist.

    DATA:
      lt_checkgrp_r TYPE RANGE OF zexcp_hist_io-check_grp.

    "Get check group from configured checks
    SELECT check_grp
      FROM zexcp_hist_io
      INTO TABLE @DATA(lt_checkgrp).
    IF sy-subrc EQ 0.
      lt_checkgrp_r = VALUE #( FOR ls_checkgrp IN lt_checkgrp
                             ( sign   = 'I'
                               option = 'EQ'
                               low    = ls_checkgrp-check_grp ) ).
      er_checkgrp = lt_checkgrp_r.
    ENDIF.

  ENDMETHOD.


  METHOD get_bpem_cases.

    DATA:
      lt_filter        TYPE zuc_isu_c4c_ssn_sel_parame_tab,
      lt_category_case TYPE tt_imr_case.

    TRY.
        DATA(lo_destination) = cl_soap_destination_provider=>create_by_cloud_destination(
          i_name = 'USQ-HTTP-SOAP-BPEM-GET' ).

        DATA(lo_proxy) = NEW zcl_uc_co_isu_c4c_bpem_get( destination = lo_destination ).
        lt_filter = VALUE #( ( field_name = 'STATUS' sign = 'I' option = 'EQ' low_value = '1' )
                             ( field_name = 'STATUS' sign = 'I' option = 'EQ' low_value = '2' ) ).

        IF iv_invoutsort IS NOT INITIAL.
          me->call_read_category_case(
            EXPORTING
              iv_outsort  = abap_true
            IMPORTING
              er_cat_case = lt_category_case ).
          IF lt_category_case IS NOT INITIAL.

            lt_filter = VALUE #( BASE lt_filter FOR ls_cat_case IN lt_category_case
                                 ( field_name = 'CATEGORY' sign = 'I' option = 'EQ' low_value = ls_cat_case-category ) ).
          ENDIF.
*          lt_filter = VALUE #( BASE lt_filter
*                                ( field_name = 'CASE' sign = 'I' option = 'EQ' low_value = '10439') ).
        ENDIF.

        IF iv_implausible IS NOT INITIAL.
          me->call_read_category_case(
            EXPORTING
              iv_imr      = abap_true
            IMPORTING
              er_cat_case = lt_category_case ).
          IF lt_category_case IS NOT INITIAL.

            lt_filter = VALUE #( BASE lt_filter FOR ls_cat_case IN lt_category_case
                                 ( field_name = 'CATEGORY' sign = 'I' option = 'EQ' low_value = ls_cat_case-category ) ).
          ENDIF.

*          lt_filter = VALUE #( BASE lt_filter
*                               ( field_name = 'CASE' sign = 'I' option = 'EQ' low_value = '10441' ) ).

        ENDIF.

        DATA(lo_request) = VALUE zcl_uc_isu_c4c_bpem_get(  it_search_parameters-item = lt_filter
                                                        iv_no_maxhits             = 'X' ).
        lo_proxy->isu_c4c_bpem_get(
          EXPORTING
            input  = lo_request
          IMPORTING
            output = DATA(lo_response)
        ).

        et_cases = lo_response-et_case_info-item.
        "handle response
      CATCH cx_soap_destination_error INTO DATA(lv_dest_error).
        "handle error
      CATCH cx_ai_system_fault INTO DATA(lv_error).
        "handle error
    ENDTRY.

  ENDMETHOD.


  METHOD get_installation.

    DATA:
      lt_business_data       TYPE TABLE OF zzexcp_i_utilitiesinstallation,

      lo_http_client         TYPE REF TO if_web_http_client,
      lo_client_proxy        TYPE REF TO /iwbep/if_cp_client_proxy,
      lo_request             TYPE REF TO /iwbep/if_cp_request_read_list,
      lo_response            TYPE REF TO /iwbep/if_cp_response_read_lst,
      lo_filter_factory      TYPE REF TO /iwbep/if_cp_filter_factory,
      lo_filter_installation TYPE REF TO /iwbep/if_cp_filter_node,
      lo_filter_node_root    TYPE REF TO /iwbep/if_cp_filter_node,

      lt_installation_r      TYPE RANGE OF zzexcp_i_utilitiesinstallation-utilitiesinstallation.

    lt_installation_r = ir_installation.

    TRY.
        "Create http client
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination(
                            cl_http_destination_provider=>create_by_cloud_destination(
                                           i_name       = 'ExceptionsConfigApp_usq_https_8001'
                                           i_authn_mode = if_a4c_cp_service=>service_specific ) ).

        lo_client_proxy = cl_web_odata_client_factory=>create_v2_remote_proxy(
          EXPORTING
            iv_service_definition_name = 'ZUC_SCM_O_IMPL_RESOLVE'
            io_http_client             = lo_http_client
            iv_relative_service_root   = '/sap/opu/odata/SAP/ISU_MR_IMPL_RESOLVE' ).


        " Navigate to the resource and create a request for the read operation
        lo_request = lo_client_proxy->create_resource_for_entity_set( 'I_UTILITIESINSTALLATIONHIST' )->create_request_for_read( ).

        " Create the filter tree
        lo_filter_factory = lo_request->create_filter_factory( ).
        lo_filter_installation = lo_filter_factory->create_by_range( iv_property_path = 'UTILITIESINSTALLATION'
                                                                     it_range         = lt_installation_r ).
        lo_filter_node_root = lo_filter_installation.
        lo_request->set_filter( lo_filter_node_root ).

        " Execute the request and retrieve the business data
        lo_response = lo_request->execute( ).
        lo_response->get_business_data( IMPORTING et_business_data = lt_business_data ).

        IF lt_business_data IS NOT INITIAL.
          et_installation = lt_business_data.
        ENDIF.

      CATCH /iwbep/cx_cp_remote INTO DATA(lv_remote).
        " Handle remote Exception
        " It contains details about the problems of your http(s) connection

      CATCH /iwbep/cx_gateway INTO DATA(lv_gateway).
        " Handle Exception

      CATCH cx_web_http_client_error INTO DATA(lv_client).
      CATCH cx_http_dest_provider_error INTO DATA(lv_dest).

    ENDTRY.
  ENDMETHOD.


  METHOD get_invoice_details.

    DATA:
      lt_business_data      TYPE STANDARD TABLE OF zzexcp_i_utilitiesinvoicingdoc,
      lt_sorted_busidata    TYPE SORTED TABLE OF zzexcp_i_utilitiesinvoicingdoc
                            WITH UNIQUE KEY utilitiesinvoicingdocument,
      lt_contract           TYPE STANDARD TABLE OF zzexcp_i_utilitiesinvoicingdoc-contractaccount,
      lt_invoice            TYPE STANDARD TABLE OF zzexcp_i_utilitiesinvoicingdoc,

      lo_http_client        TYPE REF TO if_web_http_client,
      lo_client_proxy       TYPE REF TO /iwbep/if_cp_client_proxy,
      lo_request            TYPE REF TO /iwbep/if_cp_request_read_list,
      lo_response           TYPE REF TO /iwbep/if_cp_response_read_lst,
      lo_filter_factory     TYPE REF TO /iwbep/if_cp_filter_factory,
      lo_filter_contract    TYPE REF TO /iwbep/if_cp_filter_node,
      lo_filter_billrsn     TYPE REF TO /iwbep/if_cp_filter_node,
      lo_filter_posted      TYPE REF TO /iwbep/if_cp_filter_node,
      lo_filter_simulated   TYPE REF TO /iwbep/if_cp_filter_node,
      lo_filter_notreleased TYPE REF TO /iwbep/if_cp_filter_node,
      lo_filter_reversed    TYPE REF TO /iwbep/if_cp_filter_node,
      lo_filter_revdocno    TYPE REF TO /iwbep/if_cp_filter_node,
      lo_filter_invoice     TYPE REF TO /iwbep/if_cp_filter_node,
      lo_filter_node_root   TYPE REF TO /iwbep/if_cp_filter_node,

      lt_billrsn_r          TYPE RANGE OF zzexcp_i_utilitiesinvoicingdoc-utilitiesbillingreason,
      lt_posted_r           TYPE RANGE OF zzexcp_i_utilitiesinvoicingdoc-utilsinvcgdocumentisposted,
      lt_simulated_r        TYPE RANGE OF zzexcp_i_utilitiesinvoicingdoc-utilsinvcgdocissimulated,
      lt_notreleased_r      TYPE RANGE OF zzexcp_i_utilitiesinvoicingdoc-utilsinvcgdocisnotreleased,
      lt_reversed_r         TYPE RANGE OF zzexcp_i_utilitiesinvoicingdoc-utilsinvcgdocisreversed,
      lt_revdocno_r         TYPE RANGE OF zzexcp_i_utilitiesinvoicingdoc-reversalutilsinvcgdocument,
      lt_invoice_r          TYPE RANGE OF zzexcp_i_utilitiesinvoicingdoc-utilitiesinvoicingdocument,
      lt_contract_r         TYPE RANGE OF zzexcp_i_utilitiesinvoicingdoc-contractaccount.

    IF is_released IS NOT INITIAL.
      lt_notreleased_r = VALUE #( ( sign = 'I' option = 'EQ' low = '' ) ).
    ELSE.
      lt_notreleased_r = VALUE #( ( sign = 'I' option = 'EQ' low = 'X' ) ).
    ENDIF.

    lt_billrsn_r     = VALUE #( ( sign = 'I' option = 'EQ' low = '01' ) ).
    lt_simulated_r   = VALUE #( ( sign = 'I' option = 'EQ' low = '' ) ).
*    lr_posted      = VALUE #( ( sign = 'I' option = 'EQ' low = '' ) ).
    lt_reversed_r    = VALUE #( ( sign = 'I' option = 'EQ' low = '' ) ).
    lt_revdocno_r    = VALUE #( ( sign = 'I' option = 'EQ' low = '' ) ).

    TRY.
        "Create http client
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination(
                                  cl_http_destination_provider=>create_by_cloud_destination(
                                                 i_name       = 'ExceptionsConfigApp_usq_https_8001'
                                                 i_authn_mode = if_a4c_cp_service=>service_specific ) ).


        lo_client_proxy = cl_web_odata_client_factory=>create_v2_remote_proxy(
          EXPORTING
            iv_service_definition_name = 'ZUC_SCM_O_OUTSRTD_INVCG_DOC'
            io_http_client             = lo_http_client
            iv_relative_service_root   = '/sap/opu/odata/sap/ISU_IN_OUTSRTD_INVCG_DOC_SRV' ).


        " Navigate to the resource and create a request for the read operation
        lo_request = lo_client_proxy->create_resource_for_entity_set( 'I_UTILITIESINVOICINGDOCUMENT' )->create_request_for_read( ).

        " Create the filter tree
        lo_filter_factory   = lo_request->create_filter_factory( ).
        lo_filter_billrsn = lo_filter_factory->create_by_range( iv_property_path = 'UTILITIESBILLINGREASON'
                                                                it_range         = lt_billrsn_r ).
*        lo_filter_posted = lo_filter_factory->create_by_range( iv_property_path = 'UTILSINVCGDOCUMENTISPOSTED'
*                                                               it_range         = lr_posted ).
        lo_filter_simulated = lo_filter_factory->create_by_range( iv_property_path = 'UTILSINVCGDOCISSIMULATED'
                                                                  it_range         = lt_simulated_r ).
        lo_filter_notreleased = lo_filter_factory->create_by_range( iv_property_path = 'UTILSINVCGDOCISNOTRELEASED'
                                                                    it_range         = lt_notreleased_r ).
        lo_filter_reversed = lo_filter_factory->create_by_range( iv_property_path = 'UTILSINVCGDOCISREVERSED'
                                                                 it_range         = lt_reversed_r ).
        lo_filter_revdocno = lo_filter_factory->create_by_range( iv_property_path = 'REVERSALUTILSINVCGDOCUMENT'
                                                                 it_range         = lt_revdocno_r ).

        IF ir_isuaccount IS NOT INITIAL.
          lt_contract_r = ir_isuaccount.
          lo_filter_contract = lo_filter_factory->create_by_range( iv_property_path = 'CONTRACTACCOUNT'
                                                                   it_range         = lt_contract_r ).
          lo_filter_node_root = lo_filter_contract->and( lo_filter_billrsn )->and( lo_filter_simulated )->and( lo_filter_notreleased )->and( lo_filter_reversed )->and( lo_filter_revdocno ).
        ELSEIF ir_printdoc IS NOT INITIAL.
          lt_invoice_r = ir_printdoc.
          lo_filter_invoice = lo_filter_factory->create_by_range( iv_property_path = 'UTILITIESINVOICINGDOCUMENT'
                                                                  it_range         = lt_invoice_r ).
          lo_filter_node_root = lo_filter_contract->and( lo_filter_billrsn )->and( lo_filter_simulated )->and( lo_filter_notreleased )->and( lo_filter_reversed )->and( lo_filter_revdocno ).
        ENDIF.

        lo_request->set_filter( lo_filter_node_root ).

        " Execute the request and retrieve the business data
        lo_response = lo_request->execute( ).
        lo_response->get_business_data( IMPORTING et_business_data = lt_business_data ).

        IF lt_business_data IS NOT INITIAL.
          et_invoice = lt_business_data.
        ENDIF.

      CATCH /iwbep/cx_cp_remote INTO DATA(lv_remote).
        " Handle remote Exception
        " It contains details about the problems of your http(s) connection

      CATCH /iwbep/cx_gateway INTO DATA(lv_gateway).
        " Handle Exception

      CATCH cx_web_http_client_error INTO DATA(lv_client).
      CATCH cx_http_dest_provider_error INTO DATA(lv_dest).

    ENDTRY.
  ENDMETHOD.


  METHOD get_mtrrdngdocument.

    DATA:
      lt_business_data       TYPE TABLE OF zzexcp_c_mtrrdngdocumenttp,

      lo_http_client         TYPE REF TO if_web_http_client,
      lo_client_proxy        TYPE REF TO /iwbep/if_cp_client_proxy,
      lo_request             TYPE REF TO /iwbep/if_cp_request_read_list,
      lo_response            TYPE REF TO /iwbep/if_cp_response_read_lst,
      lo_filter_factory      TYPE REF TO /iwbep/if_cp_filter_factory,
      lo_filter_meterreaddoc TYPE REF TO /iwbep/if_cp_filter_node,
      lo_filter_node_root    TYPE REF TO /iwbep/if_cp_filter_node,

      lt_meterreadingdoc_r   TYPE RANGE OF zzexcp_c_mtrrdngdocumenttp-meterreadingdocument.

    TRY.
        "Create http client
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination(
                            cl_http_destination_provider=>create_by_cloud_destination(
                                           i_name       = 'ExceptionsConfigApp_usq_https_8001'
                                           i_authn_mode = if_a4c_cp_service=>service_specific ) ).

        lo_client_proxy = cl_web_odata_client_factory=>create_v2_remote_proxy(
          EXPORTING
            iv_service_definition_name = 'ZUC_SCM_O_IMPL_RESOLVE'
            io_http_client             = lo_http_client
            iv_relative_service_root   = '/sap/opu/odata/SAP/ISU_MR_IMPL_RESOLVE' ).


        lt_meterreadingdoc_r = ir_meterreadingdoc.

        " Navigate to the resource and create a request for the read operation
        lo_request = lo_client_proxy->create_resource_for_entity_set( 'C_MTRRDNGDOCUMENTTP' )->create_request_for_read( ).

        " Create the filter tree
        lo_filter_factory = lo_request->create_filter_factory( ).
        lo_filter_meterreaddoc = lo_filter_factory->create_by_range( iv_property_path = 'METERREADINGDOCUMENT'
                                                                     it_range         = lt_meterreadingdoc_r ).
        lo_filter_node_root = lo_filter_meterreaddoc.
        lo_request->set_filter( lo_filter_node_root ).

        " Execute the request and retrieve the business data
        lo_response = lo_request->execute( ).
        lo_response->get_business_data( IMPORTING et_business_data = lt_business_data ).

        IF lt_business_data IS NOT INITIAL.
          et_meterreadingdoc = lt_business_data.
        ENDIF.

      CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote).
        " Handle remote Exception
        " It contains details about the problems of your http(s) connection

      CATCH /iwbep/cx_gateway INTO DATA(lx_gateway).
        " Handle Exception

      CATCH cx_web_http_client_error INTO DATA(lv_client).
      CATCH cx_http_dest_provider_error INTO DATA(lv_dest).

    ENDTRY.

  ENDMETHOD.


  METHOD invoice_outsort.
    DATA:
      lt_invoice             TYPE STANDARD TABLE OF zzexcp_i_utilitiesinvoicingdoc,
      lt_invoice_erdk        TYPE STANDARD TABLE OF zzexcp_i_utilitiesinvoicingdoc,
      lt_invoice_all         TYPE STANDARD TABLE OF zzexcp_i_utilitiesinvoicingdoc,
      lt_invoice_fin         TYPE STANDARD TABLE OF zzexcp_i_utilitiesinvoicingdoc,
      lt_inv_failed          TYPE STANDARD TABLE OF zzexcp_i_utilitiesinvoicingdoc,
      lt_sorted_contract     TYPE SORTED TABLE OF gs_contract
                             WITH UNIQUE KEY low,
      lt_cases               TYPE STANDARD TABLE OF zcl_uc_isu_c4c_emma_case
                             WITH NON-UNIQUE SORTED KEY main_object_type
                             COMPONENTS main_object_type,
      lt_cases_fin           TYPE STANDARD TABLE OF zcl_uc_isu_c4c_emma_case,
      lt_cases_failed        TYPE STANDARD TABLE OF zcl_uc_isu_c4c_emma_case,
      lt_isuaccount          TYPE STANDARD TABLE OF zcl_uc_isu_c4c_emma_case,
      lt_printdoc            TYPE STANDARD TABLE OF zcl_uc_isu_c4c_emma_case,
      lt_contract_checkgrp   TYPE STANDARD TABLE OF zzexcp_i_contractaccountpartne,
      lt_contract_amnt       TYPE STANDARD TABLE OF zzexcp_i_utilitiesinvoicingdoc,
      lt_sorted_busidata     TYPE SORTED TABLE OF zzexcp_i_utilitiesinvoicingdoc
                             WITH UNIQUE KEY utilitiesinvoicingdocument,
      lt_sum_contract        TYPE STANDARD TABLE OF zzexcp_i_utilitiesinvoicingdoc,
      lt_invoice_excl        TYPE STANDARD TABLE OF zzexcp_i_utilitiesinvoicingdoc,
      lt_invoice_incl        TYPE STANDARD TABLE OF zzexcp_i_utilitiesinvoicingdoc,
      lwa_invoice_tmp        TYPE zzexcp_i_utilitiesinvoicingdoc,
      lt_data                TYPE tt_contractaccnt,
      lt_checks              TYPE STANDARD TABLE OF zexcp_bpem_check,

      lwa_inv_failed         TYPE zzexcp_i_utilitiesinvoicingdoc,

      lv_contract            TYPE zzexcp_i_utilitiesinvoicingdoc-contractaccount,
      lv_prev_date           TYPE zzexcp_i_utilitiesinvoicingdoc-postingdate,
      lv_init                TYPE zzexcp_i_utilitiesinvoicingdoc-totalamountintransactioncrcy,
      lv_sum                 TYPE zzexcp_i_utilitiesinvoicingdoc-totalamountintransactioncrcy,
      lv_average             TYPE zzexcp_i_utilitiesinvoicingdoc-totalamountintransactioncrcy,
      lv_percent             TYPE zzexcp_i_utilitiesinvoicingdoc-totalamountintransactioncrcy,
      lv_percentage          TYPE zexcp_hist_perc-from_prcnt,
      lv_failed_inv          TYPE abap_bool,
      lv_curr_date           TYPE d,
      lv_date                TYPE d,
      lv_prev_year           TYPE d,
      lv_count               TYPE i,

      lt_invoice_r           TYPE RANGE OF zzexcp_i_utilitiesinvoicingdoc-utilitiesinvoicingdocument,
      lt_contract_checkgrp_r TYPE RANGE OF zzexcp_i_contractaccountpartne-contractaccount,
      lt_res_category_r      TYPE RANGE OF zexcp_outsortbpm-category,
      lt_nres_category_r     TYPE RANGE OF zexcp_outsortbpm-category,
      lt_res_checkgrp_r      TYPE RANGE OF zexcp_outsort_io-check_grp,
      lt_nres_checkgrp_r     TYPE RANGE OF zexcp_outsort_io-check_grp,
      lt_res_amount_r        TYPE RANGE OF zexcp_outsortamt-from_amnt,
      lt_nres_amount_r       TYPE RANGE OF zexcp_outsortamt-from_amnt,
      lt_category_r          TYPE RANGE OF zexcp_hist_bpem-category,
      lt_checkgrp_r          TYPE RANGE OF zexcp_hist_io-check_grp,
      lt_percentage_r        TYPE RANGE OF zexcp_hist_perc-from_prcnt,
      lt_isuaccount_r        TYPE RANGE OF zcl_uc_isu_c4c_emma_case-main_object_key,
      lt_printdoc_r          TYPE RANGE OF zcl_uc_isu_c4c_emma_case-main_object_key,
      lt_contract_r          TYPE RANGE OF zzexcp_i_utilitiesinvoicingdoc-contractaccount,
      lt_contractacct_r      TYPE RANGE OF zzexcp_i_utilitiesinvoicingdoc-contractaccount.

    "Get configured BPEM checks
    bpem_checks(
      IMPORTING
        et_checks = lt_checks ).

    IF line_exists( lt_checks[ key_id = mc_invoice_outsort-invoutchk
                               is_active = abap_true ] ).
      "Get BPEM Cases
      get_bpem_cases(
        EXPORTING
          iv_invoutsort = abap_true
        IMPORTING
          et_cases      = lt_cases ).

      IF lt_cases IS NOT INITIAL.
        SORT lt_cases BY main_object_type
                         main_object_key.
      ENDIF.

      "Main object type = ISUACCOUNT
      lt_isuaccount = FILTER #( lt_cases USING KEY main_object_type
                                        WHERE main_object_type = 'ISUACCOUNT' ).

      IF lt_isuaccount IS NOT INITIAL.
        lt_cases_fin = lt_isuaccount.
        lt_isuaccount_r[]  = VALUE #( FOR lwa_contract IN lt_isuaccount
                                  ( sign = 'I' option = 'EQ' low = lwa_contract-main_object_key ) ).
        "Get invoice in ERDK via contract
        get_invoice_details(
          EXPORTING
            ir_isuaccount = lt_isuaccount_r
          IMPORTING
            et_invoice    = lt_invoice ).

        IF lt_invoice IS NOT INITIAL.
          lt_invoice_r[]  = VALUE #( FOR lwa_invoice IN lt_invoice
                                    ( sign = 'I' option = 'EQ' low = lwa_invoice-utilitiesinvoicingdocument ) ).
          lt_invoice_all = lt_invoice.
        ENDIF.
      ENDIF.

      "Main object type = PRINTDOC
      lt_printdoc = FILTER #( lt_cases USING KEY main_object_type
                                        WHERE main_object_type = mc_invoice_outsort-printdoc ).

      IF lt_printdoc IS NOT INITIAL.
        lt_cases_fin = VALUE #( BASE lt_cases_fin FOR lwa_printdoc IN lt_printdoc
                                  ( lwa_printdoc ) ).
        lt_printdoc_r[]  = VALUE #( FOR lwa_contract IN lt_printdoc
                                  ( sign = 'I' option = 'EQ' low = lwa_contract-main_object_key ) ).
        "Get invoice details in ERDK via invoice
        get_invoice_details(
          EXPORTING
            ir_printdoc = lt_printdoc_r
          IMPORTING
            et_invoice  = lt_invoice_erdk ).

        IF lt_invoice_erdk IS NOT INITIAL.
          lt_invoice_r[]  = VALUE #( BASE lt_invoice_r FOR ls_invoice IN lt_invoice
                                  ( sign = 'I' option = 'EQ' low = ls_invoice-utilitiesinvoicingdocument ) ).

          lt_invoice_all = VALUE #( BASE lt_invoice_all FOR lwa_invoice_erdk IN lt_invoice_erdk
                                    ( lwa_invoice_erdk ) ).
        ENDIF.
      ENDIF.

      "Check if invoice outsort exist in ERDO
      IF lt_invoice_r IS NOT INITIAL.
        DATA(lr_inv_erdo) = lt_invoice_r.
        CLEAR: lt_invoice_r.
        outsorted_invoice(
          EXPORTING
            ir_invoice = lr_inv_erdo
          IMPORTING
            er_invoice = lt_invoice_r ).

        IF lt_invoice_r IS NOT INITIAL.
          DATA(lt_inv_contract) = lt_invoice_all.
          SORT lt_inv_contract BY contractaccount.
          DELETE ADJACENT DUPLICATES FROM lt_inv_contract
            COMPARING contractaccount.

          "Store all contract
          lt_contractacct_r = VALUE #( FOR lwa_contractacct IN lt_inv_contract
                                      ( sign = 'I' option = 'EQ' low = lwa_contractacct-contractaccount ) ).

          "Get invoice check outsort group
          invoicing_check_group(
            EXPORTING
              ir_contract          = lt_contractacct_r
            IMPORTING
              er_contract_checkgrp = lt_contract_checkgrp_r
              et_contract          = lt_contract_checkgrp ).

          lt_sorted_contract = lt_contract_checkgrp_r.
          lt_invoice_fin = FILTER #( lt_invoice_all IN lt_sorted_contract
                                     WHERE contractaccount = low ).
        ENDIF.
      ENDIF.

      "Credit Invoice Outsort Validation
      IF line_exists( lt_checks[ key_id = mc_invoice_outsort-crdtinv
                               is_active = abap_true ] ).
        "Retrieve configured case categories
        case_category_cred_io(
          IMPORTING
            er_res_category  = lt_res_category_r
            er_nres_category = lt_nres_category_r ).

        "Outsort check group -
        check_group_cred_io(
          IMPORTING
            er_res_checkgrp  = lt_res_checkgrp_r
            er_nres_checkgrp = lt_nres_checkgrp_r ).

        "Amount range
        amount_range(
          IMPORTING
            er_res_amount  = lt_res_amount_r
            er_nres_amount = lt_nres_amount_r ).

        "Comment exclusion logic as per request by Ahmer
*        LOOP AT lt_invoice_fin ASSIGNING FIELD-SYMBOL(<lwa_inv_fin>).
*          "Passing Contract Account and Posting Date to retrieve in
*
*          me->call_read_table_rfc_cntr_accnt( EXPORTING im_vkont = <lwa_inv_fin>-ContractAccount
*                                              IMPORTING ex_data  = lt_data ).
*
*          DATA(lv_postingdate) = CONV string( <lwa_inv_fin>-postingdate ).
*          lv_postingdate = lv_postingdate+0(8).
*          READ TABLE lt_data ASSIGNING FIELD-SYMBOL(<lfs_contract_assn>) INDEX 1.
*          IF  sy-subrc EQ 0.
*            IF  <lfs_contract_assn>-budat GT  lv_postingdate AND
*                <lfs_contract_assn>-betrw NE '0.00'.
*
*              lwa_invoice_tmp = CORRESPONDING #( <lwa_inv_fin> ).
*              APPEND lwa_invoice_tmp TO lt_invoice_excl.
*              CLEAR lwa_invoice_tmp.
*            ELSE.
*              lwa_invoice_tmp = CORRESPONDING #( <lwa_inv_fin> ).
*              APPEND lwa_invoice_tmp TO lt_invoice_incl.
*              CLEAR lwa_invoice_tmp.
*            ENDIF.
*          ENDIF.
*        ENDLOOP.
*        UNASSIGN <lwa_inv_fin>.
*
*        IF lt_invoice_incl[] IS NOT INITIAL.
*          lt_invoice_fin[] =  lt_invoice_incl[].
*          FREE lt_invoice_incl.
*        ENDIF.
*
*        IF lt_invoice_excl[] IS NOT INITIAL.
**          SORT lt_invoice_fin ASCENDING.
*          IF NOT lt_invoice_excl[] IS INITIAL.
*            LOOP AT lt_invoice_fin
*              ASSIGNING FIELD-SYMBOL(<lfs_inv_del>).
*              IF line_exists( lt_invoice_excl[ utilitiesinvoicingdocument = <lfs_inv_del>-utilitiesinvoicingdocument ] ).
*                CLEAR <lfs_inv_del>-utilitiesinvoicingdocument.
*              ENDIF.
*            ENDLOOP.
*
*            DELETE lt_invoice_fin WHERE utilitiesinvoicingdocument IS INITIAL.
*          ENDIF.
*        ENDIF.

        LOOP AT lt_cases_fin[] ASSIGNING FIELD-SYMBOL(<lwa_cases>).
          lv_failed_inv = abap_true.

          IF <lwa_cases>-main_object_type EQ 'ISUACCOUNT'.
            ASSIGN lt_invoice_fin[ contractaccount = <lwa_cases>-main_object_key ]
              TO FIELD-SYMBOL(<lwa_invoice>).
            IF sy-subrc EQ 0.
              "Do nothing
            ENDIF.
          ELSEIF <lwa_cases>-main_object_type EQ 'PRINTDOC'.
            ASSIGN lt_invoice_fin[ utilitiesinvoicingdocument = <lwa_cases>-main_object_key ]
              TO <lwa_invoice>.
            IF sy-subrc EQ 0.
              "Do nothing
            ENDIF.
          ENDIF.

          IF <lwa_invoice> IS ASSIGNED AND <lwa_invoice> IS NOT INITIAL.
            DATA(lv_checkgrp) = lt_contract_checkgrp[ contractaccount = <lwa_invoice>-contractaccount ]-utilsoutsrtgcheckgrpforinvcg.

            IF <lwa_cases>-case_category IN lt_res_category_r[].
              IF lv_checkgrp IN lt_res_checkgrp_r[].
                IF <lwa_invoice>-totalamountintransactioncrcy IN lt_res_amount_r[].
                  "Release invoice document
                  io_release_invoice(
                    EXPORTING
                      es_invoice = <lwa_invoice>
                  ).
                  CLEAR: lv_failed_inv.
                ENDIF.
              ENDIF.
            ELSEIF <lwa_cases>-case_category IN lt_nres_category_r[].
              IF lv_checkgrp IN lt_nres_checkgrp_r[].
                IF <lwa_invoice>-totalamountintransactioncrcy IN lt_nres_amount_r[].
                  "Release invoice document
                  io_release_invoice(
                    EXPORTING
                      es_invoice = <lwa_invoice>
                  ).
                  CLEAR: lv_failed_inv.
                ENDIF.
              ENDIF.
            ENDIF.
            CLEAR:
              lv_checkgrp.
          ENDIF.

          IF lv_failed_inv IS NOT INITIAL.

            IF <lwa_invoice> IS ASSIGNED.
              APPEND <lwa_invoice> TO lt_inv_failed.
            ENDIF.
            IF  <lwa_cases>   IS ASSIGNED.
              APPEND <lwa_cases> TO lt_cases_failed.
            ENDIF.

          ELSE.
            "Mark case as completed
            io_complete_case(
              EXPORTING
                es_casenumber = <lwa_cases>-casenumber
            ).
          ENDIF.

          IF <lwa_invoice> IS ASSIGNED.
            UNASSIGN <lwa_invoice>.
          ENDIF.
        ENDLOOP.

      ENDIF.

      "12 months Average Outsort Validation
      IF line_exists( lt_checks[ key_id = mc_invoice_outsort-monthsave
                               is_active = abap_true ] ).
        "Case categories
        case_category_hist(
          IMPORTING
            er_category = lt_category_r ).

        "Outsort check group -
        check_group_hist(
          IMPORTING
            er_checkgrp = lt_checkgrp_r ).

        "Percentage range
        percentage_range(
          IMPORTING
            er_percentage = lt_percentage_r ).

        IF line_exists( lt_checks[ key_id = mc_invoice_outsort-crdtinv
                                is_active = abap_true ] ).
          lt_cases_fin[] = lt_cases_failed[].
          lt_invoice_fin[] = lt_inv_failed[].
        ENDIF.

        IF lt_invoice_excl[] IS NOT INITIAL.
          lt_invoice_fin = VALUE #( BASE lt_invoice_fin FOR lwa_invoice_excl IN lt_invoice_excl
                                    ( lwa_invoice_excl ) ).
        ENDIF.

        LOOP AT lt_cases_fin[] ASSIGNING <lwa_cases>.
          lv_failed_inv = abap_true.

          IF <lwa_cases>-main_object_type EQ 'ISUACCOUNT'.
            ASSIGN lt_invoice_fin[ contractaccount = <lwa_cases>-main_object_key ]
              TO <lwa_invoice>.
            IF sy-subrc EQ 0.
              "Do nothing
            ENDIF.
          ELSEIF <lwa_cases>-main_object_type EQ 'PRINTDOC'.
            ASSIGN lt_invoice_fin[ utilitiesinvoicingdocument = <lwa_cases>-main_object_key ]
              TO <lwa_invoice>.
            IF sy-subrc EQ 0.
              "Do nothing
            ENDIF.
          ENDIF.

          IF <lwa_invoice> IS ASSIGNED AND <lwa_invoice> IS NOT INITIAL.
            lv_checkgrp = lt_contract_checkgrp[ contractaccount = <lwa_invoice>-contractaccount ]-utilsoutsrtgcheckgrpforinvcg.

            IF <lwa_cases>-case_category IN lt_category_r.
              IF lv_checkgrp IN lt_checkgrp_r.
                lt_contract_r[]  = VALUE #( ( sign = 'I' option = 'EQ' low = <lwa_invoice>-contractaccount ) ).

                "Get invoice details
                get_invoice_details(
                  EXPORTING
                    ir_isuaccount = lt_contract_r
                    is_released   = abap_true
                  IMPORTING
                    et_invoice    = lt_contract_amnt ).

                IF lt_contract_amnt IS NOT INITIAL.
                  SORT lt_contract_amnt BY postingdate DESCENDING.

                  DATA(lv_lines) = lines( lt_contract_amnt ).
                  IF lv_lines GE 12.
                    DATA(lt_temp_amnt) = lt_contract_amnt.
                    CLEAR: lt_contract_amnt.
                    LOOP AT lt_temp_amnt ASSIGNING FIELD-SYMBOL(<lfs_amnt>).
                      IF lv_count EQ 12.
                        EXIT.
                      ENDIF.
                      APPEND <lfs_amnt> TO lt_contract_amnt.
                      lv_count += 1.
                    ENDLOOP.
                    lv_sum = REDUCE #( INIT sum = lv_init
                                       FOR lwa_amount IN lt_contract_amnt
                                       NEXT sum = sum + lwa_amount-totalamountintransactioncrcy ).
                    lv_average = lv_sum / lv_count.
                    lv_percentage = ( <lwa_invoice>-totalamountintransactioncrcy / lv_average ) * mc_invoice_outsort-hundred.

                    IF lv_percentage IN lt_percentage_r.
                      "Release invoice document
                      io_release_invoice(
                        EXPORTING
                          es_invoice = <lwa_invoice>
                      ).
                      CLEAR: lv_failed_inv.
                    ENDIF.

                    CLEAR:
                      lv_count,
                      lv_sum,
                      lv_average,
                      lv_percentage.
                  ENDIF.
                  CLEAR:
                    lv_lines.
                ENDIF.
                CLEAR:
                  lt_contract_amnt,
                  lt_contract_r.
              ENDIF.
              CLEAR:
                lv_checkgrp.
            ENDIF.
          ENDIF.

          IF lv_failed_inv IS NOT INITIAL.
            IF <lwa_invoice> IS ASSIGNED.
              APPEND <lwa_invoice> TO lt_inv_failed.
            ENDIF.
            IF <lwa_cases> IS ASSIGNED.
              APPEND <lwa_cases> TO lt_cases_failed.
            ENDIF.
          ELSE.
            "Mark case as completed
            io_complete_case(
              EXPORTING
                es_casenumber = <lwa_cases>-casenumber
            ).
          ENDIF.

          IF <lwa_invoice> IS ASSIGNED.
            UNASSIGN <lwa_invoice>.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD invoicing_check_group.
    DATA:
      lt_business_data       TYPE TABLE OF zzexcp_i_contractaccountpartne,
      lt_sorted_data         TYPE SORTED TABLE OF zzexcp_i_contractaccountpartne
                           WITH UNIQUE KEY contractaccount,

      lo_http_client         TYPE REF TO if_web_http_client,
      lo_client_proxy        TYPE REF TO /iwbep/if_cp_client_proxy,
      lo_request             TYPE REF TO /iwbep/if_cp_request_read_list,
      lo_response            TYPE REF TO /iwbep/if_cp_response_read_lst,
      lo_filter_factory      TYPE REF TO /iwbep/if_cp_filter_factory,
      lo_filter_invoice      TYPE REF TO /iwbep/if_cp_filter_node,
      lo_filter_node_root    TYPE REF TO /iwbep/if_cp_filter_node,

      lt_contractacct_r      TYPE RANGE OF zzexcp_i_contractaccountpartne-contractaccount,
      lt_contract_r          TYPE RANGE OF zzexcp_i_contractaccountpartne-contractaccount,
      lt_contract_checkgrp_r TYPE RANGE OF zzexcp_i_contractaccountpartne-contractaccount.

    lt_contractacct_r = ir_contract.

    TRY.
        " Create http client
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination(
                                  cl_http_destination_provider=>create_by_cloud_destination(
                                                 i_name       = 'ExceptionsConfigApp_usq_https_8001'
                                                 i_authn_mode = if_a4c_cp_service=>service_specific ) ).

        lo_client_proxy = cl_web_odata_client_factory=>create_v2_remote_proxy(
          EXPORTING
            iv_service_definition_name = 'ZUC_SCM_O_OUTSRTD_RESOLVE'
            io_http_client             = lo_http_client
            iv_relative_service_root   = '/sap/opu/odata/sap/ISU_BI_OUTSRTD_RESOLVE' ).

        " Navigate to the resource and create a request for the read operation
        lo_request = lo_client_proxy->create_resource_for_entity_set( 'I_CONTRACTACCOUNTPARTNER' )->create_request_for_read( ).

        " Create the filter tree
        lo_filter_factory = lo_request->create_filter_factory( ).

        lo_filter_invoice = lo_filter_factory->create_by_range( iv_property_path = 'CONTRACTACCOUNT'
                                                                it_range         = lt_contractacct_r ).

        lo_filter_node_root = lo_filter_invoice.
        lo_request->set_filter( lo_filter_node_root ).

        " Execute the request and retrieve the business data
        lo_response = lo_request->execute( ).
        lo_response->get_business_data( IMPORTING et_business_data = lt_business_data ).

        IF lt_business_data IS NOT INITIAL.
          et_contract = lt_business_data.
          lt_sorted_data = lt_business_data.
          lt_contract_checkgrp_r = FILTER #( lt_contractacct_r IN lt_sorted_data
                                   WHERE low = contractaccount ).
          IF lt_contract_checkgrp_r IS NOT INITIAL.
            er_contract_checkgrp = lt_contract_checkgrp_r.
          ENDIF.
        ELSE.
          CLEAR: er_contract_checkgrp, et_contract.
        ENDIF.


      CATCH /iwbep/cx_cp_remote INTO DATA(lv_remote).
        " Handle remote Exception
        " It contains details about the problems of your http(s) connection

      CATCH /iwbep/cx_gateway INTO DATA(lv_gateway).
        " Handle Exception

      CATCH cx_web_http_client_error INTO DATA(lv_client).
      CATCH cx_http_dest_provider_error INTO DATA(lv_dest).

    ENDTRY.


  ENDMETHOD.


  METHOD io_release_invoice.

    DATA:
      ls_invoice    TYPE zzexcp_i_utilitiesinvoicingdoc,
      l_docdate     TYPE string,
      l_postingdate TYPE string.

    ls_invoice = es_invoice.
    TRY.
        DATA ls_http_header_fields  TYPE if_web_http_request=>name_value_pairs  .

        ls_http_header_fields = VALUE #( ( name = if_web_http_header=>accept value = |application/json| )
                                         ( name = if_web_http_header=>content_type value =  |application/json|  )
                                         ( name = 'x-csrf-token' value = 'fetch' ) ).

        DATA(lv_service_relative_url) = '/sap/opu/odata/sap/ISU_IN_OUTSRTD_INVCG_DOC_SRV'.
        DATA(lv_relative_url) = |{ lv_service_relative_url }?sap-client=200|.

        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination(
                                  cl_http_destination_provider=>create_by_cloud_destination(
                                                 i_name       = 'ExceptionsConfigApp_usq_https_8001'
                                                 i_authn_mode = if_a4c_cp_service=>service_specific ) ).

        " GET CSRF token
        lo_http_client->get_http_request( )->set_uri_path( i_uri_path = lv_relative_url ).
        lo_http_client->get_http_request( )->set_header_fields( ls_http_header_fields ).

        DATA(lo_http_response) = lo_http_client->execute( if_web_http_client=>get ).
        DATA(lv_http_status_code) = lo_http_response->get_status( ).
        DATA(lv_x_csrf_token) =  lo_http_response->get_header_field( 'x-csrf-token' ).
        DATA(lv_http_response_body) =  lo_http_response->get_text( ).
        l_docdate = ls_invoice-documentdate.
        l_postingdate = ls_invoice-postingdate.
        DATA(lv_relative_url2) = |{ lv_service_relative_url }/C_OutsrtdUtilsInvcgDocumentRelease?| &&
                              |DocumentDate='{ l_docdate(8) }'&| &&
                              |PostingDate='{ l_postingdate(8) }'&| &&
                              |Reasonforreversal=''&| &&
                              |Reconciliationkey='{ ls_invoice-careconciliationkey }'&| &&
                              |Reversaldate=''&| &&
                              |UtilitiesInvoicingDocument='{ ls_invoice-utilitiesinvoicingdocument }'|.
        lo_http_client->get_http_request( )->set_uri_path( i_uri_path = lv_relative_url2 ).
        DATA(lv_text) = lo_http_client->get_http_request( )->get_text( ).

        ls_http_header_fields = VALUE #( ( name = if_web_http_header=>accept value = |application/json| )
                                         ( name = 'x-csrf-token' value =  lv_x_csrf_token ) ).

        lo_http_client->get_http_request( )->set_header_fields( ls_http_header_fields ).

        lo_http_response = lo_http_client->execute( if_web_http_client=>post ).
        lv_http_status_code = lo_http_response->get_status( ).
        DATA(response) =  lo_http_response->get_text( ).

    ENDTRY.

  ENDMETHOD.


  METHOD percentage_range.

    DATA:
      lt_percentage_r  TYPE RANGE OF zexcp_hist_perc-from_prcnt.

    "Get amount range from configured checks
    SELECT from_prcnt
           to_prcnt
      FROM zexcp_hist_perc
      INTO @DATA(ls_perc)
      UP TO 1 ROWS.
    ENDSELECT.
    IF sy-subrc EQ 0.
      lt_percentage_r = VALUE #( ( sign   = 'I'
                                   option = 'BT'
                                   low    = ls_perc-from_prcnt
                                   high   = ls_perc-to_prcnt   ) ).
    ENDIF.

    IF lt_percentage_r[] IS NOT INITIAL.
      er_percentage = lt_percentage_r[].
    ENDIF.

  ENDMETHOD.


  METHOD util_invoice_doc.

    DATA:
      lt_invoice_r        TYPE RANGE OF zzexcp_c_outsrtdutilsinvcgdocu-utilitiesinvoicingdocument,
      lt_business_data    TYPE TABLE OF zzexcp_c_outsrtdutilsinvcgdocu,
      lo_http_client      TYPE REF TO if_web_http_client,
      lo_client_proxy     TYPE REF TO /iwbep/if_cp_client_proxy,
      lo_request          TYPE REF TO /iwbep/if_cp_request_read_list,
      lo_response         TYPE REF TO /iwbep/if_cp_response_read_lst,
      lo_filter_factory   TYPE REF TO /iwbep/if_cp_filter_factory,
      lo_filter_invoice   TYPE REF TO /iwbep/if_cp_filter_node,
      lo_filter_node_root TYPE REF TO /iwbep/if_cp_filter_node.

    lt_invoice_r = ir_invoice.

    TRY.
        "Create http client
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination(
                                  cl_http_destination_provider=>create_by_cloud_destination(
                                                 i_name       = 'ExceptionsConfigApp_usq_https_8001'
                                                 i_authn_mode = if_a4c_cp_service=>service_specific ) ).


        lo_client_proxy = cl_web_odata_client_factory=>create_v2_remote_proxy(
          EXPORTING
            iv_service_definition_name = 'ZUC_SCM_O_OUTSRTD_INVCG_DOC'
            io_http_client             = lo_http_client
            iv_relative_service_root   = '/sap/opu/odata/sap/ISU_IN_OUTSRTD_INVCG_DOC_SRV' ).

        " Navigate to the resource and create a request for the read operation
        lo_request = lo_client_proxy->create_resource_for_entity_set( 'C_OUTSRTDUTILSINVCGDOCUMENT' )->create_request_for_read( ).

        " Create the filter tree
        lo_filter_factory   = lo_request->create_filter_factory( ).
        lo_filter_invoice = lo_filter_factory->create_by_range( iv_property_path = 'UTILITIESINVOICINGDOCUMENT'
                                                                it_range         = lt_invoice_r ).
        lo_filter_node_root = lo_filter_invoice.

        lo_request->set_filter( lo_filter_node_root ).

        " Execute the request and retrieve the business data
        lo_response = lo_request->execute( ).
        lo_response->get_business_data( IMPORTING et_business_data = lt_business_data ).

        IF lt_business_data IS NOT INITIAL.
          et_invoice = lt_business_data.
        ENDIF.

      CATCH /iwbep/cx_cp_remote INTO DATA(lv_remote).
        " Handle remote Exception
        " It contains details about the problems of your http(s) connection

      CATCH /iwbep/cx_gateway INTO DATA(lv_gateway).
        " Handle Exception

      CATCH cx_web_http_client_error INTO DATA(lv_client).
      CATCH cx_http_dest_provider_error INTO DATA(lv_dest).

    ENDTRY.
  ENDMETHOD.
  METHOD call_read_category_case.

    IF iv_outsort IS NOT INITIAL.
      SELECT category
        FROM zexcp_outsortbpm
        INTO TABLE @DATA(lt_outsort_cat).
      IF sy-subrc EQ 0.
        "Do nothing
      ENDIF.

      SELECT category
        FROM zexcp_hist_bpem
        APPENDING TABLE @lt_outsort_cat.
      IF sy-subrc EQ 0.
        "Do nothing
      ENDIF.

      IF lt_outsort_cat[] IS NOT INITIAL.
        SORT lt_outsort_cat[] BY category.
        DELETE ADJACENT DUPLICATES FROM lt_outsort_cat[]
          COMPARING category.
        er_cat_case = lt_outsort_cat[].
      ENDIF.

    ELSEIF iv_imr IS NOT INITIAL.
      SELECT category
        FROM zexcp_imr_cat
        INTO TABLE @DATA(lt_imr_cat).
      IF sy-subrc EQ 0.
        er_cat_case = lt_imr_cat[].
      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
