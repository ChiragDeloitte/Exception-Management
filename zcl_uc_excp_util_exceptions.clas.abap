CLASS zcl_uc_excp_util_exceptions DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
        ty_numc2(2) TYPE n,
        ty_numc4(4) TYPE n.
      CLASS-METHODS:
        calculate_date
          IMPORTING
            iv_date          TYPE d
            iv_days          TYPE ty_numc2 OPTIONAL
            iv_months        TYPE ty_numc2 OPTIONAL
            iv_years         TYPE ty_numc4 OPTIONAL
            iv_signum        TYPE c
          RETURNING
            VALUE(rv_output) TYPE d.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      mc_round TYPE p DECIMALS 2 VALUE '0.50'.
ENDCLASS.



CLASS zcl_uc_excp_util_exceptions IMPLEMENTATION.

  METHOD calculate_date.
    DATA: lv_act_date  TYPE d,
          lv_dys       TYPE p,
          lv_mon       TYPE p,
          lv_yrs       TYPE p,
          lv_ttl_yrs   TYPE p VALUE 0,
          lv_round     TYPE p DECIMALS 2,
          lv_test_date TYPE d,
          lv_corr_date TYPE d.

    IF iv_date IS NOT INITIAL.
      lv_act_date = iv_date.

      IF iv_signum = '-'.
        lv_dys   = - iv_days.
        lv_mon   = - iv_months.
        lv_yrs   = - iv_years.
        lv_round = - mc_round.
      ELSE.
        lv_dys   = iv_days.
        lv_mon   = iv_months.
        lv_yrs   = iv_years.
        lv_round = mc_round.
      ENDIF.

      lv_act_date = lv_act_date + lv_dys.
*---------------------------------------------------------------------
      IF lv_mon <> 0.
        DATA(act_month) = lv_act_date+4(2).

        IF lv_mon > 11 OR
           lv_mon < -11.
          lv_yrs = lv_mon / 12 - lv_round.
          lv_mon = lv_mon - lv_yrs * 12.
        ENDIF.

        lv_mon = act_month + lv_mon.

        IF lv_mon <= 0.
          lv_yrs = lv_yrs - 1.
          lv_mon = lv_mon + 12.
        ELSE.
          IF lv_mon > 12.
            lv_yrs = lv_yrs + 1.
            lv_mon = lv_mon - 12.
          ENDIF.
        ENDIF.

        lv_act_date+4(2) = lv_mon.
      ENDIF.

      lv_ttl_yrs     = lv_yrs + lv_ttl_yrs.
      lv_act_date(4) = lv_act_date(4) + lv_ttl_yrs.
      lv_corr_date   = lv_act_date.

      DO.
        lv_test_date = 1 + lv_corr_date - 1.

        IF lv_test_date = lv_corr_date.
          EXIT.
        ENDIF.

        IF iv_signum = '-'.
          lv_corr_date+6(2) = lv_corr_date+6(2) - 1.
        ELSE.
          lv_corr_date+6(2) = '01'.
          lv_corr_date      = lv_corr_date + 32.
          lv_corr_date+6(2) = '01'.
        ENDIF.
      ENDDO.

      rv_output = lv_corr_date.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
