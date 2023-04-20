@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Implausible Meter Rates CDS Interface'
define root view entity zexcp_i_imr_rates
  as select from zexcp_imr_rates as IMRRate
{
  key IMRRate.rate_category as RateCategory,
  key IMRRate.season        as Season,
      IMRRate.monthly_ave   as MonthlyAve,
      IMRRate.tolerance_h   as ToleranceH,
      IMRRate.tolerance_l   as ToleranceL,
      IMRRate.winter_start  as WinterStart,
      IMRRate.winter_end    as WinterEnd,
      IMRRate.summer_start  as SummerStart,
      IMRRate.summer_end    as SummerEnd,
      IMRRate.client_guid   as ClientGuid,
      IMRRate.created_by    as CreatedBy,
      IMRRate.created_on    as CreatedOn,
      IMRRate.changed_by    as ChangedBy,
      IMRRate.changed_on    as ChangedOn
}
