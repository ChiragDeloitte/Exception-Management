@EndUserText.label: 'Implausible Meter Rates CDS Consumption'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity zexcp_c_imr_rates
  provider contract transactional_query
  as projection on zexcp_i_imr_rates
{
  key RateCategory,
  key Season,
      MonthlyAve,
      ToleranceH,
      ToleranceL,
      WinterStart,
      WinterEnd,
      SummerStart,
      SummerEnd,
      ClientGuid,
      CreatedBy,
      CreatedOn,
      ChangedBy,
      ChangedOn
}
