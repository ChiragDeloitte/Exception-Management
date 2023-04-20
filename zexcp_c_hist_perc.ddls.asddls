@EndUserText.label: 'History Percentage CDS Consumption'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity zexcp_c_hist_perc
  provider contract transactional_query
  as projection on zexcp_i_hist_perc
{
  key ClientId,
      FromPrcnt,
      ToPrcnt,
      ClientGuid,
      CreatedBy,
      CreatedOn,
      ChangedBy,
      ChangedOn
}
