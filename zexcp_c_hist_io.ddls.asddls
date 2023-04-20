@EndUserText.label: 'History Check Group CDS Consumption'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity zexcp_c_hist_io
  provider contract transactional_query
  as projection on zexcp_i_hist_io
{
  key CheckGrp,
      ClientGuid,
      CreatedBy,
      CreatedOn,
      ChangedBy,
      ChangedOn
}
