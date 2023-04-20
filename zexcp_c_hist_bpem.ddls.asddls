@EndUserText.label: 'History Category CDS Consumption'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity zexcp_c_hist_bpem
  provider contract transactional_query
  as projection on zexcp_i_hist_bpem
{
  key Category,
      ClientGuid,
      CreatedBy,
      CreatedOn,
      ChangedBy,
      ChangedOn
}
