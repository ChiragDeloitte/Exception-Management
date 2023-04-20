@EndUserText.label: 'Credit Invoice Outsort - Check Group CDS Consumption'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity zexcp_c_outsort_io
  provider contract transactional_query
  as projection on zexcp_i_outsort_io
{
  key CheckGrp,
  key Ctype,
      ClientGuid,
      CreatedBy,
      CreatedOn,
      ChangedBy,
      ChangedOn
}
