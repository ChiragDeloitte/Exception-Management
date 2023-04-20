@EndUserText.label: 'Credit Invoice Outsort - BPEM category CDS Consumption'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity zexcp_c_outsort_bpem
  provider contract transactional_query
  as projection on zexcp_i_outsort_bpem
{
  key Category,
  key Ctype,
      ClientGuid,
      CreatedBy,
      CreatedOn,
      ChangedBy,
      ChangedOn
}
