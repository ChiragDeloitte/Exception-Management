@EndUserText.label: 'Credit Invoice Outsort Amount CDS Consumption'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity zexcp_c_outsort_amnt
  provider contract transactional_query
  as projection on zexcp_i_outsort_amnt
{
  key Ctype,
      FromAmnt,
      ToAmnt,
      ClientGuid,
      CreatedBy,
      CreatedOn,
      ChangedBy,
      ChangedOn
}
