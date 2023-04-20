@EndUserText.label: 'Implausible Meter Category CDS Consumption'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity zexcp_c_imr_cat
  provider contract transactional_query
  as projection on zexcp_i_imr_cat
{
  key Category,
      ClientGuid,
      CreatedBy,
      CreatedOn,
      ChangedBy,
      ChangedOn
}
