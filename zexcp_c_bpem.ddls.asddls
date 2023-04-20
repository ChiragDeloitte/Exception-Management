@EndUserText.label: 'BPEM Checks'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity zexcp_c_bpem
  provider contract transactional_query
  as projection on zexcp_i_bpem_checks
{
  key KeyID,
      IsActive,
      IsMaintained,
      Description,
      CreatedBy,
      CreatedOn,
      ChangedBy,
      ChangedOn
}
