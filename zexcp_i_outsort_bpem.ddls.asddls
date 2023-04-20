@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Outsort - BPEM Category CDS Interface'
define root view entity zexcp_i_outsort_bpem
  as select from zexcp_outsortbpm as OutsortCategory
{
  key OutsortCategory.category    as Category,
  key OutsortCategory.ctype       as Ctype,
      OutsortCategory.client_guid as ClientGuid,
      OutsortCategory.created_by  as CreatedBy,
      OutsortCategory.created_on  as CreatedOn,
      OutsortCategory.changed_by  as ChangedBy,
      OutsortCategory.changed_on  as ChangedOn
}
