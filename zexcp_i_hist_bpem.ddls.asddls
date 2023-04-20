@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'History Category CDS Interface'
define root view entity zexcp_i_hist_bpem
  as select from zexcp_hist_bpem as HistCategory
{
  key HistCategory.category    as Category,
      HistCategory.client_guid as ClientGuid,
      HistCategory.created_by  as CreatedBy,
      HistCategory.created_on  as CreatedOn,
      HistCategory.changed_by  as ChangedBy,
      HistCategory.changed_on  as ChangedOn
}
