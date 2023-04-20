@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Implausible Meter Category CDS Interface'
define root view entity zexcp_i_imr_cat
  as select from zexcp_imr_cat as IMRCategory
{
  key IMRCategory.category    as Category,
      IMRCategory.client_guid as ClientGuid,
      IMRCategory.created_by  as CreatedBy,
      IMRCategory.created_on  as CreatedOn,
      IMRCategory.changed_by  as ChangedBy,
      IMRCategory.changed_on  as ChangedOn
}
