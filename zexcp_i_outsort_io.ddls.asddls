@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Credit Invoice Outsort - Check Group CDS Interface'
define root view entity zexcp_i_outsort_io
  as select from zexcp_outsort_io as OutsortCheckGrp
{
  key OutsortCheckGrp.check_grp   as CheckGrp,
  key OutsortCheckGrp.ctype       as Ctype,
      OutsortCheckGrp.client_guid as ClientGuid,
      OutsortCheckGrp.created_by  as CreatedBy,
      OutsortCheckGrp.created_on  as CreatedOn,
      OutsortCheckGrp.changed_by  as ChangedBy,
      OutsortCheckGrp.changed_on  as ChangedOn
}
