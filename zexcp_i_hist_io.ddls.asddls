@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'History Check Group CDS Interface'
define root view entity zexcp_i_hist_io
  as select from zexcp_hist_io as HistCheckGrp
{
  key HistCheckGrp.check_grp   as CheckGrp,
      HistCheckGrp.client_guid as ClientGuid,
      HistCheckGrp.created_by  as CreatedBy,
      HistCheckGrp.created_on  as CreatedOn,
      HistCheckGrp.changed_by  as ChangedBy,
      HistCheckGrp.changed_on  as ChangedOn
}
