@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'BPEM Checks'
define root view entity zexcp_i_bpem_checks
  as select from zexcp_bpem_check as BPEMChecks
{
  key BPEMChecks.key_id        as KeyID,
      BPEMChecks.is_active     as IsActive,
      BPEMChecks.is_maintained as IsMaintained,
      BPEMChecks.description   as Description,
      BPEMChecks.created_by    as CreatedBy,
      BPEMChecks.created_on    as CreatedOn,
      BPEMChecks.changed_by    as ChangedBy,
      BPEMChecks.changed_on    as ChangedOn
}
