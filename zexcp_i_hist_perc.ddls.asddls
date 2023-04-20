@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'History Percentage CDS Interface'
define root view entity zexcp_i_hist_perc
  as select from zexcp_hist_perc as HistPerc
{
  key HistPerc.client_id   as ClientId,
      HistPerc.from_prcnt  as FromPrcnt,
      HistPerc.to_prcnt    as ToPrcnt,
      HistPerc.client_guid as ClientGuid,
      HistPerc.created_by  as CreatedBy,
      HistPerc.created_on  as CreatedOn,
      HistPerc.changed_by  as ChangedBy,
      HistPerc.changed_on  as ChangedOn
}
