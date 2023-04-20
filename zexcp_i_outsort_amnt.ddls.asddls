@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Credit Invoice Outsort Amnt CDS Interface'
define root view entity zexcp_i_outsort_amnt
  as select from zexcp_outsortamt as OutsortAmount
{
  key OutsortAmount.ctype       as Ctype,
      OutsortAmount.from_amnt   as FromAmnt,
      OutsortAmount.to_amnt     as ToAmnt,
      OutsortAmount.client_guid as ClientGuid,
      OutsortAmount.created_by  as CreatedBy,
      OutsortAmount.created_on  as CreatedOn,
      OutsortAmount.changed_by  as ChangedBy,
      OutsortAmount.changed_on  as ChangedOn
}
