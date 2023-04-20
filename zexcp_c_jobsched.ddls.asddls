@EndUserText.label: 'Job Scheduler CDS Consumption'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity zexcp_c_jobsched
  provider contract transactional_query
  as projection on zexcp_i_jobsched
{
  key ClientId,
  key JobId,
      JobDesc,
      Status,
      SchedTime,
      SchedFrmdate,
      SchedTodate,
      Frequency,
      WeeklyDay,
      ClientGuid,
      CreatedBy,
      CreatedOn,
      ChangedBy,
      ChangedOn
}
