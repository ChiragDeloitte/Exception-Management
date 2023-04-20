@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Job Scheduler CDS Interface'
define root view entity zexcp_i_jobsched
  as select from zexcp_jobsched as JobSched
{
  key JobSched.client_id     as ClientId,
  key JobSched.job_id        as JobId,
      JobSched.job_desc      as JobDesc,
      JobSched.status        as Status,
      JobSched.sched_time    as SchedTime,
      JobSched.sched_frmdate as SchedFrmdate,
      JobSched.sched_todate  as SchedTodate,
      JobSched.frequency     as Frequency,
      JobSched.weekly_day    as WeeklyDay,
      JobSched.client_guid   as ClientGuid,
      JobSched.created_by    as CreatedBy,
      JobSched.created_on    as CreatedOn,
      JobSched.changed_by    as ChangedBy,
      JobSched.changed_on    as ChangedOn
}
