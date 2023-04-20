@EndUserText.label: 'Exceptions validation result'
define abstract entity zexcp_a_validate_result
{
  ReturnCode : abap.int4;
  Message    : abap.sstring(50);
}
