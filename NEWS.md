# cromwell 0.3.0

## Changes

- cromwellQuery now takes query as a list, passed along to the API call
    - allows paging (via pagesize and page)
    - allows filtering by status (Running, Succeeded, etc).
- cromwellQuery now deals gracefully with missing end times (when the tasks are not completed)
- changed all time columns in output of cromwellQuery to posixct with local timezone

## Additions

- wdltoolInputs can report back inputs to a WDL-based workflow specified as a file

