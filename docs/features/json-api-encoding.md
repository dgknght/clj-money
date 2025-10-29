# JSON API Encoding

- An API client can specify that they are submitting information in JSON
  format (instead of the default EDN format) by including an HTTP Content-Type
  header with the value "application/json".
- An API client can specify that they expect to receive information in JSON
  format by including an HTTP Accept header with the value "application/json".


## Implementation
- Each API namespace needs to support JSON requests and responses.
- Change one test file at a time to keep git commits small.
