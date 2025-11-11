# JSON API Encoding

- An API client can specify that they are submitting information in JSON
  format (instead of the default EDN format) by including an HTTP Content-Type
  header with the value "application/json".
- An API client can specify that they expect to receive information in JSON
  format by including an HTTP Accept header with the value "application/json".


## Implementation
- Each API namespace needs to support JSON requests and responses.
- Change one test file at a time to keep git commits small.
- Do not use the format-specific helper methods like
  `ring.mock.request/json-body`. Instead use `clj-money.api.test-helper/request`.
- Do not use format-specified parse functions like `parse-json-body`. Instead use
  `clj-money.api.test-helper.parse-body`.
- Tests should exercise both the default format (edn) and the JSON format.
- `with-context` blocks should be nested directly within `deftest` functions, not
  inside helper functions, as this impairs reusability.
- The majority of the changes should be in the test namespaces, but some changes
  may be necessary in the `clj-money.api` namespaces. Generally, this will be
  because the JSON format cannot communicate the same types as EDN.
