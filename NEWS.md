# mermaidr 0.2.0



# mermaidr 0.1.1

* Fix bug related to stricter row binding behaviour from updated version of `vctrs`.
* Use trailing slash on endpoints to avoid redirects.
* Suppress warning caused by introduction of `HTTP_API_VERSION` header that is not properly handled by the `httr` package (https://github.com/r-lib/httr/issues/590).
* Add missing `count` column to fishbelt observations (queried via `mermaid_get_project_data(method = "fishbelt", data = "observations")`).

# mermaidr 0.1.0

* Initial release.
