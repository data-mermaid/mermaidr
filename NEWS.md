# mermaidr 0.3.0

* Updated `mermaid_get_project_data()` function to automatically unpack and rename any data frame columns. This affects the X, Y, and Z methods, for both "sampleunits" and "sampleevents" data, removing the columns X, Y, and Z, respectively, and instead replacing them with X, Y, and Z.

* Added `mermaid_clean_columns()` function (from archived `mermaidreporting` package) to easily unpack and clean up df-cols coming from MERMAID API.

# mermaidr 0.2.2

* Added Allen Coral Atlas to all aggregated endpoints (in `mermaid_get_project_data()`).
* Added `biomass_kgha_by_fish_family` and `biomass_kgha_by_fish_family_avg` to fishbelt sample units and sample events, respectively (in `mermaid_get_project_data()`).

# mermaidr 0.2.1

* Terminating `httr::RETRY()` after one failure if the status code indicates an unauthorized request; no need to retry in those cases.

# mermaidr 0.2.0

* Big addition of Benthic LIT, Bleaching, and Habitat Complexity methods in `mermaid_get_project_data()`, and additional fields available in Fish Belt and Benthic PIT endpoints.
* Removed `url` argument from most external functions, since switching between prod and dev is more complicated than just changing the `url` - especially for authenticated endpoint calls. For now, switching between prod and dev requires installing from the main and dev branches, respectively. I will continue to explore making the token generation more robust for accessing both prod and dev, at which point the `url` argument will likely return!
* Using `httr::RETRY()` instead of `httr::GET()` to make functions more resilient to e.g. temporary API outages or timeouts
* Documentation improvements.

# mermaidr 0.1.1

* Fix bug related to stricter row binding behaviour from updated version of `vctrs`.
* Use trailing slash on endpoints to avoid redirects.
* Suppress warning caused by introduction of `HTTP_API_VERSION` header that is not properly handled by the `httr` package (https://github.com/r-lib/httr/issues/590).
* Add missing `count` column to fishbelt observations (queried via `mermaid_get_project_data(method = "fishbelt", data = "observations")`).

# mermaidr 0.1.0

* Initial release.
