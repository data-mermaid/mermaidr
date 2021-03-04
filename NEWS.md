# mermaidr 0.3.0

* Updated `mermaid_get_project_data()` to automatically unpack any data frame columns. This affects the fishbelt, benthic PIT, and benthic LIT methods, for both sample units and sample events data. This is a breaking change, expected to affect existing code that uses the `biomass_kgha_by_trophic_group`, `biomass_kgha_by_fish_family`, and `percent_cover_by_benthic_category` columns in sample units, and their `*_avg` counterparts in sample events. Instead of these columns, results will now contain a column for subgroup - for example, instead of `biomass_kgha_by_trophic_group` there will be columns such as `biomass_kgha_trophic_group_piscivore` and `biomass_kgha_trophic_group_planktivore`.

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
