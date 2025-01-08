# mermaidr 1.2.0

* Add `mermaid_get_gfcr_report()` for exporting GFCR report for project(s)
* Add bulk validation of records in Collecting via `mermaid_import_bulk_validate()`
* Add `observers` to "sampleevents" data in `mermaid_get_project_data()` and in `mermaid_get_summary_sampleevents()`
* Enable switching production/development tokens automatically

# mermaidr 1.1.3

* Add relevant life histories to `"benthiclit"` and `"benthicpqt"` methods in `mermaid_get_project_data()`
    * `data = "observations"` gains `life_histories__competitive`, `life_histories__generalist`, `life_histories__stress_tolerant`, `life_histories__weedy`
    * `data = "sampleunits"` gains `percent_cover_life_histories_weedy`, `percent_cover_life_histories_generalist`, `percent_cover_life_histories_competitive`, `percent_cover_life_histories_stress_tolerant`
    * `data = "sampleevents"` gains `percent_cover_life_histories_avg_weedy`, `percent_cover_life_histories_avg_generalist`, `percent_cover_life_histories_avg_competitive`, `percent_cover_life_histories_avg_stress-tolerant`, `percent_cover_life_histories_sd_weedy`, `percent_cover_life_histories_sd_generalist`, `percent_cover_life_histories_sd_competitive`, `percent_cover_life_histories_sd_stress-tolerant`

# mermaidr 1.1.2

* Add fields `management_est_year`, `management_size`, `management_parties`, `management_compliance`, `management_rules` to `mermaid_get_summary_sampleevents()`
* Handle users without projects in `mermaid_get_me()`

# mermaidr 1.1.1

* Handle timeout error in `mermaid_import_project_data()` with suggestions to batch import data by site or date.
* Add `mermaid_get_classification_labelmappings()`

# mermaidr 1.1.0

* Add `life_histories` and `growth_form_life_histories` to `mermaid_get_reference("benthicattributes")`
* Add relevant life histories to `"benthicpit"` method in `mermaid_get_project_data()`
    * `data = "observations"` gains `life_histories__competitive`, `life_histories__generalist`, `life_histories__stress_tolerant`, `life_histories__weedy`
    * `data = "sampleunits"` gains `percent_cover_life_histories_weedy`, `percent_cover_life_histories_generalist`, `percent_cover_life_histories_competitive`, `percent_cover_life_histories_stress_tolerant`
    * `data = "sampleevents"` gains `percent_cover_life_histories_avg_weedy`, `percent_cover_life_histories_avg_generalist`, `percent_cover_life_histories_avg_competitive`, `percent_cover_life_histories_avg_stress-tolerant`, `percent_cover_life_histories_sd_weedy`, `percent_cover_life_histories_sd_generalist`, `percent_cover_life_histories_sd_competitive`, `percent_cover_life_histories_sd_stress-tolerant`

# mermaidr 1.0.4

* Add `project_id`, `site_id`, and `management_id` to `mermaid_get_summary_sampleevents()`

# mermaidr 1.0.3

* Fix bug with refreshing token in authentication flow, introduced in 1.0.2

# mermaidr 1.0.2

* Add `projects` column to `mermaid_get_me()`
* Updates to authentication flow to allow `mermaidr` to work in Shiny

# mermaidr 1.0.1

* Added `mermaid_get_me()` endpoint to get profile information for the user.

# mermaidr 1.0.0

* `mermaid_get_project_data()` now uses CSV endpoints, which significantly speeds up getting data from projects with a large number of observations
* This endpoint brings a few changes - empty values for text fields such as `label`, `project_notes`, `site_notes`, etc, are now `NA` instead of `""`
* Fields with multiple values (e.g. `tags`, `management_parties`, etc) are now separated using a comma (`,') instead of a semi-colon (`;`)

* Standard deviations have been added for all columns that have averages in  `mermaid_get_project_data()` - e.g. fishbelt sample events now has `biomass_kgha_trophic_group_sd` in addition to `biomass_kgha_trophic_group_avg`.

* Benthic. PQT sample units and sample events data gain the column `num_points_nonother`, which counts the number of non-other points
* Fishbelt observations gains the column `assigned_transect_width_m`
* All data retrieved via `mermaid_get_project_data()` now contains the `project_admins` column

* Sample units data from `mermaid_get_project_data()` no longer has the random `id` column

# mermaidr 0.7.0

* Covariates are no longer included in `mermaid_get_project_data()` functions by default, require settings `covariates = TRUE`
* `mermaid_get_project_sites()` also gains `covariate` argument, which when set to `TRUE` returns covariates at the site level

# mermaidr 0.6.4

* Add ingestion docs

# mermaidr 0.6.3

* Allow export of "benthicpqt" data via `mermaid_get_project_data()`

# mermaidr 0.6.2

* Allow import of "benthicpqt" data via `mermaid_import_project_data()`
* Enable `mermaid_import_project_data()` to take `project`, not `project_id`, consistent with other functions

# mermaidr 0.6.1

* Remove fuzzyjoin/stringdist dependency, calculate differences for strings in `mermaid_import_check_options()` more manually.

# mermaidr 0.6.0

* Remove `mermaid_get_summary_sites()` as endpoint has been removed.
* Ensure all `NA`s are written to `''` to appear as blanks, not literal `"NA"` in mermaid_import_project_data().

# mermaidr 0.5.1

* Fix bug with retrieving content type from headers.

# mermaidr 0.5.0

* Add `mermaid_get_summary_sampleevents()` for getting aggregated metrics for all MERMAID surveys, by site, by date, and `mermaid_get_summary_sites()` for getting aggregated metrics for all MERMAID surveys, by site, for *all* dates.
* Add `mermaid_import_get_template_and_options()` to get a template and field options for importing any method into MERMAID.
* Add `mermaid_import_check_options()` for verifying that data being prepared for import matches the field options allowed.
* Remove `mermaid_import_field_options()`, replaced by the above two functions (which are more feature rich and complete).

# mermaidr 0.4.6

* Handle presence of all covariates, convert `NULL` covariates to `NA`.

# mermaidr 0.4.5

* Fix bug where `mermaid_get_project_data()` functions failed if `andrello` or `beyer` covariates were not present, and data for multiple projects is being selected (0.4.4 only fixed the single-project case).

# mermaidr 0.4.4.

* Fix bug where `mermaid_get_project_data()` functions failed if `andrello` or `beyer` covariates were not present.

# mermaidr 0.4.3

* Added all Vibrant Oceans covariates to aggregated endpoints (in `mermaid_get_project_data()`).
* Handle _any_ missing (`NA`) values in `mermaid_import_project_data()` by automatically converting to an empty value, which is processed by the API as a NULL, before importing.

# mermaidr 0.4.2

* Handle missing `Sample time` values in `mermaid_import_project_data()` by automatically converting `NA` to `""` before importing.

# mermaidr 0.4.1

* `mermaid_get_sites()` and `mermaid_get_managements()` now require authorization.
* Added vignette on accessing development data.

# mermaidr 0.4.0

* Added ability to import data into MERMAID via `mermaid_import_project_data()`.
* Added `mermaid_import_field_options()` to check valid options for fields in import.

# mermaidr 0.3.2

* Removed ability to query "beltfishes", "benthicpits", and "habitatcomplexities" in `mermaid_get_project_endpoint()`, since the endpoints were removed from the underlying API.
* Bug fixes.

# mermaidr 0.3.1

* Updated `mermaid_get_reference()` to include regions.

# mermaidr 0.3.0

* Updated `mermaid_get_project_data()` to automatically unpack any data frame columns. This affects the fishbelt, benthic PIT, and benthic LIT methods, for both sample units and sample events data. This is a breaking change, expected to affect existing code that uses the `biomass_kgha_by_trophic_group`, `biomass_kgha_by_fish_family`, and `percent_cover_by_benthic_category` columns in sample units, and their `*_avg` counterparts in sample events. Instead of these columns, results will now contain a column for subgroup - for example, instead of `biomass_kgha_by_trophic_group` there will be columns such as `biomass_kgha_trophic_group_piscivore` and `biomass_kgha_trophic_group_planktivore`.
* Updated `mermaid_get_reference()` to provide enhanced reference data, returning actual values for e.g. fish family, sizes, groups, etc, instead of their internal IDs. The `display` column for the "fishspecies" reference has been renamed to `species` ([#21](https://github.com/data-mermaid/mermaidr/issues/21)).

# mermaidr 0.2.4

* Fixed bug introduced by handling `NULL` `covariates`.

# mermaidr 0.2.3

* Fixed bug with handling of covariates (now properly handles case where `covariates` are `NULL`).

# mermaidr 0.2.2

* Added Allen Coral Atlas covariates to all aggregated endpoints (in `mermaid_get_project_data()`).
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
