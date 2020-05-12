
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mermaidr

<!-- badges: start -->

[![CircleCI build
status](https://circleci.com/gh/data-mermaid/mermaidr.svg?style=svg)](https://circleci.com/gh/data-mermaid/mermaidr)
<!-- badges: end -->

The goal of `mermaidr` is to access [MERMAID
Collect](https://collect.datamermaid.org/) data directly from R.

## Installation

You can install mermaidr from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("data-mermaid/mermaidr@package", upgrade = "never")
```

Next, load the package:

``` r
library(mermaidr)
```

## Authentication

`mermaidr` will help you interact with MERMAID Collect as an
authenticated user, as soon as you need. This is only required for
accessing project specific data. To access a list of all projects,
sites, etc, you do not need to be authenticated.

If you would like to authenticate yourself immediately, use
`mermaid_auth()`. This will open your browser to the MERMAID Collect
login. Once you log in, you can go back to R and will be authenticated.

The login credentials expire every 24 hours. Once your credentials are
expired, `mermaidr` will again help you automatically authenticate when
needed.

## Usage

All functions in `mermaidr` are of the form `mermaid_*()`, to make
functions easier to find and use when loaded with other packages\!

## Accessing project data

To access data related to your MERMAID projects, first obtain a list of
your projects with `mermaid_get_my_projects()`.

At this point, you will have to authenticate to the Collect app. R will
help you do this automatically by opening a browser window for you to
log in to Collect, either via Google sign-in or username and password -
however you normally do\!

Once you’ve logged in, come back to R. Your login credentials will be
stored for a day, until they expire, and you will need to login again.
The package handles the expiration for you, so just log in again when
prompted.

``` r
library(mermaidr)
my_projects <- mermaid_get_my_projects()

my_projects
#> # A tibble: 4 x 14
#>   id    name  countries num_sites tags  notes status data_policy_bel…
#>   <chr> <chr> <chr>         <int> <chr> <chr> <chr>  <chr>           
#> 1 3a9e… Aceh… Indonesia        18 "WCS… ""    Open   Private         
#> 2 5679… Mada… Madagasc…        33 "WCS… ""    Open   Public Summary  
#> 3 9de8… XPDC… Indonesia        37 ""    "XPD… Open   Public Summary  
#> 4 a3d1… Nort… Indonesia        44 "WCS… ""    Open   Private         
#> # … with 6 more variables: data_policy_benthiclit <chr>,
#> #   data_policy_benthicpit <chr>, data_policy_habitatcomplexity <chr>,
#> #   data_policy_bleachingqc <chr>, created_on <chr>, updated_on <chr>
```

This function returns information on your projects, including project
countries, the number of sites, tags, data policies, and more.

To filter for specific projects, you can use the `filter` function from
`dplyr`:

``` r
library(dplyr)

indonesia_projects <- my_projects %>%
  filter(countries == "Indonesia")

indonesia_projects
#> # A tibble: 3 x 14
#>   id    name  countries num_sites tags  notes status data_policy_bel…
#>   <chr> <chr> <chr>         <int> <chr> <chr> <chr>  <chr>           
#> 1 3a9e… Aceh… Indonesia        18 "WCS… ""    Open   Private         
#> 2 9de8… XPDC… Indonesia        37 ""    "XPD… Open   Public Summary  
#> 3 a3d1… Nort… Indonesia        44 "WCS… ""    Open   Private         
#> # … with 6 more variables: data_policy_benthiclit <chr>,
#> #   data_policy_benthicpit <chr>, data_policy_habitatcomplexity <chr>,
#> #   data_policy_bleachingqc <chr>, created_on <chr>, updated_on <chr>
```

Alternatively, you can search your projects using
`mermaid_search_my_projects()`, narrowing projects down by name,
country, or tag:

``` r
mermaid_search_my_projects(countries = "Indonesia")
#> # A tibble: 3 x 14
#>   id    name  countries num_sites tags  notes status data_policy_bel…
#>   <chr> <chr> <chr>         <int> <chr> <chr> <chr>  <chr>           
#> 1 3a9e… Aceh… Indonesia        18 "WCS… ""    Open   Private         
#> 2 9de8… XPDC… Indonesia        37 ""    "XPD… Open   Public Summary  
#> 3 a3d1… Nort… Indonesia        44 "WCS… ""    Open   Private         
#> # … with 6 more variables: data_policy_benthiclit <chr>,
#> #   data_policy_benthicpit <chr>, data_policy_habitatcomplexity <chr>,
#> #   data_policy_bleachingqc <chr>, created_on <chr>, updated_on <chr>
```

Then, you can start to access data about your projects, like project
sites via `mermaid_get_project_sites()`:

``` r
indonesia_projects %>%
  mermaid_get_project_sites()
#> # A tibble: 99 x 13
#>    project id    name  notes latitude longitude country reef_type reef_zone
#>    <chr>   <chr> <chr> <chr>    <dbl>     <dbl> <chr>   <chr>     <chr>    
#>  1 Aceh J… 026e… Pula… ""        4.78      95.4 Indone… fringing  fore reef
#>  2 Aceh J… 1b8a… Inti… ""        5.11      95.3 Indone… fringing  fore reef
#>  3 Aceh J… 1ce4… Inti… ""        4.52      95.7 Indone… fringing  fore reef
#>  4 Aceh J… 29a2… Ujun… ""        5.12      95.3 Indone… fringing  fore reef
#>  5 Aceh J… 38f7… Pula… ""        5.08      95.3 Indone… fringing  back reef
#>  6 Aceh J… 5436… Wisa… ""        5.04      95.4 Indone… fringing  fore reef
#>  7 Aceh J… 7069… Lhok… ""        4.63      95.6 Indone… fringing  fore reef
#>  8 Aceh J… 7519… Pula… ""        5.13      95.3 Indone… fringing  fore reef
#>  9 Aceh J… 81a0… Inti… ""        4.65      95.6 Indone… fringing  fore reef
#> 10 Aceh J… 9283… Abah… ""        4.99      95.4 Indone… fringing  fore reef
#> # … with 89 more rows, and 4 more variables: exposure <chr>, predecessor <lgl>,
#> #   created_on <chr>, updated_on <chr>
```

Or the managements for your projects via
`mermaid_get_project_managements()`:

``` r
indonesia_projects %>%
  mermaid_get_project_managements()
#> # A tibble: 21 x 17
#>    project id    name  name_secondary notes est_year no_take periodic_closure
#>    <chr>   <chr> <chr> <chr>          <chr>    <int> <lgl>   <lgl>           
#>  1 Aceh J… 0f0f… Open  ""             ""        2019 FALSE   FALSE           
#>  2 Aceh J… 1498… Tour… ""             ""        2019 TRUE    FALSE           
#>  3 Aceh J… 646c… Fish… ""             ""        2019 FALSE   FALSE           
#>  4 Aceh J… a579… Aqua… ""             ""        2019 FALSE   FALSE           
#>  5 Aceh J… a803… Open… ""             ""        2019 FALSE   FALSE           
#>  6 Aceh J… cc92… Core… ""             ""        2019 TRUE    FALSE           
#>  7 Aceh J… dce8… Reha… ""             ""        2019 TRUE    FALSE           
#>  8 XPDC K… 04fc… Outs… "Control"      ""          NA FALSE   FALSE           
#>  9 XPDC K… 592e… Limi… "Use Zone"     ""          NA FALSE   FALSE           
#> 10 XPDC K… 9ad0… Tour… "No Take Zone" ""          NA TRUE    FALSE           
#> # … with 11 more rows, and 9 more variables: open_access <lgl>,
#> #   size_limits <lgl>, gear_restriction <lgl>, species_restriction <lgl>,
#> #   compliance <chr>, predecessor <lgl>, parties <chr>, created_on <chr>,
#> #   updated_on <chr>
```

### Method data

You can also currently access data on your projects’ fish belt and
benthic PIT methods, with more methods to come. The details are in the
following sections.

#### Fish Belt data

To access fish belt data for a project, use `mermaid_get_project_data()`
with `method = "fishbelt"`.

You can access individual observations (i.e., a record of each
observation) by setting `data = "observations`":

``` r
xpdc <- my_projects %>%
  filter(name == "XPDC Kei Kecil 2018")

xpdc %>%
  mermaid_get_project_data(method = "fishbelt", data = "observations")
#> # A tibble: 3,069 x 45
#>    project tags  country site  latitude longitude reef_type reef_zone
#>    <chr>   <lgl> <chr>   <chr>    <dbl>     <dbl> <chr>     <chr>    
#>  1 XPDC K… NA    Indone… KE20     -5.67      133. fringing  fore reef
#>  2 XPDC K… NA    Indone… KE31     -5.78      133. fringing  crest    
#>  3 XPDC K… NA    Indone… KE11     -5.59      133. fringing  crest    
#>  4 XPDC K… NA    Indone… KE03     -5.61      132. fringing  crest    
#>  5 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#>  6 XPDC K… NA    Indone… KE10     -5.57      133. fringing  crest    
#>  7 XPDC K… NA    Indone… KE23     -5.80      133. fringing  fore reef
#>  8 XPDC K… NA    Indone… KE15     -5.62      133. fringing  crest    
#>  9 XPDC K… NA    Indone… KE28     -5.75      133. patch     fore reef
#> 10 XPDC K… NA    Indone… KE11     -5.59      133. fringing  crest    
#> # … with 3,059 more rows, and 37 more variables: reef_exposure <chr>,
#> #   reef_slope <chr>, tide <chr>, current <chr>, visibility <chr>,
#> #   management <chr>, management_secondary <chr>, management_est_year <lgl>,
#> #   management_size <lgl>, management_parties <lgl>,
#> #   management_compliance <chr>, management_rules <chr>, sample_date <date>,
#> #   sample_time <chr>, transect_length <int>, transect_width <chr>,
#> #   size_bin <int>, observers <chr>, depth <dbl>, transect_number <int>,
#> #   label <chr>, fish_family <chr>, fish_genus <chr>, fish_taxon <chr>,
#> #   size <dbl>, biomass_constant_a <dbl>, biomass_constant_b <dbl>,
#> #   biomass_constant_c <dbl>, biomass_kgha <dbl>, trophic_level <dbl>,
#> #   functional_group <chr>, vulnerability <dbl>, data_policy_beltfish <chr>,
#> #   project_notes <chr>, site_notes <chr>, management_notes <chr>,
#> #   contact_link <chr>
```

You can access sample units data, which are observations aggregated to
the sample units level. Fish belt sample units contain total biomass in
kg/ha per sample unit, by trophic group:

``` r
xpdc %>%
  mermaid_get_project_data("fishbelt", "sampleunits")
#> # A tibble: 335 x 34
#>    project tags  country site  latitude longitude reef_type reef_zone
#>    <chr>   <lgl> <chr>   <chr>    <dbl>     <dbl> <chr>     <chr>    
#>  1 XPDC K… NA    Indone… KE15     -5.62      133. fringing  crest    
#>  2 XPDC K… NA    Indone… KE39     -5.99      132. fringing  fore reef
#>  3 XPDC K… NA    Indone… KE33     -5.82      133. fringing  fore reef
#>  4 XPDC K… NA    Indone… KE37     -5.90      133. fringing  fore reef
#>  5 XPDC K… NA    Indone… KE26     -5.70      133. fringing  crest    
#>  6 XPDC K… NA    Indone… KE13     -5.51      133. patch     crest    
#>  7 XPDC K… NA    Indone… KE20     -5.67      133. fringing  fore reef
#>  8 XPDC K… NA    Indone… KE26     -5.70      133. fringing  crest    
#>  9 XPDC K… NA    Indone… KE03     -5.61      132. fringing  crest    
#> 10 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#> # … with 325 more rows, and 33 more variables: reef_exposure <chr>,
#> #   reef_slope <chr>, tide <chr>, current <chr>, visibility <chr>,
#> #   management <chr>, management_secondary <chr>, management_est_year <lgl>,
#> #   management_size <lgl>, management_parties <lgl>,
#> #   management_compliance <chr>, management_rules <chr>, sample_date <date>,
#> #   depth <dbl>, transect_number <int>, size_bin <int>, transect_length <int>,
#> #   transect_width <chr>, biomass_kgha <dbl>,
#> #   biomass_kgha_by_trophic_group$piscivore <dbl>, $planktivore <dbl>,
#> #   $`invertivore-mobile` <dbl>, $`herbivore-detritivore` <dbl>,
#> #   $omnivore <dbl>, $`invertivore-sessile` <dbl>,
#> #   $`herbivore-macroalgae` <dbl>, $other <dbl>, data_policy_beltfish <chr>,
#> #   project_notes <chr>, site_notes <chr>, management_notes <chr>, id <chr>,
#> #   contact_link <chr>
```

And finally, sample events data, which are aggregated further, to the
sample event level. Fish belt sample events contain *mean* total biomass
in kg/ha per sample event and by trophic group:

``` r
xpdc %>%
  mermaid_get_project_data("fishbelt", "sampleevents")
#> # A tibble: 46 x 29
#>    project tags  country site  latitude longitude reef_type reef_zone
#>    <chr>   <lgl> <chr>   <chr>    <dbl>     <dbl> <chr>     <chr>    
#>  1 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#>  2 XPDC K… NA    Indone… KE36     -5.88      133. fringing  fore reef
#>  3 XPDC K… NA    Indone… KE36     -5.88      133. fringing  fore reef
#>  4 XPDC K… NA    Indone… KE06     -5.52      132. fringing  crest    
#>  5 XPDC K… NA    Indone… KE23     -5.80      133. fringing  fore reef
#>  6 XPDC K… NA    Indone… KE18     -5.70      133. fringing  fore reef
#>  7 XPDC K… NA    Indone… KE13     -5.51      133. patch     crest    
#>  8 XPDC K… NA    Indone… KE13     -5.51      133. patch     crest    
#>  9 XPDC K… NA    Indone… KE19     -5.73      133. fringing  fore reef
#> 10 XPDC K… NA    Indone… KE31     -5.78      133. fringing  crest    
#> # … with 36 more rows, and 28 more variables: reef_exposure <chr>, tide <chr>,
#> #   current <chr>, visibility <chr>, management <chr>,
#> #   management_secondary <chr>, management_est_year <lgl>,
#> #   management_size <lgl>, management_parties <lgl>,
#> #   management_compliance <chr>, management_rules <chr>, sample_date <date>,
#> #   depth_avg <dbl>, biomass_kgha_avg <dbl>,
#> #   biomass_kgha_by_trophic_group_avg$omnivore <dbl>, $piscivore <dbl>,
#> #   $planktivore <dbl>, $`invertivore-mobile` <dbl>,
#> #   $`herbivore-detritivore` <dbl>, $`invertivore-sessile` <dbl>,
#> #   $`herbivore-macroalgae` <dbl>, $other <dbl>, data_policy_beltfish <chr>,
#> #   project_notes <chr>, site_notes <chr>, management_notes <chr>,
#> #   sample_unit_count <int>, contact_link <chr>
```

#### Benthic PIT data

Accessing Benthic PIT data works the exact same way as Fish Belt data,
except you change `method` to “benthicpit”:

``` r
xpdc %>%
  mermaid_get_project_data(method = "benthicpit", data = "observations")
#> # A tibble: 11,100 x 39
#>    project tags  country site  latitude longitude reef_type reef_zone
#>    <chr>   <lgl> <chr>   <chr>    <dbl>     <dbl> <chr>     <chr>    
#>  1 XPDC K… NA    Indone… KE36     -5.88      133. fringing  fore reef
#>  2 XPDC K… NA    Indone… KE27     -5.73      133. fringing  fore reef
#>  3 XPDC K… NA    Indone… KE07     -5.57      133. fringing  crest    
#>  4 XPDC K… NA    Indone… KE09     -5.60      133. fringing  fore reef
#>  5 XPDC K… NA    Indone… KE10     -5.57      133. fringing  crest    
#>  6 XPDC K… NA    Indone… KE07     -5.57      133. fringing  crest    
#>  7 XPDC K… NA    Indone… KE03     -5.61      132. fringing  crest    
#>  8 XPDC K… NA    Indone… KE37     -5.90      133. fringing  fore reef
#>  9 XPDC K… NA    Indone… KE13     -5.51      133. patch     crest    
#> 10 XPDC K… NA    Indone… KE14     -5.51      133. patch     crest    
#> # … with 11,090 more rows, and 31 more variables: reef_exposure <chr>,
#> #   reef_slope <chr>, tide <chr>, current <chr>, visibility <chr>,
#> #   management <chr>, management_secondary <chr>, management_est_year <lgl>,
#> #   management_size <lgl>, management_parties <lgl>,
#> #   management_compliance <chr>, management_rules <chr>, sample_date <date>,
#> #   sample_time <chr>, depth <dbl>, transect_number <int>,
#> #   transect_length <int>, interval_start <dbl>, interval_size <dbl>,
#> #   label <chr>, observers <chr>, interval <dbl>, benthic_category <chr>,
#> #   benthic_attribute <chr>, growth_form <chr>, data_policy_benthicpit <chr>,
#> #   project_notes <chr>, site_notes <chr>, management_notes <chr>,
#> #   observation_notes <chr>, contact_link <chr>
```

You can access sample units and sample events the same way.

For Benthic PIT, sample units contain percent cover per sample unit, by
benthic category. Sample *events* contain *mean* percent cover per
sample event, by benthic category.

You can return both of these by setting the `data` argument. This will
return a list of two data frames: one containing sample units, and the
other sample events.

``` r
xpdc_sample_units_events <- xpdc %>%
  mermaid_get_project_data(method = "benthicpit", data = c("sampleunits", "sampleevents"))

names(xpdc_sample_units_events)
#> [1] "sampleunits"  "sampleevents"
xpdc_sample_units_events[["sampleunits"]]
#> # A tibble: 111 x 34
#>    project tags  country site  latitude longitude reef_type reef_zone
#>    <chr>   <lgl> <chr>   <chr>    <dbl>     <dbl> <chr>     <chr>    
#>  1 XPDC K… NA    Indone… KE36     -5.88      133. fringing  fore reef
#>  2 XPDC K… NA    Indone… KE19     -5.73      133. fringing  fore reef
#>  3 XPDC K… NA    Indone… KE22     -5.85      133. fringing  fore reef
#>  4 XPDC K… NA    Indone… KE28     -5.75      133. patch     fore reef
#>  5 XPDC K… NA    Indone… KE13     -5.51      133. patch     crest    
#>  6 XPDC K… NA    Indone… KE23     -5.80      133. fringing  fore reef
#>  7 XPDC K… NA    Indone… KE39     -5.99      132. fringing  fore reef
#>  8 XPDC K… NA    Indone… KE24     -5.93      133. fringing  fore reef
#>  9 XPDC K… NA    Indone… KE09     -5.60      133. fringing  fore reef
#> 10 XPDC K… NA    Indone… KE10     -5.57      133. fringing  crest    
#> # … with 101 more rows, and 33 more variables: reef_exposure <chr>,
#> #   reef_slope <chr>, tide <chr>, current <chr>, visibility <chr>,
#> #   management <chr>, management_secondary <chr>, management_est_year <lgl>,
#> #   management_size <lgl>, management_parties <lgl>,
#> #   management_compliance <chr>, management_rules <chr>, sample_date <date>,
#> #   depth <dbl>, transect_number <int>, transect_length <int>,
#> #   interval_start <dbl>, interval_size <dbl>, observers <chr>,
#> #   percent_cover_by_benthic_category$Sand <dbl>, $Rubble <dbl>, $`Hard
#> #   coral` <dbl>, $Macroalgae <dbl>, $`Soft coral` <dbl>, $`Turf algae` <dbl>,
#> #   $`Bare substrate` <dbl>, $`Other invertebrates` <dbl>,
#> #   data_policy_benthicpit <chr>, project_notes <chr>, site_notes <chr>,
#> #   management_notes <chr>, id <chr>, contact_link <chr>
```

#### Multiple methods data

To pull data for both fish belt and benthic PIT methods, you can set
`method` to include both.

``` r
xpdc_sample_events <- xpdc %>%
  mermaid_get_project_data(method = c("fishbelt", "benthicpit"), data = "sampleevents")
```

The result is a list of data frames, containing sample events for both
fish belt and benthic PIT methods:

``` r
names(xpdc_sample_events)
#> [1] "fishbelt"   "benthicpit"

xpdc_sample_events[["benthicpit"]]
#> # A tibble: 38 x 28
#>    project tags  country site  latitude longitude reef_type reef_zone
#>    <chr>   <lgl> <chr>   <chr>    <dbl>     <dbl> <chr>     <chr>    
#>  1 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#>  2 XPDC K… NA    Indone… KE36     -5.88      133. fringing  fore reef
#>  3 XPDC K… NA    Indone… KE06     -5.52      132. fringing  crest    
#>  4 XPDC K… NA    Indone… KE23     -5.80      133. fringing  fore reef
#>  5 XPDC K… NA    Indone… KE18     -5.70      133. fringing  fore reef
#>  6 XPDC K… NA    Indone… KE13     -5.51      133. patch     crest    
#>  7 XPDC K… NA    Indone… KE19     -5.73      133. fringing  fore reef
#>  8 XPDC K… NA    Indone… KE31     -5.78      133. fringing  crest    
#>  9 XPDC K… NA    Indone… KE20     -5.67      133. fringing  fore reef
#> 10 XPDC K… NA    Indone… KE40     -6.00      132. fringing  fore reef
#> # … with 28 more rows, and 27 more variables: reef_exposure <chr>, tide <chr>,
#> #   current <chr>, visibility <chr>, management <chr>,
#> #   management_secondary <chr>, management_est_year <lgl>,
#> #   management_size <lgl>, management_parties <lgl>,
#> #   management_compliance <chr>, management_rules <chr>, sample_date <date>,
#> #   depth_avg <dbl>, percent_cover_by_benthic_category_avg$Sand <dbl>,
#> #   $Rubble <dbl>, $`Hard coral` <dbl>, $Macroalgae <dbl>, $`Soft coral` <dbl>,
#> #   $`Turf algae` <dbl>, $`Other invertebrates` <dbl>, $`Bare substrate` <dbl>,
#> #   data_policy_benthicpit <chr>, project_notes <chr>, site_notes <chr>,
#> #   management_notes <chr>, sample_unit_count <int>, contact_link <chr>
```

Alternatively, you can set `method` to “all” to pull for all methods\!
This will be handy once there are more than two methods. Similarly, you
can set `data` to “all” to pull all types of data:

``` r
all_project_data <- xpdc %>%
  mermaid_get_project_data(method = "all", data = "all", limit = 1)

names(all_project_data)
#> [1] "fishbelt"   "benthicpit"

names(all_project_data[["benthicpit"]])
#> [1] "observations" "sampleunits"  "sampleevents"
```

#### Multiple projects

Pulling data for multiple projects is the exact same, except there will
be an additional “project” column at the beginning to distinguish which
projects the data comes from. Recall that \`my\_projects contains four
projects:

``` r
my_projects
#> # A tibble: 4 x 14
#>   id    name  countries num_sites tags  notes status data_policy_bel…
#>   <chr> <chr> <chr>         <int> <chr> <chr> <chr>  <chr>           
#> 1 3a9e… Aceh… Indonesia        18 "WCS… ""    Open   Private         
#> 2 5679… Mada… Madagasc…        33 "WCS… ""    Open   Public Summary  
#> 3 9de8… XPDC… Indonesia        37 ""    "XPD… Open   Public Summary  
#> 4 a3d1… Nort… Indonesia        44 "WCS… ""    Open   Private         
#> # … with 6 more variables: data_policy_benthiclit <chr>,
#> #   data_policy_benthicpit <chr>, data_policy_habitatcomplexity <chr>,
#> #   data_policy_bleachingqc <chr>, created_on <chr>, updated_on <chr>
```

``` r
my_projects %>%
  mermaid_get_project_data("fishbelt", "sampleevents", limit = 1)
#> # A tibble: 4 x 29
#>   project tags  country site  latitude longitude reef_type reef_zone
#>   <chr>   <chr> <chr>   <chr>    <dbl>     <dbl> <chr>     <chr>    
#> 1 Aceh J… WCS … Indone… Pula…     4.78      95.4 fringing  fore reef
#> 2 Madaga… WCS … Madaga… Anta…   -16.4       49.8 fringing  fore reef
#> 3 XPDC K… <NA>  Indone… KE02     -5.44     133.  fringing  crest    
#> 4 North … WCS … Indone… Sali…     3.09     125.  fringing  fore reef
#> # … with 28 more variables: reef_exposure <chr>, tide <chr>, current <chr>,
#> #   visibility <chr>, management <chr>, management_secondary <chr>,
#> #   management_est_year <int>, management_size <lgl>, management_parties <chr>,
#> #   management_compliance <chr>, management_rules <chr>, sample_date <date>,
#> #   depth_avg <dbl>, biomass_kgha_avg <dbl>,
#> #   biomass_kgha_by_trophic_group_avg$other <dbl>, $omnivore <dbl>,
#> #   $piscivore <dbl>, $planktivore <dbl>, $`invertivore-mobile` <dbl>,
#> #   $`invertivore-sessile` <dbl>, $`herbivore-detritivore` <dbl>,
#> #   $`herbivore-macroalgae` <dbl>, data_policy_beltfish <chr>,
#> #   project_notes <chr>, site_notes <chr>, management_notes <chr>,
#> #   sample_unit_count <int>, contact_link <chr>
```

Note the `limit` argument here, which just limits the data pulled to one
record (per project, method, and data combination). This is useful if
you want to get a preview of what your data will look like without
having to pull it all in.

### Accessing non-project data

You may also want to access data that is not related to projects. To
access this data, you do not need to authenticate R with MERMAID.

For example, you can also pull the reference data (the names and
information of the fish and benthic attributes you can choose in
MERMAID), using `mermaid_get_reference()`:

``` r
mermaid_get_reference(reference = "fishfamilies")
#> # A tibble: 162 x 8
#>    id    name  status biomass_constan… biomass_constan… biomass_constan…
#>    <chr> <chr> <chr>             <dbl>            <dbl>            <dbl>
#>  1 0091… Kyph… Open            0.0193              3.03            0.986
#>  2 00b6… Mugi… Open            0.0166              2.94            0.974
#>  3 00f4… Zena… Open            0.00427             3.02            1    
#>  4 0226… Sphy… Open            0.00448             3.11            1    
#>  5 0880… Labr… Open            0.0120              3.04            0.997
#>  6 0aff… Scom… Open            0.0111              3.03            0.988
#>  7 0b69… Ophi… Open            0.00139             2.93            1    
#>  8 0d99… Albu… Open            0.0105              2.99            1    
#>  9 0e5b… Hemi… Open            0.0373              3.16            0.99 
#> 10 1513… Serr… Open            0.0136              3.03            0.997
#> # … with 152 more rows, and 2 more variables: created_on <chr>,
#> #   updated_on <chr>
```

Using this function, you can access the fish family, fish genera, fish
species, and benthic attributes references by changing the `reference`
argument.

You can also get a list of *all* projects (not just your own):

``` r
mermaid_get_projects()
#> # A tibble: 93 x 14
#>    id    name  countries num_sites tags  notes status data_policy_bel…
#>    <chr> <chr> <chr>         <int> <chr> <chr> <chr>  <chr>           
#>  1 01bb… Mada… "Madagas…        12 "WCS… "Sur… Open   Private         
#>  2 07df… Cend… "Indones…        36 "TNC… ""    Open   Public Summary  
#>  3 0c00… 2019… "Fiji"           18 "WCS… ""    Open   Private         
#>  4 0c16… REEF… ""                0 ""    ""    Open   Public Summary  
#>  5 0f17… what  ""                0 ""    ""    Open   Public Summary  
#>  6 124b… Sam   ""                0 ""    ""    Open   Private         
#>  7 170e… 2018… "Fiji"           10 "WCS… "Thi… Open   Private         
#>  8 1a49… Comm… ""                0 "WCS… "Thi… Open   Private         
#>  9 1c0b… Base… "Indones…        11 "WCS… "Bas… Open   Public Summary  
#> 10 1ed2… Opwa… ""                0 "Ope… "Ope… Open   Public Summary  
#> # … with 83 more rows, and 6 more variables: data_policy_benthiclit <chr>,
#> #   data_policy_benthicpit <chr>, data_policy_habitatcomplexity <chr>,
#> #   data_policy_bleachingqc <chr>, created_on <chr>, updated_on <chr>
```

As well as all sites:

``` r
mermaid_get_sites()
#> # A tibble: 1,746 x 13
#>    id    name  notes project latitude longitude country reef_type reef_zone
#>    <chr> <chr> <chr> <chr>      <dbl>     <dbl> <chr>   <chr>     <chr>    
#>  1 003f… GUZF… ""    bacd35…    16.8      -87.8 Belize  atoll     fore reef
#>  2 0059… Sapa… ""    53c98d…    -8.16     117.  Indone… barrier   fore reef
#>  3 00c9… DAM05 ""    8ff8c8…   -16.9      179.  Fiji    fringing  fore reef
#>  4 00cb… Anta… ""    408067…   -16.4       49.9 Madaga… fringing  fore reef
#>  5 016b… WAL0… ""    dcc880…   -16.8      179.  Fiji    fringing  back reef
#>  6 016d… Tanj… ""    5ab091…    -8.42     118.  Indone… fringing  crest    
#>  7 016e… SOL0… ""    dcc880…   -17.1      179.  Fiji    fringing  fore reef
#>  8 016e… Yac11 ""    70fd60…   -17.3      179.  Fiji    lagoon    back reef
#>  9 0181… WFR4  ""    bacd35…    16.8      -87.9 Belize  atoll     fore reef
#> 10 01b4… Kisi… ""    e0a252…   -13.6       48.1 Madaga… fringing  fore reef
#> # … with 1,736 more rows, and 4 more variables: exposure <chr>,
#> #   predecessor <chr>, created_on <chr>, updated_on <chr>
```

And all managements:

``` r
mermaid_get_managements()
#> # A tibble: 455 x 17
#>    id    name  name_secondary rules notes est_year no_take periodic_closure
#>    <chr> <chr> <chr>          <chr> <chr>    <int> <lgl>   <lgl>           
#>  1 0260… Dawa… ""             Open… ""          NA FALSE   FALSE           
#>  2 02cd… Kaib… ""             Peri… ""        2017 FALSE   TRUE            
#>  3 02e5… VIR3  ""             No T… ""        2012 TRUE    FALSE           
#>  4 03ba… VIR9  ""             No T… ""        2016 TRUE    FALSE           
#>  5 04e2… Bu_O… ""             Open… ""          NA FALSE   FALSE           
#>  6 04fc… Outs… "Control"      Open… ""          NA FALSE   FALSE           
#>  7 0543… LV02  ""             No T… ""          NA TRUE    FALSE           
#>  8 05a4… Cons… ""             No T… ""          NA TRUE    FALSE           
#>  9 05ec… Ra_t… ""             Peri… ""        2012 FALSE   TRUE            
#> 10 066e… Kana… ""             Open… ""          NA FALSE   FALSE           
#> # … with 445 more rows, and 9 more variables: open_access <lgl>,
#> #   size_limits <lgl>, gear_restriction <lgl>, species_restriction <lgl>,
#> #   compliance <chr>, predecessor <chr>, parties <chr>, created_on <chr>,
#> #   updated_on <chr>
```

### Other data

There is additional data available from the MERMAID API, both related to
specific projects and not. If you think you’ll need to use these, please
see the help for them by typing `?mermaid_get_endpoint` or
`?mermaid_get_project_endpoint`.
