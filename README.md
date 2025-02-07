
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mermaidr

<!-- badges: start -->

[![R build
status](https://github.com/data-mermaid/mermaidr/workflows/R-CMD-check/badge.svg)](https://github.com/data-mermaid/mermaidr/actions)
<!-- badges: end -->

`mermaidr` is an R package that enables you to access data from
[MERMAID](https://datamermaid.org/), an open-source data platform
developed to help you collect, analyze, and share coral reef monitoring
data. Through `mermaidr` you can access data from
[MERMAID](https://collect.datamermaid.org/) directly in R.

For more information and detailed instructions on usage, please visit
the [package website](https://data-mermaid.github.io/mermaidr/).

If you are new to the R programming language, our [new R users
guide](https://data-mermaid.github.io/mermaidr/articles/new_to_r.html)
is a great place to start! If you find yourself stuck, please don’t
hesitate to [ask for
help](https://data-mermaid.github.io/mermaidr/articles/getting_help.html).

## Installation

You can install mermaidr from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("data-mermaid/mermaidr")
```

## Usage

Through `mermaidr`, you can access aggregated data from your coral reef
surveys. To do this, first load the package and access your MERMAID
projects:

``` r
library(mermaidr)

projects <- mermaid_get_my_projects()
```

At this point, you will have to authenticate to the Collect app. R will
help you do this automatically by opening a browser window for you to
log in to Collect, either via Google sign-in or username and password -
however you normally do! Once you’ve logged in, come back to R. Your
login credentials will be stored for a day, until they expire, and you
will need to log in again. The package handles the expiration for you,
so just log in again when prompted.

This function gives us information on your projects, including project
countries, the number of sites, tags, data policies, and more:

``` r
projects
#> # A tibble: 19 × 15
#>    id          name  countries num_sites tags  notes status data_policy_beltfish
#>    <chr>       <chr> <chr>         <int> <chr> <chr> <chr>  <chr>               
#>  1 02e6915c-1… TWP … Indonesia        14 "WCS… ""    Open   Private             
#>  2 170e7182-7… 2018… Fiji             10 "WCS… "Thi… Open   Private             
#>  3 173c2353-3… Copy… Fiji              8 "WCS… "Nam… Open   Public Summary      
#>  4 1fbdb9ea-9… a2    Canada, …         9 "WWF… "Nam… Open   Private             
#>  5 2c0c9857-b… Shar… Canada, …        27 ""    "dhf… Open   Public Summary      
#>  6 2d6cee25-c… WCS … Mozambiq…        74 "WCS… "Dat… Open   Private             
#>  7 3a9ecb7c-f… Aceh… Indonesia        18 "WCS… ""    Open   Private             
#>  8 4080679f-1… Mada… Madagasc…        74 "WCS… "MAC… Open   Private             
#>  9 4d23d2a1-7… Mada… Madagasc…        16 "WCS… "Mon… Open   Public Summary      
#> 10 507d1af9-e… Kari… Indonesia        43 "WCS… ""    Open   Private             
#> 11 5679ef3d-b… Mada… Madagasc…        33 "WCS… ""    Open   Public Summary      
#> 12 5f13e6dc-4… Copy… Indonesia        43 "WCS… ""    Open   Public Summary      
#> 13 75ef7a5a-c… Kubu… Fiji             78 "WCS… ""    Open   Private             
#> 14 7a6bfd69-6… Copy… Belize           31 "WCS… ""    Open   Public Summary      
#> 15 9de82789-c… XPDC… Indonesia        37 ""    "XPD… Open   Private             
#> 16 a1b7ff1f-8… Grea… Fiji             76 "Uni… ""    Open   Private             
#> 17 bacd3529-e… Beli… Belize, …        35 "WCS… ""    Open   Public Summary      
#> 18 d065cba4-e… 2019… Fiji             31 "WCS… "Ble… Open   Private             
#> 19 e1efb1e0-0… 2016… Fiji              8 "WCS… "Nam… Open   Private             
#> # ℹ 7 more variables: data_policy_benthiclit <chr>,
#> #   data_policy_benthicpit <chr>, data_policy_benthicpqt <chr>,
#> #   data_policy_habitatcomplexity <chr>, data_policy_bleachingqc <chr>,
#> #   created_on <chr>, updated_on <chr>
```

To focus on just one or a few projects, you can filter by fields like
the project name, country, or tags using the `dplyr` package. For
example, I’ll narrow in on the WCS Mozambique Coral Reef Monitoring
project.

``` r
library(dplyr)

wcs_mozambique <- projects %>%
  filter(name == "WCS Mozambique Coral Reef Monitoring")
```

You can access data collected on fishbelt, benthic LIT, benthic PIT,
bleaching, or habitat complexity - the main function to pull data
related to your project is `mermaid_get_project_data()`:

``` r
wcs_mozambique_fishbelt_samples <- wcs_mozambique %>%
  mermaid_get_project_data(method = "fishbelt", data = "sampleevents")
```

The `data = "sampleevents"` argument specifies that I’d like to pull
data summarised to the level of a sample **event**, which is a site and
date - we can see that this pulls information about the site and date of
samples, along with aggregations like the total biomass of that
site/date, and broken down by trophic group and fish family.

``` r
wcs_mozambique_fishbelt_samples
#> # A tibble: 79 × 91
#>    project            tags  country site  latitude longitude reef_type reef_zone
#>    <chr>              <chr> <chr>   <chr>    <dbl>     <dbl> <chr>     <chr>    
#>  1 WCS Mozambique Co… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  2 WCS Mozambique Co… WCS … Mozamb… Baby…    -11.0      40.7 fringing  fore reef
#>  3 WCS Mozambique Co… WCS … Mozamb… Balu…    -22.0      35.5 patch     fore reef
#>  4 WCS Mozambique Co… WCS … Mozamb… Barr…    -26.0      32.9 barrier   back reef
#>  5 WCS Mozambique Co… WCS … Mozamb… Barr…    -26.1      32.9 barrier   back reef
#>  6 WCS Mozambique Co… WCS … Mozamb… Bunt…    -12.6      40.6 fringing  fore reef
#>  7 WCS Mozambique Co… WCS … Mozamb… Bunt…    -12.6      40.6 fringing  fore reef
#>  8 WCS Mozambique Co… WCS … Mozamb… Chec…    -26.8      32.9 patch     fore reef
#>  9 WCS Mozambique Co… WCS … Mozamb… Coli…    -12.6      40.6 fringing  fore reef
#> 10 WCS Mozambique Co… WCS … Mozamb… Dogt…    -12.5      40.6 fringing  crest    
#> # ℹ 69 more rows
#> # ℹ 83 more variables: reef_exposure <chr>, tide <lgl>, current <lgl>,
#> #   visibility <lgl>, management <chr>, management_secondary <lgl>,
#> #   management_est_year <dbl>, management_size <dbl>, management_parties <chr>,
#> #   management_compliance <chr>, management_rules <chr>, sample_date <date>,
#> #   depth_avg <dbl>, depth_sd <dbl>, biomass_kgha_avg <dbl>,
#> #   biomass_kgha_sd <dbl>, biomass_kgha_trophic_group_avg_piscivore <dbl>, …
```

If you’d like data related to the **units** of survey (for example, to
transects or quadrats), it’s just a matter of changing `data` to
“sampleunits”:

``` r
wcs_mozambique %>%
  mermaid_get_project_data(method = "fishbelt", data = "sampleunits")
#> # A tibble: 108 × 70
#>    project            tags  country site  latitude longitude reef_type reef_zone
#>    <chr>              <chr> <chr>   <chr>    <dbl>     <dbl> <chr>     <chr>    
#>  1 WCS Mozambique Co… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  2 WCS Mozambique Co… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  3 WCS Mozambique Co… WCS … Mozamb… Baby…    -11.0      40.7 fringing  fore reef
#>  4 WCS Mozambique Co… WCS … Mozamb… Balu…    -22.0      35.5 patch     fore reef
#>  5 WCS Mozambique Co… WCS … Mozamb… Barr…    -26.0      32.9 barrier   back reef
#>  6 WCS Mozambique Co… WCS … Mozamb… Barr…    -26.0      32.9 barrier   back reef
#>  7 WCS Mozambique Co… WCS … Mozamb… Barr…    -26.1      32.9 barrier   back reef
#>  8 WCS Mozambique Co… WCS … Mozamb… Barr…    -26.1      32.9 barrier   back reef
#>  9 WCS Mozambique Co… WCS … Mozamb… Bunt…    -12.6      40.6 fringing  fore reef
#> 10 WCS Mozambique Co… WCS … Mozamb… Bunt…    -12.6      40.6 fringing  fore reef
#> # ℹ 98 more rows
#> # ℹ 62 more variables: reef_exposure <chr>, reef_slope <lgl>, tide <lgl>,
#> #   current <lgl>, visibility <lgl>, relative_depth <lgl>, management <chr>,
#> #   management_secondary <lgl>, management_est_year <dbl>,
#> #   management_size <dbl>, management_parties <chr>,
#> #   management_compliance <chr>, management_rules <chr>, sample_date <date>,
#> #   sample_time <time>, depth <dbl>, transect_number <dbl>, label <dbl>, …
```

And raw observations are available by changing it to “observations”:

``` r
wcs_mozambique %>%
  mermaid_get_project_data(method = "fishbelt", data = "observations")
#> # A tibble: 2,637 × 52
#>    project            tags  country site  latitude longitude reef_type reef_zone
#>    <chr>              <chr> <chr>   <chr>    <dbl>     <dbl> <chr>     <chr>    
#>  1 WCS Mozambique Co… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  2 WCS Mozambique Co… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  3 WCS Mozambique Co… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  4 WCS Mozambique Co… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  5 WCS Mozambique Co… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  6 WCS Mozambique Co… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  7 WCS Mozambique Co… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  8 WCS Mozambique Co… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  9 WCS Mozambique Co… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#> 10 WCS Mozambique Co… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#> # ℹ 2,627 more rows
#> # ℹ 44 more variables: reef_exposure <chr>, reef_slope <lgl>, tide <lgl>,
#> #   current <lgl>, visibility <lgl>, relative_depth <lgl>, management <chr>,
#> #   management_secondary <lgl>, management_est_year <dbl>,
#> #   management_size <dbl>, management_parties <chr>,
#> #   management_compliance <chr>, management_rules <chr>, sample_date <date>,
#> #   sample_time <time>, depth <dbl>, transect_length <dbl>, …
```

For more details on accessing project data, please see the [Accessing
Project
Data](https://data-mermaid.github.io/mermaidr/articles/accessing_project_data.html)
article.

You may also want to access data that is not related to projects. To
access this data, you do not need to authenticate R with MERMAID.

For example, you can pull reference data (the names and information of
the fish and benthic attributes you can choose in MERMAID), using
`mermaid_get_reference()`:

``` r
mermaid_get_reference(reference = "fishfamilies")
#> # A tibble: 161 × 9
#>    id      name  status biomass_constant_a biomass_constant_b biomass_constant_c
#>    <chr>   <chr> <chr>               <dbl>              <dbl>              <dbl>
#>  1 0091bb… Kyph… Open              0.0193                3.03              0.986
#>  2 00b644… Mugi… Open              0.0166                2.94              0.974
#>  3 00f427… Zena… Open              0.00427               3.02              1    
#>  4 02268a… Sphy… Open              0.00448               3.11              1    
#>  5 0880aa… Labr… Open              0.0120                3.04              0.997
#>  6 0aff09… Scom… Open              0.0111                3.03              0.988
#>  7 0b69f2… Ophi… Open              0.00139               2.93              1    
#>  8 0d9904… Albu… Open              0.0105                2.99              1    
#>  9 0e5b1d… Hemi… Open              0.0373                3.16              0.99 
#> 10 151384… Serr… Open              0.0136                3.03              0.997
#> # ℹ 151 more rows
#> # ℹ 3 more variables: regions <chr>, created_on <chr>, updated_on <chr>
```

Using this function, you can access the fish family, fish genera, fish
species, and benthic attributes references by changing the `reference`
argument.

You can also get a list of *all* projects (not just your own):

``` r
mermaid_get_projects()
#> # A tibble: 170 × 15
#>    id          name  countries num_sites tags  notes status data_policy_beltfish
#>    <chr>       <chr> <chr>         <int> <chr> <chr> <chr>  <chr>               
#>  1 00673bdf-b… TPK … "Indones…        15 "WCS… ""    Open   Private             
#>  2 01bbe407-f… Mada… "Madagas…        12 "WCS… "Sur… Open   Private             
#>  3 02e6915c-1… TWP … "Indones…        14 "WCS… ""    Open   Private             
#>  4 07df6a50-6… Cend… "Indones…        37 "TNC… ""    Open   Private             
#>  5 0b39fe6c-0… Open… "Indones…         2 "WCS… "Thi… Open   Private             
#>  6 0c000a00-f… 2019… "Fiji"           18 "WCS… ""    Open   Private             
#>  7 0c16681c-6… REEF… ""                0 ""    ""    Open   Public Summary      
#>  8 0de6f1fc-1… Copy… "Fiji"            9 "WWF… "Dat… Open   Public Summary      
#>  9 0f17035f-0… what  ""                0 ""    ""    Open   Public Summary      
#> 10 124b9142-3… Sam   ""                0 ""    ""    Open   Private             
#> # ℹ 160 more rows
#> # ℹ 7 more variables: data_policy_benthiclit <chr>,
#> #   data_policy_benthicpit <chr>, data_policy_benthicpqt <chr>,
#> #   data_policy_habitatcomplexity <chr>, data_policy_bleachingqc <chr>,
#> #   created_on <chr>, updated_on <chr>
```

As well as all sites:

``` r
mermaid_get_sites()
#> # A tibble: 2,725 × 13
#>    id         name  notes project latitude longitude country reef_type reef_zone
#>    <chr>      <chr> <chr> <chr>      <dbl>     <dbl> <chr>   <chr>     <chr>    
#>  1 0415d9e5-… mysi… <NA>  2c56b9…     -1         -1  Bangla… atoll     back reef
#>  2 6cd334f9-… meli… <NA>  ea4751…     49       -110  Canada  atoll     back reef
#>  3 afe4dac0-… meli… <NA>  ea4751…     49       -110  Canada  atoll     back reef
#>  4 02355d6c-… BA09  <NA>  a1b7ff…    -17.4      178. Fiji    atoll     back reef
#>  5 03e5576e-… BA03  <NA>  89f2d4…    -17.4      178. Fiji    atoll     back reef
#>  6 0879390b-… BA16  <NA>  a1b7ff…    -17.2      178. Fiji    atoll     back reef
#>  7 18f09a09-… BA06  <NA>  0de6f1…    -17.4      178. Fiji    atoll     back reef
#>  8 19258ea5-… BA15  <NA>  a1b7ff…    -17.2      178. Fiji    atoll     back reef
#>  9 19e60884-… YA02  <NA>  a1b7ff…    -17.0      177. Fiji    atoll     back reef
#> 10 20aeb13f-… BA11  <NA>  a1b7ff…    -17.3      178. Fiji    atoll     back reef
#> # ℹ 2,715 more rows
#> # ℹ 4 more variables: exposure <chr>, predecessor <chr>, created_on <chr>,
#> #   updated_on <chr>
```

And all managements:

``` r
mermaid_get_managements()
#> # A tibble: 1,344 × 17
#>    id         name  name_secondary est_year  size parties compliance open_access
#>    <chr>      <chr> <chr>             <int> <dbl> <chr>   <chr>      <lgl>      
#>  1 0031d438-… Mata… Fish Habitat …     2018    25 commun… full       FALSE      
#>  2 00c920d6-… Lape… Special Manag…     2017   198 commun… full       FALSE      
#>  3 02479d18-… Prot… Zona Perlindu…     2015    NA commun… full       FALSE      
#>  4 029852d5-… Haaf… Fish Habitat …     2007   139 commun… full       FALSE      
#>  5 02cd9d54-… Kaib… <NA>               2017    NA commun… full       FALSE      
#>  6 02e546ac-… VIR3  <NA>               2012    NA commun… full       FALSE      
#>  7 03bab6aa-… VIR9  <NA>               2016    NA commun… full       FALSE      
#>  8 04286cba-… Test… <NA>               2018    NA govern… full       FALSE      
#>  9 044c6e26-… Fono… Fish Habitat …     2017   191 commun… full       FALSE      
#> 10 05227cee-… Test… <NA>               2018     5 <NA>    full       FALSE      
#> # ℹ 1,334 more rows
#> # ℹ 9 more variables: no_take <lgl>, access_restriction <lgl>,
#> #   periodic_closure <lgl>, size_limits <lgl>, gear_restriction <lgl>,
#> #   species_restriction <lgl>, notes <chr>, created_on <chr>, updated_on <chr>
```

There is additional data available from the MERMAID API, both related to
specific projects and not. If you think you’ll need to use these, please
see `mermaid_get_endpoint()` and `mermaid_get_project_endpoint()`.

This is a small sample of the wealth of data that’s available on your
MERMAID projects, and on the ecosystem as a whole! Please explore the
[package website](https://data-mermaid.github.io/mermaidr/) for more.
