
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
#> # A tibble: 22 × 20
#>    id    name  countries num_sites num_active_sample_un…¹ num_sample_units tags 
#>    <chr> <chr> <chr>         <int>                  <int>            <dbl> <chr>
#>  1 e1ef… 2016… Fiji              9                     10               80 WCS …
#>  2 170e… 2018… Fiji             10                      5              121 WCS …
#>  3 d065… 2019… Fiji             31                      3               32 WCS …
#>  4 1fbd… a2    Canada, …         9                      9                0 WWF-…
#>  5 3a9e… Aceh… Indonesia        18                     55              198 WCS …
#>  6 bacd… Beli… Belize, …        37                    109              248 WCS …
#>  7 173c… Copy… Fiji              8                      0                0 WCS …
#>  8 7a6b… Copy… Belize           31                      3                0 WCS …
#>  9 5f13… Copy… Indonesia        43                      0                0 WCS …
#> 10 a1b7… Grea… Fiji             76                      8              648 Fiji…
#> # ℹ 12 more rows
#> # ℹ abbreviated name: ¹​num_active_sample_units
#> # ℹ 13 more variables: project_admins <chr>, suggested_citation <chr>,
#> #   bbox <df[,4]>, notes <chr>, status <chr>, data_policy_beltfish <chr>,
#> #   data_policy_benthiclit <chr>, data_policy_benthicpit <chr>,
#> #   data_policy_benthicpqt <chr>, data_policy_habitatcomplexity <chr>,
#> #   data_policy_bleachingqc <chr>, created_on <chr>, updated_on <chr>
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
#> # A tibble: 79 × 92
#>    project            tags  country site  latitude longitude reef_type reef_zone
#>    <chr>              <chr> <chr>   <chr>    <dbl>     <dbl> <chr>     <chr>    
#>  1 WCS Mozambique Co… WCS … Mozamb… Bunt…    -12.6      40.6 fringing  fore reef
#>  2 WCS Mozambique Co… WCS … Mozamb… Chec…    -26.8      32.9 patch     fore reef
#>  3 WCS Mozambique Co… WCS … Mozamb… Mate…    -12.2      40.6 fringing  fore reef
#>  4 WCS Mozambique Co… WCS … Mozamb… Jagg…    -26.8      32.9 patch     fore reef
#>  5 WCS Mozambique Co… WCS … Mozamb… Ligh…    -12.3      40.6 fringing  fore reef
#>  6 WCS Mozambique Co… WCS … Mozamb… Kisi…    -11.0      40.7 lagoon    back reef
#>  7 WCS Mozambique Co… WCS … Mozamb… Kisi…    -11.0      40.7 lagoon    back reef
#>  8 WCS Mozambique Co… WCS … Mozamb… Kisi…    -11.0      40.7 lagoon    back reef
#>  9 WCS Mozambique Co… WCS … Mozamb… Two …    -21.8      35.5 barrier   fore reef
#> 10 WCS Mozambique Co… WCS … Mozamb… Luta…    -12.3      40.6 fringing  fore reef
#> # ℹ 69 more rows
#> # ℹ 84 more variables: reef_exposure <chr>, tide <lgl>, current <lgl>,
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
#> # A tibble: 108 × 71
#>    project            tags  country site  latitude longitude reef_type reef_zone
#>    <chr>              <chr> <chr>   <chr>    <dbl>     <dbl> <chr>     <chr>    
#>  1 WCS Mozambique Co… WCS … Mozamb… Metu…    -11.1      40.7 fringing  fore reef
#>  2 WCS Mozambique Co… WCS … Mozamb… Lond…    -12.9      40.5 fringing  fore reef
#>  3 WCS Mozambique Co… WCS … Mozamb… Pemb…    -13.0      40.6 fringing  fore reef
#>  4 WCS Mozambique Co… WCS … Mozamb… Pang…    -11.0      40.6 lagoon    back reef
#>  5 WCS Mozambique Co… WCS … Mozamb… Bunt…    -12.6      40.6 fringing  fore reef
#>  6 WCS Mozambique Co… WCS … Mozamb… Para…    -14.5      40.7 fringing  fore reef
#>  7 WCS Mozambique Co… WCS … Mozamb… Libe…    -14.5      40.7 fringing  back reef
#>  8 WCS Mozambique Co… WCS … Mozamb… Patc…    -12.3      40.6 fringing  fore reef
#>  9 WCS Mozambique Co… WCS … Mozamb… Pemb…    -13.0      40.6 fringing  fore reef
#> 10 WCS Mozambique Co… WCS … Mozamb… Pang…    -11.0      40.6 lagoon    back reef
#> # ℹ 98 more rows
#> # ℹ 63 more variables: reef_exposure <chr>, reef_slope <lgl>, tide <lgl>,
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
#>  1 WCS Mozambique Co… WCS … Mozamb… Moti…    -14.4      40.7 fringing  back reef
#>  2 WCS Mozambique Co… WCS … Mozamb… Moti…    -14.4      40.7 fringing  back reef
#>  3 WCS Mozambique Co… WCS … Mozamb… Moti…    -14.4      40.7 fringing  back reef
#>  4 WCS Mozambique Co… WCS … Mozamb… Moti…    -14.4      40.7 fringing  back reef
#>  5 WCS Mozambique Co… WCS … Mozamb… Moti…    -14.4      40.7 fringing  back reef
#>  6 WCS Mozambique Co… WCS … Mozamb… Moti…    -14.4      40.7 fringing  back reef
#>  7 WCS Mozambique Co… WCS … Mozamb… Moti…    -14.4      40.7 fringing  back reef
#>  8 WCS Mozambique Co… WCS … Mozamb… Moti…    -14.4      40.7 fringing  back reef
#>  9 WCS Mozambique Co… WCS … Mozamb… Moti…    -14.4      40.7 fringing  back reef
#> 10 WCS Mozambique Co… WCS … Mozamb… Moti…    -14.4      40.7 fringing  back reef
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
#>  1 3f7bf3… Acan… Open              0.0256                2.97              0.990
#>  2 455a7b… Achi… Open              0.0164                3.11              1    
#>  3 78d71a… Acip… Open              0.00497               3.25              1    
#>  4 0d9904… Albu… Open              0.0105                2.99              1    
#>  5 530c63… Amba… Open              0.0130                2.96              1    
#>  6 e8dfe9… Angu… Open              0.0006                3.35              1    
#>  7 cf2b18… Anom… Open              0.0112                3.04              1    
#>  8 9ef4d9… Ante… Open              0.0200                3.01              1    
#>  9 7a9316… Aplo… Open              0.00487               3.16              1    
#> 10 26fff3… Apog… Open              0.0142                3.15              0.993
#> # ℹ 151 more rows
#> # ℹ 3 more variables: regions <chr>, created_on <chr>, updated_on <chr>
```

Using this function, you can access the fish family, fish genera, fish
species, and benthic attributes references by changing the `reference`
argument.

You can also get a list of *all* projects (not just your own):

``` r
mermaid_get_projects()
#> # A tibble: 197 × 20
#>    id    name  countries num_sites num_active_sample_un…¹ num_sample_units tags 
#>    <chr> <chr> <chr>         <int>                  <int>            <dbl> <chr>
#>  1 fe3f… 1000… ""                0                      0                0 ""   
#>  2 60dd… 2013… "Fiji"           17                      3              130 "WCS…
#>  3 7376… 2014… "Fiji"           24                      2              345 "WCS…
#>  4 ac93… 2016… "Fiji"           24                      2              146 "WCS…
#>  5 e1ef… 2016… "Fiji"            9                     10               80 "WCS…
#>  6 d549… 2017… "Fiji"           31                      0              279 "WCS…
#>  7 c0ba… 2018… "Fiji"           22                      0              130 "WCS…
#>  8 170e… 2018… "Fiji"           10                      5              121 "WCS…
#>  9 95e0… 2019… "Fiji"           44                      0              406 "WCS…
#> 10 d065… 2019… "Fiji"           31                      3               32 "WCS…
#> # ℹ 187 more rows
#> # ℹ abbreviated name: ¹​num_active_sample_units
#> # ℹ 13 more variables: project_admins <chr>, suggested_citation <chr>,
#> #   bbox <df[,4]>, notes <chr>, status <chr>, data_policy_beltfish <chr>,
#> #   data_policy_benthiclit <chr>, data_policy_benthicpit <chr>,
#> #   data_policy_benthicpqt <chr>, data_policy_habitatcomplexity <chr>,
#> #   data_policy_bleachingqc <chr>, created_on <chr>, updated_on <chr>
```

As well as all sites:

``` r
mermaid_get_sites()
#> # A tibble: 2,783 × 13
#>    id         name  notes project latitude longitude country reef_type reef_zone
#>    <chr>      <chr> <chr> <chr>      <dbl>     <dbl> <chr>   <chr>     <chr>    
#>  1 0415d9e5-… mysi… <NA>  2c56b9…     -1        -1   Bangla… atoll     back reef
#>  2 547dae1b-… bulk… <NA>  2c0c98…     47.5     -81.8 Canada  atoll     back reef
#>  3 42d67bb1-… dupe… <NA>  c08ff9…      1         1   Canada  atoll     back reef
#>  4 874c5a80-… dupe… <NA>  c08ff9…      1         1   Canada  atoll     back reef
#>  5 706df098-… meli… <NA>  6cb15b…     49      -110   Canada  atoll     back reef
#>  6 a022b5c5-… meli… <NA>  6cb15b…     49      -110   Canada  atoll     back reef
#>  7 85942572-… meli… <NA>  9c04dd…     49      -110   Canada  atoll     back reef
#>  8 afe4dac0-… meli… <NA>  ea4751…     49      -110   Canada  atoll     back reef
#>  9 f53b19b2-… meli… <NA>  9c04dd…     49      -110   Canada  atoll     back reef
#> 10 6cd334f9-… meli… <NA>  ea4751…     49      -110   Canada  atoll     back reef
#> # ℹ 2,773 more rows
#> # ℹ 4 more variables: exposure <chr>, predecessor <chr>, created_on <chr>,
#> #   updated_on <chr>
```

And all managements:

``` r
mermaid_get_managements()
#> # A tibble: 1,744 × 17
#>    id         name  name_secondary est_year  size parties compliance open_access
#>    <chr>      <chr> <chr>             <int> <dbl> <chr>   <chr>      <lgl>      
#>  1 248e26fe-… 678j… <NA>                 NA     7 <NA>    full       FALSE      
#>  2 c0956a14-… 678j… <NA>                 NA     7 <NA>    full       FALSE      
#>  3 ea40bbe3-… 678j… <NA>                 NA     7 <NA>    full       FALSE      
#>  4 0e9aa677-… 678j… <NA>                 NA     7 <NA>    full       FALSE      
#>  5 264344e1-… 678j… <NA>                 NA     7 <NA>    full       FALSE      
#>  6 227a82a5-… 678j… <NA>                 NA     7 <NA>    full       FALSE      
#>  7 9168b477-… 678j… <NA>                 NA     7 <NA>    full       FALSE      
#>  8 9b1df02a-… 678j… <NA>                 NA     7 <NA>    full       FALSE      
#>  9 2cd81d70-… 678j… <NA>                 NA     7 <NA>    full       FALSE      
#> 10 d70de8ff-… 678j… <NA>                 NA     7 <NA>    full       FALSE      
#> # ℹ 1,734 more rows
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
