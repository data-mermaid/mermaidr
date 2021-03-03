
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mermaidr

<!-- badges: start -->
[![R build status](https://github.com/data-mermaid/mermaidr/workflows/R-CMD-check/badge.svg)](https://github.com/data-mermaid/mermaidr/actions)
<!-- badges: end -->

`mermaidr` is an R package that enables you to access data from
[MERMAID](https://datamermaid.org/), an open-source data platform
developed to help you collect, analyze, and share coral reef monitoring
data. Through `mermaidr` you can access data input in the collection
portal, [MERMAID Collect](https://collect.datamermaid.org/), directly
from R.

For more information and detailed instructions on usage, please visit
the [package website](https://data-mermaid.github.io/mermaidr/).

If you are new to the R programming language, our [new R users guide]()
is a great place to start!

## Installation

You can install mermaidr from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("data-mermaid/mermaidr")
```

## Usage

Through `mermaidr`, you can access aggregated data from your coral reef
studies. To do this, first load the package and access your MERMAID
Collect projects:

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
#> # A tibble: 9 x 14
#>   id    name  countries num_sites tags  notes status data_policy_bel…
#>   <chr> <chr> <chr>         <int> <chr> <chr> <chr>  <chr>           
#> 1 2d6c… WCS … Mozambiq…        74 "WCS… "Dat… Open   Private         
#> 2 3a9e… Aceh… Indonesia        18 "Vib… ""    Open   Private         
#> 3 4080… Mada… Madagasc…        74 "WCS… "MAC… Open   Private         
#> 4 4d23… Mada… Madagasc…        16 "WCS… "Mon… Open   Public Summary  
#> 5 507d… Kari… Indonesia        43 "Vib… ""    Open   Private         
#> 6 5679… Mada… Madagasc…        33 "WCS… ""    Open   Public Summary  
#> 7 75ef… Kubu… Fiji             78 "WCS… ""    Open   Private         
#> 8 9de8… XPDC… Indonesia        37 ""    "XPD… Open   Private         
#> 9 a1b7… Grea… Fiji             76 "Fij… ""    Open   Private         
#> # … with 6 more variables: data_policy_benthiclit <chr>,
#> #   data_policy_benthicpit <chr>, data_policy_habitatcomplexity <chr>,
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
#> # A tibble: 80 x 31
#>    project tags  country site  latitude longitude reef_type reef_zone
#>    <chr>   <chr> <chr>   <chr>    <dbl>     <dbl> <chr>     <chr>    
#>  1 WCS Mo… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  2 WCS Mo… WCS … Mozamb… Baby…    -11.0      40.7 fringing  fore reef
#>  3 WCS Mo… WCS … Mozamb… Balu…    -22.0      35.5 patch     fore reef
#>  4 WCS Mo… WCS … Mozamb… Barr…    -26.0      32.9 barrier   back reef
#>  5 WCS Mo… WCS … Mozamb… Barr…    -26.1      32.9 barrier   back reef
#>  6 WCS Mo… WCS … Mozamb… Bunt…    -12.6      40.6 fringing  fore reef
#>  7 WCS Mo… WCS … Mozamb… Bunt…    -12.6      40.6 fringing  fore reef
#>  8 WCS Mo… WCS … Mozamb… Chec…    -26.8      32.9 patch     fore reef
#>  9 WCS Mo… WCS … Mozamb… Coli…    -12.6      40.6 fringing  fore reef
#> 10 WCS Mo… WCS … Mozamb… Dogt…    -12.5      40.6 fringing  crest    
#> # … with 70 more rows, and 26 more variables: reef_exposure <chr>, tide <lgl>,
#> #   current <lgl>, visibility <lgl>, management <chr>,
#> #   management_secondary <chr>, management_est_year <int>,
#> #   management_size <dbl>, management_parties <chr>,
#> #   management_compliance <chr>, management_rules <chr>, sample_date <date>,
#> #   depth_avg <dbl>, biomass_kgha_avg <dbl>,
#> #   biomass_kgha_by_trophic_group_avg$piscivore <dbl>, $planktivore <dbl>,
#> #   $`invertivore-mobile` <dbl>, $`herbivore-detritivore` <dbl>,
#> #   data_policy_beltfish <chr>, project_notes <chr>, …
```

If you’d like data related to the **units** of survey (for example, to
transects or quadrats), it’s just a matter of changing `data` to
“sampleunits”:

``` r
wcs_mozambique %>%
  mermaid_get_project_data(method = "fishbelt", data = "sampleunits")
#> # A tibble: 111 x 41
#>    project tags  country site  latitude longitude reef_type reef_zone
#>    <chr>   <chr> <chr>   <chr>    <dbl>     <dbl> <chr>     <chr>    
#>  1 WCS Mo… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  2 WCS Mo… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  3 WCS Mo… WCS … Mozamb… Baby…    -11.0      40.7 fringing  fore reef
#>  4 WCS Mo… WCS … Mozamb… Balu…    -22.0      35.5 patch     fore reef
#>  5 WCS Mo… WCS … Mozamb… Barr…    -26.0      32.9 barrier   back reef
#>  6 WCS Mo… WCS … Mozamb… Barr…    -26.0      32.9 barrier   back reef
#>  7 WCS Mo… WCS … Mozamb… Barr…    -26.1      32.9 barrier   back reef
#>  8 WCS Mo… WCS … Mozamb… Barr…    -26.1      32.9 barrier   back reef
#>  9 WCS Mo… WCS … Mozamb… Bunt…    -12.6      40.6 fringing  fore reef
#> 10 WCS Mo… WCS … Mozamb… Bunt…    -12.6      40.6 fringing  fore reef
#> # … with 101 more rows, and 36 more variables: reef_exposure <chr>,
#> #   reef_slope <lgl>, tide <lgl>, current <lgl>, visibility <lgl>,
#> #   relative_depth <lgl>, management <chr>, management_secondary <chr>,
#> #   management_est_year <int>, management_size <dbl>, management_parties <chr>,
#> #   management_compliance <chr>, management_rules <chr>, sample_date <date>,
#> #   sample_time <chr>, depth <dbl>, transect_number <int>, label <chr>,
#> #   size_bin <chr>, transect_length <int>, …
```

And raw observations are available by changing it to “observations”:

``` r
wcs_mozambique %>%
  mermaid_get_project_data(method = "fishbelt", data = "observations")
#> # A tibble: 2,714 x 50
#>    project tags  country site  latitude longitude reef_type reef_zone
#>    <chr>   <chr> <chr>   <chr>    <dbl>     <dbl> <chr>     <chr>    
#>  1 WCS Mo… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  2 WCS Mo… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  3 WCS Mo… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  4 WCS Mo… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  5 WCS Mo… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  6 WCS Mo… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  7 WCS Mo… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  8 WCS Mo… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  9 WCS Mo… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#> 10 WCS Mo… WCS … Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#> # … with 2,704 more rows, and 42 more variables: reef_exposure <chr>,
#> #   reef_slope <lgl>, tide <lgl>, current <lgl>, visibility <lgl>,
#> #   relative_depth <lgl>, management <chr>, management_secondary <chr>,
#> #   management_est_year <int>, management_size <dbl>, management_parties <chr>,
#> #   management_compliance <chr>, management_rules <chr>, sample_date <date>,
#> #   sample_time <chr>, transect_length <int>, transect_width <chr>,
#> #   size_bin <chr>, observers <chr>, depth <dbl>, …
```

This is a small sample of the wealth of data that’s available on your
MERMAID projects, and on the ecosystem as a whole! Please explore the
[package website](https://data-mermaid.github.io/mermaidr/) for more.
