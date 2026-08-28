
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
#> # A tibble: 16 × 21
#>    id    name  countries num_sites num_active_sample_un…¹ num_sample_units tags 
#>    <chr> <chr> <chr>         <int>                  <int>            <dbl> <chr>
#>  1 e1ef… 2016… Fiji              9                     10               80 WCS …
#>  2 170e… 2018… Fiji             10                      5              121 WCS …
#>  3 d065… 2019… Fiji             31                      3               32 WCS …
#>  4 1fbd… a2    Canada, …         9                      9                0 WWF-…
#>  5 3a9e… Aceh… Indonesia        18                     55              198 WCS …
#>  6 bacd… Beli… Belize, …        39                    120              259 WCS …
#>  7 a1b7… Grea… Fiji             76                      8              648 Fiji…
#>  8 507d… Kari… Indonesia        43                     18              842 WCS …
#>  9 75ef… Kubu… Fiji             78                      1             1145 WCS …
#> 10 5679… Mada… Madagasc…        33                      0               49 WCS …
#> 11 4080… Mada… Madagasc…        74                      4               84 WCS …
#> 12 4d79… MERM… Indonesi…        13                     73               31 test…
#> 13 2c0c… Shar… Canada, …        28                     10                7 <NA> 
#> 14 02e6… TWP … Indonesia        14                     10                2 WCS …
#> 15 2d6c… WCS … Mozambiq…        74                      6              247 WCS …
#> 16 9de8… XPDC… Indonesia        37                     71              450 <NA> 
#> # ℹ abbreviated name: ¹​num_active_sample_units
#> # ℹ 14 more variables: suggested_citation <chr>, bbox <df[,4]>, notes <chr>,
#> #   status <chr>, data_policy_beltfish <chr>, data_policy_benthiclit <chr>,
#> #   data_policy_benthicpit <chr>, data_policy_habitatcomplexity <chr>,
#> #   data_policy_bleachingqc <chr>, data_policy_benthicpqt <chr>,
#> #   data_policy_macroinvertebrate <chr>, includes_gfcr <lgl>, created_on <chr>,
#> #   updated_on <chr>
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
macroinvertebrates, bleaching, or habitat complexity - the main function
to pull data related to your project is `mermaid_get_project_data()`:

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
#> # A tibble: 79 × 94
#>    id           project project_admins country contact_link tags  site  latitude
#>    <chr>        <chr>   <chr>          <chr>   <chr>        <chr> <chr>    <dbl>
#>  1 36316e77-ea… WCS Mo… Emily Darling… Mozamb… https://dat… WCS … Bunt…    -12.6
#>  2 cc6b0a95-64… WCS Mo… Emily Darling… Mozamb… https://dat… WCS … Chec…    -26.8
#>  3 83353357-69… WCS Mo… Emily Darling… Mozamb… https://dat… WCS … Mate…    -12.2
#>  4 364e439d-e4… WCS Mo… Emily Darling… Mozamb… https://dat… WCS … Jagg…    -26.8
#>  5 324f2b0a-4a… WCS Mo… Emily Darling… Mozamb… https://dat… WCS … Ligh…    -12.3
#>  6 1c2d7c1e-c6… WCS Mo… Emily Darling… Mozamb… https://dat… WCS … Kisi…    -11.0
#>  7 cff14283-95… WCS Mo… Emily Darling… Mozamb… https://dat… WCS … Kisi…    -11.0
#>  8 de6b403d-29… WCS Mo… Emily Darling… Mozamb… https://dat… WCS … Kisi…    -11.0
#>  9 fcff6a0b-4c… WCS Mo… Emily Darling… Mozamb… https://dat… WCS … Two …    -21.8
#> 10 910b3d5b-fa… WCS Mo… Emily Darling… Mozamb… https://dat… WCS … Luta…    -12.3
#> # ℹ 69 more rows
#> # ℹ 86 more variables: longitude <dbl>, reef_exposure <chr>, reef_type <chr>,
#> #   reef_zone <chr>, tide <lgl>, visibility <lgl>, current <lgl>,
#> #   depth_avg <dbl>, depth_sd <dbl>, management <chr>,
#> #   management_secondary <lgl>, management_est_year <dbl>,
#> #   management_size <dbl>, management_parties <chr>,
#> #   management_compliance <chr>, management_rules <chr>, sample_date <date>, …
```

If you’d like data related to the **units** of survey (for example, to
transects or quadrats), it’s just a matter of changing `data` to
“sampleunits”:

``` r
wcs_mozambique %>%
  mermaid_get_project_data(method = "fishbelt", data = "sampleunits")
#> # A tibble: 108 × 73
#>    project    project_admins country contact_link tags  site  latitude longitude
#>    <chr>      <chr>          <chr>   <chr>        <chr> <chr>    <dbl>     <dbl>
#>  1 WCS Mozam… Emily Darling… Mozamb… https://dat… WCS … Kisi…    -11.0      40.7
#>  2 WCS Mozam… Emily Darling… Mozamb… https://dat… WCS … Ligh…    -12.3      40.6
#>  3 WCS Mozam… Emily Darling… Mozamb… https://dat… WCS … Aqua…    -21.8      35.5
#>  4 WCS Mozam… Emily Darling… Mozamb… https://dat… WCS … Pang…    -11.0      40.6
#>  5 WCS Mozam… Emily Darling… Mozamb… https://dat… WCS … Pemb…    -13.0      40.6
#>  6 WCS Mozam… Emily Darling… Mozamb… https://dat… WCS … Aqua…    -21.8      35.5
#>  7 WCS Mozam… Emily Darling… Mozamb… https://dat… WCS … Pemb…    -13.0      40.6
#>  8 WCS Mozam… Emily Darling… Mozamb… https://dat… WCS … Jagg…    -26.8      32.9
#>  9 WCS Mozam… Emily Darling… Mozamb… https://dat… WCS … Papa…    -11.0      40.7
#> 10 WCS Mozam… Emily Darling… Mozamb… https://dat… WCS … Lond…    -12.9      40.5
#> # ℹ 98 more rows
#> # ℹ 65 more variables: reef_exposure <chr>, reef_slope <lgl>, reef_type <chr>,
#> #   reef_zone <chr>, sample_time <time>, tide <lgl>, visibility <lgl>,
#> #   current <lgl>, depth <dbl>, relative_depth <lgl>, management <chr>,
#> #   management_secondary <lgl>, management_est_year <dbl>,
#> #   management_size <dbl>, management_parties <chr>,
#> #   management_compliance <chr>, management_rules <chr>, sample_date <date>, …
```

And raw observations are available by changing it to “observations”:

``` r
wcs_mozambique %>%
  mermaid_get_project_data(method = "fishbelt", data = "observations")
#> # A tibble: 2,637 × 55
#>    project    project_admins country contact_link tags  site  latitude longitude
#>    <chr>      <chr>          <chr>   <chr>        <chr> <chr>    <dbl>     <dbl>
#>  1 WCS Mozam… Emily Darling… Mozamb… https://dat… WCS … Moti…    -14.4      40.7
#>  2 WCS Mozam… Emily Darling… Mozamb… https://dat… WCS … Moti…    -14.4      40.7
#>  3 WCS Mozam… Emily Darling… Mozamb… https://dat… WCS … Moti…    -14.4      40.7
#>  4 WCS Mozam… Emily Darling… Mozamb… https://dat… WCS … Moti…    -14.4      40.7
#>  5 WCS Mozam… Emily Darling… Mozamb… https://dat… WCS … Moti…    -14.4      40.7
#>  6 WCS Mozam… Emily Darling… Mozamb… https://dat… WCS … Moti…    -14.4      40.7
#>  7 WCS Mozam… Emily Darling… Mozamb… https://dat… WCS … Moti…    -14.4      40.7
#>  8 WCS Mozam… Emily Darling… Mozamb… https://dat… WCS … Moti…    -14.4      40.7
#>  9 WCS Mozam… Emily Darling… Mozamb… https://dat… WCS … Moti…    -14.4      40.7
#> 10 WCS Mozam… Emily Darling… Mozamb… https://dat… WCS … Moti…    -14.4      40.7
#> # ℹ 2,627 more rows
#> # ℹ 47 more variables: reef_exposure <chr>, reef_slope <lgl>, reef_type <chr>,
#> #   reef_zone <chr>, sample_time <time>, tide <lgl>, visibility <lgl>,
#> #   current <lgl>, depth <dbl>, relative_depth <lgl>, management <chr>,
#> #   management_secondary <lgl>, management_est_year <dbl>,
#> #   management_size <dbl>, management_parties <chr>,
#> #   management_compliance <chr>, management_rules <chr>, sample_date <date>, …
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
#> # A tibble: 112 × 21
#>    id    name  countries num_sites num_active_sample_un…¹ num_sample_units tags 
#>    <chr> <chr> <chr>         <int>                  <int>            <dbl> <chr>
#>  1 13a2… 0909… <NA>              0                      0                0 <NA> 
#>  2 f9c5… 1313… <NA>              0                      1                0 <NA> 
#>  3 60dd… 2013… Fiji             17                      8              130 WCS …
#>  4 7376… 2014… Fiji             24                      2              345 WCS …
#>  5 ac93… 2016… Fiji             24                      2              146 WCS …
#>  6 e1ef… 2016… Fiji              9                     10               80 WCS …
#>  7 d549… 2017… Fiji             31                      0              279 WCS …
#>  8 c0ba… 2018… Fiji             22                      0              130 WCS …
#>  9 170e… 2018… Fiji             10                      5              121 WCS …
#> 10 95e0… 2019… Fiji             44                      0              406 WCS …
#> # ℹ 102 more rows
#> # ℹ abbreviated name: ¹​num_active_sample_units
#> # ℹ 14 more variables: suggested_citation <chr>, bbox <df[,4]>, notes <chr>,
#> #   status <chr>, data_policy_beltfish <chr>, data_policy_benthiclit <chr>,
#> #   data_policy_benthicpit <chr>, data_policy_habitatcomplexity <chr>,
#> #   data_policy_bleachingqc <chr>, data_policy_benthicpqt <chr>,
#> #   data_policy_macroinvertebrate <chr>, includes_gfcr <lgl>, …
```

As well as all sites:

``` r
mermaid_get_sites()
#> # A tibble: 2,153 × 12
#>    exposure     reef_zone reef_type country id    name  latitude longitude notes
#>    <chr>        <chr>     <chr>     <chr>   <chr> <chr>    <dbl>     <dbl> <chr>
#>  1 very shelte… back reef atoll     Bangla… 0415… mysi…     -1        -1   <NA> 
#>  2 very shelte… back reef atoll     Canada  547d… bulk…     47.5     -81.8 <NA> 
#>  3 very shelte… back reef atoll     Canada  874c… dupe…      1         1   <NA> 
#>  4 very shelte… back reef atoll     Canada  42d6… dupe…      1         1   <NA> 
#>  5 very shelte… back reef atoll     Canada  6cd3… meli…     49      -110   <NA> 
#>  6 very shelte… back reef atoll     Canada  afe4… meli…     49      -110   <NA> 
#>  7 very shelte… back reef atoll     Fiji    af55… BA02     -17.4     178.  Sour…
#>  8 very shelte… back reef atoll     Fiji    9c2f… BA02     -17.4     178.  Sour…
#>  9 very shelte… back reef atoll     Fiji    03e5… BA03     -17.4     178.  <NA> 
#> 10 very shelte… back reef atoll     Fiji    a9c8… BA03     -17.4     178.  <NA> 
#> # ℹ 2,143 more rows
#> # ℹ 3 more variables: project <chr>, created_on <chr>, updated_on <chr>
```

And all managements:

``` r
mermaid_get_managements()
#> # A tibble: 1,682 × 18
#>    id                    name  rules  size name_secondary est_year notes no_take
#>    <chr>                 <chr> <chr> <dbl> <chr>             <int> <chr> <lgl>  
#>  1 a10d4cfc-dac4-4fde-b… 123   Peri…   123 <NA>               1979 <NA>  FALSE  
#>  2 9168b477-3515-4627-b… 678j… No T…     7 <NA>                 NA <NA>  TRUE   
#>  3 d70de8ff-079a-4962-8… 678j… No T…     7 <NA>                 NA <NA>  TRUE   
#>  4 79fb2463-7a02-47c7-a… 678j… No T…     7 <NA>                 NA <NA>  TRUE   
#>  5 9788b7f9-d28d-4d65-9… 678j… No T…     7 <NA>                 NA <NA>  TRUE   
#>  6 9b1df02a-fd47-440e-9… 678j… No T…     7 <NA>                 NA <NA>  TRUE   
#>  7 3ad9eb1e-9727-4f82-8… 678j… No T…     7 <NA>                 NA <NA>  TRUE   
#>  8 0e9aa677-7105-420f-b… 678j… No T…     7 <NA>                 NA <NA>  TRUE   
#>  9 9f54940e-37aa-4548-8… 678j… No T…     7 <NA>                 NA <NA>  TRUE   
#> 10 9c204b0d-ae85-4bf7-b… 678j… No T…     7 <NA>                 NA <NA>  TRUE   
#> # ℹ 1,672 more rows
#> # ℹ 10 more variables: periodic_closure <lgl>, open_access <lgl>,
#> #   size_limits <lgl>, gear_restriction <lgl>, species_restriction <lgl>,
#> #   access_restriction <lgl>, compliance <chr>, parties <chr>,
#> #   created_on <chr>, updated_on <chr>
```

There is additional data available from the MERMAID API, both related to
specific projects and not. If you think you’ll need to use these, please
see `mermaid_get_endpoint()` and `mermaid_get_project_endpoint()`.

This is a small sample of the wealth of data that’s available on your
MERMAID projects, and on the ecosystem as a whole! Please explore the
[package website](https://data-mermaid.github.io/mermaidr/) for more.
