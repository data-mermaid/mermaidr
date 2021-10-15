
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
guide](https://data-mermaid.github.io/mermaidr/articles/articles/new_to_r.html)
is a great place to start! If you find yourself stuck, please don’t
hesitate to [ask for
help](https://data-mermaid.github.io/mermaidr/articles/articles/getting_help.html).

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
#> # A tibble: 12 × 14
#>    id      name    countries num_sites tags     notes    status data_policy_bel…
#>    <chr>   <chr>   <chr>         <int> <chr>    <chr>    <chr>  <chr>           
#>  1 02e691… TWP Gi… Indonesia        14 "WCS In… ""       Open   Private         
#>  2 170e71… 2018_V… Fiji             10 "WCS Fi… "This i… Open   Private         
#>  3 2d6cee… WCS Mo… Mozambiq…        74 "WCS Mo… "Databa… Open   Private         
#>  4 3a9ecb… Aceh J… Indonesia        18 "Vibran… ""       Open   Private         
#>  5 408067… Madaga… Madagasc…        74 "WCS Ma… "MACMON… Open   Private         
#>  6 4d23d2… Madaga… Madagasc…        16 "WCS Ma… "Monito… Open   Public Summary  
#>  7 507d1a… Karimu… Indonesia        43 "Vibran… ""       Open   Private         
#>  8 5679ef… Madaga… Madagasc…        33 "WCS Ma… ""       Open   Public Summary  
#>  9 75ef7a… Kubula… Fiji             78 "WCS Fi… ""       Open   Private         
#> 10 9de827… XPDC K… Indonesia        37 ""       "XPDC K… Open   Private         
#> 11 a1b7ff… Great … Fiji             76 "Fiji M… ""       Open   Private         
#> 12 e1efb1… 2016_N… Fiji              8 "WCS Fi… "Namena… Open   Private         
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
#> # A tibble: 80 × 75
#>    project       tags    country site     latitude longitude reef_type reef_zone
#>    <chr>         <chr>   <chr>   <chr>       <dbl>     <dbl> <chr>     <chr>    
#>  1 WCS Mozambiq… WCS Mo… Mozamb… Aquarium    -21.8      35.5 barrier   back reef
#>  2 WCS Mozambiq… WCS Mo… Mozamb… Babylon     -11.0      40.7 fringing  fore reef
#>  3 WCS Mozambiq… WCS Mo… Mozamb… Baluba      -22.0      35.5 patch     fore reef
#>  4 WCS Mozambiq… WCS Mo… Mozamb… Barreir…    -26.0      32.9 barrier   back reef
#>  5 WCS Mozambiq… WCS Mo… Mozamb… Barreir…    -26.1      32.9 barrier   back reef
#>  6 WCS Mozambiq… WCS Mo… Mozamb… Bunting…    -12.6      40.6 fringing  fore reef
#>  7 WCS Mozambiq… WCS Mo… Mozamb… Bunting…    -12.6      40.6 fringing  fore reef
#>  8 WCS Mozambiq… WCS Mo… Mozamb… Checkers    -26.8      32.9 patch     fore reef
#>  9 WCS Mozambiq… WCS Mo… Mozamb… Coliseum    -12.6      40.6 fringing  fore reef
#> 10 WCS Mozambiq… WCS Mo… Mozamb… Dogtooth    -12.5      40.6 fringing  crest    
#> # … with 70 more rows, and 67 more variables: reef_exposure <chr>, tide <lgl>,
#> #   current <lgl>, visibility <lgl>, aca_geomorphic <chr>, aca_benthic <chr>,
#> #   andrello_grav_nc <dbl>, andrello_sediment <dbl>, andrello_nutrient <dbl>,
#> #   andrello_pop_count <dbl>, andrello_num_ports <dbl>,
#> #   andrello_reef_value <dbl>, andrello_cumul_score <dbl>, beyer_score <dbl>,
#> #   beyer_scorecn <dbl>, beyer_scorecy <dbl>, beyer_scorepfc <dbl>,
#> #   beyer_scoreth <dbl>, beyer_scoretr <dbl>, management <chr>, …
```

If you’d like data related to the **units** of survey (for example, to
transects or quadrats), it’s just a matter of changing `data` to
“sampleunits”:

``` r
wcs_mozambique %>%
  mermaid_get_project_data(method = "fishbelt", data = "sampleunits")
#> # A tibble: 111 × 85
#>    project       tags    country site     latitude longitude reef_type reef_zone
#>    <chr>         <chr>   <chr>   <chr>       <dbl>     <dbl> <chr>     <chr>    
#>  1 WCS Mozambiq… WCS Mo… Mozamb… Aquarium    -21.8      35.5 barrier   back reef
#>  2 WCS Mozambiq… WCS Mo… Mozamb… Aquarium    -21.8      35.5 barrier   back reef
#>  3 WCS Mozambiq… WCS Mo… Mozamb… Babylon     -11.0      40.7 fringing  fore reef
#>  4 WCS Mozambiq… WCS Mo… Mozamb… Baluba      -22.0      35.5 patch     fore reef
#>  5 WCS Mozambiq… WCS Mo… Mozamb… Barreir…    -26.0      32.9 barrier   back reef
#>  6 WCS Mozambiq… WCS Mo… Mozamb… Barreir…    -26.0      32.9 barrier   back reef
#>  7 WCS Mozambiq… WCS Mo… Mozamb… Barreir…    -26.1      32.9 barrier   back reef
#>  8 WCS Mozambiq… WCS Mo… Mozamb… Barreir…    -26.1      32.9 barrier   back reef
#>  9 WCS Mozambiq… WCS Mo… Mozamb… Bunting…    -12.6      40.6 fringing  fore reef
#> 10 WCS Mozambiq… WCS Mo… Mozamb… Bunting…    -12.6      40.6 fringing  fore reef
#> # … with 101 more rows, and 77 more variables: reef_exposure <chr>,
#> #   reef_slope <lgl>, tide <lgl>, current <lgl>, visibility <lgl>,
#> #   relative_depth <lgl>, aca_geomorphic <chr>, aca_benthic <chr>,
#> #   andrello_grav_nc <dbl>, andrello_sediment <dbl>, andrello_nutrient <dbl>,
#> #   andrello_pop_count <dbl>, andrello_num_ports <dbl>,
#> #   andrello_reef_value <dbl>, andrello_cumul_score <dbl>, beyer_score <dbl>,
#> #   beyer_scorecn <dbl>, beyer_scorecy <dbl>, beyer_scorepfc <dbl>, …
```

And raw observations are available by changing it to “observations”:

``` r
wcs_mozambique %>%
  mermaid_get_project_data(method = "fishbelt", data = "observations")
#> # A tibble: 2,714 × 65
#>    project         tags     country site  latitude longitude reef_type reef_zone
#>    <chr>           <chr>    <chr>   <chr>    <dbl>     <dbl> <chr>     <chr>    
#>  1 WCS Mozambique… WCS Moz… Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  2 WCS Mozambique… WCS Moz… Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  3 WCS Mozambique… WCS Moz… Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  4 WCS Mozambique… WCS Moz… Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  5 WCS Mozambique… WCS Moz… Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  6 WCS Mozambique… WCS Moz… Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  7 WCS Mozambique… WCS Moz… Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  8 WCS Mozambique… WCS Moz… Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#>  9 WCS Mozambique… WCS Moz… Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#> 10 WCS Mozambique… WCS Moz… Mozamb… Aqua…    -21.8      35.5 barrier   back reef
#> # … with 2,704 more rows, and 57 more variables: reef_exposure <chr>,
#> #   reef_slope <lgl>, tide <lgl>, current <lgl>, visibility <lgl>,
#> #   relative_depth <lgl>, aca_geomorphic <chr>, aca_benthic <chr>,
#> #   andrello_grav_nc <dbl>, andrello_sediment <dbl>, andrello_nutrient <dbl>,
#> #   andrello_pop_count <dbl>, andrello_num_ports <dbl>,
#> #   andrello_reef_value <dbl>, andrello_cumul_score <dbl>, beyer_score <dbl>,
#> #   beyer_scorecn <dbl>, beyer_scorecy <dbl>, beyer_scorepfc <dbl>, …
```

For more details on accessing project data, please see the [Accessing
Project
Data](https://data-mermaid.github.io/mermaidr/articles/articles/detailed_usage.html)
article.

You may also want to access data that is not related to projects. To
access this data, you do not need to authenticate R with MERMAID.

For example, you can pull reference data (the names and information of
the fish and benthic attributes you can choose in MERMAID), using
`mermaid_get_reference()`:

``` r
mermaid_get_reference(reference = "fishfamilies")
#> # A tibble: 162 × 9
#>    id    name  status biomass_constan… biomass_constan… biomass_constan… regions
#>    <chr> <chr> <chr>             <dbl>            <dbl>            <dbl> <chr>  
#>  1 0091… Kyph… Open            0.0193              3.03            0.986 Easter…
#>  2 00b6… Mugi… Open            0.0166              2.94            0.974 Easter…
#>  3 00f4… Zena… Open            0.00427             3.02            1     Easter…
#>  4 0226… Sphy… Open            0.00448             3.11            1     Easter…
#>  5 0880… Labr… Open            0.0120              3.04            0.997 Easter…
#>  6 0aff… Scom… Open            0.0111              3.03            0.988 Easter…
#>  7 0b69… Ophi… Open            0.00139             2.93            1     Easter…
#>  8 0d99… Albu… Open            0.0105              2.99            1     Tropic…
#>  9 0e5b… Hemi… Open            0.0373              3.16            0.99  Easter…
#> 10 1513… Serr… Open            0.0136              3.03            0.997 Easter…
#> # … with 152 more rows, and 2 more variables: created_on <chr>,
#> #   updated_on <chr>
```

Using this function, you can access the fish family, fish genera, fish
species, and benthic attributes references by changing the `reference`
argument.

You can also get a list of *all* projects (not just your own):

``` r
mermaid_get_projects()
#> # A tibble: 129 × 14
#>    id      name    countries  num_sites tags    notes    status data_policy_bel…
#>    <chr>   <chr>   <chr>          <int> <chr>   <chr>    <chr>  <chr>           
#>  1 00673b… TPK Gi… "Indonesi…        15 "WCS I… ""       Open   Private         
#>  2 01bbe4… Madaga… "Madagasc…        12 "WCS M… "Survey… Open   Private         
#>  3 02e691… TWP Gi… "Indonesi…        14 "WCS I… ""       Open   Private         
#>  4 07df6a… Cender… "Indonesi…        36 "Cende… ""       Open   Private         
#>  5 0b39fe… Open C… "Indonesi…         2 "WCS I… "This i… Open   Private         
#>  6 0c000a… 2019_O… "Fiji"            18 "WCS F… ""       Open   Private         
#>  7 0c1668… REEFol… ""                 0 ""      ""       Open   Public Summary  
#>  8 0f1703… what    ""                 0 ""      ""       Open   Public Summary  
#>  9 124b91… Sam     ""                 0 ""      ""       Open   Private         
#> 10 1277ef… Taka B… "Indonesi…        39 "WCS I… ""       Open   Public Summary  
#> # … with 119 more rows, and 6 more variables: data_policy_benthiclit <chr>,
#> #   data_policy_benthicpit <chr>, data_policy_habitatcomplexity <chr>,
#> #   data_policy_bleachingqc <chr>, created_on <chr>, updated_on <chr>
```

As well as all sites:

``` r
mermaid_get_sites()
#> # A tibble: 2,543 × 13
#>    id       name  notes project   latitude longitude country reef_type reef_zone
#>    <chr>    <chr> <chr> <chr>        <dbl>     <dbl> <chr>   <chr>     <chr>    
#>  1 02355d6… BA09  ""    a1b7ff1f…    -17.4      178. Fiji    atoll     back reef
#>  2 03e5576… BA03  ""    89f2d43e…    -17.4      178. Fiji    atoll     back reef
#>  3 0879390… BA16  ""    a1b7ff1f…    -17.2      178. Fiji    atoll     back reef
#>  4 19258ea… BA15  ""    a1b7ff1f…    -17.2      178. Fiji    atoll     back reef
#>  5 19e6088… YA02  ""    a1b7ff1f…    -17.0      177. Fiji    atoll     back reef
#>  6 20aeb13… BA11  ""    a1b7ff1f…    -17.3      178. Fiji    atoll     back reef
#>  7 2831d61… BA06  ""    89f2d43e…    -17.4      178. Fiji    atoll     back reef
#>  8 2a4625f… BA04  ""    89f2d43e…    -17.4      178. Fiji    atoll     back reef
#>  9 2af472d… BA12  ""    a1b7ff1f…    -17.3      178. Fiji    atoll     back reef
#> 10 2c31d8c… BA05  ""    89f2d43e…    -17.4      178. Fiji    atoll     back reef
#> # … with 2,533 more rows, and 4 more variables: exposure <chr>,
#> #   predecessor <chr>, created_on <chr>, updated_on <chr>
```

And all managements:

``` r
mermaid_get_managements()
#> # A tibble: 695 × 17
#>    id      name    name_secondary  rules notes est_year no_take periodic_closure
#>    <chr>   <chr>   <chr>           <chr> <chr>    <int> <lgl>   <lgl>           
#>  1 0031d4… Matafo… "Fish Habitat … No T… ""        2018 TRUE    FALSE           
#>  2 004bb2… Pulau … ""              No T… ""          NA TRUE    FALSE           
#>  3 00c920… Lape B… "Special Manag… Peri… ""        2017 FALSE   TRUE            
#>  4 0118e3… Sustai… "Perikanan Ber… Gear… ""        2020 FALSE   FALSE           
#>  5 02479d… Protec… "Zona Perlindu… No T… ""        2015 TRUE    FALSE           
#>  6 02600e… Dawa_L… ""              Acce… ""          NA FALSE   FALSE           
#>  7 029852… Haafev… "Fish Habitat … No T… ""        2007 TRUE    FALSE           
#>  8 02cd9d… Kaibu_… ""              Peri… ""        2017 FALSE   TRUE            
#>  9 02e546… VIR3    ""              No T… ""        2012 TRUE    FALSE           
#> 10 03bab6… VIR9    ""              No T… ""        2016 TRUE    FALSE           
#> # … with 685 more rows, and 9 more variables: open_access <lgl>,
#> #   size_limits <lgl>, gear_restriction <lgl>, species_restriction <lgl>,
#> #   compliance <chr>, predecessor <chr>, parties <chr>, created_on <chr>,
#> #   updated_on <chr>
```

There is additional data available from the MERMAID API, both related to
specific projects and not. If you think you’ll need to use these, please
see `mermaid_get_endpoint()` and `mermaid_get_project_endpoint()`.

This is a small sample of the wealth of data that’s available on your
MERMAID projects, and on the ecosystem as a whole! Please explore the
[package website](https://data-mermaid.github.io/mermaidr/) for more.
