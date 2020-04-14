
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mermaidr

<!-- badges: start -->

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
accessing project specific data. To access a list of projects, sites,
etc, you do not need to be authenticated.

If you would like to authenticate yourself immediately, use
`mermaid_auth()`. This will open your browser to the MERMAID Collect
login. Once you log in, you can go back to R and will be authenticated.

The login credentials expire every 24 hours. Once your credentials are
expired, `mermaidr` will again help you automatically authenticate when
needed.

## Usage

All functions in `mermaidr` are of the form `mermaid_*()`, to make
functions easier to find and use when loaded with other packages\!

To access the unauthenticated API endpoints (e.g. endpoints that are not
related to a specific project), use `mermaid_get_endpoint()`. The
results will return as a `tibble.` The following endpoints are
available: “benthicattributes”, “choices” (a way to lookup values from
IDs), “fishfamilies”, “fishgenera”, “fishspecies”, “fishsizes”,
“managements”, “projects”, “projecttags”, “sites”.

For example,

``` r
library(mermaidr)

mermaid_get_endpoint("sites")
#> # A tibble: 1,746 x 13
#>    id    name  notes project latitude longitude country reef_type reef_zone
#>    <chr> <chr> <chr> <chr>      <dbl>     <dbl> <chr>   <chr>     <chr>    
#>  1 baaa… 1201  "Pul… 841534…    -2.02      134. Indone… fringing  fore reef
#>  2 0a26… 1201  "Pul… 841534…    -2.02      134. Indone… fringing  fore reef
#>  3 c7e2… 1201  "Pul… 988e75…    -2.02      134. Indone… fringing  fore reef
#>  4 e981… 1201  "Pul… c29a9e…    -2.02      134. Indone… fringing  fore reef
#>  5 95ad… 1201  "Pul… 3d6edb…    -2.02      134. Indone… fringing  fore reef
#>  6 6e7f… 1201  "Pul… c08ff9…    -2.02      134. Indone… fringing  fore reef
#>  7 9fe1… 1201  "Pul… 07df6a…    -2.02      134. Indone… fringing  fore reef
#>  8 d74d… 1202  "Nap… 3d6edb…    -2.91      135. Indone… fringing  fore reef
#>  9 a467… 1202  "Nap… 07df6a…    -2.91      135. Indone… fringing  fore reef
#> 10 46ac… 1203  "Pul… 07df6a…    -3.06      135. Indone… fringing  back reef
#> # … with 1,736 more rows, and 4 more variables: exposure <chr>,
#> #   predecessor <chr>, created_on <chr>, updated_on <chr>
```

By default, the function returns all results - to get less, use the
`limit` argument:

``` r
mermaid_get_endpoint("managements", limit = 5)
#> # A tibble: 5 x 17
#>   id    name  name_secondary rules notes est_year no_take periodic_closure
#>   <chr> <chr> <chr>          <chr> <chr> <lgl>    <lgl>   <lgl>           
#> 1 1f89… Adav… ""             Open… ""    NA       FALSE   FALSE           
#> 2 66f9… Ahus… ""             Open… ""    NA       FALSE   FALSE           
#> 3 f1ff… Ahus… ""             Open… ""    NA       FALSE   FALSE           
#> 4 af7a… Ahus… ""             Open… ""    NA       FALSE   FALSE           
#> 5 f012… Ahus… ""             Peri… ""    NA       FALSE   TRUE            
#> # … with 9 more variables: open_access <lgl>, size_limits <lgl>,
#> #   gear_restriction <lgl>, species_restriction <lgl>, compliance <chr>,
#> #   predecessor <lgl>, parties <chr>, created_on <chr>, updated_on <chr>
```

For specifically listing projects, there is a wrapper function
`mermaid_list_projects()`:

``` r
mermaid_list_projects(limit = 5)
#> # A tibble: 5 x 14
#>   id    name  countries num_sites tags  notes status data_policy_bel…
#>   <chr> <chr> <chr>         <int> <chr> <chr> <chr>  <chr>           
#> 1 fe3f… 1000… ""                0 ""    "The… Open   Public Summary  
#> 2 60dd… 2013… "Fiji"           17 "WCS… ""    Open   Private         
#> 3 7376… 2014… "Fiji"           24 "WCS… "Thi… Open   Private         
#> 4 ac93… 2016… "Fiji"           24 "WCS… "Thi… Open   Private         
#> 5 e1ef… 2016… "Fiji"            8 "WCS… "Nam… Open   Private         
#> # … with 6 more variables: data_policy_benthiclit <chr>,
#> #   data_policy_benthicpit <chr>, data_policy_habitatcomplexity <chr>,
#> #   data_policy_bleachingqc <chr>, created_on <chr>, updated_on <chr>
```

This will list all (as many as `limit`) projects. By default, it does
*not* include test projects. To include test projects, set
`include_test_projects = TRUE`.

To specifically access projects that you *have access to*, use
`mermaid_list_my_projects()`:

``` r
mermaid_list_my_projects(limit = 1)
#> # A tibble: 1 x 14
#>   id    name  countries num_sites tags  notes status data_policy_bel…
#>   <chr> <chr> <chr>         <int> <chr> <chr> <chr>  <chr>           
#> 1 d549… 2017… Fiji             31 WCS … This… Open   Private         
#> # … with 6 more variables: data_policy_benthiclit <chr>,
#> #   data_policy_benthicpit <chr>, data_policy_habitatcomplexity <chr>,
#> #   data_policy_bleachingqc <chr>, created_on <chr>, updated_on <chr>
```

This will return a list of projects that you have access to in Collect.
Again, this does not include test projects.

#### Multiple endpoints

To get data from multiple endpoints, pass a vector of endpoints. You
will get a list of tibbles:

``` r
mermaid_get_endpoint(c("managements", "sites"), limit = 1)
#> $managements
#> # A tibble: 1 x 17
#>   id    name  name_secondary rules notes est_year no_take periodic_closure
#>   <chr> <chr> <chr>          <chr> <chr> <lgl>    <lgl>   <lgl>           
#> 1 1f89… Adav… ""             Open… ""    NA       FALSE   FALSE           
#> # … with 9 more variables: open_access <lgl>, size_limits <lgl>,
#> #   gear_restriction <lgl>, species_restriction <lgl>, compliance <lgl>,
#> #   predecessor <lgl>, parties <chr>, created_on <chr>, updated_on <chr>
#> 
#> $sites
#> # A tibble: 1 x 13
#>   id    name  notes project latitude longitude country reef_type reef_zone
#>   <chr> <chr> <chr> <chr>      <dbl>     <dbl> <chr>   <chr>     <chr>    
#> 1 9fe1… 1201  Pula… 07df6a…    -2.02      134. Indone… fringing  fore reef
#> # … with 4 more variables: exposure <chr>, predecessor <lgl>, created_on <chr>,
#> #   updated_on <chr>
```

### Accessing project data

#### Getting projects

You will be able to access data from a specific project, provided that
you have access to it in the Collect app. To access data for a project,
you can either use a project from `mermaid_list_my_projects()` (as
above), a `project_id` directly, or a project from
`mermaid_search_projects()`.

For
example:

``` r
mermaidr_project <- mermaid_search_projects(name = "Sharla test", include_test_projects = TRUE)

mermaidr_project
#> # A tibble: 1 x 14
#>   id    name  countries num_sites tags  notes status data_policy_bel…
#>   <chr> <chr> <chr>         <int> <chr> <chr> <chr>  <chr>           
#> 1 2c0c… Shar… Indonesia         1 ""    "dhf… Test   Public Summary  
#> # … with 6 more variables: data_policy_benthiclit <chr>,
#> #   data_policy_benthicpit <chr>, data_policy_habitatcomplexity <chr>,
#> #   data_policy_bleachingqc <chr>, created_on <chr>, updated_on <chr>
```

returns a single project with the name “Sharla test”.

You can also search projects by country or tag:

``` r
mermaid_search_projects(country = "Fiji")
#> # A tibble: 25 x 14
#>    id    name  countries num_sites tags  notes status data_policy_bel…
#>    <chr> <chr> <chr>         <int> <chr> <chr> <chr>  <chr>           
#>  1 60dd… 2013… Fiji             17 WCS … ""    Open   Private         
#>  2 7376… 2014… Fiji             24 WCS … "Thi… Open   Private         
#>  3 ac93… 2016… Fiji             24 WCS … "Thi… Open   Private         
#>  4 e1ef… 2016… Fiji              8 WCS … "Nam… Open   Private         
#>  5 d549… 2017… Fiji             31 WCS … "Thi… Open   Private         
#>  6 c0ba… 2018… Fiji             22 WCS … "Thi… Open   Private         
#>  7 170e… 2018… Fiji             10 WCS … "Thi… Open   Private         
#>  8 95e0… 2019… Fiji             44 WCS … ""    Open   Private         
#>  9 d065… 2019… Fiji             31 WCS … "Ble… Open   Private         
#> 10 6c6c… 2019… Fiji             18 WCS … "Mac… Open   Private         
#> # … with 15 more rows, and 6 more variables: data_policy_benthiclit <chr>,
#> #   data_policy_benthicpit <chr>, data_policy_habitatcomplexity <chr>,
#> #   data_policy_bleachingqc <chr>, created_on <chr>, updated_on <chr>
```

and if you only want to search *your* projects, pass your token to the
function:

``` r
mermaid_search_projects(country = "Fiji", token = mermaid_token())
#> # A tibble: 1 x 14
#>   id    name  countries num_sites tags  notes status data_policy_bel…
#>   <chr> <chr> <chr>         <int> <chr> <chr> <chr>  <chr>           
#> 1 d549… 2017… Fiji             31 WCS … This… Open   Private         
#> # … with 6 more variables: data_policy_benthiclit <chr>,
#> #   data_policy_benthicpit <chr>, data_policy_habitatcomplexity <chr>,
#> #   data_policy_bleachingqc <chr>, created_on <chr>, updated_on <chr>
```

Note that the country and tag searches search if the countries/tag
fields *contain* that value, since they may not always be exactly what
you expect. For example, to search projects in Tanzania:

``` r
mermaid_search_projects(country = "Tanzania", limit = 1)[["countries"]]
#> [1] "Tanzania, United Republic of"
```

If you need help figuring out what a country is named, use
`mermaid_countries()`, which will list how countries are named in
MERMAID:

``` r
head(mermaid_countries())
#> [1] "Afghanistan"    "Åland Islands"  "Albania"        "Algeria"       
#> [5] "American Samoa" "Andorra"
```

#### Getting the data

You can use this project result to access an endpoint for the project,
using `mermaid_get_project_endpoint()`. Details on the endpoints
available are below.

At this point, you will have to authenticate to the Collect app. R will
help you do this automatically by opening a browser window for you to
log in to Collect, either via Google sign-in or username and password -
however you normally do\!

Once you’ve logged in, come back to R. Your login credentials will be
stored for a day, until they expire, and you will need to login again.
The package handles the expiration for you, so just log in again when
prompted.

#### Aggregated endpoints

The “clean”, aggregated endpoints currently available are for beltfish
records: “beltfishes/obstransectbeltfishes”, “beltfishes/sampleunits”,
and “beltfishes/sampleevents” and Benthic PIT records:
“benthicpits/obstransectbenthicpits”, “benthicpits/sampleunits”, and
“benthicpits/sampleevents”.

##### Observations

The “obs” endpoints show individual observations:

``` r
xpdc <- mermaid_search_projects("XPDC Kei Kecil 2018")

mermaid_get_project_endpoint(xpdc, "beltfishes/obstransectbeltfishes", limit = 5)
#> # A tibble: 5 x 45
#>   project tags  country site  latitude longitude reef_type reef_zone
#>   <chr>   <lgl> <chr>   <chr>    <dbl>     <dbl> <chr>     <chr>    
#> 1 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#> 2 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#> 3 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#> 4 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#> 5 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#> # … with 37 more variables: reef_exposure <chr>, reef_slope <lgl>, tide <chr>,
#> #   current <chr>, visibility <chr>, management <chr>,
#> #   management_secondary <chr>, management_est_year <lgl>,
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
mermaid_get_project_endpoint(xpdc, "benthicpits/obstransectbenthicpits", limit = 5)
#> # A tibble: 5 x 39
#>   project tags  country site  latitude longitude reef_type reef_zone
#>   <chr>   <lgl> <chr>   <chr>    <dbl>     <dbl> <chr>     <chr>    
#> 1 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#> 2 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#> 3 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#> 4 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#> 5 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#> # … with 31 more variables: reef_exposure <chr>, reef_slope <lgl>, tide <lgl>,
#> #   current <chr>, visibility <chr>, management <chr>,
#> #   management_secondary <chr>, management_est_year <lgl>,
#> #   management_size <lgl>, management_parties <lgl>,
#> #   management_compliance <chr>, management_rules <chr>, sample_date <date>,
#> #   sample_time <chr>, depth <dbl>, transect_number <int>,
#> #   transect_length <int>, interval_start <dbl>, interval_size <dbl>,
#> #   label <chr>, observers <chr>, interval <dbl>, benthic_category <chr>,
#> #   benthic_attribute <chr>, growth_form <lgl>, data_policy_benthicpit <chr>,
#> #   project_notes <chr>, site_notes <chr>, management_notes <chr>,
#> #   observation_notes <chr>, contact_link <chr>
```

##### Sample Units

The “sampleunits” endpoints are aggregated to the sample unit level.

Fishbelt sample units contain total biomass in kg/ha per sample unit, by
trophic group:

``` r
mermaid_get_project_endpoint(xpdc, "beltfishes/sampleunits", limit = 5)
#> # A tibble: 5 x 34
#>   project tags  country site  latitude longitude reef_type reef_zone
#>   <chr>   <lgl> <chr>   <chr>    <dbl>     <dbl> <chr>     <chr>    
#> 1 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#> 2 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#> 3 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#> 4 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#> 5 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#> # … with 30 more variables: reef_exposure <chr>, reef_slope <lgl>, tide <chr>,
#> #   current <chr>, visibility <chr>, management <chr>,
#> #   management_secondary <chr>, management_est_year <lgl>,
#> #   management_size <lgl>, management_parties <lgl>,
#> #   management_compliance <chr>, management_rules <chr>, sample_date <date>,
#> #   depth <dbl>, transect_number <int>, size_bin <int>, transect_length <int>,
#> #   transect_width <chr>, biomass_kgha <dbl>,
#> #   biomass_kgha_by_trophic_group$omnivore <dbl>, $piscivore <dbl>,
#> #   $planktivore <dbl>, $`invertivore-mobile` <dbl>,
#> #   $`herbivore-detritivore` <dbl>, data_policy_beltfish <chr>,
#> #   project_notes <chr>, site_notes <chr>, management_notes <chr>, id <chr>,
#> #   contact_link <chr>
```

Benthic PIT sample units contain percent cover per sample unit, by
benthic category:

``` r
mermaid_get_project_endpoint(xpdc, "benthicpits/sampleunits", limit = 5)
#> # A tibble: 5 x 34
#>   project tags  country site  latitude longitude reef_type reef_zone
#>   <chr>   <lgl> <chr>   <chr>    <dbl>     <dbl> <chr>     <chr>    
#> 1 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#> 2 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#> 3 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#> 4 XPDC K… NA    Indone… KE03     -5.61      132. fringing  crest    
#> 5 XPDC K… NA    Indone… KE03     -5.61      132. fringing  crest    
#> # … with 32 more variables: reef_exposure <chr>, reef_slope <lgl>, tide <chr>,
#> #   current <chr>, visibility <chr>, management <chr>,
#> #   management_secondary <chr>, management_est_year <lgl>,
#> #   management_size <lgl>, management_parties <lgl>,
#> #   management_compliance <chr>, management_rules <chr>, sample_date <date>,
#> #   depth <dbl>, transect_number <int>, transect_length <int>,
#> #   interval_start <dbl>, interval_size <dbl>, observers <chr>,
#> #   percent_cover_by_benthic_category$Sand <dbl>, $Rubble <dbl>, $`Hard
#> #   coral` <dbl>, $Macroalgae <dbl>, $`Soft coral` <dbl>, $`Turf algae` <dbl>,
#> #   $`Other invertebrates` <dbl>, data_policy_benthicpit <chr>,
#> #   project_notes <chr>, site_notes <chr>, management_notes <chr>, id <chr>,
#> #   contact_link <chr>
```

##### Sample Events

The “sampleevents” endpoints are aggregated further, to the sample event
level.

Fishbelt sample events contain *mean* total biomass in kg/ha per sample
event and by trophic group:

``` r
mermaid_get_project_endpoint(xpdc, "beltfishes/sampleevents", limit = 5)
#> # A tibble: 5 x 29
#>   project tags  country site  latitude longitude reef_type reef_zone
#>   <chr>   <lgl> <chr>   <chr>    <dbl>     <dbl> <chr>     <chr>    
#> 1 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#> 2 XPDC K… NA    Indone… KE03     -5.61      132. fringing  crest    
#> 3 XPDC K… NA    Indone… KE04     -5.58      132. fringing  crest    
#> 4 XPDC K… NA    Indone… KE05     -5.47      133. fringing  crest    
#> 5 XPDC K… NA    Indone… KE06     -5.52      132. fringing  crest    
#> # … with 27 more variables: reef_exposure <chr>, tide <chr>, current <chr>,
#> #   visibility <chr>, management <chr>, management_secondary <chr>,
#> #   management_est_year <lgl>, management_size <lgl>, management_parties <lgl>,
#> #   management_compliance <chr>, management_rules <chr>, sample_date <date>,
#> #   depth_avg <dbl>, biomass_kgha_avg <dbl>,
#> #   biomass_kgha_by_trophic_group_avg$omnivore <dbl>, $piscivore <dbl>,
#> #   $planktivore <dbl>, $`invertivore-mobile` <dbl>,
#> #   $`herbivore-detritivore` <dbl>, $`invertivore-sessile` <dbl>,
#> #   $`herbivore-macroalgae` <dbl>, data_policy_beltfish <chr>,
#> #   project_notes <chr>, site_notes <chr>, management_notes <chr>,
#> #   sample_unit_count <int>, contact_link <chr>
```

Benthic PIT sample events contain *mean* percent cover per sample event
by benthic
category:

``` r
mermaid_get_project_endpoint(xpdc, "benthicpits/sampleevents", limit = 5)
#> # A tibble: 5 x 28
#>   project tags  country site  latitude longitude reef_type reef_zone
#>   <chr>   <lgl> <chr>   <chr>    <dbl>     <dbl> <chr>     <chr>    
#> 1 XPDC K… NA    Indone… KE02     -5.44      133. fringing  crest    
#> 2 XPDC K… NA    Indone… KE03     -5.61      132. fringing  crest    
#> 3 XPDC K… NA    Indone… KE04     -5.58      132. fringing  crest    
#> 4 XPDC K… NA    Indone… KE05     -5.47      133. fringing  crest    
#> 5 XPDC K… NA    Indone… KE06     -5.52      132. fringing  crest    
#> # … with 26 more variables: reef_exposure <chr>, tide <chr>, current <chr>,
#> #   visibility <chr>, management <chr>, management_secondary <chr>,
#> #   management_est_year <lgl>, management_size <lgl>, management_parties <lgl>,
#> #   management_compliance <chr>, management_rules <chr>, sample_date <date>,
#> #   depth_avg <dbl>, percent_cover_by_benthic_category_avg$Sand <dbl>,
#> #   $Rubble <dbl>, $`Hard coral` <dbl>, $Macroalgae <dbl>, $`Soft coral` <dbl>,
#> #   $`Turf algae` <dbl>, $`Other invertebrates` <dbl>,
#> #   data_policy_benthicpit <chr>, project_notes <chr>, site_notes <chr>,
#> #   management_notes <chr>, sample_unit_count <int>, contact_link <chr>
```

#### Raw project endpoints

The following endpoints contain raw project data:
“beltfishtransectmethods”, “beltfishes”, “benthiclittransectmethods”,
“benthicpittransectmethods”, “benthicpits”, “benthictransects”,
“collectrecords”, “fishbelttransects”, “habitatcomplexities”,
“obsbenthiclits”, “obsbenthicpits”, “obshabitatcomplexities”,
“obstransectbeltfishs”, “managements”, “observers”,
“project\_profiles”, “sampleevents”, “sites”.

For example, to see the sites in this project:

``` r
mermaid_get_project_endpoint(mermaidr_project, "sites")
#> # A tibble: 1 x 12
#>   id    name  notes latitude longitude country reef_type reef_zone exposure
#>   <chr> <chr> <chr>    <dbl>     <dbl> <chr>   <chr>     <chr>     <chr>   
#> 1 7465… 1201  Pula…    -2.02      134. Indone… fringing  fore reef exposed 
#> # … with 3 more variables: predecessor <chr>, created_on <chr>,
#> #   updated_on <chr>
```

### How to specify projects

If you want to access data from the same project multiple times within a
session, it may be useful to set the default project, rather than having
to supply it every time. You can do this using
`mermaid_set_default_project()`. Then, you can just supply the endpoint,
and the default project is used.

``` r
mermaid_set_default_project(xpdc)
mermaid_get_project_endpoint(endpoint = "sites", limit = 1)
#> # A tibble: 1 x 12
#>   id    name  notes latitude longitude country reef_type reef_zone exposure
#>   <chr> <chr> <chr>    <dbl>     <dbl> <chr>   <chr>     <chr>     <chr>   
#> 1 02ff… KE02  ""       -5.44      133. Indone… fringing  crest     exposed 
#> # … with 3 more variables: predecessor <lgl>, created_on <chr>,
#> #   updated_on <chr>
```

You can also use the `project_id` directly to access data from a
project, without having to search for it first. This may be handy since
the `project_id` is directly available from the URL when using the
Collect
app.

``` r
mermaid_get_project_endpoint("9de82789-c38e-462e-a1a8-e02c020c7a35", endpoint = "managements", limit = 1)
#> # A tibble: 1 x 16
#>   id    name  name_secondary notes est_year no_take periodic_closure open_access
#>   <chr> <chr> <chr>          <chr> <lgl>    <lgl>   <lgl>            <lgl>      
#> 1 c19f… Aqua… Use Zone       ""    NA       FALSE   FALSE            FALSE      
#> # … with 8 more variables: size_limits <lgl>, gear_restriction <lgl>,
#> #   species_restriction <lgl>, compliance <lgl>, predecessor <lgl>,
#> #   parties <chr>, created_on <chr>, updated_on <chr>
```

And you can access data from multiple project simultaneously:

``` r
my_projects <- mermaid_list_my_projects()

my_projects
#> # A tibble: 3 x 14
#>   id    name  countries num_sites tags  notes status data_policy_bel…
#>   <chr> <chr> <chr>         <int> <chr> <chr> <chr>  <chr>           
#> 1 d549… 2017… Fiji             31 "WCS… "Thi… Open   Private         
#> 2 3a9e… Aceh… Indonesia        18 "WCS… ""    Open   Private         
#> 3 9de8… XPDC… Indonesia        37 ""    "XPD… Open   Public Summary  
#> # … with 6 more variables: data_policy_benthiclit <chr>,
#> #   data_policy_benthicpit <chr>, data_policy_habitatcomplexity <chr>,
#> #   data_policy_bleachingqc <chr>, created_on <chr>, updated_on <chr>

mermaid_get_project_endpoint(my_projects, "sites", limit = 1)
#> # A tibble: 3 x 13
#>   project id    name  notes latitude longitude country reef_type reef_zone
#>   <chr>   <chr> <chr> <chr>    <dbl>     <dbl> <chr>   <chr>     <chr>    
#> 1 2017_K… f2c8… KD2   ""      -17.3      179.  Fiji    fringing  fore reef
#> 2 Aceh J… 9283… Abah… ""        5.99      95.4 Indone… fringing  fore reef
#> 3 XPDC K… 02ff… KE02  ""       -5.44     133.  Indone… fringing  crest    
#> # … with 4 more variables: exposure <chr>, predecessor <lgl>, created_on <chr>,
#> #   updated_on <chr>
```
