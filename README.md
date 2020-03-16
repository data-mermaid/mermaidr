
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mermaidr

<!-- badges: start -->

<!-- badges: end -->

The goal of `mermaidr` is to access [MERMAID
Collect](https://collect.datamermaid.org/) data directly from R. The
package is presently set to access the [dev MERMAID
Collect](https://dev-collect.datamermaid.org/) only.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mermaidr/mermaidr@package")
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

To access the unauthenticated API endpoints, use
`mermaid_get_endpoint()`. The results will return as a `tibble.` The
following endpoints are available: “benthicattributes”,
“fishattributes”, “fishfamilies”, “fishgenera”, “fishspecies”,
“managements”, “projects”, “sites”.

For example,

``` r
library(mermaidr)

mermaid_get_endpoint("sites")
#> # A tibble: 50 x 17
#>    id    name  notes project latitude longitude country_id country_name
#>    <chr> <chr> <chr> <chr>      <dbl>     <dbl> <chr>      <chr>       
#>  1 c7e2… 1201  "Pul… 988e75…    -2.02      134. c570ff86-… Indonesia   
#>  2 95ad… 1201  "Pul… 3d6edb…    -2.02      134. c570ff86-… Indonesia   
#>  3 e981… 1201  "Pul… c29a9e…    -2.02      134. c570ff86-… Indonesia   
#>  4 9fe1… 1201  "Pul… 07df6a…    -2.02      134. c570ff86-… Indonesia   
#>  5 6e7f… 1201  "Pul… c08ff9…    -2.02      134. c570ff86-… Indonesia   
#>  6 d74d… 1202  "Nap… 3d6edb…    -2.91      135. c570ff86-… Indonesia   
#>  7 a467… 1202  "Nap… 07df6a…    -2.91      135. c570ff86-… Indonesia   
#>  8 46ac… 1203  "Pul… 07df6a…    -3.06      135. c570ff86-… Indonesia   
#>  9 b0fd… 1204  "Kwa… 07df6a…    -3.22      135. c570ff86-… Indonesia   
#> 10 5a62… 1205  "Pul… 07df6a…    -3.10      135. c570ff86-… Indonesia   
#> # … with 40 more rows, and 9 more variables: reef_type_id <chr>,
#> #   reef_type_name <chr>, reef_zone_id <chr>, reef_zone_name <chr>,
#> #   exposure_id <chr>, exposure_name <chr>, predecessor <chr>,
#> #   created_on <chr>, updated_on <chr>
```

By default, the function returns 50 results - to get more (or less\!),
use the `limit` argument:

``` r
mermaid_get_endpoint("managements", limit = 5)
#> # A tibble: 5 x 19
#>   id    name  name_secondary project project_name rules notes est_year no_take
#>   <chr> <chr> <chr>          <chr>   <chr>        <chr> <chr>    <int> <lgl>  
#> 1 2374… Amba… ""             5679ef… Madagascar … No T… ""        2013 TRUE   
#> 2 bbe7… Amba… ""             3d6edb… WILELIFE OC… No T… ""        2013 TRUE   
#> 3 704e… Amba… ""             c29a9e… tesst adc    No T… ""        2013 TRUE   
#> 4 d007… Amba… ""             c29a9e… tesst adc    No T… ""        2013 TRUE   
#> 5 23c6… Amba… ""             408067… Madagascar … No T… ""        2013 TRUE   
#> # … with 10 more variables: periodic_closure <lgl>, open_access <lgl>,
#> #   size_limits <lgl>, gear_restriction <lgl>, species_restriction <lgl>,
#> #   compliance <chr>, predecessor <chr>, parties <list>, created_on <chr>,
#> #   updated_on <chr>
```

For specifically listing projects, there is a wrapper function
`mermaid_list_projects()`:

``` r
mermaid_list_projects(limit = 5)
#> # A tibble: 5 x 14
#>   id    name  countries num_sites tags  notes status data_policy_bel…
#>   <chr> <chr> <list>        <int> <lis> <chr>  <int>            <int>
#> 1 60dd… 2013… <chr [1]>         6 <chr… ""        90               10
#> 2 7376… 2014… <chr [1]>        24 <chr… "Thi…     90               10
#> 3 ac93… 2016… <chr [1]>        24 <chr… "Thi…     90               10
#> 4 e1ef… 2016… <chr [1]>         8 <chr… "Nam…     90               10
#> 5 d549… 2017… <chr [1]>        31 <chr… "Thi…     90               10
#> # … with 6 more variables: data_policy_benthiclit <int>,
#> #   data_policy_benthicpit <int>, data_policy_habitatcomplexity <int>,
#> #   data_policy_bleachingqc <int>, created_on <chr>, updated_on <chr>
```

This will list all (as many as `limit`) projects.

To specifically access projects that you *have access to*, use
`mermaid_list_my_projects()`:

``` r
mermaid_list_my_projects(limit = 1)
#> # A tibble: 1 x 14
#>   id    name  countries num_sites tags  notes status data_policy_bel…
#>   <chr> <chr> <list>        <int> <lis> <chr>  <int>            <int>
#> 1 ac93… 2016… <chr [1]>        24 <chr… This…     90               10
#> # … with 6 more variables: data_policy_benthiclit <int>,
#> #   data_policy_benthicpit <int>, data_policy_habitatcomplexity <int>,
#> #   data_policy_bleachingqc <int>, created_on <chr>, updated_on <chr>
```

This will return a list of projects that you have access to in Collect.

### Accessing project data

You will be able to access data from a specific project, provided that
you have access to it in the Collect app. To access data for a project,
you can either use a project from `mermaid_list_my_projects()` (as
above), a `project_id` directly, or a project from
`mermaid_search_projects()`.

For example:

``` r
mermaidr_project <- mermaid_search_projects(name = "Sharla test")

mermaidr_project
#> # A tibble: 1 x 14
#>   id    name  countries num_sites tags  notes status data_policy_bel…
#>   <chr> <chr> <list>        <int> <lis> <chr>  <int>            <int>
#> 1 2c0c… Shar… <chr [2]>         2 <lis… ""        80               50
#> # … with 6 more variables: data_policy_benthiclit <int>,
#> #   data_policy_benthicpit <int>, data_policy_habitatcomplexity <int>,
#> #   data_policy_bleachingqc <int>, created_on <chr>, updated_on <chr>
```

returns a single project with the name “Sharla test”.

You can also search projects by country or tag:

``` r
mermaid_search_projects(country = "Fiji")
#> # A tibble: 37 x 14
#>    id    name  countries num_sites tags  notes status data_policy_bel…
#>    <chr> <chr> <list>        <int> <lis> <chr>  <int>            <int>
#>  1 60dd… 2013… <chr [1]>         6 <chr… ""        90               10
#>  2 7376… 2014… <chr [1]>        24 <chr… "Thi…     90               10
#>  3 ac93… 2016… <chr [1]>        24 <chr… "Thi…     90               10
#>  4 e1ef… 2016… <chr [1]>         8 <chr… "Nam…     90               10
#>  5 d549… 2017… <chr [1]>        31 <chr… "Thi…     90               10
#>  6 c0ba… 2018… <chr [1]>        22 <chr… "Thi…     90               10
#>  7 170e… 2018… <chr [1]>        10 <chr… "Thi…     90               10
#>  8 95e0… 2019… <chr [1]>        44 <chr… ""        90               10
#>  9 d065… 2019… <chr [1]>        31 <chr… "Ble…     90               10
#> 10 6c6c… 2019… <chr [1]>        18 <chr… "Mac…     90               10
#> # … with 27 more rows, and 6 more variables: data_policy_benthiclit <int>,
#> #   data_policy_benthicpit <int>, data_policy_habitatcomplexity <int>,
#> #   data_policy_bleachingqc <int>, created_on <chr>, updated_on <chr>
```

and if you only want to search *your* projects, pass your token to the
function:

``` r
mermaid_search_projects(country = "Fiji", token = mermaid_token())
#> # A tibble: 3 x 14
#>   id    name  countries num_sites tags  notes status data_policy_bel…
#>   <chr> <chr> <list>        <int> <lis> <chr>  <int>            <int>
#> 1 ac93… 2016… <chr [1]>        24 <chr… "Thi…     90               10
#> 2 95e0… 2019… <chr [1]>        44 <chr… ""        90               10
#> 3 6c6c… 2019… <chr [1]>        18 <chr… "Mac…     90               10
#> # … with 6 more variables: data_policy_benthiclit <int>,
#> #   data_policy_benthicpit <int>, data_policy_habitatcomplexity <int>,
#> #   data_policy_bleachingqc <int>, created_on <chr>, updated_on <chr>
```

You can use this to access an endpoint for the project, using
`get_mermaid_project_endpoint()`. The following project endpoints are
available: “beltfishtransectmethods”, “beltfishes”,
“benthiclittransectmethods”, “benthicpittransectmethods”,
“benthicpits”, “collectrecords”, “habitatcomplexities”,
“obsbenthiclits”, “obsbenthicpits”, “obshabitatcomplexities”,
“obstransectbeltfishs”, “managements”, “observers”,
“project\_profiles”, “sampleevents”, “sites”.

At this point, you will have to authenticate to the Collect app. R will
help you do this automatically by opening a browser window for you to
log in to Collect, either via Google sign-in or username and password -
however you normally do\!

Once you’ve logged in, come back to R. Your login credentials will be
stored for a day, until they expire, and you will need to login again.
The package handles the expiration for you, so just log in again when
prompted.

#### Raw endpoints

The following endpoints contain raw data: “beltfishtransectmethods”,
“beltfishes”, “benthiclittransectmethods”,
“benthicpittransectmethods”, “benthicpits”, “collectrecords”,
“habitatcomplexities”, “obsbenthiclits”, “obsbenthicpits”,
“obshabitatcomplexities”, “obstransectbeltfishs”, “managements”,
“observers”, “project\_profiles”, “sampleevents”, “sites”,
“beltfishes/obstransectbeltfishes/”, “beltfishes/sampleunits/”, and
“beltfishes/sampleevents/”.

Cleaner endpoints are covered in the next section. These are:
“beltfishes/obstransectbeltfishes/”, “beltfishes/sampleunits/”, and
“beltfishes/sampleevents/”.

For example, to see the sites in this project:

``` r
mermaid_get_project_endpoint(mermaidr_project, "sites")
#> # A tibble: 2 x 17
#>   id    name  notes project latitude longitude country_id country_name
#>   <chr> <chr> <chr> <chr>      <dbl>     <dbl> <chr>      <chr>       
#> 1 7465… 1201  "Pul… 2c0c98…    -2.02     134.  c570ff86-… Indonesia   
#> 2 7c02… Amba… ""    2c0c98…   -13.6       47.8 daa14665-… Madagascar  
#> # … with 9 more variables: reef_type_id <chr>, reef_type_name <chr>,
#> #   reef_zone_id <chr>, reef_zone_name <chr>, exposure_id <chr>,
#> #   exposure_name <chr>, predecessor <chr>, created_on <chr>, updated_on <chr>
```

You can also use the `project_id` directly to access data from a
project, without having to search for it first. This may be handy since
the `project_id` is directly available from the URL when using the
collect
app.

``` r
mermaid_get_project_endpoint("2c0c9857-b11c-4b82-b7ef-e9b383d1233c", "managements")
#> # A tibble: 1 x 17
#>   id    name  name_secondary project notes est_year no_take periodic_closure
#>   <chr> <chr> <chr>          <chr>   <chr> <lgl>    <lgl>   <lgl>           
#> 1 0c2b… test  ""             2c0c98… ""    NA       FALSE   TRUE            
#> # … with 9 more variables: open_access <lgl>, size_limits <lgl>,
#> #   gear_restriction <lgl>, species_restriction <lgl>, compliance <lgl>,
#> #   predecessor <lgl>, parties <list>, created_on <chr>, updated_on <chr>
```

If you want to access data from the same project multiple times within a
session, it may be useful to set the default project, rather than having
to supply it every time. You can do this using
`mermaid_set_default_project()`. Then, you can just supply the endpoint,
and the default project is used.

``` r
mermaid_set_default_project(mermaidr_project)
mermaid_get_project_endpoint(endpoint = "beltfishes")
#> # A tibble: 0 x 4
#> # … with 4 variables: id <lgl>, transect <lgl>, created_on <lgl>,
#> #   updated_on <lgl>
```

To get data for *all* endpoints associated with a project, use
`mermaid_get_all_project_endpoints()`. This will return a list of
tibbles, one for each endpoint.

``` r
all_endpoints <- mermaid_get_all_project_endpoints()

names(all_endpoints)
#>  [1] "beltfishtransectmethods"   "beltfishes"               
#>  [3] "benthiclittransectmethods" "benthicpittransectmethods"
#>  [5] "benthicpits"               "benthictransects"         
#>  [7] "collectrecords"            "fishbelttransects"        
#>  [9] "habitatcomplexities"       "obsbenthiclits"           
#> [11] "obsbenthicpits"            "obshabitatcomplexities"   
#> [13] "obstransectbeltfishs"      "managements"              
#> [15] "observers"                 "project_profiles"         
#> [17] "sampleevents"              "sites"

all_endpoints
#> $beltfishtransectmethods
#> # A tibble: 0 x 8
#> # … with 8 variables: id <lgl>, transect <lgl>, sample_event <lgl>,
#> #   fishbelt_transect <lgl>, observers <lgl>, obs_belt_fishes <lgl>,
#> #   created_on <lgl>, updated_on <lgl>
#> 
#> $beltfishes
#> # A tibble: 0 x 4
#> # … with 4 variables: id <lgl>, transect <lgl>, created_on <lgl>,
#> #   updated_on <lgl>
#> 
#> $benthiclittransectmethods
#> # A tibble: 0 x 0
#> 
#> $benthicpittransectmethods
#> # A tibble: 0 x 9
#> # … with 9 variables: id <lgl>, transect <lgl>, interval_size <lgl>,
#> #   sample_event <lgl>, benthic_transect <lgl>, observers <lgl>,
#> #   obs_benthic_pits <lgl>, created_on <lgl>, updated_on <lgl>
#> 
#> $benthicpits
#> # A tibble: 0 x 5
#> # … with 5 variables: id <lgl>, transect <lgl>, interval_size <lgl>,
#> #   created_on <lgl>, updated_on <lgl>
#> 
#> $benthictransects
#> # A tibble: 0 x 0
#> 
#> $collectrecords
#> # A tibble: 0 x 8
#> # … with 8 variables: id <lgl>, project <lgl>, profile <lgl>, stage <lgl>,
#> #   data <lgl>, validations <lgl>, created_on <lgl>, updated_on <lgl>
#> 
#> $fishbelttransects
#> # A tibble: 0 x 0
#> 
#> $habitatcomplexities
#> # A tibble: 0 x 5
#> # … with 5 variables: id <lgl>, transect <lgl>, interval_size <lgl>,
#> #   created_on <lgl>, updated_on <lgl>
#> 
#> $obsbenthiclits
#> # A tibble: 0 x 0
#> 
#> $obsbenthicpits
#> # A tibble: 0 x 10
#> # … with 10 variables: id <lgl>, data <lgl>, interval <lgl>, include <lgl>,
#> #   notes <lgl>, benthicpit <lgl>, attribute <lgl>, growth_form <lgl>,
#> #   created_on <lgl>, updated_on <lgl>
#> 
#> $obshabitatcomplexities
#> # A tibble: 0 x 9
#> # … with 9 variables: id <lgl>, data <lgl>, interval <lgl>, include <lgl>,
#> #   notes <lgl>, habitatcomplexity <lgl>, score <lgl>, created_on <lgl>,
#> #   updated_on <lgl>
#> 
#> $obstransectbeltfishs
#> # A tibble: 0 x 11
#> # … with 11 variables: id <lgl>, data <lgl>, size <lgl>, count <lgl>,
#> #   include <lgl>, notes <lgl>, beltfish <lgl>, fish_attribute <lgl>,
#> #   size_bin <lgl>, created_on <lgl>, updated_on <lgl>
#> 
#> $managements
#> # A tibble: 1 x 17
#>   id    name  name_secondary project notes est_year no_take periodic_closure
#>   <chr> <chr> <chr>          <chr>   <chr> <lgl>    <lgl>   <lgl>           
#> 1 0c2b… test  ""             2c0c98… ""    NA       FALSE   TRUE            
#> # … with 9 more variables: open_access <lgl>, size_limits <lgl>,
#> #   gear_restriction <lgl>, species_restriction <lgl>, compliance <lgl>,
#> #   predecessor <lgl>, parties <list>, created_on <chr>, updated_on <chr>
#> 
#> $observers
#> # A tibble: 0 x 7
#> # … with 7 variables: id <lgl>, profile <lgl>, profile_name <lgl>, rank <lgl>,
#> #   transectmethod <lgl>, created_on <lgl>, created_by <lgl>
#> 
#> $project_profiles
#> # A tibble: 1 x 9
#>   id    profile profile_name project is_collector is_admin  role created_on
#>   <chr> <chr>   <chr>        <chr>   <lgl>        <lgl>    <int> <chr>     
#> 1 b9e9… 746464… Sharla Gelf… 2c0c98… TRUE         TRUE        90 2020-02-0…
#> # … with 1 more variable: updated_on <chr>
#> 
#> $sampleevents
#> # A tibble: 0 x 15
#> # … with 15 variables: id <lgl>, depth <lgl>, data <lgl>, sample_date <lgl>,
#> #   sample_time <lgl>, notes <lgl>, created_by <lgl>, site <lgl>,
#> #   management <lgl>, visibility <lgl>, current <lgl>, relative_depth <lgl>,
#> #   tide <lgl>, created_on <lgl>, updated_on <lgl>
#> 
#> $sites
#> # A tibble: 2 x 17
#>   id    name  notes project latitude longitude country_id country_name
#>   <chr> <chr> <chr> <chr>      <dbl>     <dbl> <chr>      <chr>       
#> 1 7465… 1201  "Pul… 2c0c98…    -2.02     134.  c570ff86-… Indonesia   
#> 2 7c02… Amba… ""    2c0c98…   -13.6       47.8 daa14665-… Madagascar  
#> # … with 9 more variables: reef_type_id <chr>, reef_type_name <chr>,
#> #   reef_zone_id <chr>, reef_zone_name <chr>, exposure_id <chr>,
#> #   exposure_name <chr>, predecessor <chr>, created_on <chr>, updated_on <chr>
```

Keep in mind that the default `limit` is 50, and should be increased if
you want more records from each endpoint.

#### Clean Endpoints

The “clean” endpoints currently available are for beltfish records:
“beltfishes/obstransectbeltfishes/”, “beltfishes/sampleunits/”, and
“beltfishes/sampleevents/”.

You can query them the same way, using `mermaid_get_project_endpoint()`:

“beltfishes/obstransectbeltfishes” are individual observations:

``` r
xpdc <- mermaid_search_projects("XPDC Kei Kecil 2018")

mermaid_get_project_endpoint(xpdc, "beltfishes/obstransectbeltfishes", limit = 5)
#> # A tibble: 5 x 56
#>   id    latitude longitude project_id project_name project_notes contact_link
#>   <chr>    <dbl>     <dbl> <chr>      <chr>        <chr>         <chr>       
#> 1 bb30…    -5.44      133. 9de82789-… XPDC Kei Ke… XPDC Kei Kec… https://dat…
#> 2 e4c2…    -5.44      133. 9de82789-… XPDC Kei Ke… XPDC Kei Kec… https://dat…
#> 3 8ee0…    -5.44      133. 9de82789-… XPDC Kei Ke… XPDC Kei Kec… https://dat…
#> 4 39ba…    -5.44      133. 9de82789-… XPDC Kei Ke… XPDC Kei Kec… https://dat…
#> 5 258f…    -5.44      133. 9de82789-… XPDC Kei Ke… XPDC Kei Kec… https://dat…
#> # … with 49 more variables: tags <lgl>, site_id <chr>, site_name <chr>,
#> #   site_notes <chr>, country_id <chr>, country_name <chr>, reef_type <chr>,
#> #   reef_zone <chr>, reef_exposure <chr>, management_id <chr>,
#> #   management_name <chr>, management_name_secondary <chr>,
#> #   management_est_year <lgl>, management_size <lgl>, management_parties <lgl>,
#> #   management_compliance <chr>, management_rules <list>,
#> #   management_notes <chr>, sample_event_id <chr>, sample_date <chr>,
#> #   sample_time <chr>, current_name <chr>, tide_name <chr>,
#> #   visibility_name <chr>, depth <dbl>, sample_event_notes <chr>,
#> #   sample_unit_id <chr>, number <int>, label <chr>,
#> #   transect_len_surveyed <int>, reef_slope <lgl>, transect_width <int>,
#> #   observers <list>, fish_family <chr>, fish_genus <chr>, fish_taxon <chr>,
#> #   trophic_group <chr>, trophic_level <dbl>, functional_group <chr>,
#> #   vulnerability <dbl>, biomass_constant_a <dbl>, biomass_constant_b <dbl>,
#> #   biomass_constant_c <dbl>, size_bin <int>, size <dbl>, count <int>,
#> #   biomass_kgha <dbl>, observation_notes <chr>, data_policy_beltfish <chr>
```

“beltfishes/sampleunits/” are aggregated to the sample unit, and contain
total biomass in kg/ha per sample unit, by trophic group:

``` r
mermaid_get_project_endpoint(xpdc, "beltfishes/sampleunits", limit = 5)
#> # A tibble: 5 x 33
#>   id    latitude longitude project_id project_name project_notes contact_link
#>   <lgl>    <dbl>     <dbl> <chr>      <chr>        <chr>         <chr>       
#> 1 NA       -5.44      133. 9de82789-… XPDC Kei Ke… XPDC Kei Kec… https://dat…
#> 2 NA       -5.44      133. 9de82789-… XPDC Kei Ke… XPDC Kei Kec… https://dat…
#> 3 NA       -5.44      133. 9de82789-… XPDC Kei Ke… XPDC Kei Kec… https://dat…
#> 4 NA       -5.44      133. 9de82789-… XPDC Kei Ke… XPDC Kei Kec… https://dat…
#> 5 NA       -5.44      133. 9de82789-… XPDC Kei Ke… XPDC Kei Kec… https://dat…
#> # … with 30 more variables: tags <lgl>, site_id <chr>, site_name <chr>,
#> #   site_notes <chr>, country_id <chr>, country_name <chr>, reef_type <chr>,
#> #   reef_zone <chr>, reef_exposure <chr>, management_id <chr>,
#> #   management_name <chr>, management_name_secondary <chr>,
#> #   management_est_year <lgl>, management_size <lgl>, management_parties <lgl>,
#> #   management_compliance <chr>, management_rules <list>,
#> #   management_notes <chr>, sample_date <chr>, number <int>,
#> #   transect_len_surveyed <int>, reef_slope <lgl>, size_bin <int>,
#> #   biomass_kgha <dbl>, biomass_kgha_by_trophic_group$omnivore <dbl>,
#> #   $piscivore <dbl>, $planktivore <dbl>, $`invertivore-mobile` <dbl>,
#> #   $`herbivore-detritivore` <dbl>, data_policy_beltfish <chr>
```

“beltfishes/sampleevents/” are aggregated to the sample event, and
contain *mean* total biomass in kg/ha per sample event and by trophic
group:

``` r
mermaid_get_project_endpoint(xpdc, "beltfishes/sampleevents", limit = 5)
#> # A tibble: 5 x 29
#>   id    latitude longitude project_id project_name project_notes contact_link
#>   <lgl>    <dbl>     <dbl> <chr>      <chr>        <chr>         <chr>       
#> 1 NA       -5.44      133. 9de82789-… XPDC Kei Ke… XPDC Kei Kec… https://dat…
#> 2 NA       -5.61      132. 9de82789-… XPDC Kei Ke… XPDC Kei Kec… https://dat…
#> 3 NA       -5.58      132. 9de82789-… XPDC Kei Ke… XPDC Kei Kec… https://dat…
#> 4 NA       -5.47      133. 9de82789-… XPDC Kei Ke… XPDC Kei Kec… https://dat…
#> 5 NA       -5.52      132. 9de82789-… XPDC Kei Ke… XPDC Kei Kec… https://dat…
#> # … with 28 more variables: tags <lgl>, site_id <chr>, site_name <chr>,
#> #   site_notes <chr>, country_id <chr>, country_name <chr>, reef_type <chr>,
#> #   reef_zone <chr>, reef_exposure <chr>, management_id <chr>,
#> #   management_name <chr>, management_name_secondary <chr>,
#> #   management_est_year <lgl>, management_size <lgl>, management_parties <lgl>,
#> #   management_compliance <chr>, management_rules <list>,
#> #   management_notes <chr>, sample_date <chr>, biomass_kgha_avg <dbl>,
#> #   biomass_kgha_by_trophic_group_avg$omnivore <dbl>, $piscivore <dbl>,
#> #   $planktivore <dbl>, $`invertivore-mobile` <dbl>,
#> #   $`herbivore-detritivore` <dbl>, $`invertivore-sessile` <dbl>,
#> #   $`herbivore-macroalgae` <dbl>, data_policy_beltfish <chr>
```
