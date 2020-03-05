
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

To access the unauthenticated API endpoints, use
`get_mermaid_endpoints()`. The results will return as a `tibble.` The
following endpoints are available: “benthicattributes”,
“fishattributes”, “fishfamilies”, “fishgenera”, “fishspecies”,
“managements”, “projects”, “sites”.

For example,

``` r
library(mermaidr)

get_mermaid_endpoint("projects")
#> # A tibble: 50 x 14
#>    id    name  countries num_sites tags  notes status data_policy_bel…
#>    <chr> <chr> <list>        <int> <lis> <chr>  <int>            <int>
#>  1 60dd… 2013… <chr [1]>        17 <chr… ""        90               10
#>  2 7376… 2014… <chr [1]>        24 <chr… "Thi…     90               10
#>  3 ac93… 2016… <chr [1]>        24 <chr… "Thi…     90               10
#>  4 e1ef… 2016… <chr [1]>         8 <chr… "Nam…     90               10
#>  5 d549… 2017… <chr [1]>        31 <chr… "Thi…     90               10
#>  6 c0ba… 2018… <chr [1]>        22 <chr… "Thi…     90               10
#>  7 170e… 2018… <chr [1]>        10 <chr… "Thi…     90               10
#>  8 95e0… 2019… <chr [1]>        44 <chr… ""        90               10
#>  9 d065… 2019… <chr [1]>        31 <chr… "Ble…     90               10
#> 10 6c6c… 2019… <chr [1]>        18 <chr… "Mac…     90               10
#> # … with 40 more rows, and 6 more variables: data_policy_benthiclit <int>,
#> #   data_policy_benthicpit <int>, data_policy_habitatcomplexity <int>,
#> #   data_policy_bleachingqc <int>, created_on <chr>, updated_on <chr>
```

By default, the function returns 50 results - to get more (or less\!),
use the `limit` argument:

``` r
get_mermaid_endpoint("managements", limit = 5)
#> # A tibble: 5 x 19
#>   id    name  name_secondary project project_name rules notes est_year no_take
#>   <chr> <chr> <chr>          <chr>   <chr>        <chr> <chr>    <int> <lgl>  
#> 1 d007… Amba… ""             c29a9e… tesst adc    No T… ""        2013 TRUE   
#> 2 704e… Amba… ""             c29a9e… tesst adc    No T… ""        2013 TRUE   
#> 3 bbe7… Amba… ""             3d6edb… WILELIFE OC… No T… ""        2013 TRUE   
#> 4 9ee6… Amba… ""             81e144… FIsh Patch … No T… ""        2013 TRUE   
#> 5 f0be… Amba… ""             c9fc25… Test Project No T… ""        2013 TRUE   
#> # … with 10 more variables: periodic_closure <lgl>, open_access <lgl>,
#> #   size_limits <lgl>, gear_restriction <lgl>, species_restriction <lgl>,
#> #   compliance <chr>, predecessor <chr>, parties <list>, created_on <chr>,
#> #   updated_on <chr>
```

### Accessing project data

You will be able to access data from a specific project, provided that
you have access to it in the Collect app. To access data for a project,
you can either use a project from `get_mermaid_endpoint("projects")` (as
above), a `project_id` directly, or a project from `search_projects()`.

For example:

``` r
mermaidr_project <- search_projects(name = "Sharla test")

mermaidr_project
#> # A tibble: 1 x 14
#>   id    name  countries num_sites tags  notes status data_policy_bel…
#>   <chr> <chr> <list>        <int> <lis> <chr>  <int>            <int>
#> 1 2c0c… Shar… <chr [1]>         1 <chr… ""        80               50
#> # … with 6 more variables: data_policy_benthiclit <int>,
#> #   data_policy_benthicpit <int>, data_policy_habitatcomplexity <int>,
#> #   data_policy_bleachingqc <int>, created_on <chr>, updated_on <chr>
```

returns a single project with the exact name “Sharla test”.

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

For example, to see the sites in this project:

``` r
get_mermaid_project_endpoint("sites", mermaidr_project)
#> # A tibble: 1 x 12
#>   id    name  notes project location$type $coordinates country reef_type
#>   <chr> <chr> <chr> <chr>   <chr>         <list>       <chr>   <chr>    
#> 1 7465… 1201  Pula… 2c0c98… Point         <dbl [2]>    c570ff… 19534716…
#> # … with 5 more variables: reef_zone <chr>, exposure <chr>, predecessor <chr>,
#> #   created_on <chr>, updated_on <chr>
```

You can also use the `project_id` directly to access data from a
project, without having to search for it first. This may be handy since
the `project_id` is directly available from the URL when using the
collect
app.

``` r
get_mermaid_project_endpoint("managements", "2c0c9857-b11c-4b82-b7ef-e9b383d1233c")
#> # A tibble: 0 x 17
#> # … with 17 variables: id <lgl>, name <lgl>, name_secondary <lgl>,
#> #   project <lgl>, notes <lgl>, est_year <lgl>, no_take <lgl>,
#> #   periodic_closure <lgl>, open_access <lgl>, size_limits <lgl>,
#> #   gear_restriction <lgl>, species_restriction <lgl>, compliance <lgl>,
#> #   predecessor <lgl>, parties <lgl>, created_on <lgl>, updated_on <lgl>
```

If you want to access data from the same project multiple times within a
session, it may be useful to set the default project, rather than having
to supply it every time. You can do this using `set_default_project()`.
Then, you can just supply the endpoint, and the default project is used.

``` r
set_default_project(mermaidr_project)
get_mermaid_project_endpoint("beltfishes")
#> # A tibble: 0 x 4
#> # … with 4 variables: id <lgl>, transect <lgl>, created_on <lgl>,
#> #   updated_on <lgl>
```
