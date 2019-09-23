
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
#>  1 9b07… Abel… <chr [1]>         1 <chr… ""        90               50
#>  2 834a… Admi… <chr [1]>         3 <chr… ""        90               50
#>  3 eab7… Admi… <chr [1]>         2 <chr… ""        80               50
#>  4 8701… Ahus… <chr [1]>         1 <chr… Offl…     90               50
#>  5 a515… Ashi… <chr [1]>         1 <chr… Play…     90               50
#>  6 cbb3… Auto… <chr [1]>         3 <chr… ""        80               50
#>  7 cc0d… Awal… <chr [0]>         0 <chr… Lear…     90               50
#>  8 7d38… Awal… <chr [0]>         0 <chr… Lear…     90               50
#>  9 1243… Bana… <chr [2]>         4 <chr… ""        90               50
#> 10 1ee5… Bana… <chr [1]>         4 <chr… ""        90               50
#> # … with 40 more rows, and 6 more variables: data_policy_benthiclit <int>,
#> #   data_policy_benthicpit <int>, data_policy_habitatcomplexity <int>,
#> #   data_policy_bleachingqc <int>, created_on <chr>, updated_on <chr>
```

``` r
get_mermaid_endpoint("managements")
#> # A tibble: 50 x 19
#>    id    name  name_secondary project project_name rules notes est_year
#>    <chr> <chr> <chr>          <chr>   <chr>        <chr> <chr>    <int>
#>  1 781a… a ne… j              5a57df… fishman pro… Open… ""        1990
#>  2 6fb5… Aqua… Use Zone       e0119b… Cañada Test  ""    ""          NA
#>  3 6f79… Aqua… Use Zone       f123d4… Test Zoom 1  ""    ""          NA
#>  4 7f35… Aqua… Use Zone       13b516… Collector I… ""    ""        2017
#>  5 469c… Aqua… Use Zone       834aa6… Admin Is Te… ""    ""        2017
#>  6 c19f… Aqua… Use Zone       9de827… XPDC Kei Ke… ""    ""          NA
#>  7 9517… Aqua… Use Zone       30e47b… Lamu         ""    ""          NA
#>  8 7631… Aqua… Use Zone       bbd8b9… Test Projec… ""    ""        2017
#>  9 f50f… Aqua… Use Zone       99f71b… Golem Autom… ""    ""        2017
#> 10 7234… Aqua… Use Zone       1d0c0f… Collector I… ""    ""        2017
#> # … with 40 more rows, and 11 more variables: no_take <lgl>,
#> #   periodic_closure <lgl>, open_access <lgl>, size_limits <lgl>,
#> #   gear_restriction <lgl>, species_restriction <lgl>, compliance <chr>,
#> #   predecessor <chr>, parties <list>, created_on <chr>, updated_on <chr>
```

### Accessing project data

You will be able to access data from a specific project, provided that
you have access to it in the Collect app. To access data for a project,
you can either use a project from `get_mermaid_endpoint("projects")` (as
above), a `project_id` directly, or a project from `search_projects()`.

For example:

``` r
library(dplyr)

mermaidr_project <- search_projects(name = "mermaidr testing", exact_name = TRUE)

mermaidr_project
#> # A tibble: 1 x 14
#>   id    name  countries num_sites tags  notes status data_policy_bel…
#>   <chr> <chr> <list>        <int> <lis> <chr>  <int>            <int>
#> 1 e477… merm… <chr [2]>         2 <chr… ""        80               50
#> # … with 6 more variables: data_policy_benthiclit <int>,
#> #   data_policy_benthicpit <int>, data_policy_habitatcomplexity <int>,
#> #   data_policy_bleachingqc <int>, created_on <chr>, updated_on <chr>
```

returns a single project with the exact name “mermaidr testing”.

You can use this to access an endpoint for the project, using
`get_mermaid_project_endpoint()`. The following project endpoints are
available: “beltfishtransectmethods”, “beltfishes”,
“benthiclittransectmethods”, “benthicpittransectmethods”,
“benthicpits”, “collectrecords”, “habitatcomplexities”,
“obsbenthiclits”, “obsbenthicpits”, “obshabitatcomplexities”,
“obstransectbeltfishs”, “managements”, “observers”,
“project\_profiles”, “sampleevents”, “sites”.

For example, to see the sites in this project:

``` r
get_mermaid_project_endpoint("sites", mermaidr_project)
#> # A tibble: 2 x 12
#>   id    name  notes project location$type $coordinates country reef_type
#>   <chr> <chr> <chr> <chr>   <chr>         <list>       <chr>   <chr>    
#> 1 42c5… Belo… site… e477b0… Point         <dbl [2]>    dd865c… 2b99cdf4…
#> 2 14cd… Belo… ""    e477b0… Point         <dbl [2]>    c570ff… 19534716…
#> # … with 5 more variables: reef_zone <chr>, exposure <chr>,
#> #   predecessor <chr>, created_on <chr>, updated_on <chr>
```

You can also use the `project_id` directly to access data from a
project, without having to search for it first. This may be handy since
the `project_id` is directly available from the URL when using the
collect
app.

``` r
get_mermaid_project_endpoint("managements", "e477b009-cfd9-4d71-9b8c-d1684f38b954")
#> # A tibble: 1 x 17
#>   id    name  name_secondary project notes est_year no_take
#>   <chr> <chr> <chr>          <chr>   <chr>    <int> <lgl>  
#> 1 4fb1… Test… For Testing 1… e477b0… Addi…     1970 NA     
#> # … with 10 more variables: periodic_closure <lgl>, open_access <lgl>,
#> #   size_limits <lgl>, gear_restriction <lgl>, species_restriction <lgl>,
#> #   compliance <chr>, predecessor <chr>, parties <list>, created_on <chr>,
#> #   updated_on <chr>
```

If you want to access data from the same project multiple times within a
session, it may be useful to set the default project, rather than having
to supply it every time. You can do this using `set_default_project()`.
Then, you can just supply the endpoint, and the default project is used.

``` r
set_default_project(mermaidr_project)
get_mermaid_project_endpoint("beltfishes")
#> # A tibble: 1 x 4
#>   id                 transect             created_on       updated_on      
#>   <chr>              <chr>                <chr>            <chr>           
#> 1 756dcdf1-df8a-4f5… 2690439d-9984-4037-… 2019-08-26T23:2… 2019-08-26T23:2…
```
