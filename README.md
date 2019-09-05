
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
You will not need to do this again, so long as you tell R that you’d
like to cache your token the first time around.

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
#>  6 7d38… Awal… <chr [0]>         0 <chr… Lear…     90               50
#>  7 cc0d… Awal… <chr [0]>         0 <chr… Lear…     90               50
#>  8 1243… Bana… <chr [2]>         4 <chr… ""        90               50
#>  9 1ee5… Bana… <chr [1]>         4 <chr… ""        90               50
#> 10 363a… bela… <chr [0]>         0 <chr… ""        90               50
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
#>  2 f50f… Aqua… Use Zone       99f71b… Golem Autom… ""    ""        2017
#>  3 469c… Aqua… Use Zone       834aa6… Admin Is Te… ""    ""        2017
#>  4 c19f… Aqua… Use Zone       9de827… XPDC Kei Ke… ""    ""          NA
#>  5 7234… Aqua… Use Zone       1d0c0f… Collector I… ""    ""        2017
#>  6 9517… Aqua… Use Zone       30e47b… Lamu         ""    ""          NA
#>  7 274d… Aqua… Use Zone       170ff1… QA Project … ""    ""          NA
#>  8 7f35… Aqua… Use Zone       13b516… Collector I… ""    ""        2017
#>  9 6f79… Aqua… Use Zone       f123d4… Test Zoom 1  ""    ""          NA
#> 10 7631… Aqua… Use Zone       bbd8b9… Test Projec… ""    ""        2017
#> # … with 40 more rows, and 11 more variables: no_take <lgl>,
#> #   periodic_closure <lgl>, open_access <lgl>, size_limits <lgl>,
#> #   gear_restriction <lgl>, species_restriction <lgl>, compliance <chr>,
#> #   predecessor <chr>, parties <list>, created_on <chr>, updated_on <chr>
```

### Accessing project data

To access data for a specific project, use
`get_mermaid_project_endpoint()` with the `project_id`, from
`get_mermaid_endpoint("projects")`. This requires authentication,
i.e. you will only be able to pull data for projects that you have
access to.

The following project endpoints are available:
“beltfishtransectmethods”, “beltfishes”,
“benthiclittransectmethods”, “benthicpittransectmethods”,
“benthicpits”, “collectrecords”, “habitatcomplexities”,
“obsbenthiclits”, “obsbenthicpits”, “obshabitatcomplexities”,
“obstransectbeltfishs”, “managements”, “observers”,
“project\_profiles”, “sampleevents”, “sites”.

For example:

``` r
library(dplyr)

mermaidr_project <- get_mermaid_endpoint("projects", limit = 100) %>%
  filter(name == "mermaidr testing")

mermaidr_project %>%
  get_mermaid_project_endpoint("obshabitatcomplexities")
#> # A tibble: 2 x 9
#>   id    data  interval include notes habitatcomplexi… score created_on
#>   <chr> <lgl>    <dbl> <lgl>   <chr> <chr>            <chr> <chr>     
#> 1 0734… NA           2 TRUE    ""    dd6cc72d-b9d9-4… a5e0… 2019-08-2…
#> 2 5704… NA           1 TRUE    ""    dd6cc72d-b9d9-4… b72a… 2019-08-2…
#> # … with 1 more variable: updated_on <chr>
```
