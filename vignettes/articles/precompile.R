# Precompiled vignettes that depend on API key
# Must manually move image files to vignettes/articles/ after knit

knitr::knit("vignettes/articles/accessing_project_data.Rmd.orig", output = "vignettes/articles/accessing_project_data.Rmd")
# This one is on prod, not dev
knitr::knit("vignettes/articles/importing_fishbelt.Rmd.orig", output = "vignettes/articles/importing_fishbelt.Rmd")
