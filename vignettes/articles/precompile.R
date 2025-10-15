# Precompiled vignettes that depend on API key
# Must manually move image files to vignettes/articles/ after knit

knitr::knit("vignettes/articles/accessing_project_data.Rmd.orig", output = "vignettes/articles/accessing_project_data.Rmd")

# On prod, not dev
knitr::knit("vignettes/articles/importing_fishbelt.Rmd.orig", output = "vignettes/articles/importing_fishbelt.Rmd")
knitr::knit("vignettes/articles/import_cpce.Rmd.orig", output = "vignettes/articles/import_cpce.Rmd")
