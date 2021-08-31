# Precompiled vignettes that depend on API key
# Must manually move image files to vignettes/articles/ after knit

knitr::knit("vignettes/articles/vo_baseline_monitoring.Rmd.orig", output = "vignettes/articles/vo_baseline_monitoring.Rmd")
knitr::knit("vignettes/articles/accessing_project_data.Rmd.orig", output = "vignettes/articles/accessing_project_data.Rmd")
