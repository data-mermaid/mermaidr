# Calculating standard deviations for Kim to test against when building SDs into API

# Fishbelt:
# Needs to be in a format where NA/NULLs are retained when there actually is data
# i.e. an NA sd when there is only 1 value, is valid
# an NA sd because there is no data (but there is data for other SEs) is NOT valid

# Some protocols have columns/fields for all data, regardless of whether it is present (i.e., explicit 0s):
# Benthic PIT, Benthic LIT

library(mermaidr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(jsonlite)

p <- mermaid_get_my_projects() %>% filter(name != "Sharla test")

# Fishbelt ----
# DONE
fishbelt <- p %>%
  mermaid_get_project_data("fishbelt", c("sampleunits", "sampleevents"))

# The SE average is (sum of SU totals) / (# SUs), with # of SUs regardless of whether or not those SUs have observations for that trophic group etc
# So the SE SD needs to be calculated by propagating fake 0s for SUs that do not have any observations for that group

fishbelt_su <- fishbelt[["sampleunits"]] %>%
  select(project, sample_event_id, contains("biomass_kgha")) %>%
  pivot_longer(contains("biomass_kgha")) %>%
  filter(!str_detect(name, "zeroes")) %>%
  mutate(
    name = str_replace(name, "trophic_group", "trophic_group_avg"),
    name = str_replace(name, "fish_family", "fish_family_avg"),
    name = ifelse(name == "biomass_kgha", "biomass_kgha_avg", name)
  )

# Flag groups with any data (i.e. not all 0s across all SUs)
fishbelt_se_contains_data <- fishbelt_su %>%
  filter(!is.na(value)) %>%
  distinct(project, sample_event_id, name)

# Replace NAs with 0s, only for combinations with *any* data
fishbelt_su <- fishbelt_su %>%
  inner_join(fishbelt_se_contains_data, by = c("project", "sample_event_id", "name")) %>%
  mutate(
    value = coalesce(value, 0)
  )

# Aggregate mean and SD
fishbelt_su_agg <- fishbelt_su %>%
  group_by(project, sample_event_id, name) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    .groups = "drop"
  )

# Get SE values
fishbelt_se <- fishbelt[["sampleevents"]] %>%
  select(project, sample_event_id = id, contains("avg")) %>%
  pivot_longer(contains("avg")) %>%
  filter(!is.na(value))

# Compare values
fishbelt_su_agg %>%
  inner_join(fishbelt_se,
    by = c("project", "sample_event_id", "name")
  ) %>%
  filter(round(mean) != round(value))
# Just some rounding

# Compare depth means
fishbelt_su_depth_agg <- fishbelt[["sampleunits"]] %>%
  select(project, sample_event_id, depth) %>%
  group_by(project, sample_event_id) %>%
  summarise(mean = mean(depth), sd = sd(depth), .groups = "drop")

fishbelt_su_depth_agg %>%
  left_join(fishbelt_se %>% filter(name == "depth_avg"), by = c("project", "sample_event_id")) %>%
  filter(round(mean, 1) != round(mean, 1))

# Convert to JSON format
fishbelt_sd_json <- fishbelt_su_agg %>%
  split(.$sample_event_id) %>%
  map(function(x) {
    fish_family <- x %>%
      select(-mean) %>%
      filter(str_starts(name, "biomass_kgha_fish_family_avg")) %>%
      mutate(
        name = str_remove(name, "biomass_kgha_fish_family_avg_"),
        name = str_to_title(name)
      ) %>%
      pivot_wider(names_from = name, values_from = sd) %>%
      nest(biomass_kgha_fish_family_sd = -c(project, sample_event_id))

    trophic_group <- x %>%
      select(-mean) %>%
      filter(str_starts(name, "biomass_kgha_trophic_group_avg")) %>%
      mutate(name = str_remove(name, "biomass_kgha_trophic_group_avg_")) %>%
      pivot_wider(names_from = name, values_from = sd) %>%
      nest(biomass_kgha_trophic_group_sd = -c(project, sample_event_id))

    metadata <- x %>%
      filter(name == "biomass_kgha_avg") %>%
      select(project, sample_event_id, biomass_kgha_sd = sd) %>%
      left_join(fishbelt_su_depth_agg %>% select(-mean) %>% rename(depth_sd = sd), by = c("project", "sample_event_id"))

    metadata %>%
      left_join(fish_family, by = c("project", "sample_event_id")) %>%
      left_join(trophic_group, by = c("project", "sample_event_id")) %>%
      rename(project_name = project)
  }) %>%
  unname() %>%
  toJSON(pretty = TRUE, na = "null")

writeLines(fishbelt_sd_json, here::here("scratch", "fishbelt_sd.json"))

# Benthic PIT ----
# DONE

benthicpit <- p %>%
  mermaid_get_project_data("benthicpit", c("sampleunits", "sampleevents"))

# percent_cover_benthic_category_avg is calculated from SU values of the same names

benthicpit_su <- benthicpit[["sampleunits"]] %>%
  select(project, sample_event_id, starts_with("percent_cover_benthic_category_")) %>%
  pivot_longer(starts_with("percent_cover_benthic_category_")) %>%
  mutate(name = str_remove(name, "percent_cover_benthic_category_"))

benthicpit_se <- benthicpit[["sampleevents"]] %>%
  select(project, sample_event_id = id, starts_with("percent_cover_benthic_category_avg_")) %>%
  pivot_longer(starts_with("percent_cover_benthic_category_avg_")) %>%
  mutate(name = str_remove(name, "percent_cover_benthic_category_avg_"))

benthicpit_su_agg <- benthicpit_su %>%
  group_by(project, sample_event_id, name) %>%
  summarise(mean = mean(value), sd = sd(value), .groups = "drop")

# Compare manual means to API

benthicpit_su_agg %>%
  full_join(benthicpit_se, by = c("project", "sample_event_id", "name")) %>%
  filter(round(mean, 1) != round(value, 1))

# Get SDs (and depth SD) into JSON format
benthicpit_sd <- benthicpit[["sampleunits"]] %>%
  select(project, sample_event_id, depth) %>%
  group_by(project, sample_event_id) %>%
  summarise(depth_sd = sd(depth), .groups = "drop") %>%
  left_join(benthicpit_su_agg %>%
    select(-mean) %>%
    pivot_wider(names_from = name, values_from = sd) %>%
    group_by(project, sample_event_id) %>%
    nest() %>%
    rename(percent_cover_benthic_category_sd = data),
  by = c("project", "sample_event_id")
  )

benthicpit_sd_json <- benthicpit_sd %>%
  toJSON(pretty = TRUE, na = "null")

writeLines(benthicpit_sd_json, here::here("scratch", "benthicpit_sd.json"))

# Benthic LIT ----
# DONE

# Same as PIT

benthiclit <- p %>%
  mermaid_get_project_data("benthiclit", c("sampleunits", "sampleevents"))

# percent_cover_benthic_category_avg is calculated from SU values of the same names

benthiclit_su <- benthiclit[["sampleunits"]] %>%
  select(project, sample_event_id, starts_with("percent_cover_benthic_category_")) %>%
  pivot_longer(starts_with("percent_cover_benthic_category_")) %>%
  mutate(name = str_remove(name, "percent_cover_benthic_category_"))

benthiclit_se <- benthiclit[["sampleevents"]] %>%
  select(project, sample_event_id = id, starts_with("percent_cover_benthic_category_avg_")) %>%
  pivot_longer(starts_with("percent_cover_benthic_category_avg_")) %>%
  mutate(name = str_remove(name, "percent_cover_benthic_category_avg_"))

benthiclit_su_agg <- benthiclit_su %>%
  group_by(project, sample_event_id, name) %>%
  summarise(mean = mean(value), sd = sd(value), .groups = "drop")

# Compare manual means to API

benthiclit_su_agg %>%
  full_join(benthiclit_se, by = c("project", "sample_event_id", "name")) %>%
  filter(round(mean, 1) != round(value, 1))

# Get SDs (and depth SD) into JSON format
benthiclit_sd <- benthiclit[["sampleunits"]] %>%
  select(project, sample_event_id, depth) %>%
  group_by(project, sample_event_id) %>%
  summarise(depth_sd = sd(depth), .groups = "drop") %>%
  left_join(benthiclit_su_agg %>%
    select(-mean) %>%
    pivot_wider(names_from = name, values_from = sd) %>%
    group_by(project, sample_event_id) %>%
    nest() %>%
    rename(percent_cover_benthic_category_sd = data),
  by = c("project", "sample_event_id")
  )

benthiclit_sd_json <- benthiclit_sd %>%
  toJSON(pretty = TRUE, na = "null")

writeLines(benthiclit_sd_json, here::here("scratch", "benthiclit_sd.json"))

# Habitat Complexity ----

# In habitat complexity, SU contains avg (of score), and SE contains avg of those avgs
# So both SU and SE need SDs

habitatcomplexity <- p %>%
  mermaid_get_project_data("habitatcomplexity", "all")

# The sampleunits does not have a usable ID, only sample_unit_ids which is combined
# id seems to be unrelated, which is not good

# So we can only aggregate SU -> SE for now, can't do obs -> SU

## Obs to SU ----
# TODO: Can't do this yet

## SU to SE ----
# DONE

hc_su <- habitatcomplexity[["sampleunits"]] %>%
  select(project, sample_event_id, depth, score_avg) %>%
  pivot_longer(cols = c(depth, score_avg))

hc_se <- habitatcomplexity[["sampleevents"]] %>%
  select(project, sample_event_id = id, contains("avg")) %>%
  pivot_longer(cols = contains("avg"))

hc_su_agg <- hc_su %>%
  group_by(project, sample_event_id, name) %>%
  summarise(mean = mean(value), sd = sd(value), .groups = "drop") %>%
  mutate(
    name_mean = paste0(name, "_avg"),
    name_sd = paste0(name, "_sd")
  )

# Check means match
hc_su_agg %>%
  left_join(hc_se, by = c("project", "sample_event_id", "name_mean" = "name")) %>%
  filter(round(mean, 1) != round(value, 1))

# Save SDs as JSON
hc_su_sd <- hc_su_agg %>%
  select(project, sample_event_id, name = name_sd, sd) %>%
  pivot_wider(names_from = name, values_from = sd)

hc_su_sd_json <- hc_su_sd %>%
  toJSON(pretty = TRUE, na = "null")

writeLines(hc_su_sd_json, here::here("scratch", "habitatcomplexity_sd_for_se.json"))

# Bleaching ----

# In bleaching, SU contains avg (of percent cover), and SE contains avg of those avgs
# So both SU and SE need SDs

bleaching <- p %>%
  mermaid_get_project_data("bleaching", "all")

## Obs to SU ----
# TODO: Can't do this yet

# The sampleunits does not have a usable ID, only sample_unit_ids which is combined
# id seems to be unrelated, which is not good

bleaching$sampleunits %>% filter(str_detect(sample_unit_ids, id))

bleaching_percent_cover_obs <- bleaching$observations$percent_cover %>%
  select(project, sample_unit_id, starts_with("percent_"))

bleaching_percent_cover_obs_agg <- bleaching_percent_cover_obs %>%
  pivot_longer(starts_with("percent_")) %>%
  mutate(name = paste0(name, "_avg")) %>%
  group_by(project, sample_unit_id, name) %>%
  summarise(mean = mean(value), sd = sd(value), .groups = "drop")

bleaching_su <- bleaching$sampleunits %>%
  select(project, sample_unit_id = sample_unit_ids, contains("avg")) %>%
  pivot_longer(starts_with("percent_"))

# Compare mean

bleaching_percent_cover_obs_agg %>%
  left_join(bleaching_su)

## SU to SE ----

avg_cols <- bleaching$sampleevents %>%
  select(contains("avg")) %>%
  names() %>%
  str_remove("_avg$")

bleaching_su <- bleaching$sampleunits %>%
  select(project, sample_event_id, all_of(avg_cols)) %>%
  pivot_longer(-c(project, sample_event_id))

bleaching_su_agg <- bleaching_su %>%
  group_by(project, sample_event_id, name) %>%
  summarise(mean = mean(value), sd = sd(value), .groups = "drop") %>%
  mutate(
    name_avg = paste0(name, "_avg"),
    name_sd = paste0(name, "_sd")
  )

bleaching_se <- bleaching$sampleevents %>%
  select(project, sample_event_id = id, contains("avg")) %>%
  pivot_longer(contains("avg"))

bleaching_su_agg %>%
  left_join(bleaching_se) %>%
  filter(round(mean) != round(value))

bleaching_sd <- bleaching_su_agg %>%
  select(-mean) %>%
  select(project, sample_event_id, name = name_sd, sd) %>%
  pivot_wider(names_from = name, values_from = sd)

bleaching_sd_json <- bleaching_sd %>%
  toJSON(pretty = TRUE, na = "null")

writeLines(bleaching_sd_json, here::here("scratch", "bleaching_sd_for_se.json"))
