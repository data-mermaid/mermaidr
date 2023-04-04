# Calculating standard deviations for Kim to test against when building SDs into API

library(mermaidr)
library(dplyr)
library(tidyr)
library(stringr)

p <- mermaid_get_my_projects()

# Fishbelt ----

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
fishbelt_sd_fish_family <- fishbelt_su_agg %>%
  select(-mean) %>%
  filter(str_starts(name, "biomass_kgha_fish_family_avg")) %>%
  mutate(name = str_remove(name, "biomass_kgha_fish_family_avg_"),
         name = str_to_title(name)) %>%
  pivot_wider(names_from = name, values_from = sd) %>%
  nest(biomass_kgha_fish_family_sd = -c(project, sample_event_id))

fishbelt_sd_trophic_group <- fishbelt_su_agg %>%
  select(-mean) %>%
  filter(str_starts(name, "biomass_kgha_trophic_group_avg")) %>%
  mutate(name = str_remove(name, "biomass_kgha_trophic_group_avg_")) %>%
  pivot_wider(names_from = name, values_from = sd) %>%
  nest(biomass_kgha_trophic_group_sd = -c(project, sample_event_id))

fishbelt_sd_metadata <- fishbelt_su_agg %>%
  filter(name == "biomass_kgha_avg") %>%
  select(project, sample_event_id, biomass_kgha_sd = sd) %>%
  left_join(fishbelt_su_depth_agg %>% select(-mean) %>% rename(depth_sd = sd), by = c("project", "sample_event_id"))

fishbelt_sd <- fishbelt_sd_metadata %>%
  left_join(fishbelt_sd_fish_family, by = c("project", "sample_event_id")) %>%
  left_join(fishbelt_sd_trophic_group, by = c("project", "sample_event_id")) %>%
  left_join(p %>% select(id, name), by = c("project" = "name")) %>%
  rename(project_id = id) %>%
  select(-project) %>%
  relocate(project_id)

fishbelt_sd_json <- fishbelt_sd %>%
  jsonlite::toJSON(pretty = TRUE)

# Benthic PIT ----

# Benthic LIT ----

# Benthic PQT ----

# Bleaching ----

# Habitat Complexity ----
