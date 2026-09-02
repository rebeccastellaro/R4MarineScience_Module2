#------------------------------------#
# MB5370: R for Marine Science
# Workshop 1: Foundations of Data Science - Wrangling and Plotting
# Rebecca Castellaro
#
#------------------------------------#

# ============================================================
# 1.3 Memory allocation and environment hygiene
# ============================================================

# Housekeeping - run this at the top of every fresh script
## Inventory every active object currently residing in session RAM
objects()
## Purge global environment
rm(list = ls())
## Confirm that global session memory is now completely vacant
objects()


# ============================================================
# 1.4 Ingesting data into R
# ============================================================

library(tidyverse)
library(readxl)

# Practice Import A: standard comma-separated plain text file
benthic_cover <- read_csv(here::here("data/reef_cover_log.csv"))

# Practice Import B: tab-separated telemetry instrument array string
acoustic_stream <- read_tsv(here::here("data/acoustic_telemetry_stream.txt"))

# Practice Import C: specific sheet in a multi-tab Excel spreadsheet
fisheries_annual <- read_excel(here::here("data/fish_catch_data.xlsx"), sheet = "Commercial_2026")

# Reading a messier file - field notes at the top corrupt the naive import
mangrove_data <- read_csv(file = here::here("data/mangrove_survey_raw.csv"))
# (Open the raw csv in Excel to see why this breaks the naive read - conversational
#  notes at the top get treated as column headers.)

# Fixed version: skip the field notes and normalise non-standard NA flags
mangrove_data <- read_csv(
  here::here("data/mangrove_survey_raw.csv"),
  skip = 5,   # skip the first 5 lines of field notes
  na = c(".", "NA", "9999", "ND", "blank")   # convert known text alternatives to true NA
)


# ============================================================
# 1.5 Data frame architectures: Tibbles versus legacy tables
# ============================================================

# Force a modern tibble to degrade into a legacy base R data frame structure
benthic_cover_df <- as.data.frame(benthic_cover)

# Compare console behaviour between the two structures
print(benthic_cover_df)   # legacy: prints everything, no type hints
print(benthic_cover)      # tibble: limited rows, shows column types (<chr>, <dbl>, etc.)


# ============================================================
# 1.6 Wrangling out ecological signals using Palmer Penguins
# ============================================================

# install.packages("palmerpenguins")  # run once in the console, then delete/comment out
library(palmerpenguins)
data("penguins")

# Always inspect the structure of a new dataset first
glimpse(penguins)   # tidyverse version
str(penguins)        # base R version

# Statistical overview - flags missing data and value ranges at a glance
summary(penguins)


# ============================================================
# 1.7 Foundational grammar: Slicing, filtering, sorting, transforming
# ============================================================

# --- 1.7.1 select() - isolate columns ---
morphology_metrics <- select(penguins, species, bill_length_mm, bill_depth_mm, body_mass_g)
glimpse(morphology_metrics)

spatial_block <- select(penguins, species:island)

clean_scientific_fields <- select(penguins, -year)

# --- 1.7.2 filter() - isolate rows ---
adelie_cohort <- filter(penguins, species == "Adelie")

heavy_penguins <- filter(penguins, body_mass_g > 4500)

biscoe_gentoo <- filter(penguins, species == "Gentoo" & island == "Biscoe")

sub_islands <- filter(penguins, island %in% c("Dream", "Torgersen"))

# --- 1.7.3 arrange() - sort rows ---
lightest_first <- arrange(penguins, body_mass_g)

heaviest_first <- arrange(penguins, desc(body_mass_g))

stratified_morphology <- arrange(penguins, species, desc(bill_length_mm))

# --- 1.7.4 The pipe (|>) - chaining steps together ---
# Instead of creating intermediate objects at every step:
penguins_subset <- mutate(penguins, bill_ratio = bill_length_mm / bill_depth_mm)
penguins_final  <- filter(penguins_subset, species == "Adelie")

# ...we can chain the same logic in one readable flow:
penguins_final <- penguins |>
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) |>
  filter(species == "Adelie")

# --- 1.7.5 mutate() - compute new attributes ---
penguin_ratios <- penguins |>
  mutate(
    body_mass_kg = body_mass_g / 1000,               # convert grams to kilograms
    bill_ratio   = bill_length_mm / bill_depth_mm      # bill ratio
  )

glimpse(penguin_ratios)


# ============================================================
# 1.8 Data aggregation and ecological summarisation
# ============================================================

# group_by() creates hidden virtual "buckets" by category - table looks unchanged
grouped_penguins <- group_by(penguins, species)
print(grouped_penguins)   # note the 'Groups: species [3]' metadata line

# summarise() collapses each bucket into a single summary row
species_mass_summary <- summarise(grouped_penguins,
                                  mean_mass_g = mean(body_mass_g)
)
print(species_mass_summary)
# NOTE: this returns NA for groups containing any missing body_mass_g values -
# this is the "Missing Value Trap": mean()/sd()/sum() return NA by default if
# ANY value in the group is missing, to stop you silently miscalculating.

# Fix: explicitly tell the aggregation functions to drop NAs before calculating
biological_signal <- penguins %>%
  group_by(species, sex) %>%
  summarise(
    sample_size = n(),                              # count individuals per category
    mean_mass_g = mean(body_mass_g, na.rm = TRUE),  # mean ignoring missing cells
    sd_mass_g   = sd(body_mass_g, na.rm = TRUE)     # standard deviation
  )

print(biological_signal)

