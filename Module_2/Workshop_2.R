#------------------------------------#
# MB5370: R for Marine Science
# Workshop 2 - Module 2: Advanced Data Wrangling - Extracting Ecological Signals from Noisy Systems
# Rebecca Castellaro
# AI-On phase 
#------------------------------------#

library(tidyverse)
library(palmerpenguins)
library(lubridate)

# ============================================================
# 2.2 Tidy data - understanding what "tidy" means
# ============================================================

# table1, table2, table3 are built into the tidyr package (part of tidyverse)
# and all represent the SAME underlying dataset, just structured differently.
table1   # tidy: each variable (country, year, cases, population) has its own column
table2   # untidy: 'type' column mixes two different variables (cases/population) together
table3   # untidy: 'rate' column crams two variables (cases and population) into one string

# Why table1 is useful: because every variable is its own column, we can
# directly compute on it without any restructuring first.
table1 %>%
  mutate(rate = cases / population * 10000)

table1 %>%
  count(year, wt = cases)

ggplot(table1, aes(year, cases)) +
  geom_line(aes(group = country), colour = "grey50") +
  geom_point(aes(colour = country))


# ============================================================
# 2.4 Lengthening datasets with pivot_longer()
# ============================================================

# billboard is a built-in tidyr dataset: wk1-wk76 columns hold RANK VALUES,
# but the column NAMES themselves are actually a variable (week number).
billboard

# Pivot so week becomes its own column, and rank becomes its own column
billboard_long <- billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank"
  )
billboard_long

# Many of those NAs exist only because a song wasn't charting that week -
# they're not "missing data", they're structural. Drop them:
billboard_long_clean <- billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  )
billboard_long_clean

# Small hand-built example (tribble = manually typed tibble) to see the
# mechanics of pivot_longer() more clearly on a tiny dataset
df <- tribble(
  ~id, ~bp1, ~bp2,
  "A", 100, 120,
  "B", 140, 115,
  "C", 120, 125
)

df |>
  pivot_longer(
    cols = bp1:bp2,
    names_to = "measurement",
    values_to = "value"
  )


# ============================================================
# 2.5 Widening datasets with pivot_wider()
# ============================================================

# cms_patient_experience is another built-in tidyr example dataset
cms_patient_experience

# See the unique measure codes/titles first
cms_patient_experience |>
  distinct(measure_cd, measure_title)

# First attempt - missing id_cols means we still get multiple rows per org
cms_patient_experience |>
  pivot_wider(
    names_from = measure_cd,
    values_from = prf_rate
  )

# Correct version - explicitly declare which columns uniquely identify a row
cms_patient_experience |>
  pivot_wider(
    id_cols = starts_with("org"),
    names_from = measure_cd,
    values_from = prf_rate
  )

# Small hand-built example to see the mechanics clearly
df_wide_example <- tribble(
  ~id, ~measurement, ~value,
  "A", "bp1", 100,
  "B", "bp1", 140,
  "B", "bp2", 115,
  "A", "bp2", 120,
  "A", "bp3", 105
)

df_wide_example |>
  pivot_wider(
    names_from = measurement,
    values_from = value
  )


# ============================================================
# 2.6 Pivoting exercises using Palmer Penguins
# ============================================================

# Lengthen: stack the four morphometric columns into one "measurement_type"
# column and one "value" column, so ggplot2 can facet by measurement type
penguins_long <- penguins |>
  pivot_longer(
    cols = c(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g),
    names_to = "measurement_type",
    values_to = "value"
  )
head(penguins_long)

morphometric_distributions_plot <- penguins_long |>
  drop_na(value) |>
  ggplot(aes(x = value, fill = species)) +
  geom_histogram(bins = 30, alpha = 0.7, colour = "black") +
  facet_wrap(~measurement_type, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Morphometric distributions across penguin species",
    x = "Measurement value",
    y = "Frequency"
  )
morphometric_distributions_plot

# Widen: build a clean species-by-island summary matrix for publication
mass_summary <- penguins |>
  drop_na(body_mass_g) |>
  group_by(species, island) |>
  summarise(mean_mass = mean(body_mass_g), .groups = "drop")
head(mass_summary)

mass_matrix <- mass_summary |>
  pivot_wider(
    names_from = island,
    values_from = mean_mass
  )
head(mass_matrix)
# NAs here are meaningful - they show a species simply doesn't occur on that island


# ============================================================
# 2.7 Separating and uniting columns
# ============================================================

table3   # rate column mixes two variables together: "745/19987071"

# Split rate into two separate columns at the "/" character
table3 %>%
  separate(rate, into = c("cases", "population"), sep = "/", convert = TRUE)

# separate() can also split by character position (useful for e.g. splitting
# a 4-digit year into century + year)
table3 %>%
  separate(year, into = c("century", "year"), sep = 2)

# unite() does the reverse - combine multiple columns into one
table5 %>%
  unite(new, century, year, sep = "")


# ============================================================
# 2.8 Wrangling strings and dates
# ============================================================

# --- Standardising messy text with stringr ---
messy_sites <- tibble(
  site_id = c(" Nelly Bay", "nelly_bay", "NELLY BAY", " Geoffrey_Bay ", "geoffrey bay")
)

clean_sites <- messy_sites |>
  mutate(
    site_clean = str_to_lower(site_id),                              # 1. lowercase everything
    site_clean = str_replace_all(site_clean, pattern = " ", replacement = "_"),  # 2. spaces -> underscores
    site_clean = str_trim(site_clean)                                 # 3. trim leading/trailing whitespace
  )
print(clean_sites)
# Five messy inputs collapse down to just two clean, matching categories

# --- Parsing dates with lubridate ---
# dmy() / ymd() etc. let you spell out the order of the date components
date_1 <- dmy("25/12/2026")
date_2 <- ymd("2026-12-25")
date_1 == date_2   # TRUE - same date, different original formats

# Appending _hms handles full timestamps (date + time)
sensor_data <- tibble(
  raw_time = c("14-05-2026 08:30:00", "14-05-2026 08:45:00", "14-05-2026 09:00:00"),
  temperature = c(24.5, 24.6, 24.4)
)

sensor_clean <- sensor_data |>
  mutate(true_time = dmy_hms(raw_time))
print(sensor_clean)
# Once true_time is a real datetime object, ggplot2 will format the x-axis
# chronologically without any extra work, and month()/day() etc. become available


# ============================================================
# 2.9 Relational data: joining tables
# ============================================================

observations <- tibble(
  site_code = c("NB", "GB", "MI", "NB", "HB"),
  species = c("Trout", "Snapper", "Trout", "Cod", "Trout"),
  count = c(5, 2, 1, 3, 8)
)

site_metadata <- tibble(
  site_code = c("NB", "GB", "MI", "RP", "WP"),
  zone = c("Marine National Park", "Conservation Park", "Habitat Protection", "General Use", "Other Use"),
  lat = c(-19.16, -19.15, -19.14, -19.12, -19.11)
)

# left_join(): keeps every row of observations, fills in NA where no metadata match exists
joined_data <- observations |>
  left_join(site_metadata, by = join_by(site_code))
print(joined_data)

# inner_join(): only keeps rows with a complete match in BOTH tables
matched_data <- observations |>
  inner_join(site_metadata, by = join_by(site_code))
glimpse(matched_data)

# anti_join(): diagnostic tool - shows rows in observations with NO match in site_metadata
missing_context <- observations |>
  anti_join(site_metadata, by = join_by(site_code))
print(missing_context)
# HB shows up here because it has no matching site_code in site_metadata


# ============================================================
# 2.10 Handling missing values
# ============================================================

# --- na_if(): converting legacy sensor error codes into true NA ---
logger_data <- tibble(
  depth_m = c(10, 20, 30, 40),
  temp_c = c(24.5, 24.1, -999, 23.5)   # -999 is a known sensor error code
)

fixed_logger <- logger_data |>
  mutate(temp_c = na_if(temp_c, -999))
print(fixed_logger)

# --- coalesce(): replacing NA with a known fixed value (e.g. a blank meant "zero") ---
shark_counts <- tibble(
  site = c("Reef_A", "Reef_B", "Reef_C"),
  shark_count = c(3, NA, 5)   # blank cell actually meant "zero sharks seen"
)

shark_fixed <- shark_counts |>
  mutate(shark_count = coalesce(shark_count, 0))
print(shark_fixed)

# --- NaN: mathematically impossible results, e.g. 0/0 ---
cpue_data <- tibble(
  site = c("Bay_1", "Bay_2"),
  catch = c(10, 0),
  effort_hours = c(2, 0)
)

cpue_calc <- cpue_data |>
  mutate(cpue = catch / effort_hours)
print(cpue_calc)   # Bay_2 shows NaN (0/0), not NA

# --- complete(): building a zero-catch framework for implicit missing data ---
raw_catch <- tibble(
  site = c("Reef_1", "Reef_1", "Reef_2"),
  species = c("Pmaculatus", "Pleopardus", "Pmaculatus"),
  count = c(5, 2, 8)
)

full_catch_matrix <- raw_catch |>
  complete(site, species, fill = list(count = 0))
print(full_catch_matrix)
# Reef_2/Pleopardus now explicitly shows 0, instead of being silently absent

# --- drop_na(): removing rows where the core response variable failed ---
sensor_log <- tibble(
  day = 1:4,
  salinity = c(35.2, 35.1, NA, 35.3)
)

clean_log <- sensor_log |>
  drop_na(salinity)
print(clean_log)


# ============================================================
# 2.11 Practical exercises: Penguins, Pivots, and Relational Data
# ============================================================

# --- Exercise 1: Cleaning the messy metadata ---
island_metadata <- tibble(
  island_name = c(" biscoe", "Dream ", "Torgersen"),
  station_install = c("15/01/2003", "22-03-2004", "05/11/2001"),
  latitude = c(-64.81, -64.73, -64.76)
)
print(island_metadata)

clean_metadata <- island_metadata |>
  mutate(
    island_name = str_trim(island_name),          # remove leading/trailing spaces
    island_name = str_to_title(island_name),        # match penguins' capitalisation (Biscoe, Dream, Torgersen)
    station_install = dmy(station_install)           # parse mixed day-month-year formats into true Date objects
  )
print(clean_metadata)

# --- Exercise 2: The relational join ---
penguins_spatial <- penguins |>
  left_join(clean_metadata, by = join_by(island == island_name))
head(penguins_spatial)

# --- Exercise 3: The wide summary matrix ---
max_mass_matrix <- penguins_spatial |>
  drop_na(body_mass_g) |>
  group_by(species, island) |>
  summarise(max_mass = max(body_mass_g), .groups = "drop") |>
  pivot_wider(
    names_from = island,
    values_from = max_mass
  )
print(max_mass_matrix)
