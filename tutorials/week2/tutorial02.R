###############################
# Data Viz  
# Tutorial 2:  
# Tidyverse & Manipulating Data        
###############################  

# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
    basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
    package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
    package.list <- setdiff(package.list, basic.packages)
    if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
    }
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg,  dependencies = TRUE)
    sapply(pkg,  require,  character.only = TRUE)
    }

# Load any necessary packages
lapply(c("tidyverse", "ggplot2", "ggridges", "tradestatistics"),  pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#################
### In class demo
#################

# Import dataset and load library
SAFI <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/DataViz_2026/refs/heads/main/datasets/SAFI.csv")		

# All households in village "God"
SAFI_god <- filter(SAFI, village == "God")

# Households with more than 6 members
large_households <- filter(SAFI, no_membrs > 6)

# Households in "God" or "Ruaca" with more than 4 members
god_ruaca_large <- filter(SAFI, village %in% c("God", "Ruaca") & no_membrs > 4)

# Keep only identification and location variables
id_loc <- SAFI |>
  select(key_ID, village)

# Move village to the first position
village_first <- SAFI |>
  select(village, everything())

# Drop columns related to survey timing
no_timing <- SAFI |>
  select(-starts_with("interview_date"))

# Households ordered by size (ascending)
SAFI_by_size <- SAFI |>
  arrange(no_membrs)

# Households ordered by size (descending) within village
SAFI_by_village_size <- SAFI |>
  arrange(village, desc(no_membrs))

# Add a logical indicator for "large" households
SAFI <- SAFI |>
  mutate(large_hh = no_membrs >= 6)

# Years of living in village in decades
SAFI <- SAFI |>
  mutate(living_decades = years_liv / 10)

# Categorize household size
SAFI <- SAFI |>
  mutate(
    hh_size_cat = case_when(
      no_membrs <= 3            ~ "small",
      no_membrs >= 4 & no_membrs <= 7 ~ "medium",
      no_membrs > 7             ~ "large"
      )
  )

# Large households in Ruaca with selected columns
ruaca_large_small_df <- SAFI |>
  filter(village == "Ruaca", no_membrs > 6) |>
  mutate(
    large_hh = TRUE,
    members_per_room = no_membrs / rooms
  ) |>
  select(village, no_membrs, rooms, members_per_room, large_hh)

# Mean household size by village
village_hh_summary <- SAFI |>
  group_by(village) |>
  summarize(
    mean_members = mean(no_membrs, na.rm = TRUE),
    median_members = median(no_membrs, na.rm = TRUE),
    n_households = n(),
    .groups = "drop"
  )

# Mean years in village by roof type
wall_living_summary <- SAFI |>
  count(wall_type, village, name = "n") |>
  group_by(village) |>
  mutate(prop = n / sum(n))

pdf("wall_type_village.pdf")
ggplot(wall_living_summary,
       aes(x = village, y = prop, fill = wall_type)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "\nVillage",
    y = "Proportion of households\n",
    fill = "Wall type",
    title = "Wall material proportions by village"
  ) 
dev.off()

pdf("hist_bin30.pdf")
ggplot(SAFI, aes(x = no_membrs)) +
  geom_histogram(color = "white", fill = "#E16462") +
  labs(x = "\nNumber of household members", 
       y = "Count\n")
dev.off()

pdf("hist_bin1.pdf")
ggplot(SAFI, aes(x = no_membrs)) +
  geom_histogram(binwidth = 1, color = "white", fill = "#E16462") +
  labs(x = "\nNumber of household members", 
       y = "Count\n")
dev.off()

pdf("hist_group1.pdf")
ggplot(SAFI, aes(x = no_membrs, fill = memb_assoc)) +
  geom_histogram(position="dodge") +
  labs(x = "\nNumber of household members", 
       y = "Count\n")
dev.off()

pdf("hist_group2.pdf")
ggplot(SAFI, aes(x = no_membrs, fill = memb_assoc)) +
  geom_histogram(position="dodge") +
  facet_wrap(vars(village)) +
  labs(x = "\nNumber of household members", 
       y = "Count\n")
dev.off()

pdf("density.pdf")
ggplot(SAFI, aes(x = no_membrs, fill = village)) +
  geom_density(alpha = 0.5, color = NA, adjust = 1) +
  labs(x = "\nNumber of household members",
       y = "Density\n",
       fill = "Village")
dev.off()

pdf("ridge_density.pdf")
ggplot(SAFI, aes(x = no_membrs, y = village, fill = village)) +
  geom_density_ridges(alpha = 0.7, color = "white", scale = 1.2) +
  labs(x = "\nNumber of household members",
       y = "Village\n",
       fill = "Village") 
dev.off()

pdf("boxplot.pdf")
ggplot(SAFI, aes(x = village, y = no_membrs, fill = village)) +
  geom_boxplot(alpha = 0.7, color = "#5ABD51", coef = 3, width = 0.4) +
  labs(fill = "Village", x = "\nVillage", y = "Household members\n")
dev.off()

pdf("violin.pdf")
ggplot(SAFI, aes(x = village, y = no_membrs, fill = village)) +
  geom_violin(alpha = 0.7, width = 0.8, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 0.8) +
  labs(fill = "Village", x = "\nVillage", y = "Household members\n")
dev.off()

##############
### Group work
##############

# Download US trade data with Ireland for 2018-2020 
# table: yrpc
# reporters: USA
us_trade <- ots_create_tidy_data(
  years = 2018:2020,
  reporters = "USA",
  table = "yrpc"
)

### Data Manipulation

# Filter US imports from China (2018-2020),
# create a variable for log import values, and 
# categorize commodities as:
# (1) Primary Goods (Agriculture & Raw Materials)
#   "Live animals; animal products", "Vegetable products", "Animal, vegetable or microbial fats and oils...", "Mineral products", 
#   "Raw hides and skins, leather, furskins...", "Wood and articles of wood...", "Natural or cultured pearls, precious..."
# (2) Intermediate Goods (Manufacturing Inputs)
#   "Products of the chemical or allied industries", "Plastics and articles thereof; rubber...", "Pulp of wood... paper and paperboard...", 
#   "Textiles and textile articles", "Articles of stone, plaster, cement...", "Base metals and articles of base metal"
# (3) Final Goods (Consumer & Capital Goods)
#   "Prepared foodstuffs; beverages...", "Footwear, headgear, umbrellas...", "Machinery and mechanical appliances...", "Works of art, collectors' pieces...",
#   "Vehicles, aircraft, vessels...","Optical, photographic... medical instruments", "Arms and ammunition", "Miscellaneous manufactured articles"

# For 2020 US imports:
# What are the top 5 import partners by total value
# What are the average import value by 2-digit HS code for top 3 partners
# What is the growth rate 2018→2020 by partner (requires pivot_wider)

# Let's investiage electronics growth
# Filter chapter_code = 85 (Machinery and mechanical appliances...) imports
# Calculate year-over-year growth
# Rank partners by 2020 value AND growth rate
# Select & arrange top 10

### Visualizations

# Visualize 2020 US import values:
# Histogram with default bins vs binwidth = 1e9, boundary = 0
# Log-scale version of best histogram
# Density plot overlay (import vs export)

# Who are the top partners? Create:
# Stacked bar: Top 5 partners' share of total 2020 imports
# Dodged bars: Import vs Export comparison for top 5 partners

# Distribution exploration:
# Boxplot: Import value distribution by top 5 partners
# Violin + jitter: Same data with individual shipments
# Ridge plot: Partner distributions stacked vertically

# Extra questions:
# Which partner shows most growth variability 2018-2020?
# What binwidth best reveals the import value distribution?
# Which visualization best communicates "China dominates electronics"?