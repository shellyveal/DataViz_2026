#####################
# load libraries
# set wd
# clear global .envir
#####################

# https://www.kaggle.com/datasets/akhilv11/border-crossing-entry-data

rm(list=ls())
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("tidyverse", "ggplot2",
         "patchwork", "showtext", "ggtext",
         "purrr", "systemfonts", 
         "ggfortify", "usmap"),  pkgTest)
options(scipen = 999)
# set working directory to current parent folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
theme_set(theme_minimal())
showtext_auto()

all_fonts <- system_fonts()

font_table <- all_fonts %>%
  dplyr::group_by(family) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup()

purrr::walk2(font_table$family, font_table$path, function(fam, path) {
  try(font_add(fam, regular = path), silent = TRUE)
})

showtext_auto()

SVU <- theme_minimal(base_family = "Tahoma", base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(margin = margin(t = 10, b = 10), 
                                  face = "bold", size = rel(1.7)),
        plot.subtitle = element_text(face = "plain", size = rel(1.3),
                                     color = "grey40"),
        plot.caption = element_text(face = "italic", size = rel(0.7), 
                                    color = "grey70", hjust = 0),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", 
                                  size = rel(1.1), hjust = 0.5),
        axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10, 
                                                    b = 10), hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 10,
                                                    l = 8), hjust = 0.5),
        strip.background = element_rect(fill = "grey90", color = NA),
        panel.border = element_rect(color = "grey90", fill = NA),
        palette.colour.discrete = function(n)
          scales::pal_viridis(option = "D")(n),
        palette.fill.discrete = function(n) 
          scales::pal_viridis(option = "D")(n),
        palette.colour.continuous = scales::pal_viridis(option = "D"),
        palette.fill.continuous = scales::pal_viridis(option = "D"))













#####################
# Data Manipulation
#####################

### 

dataset_backup <- read_csv("Border_Crossing_Entry_Data.csv")
data <- dataset_backup
data$Date <- mdy_hms(data$Date)
data$Location <- gsub("[()]", "", data$Location)

data <- separate(data, 
                 Location, 
                 into = c("POINT", "Longitude", "Latitude"), 
                 sep = " ", 
                 convert = TRUE)

data <- data %>% select(!POINT) %>% filter(`Port Code` != 2507)

unique(data$Measure)

df_people <- data %>% filter(Measure == "Personal Vehicle Passengers" | 
                               Measure == "Bus Passengers" |
                               Measure == "Train Passengers" |
                               Measure == "Pedestrians")

df_vehicles <- data %>% filter(Measure == "Trucks" |
                                 Measure == "Rail Containers Full" |
                                 Measure == "Trains" |
                                 Measure == "Truck Containers Empty" |
                                 Measure == "Rail Containers Empty" |
                                 Measure == "Personal Vehicles" |
                                 Measure == "Buses" |
                                 Measure == "Truck Containers Full")
### 
# map of people movement

port_sums <- df_people %>%
  group_by(`Port Code`, Year = year(Date), State) %>%
  summarize(total_people = sum(Value, na.rm = TRUE))

port_coords <- data %>%
  select(`Port Code`, `Port Name`, Latitude, Longitude, Border) %>%
  distinct(`Port Code`, .keep_all = TRUE) %>%
  arrange(`Port Code`)

map_data <- port_sums %>%
  group_by(`Port Code`, State) %>%
  summarize(avg_people_yearly = round(sum(total_people)/length(Year), 0)) %>%
  left_join(port_coords, by = "Port Code")

transformed <- usmap_transform(map_data,
                               input_names = c("Longitude", 'Latitude'))

map_plot <- plot_usmap(color = "black", fill = "white") +
  SVU +
  geom_sf(data = transformed,
          aes(size = avg_people_yearly),
          color = "#354B85", alpha = 0.5) +
  scale_size_continuous(
    range = c(1, 10),               
    breaks = c(250000, 500000, 2000000, 5000000, 10000000),  
    labels = c("0.25M", "0.5M", "2M", "5M", "10M")          
  ) +
  labs(
    title = "Average Individuals Crossing U.S. Borders Yearly",
    subtitle = "Ports of Entry Markers Differing by Size According to Yearly Averages",
    size = "People per Year")

# boxplot, group by port name and date, [+/- fill by year] facet wrap by state monthly average human/ vehicles entering U.S.

state_borders <- df_people %>%
  select(State, Border) %>%
  distinct(State, .keep_all = TRUE) %>%
  arrange(State)

bp_data <- df_people %>%
  group_by(State, Year = year(Date)) %>%
  summarize(total_people = sum(Value, na.rm = TRUE)) %>%
  left_join(state_borders, by = "State") %>%
  mutate(
    total_people = total_people/1000000)


bp_MX <- ggplot(bp_data[bp_data$Border == "US-Mexico Border", ], 
       aes(x = State, y = total_people)) +
  geom_boxplot(alpha = 0.6, linewidth = 0.8, 
               show.legend = FALSE, fill = "#403891b2", color = "#403891b2") +
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0, 180, by = 10)) +
  labs(y = "People (millions)", 
      title = "Yearly U.S.-Mexico Border Crossings",
      subtitle = "1996-2019") +
  SVU

bp_CA <- ggplot(bp_data[bp_data$Border == "US-Canada Border", ], 
       aes(x = State, y = total_people)) +
  geom_boxplot(alpha = 0.6, linewidth = 0.8, 
               show.legend = FALSE, fill = "#354B85", color = "#354B85") +
  scale_y_continuous(expand = c(0,0.25),
                     breaks = seq(0, 180, by = 10)) +
  labs(y = "People (millions)", 
       title = "Yearly U.S.-Canada Border Crossings",
       subtitle = "1996-2019") +
  SVU

# seasonality and overall trends of migration in both US-Canada Border and US-Mexico border

seasons_data <- df_people %>%
  group_by(Border, Date) %>%
  summarize(total_people = sum(Value, na.rm = TRUE))

glimpse(seasons_data)

ggplot(seasons_data, aes(x = Date, y = total_people)) +
  geom_point() +
  facet_wrap(~Border) +
  ylim(0, 30000000)


## US-CANADA CROSSING DECOMPOSING TIMESERIES BASEPLOT WRANGLING


CA_data <- seasons_data %>% filter(Border == 'US-Canada Border') %>%
  arrange(Date)

CA_ts <- ts(
  CA_data$total_people,
  start = c(year(min(CA_data$Date)), month(min(CA_data$Date))),
  frequency = 12
)

CA_decomp <- decompose(CA_ts)

decomp_df <- tibble(
  Date = seq(
    from = as.Date(paste0(start(CA_ts)[1], "-", start(CA_ts)[2], "-01")),
    by = "month",
    length.out = length(CA_ts)),
  Total = as.numeric(CA_decomp$x),
  Trend = as.numeric(CA_decomp$trend),
  Seasonality = as.numeric(CA_decomp$seasonal),
  Remainder = as.numeric(CA_decomp$random)) %>%
  pivot_longer(
    cols = -Date,
    names_to = "Component",
    values_to = "Value")

decomp_df$Component <- factor(
  decomp_df$Component,
  levels = c("Total", "Trend", "Seasonality", "Remainder"))

y_labels <- c(
  "Total" = "People",
  "Trend" = "Trend (people)",
  "Seasonality" = "Seasonal effect",
  "Remainder" = "Residual changes"
)

top_df <- decomp_df %>% 
  filter(Component %in% c("Total", "Trend"))

bottom_df <- decomp_df %>% 
  filter(!Component %in% c("Total", "Trend"))

min_val_CA <- min(top_df$Value, na.rm = TRUE)
max_val_CA <- max(top_df$Value, na.rm = TRUE)

## US-CANADA CROSSING DECOMPOSING TIMESERIES BASEPLOT

p1_CA <- ggplot(top_df, aes(x = Date, y = Value)) +
  geom_line(color = "black", linewidth = 0.6) +
  annotate("text", x = top_df$Date[top_df$Value == min_val_CA] - 600, 
           y = 15000000, label = "Fewest Crossings: \n2.9M in Feb 2019", 
           color = "red", size = 3) +
  annotate("text", x = top_df$Date[top_df$Value == max_val_CA] + 600, 
           y = 18000000, label = "Most Crossings: \n12.8M in Aug 1996", 
           color = "darkgreen", size = 3) +
  facet_wrap(~Component, ncol = 1, scales = "free_y") +
  coord_cartesian(ylim = c(0, 28000000)) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y") +
  geom_vline(
    xintercept = top_df$Date[top_df$Value == min_val_CA],
    linetype = "dotted",
    color = "red") +
  geom_vline(
    xintercept = top_df$Date[top_df$Value == max_val_CA],
    linetype = "dotted",
    color = "darkgreen") +
  scale_y_continuous(breaks = c(10000000, 20000000, 30000000),  
                     labels = c("10M", "20M", "30M")) +
  labs(y = "People", x = NULL, 
       title = "People Crossing the U.S.-Canada Border (1996-2019)",
       subtitle = "Decomposition of Total Crossings into Trend, Seasonality, and Remainder") +
  SVU

p2_CA <- ggplot(bottom_df, aes(x = Date, y = Value)) +
  geom_line(color = "black", linewidth = 0.6) +
  facet_wrap(~Component, ncol = 1, scales = "free_y", labeller = labeller(Component = y_labels)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = c( -2000000, -1000000,
                                0, 1000000, 2000000),  
                     labels = c("-2M", "-1M", "0",
                                "1M", "2M")) +
  labs(x = NULL, y = "People") + SVU

canada_crossings <- p1_CA / p2_CA

canada_crossings

## US-MEXICO CROSSING DECOMPOSING TIMESERIES BASEPLOT WRANGLING

MX_data <- seasons_data %>% filter(Border == 'US-Mexico Border') %>%
  arrange(Date)

MX_ts <- ts(
  MX_data$total_people,
  start = c(year(min(CA_data$Date)), month(min(CA_data$Date))),
  frequency = 12
)

MX_decomp <- decompose(MX_ts)

decomp_df_MX <- tibble(
  Date = seq(
    from = as.Date(paste0(start(MX_ts)[1], "-", start(MX_ts)[2], "-01")),
    by = "month",
    length.out = length(MX_ts)),
  Total = as.numeric(MX_decomp$x),
  Trend = as.numeric(MX_decomp$trend),
  Seasonality = as.numeric(MX_decomp$seasonal),
  Remainder = as.numeric(MX_decomp$random)) %>%
  pivot_longer(
    cols = -Date,
    names_to = "Component",
    values_to = "Value")

decomp_df_MX$Component <- factor(
  decomp_df_MX$Component,
  levels = c("Total", "Trend", "Seasonality", "Remainder"))

top_df_MX <- decomp_df_MX %>% 
  filter(Component %in% c("Total", "Trend"))

bottom_df_MX <- decomp_df_MX %>% 
  filter(!Component %in% c("Total", "Trend"))

min_val <- min(top_df_MX$Value, na.rm = TRUE)
max_val <- max(top_df_MX$Value, na.rm = TRUE)

## US-MEXICO CROSSING DECOMPOSING TIMESERIES BASEPLOT

p1_MX <- ggplot(top_df_MX, aes(x = Date, y = Value)) +
  geom_line(color = "black", linewidth = 0.6) +
  annotate("text", x = top_df_MX$Date[top_df_MX$Value == min_val] - 600, 
           y = 5000000, label = "Fewest Crossings: \n11.4M in Feb 2011", 
           color = "red", size = 3) +
  annotate("text", x = top_df_MX$Date[top_df_MX$Value == max_val] - 600, 
           y = 15000000, label = "Most Crossings: \n26.2M in Mar 2001", 
           color = "darkgreen", size = 3) +
  facet_wrap(~Component, ncol = 1, scales = "free_y") +
  coord_cartesian(ylim = c(0, 28000000)) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y") +
  geom_vline(
    xintercept = top_df_MX$Date[top_df_MX$Value == min_val],
    linetype = "dotted",
    color = "red") +
  geom_vline(
    xintercept = top_df_MX$Date[top_df_MX$Value == max_val],
    linetype = "dotted",
    color = "darkgreen") +
  scale_y_continuous(breaks = c(10000000, 20000000, 30000000),  
    labels = c("10M", "20M", "30M")) +
  labs(y = "People", x = NULL, 
       title = "People Crossing the U.S.-Mexico Border (1996-2019)",
       subtitle = "Decomposition of Total Crossings into Trend, Seasonality, and Remainder") +
  SVU

p2_MX <- ggplot(bottom_df_MX, aes(x = Date, y = Value)) +
  geom_line(color = "black", linewidth = 0.6) +
  scale_y_continuous(breaks = c(-4000000, -3000000, -2000000, -1000000,
                                0, 1000000, 2000000, 3000000, 4000000),  
                     labels = c("-4M", "-3M", "-2M", "-1M", "0",
                                "1M", "2M", "3M", "4M")) +
  facet_wrap(~Component, ncol = 1, scales = "free_y", labeller = labeller(Component = y_labels)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(x = NULL, y = "People") + SVU

mexico_crossings <- p1_MX / p2_MX
 
map_plot

mexico_crossings
canada_crossings

bp_MX
bp_CA

boxes


ggsave("mx_timescale_test.pdf", mexico_crossings, device = cairo_pdf,
       width = 10, height = 7, units = "in", bg = "transparent")

ggsave("ca_timescale.pdf", canada_crossings,
       width = 10, height = 7, units = "in")

ggsave("mx_bp.pdf", bp_MX,
       width = 10, height = 7, units = "in")

ggsave("ca_bp.pdf", bp_CA,
       width = 10, height = 7, units = "in")

ggsave("map.pdf", map_plot,
       width = 10, height = 7, units = "in")

