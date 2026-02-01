#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
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

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("tidyverse", "ggplot2", "readxl", "xtable", "ggridges"),  pkgTest)
options(scipen = 999)
# set working directory to current parent folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
theme_set(theme_minimal())
#####################
# Data Manipulation
#####################

### 1

dataset_backup <- read_csv("../../../datasets/NCSS_v1.csv")
data <- dataset_backup

data <- data %>%
  select(
    CASEID,
    YEAR,
    GDREGION,
    NUMOFFMBR,
    TRAD6,
    TRAD12,
    INCOME
  )

### 2

data <- data %>%
  filter(TRAD6 == c("Chretiennes",
                    "Juives",
                    "Musulmanes"))

unique(data$TRAD6) # Checking to ensure filtered correctly

### 3

congregations_by_year <- data %>%
  group_by(TRAD6, YEAR) %>%
  summarize(
    N = n()
  )

view(congregations_by_year)

income_by_year <- data %>%
  group_by(TRAD6, YEAR) %>%
  summarize(
    mean_income = mean(INCOME, na.rm = TRUE),
    median_income = median(INCOME, na.rm = TRUE)
  )

view(income_by_year)

### 4

mean_2009 <- mean(data$INCOME[data$YEAR == 2009], na.rm = TRUE)
mean_2022 <- mean(data$INCOME[data$YEAR == 2022], na.rm = TRUE)

data <- data %>%
  mutate(
    AVG_INCOME = case_when(
      (YEAR = 2009 & INCOME >= mean_2009) ~ 1,
      (YEAR = 2009 & INCOME < mean_2009) ~ 0,
      (YEAR = 2022 & INCOME >= mean_2022) ~ 1,
      (YEAR = 2022 & INCOME < mean_2022) ~ 0
    )
  )

data$AVG_INCOME <- factor(data$AVG_INCOME,
                          levels = c(0, 1),
                          labels = c("Below Average", "Above Average"))


##########
# DATA VIZ
##########

### Question 1:

prop_12 <- data %>%
  group_by(TRAD12, YEAR) %>%
  summarize(N = n()) %>%
  group_by(YEAR) %>%
  mutate(PROPORTION = N/sum(N))

sum(prop_12$PROPORTION[prop_12$YEAR == 2009]) # Checking 2009
sum(prop_12$PROPORTION[prop_12$YEAR == 2022]) # Checking 2022

bp_1.1 <- ggplot(prop_12, aes(
    x = TRAD12, 
    y = PROPORTION,
    fill = TRAD12)) +
  ylim(0, .4) +
  geom_col() +
  facet_wrap(prop_12$YEAR, strip.position = "bottom") +
  scale_fill_viridis_d() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(
    title = "Proportion of Congregations for 2009, 2022",
    y = "Proportion",
    fill = "Congregation"
  )
ggsave("bar_plot_1.1.pdf", bp_1.1,
       width = 8, height = 5, units = "in")

bp_1.2 <- ggplot(prop_12, aes(
  x = TRAD12, 
  y = PROPORTION,
  fill = factor(YEAR))) +
  ylim(0, .4) +
  geom_col(position = "dodge2") +
  scale_fill_viridis_d() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 25, hjust = 0.85)) +
  labs(
    title = "Proportion of Congregations for 2009, 2022",
    y = "Proportion\n\n",
    fill = "Year"
  )

ggsave("bar_plot_1.2.pdf", bp_1.2,
       width = 12, height = 5, units = "in")

### Question 2: 

bp_2.1 <- ggplot(data, aes(x = TRAD6, y = NUMOFFMBR, fill = TRAD12)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d() +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(
    title = "Congregational Member Counts (2022)",
    y = "Number of Official Members",
    fill = "Congregation"
  )
ggsave("bar_plot_2.1.pdf", bp_2.1,
       width = 8, height = 5, units = "in")

### Question 3:

ridges_together <- ggplot(data %>% filter(!is.na(AVG_INCOME), YEAR == 2022), 
       aes(
         x = INCOME,
         y = GDREGION,
         fill = GDREGION,
         color = GDREGION)) +
  geom_density_ridges(alpha = 0.7) +
  facet_wrap(~ AVG_INCOME, labeller = as_labeller(data$AVG_INCOME)) +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Income",
       y = "Region",
       title = "Yearly Incomes by Region (2022)") +
  xlim(0, 6000000) +
  scale_fill_viridis_d() +
  scale_color_viridis_d()

ggsave("ridges_together.pdf", ridges_together,
       width = 10, height = 5, units = "in")


### Question 4:

box_plot <- ggplot(data, aes(x = GDREGION, y = NUMOFFMBR, fill = TRAD6, color = TRAD6)) +
  geom_boxplot() +
  scale_fill_viridis_d(alpha = 0.7) +
  scale_color_viridis_d() +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(
    title = "Congregational Member Counts (2022)",
    y = "Number of Official Members",
    fill = "Congregation"
  ) +
  ylim(0, 10000) +
  annotate("text", label = "Note: certain outliers not captured", x = 6.5, y = 10000) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

ggsave("box_plot.pdf", box_plot,
       width = 10, height = 5, units = "in")








### Question 3.2
ridges_above <- data %>% filter(!is.na(AVG_INCOME), YEAR == 2022, 
                                AVG_INCOME == 'Above Average')
ridges_above_plot <- ggplot(ridges_above, aes(
                            x = INCOME,
                            y = GDREGION,
                            fill = GDREGION,
                            color = GDREGION)) +
  geom_density_ridges(alpha = 0.7) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Income",
       y = "Region",
       title = "Yearly Incomes by Region (2022, Above Average)") +
  scale_fill_viridis_d() +
  scale_color_viridis_d()

ggsave("ridges_above.pdf", ridges_above_plot,
       width = 10, height = 5, units = "in")

ridges_below <- data %>% filter(!is.na(AVG_INCOME), YEAR == 2022, 
                                AVG_INCOME == 'Below Average')
ridges_below_plot <- ggplot(ridges_below, aes(
  x = INCOME,
  y = GDREGION,
  fill = GDREGION,
  color = GDREGION)) +
  geom_density_ridges(alpha = 0.7) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Income",
       y = "Region",
       title = "Yearly Incomes by Region (2022, Below Average)") +
  scale_fill_viridis_d() +
  scale_color_viridis_d()

ggsave("ridges_below.pdf", ridges_below_plot,
       width = 10, height = 5, units = "in")


### Question 4.2:

box_plot_wrapped <- ggplot(data, aes(x = GDREGION, y = NUMOFFMBR, fill = TRAD6, color = TRAD6)) +
  geom_boxplot() +
  scale_fill_viridis_d(alpha = 0.7) +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~TRAD6, scales = "free_y") +
  scale_color_manual(name = "Religion",
                     labels = c("Chretiennes", "Juives", "Musulmanes"),
                     values = c("#440154", "#21918c", "#fde725")) +
  labs(
    title = "Congregational Member Counts (2022)",
    y = "Number of Official Members",
    fill = "Religion"
  ) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

ggsave("box_plot_wrapped.pdf", box_plot_wrapped,
       width = 10, height = 5, units = "in")




#Question 2.2
bp_2.2 <- ggplot(data %>% filter(YEAR == 2022),
                  aes(x = TRAD12, y = NUMOFFMBR, fill = TRAD12)) +
  geom_col(position = "dodge") +
  facet_wrap(~ TRAD6, scales = "free_y") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1.5, "lines"),
    panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.8)
  ) +
  labs(
    title = "Number of Official Members by Religion (2022)\n",
    x = "",
    y = "Number of Official Members\n\n\n",
    fill = "Congregation"
  )

ggsave("bar_plot_2.2.pdf", bp_2.2,
       width = 10, height = 5, units = "in")
