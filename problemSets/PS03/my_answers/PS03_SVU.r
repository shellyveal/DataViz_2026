#####################
# load libraries
# set wd
# clear global .envir
#####################

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

lapply(c("tidyverse", "ggplot2", "readxl", "xtable", "ggridges",
         "patchwork", "showtext", "ggtext", "ggrepel",
         "extrafont", "WDI", "purrr", "systemfonts"),  pkgTest)
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
        # Bold, bigger title
        plot.title = element_text(margin = margin(t = 10, b = 10), 
                                  face = "bold", size = rel(1.7)),
        # Plain, slightly bigger subtitle that is grey
        plot.subtitle = element_text(face = "plain", size = rel(1.3),
                                     color = "grey40"),
        # Italic, smaller, grey caption that is left-aligned
        plot.caption = element_text(face = "italic", size = rel(0.7), 
                                    color = "grey70", hjust = 0),
        # Bold legend titles
        legend.title = element_text(face = "bold"),
        # Bold, slightly larger facet titles that are centered
        strip.text = element_text(face = "bold", 
                                  size = rel(1.1), hjust = 0.5),
        # Bold axis titles
        axis.title = element_text(face = "bold"),
        # Add some space above the x-axis title
        axis.title.x = element_text(margin = margin(t = 10, 
                                                    b = 10), hjust = 0.5),
        # Add some space to the right of the y-axis title
        axis.title.y = element_text(margin = margin(r = 10,
                                                    l = 8), hjust = 0.5),
        # Facet titles grey background
        strip.background = element_rect(fill = "grey90", color = NA),
        # Facet panel border
        panel.border = element_rect(color = "grey90", fill = NA),
        # color / fill for discrete scales
        palette.colour.discrete = function(n)
          scales::pal_viridis(option = "G")(n),
        palette.fill.discrete = function(n) 
          scales::pal_viridis(option = "G")(n),
        # color / fill for continuous scales
        palette.colour.continuous = scales::pal_viridis(option = "G"),
        palette.fill.continuous = scales::pal_viridis(option = "G"))

#####################
# Data Manipulation
#####################

### 1

dataset_backup <- read_csv("../../../datasets/CES2015.csv")
data <- dataset_backup

data <- data %>% filter(discard == "Good quality") %>% select(
  date, age, p_voted, province, income_full, p_selfplace, vote_for, sex_r)

### 2

data$p_voted[data$p_voted == "Yes"] <- 1
data$p_voted[data$p_voted == "No"] <- 0
data$p_voted <- as.numeric(ifelse(data$p_voted == 1 | data$p_voted == 0,
                       data$p_voted, NA))

### 3

data$date <- dmy(data$date)
data$age_groups <- as.numeric(format(data$date, "%Y")) - as.numeric(data$age) 
data <- data %>% mutate(age_groups = case_when(
  age_groups < 30 ~ "<30",
  age_groups >= 30 & age_groups < 45 ~ "30-44",
  age_groups >= 45 & age_groups < 60 ~ "45-59",
  age_groups >= 60 & age_groups < 75 ~ "60-74",
  age_groups >= 75 ~ "75+"
))

data$age_groups

##########
# DATA VIZ
##########

### Question 1:

turnout_data <- data %>%
  drop_na(age_groups) %>%
  group_by(age_groups) %>%
  summarize(turnout = mean(p_voted, na.rm = TRUE))

no_nas <- ggplot(turnout_data, aes(x = age_groups, y = turnout*100)) + 
  geom_col(fill = "#004999") +
  ylim(0, 100) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Age Groups (Years)",
       y = "Turnout (%)",
       title = "Turnout by Age Group")

turnout_data_nas <- data %>%
  mutate(p_voted = case_when(
    is.na(p_voted) ~ 0,
    !is.na(p_voted) ~ p_voted
  )) %>%
  drop_na(age_groups) %>%
  group_by(age_groups) %>%
  summarize(turnout = mean(p_voted))

w_nas <- ggplot(turnout_data_nas, aes(x = age_groups, y = turnout*100)) + 
  geom_col(fill = "steelblue") +
  ylim(0, 100) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Age Groups (Years)",
       y = element_blank(),
       title = "Turnout by Age Group",
       subtitle = "(Missing Data Included)")

turnout_graphs <- no_nas + SVU + w_nas + SVU
turnout_graphs




### Question 2:

q2_data <- data %>% mutate(p_selfplace = as.numeric(p_selfplace))
q2_data <- q2_data %>% 
  filter(!is.na(p_selfplace) & !is.na(vote_for)) %>% 
  filter(p_selfplace != 1000) %>%
  filter(vote_for == "Liberal" | vote_for == "Conservatives" |
           vote_for == "Bloc Quebecois" | vote_for == "Green Party" |
           vote_for == "ndp")

plot_2.1 <- ggplot(q2_data %>% group_by(vote_for), aes(x = p_selfplace, 
                                           y = vote_for,
                                           fill = vote_for,
                                           color = vote_for)) +
  geom_density_ridges(alpha = 0.6) + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Left (0) - Right (10) Ideology Self-Placement",
       y = "Party Affiliation",
       title = "Ideology Self-Placement by Party") +
  scale_x_continuous(
    breaks = seq(0, 10, by = 1),
    limits = c(0, 10)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d()

plot_2.2 <- plot_2.1 + SVU + theme(legend.position = "none")

### Question 3:

prov_data <- data %>% 
  filter(income_full != ".d" & income_full != ".r" &
        !is.na(income_full) & province != 1000 &
        province != "Yukon" & province != "nunavut" &
        province != "Quebec" & province != "Ontario")

ont_que_data <- data %>% filter(income_full != ".d" & income_full != ".r" &
                               !is.na(income_full))
ont_que_data <- ont_que_data %>% 
  filter(province == "Quebec" | province == "Ontario")
prov_data$income_full <- factor(prov_data$income_full, 
                                ordered = TRUE, levels = c(
  "less than $29,999", "between $30,000 and $59,999",
  "between $60,000 and $89,999", "between $90,000 and $109,999",
  "more than $110,000"))
ont_que_data$income_full <- factor(ont_que_data$income_full, 
                                   ordered = TRUE, levels = c(
  "less than $29,999", "between $30,000 and $59,999",
  "between $60,000 and $89,999", "between $90,000 and $109,999", 
  "more than $110,000"))
# plot with all provinces except ontario and quebec, NA's included (prov_na)
province_labs <- c("Alberta", "British Columbia", "Manitoba",
                   "New Brunswick", "Newfoundland", "Nova Scotia", 
                   "P.E. Island", "Saskatchewan")
names(province_labs) <- c("Alberta", "bc", "Manitoba", "nb", "Nfld",
                          "ns", "pei", "Sask")

prov_na <- ggplot(prov_data, aes(x = income_full, fill = factor(p_voted))) + 
  geom_histogram(stat = "count", position = "dodge") +
  scale_x_discrete(labels = c("less than $29,999" = "<$29,000",
                              "between $30,000 and $59,999" = "$30,000-$59,999",
                              "between $60,000 and $89,999" = "60,000-$89,999",
                              "between $90,000 and $109,999" = "90,000-$109,999",
                              "more than $110,000" = ">$110,000")) +
  facet_wrap(~province, labeller = labeller(province = province_labs)) + 
  theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(
    x = "Income Group",
    title = "Voting Turnout by Income Group",
    y = "Voted (count)",
    fill = "Voted (yes/no/NA)"
  ) +
  scale_fill_discrete(labels = c("No", "Yes", "N/A"))


# plot with ontario and quebec, NA's included
ont_que_na <- ggplot(ont_que_data, aes(x = income_full, fill = factor(p_voted))) + 
  geom_bar(position = "dodge") +
  scale_x_discrete(labels = c("less than $29,999" = "<$29,000",
                              "between $30,000 and $59,999" = "$30,000-$59,999",
                              "between $60,000 and $89,999" = "60,000-$89,999",
                              "between $90,000 and $109,999" = "90,000-$109,999",
                              "more than $110,000" = ">$110,000")) +
  facet_wrap(~province) + 
  theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ylim(0, 500) +
  labs(
    x = "Income Group",
    title = "Voting Turnout by Income Group: Ontario and Quebec",
    subtitle = "A closer look at the two highest population provinces",
    y = "Voted (count)",
    fill = "Voted (yes/no/NA)"
  ) +
  scale_fill_discrete(labels = c("No", "Yes", "N/A"))

# plot with all provinces except ontario and quebec (AND YUKON AND NUNAVUT DUE TO HOW FEW DATAPOINTS THEY HAVE), NA's NOT included
prov_base <- ggplot(prov_data %>% filter(!is.na(p_voted)),
       aes(x = income_full, fill = factor(p_voted))) + 
  geom_histogram(stat = "count", position = "dodge") +
  scale_x_discrete(labels = c("less than $29,999" = "<$29,000",
                              "between $30,000 and $59,999" = "$30,000-$59,999",
                              "between $60,000 and $89,999" = "60,000-$89,999",
                              "between $90,000 and $109,999" = "90,000-$109,999",
                              "more than $110,000" = ">$110,000")) +
  facet_wrap(~province, labeller = labeller(province = province_labs)) + 
  theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(
    x = "Income Group",
    title = "Voting Turnout by Income Group",
    y = "Voted (count)",
    fill = "Voted (yes/no)"
  ) +
  scale_fill_discrete(labels = c("No", "Yes"))

# plot with ontario and quebec, NA's NOT included
ont_que <- ggplot(ont_que_data %>% filter(!is.na(p_voted)), 
  aes(x = income_full, fill = factor(p_voted))) + 
  geom_histogram(stat = "count", position = "dodge") +
  scale_x_discrete(labels = c("less than $29,999" = "<$29,000",
                              "between $30,000 and $59,999" = "$30,000-$59,999",
                              "between $60,000 and $89,999" = "60,000-$89,999",
                              "between $90,000 and $109,999" = "90,000-$109,999",
                              "more than $110,000" = ">$110,000")) +
  facet_wrap(~province) + 
  theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ylim(0, 500) + 
  labs(
    x = "Income Group",
    title = "Voting Turnout by Income Group: Ontario and Quebec",
    subtitle = "A closer look at the two highest population provinces",
    y = "Voted (count)",
    fill = "Voted (yes/no)"
  ) +
  scale_fill_discrete(labels = c("No", "Yes"))



plot_3.1 <- prov_base + SVU + theme(axis.text.x = element_text(
  angle = 25, hjust = 1, vjust = 1),                      
  panel.spacing = unit(2, "lines"))

plot_3.2 <- prov_na + SVU + theme(axis.text.x = element_text(
  angle = 25, hjust = 1, vjust = 1),
  panel.spacing = unit(2, "lines"))

plot_3.3 <- ont_que + SVU + theme(axis.text.x = element_text(
  angle = 25, hjust = 1, vjust = 1))

plot_3.4 <- ont_que_na + SVU + theme(axis.text.x = element_text(
  angle = 25, hjust = 1, vjust = 1))


density_peaks <- q2_data %>%
  group_by(vote_for) %>%
  summarize({
    dens <- density(p_selfplace, from = 0, to = 10, na.rm = TRUE)
    tibble(
      x = dens$x[which.max(dens$y)],
      peak_height = max(dens$y))}) %>%
  mutate(y_group = c(2.35, 3.5, 4.5, 5.5, 6.6))


view(density_peaks)


plot_2.3 <- ggplot(q2_data %>% group_by(vote_for), aes(x = p_selfplace, 
                                                       y = vote_for,
                                                       fill = vote_for,
                                                       color = vote_for)) +
  geom_density_ridges(alpha = 0.6) + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  geom_label_repel(
    data = density_peaks,
    aes(
      x = x,
      y = y_group + (peak_height * 1.2) + 0.1,
      label = paste0(vote_for, ": ", round(x, 2))),
    color = "black",
    fill = "white",
    size = 3,
    show.legend = FALSE,
    segment.color = NA
  ) +
  labs(x = "Left (0) - Right (10) Ideological Self-Placement",
 subtitle = "Peak response level points labeled for each party",
 y = "Party Affiliation",
 title = "Ideological Self-Placement by Party",
caption = "Source: Canadian Election Survey 2015 \nParty Affiliation Grouped by Respondents' Naming the Party they Intend to Vote for") +
  scale_x_continuous(
    breaks = seq(0, 10, by = 1),
    limits = c(0, 10))
  

plot_2.4 <- plot_2.3 + SVU + theme(legend.position = "none")

plot_2.4

ggsave("plot_2.4.pdf", plot_2.4,
          width = 12, height = 5, units = "in")

geom_label_repel(
  data = density_peaks,
  aes(
    x = x,
    y = as.numeric(factor(vote_for)) + 0.2,  # slightly above each ridge
    label = paste0(round(x, 2))
  ),
  nudge_y = 0.2,
  direction = "y",
  segment.curvature = 0.2,
  segment.ncp = 3,
  segment.angle = 20,
  fill = "white",
  color = "black",
  size = 3,
  show.legend = FALSE
) 
