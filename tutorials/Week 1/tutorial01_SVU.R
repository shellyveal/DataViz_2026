######################
# Data Viz  
# Tutorial 1:  
# Tidyverse and ggplot        
######################

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
lapply(c("tidyverse", "ggplot2"),  pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#################
### In class demo
#################
view(AB_ZIM)
# load data
AB_ZIM <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/DataViz_2026/refs/heads/main/datasets/AB_ZIM.csv")
# reduce data
AB_ZIM <- AB_ZIM |> select(Q1, Q101, Q102, Q94A, Q97, Q98)
# organize data
AB_ZIM <- AB_ZIM |> 
  # rename some columns
  rename(age = `Q1`, 
         gender = `Q101`,
         interview_lang = `Q102`,
         employed = `Q94A`,
         religion = `Q97`,
         party_vote = `Q98`) 

# histogram example
pdf("AB_ZIM_hist1.pdf")
ggplot(data = AB_ZIM, aes(age)) + 
  geom_histogram(binwidth = 2) 
dev.off()

pdf("AB_ZIM_hist2.pdf")
ggplot(data = AB_ZIM, aes(age)) +
  geom_histogram(binwidth = 2) + 
  scale_y_continuous(breaks = c(20, 40, 60, 80, 100))
dev.off()

pdf("AB_ZIM_hist3.pdf")
ggplot(data = AB_ZIM, aes(x=age)) +
  geom_histogram(binwidth = 2) + 
  scale_y_continuous(breaks = c(20, 40, 60, 80, 100)) +
  facet_wrap(vars(gender))
dev.off()

pdf("AB_ZIM_hist4.pdf")
ggplot(data = AB_ZIM, aes(x=age, fill=gender)) +
  geom_histogram(binwidth = 2) + 
  scale_y_continuous(breaks = c(20, 40, 60, 80, 100)) +
  facet_wrap(vars(gender)) +
  coord_flip()
dev.off()

pdf("AB_ZIM_hist5.pdf")
ggplot(data = AB_ZIM, aes(x=age, fill=gender)) +
  geom_histogram(binwidth = 2) + 
  scale_y_continuous(breaks = c(20, 40, 60, 80, 100)) +
  facet_wrap(vars(gender)) +
  coord_flip() +
  labs(x="\nAge", y="\nCount", fill="Gender")
dev.off()

pdf("AB_ZIM_hist6.pdf")
ggplot(data = AB_ZIM, aes(x=age, fill=gender)) +
  geom_histogram(binwidth = 2) + 
  scale_y_continuous(breaks = c(20, 40, 60, 80, 100)) +
  facet_wrap(vars(gender)) +
  coord_flip() +
  labs(x="\nAge", y="\nCount", fill="Gender") +
  theme_bw()
dev.off()

##############
### Group work
##############

# (1) Organize data yourself in groups using tidy
# (2) Create informative plots of example RQs
# (3) Start to add basic elements using ggplot

# Research questions: 
# What is the relationship between social demographic characteristics (education, employment, age, gender)
# & informal politics (official political parties vs traditional leaders)?

# download data from AfroBarometer (Malawi R10): https://www.afrobarometer.org/survey-resource/malawi-round-10-data-2024/
# here is the codebook: https://www.afrobarometer.org/survey-resource/malawi-round-10-codebook-2024/
AB_MALAWI <- read.csv("MLW.csv")
View(AB_MALAWI)

AB_MALAWI <- AB_MALAWI |> select(URBRUR, Q1, Q101, Q102, Q94A, Q97, Q98, Q12B, Q12C)
# reduce your data to these variables: 
# URBRUR - urban/rural respondent
# Q1 - age 
# Q101 - gender
# Q102 - interview language
# Q94A - employed
# Q97 - religion
# Q98 - voted for party in last election
# Q12B - contacted party official
# Q12C - contacted traditional leader

# rename your variables to informative/easy names
names(AB_MALAWI) <- c("urban", "age", "gender", "language", "employed", "religion",
                      "voted", "contacted_po", "contacted_tl")

# create a couple of visualizations that shed light on our RQ
AB_MALAWI$age <- as.integer(AB_MALAWI$age)

ggplot(AB_MALAWI, aes(x = age,
                      y = contacted_tl)) +
  geom_point()

# (we will present your "findings" to the class)

AB_MALAWI <- AB_MALAWI %>%
  mutate(contacted_po = recode(contacted_po,
                               "Don't Know" = 0,
                               "Refused to answer" = 0,
                               "Never" = 1,
                               "Only once" = 2,
                               "A few times" = 3,
                               "Often" = 4))

AB_MALAWI <- AB_MALAWI %>%
  mutate(contacted_tl = recode(contacted_tl,
                               "Don't Know" = 0,
                               "Refused to answer" = 0,
                               "Never" = 1,
                               "Only once" = 2,
                               "A few times" = 3,
                               "Often" = 4))

view(AB_MALAWI)

tl_contact_age <- ggplot(data = AB_MALAWI, aes(x=age, fill=contacted_tl)) +
  geom_histogram(binwidth = 2) + 
  scale_y_continuous(breaks = c(20, 40, 60, 80, 100)) +
  labs(x="\nAge", y="\nCount", fill="Contact", title = "Traditional Leader Contact") +
  theme_bw()


po_contact_age <- ggplot(data = AB_MALAWI, aes(x=age, fill=contacted_po)) +
  geom_histogram(binwidth = 2) + 
  scale_y_continuous(breaks = c(20, 40, 60, 80, 100)) +
  labs(x="\nAge", y="\nCount", fill="Contact", title = "Party Official Contact") +
  theme_bw()

install.packages("patchwork")
library(patchwork)
tl_contact_age + po_contact_age

AB_MALAWI_long <- AB_MALAWI %>%
  pivot_longer(cols = c(contacted_tl, contacted_po),
               names_to = "contact_type",
               values_to = "frequency")

# Filter to keep only "often"
AB_MALAWI_long <- AB_MALAWI_long %>%
  filter(frequency == "Often")

# Now plot
contact_type_plot <- ggplot(AB_MALAWI_long, aes(x = age, fill = contact_type)) +
  geom_histogram(binwidth = 4, position = "dodge") +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  labs(x = "\nAge",
       y = "\nCount",
       fill = "Contacted",
       title = "Frequent Contact Type by Age") +
  scale_fill_discrete(labels = c("Contacted Party Official", 
                                  "Contacted Traditional Leader")) +
  theme_bw()

AB_MALAWI_long


tl_contact_age + po_contact_age
contact_type_plot
