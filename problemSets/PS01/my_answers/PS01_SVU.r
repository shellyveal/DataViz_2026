# https://personal.lse.ac.uk/hix/EP%20Data/rcv_ep1.zip
library(tidyverse)
library(modeldata)
library(stargazer)
library(readr)
library(xtable)
theme_set(theme_minimal())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

ep1_rcv <- read_csv('rcv_ep1.txt')
mep_info <- read_csv('mep_info_26Jul11.csv')

ep1_long <- ep1_rcv %>% #converting to long format-- each row is a single vote
  pivot_longer(
    cols = starts_with("V"),
    names_to = "VOTE_ID",
    values_to = "VOTE_CAST"
  )

ep1_rcv <- ep1_rcv %>% select(where(~!all(is.na(.)))) # removing any empty cols

vote_counts <- table(ep1_long$VOTE_CAST) # compiling all votes
names(vote_counts) <- c("Absent", "Yes", "No", "Abstain",
                        "Present but did not vote",
                        "Non-MEP") #re-labeling vote types
vote_counts <- vote_counts[names(vote_counts) != "Non-MEP"] # removing non-MEPs

vote_counts_df <- as.data.frame(vote_counts)
colnames(vote_counts_df) <- c("Vote_Type", "Count")

xtable(vote_counts_df, caption = "Total Vote Counts for EP 1")

mep_info <- mep_info %>% select(where(~!all(is.na(.)))) # removing empty columns
names(mep_info)[1] <- "MEPID" # making MEP id column name identical

merged_df <- merge(mep_info, ep1_rcv, by = "MEPID", all = TRUE)
merged_df <- merged_df %>% mutate(across(c(`NOM-D1`, `NOM-D2`), as.numeric))

w_out_dims <- merged_df[-c(6,7)] #check for missingness (except in na-heavy cols)
rows_w_nas <- w_out_dims[!complete.cases(w_out_dims), ]
nrow(rows_w_nas)

problem_rows <- as.numeric(rownames(rows_w_nas))
print(problem_rows)

# ok so we can see that rows 45, 444, 470, 493, and 543 have issues
# 470 is the only one with data from ep1 that wasn't in mep_info
# 45, 444, 493, and 543 are data that were in mep_info but not ep1.

merged_df[470, 2] <- merged_df[470, 8] # bringing one missing name to "Name" 
#                                      # before dropping duplicate columns
problem_rows

merged_df[problem_rows, c(8:11)] <- merged_df[problem_rows, c(2:5)]

merged_df_99 <- merged_df[-2]
merged_df_99[is.na(merged_df_99)] <- 99
non_identicals <- merged_df_99[merged_df_99$EPG != merged_df_99$`EP Group`, ]
print(non_identicals) # checking for all the important cols, 
#                       none had any rows except Names

# now we are just going to drop ep1 cols, as they're identical
merged_df <- merged_df %>% select(!MEPNAME:EPG)

df_long <- merged_df %>% #converting to long format-- each row is a single vote
  pivot_longer(
    cols = starts_with("V"),
    names_to = "VOTE_ID",
    values_to = "VOTE_CAST"
  )

vote_sums <- as.data.frame.matrix(table(df_long$`EP Group`, df_long$VOTE_CAST))

vote_sums$YESPROP <- vote_sums$`1`/(vote_sums$`1` + 
                                      vote_sums$`2` + 
                                      vote_sums$`3`)

vote_sums$ABSPROP <- vote_sums$`3`/(vote_sums$`1` + 
                                      vote_sums$`2` + 
                                      vote_sums$`3`)

tapply(df_long$VOTE_CAST, list(df_long$`EP Group`, 
                               df_long$`NOM-D1`, 
                               df_long$`NOM-D2`), mean)

xtable(vote_sums)

sel <- !is.na(df_long$VOTE_CAST) &
  df_long$VOTE_CAST >= 1 &
  df_long$VOTE_CAST <= 3

d1_averages <- tapply(
  df_long$VOTE_CAST[sel],
  list(
    df_long$`EP Group`[sel],
    df_long$`NOM-D1`[sel]
  ),
  mean
)

d2_averages <- tapply(
  df_long$VOTE_CAST[sel],
  list(
    df_long$`EP Group`[sel],
    df_long$`NOM-D2`[sel]
  ),
  mean
)

# NOM-D1 By EP Group

pdf("1.1.pdf")
ggplot(data = na.omit(merged_df), aes(x = `EP Group`, y = `NOM-D1`,
                                      color = `EP Group`)) +
  scale_color_viridis_d() +
  geom_point(alpha = 0.5) +
  labs(title = "MEP NOMINATE D1 Coordinates by EP Group",
       x = "EP Group",
       y = "NOMINATE Dimension 1",
       color = "EP Group Code")
dev.off()

pdf("1.2.pdf")
ggplot(data = na.omit(merged_df), aes(x = `EP Group`, y = `NOM-D1`,
                                      color = `EP Group`)) +
  geom_boxplot() +
  scale_color_viridis_d() +
  stat_boxplot(geom = "errorbar", width = .3) +
  labs(title = "MEP NOMINATE D1 Coordinates by EP Group",
       x = "EP Group",
       y = "NOMINATE Dimension 1",
       color = "EP Group Code")
dev.off()

# NOM-D1 VS NOM-D2

pdf("2.1.pdf")
ggplot(data = na.omit(merged_df), aes(x = `NOM-D1`, y = `NOM-D2`, 
                      color = `EP Group`)) +
  geom_point(alpha = 0.5) +
  scale_color_viridis_d() +
  labs(title = "MEP NOMINATE Coordinates EP1",
       x = "NOMINATE Dimension 1",
       y = "NOMINATE Dimension 2",
       color = "EP Group Code")
dev.off()

# Boxplot of Yes vote proportions in EP Groups each vote

yes_means_yna <- df_long %>%
  group_by(`EP Group`, `VOTE_ID`) %>%
  summarize(mean_yes = sum(VOTE_CAST == 1, na.rm = TRUE)/
                       sum(VOTE_CAST %in% c(1,2,3), na.rm = TRUE))

pdf("3.1.pdf")
ggplot(data = na.omit(yes_means_yna), aes(x = `EP Group`, y = mean_yes*100,
                                      color = `EP Group`)) +
  geom_boxplot() +
  scale_color_viridis_d() +
  stat_boxplot(geom = "errorbar", width = .3) +
  labs(title = "Average 'Yes' votes per EP group (out of Yes/No/Abstain)",
       x = "EP Group",
       y = "Percent voting 'Yes' on any given issue",
       color = "EP Group Code")

dev.off()

yes_means <- df_long %>%
  group_by(`EP Group`, `VOTE_ID`) %>%
  summarize(mean_yes = sum(VOTE_CAST == 1, na.rm = TRUE)/
              sum(VOTE_CAST %in% c(0,1,2,3,4), na.rm = TRUE))

pdf("3.2.pdf")
ggplot(data = na.omit(yes_means), aes(x = `EP Group`, y = mean_yes*100,
                                      color = `EP Group`)) +
  geom_boxplot() +
  scale_color_viridis_d() +
  stat_boxplot(geom = "errorbar", width = .3) +
  labs(
    title = "Average 'Yes' votes per EP group (Including Absentees/non-votes)",
    x = "EP Group",
    y = "Percent voting 'Yes' on any given issue",
    color = "EP Group Code")

dev.off()

# Proportion voting Yes per NP 

vote_sums_np <- as.data.frame.matrix(table(df_long$`National Party`, 
                                           df_long$VOTE_CAST))

vote_sums_np$YESPROP <- vote_sums_np$`1`/(vote_sums_np$`1` + 
                                            vote_sums_np$`2` + 
                                            vote_sums_np$`3`)
pdf("4.1.pdf")
ggplot(data = vote_sums_np, aes(x = rownames(vote_sums_np), 
                                y = YESPROP*100)) +
  geom_col(fill = "darkgreen") +
  ylim(0, 100) +
  labs(title = "Percent Votes 'Yes' Per National Party EP1",
       x = "National Party Code",
       y = "Percent MEPs Voting 'Yes'") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) +
  coord_flip()
dev.off()








