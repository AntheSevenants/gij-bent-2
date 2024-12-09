library(stringr)
library(emmeans)
library(broom)
library(tidycat)

df <- read.delim("../data/tweets_geo_full.tsv")

num_tweets <- dim(df)[1]

MIN_TWEET_THRESHOLD = 10

# Make sure the factor levels are set up so *bent* is the non-default level
df$construction_type <- factor(df$construction_type, levels = c("zijt", "bent"))

# Make sure dialect is recognized as an "other coefficients" variable
df$dialect <- paste0("_is_dialect_", df$dialect)
# Turn dialect into a factor
df$dialect <- factor(df$dialect)

# Turn context into a factor
df$context <- factor(df$context, levels = c("main", "inversion", "other"))

# Make sure gender is recognized as an "other coefficients" variable
df["gender"][df["gender"] == ''] <- "unknown"
df$gender <- paste0("_is_gender_", df$gender)
# Turn gender into a factor, male = reference
df$gender <- factor(df$gender,
                    levels = c("_is_gender_male", "_is_gender_female", "_is_gender_unknown"))


# If a user is mentioned in the tweet, we consider it a "reply"
df$is_reply <- df$mentioned_users_count > 0
df$is_reply <- ifelse(df$is_reply, "reply_yes", "reply_no")
# Has to be turned into a factor to work in some plots
df$is_reply <- as.factor(df$is_reply)

# User ID should also be a factor
df$user_id <- as.factor(df$user_id)
df$username <- as.factor(df$username)

# Remove worthless Twitter coordinates and replace with my own
df <- df[, -which(names(df) %in% c("lat", "long"))]
colnames(df)[colnames(df) == "norm_lat"] <- "lat"
colnames(df)[colnames(df) == "norm_long"] <- "long"

# Build a frequency table so we know the user_id counts
user_id_counts <- table(df$user_id)
df <- subset(df, user_id %in%
              names(user_id_counts[user_id_counts >= MIN_TWEET_THRESHOLD]))

num_tweets_filtered <- dim(df)[1]
num_users_filtered <- length(unique(df$user_id))

# Only one tweet per user
one_user_one_tweet <- function(df) {
  df[!duplicated(df$user_id),]
}

no_unknown_gender <- function(df) {
  df <- subset(df, gender != "_is_gender_unknown")
  df$gender <- ifelse(df$gender == "_is_gender_male", "gender_male", "gender_female")
  df$gender <- factor(df$gender, levels = c("gender_male", "gender_female"))
  return(df)
}

