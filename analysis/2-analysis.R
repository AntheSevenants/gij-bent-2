library(ggplot2)
library(magrittr)

# Load the dataset
source("0-common.R")

#
# Plots
#

coeff <- read.csv("../output/gij_bent_coefficients_two_genders.csv")

coeff <- merge(x = coeff %>% subset(substr(feature, 1, 1) != "_"),
               y = df %>% one_user_one_tweet,
               by.x = "feature",
               by.y = "user_id",
               all.x = TRUE)
coeff$log_followers = log(coeff$user_followers_count)
coeff$log_following = log(coeff$user_friends_count)
coeff$log_tweet_count = log(coeff$user_tweet_count)
coeff$influence = log((coeff$user_followers_count + 0.001) / (coeff$user_friends_count + 0.001))

coeff %>%
  ggplot(aes(x = coefficient)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.5) +
  geom_vline(xintercept=0, color="black", linewidth=1, linetype="dotdash")

coeff %>% no_unknown_gender %>%
  ggplot(aes(x = gender, y = coefficient, fill = gender)) +
  geom_violin() +
  geom_hline(yintercept=0, color="black", linewidth=1, linetype="dotdash")

coeff %>%
  ggplot(aes(x = dialect, y = coefficient, fill = dialect)) +
  geom_violin() +
  geom_hline(yintercept=0, color="black", linewidth=1, linetype="dotdash")

coeff %>% 
  ggplot(aes(y=coefficient)) +
  geom_boxplot() +
  coord_flip()

plot(coeff$coefficient, coeff$log_followers)

#
# Cross tables
#

xtabs(~ dialect, coeff %>% filter(coefficient > 0))
xtabs(~ dialect, coeff %>% filter(coefficient < 0))

#
# Find forefront
#
brabant_snippet <- coeff %>% filter(dialect == "_is_dialect_Brabants") %>% arrange(coefficient)
fit <- lm(coefficient ~ log_followers + gender, data=brabant_snippet %>% no_unknown_gender())
summary(fit)


limburg_snippet <- coeff %>% filter(dialect == "_is_dialect_Limburgs") %>% arrange(coefficient)
fit <- lm(coefficient ~ log_followers + gender, data=limburg_snippet %>% no_unknown_gender())
summary(fit)

#
# Measures
#

full_range <- function(vector) {
  returned_range <- range(vector)
  return(abs(returned_range[[2]] - returned_range[[1]]))
}

# Range
subset(coeff, gender == "_is_gender_male")$coefficient %>% full_range
subset(coeff, gender == "_is_gender_female")$coefficient %>% full_range

# Variance

subset(coeff, gender == "_is_gender_male")$coefficient %>% var
subset(coeff, gender == "_is_gender_female")$coefficient %>% var

ansari.test(coefficient ~ gender, data = coeff) # otherwise, option a

#
# Individuals
#
extreme_limburgers <- coeff %>% filter(coefficient < 0 & dialect == "_is_dialect_Limburgs") %>% no_unknown_gender()
xtabs(~ gender, extreme_limburgers)

extreme_brabanders <- coeff %>% filter(coefficient > 0 & dialect == "_is_dialect_Brabants") %>% no_unknown_gender()
xtabs(~ gender, extreme_brabanders)
lm(coefficient ~ gender, data=extreme_brabanders) %>% summary
mean(extreme_brabanders %>% filter(gender == "_is_gender_female") %>% .$coefficient)
mean(extreme_brabanders %>% filter(gender == "_is_gender_male") %>% .$coefficient)

fanatic_zijt_user <- coeff[which.min(coeff$coefficient),]
fanatic_bent_user <- coeff[which.max(coeff$coefficient),]

#
# Analysis
#

fit <- lm(coefficient ~ influence + dialect + gender, data=coeff %>% no_unknown_gender())
summary(fit)

posthoc <- emmeans(fit,
                   pairwise ~ dialect,
                   type="response",
                   weights="proportional")
posthoc$contrasts

ggpredict(fit, "gender") %>% plot

wilcox.test(coefficient ~ gender, data = coeff %>% no_unknown_gender, var.equal=T)

write.csv(coeff %>% arrange(coefficient), "../output/gij_bent_coefficients_rich.csv", row.names = FALSE)
