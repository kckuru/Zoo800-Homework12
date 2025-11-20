###################
### Homework 12 ###
###################

# Group members: Keeley Kuru, Joey Munoz
# Date: 11/18/25

# ===== Objective 1 ===== # 
# Objective 1a
library(tidyverse)
library(lubridate)
library(readxl)

# Load data
bsb <- read_xlsx("BSB_tagging_data.xlsx") 

# Convert dates and extract month; create month_recap column
bsb <- bsb %>%
  mutate(Date_at_recapture = as.Date(Date_at_recapture), # as.Date converts to date format
         month_recap = month(Date_at_recapture)) # extract month from recapture date

# Keep only those recaptured after July (i.e., month >= 8)
bsb_afterJuly <- bsb %>% filter(month_recap >= 8) 

# Keep only individuals that were female at capture 
females <- bsb_afterJuly %>% filter(Sex_at_capture == "F") 

# Identify individuals that changed sex
females <- females %>% 
  mutate(changed = ifelse(Sex_at_recapture %in% c("M", "I"), 1, 0)) # 1 if changed, 0 if not

# Count
n_changed <- sum(females$changed, na.rm = TRUE) 
n_total   <- nrow(females)

n_changed; n_total 

alpha <- 1 + n_changed               # number of successes
beta  <- 1 + (n_total - n_changed)  # number of failures

x <- seq(0, 1, length.out = 500) # x values for plotting the posterior density; length.out is number of points

pdf_vals <- dbeta(x, alpha, beta) # posterior density values

plot(x, pdf_vals, type = "l", lwd = 3,
     xlab = "Probability of sex change",
     ylab = "Beta posterior density",
     main = "Posterior PDF for Probability of Sex Change (Females Recaptured After July)")

# Objective 1b
# 95% credible interval
lower <- qbeta(0.025, alpha, beta) # lower bound
upper <- qbeta(0.975, alpha, beta) # upper bound 

c(lower, upper)


# ===== Objective 2 ===== #
# Objective 2a
# Does length influence probability of sex change?
model <- glm(changed ~ Length_at_capture,
             data = females,
             family = binomial)

summary(model)
# Length significantly influences the probability of sex change for females recaptured after July.
# The p-value = 0.0416, which is < 0.05,
# Therefore: longer females are more likely to change sex.

# Objective 2b
# How much do log-odds change per mm of length?
coef_length <- coef(model)["Length_at_capture"] # coef_length extracts the coefficient for Length_at_capture
coef_length
# For every 1-mm increase in length, the log-odds of sex change increase by 0.0853.

# Objective 2c
library(ggplot2)

# Create a sequence of lengths to predict over
new_lengths <- data.frame(
  Length_at_capture = seq(
    min(females$Length_at_capture, na.rm = TRUE),
    max(females$Length_at_capture, na.rm = TRUE),
    length.out = 200
  )
)

# Get predicted probabilities from the model
new_lengths$predicted_prob <- predict(
  model,
  newdata = new_lengths,
  type = "response"   # converts log-odds to probability
)

# Plot observed data + model-estimated curve
ggplot(females, aes(x = Length_at_capture, y = changed)) +
  geom_jitter(width = 0.5, height = 0.05, alpha = 0.4) + # jittered points for observed data
  geom_line(
    data = new_lengths,
    aes(x = Length_at_capture, y = predicted_prob),
    size = 1.2,
    color = "blue"
  ) + 
  labs(
    x = "Length at Capture (mm)",
    y = "Probability of Sex Change",
    title = "Probability of Sex Change vs. Length in Female Black Sea Bass",
    caption = "Figure: Black points show observed sex-change outcomes (0 = remained female, 1 = changed\nsex or intersex). The blue curve shows the logistic regression estimate of the probability of sex\nchange as a function of length. Only females recaptured after July were included in this analysis."
  ) +
  theme_bw()
# Figure explanation:
# The blue curve shows the model’s prediction of sex-change probability as length increases.
# Since the slope was positive and significant (p = 0.0416), the curve should slope upward, meaning
# larger females are more likely to transition.
