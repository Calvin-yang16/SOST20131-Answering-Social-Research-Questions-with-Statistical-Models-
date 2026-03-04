# ============================================================
# 3.0 Load the data
# ============================================================
diabetes <- read.csv("Diabetes.csv")
# ============================================================
# 3.2 Data Description and Exploration
# ============================================================
library(dplyr)
library(ggplot2)
# 1) Function to create count & proportion tables
binary_vars <- c("HighBP", "HighChol", "Smoker", "Fruits")
lapply(binary_vars, function(var) {
  diabetes %>%
    group_by(Diabetes, .data[[var]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(Diabetes) %>%
    mutate(prop = n / sum(n))
})
# 2) Summary statistics for BMI and Age by diabetes status
diabetes %>%
  group_by(Diabetes) %>%
  summarise(
    mean_BMI = mean(BMI, na.rm = TRUE),
    sd_BMI   = sd(BMI, na.rm = TRUE),
    min_BMI  = min(BMI, na.rm = TRUE),
    max_BMI  = max(BMI, na.rm = TRUE),
    
    mean_Age = mean(Age, na.rm = TRUE),
    sd_Age   = sd(Age, na.rm = TRUE),
    min_Age  = min(Age, na.rm = TRUE),
    max_Age  = max(Age, na.rm = TRUE)
  )

# 3) Preparation 
library(ggplot2)

# 3.1) Preparation:Recode variables to factors with clear labels ----
diabetes$Diabetes <- factor(
  diabetes$Diabetes,
  levels = c(0, 1),
  labels = c("No diabetes", "Prediabetes / Diabetes")
)

diabetes$Fruits <- factor(
  diabetes$Fruits,
  levels = c(0, 1),
  labels = c("No daily fruit intake", "Daily fruit intake")
)

diabetes$HighBP <- factor(
  diabetes$HighBP,
  levels = c(0, 1),
  labels = c("No high blood pressure", "High blood pressure")
)

diabetes$HighChol <- factor(
  diabetes$HighChol,
  levels = c(0, 1),
  labels = c("No high cholesterol", "High cholesterol")
)

diabetes$Smoker <- factor(
  diabetes$Smoker,
  levels = c(0, 1),
  labels = c("Non-smoker", "Smoker")
)

# 3.2)  Common settings
fill_cols <- c("lightblue", "steelblue")  # consistent fill across all proportion plots
bar_border <- "grey30"                    # crisp borders for print/PDF

#  4) plots 
# 4.1) Proportion plots:binary predictors vs Diabetes
# Fruits vs Diabetes
ggplot(diabetes, aes(x = Fruits, fill = Diabetes)) +
  geom_bar(position = "fill", colour = bar_border) +
  scale_fill_manual(values = fill_cols) +
  labs(
    x = "Fruit consumption",
    y = "Proportion",
    fill = "Diabetes status"
  ) +
  theme_minimal()

# HighBP vs Diabetes
ggplot(diabetes, aes(x = HighBP, fill = Diabetes)) +
  geom_bar(position = "fill", colour = bar_border) +
  scale_fill_manual(values = fill_cols) +
  labs(
    x = "Blood pressure status",
    y = "Proportion",
    fill = "Diabetes status"
  ) +
  theme_minimal()

# HighChol vs Diabetes
ggplot(diabetes, aes(x = HighChol, fill = Diabetes)) +
  geom_bar(position = "fill", colour = bar_border) +
  scale_fill_manual(values = fill_cols) +
  labs(
    x = "Cholesterol status",
    y = "Proportion",
    fill = "Diabetes status"
  ) +
  theme_minimal()

# Smoker vs Diabetes
ggplot(diabetes, aes(x = Smoker, fill = Diabetes)) +
  geom_bar(position = "fill", colour = bar_border) +
  scale_fill_manual(values = fill_cols) +
  labs(
    x = "Smoking status",
    y = "Proportion",
    fill = "Diabetes status"
  ) +
  theme_minimal()

# 4.2) Boxplots (measured variables by Diabetes status) 

# BMI by Diabetes status
ggplot(diabetes, aes(x = Diabetes, y = BMI)) +
  geom_boxplot(colour = bar_border) +
  labs(
    x = "Diabetes status",
    y = "BMI"
  ) +
  theme_minimal()

# Age by Diabetes status
ggplot(diabetes, aes(x = Diabetes, y = Age)) +
  geom_boxplot(colour = bar_border) +
  labs(
    x = "Diabetes status",
    y = "Age"
  ) +
  theme_minimal()

# ============================================================
# 3.3. initial logistic regression model
# ============================================================
# 1) Before we start the model fitting procedure we will make test-train split data in the proportion of 80:20, as we will later on assess model accuracy in correct prediction using the test data set
nrow(diabetes)
set.seed(123)
split_idx = sample(nrow(diabetes), 120)

diabetes_train <- diabetes[split_idx, ]
diabetes_test  <- diabetes[-split_idx, ]

# 2) check the balance 
prop.table(table(diabetes_test$Diabetes))
# 3) Initial model
model_initial <- glm(
  Diabetes ~ HighBP + HighChol + BMI + Smoker + Fruits + Age,
  data = diabetes_train,
  family = binomial(link = "logit")
)
# 4) Model summary (coefficients, SEs, z-values, p-values)
summary(model_initial)

# ============================================================
# Section 3.4: Model Selection and Goodness of Fit
# ============================================================

# Overall G test: Full vs Null

# 1) G statistic
G_calc <- model_initial$null.deviance - model_initial$deviance
Gdf    <- model_initial$df.null - model_initial$df.residual

G_calc
Gdf

# 2) Critical value at 5%
G_crit <- qchisq(0.95, df = Gdf)
G_crit

# 3) p-value
p_value_G <- 1 - pchisq(G_calc, df = Gdf)
p_value_G

# 4) Pseudo R^2 
pscl::pR2(model_initial)

# 5) Individual-variable screening using Wald z and p-values
#    (rule of thumb: |z| < 2 and p > 0.05 -> candidate to remove)

full_sum <- summary(model_initial)$coefficients
full_sum

# ============================================================
# 3.4.1 anova(model, test="Chisq") 
# ============================================================

# 1): fit the full model
# We have fitted the same one in section 2.3


# 2): sequential (Type I) LR tests using deviance table
anova(model_initial, test = "Chisq")

# Model 1: remove Smoker
m1 <- update(model_initial, ~ . - Smoker)

anova(model_initial, m1, test = "LRT")
AIC(model_initial, m1)
# delete smoker
# ------------------------------------------------------------
# Model 2: from m_noSmoker, consider removing fruits(next least significant)
# ------------------------------------------------------------
m2 <- update(m1, ~ . - Fruits)

anova(m1, m2, test = "LRT")
AIC(m1, m2)

# remove HighBP
m3 <- update(m2, ~ . - HighBP)

anova(m2, m3, test = "LRT")
AIC(m2, m3)
# remove HighChol
m4 <- update(m3, ~ . - HighChol)

anova(m3, m4, test = "LRT")
AIC(m3, m4)
# ------------------------------------------------------------
# 3.5) Model_final
# ------------------------------------------------------------
# Final selected model (after removing Smoker, Fruits, HighBP)
model_final <- m3   # i.e. Diabetes ~ HighChol + BMI + Age
summary(model_final)

# easier interpretation
round(exp(coef(model_final)), 4)

# ============================================================
# 3.6 Classification accuracy and model performance
# ============================================================

# 1) Obtain predicted log-odds and probabilities for the test set
link_pr     <- round(predict(model_final, diabetes_test, type = "link"),     2)
response_pr <- round(predict(model_final, diabetes_test, type = "response"), 2)

# 2) Build a small data frame to inspect predictions
# Convert the factor outcome to 0/1 for convenience
Y <- ifelse(diabetes_test$Diabetes == "Prediabetes / Diabetes", 1, 0)

library(dplyr)

how_well <- data.frame(
  prob      = response_pr,
  actual    = Y,
  predicted = round(response_pr)
) %>%
  mutate(result = (actual == predicted))

how_well

# 3) Confusion matrix: actual (rows) vs predicted (columns)
confusion_matrix <- table(Y, round(response_pr))
confusion_matrix

# 4) Overall percentage accuracy
accuracy <- function(x){
  sum(diag(x)) / sum(x) * 100
}

accuracy(confusion_matrix)
