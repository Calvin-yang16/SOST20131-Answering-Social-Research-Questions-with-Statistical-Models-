# 2.0) Packages
library(dplyr)
library(ggplot2)
library(GGally)

# 2.0.1) Read data
emp <- read.csv("Employee_Performance.csv", header = TRUE)

# 2.0.2 Quick check
dim(emp)
str(emp)
head(emp)
# ============================================================
# 2.2 Data Description and Exploratory Analysis
# ============================================================
# 2) Convert categorical variables to factors (so R uses dummy coding)
emp <- emp %>%
  mutate(
    Type = factor(Type, levels = c(0, 1), labels = c("Science", "Business")),
    Level = factor(Level, levels = c(1, 2, 3), labels = c("Degree", "Masters", "Doctorate")),
    Expertise = factor(Expertise, levels = c(1, 2, 3, 4),
                       labels = c("Logistics", "Promotion", "Sales", "Strategy"))
  )

# 3) Descriptive statistics
summary(emp)

# SDs for numeric variables
sapply(emp %>% select(Sales_Perf, Creativity, Mechanical, Abstract, Maths), sd)

# Group counts 
table(emp$Type)
table(emp$Level)
table(emp$Expertise)

# 4) ggpairs on variables 
GGally::ggpairs(emp)

# 5) Correlation matrix among numeric variables (supports the multicollinearity comment)
cor(emp %>% select(Sales_Perf, Creativity, Mechanical, Abstract, Maths))

# 6) Boxplots: Sales_Perf by categorical predictors (useful for 2.2 and later interpretation)
ggplot(emp, aes(x = Type, y = Sales_Perf)) + geom_boxplot()
ggplot(emp, aes(x = Level, y = Sales_Perf)) + geom_boxplot()
ggplot(emp, aes(x = Expertise, y = Sales_Perf)) + geom_boxplot()

# confirm reference group in dummy variables
contrasts(emp$Type)
contrasts(emp$Level)
contrasts(emp$Expertise)
# ============================================================
# 2.3 Initial Model Specification
# ============================================================
# Initial model
model_initial <- lm(
  Sales_Perf ~ Creativity + Mechanical + Abstract + Maths +
    Type + Level + Expertise,
  data = emp
)
model_initial
summary(model_initial)
# ============================================================
# 2.4 Model Selection and Refinement
# ============================================================
# check vif to avoid multicollinearity
library(car)
vif(model_initial)
# Delete Abstract 
model_1 <- update(model_initial, . ~ . - Abstract)
summary(model_1)
# Delete Creativity  
model_2 <- update(model_1, . ~ . - Creativity)
summary(model_2)
# Delete Expertise
model_3 <- update(model_1, . ~ . - Expertise )
summary(model_3)
# residual plots
par(mfrow = c(2, 2))
plot(model_2)
par(mfrow = c(1, 1))
# ============================================================
# 2.5 Final Model and Interpretation
# ============================================================
# Using final model to predict 
Sales_Perf = 83.0281 + 0.6510 * 18 + 0.2678 * 60 + 10.1580 -0.5668
Sales_Perf