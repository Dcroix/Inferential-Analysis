# Learning R Activity 4
# Trainer: Paolo G. Hilado

# Data Set1
# Load the Dataset AgePerf
library(xlsx)
df <- read.xlsx("AgePerf.xlsx", sheetIndex = "Sheet1")
df$AgeCat <- factor(df$AgeCat)
write.xlsx(df, file = "AgePerf.xlsx", sheetName = "Sheet1")
# Remove unwanted column
df$NA. <- NULL
# check the first and last 7 rows
head(df, 7); tail(df, 7)
# Generate a summary of the dataset
summary(df)
# Check the structure of the dataframe
str(df)

# Conduct Normality Testing
library(nortest)
ad.test(df[df$AgeCat == "Young Adult",]$Perf)
ad.test(df[df$AgeCat == "Middle Aged",]$Perf)
ad.test(df[df$AgeCat == "Late Adult",]$Perf)

# Visualize Boxplots
boxplot(df[df$AgeCat == "Young Adult",]$Perf)
boxplot(df[df$AgeCat == "Middle Aged",]$Perf)
boxplot(df[df$AgeCat == "Late Adult",]$Perf)

# Generate Descriptives
library(psych)
Desc <- describeBy(df$Perf, df$AgeCat)
Desc <- rbind(Desc$`Young Adult`, Desc$`Middle Aged`, Desc$`Late Adult`)
Desc <- round(Desc[, c(2,5,7,8,9)],2)
Group <- c("Young", "Middle", "Late")
Group <- data.frame(Group)
Desc <- cbind(Desc, Group)
Desc <- Desc[, c(6,1,2,3,4,5)]

# Comparison of Groups (3)
library(broom)
library(FSA)
KW <- tidy(kruskal.test(df$Perf~df$AgeCat))
# Conduct Post Hoc 
PH <- dunnTest(df$Perf~df$AgeCat)
PH <- data.frame(c(PH$method, PH$res))
PH$P.unadj <- NULL


# Data Set 2
# Open Data on Exp3Groups
library(xlsx)
df <- read.xlsx("Exp3Groups.xlsx", sheetIndex = "Sheet1")
# Check the first and last 12 rows
head(df, 12); tail(df, 12)
# Remove the First Column
df$NA. <- NULL
# Check the Structure
str(df)
# Generate a Summary
summary(df)
# Conduct Normality Testing
library(nortest)
ad.test(df[df$Groups == "Grp A",]$Perf)
ad.test(df[df$Groups == "Grp B",]$Perf)
ad.test(df[df$Groups == "Grp C",]$Perf)
# Generate Descriptives
library(psych)
Desc <- describeBy(df$Perf, df$Groups)
Desc <- rbind(Desc$`Grp A`, Desc$`Grp B`, Desc$`Grp C`)
Desc <- round(Desc[, c(2,3,4,8,9)],2)
Group <- c('Group A', 'Group B', 'Group C')
Desc <- cbind(Desc, data.frame(Group))
Desc <- Desc[, c(6,1,2,3,4,5)]
# Comparison of Groups (3)
library(broom)
library(FSA)
ANOVA <- aov(df$Perf~df$Groups)
ANOVAr <- tidy(ANOVA)
# Conduct Post Hoc Analysis
PH <- tidy(TukeyHSD(ANOVA))
