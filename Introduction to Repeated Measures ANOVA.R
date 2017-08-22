###Chapter 1 Introduction to Repeated Measures ANOVA

###Explore the Working Memory Data
# wm in Data Camp's workspace
# Convert subject to a factor
wm$subject <- factor(wm$subject)

# Summary statistics by all groups
describeBy(wm, wm$condition)

# Boxplot iq versus condition
boxplot(wm$iq ~ wm$condition, main = "Boxplot", 
        xlab = "Training days", ylab = "IQ")

# Plot the data
ggplot(data = wm, aes(x = condition, y = iq, group = subject, colour = subject)) + 
  geom_line() + geom_point()

###Mauchly's Test
# Define the iq data frame
iq <- cbind(wm$iq[wm$condition == "8 days"], 
            wm$iq[wm$condition == "12 days"], 
            wm$iq[wm$condition == "17 days"],
            wm$iq[wm$condition == "19 days"]) 

# Make an mlm object
mlm <- lm(iq ~ 1)

# Mauchly's test
mauchly.test(mlm, x = ~ 1) 

###Chapter 2 More Repeated Measures ANOVA

###The Systematic Between-Groups Variance
## Calculate the systematic variance due to conditions

# Define number of subjects for each condition
n <- 20

# Calculate group means
y_j <- tapply(wm$iq, wm$condition, mean)

# Calculate the grand mean
y_t <- mean(wm$iq)

# Calculate the sum of squares
ss_cond <- n * sum((y_j - y_t)^2)

# Define the degrees of freedom for conditions
df <- 3

# Calculate the mean squares (variance)
ms_cond <- ss_cond / df

###The Subject Variance
## wm is already loaded

# Define number of conditions for each subject
n <- 4

# Calculate subject means
y_j <- tapply(wm$iq, wm$subject, mean)

# Calculate the grand mean
y_t <- mean(wm$iq)

# Calculate the sum of squares
ss_subjects <- n * sum((y_j - y_t)^2)

# Define the degrees of freedom for subjects
df <- 19

# Calculate the mean squares (variance)
ms_subjects <- ss_subjects / df

###The Unsystematic Within-Groups Variance
## wm is still loaded

# Create four subsets of the four groups, containing the IQ results
# Make the subset for the group condition = "8 days"
y_i1 <- subset(wm$iq, wm$cond == "8 days")
# Make the subset for the group condition = "12 days"
y_i2 <- subset(wm$iq, wm$cond == "12 days")
# Make the subset for the group condition = "17 days"
y_i3 <- subset(wm$iq, wm$cond == "17 days")
# Make the subset for the group condition = "19 days"
y_i4 <- subset(wm$iq, wm$cond == "19 days")

# Subtract the individual values by their group means
s_1 <- y_i1 - mean(y_i1)
s_2 <- y_i2 - mean(y_i2)
s_3 <- y_i3 - mean(y_i3)
s_4 <- y_i4 - mean(y_i4)

# Put everything back into one vector
s_t <- c(s_1, s_2, s_3, s_4)

# Calculate the within sum of squares by using the vector s_t
ss_sa <- sum(s_t^2)

# Define the degrees of freedom
df <- 4 * (20 - 1)

# Calculate the mean squares (variances)
ms_sa <- ss_sa / df

###The Unsystematic Variance for the Repeated Measures Design
## wm, ss_sa, and ss_subjects are available in your workspace

# Unsystematic sum of squares
ss_rm <- ss_sa - ss_subjects

# Define the degrees of freedom
df <- 3*19

# Calculate the mean squares (variances)
ms_rm <- ss_rm / df

###F-ratio and p-value
## wm, ss_sa, and ss_subjects are available in your workspace

# Calculate the F-ratio
f_rat <- ms_cond / ms_rm

# Define the degrees of freedom of the F-distribution
df1 <- 3
df2 <- 3 * 19

# Calculate the p-value
p <- 1 - pf(f_rat, df1, df2)

###ANOVA in R
## wm is available in your workspace

# ANOVA model
model <- aov(wm$iq ~ wm$condition + Error(wm$subject / wm$condition))

# Summary of model
summary(model)

###Effect Size
# Define the total sum of squares
ss_total <- ss_rm + ss_cond

# Calculate the effect size
eta_sq <- ss_cond / ss_total

###Post-hoc Test One
## wm is still loaded

# Post-hoc test: default procedure
with(wm, pairwise.t.test(iq, condition, paired = T))

###Post-hoc Test:Bonferroni
## wm is still loaded

# Post-hoc test: Bonferroni procedure
with(wm, pairwise.t.test(iq, condition, paired = T, p.adjust.method = "bonferroni"))

###Paired t-test
# Define two subsets
cond_12days <- wm$iq[wm$condition == "12 days"]
cond_17days <- wm$iq[wm$condition == "17 days"]

# Perform t-test
t.test(cond_12days, cond_17days, paired = T)

