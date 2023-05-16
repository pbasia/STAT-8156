# load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)
library(psych)

#--------------------------------(2018)--------------------------------
# load the data
data <- read.csv("2018_CleanData.csv")

data <- data[, c('Age','Test_Type', 'RateQ1', 'RateQ2', 'RateQ3', 'RateQ4',
                 'RateQ5', 'RateQ6', 'RateQ7', 'RateQ8', 'RateQ9', 'RateQ10', 'RateQ11',
                 'RateQ12', 'RateQ13', 'RateQ14', 'RateQ15', 'RateQ16', 'RateQ17',
                 'RateQ18', 'RateQ19', 'RateQ20', 'RateQ21', 'RateQ22', 'RateQ23',
                 'RateQ24', 'RateQ25', 'LikertQ1', 'LikertQ2', 'LikertQ3')]

scale_items <- data[, c('RateQ1', 'RateQ2', 'RateQ3', 'RateQ4',
                        'RateQ5', 'RateQ6', 'RateQ7', 'RateQ8', 'RateQ9', 'RateQ10', 'RateQ11',
                        'RateQ12', 'RateQ13', 'RateQ14', 'RateQ15', 'RateQ16', 'RateQ17',
                        'RateQ18', 'RateQ19', 'RateQ20', 'RateQ21', 'RateQ22', 'RateQ23',
                        'RateQ24', 'RateQ25', 'LikertQ1', 'LikertQ2', 'LikertQ3')]

# Calculate Cronbach's Alpha
alpha(scale_items)

# calculate means and convert to dataframe
means <- data %>% group_by(Age, Test_Type) %>% summarise_all(mean) %>% as.data.frame()

# separate the data into pre-test and post-test
pre_test <- means %>% filter(Test_Type == "Pre Test") %>% select(-Test_Type) %>% column_to_rownames("Age")
post_test <- means %>% filter(Test_Type == "Post Test") %>% select(-Test_Type) %>% column_to_rownames("Age")

# calculate the difference
diff <- as.data.frame(post_test - pre_test, row.names = rownames(post_test))
diff$Age <- as.numeric(rownames(diff))

# plot the distribution of differences
ggplot(gather(diff, key = "Question", value = "Difference", -Age), aes(Difference)) + 
  geom_histogram(aes(y=..density..), binwidth = 2, colour = "black", fill = "white") +
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title = "Distribution of Score Differences (2018)", x = "Difference", y = "Density")

# plot the differences by age
ggplot(gather(diff, key = "Question", value = "Difference", -Age), aes(Age, Difference, color = Question)) + 
  geom_line() + 
  labs(title = "Score Differences by Age and Question (2018)", x = "Age", y = "Difference") +
  theme(legend.position="bottom", legend.box = "horizontal")

# conduct ANOVA for each question
results <- data.frame(Question = character(), F_Value = numeric(), P_Value = numeric(), stringsAsFactors=FALSE)

for(question in colnames(diff)[-length(colnames(diff))]){
  model <- aov(as.formula(paste(question, "~ Age")), data = diff)
  anova_summary <- summary(model)
  results <- rbind(results, data.frame(Question = question, F_Value = anova_summary[[1]][[4]][1], P_Value = anova_summary[[1]][[5]][1]))
}


#print(results)

results$P_Value_Adjusted <- p.adjust(results$P_Value, method = "bonferroni")

print(results)


write.csv(results, file = "results_table2018.csv", row.names = FALSE)


#---------------(HSD 2018)----------------

# Convert 'Age' to a factor
data$Age <- as.factor(data$Age)

rateQ24_data <- data[, c("Age", "RateQ24")]

# Run one-way ANOVA
model <- aov(RateQ24 ~ Age, data = rateQ24_data)

# Run Tukey's HSD test
tukey_results <- TukeyHSD(model)

# Print the results
print(tukey_results)


#--------------------------------(2019)--------------------------------

# load the data
data <- read.csv("2019_CleanData.csv")

data <- data[, c('Age','Test_Type', 'RateQ1', 'RateQ2', 'RateQ3', 'RateQ4',
                 'RateQ5', 'RateQ6', 'RateQ7', 'RateQ8', 'RateQ9', 'RateQ10', 'RateQ11',
                 'RateQ12', 'RateQ13', 'RateQ14', 'RateQ15', 'RateQ16', 'RateQ17',
                 'RateQ18', 'RateQ19', 'RateQ20', 'RateQ21', 'RateQ22', 'RateQ23',
                 'RateQ24', 'RateQ25', 'LikertQ1', 'LikertQ2', 'LikertQ3')]

scale_items <- data[, c('RateQ1', 'RateQ2', 'RateQ3', 'RateQ4',
                        'RateQ5', 'RateQ6', 'RateQ7', 'RateQ8', 'RateQ9', 'RateQ10', 'RateQ11',
                        'RateQ12', 'RateQ13', 'RateQ14', 'RateQ15', 'RateQ16', 'RateQ17',
                        'RateQ18', 'RateQ19', 'RateQ20', 'RateQ21', 'RateQ22', 'RateQ23',
                        'RateQ24', 'RateQ25', 'LikertQ1', 'LikertQ2', 'LikertQ3')]

# Calculate Cronbach's Alpha
alpha(scale_items)

# calculate means and convert to dataframe
means <- data %>% group_by(Age, Test_Type) %>% summarise_all(mean) %>% as.data.frame()

# separate the data into pre-test and post-test
pre_test <- means %>% filter(Test_Type == "Pre Test") %>% select(-Test_Type) %>% column_to_rownames("Age")
post_test <- means %>% filter(Test_Type == "Post Test") %>% select(-Test_Type) %>% column_to_rownames("Age")

# calculate the difference
diff <- as.data.frame(post_test - pre_test, row.names = rownames(post_test))
diff$Age <- as.numeric(rownames(diff))

# plot the distribution of differences
ggplot(gather(diff, key = "Question", value = "Difference", -Age), aes(Difference)) + 
  geom_histogram(aes(y=..density..), binwidth = 2, colour = "black", fill = "white") +
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title = "Distribution of Score Differences (2019)", x = "Difference", y = "Density")

# plot the differences by age
ggplot(gather(diff, key = "Question", value = "Difference", -Age), aes(Age, Difference, color = Question)) + 
  geom_line() + 
  labs(title = "Score Differences by Age and Question (2019)", x = "Age", y = "Difference") +
  theme(legend.position="bottom", legend.box = "horizontal")

# conduct ANOVA for each question
results <- data.frame(Question = character(), F_Value = numeric(), P_Value = numeric(), stringsAsFactors=FALSE)

for(question in colnames(diff)[-length(colnames(diff))]){
  model <- aov(as.formula(paste(question, "~ Age")), data = diff)
  anova_summary <- summary(model)
  results <- rbind(results, data.frame(Question = question, F_Value = anova_summary[[1]][[4]][1], P_Value = anova_summary[[1]][[5]][1]))
}


#print(results)

results$P_Value_Adjusted <- p.adjust(results$P_Value, method = "bonferroni")

print(results)


write.csv(results, file = "results_table2019.csv", row.names = FALSE)


#---------------(HSD 2019)----------------

# Convert 'Age' to a factor
data$Age <- as.factor(data$Age)

rateQ3_data <- data[, c("Age", "RateQ3")]
rateQ8_data <- data[, c("Age", "RateQ8")]

# Run one-way ANOVA
modelq3 <- aov(RateQ3 ~ Age, data = rateQ3_data)
modelq8 <- aov(RateQ8 ~ Age, data = rateQ8_data)

# Run Tukey's HSD test
tukey_results3 <- TukeyHSD(modelq3)
# Print the results
print(tukey_results3)


# Run Tukey's HSD test
tukey_results8 <- TukeyHSD(modelq8)
# Print the results
print(tukey_results8)

#--------------------------------(End)--------------------------------

