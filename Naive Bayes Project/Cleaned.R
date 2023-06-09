# Load Libraries
library(readxl)
library(e1071)
library(writexl)
library(caret)
library(caTools)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(corrplot)
library(Rfast2)
library(klaR)
library(naivebayes)
library(scales)
library(patchwork)



# Read excel file and remove blank rows and columns
df <- read_xlsx("Complete_Final.xlsx")
df <- df[, -5]
df <- df[rowSums(is.na(df[ , 7])) == 0, ]

# write_xlsx(df,"Complete_Final.xlsx")                        


# Change class types
df$`Overall grade by the peers` <- as.factor(df$`Overall grade by the peers`)
df$`Final grade` <- as.factor(df$`Final grade`)
df$`Assigment grade` <- as.factor(df$`Assigment grade`)
df$`Grade Level` <- as.factor(df$`Grade Level`)

df$`Detailed label` <- as.factor(df$`Detailed label`)
df$`Lie factor` <- as.factor(df$`Lie factor`)
df$`Data/color ink ratio` <- as.factor(df$`Data/color ink ratio`)
df$`Chart junk` <- as.factor(df$`Chart junk`)

df$`Count # of words` <- as.integer(df$`Count # of words`)
df$Nouns <- as.integer(df$Nouns)
df$Verb <- as.integer(df$Verb)
df$adv <- as.integer(df$adv)
df$Adj <- integer(df$Adj)


# 2 Bin
df$CommentDensity <- cut(df$`Count # of words`, breaks = c(0, 50,100), include.lowest = T)



# Fill in the blanks in the dataframe
fill_down_and_repeat <- function(df) {
  # create a copy of the input dataframe
  df_filled <- df
  
  # loop over each row in the dataframe
  for (i in 1:nrow(df_filled)) {
    # loop over each column in the dataframe
    for (j in 1:ncol(df_filled)) {
      # check if the current cell is missing data and the previous row and column have data
      if (is.na(df_filled[i, j]) && !is.na(df_filled[i-1, j]) && !is.na(df_filled[i, j-1])) {
        # fill the current cell with the data from the previous row with data in the current column
        df_filled[i, j] <- df_filled[i-1, j]
      }
    }
  }
  
  # return the filled dataframe
  return(df_filled)
}

df <- fill_down_and_repeat(df)





# Set Seed
set.seed(22)

# Create train and test df
x = sort(sample(nrow(df), nrow(df) * .7))

df_train <- df[x,]
df_test <- df[-x,]




# 2 bins naive bayes test
model_2bin <- naiveBayes(CommentDensity~., data = df_train)
pred_2bin <- (predict(model_2bin, df_test))

# frequency Table
cm_2bin <- table(df_test$CommentDensity, pred_2bin)
x <- as.data.frame(cm_2bin)
x$Type <- c("True", "False", "True", "False")

ggplot(x, aes(x = Var1, y = Freq, fill = Type)) + geom_col(aes(x = pred_2bin), position = "dodge") +
  labs(title = "Naive Bayes - Word Count: True vs False Results", x = "Word Count (Bins)", y = "Frequency")

# Probility Table
probo_2bin <- (cm_2bin/ sum(cm_2bin))
probo_2bin

confusionMatrix(cm_2bin)
# 96.9%


# Naive Bayes with contigency table from the df_train and test datasets
con_table_Train <- data.table::data.table(df_train)
con_table_Test <- data.table::data.table(df_test)

con_B <- naiveBayes(CommentDensity~., data = con_table_Train)
con_pred <- predict(con_B, con_table_Test)

cm_con <- table(con_table_Test$CommentDensity, con_pred)
confusionMatrix(cm_con)


# Correlation Matrix (Can only is numerical data)
cor <- cor(df[, 11:15])
cor
corrplot(cor, method = "color", tl.srt = 45)
corrplot(cor, method = "number", tl.srt = 45)



# Mcnemars Test
cm_2bin
mcnemar.test(table(df_test$CommentDensity, pred_2bin))




 




# Final Grade Classifier
# Change Grades to a solid "A" or "B" no + or -
  # New Train data set for it
qq <- df_train
qq$`Final grade`[qq$`Final grade`== "A-"] <- "A"
qq$`Final grade`[qq$`Final grade`== "A -"] <- "A"
qq$`Final grade`[qq$`Final grade`== "A+"] <- "A"

qq$`Final grade`[qq$`Final grade`== "B+"] <- "B"
qq$`Final grade`[qq$`Final grade`== "C+"] <- "C"


qq$`Overall grade by the peers`[qq$`Overall grade by the peers` == "A-"] <- "A"
qq$`Overall grade by the peers`[qq$`Overall grade by the peers` == "A -"] <- "A"
qq$`Overall grade by the peers`[qq$`Overall grade by the peers` == "A+"] <- "A"

qq$`Overall grade by the peers`[qq$`Overall grade by the peers` == "B+"] <- "B"
qq$`Overall grade by the peers`[qq$`Overall grade by the peers` == "C+"] <- "C"


qq$`Assigment grade`[qq$`Assigment grade` == "A-"] <- "A"
qq$`Assigment grade`[qq$`Assigment grade` == "A -"] <- "A"
qq$`Assigment grade`[qq$`Assigment grade` == "A+"] <- "A"

qq$`Assigment grade`[qq$`Assigment grade` == "B+"] <- "B"
qq$`Assigment grade`[qq$`Assigment grade` == "C+"] <- "C"

# New test Data set
qw <- df_test
qw$`Final grade`[qw$`Final grade`== "A-"] <- "A"
qw$`Final grade`[qw$`Final grade`== "A -"] <- "A"
qw$`Final grade`[qw$`Final grade`== "A+"] <- "A"

qw$`Final grade`[qw$`Final grade`== "B+"] <- "B"
qw$`Final grade`[qw$`Final grade`== "C+"] <- "C"


qw$`Overall grade by the peers`[qw$`Overall grade by the peers` == "A-"] <- "A"
qw$`Overall grade by the peers`[qw$`Overall grade by the peers` == "A -"] <- "A"
qw$`Overall grade by the peers`[qw$`Overall grade by the peers` == "A+"] <- "A"

qw$`Overall grade by the peers`[qw$`Overall grade by the peers` == "B+"] <- "B"
qw$`Overall grade by the peers`[qw$`Overall grade by the peers` == "C+"] <- "C"


qw$`Assigment grade`[qw$`Assigment grade` == "A-"] <- "A"
qw$`Assigment grade`[qw$`Assigment grade` == "A -"] <- "A"
qw$`Assigment grade`[qw$`Assigment grade` == "A+"] <- "A"

qw$`Assigment grade`[qw$`Assigment grade` == "B+"] <- "B"
qw$`Assigment grade`[qw$`Assigment grade` == "C+"] <- "C"


# Naive Bayes with new updated Grades
model_FG <- naiveBayes(`Final grade`~., data = qq)
pred_FG <- predict(model_FG, qw)

cm_FG <- table(qw$`Final grade`, pred_FG)
confusionMatrix(cm_FG)
#92.6%

# Remove unneeded + and - factors
cm_FG <- cm_FG[-c(2:4, 6, 8), -c(2:4, 6, 8)]

# Make table as dataframe to grpah
FG <- as.data.frame(cm_FG)

# New Column for data type. True = Correctly predicted  False = Incorrectly predicted
FG$Type <- c("True", rep("False", 4), "True", rep("False", 4), "True", rep("False", 4), "True")

ggplot(FG, aes(x = Var1, y = Freq, fill = Type)) + geom_col(aes(x = pred_FG), position = "dodge") +
  labs(title = "Naive Bayes - Final Grade: True vs False Results", x = "Final Grade", y = "Frequency")



# Final Grade Classifier
# Naive Bayes with contigency table from the df_train and test datasets
con_FG <- naiveBayes(`Final grade`~., data = con_table_Train)
con_FG <- predict(con_FG, con_table_Test)

cm_FG <- table(con_table_Test$`Final grade`, con_FG)
confusionMatrix(cm_FG)


# Mcnemars Test (2 bin)
# Change Final Grade into a 2 factored data entry
# Only grades are A,B,C,D. High = Grades A and B; Low = Grades C and F
qq <- df_train
qq$FG <- ifelse(qq$`Assigment grade` %in% c("A", "B"), "High", "Low")

qw <- df_test
qw$FG <- ifelse(qw$`Assigment grade` %in% c("A", "B"), "High", "Low")

# Naive Bayes with newly created Classifier 
mod_FG <- naiveBayes(FG~., data = qq)
pre_FG <- predict(mod_FG, qw)

cm_fg <- table(qw$FG, pre_FG)
confusionMatrix(cm_fg)

# Final Mcnemar test
mcnemar.test(table(qw$FG, pre_FG))

# Create contingency table
cont_table <- table(qw$FG, pre_FG, 
                    dnn = c("Actual", "Predicted"))

cont_table










# Peer Review Rubric

# Correlation Matrix
PR <- df
PR$`Detailed label` <- as.numeric(PR$`Detailed label`)
PR$`Lie factor`<- as.numeric(PR$`Lie factor`)
PR$`Data/color ink ratio` <- as.numeric(PR$`Data/color ink ratio`)
PR$`Chart junk` <- as.numeric(PR$`Chart junk`)

pr_cor <- cor(PR[,7:10])
pr_cor
corrplot(pr_cor, method = "color", tl.srt = 45)
corrplot(pr_cor, method = "number", tl.srt = 45)



# Naive Bayes
nb_pr <- naiveBayes(`Detailed label`~ `Lie factor` + `Data/color ink ratio` + `Chart junk`, data = df_train)
pre_pr <- predict(nb_pr, df_test)

cm_pr <- table(df_test$`Detailed label`, pre_pr)
confusionMatrix(cm_pr)

pr_tab <- as.data.frame(cm_pr)
pr_tab$Type <- c("True", rep("False", 5), "True", rep("False", 5), "True", rep("False", 5), "True",
                 rep("False", 5), "True")

ggplot(pr_tab, aes(x = Var1, fill = Type, y = Freq)) + geom_col(aes(x = pre_pr), position = "dodge") +
  labs(title = "Naive Bayes Peer Rubric: True vs False Results",
       x = "Rubric Mark Points",
       y = "Frequency")



# McNemar Test
pr1 <- df_train
pr1$`Detailed label` <- ifelse(pr1$`Detailed label` %in% c(1:2, 3:4), "Low", "High")
pr1$`Chart junk` <- ifelse(pr1$`Chart junk` %in% c(1:2, 3:4), "Low", "High")
pr1$`Data/color ink ratio` <- ifelse(pr1$`Data/color ink ratio` %in% c(1:2, 3:4), "Low", "High")
pr1$`Lie factor` <- ifelse(pr1$`Lie factor` %in% c(1:2, 3:4), "Low", "High")

pr2 <- df_test
pr2$`Detailed label` <- ifelse(pr2$`Detailed label` %in% c(1:2, 3:4), "Low", "High")
pr2$`Chart junk` <- ifelse(pr2$`Chart junk` %in% c(1:2, 3:4), "Low", "High")
pr2$`Data/color ink ratio` <- ifelse(pr2$`Data/color ink ratio` %in% c(1:2, 3:4), "Low", "High")
pr2$`Lie factor` <- ifelse(pr2$`Lie factor` %in% c(1:2, 3:4), "Low", "High")

# Naive Bayes with newly created Classifier 
mod_pr <- naiveBayes(`Detailed label`~ `Lie factor` + `Chart junk` + `Data/color ink ratio`, data = pr1)
pre_pr <- predict(mod_pr, pr2)

cm_pr <- table(pr2$`Detailed label`, pre_pr)
confusionMatrix(cm_pr)

# Final Mcnemar test
cm_pr
mcnemar.test(table(pr2$`Detailed label`, pre_pr))
 ### Because of 100% accuracy the mcnemar test is returning NAs as the result. There needs to be at
# least something inccorect for it to work.




#### Cross Validation ####
dft <- df_train
dft$`Detailed label` <- as.numeric(dft$`Detailed label`)
dft$`Lie factor` <- as.numeric(dft$`Lie factor`)
dft$`Data/color ink ratio` <- as.numeric(dft$`Data/color ink ratio`)
dft$`Chart junk` <- as.numeric(dft$`Chart junk`)






# Convert Rubric data type to numeric for cross validation
dft <- df_train
dft$`Detailed label` <- as.numeric(dft$`Detailed label`)
colnames(dft)[7] <- "Detailed_label"

dft$`Lie factor` <- as.numeric(dft$`Lie factor`)
colnames(dft)[8] <- "Lie_factor"

dft$`Data/color ink ratio` <- as.numeric(dft$`Data/color ink ratio`)
colnames(dft)[9] <- "Data_color_ink_ratio"

dft$`Chart junk` <- as.numeric(dft$`Chart junk`)
colnames(dft)[10] <- "Chart_junk"



# Cross Validate Rubric
data <- dft[, c("Detailed_label", "Lie_factor", "Data_color_ink_ratio", "Chart_junk")]
ctrl <- trainControl(method = "cv", number = 5)
model <- train(Detailed_label ~ ., data = data, method = "lm", trControl = ctrl)

# CV Results
cv_results <- model$results
cv_results




# Prep POS data type to numeric
dft$`Count # of words` <- as.numeric(dft$`Count # of words`)
colnames(dft)[11] <- "Word_Count"

dft$Nouns <- as.numeric(dft$Nouns)

dft$Verb <- as.numeric(dft$Verb)

dft$adv <- as.numeric(dft$adv)

dft$Adj <- as.numeric(dft$Adj)

# Cross Validate POS
data2 <- dft[, c("Word_Count", "Nouns", "Verb", "adv", "Adj")]
ctrl2 <- trainControl(method = "cv", number = 5)
model2 <- train(Word_Count ~ ., data = data2, method = "lm", trControl = ctrl)

# Results
cv_results2 <- model2$results
cv_results2



# Create contingency table
cont_table_pr <- table(pr2$`Detailed label`, pre_pr, 
                       dnn = c("Actual", "Predicted"))
cont_table_pr




# BOXPLOT of Predicted Rubric and Acutal Rubric

# Data frame from original rubric prediction
line1 <- pr_tab

# Rename columns
line1_renamed <- line1 %>%
  rename(Actual = Var1, Predicted = pre_pr, Frequency = Freq)

# Convert columns to numeric
line1_renamed$Predicted <- as.numeric(as.character(line1_renamed$Predicted))
line1_renamed$Actual <- as.numeric(as.character(line1_renamed$Actual))

# Calculate statistics for predicted values
predicted_stats <- line1_renamed %>%
  filter(Predicted != 0) %>%
  summarise(
    median = median(Predicted),
    iqr = IQR(Predicted),
    lower_outliers = paste(Predicted[Predicted < median - 1.5 * iqr], collapse = ", "),
    upper_outliers = paste(Predicted[Predicted > median + 1.5 * iqr], collapse = ", ")
  )

# Calculate statistics for actual values
actual_stats <- line1_renamed %>%
  filter(Actual != 0) %>%
  summarise(
    median = median(Actual),
    iqr = IQR(Actual),
    lower_outliers = paste(Actual[Actual < median - 1.5 * iqr], collapse = ","),
    upper_outliers = paste(Actual[Actual > median + 1.5 * iqr], collapse = ",")
  )


# Create box plots for predicted values
predicted_boxplot <- ggplot(line1_renamed, aes(x = "Predicted", y = Predicted)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  geom_text(data = predicted_stats, aes(x = median, y = median, label = paste("Median:", median)),
            vjust = 1.5, hjust = 1.5, color = "red") +
  geom_text(data = predicted_stats, aes(x = iqr, y = iqr, label = paste("IQR:", iqr)),
            vjust = -2, hjust = -0.5, color = "red") +
  labs(x = "Values", y = "Predicted", title = "Box Plot - Predicted Values")

# Create box plots for actual values
actual_boxplot <- ggplot(line1_renamed, aes(x = "Actual", y = Actual)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  geom_text(data = actual_stats, aes(x = median, y = median, label = paste("Median:", median)),
            vjust = 1.5, hjust = 1.5, color = "red") +
  geom_text(data = actual_stats, aes(x = iqr, y = iqr, label = paste("IQR:", iqr)),
            vjust = -2, hjust = -0.5, color = "red") +
  labs(x = "Values", y = "Actual", title = "Box Plot - Actual Values")

# Combine box plots
combined_plot <- predicted_boxplot + actual_boxplot +
  plot_layout(ncol = 2) + 
  plot_annotation(title = "Grading Rubric", 
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))

# Display the combined plot
combined_plot

