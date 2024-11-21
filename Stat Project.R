install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyr")
library(tidyr)
install.packages("DescTools")
library(DescTools)
library(rpart)
install.packages("e1071")
library(e1071)
install.packages("magrittr")
library(magrittr)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("caret")
library(caret)
#___________________________________________________________________________________________________________________

# Read the CSV file into a variable named studs
studs <-read.csv("StudentsPerformance.csv",na.strings="")
print(studs)
studs <- studs[, -which(names(studs) == "X")]

# get Sum for three exams
studs$Sum_Grades <- (studs$G1 + studs$G2 + studs$G3)
studs <- subset(studs, select = -c(G1, G2, G3))
#______________________________________________________________________________
# Data processing
#________________

# column sex (F&M)
studs$sex[ studs$sex == "Female"] <- "F"
studs$sex[ studs$sex == "Male"] <- "M"

#Function to calc Mode 
calculate_mode <- function(x) {
  table_result <- table(x)
  mode_values <- as.character(names(table_result[table_result == max(table_result)]))
  return(mode_values)
}



# Check for Nulls in the entire data frame
nulls <- !complete.cases(studs)
print(sum(nulls))

# Check for missing values in each column
missing_values <- colSums(is.na(studs))
print(missing_values)

# Print columns with missing values
cols_with_missing <- names(missing_values[missing_values > 0])
print(cols_with_missing)


# Check for duplicates in the entire data 
duplicates <- duplicated(studs) 
print(sum(duplicates))

#REPLACE Null values in internet col WITH MODE
studs$internet[studs$internet == ""] <- NA
mode_result <- calculate_mode(studs$internet)
print(mode_result)
studs$internet<- ifelse(is.na(studs$internet), mode_result ,studs$internet )


# Check for Nulls in the entire data frame
nulls <- !complete.cases(studs)
print(sum(nulls))

#Get outliers
#__________________
# 1- Age col
#___________
boxplot(studs$age, main="students ages",  ylab="age")

my_column <- studs$age
q1 <- quantile(my_column, 0.25)
q3 <- quantile(my_column, 0.75)
iqr <- q3 - q1
outliers <- my_column > (q3 + 1.5 * iqr)

# Replace outliers with max value
whisker_upper <- q3 + (1.5 * iqr)
index <- which(outliers)
my_column[index] <- whisker_upper

#Update the column in the dataset
studs$age <- my_column

# 2- Absences col
#_________________
boxplot(studs$absences, main="students absences",  ylab="days")

column <- studs$absences  
q1 <- quantile(column, 0.25)
q3 <- quantile(column, 0.75)
iqr <- q3 - q1
outlier <-column > (q3 + 3 * iqr)

whisker__upper <- q3 + (3 * iqr)
print(whisker__upper)
indexs <- which(outlier)
column[indexs] <- whisker__upper

#Update the column in the dataset
studs$absences <- column

#________________________________________________________________
# visualization and Statistics 
#___________________________
hist(studs$Sum_Grades, col = "skyblue", main = "Sum Grades", xlab = "sum Grades")
 

# function to plot 
create_plots <- function(data, group_var) {
  result <- aggregate(data$Sum_Grades ~ data[[group_var]], data = data, FUN = mean)
  print(result)
  par(mfrow = c(2,1))
  
  # Bar Plot
  barplot(result$`data$Sum_Grades`, space = 0.1, names.arg = result$`data[[group_var]]`,  main = group_var, ylab = "Avg Grades")
  
  # Pie Chart
  pie(result$`data$Sum_Grades`, labels = unique(data[[group_var]]), main = paste("Avg Grades -", group_var))
}



# 1- School #GP

create_plots(studs, "school")
#_____________________________________
# 2- Statistics bet Mjob and  Sum_Grades #health 

create_plots(studs, "Mjob")
#_____________________________________
# 3- Statistics bet Fjob and  Sum_Grades #teacher

create_plots(studs, "Fjob")

#_________________________________________________________________________
# 4- Statistics bet sex and  Sum_Grades #Male 

create_plots(studs, "sex")

#_____________________________________
# 5- Statistics bet goout and  Sum_Grades #2

create_plots(studs, "goout")

#_____________________________________
# 6- Statistics bet internet and  Sum_Grades #yes

create_plots(studs, "internet")

#_____________________________________
# 7- Statistics bet romantic and  Sum_Grades #no

create_plots(studs, "romantic")
#_____________________________________
# 8- Statistics bet studytime and  Sum_Grades #4

create_plots(studs, "studytime")
#_____________________________________
# 9- Statistics bet health and  Sum_Grades #1

create_plots(studs, "health")
#_____________________________________
# 10- Statistics bet absences and  Sum_Grades #21

create_plots(studs, "absences")
# Reset 
par(mfrow = c(1, 1))
#___________________________________________________________________________________________________________________________________________________
# Encoding
#__________

try<- studs
str(try)

try$romantic [try$romantic == "no"] <- 0
try$romantic [try$romantic == "yes"] <- 1

try$internet [try$internet == "yes"] <- 1
try$internet [try$internet == "no"] <- 0

try$sex [try$sex == "M"] <- 1
try$sex [try$sex == "F"] <- 0


try$school [try$school == "GP"] <- 1
try$school [try$school == "MS"] <- 0


encoded_data <- model.matrix(~ Fjob - 1, data = try)
print(encoded_data)

Mencoded_data <- model.matrix(~ Mjob - 1, data = try)
print(Mencoded_data)

combined_df <- cbind(try, encoded_data)
combined_df <- cbind(combined_df, Mencoded_data)
combined_df <- combined_df[, -which(names(combined_df) == "Fjob")]
combined_df <- combined_df[, -which(names(combined_df) == "Mjob")]

try<-combined_df 

str(try)
try$internet <- as.integer(try$internet)
try$romantic <- as.integer(try$romantic)
try$school <- as.integer(try$school)
try$sex <- as.integer(try$sex)

#_____________________________________________________________________________________________
# ML Algorithms
#______________
#1-Regression
#____________

try$AVG_grades<- try$Sum_Grades/3
try <- try[, -which(names(try) == "Sum_Grades")]
#_________________________________________________________
#1-linear regression #0.48
#_________________________

set.seed(123)
train_indices <- sample(seq_len(nrow(try)), 0.8 * nrow(try))
train_data <- try[train_indices, ]
test_data <- try[-train_indices, ]


prediction <- lm(AVG_grades~ .,data=train_data)
prediction
test_predictions <- predict(prediction, newdata = test_data)
# Mean Squared Error (MSE)
mse <- mean((test_data$AVG_grades - test_predictions)^2)
print(mse)

summary(prediction)
#______________________________________________________________________________
#2-SVR 
# radial..0.69
# poly....0.67
# linear..0.43
#_________

model <- svm(AVG_grades~ . ,data=try, kernel = "radial")

test_predictions <- predict(model, newdata = test_data)


# Calculate SSR (Sum of Squared Residuals)
ssr <- sum((test_data$AVG_grades - test_predictions)^2)

# Calculate SST (Total Sum of Squares)
sst <- sum((test_data$AVG_grades - mean(test_data$AVG_grades))^2)

# Calculate R-squared
r_squared <- 1 - (ssr / sst)

# Print or use the R-squared value
print(r_squared)

# Mean Squared Error (MSE)
mse <- mean((test_data$AVG_grades - test_predictions)^2)
print(mse)

#___________________________________________________________________
#2-Classification
#________________
#new col (label col) Pass or Fail 
try$Pass <- ifelse(try$AVG_grades >= 10, 1, 0)
try2<- try[, -which(names(try) == "AVG_grades")]
#_________________________________________________
#1-NB #64
#_________

set.seed(123)
train_indices <- sample(seq_len(nrow(try2)), 0.7 * nrow(try2))
train_data <- try2[train_indices, ]
test_data <- try2[-train_indices, ]

# Build the Naive Bayes model
nb_model <- naiveBayes(Pass ~ ., data = train_data)

# Make predictions on the test set
predictions <- predict(nb_model, test_data)
print(predictions)

# Evaluate the accuracy
conf_matrix <- table(predictions, test_data$Pass)
print(conf_matrix)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy))

# 2-Decision Tree #74
#_________________

tree_model <- rpart(Pass ~ ., data = train_data, method = "class",parms = list(split = "information"))
printcp(tree_model)

predictions <- predict(tree_model, newdata = test_data, type = "class")

# Visualize Decision Tree Structure
plot(tree_model,main = "Decision Tree Structure")
text(tree_model, cex = 0.8)
rpart.plot(tree_model)

# Visualize Decision Tree Predictions
conf_matrix <- table(test_data$Pass, predictions)
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy))

#_______________________________________________________________________________________
