# packages and libraries installed 
# Please make sure to install the below packages in order to run the code
install.packages("dplyr")
install.packages("caret")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("formattable")
install.packages("DT")
install.packages("wordcloud")
install.packages("tm")
install.packages("corrgram")
install.packages("ROSE")
install.packages("SnowballC")

# library("dplyr")
# library("caret")
# library("corrplot")
# library("ggplot2")
# library("rpart")
# library("rpart.plot")
# library("formattable")
# library("DT")
# library("wordcloud")
# library("tm")
# library("corrgram")
# library("ROSE")
# library("SnowballC")


# Importing the dataset from a csv file "HR.csv"
hr.data <- read.csv('HR.csv', header = T)

# glimpse of data
library(dplyr)
glimpse(hr.data)

# structure of data
str(hr.data)

# subsetting data <- putting the numeric attributes into "empData"
empData <- hr.data[1:10]

# displaying the head of the data
head(empData)

# Data Exploration / Data Quality Report
summary(empData)


# Correlation Matrix 
library(corrplot)
library(dplyr)
HR_correlation <- empData %>% 
        select(satisfaction_level:promotion_last_5years)
M <- cor(HR_correlation)
corrplot(M, method="circle")

# Visualize correlation matrix using corrgram
library(corrgram)
corrgram(empData, lower.panel = panel.shade, upper.panel = panel.pie, 
         text.panel = panel.txt, main = "Corrgram of all  variables")


# Data Exploration <- Who is leaving? 

# Filtering the employees that left the company
library(dplyr)
emp.left <- empData %>%  
        filter(left==1) 

# number of employees that have left the organization
nrow(emp.left)

# visualizing each numeric variable in our dataset
hist(emp.left$satisfaction_level,col="rosybrown2",main="Satisfaction level")
hist(emp.left$last_evaluation,col="rosybrown2",main="Last Evaluation")
hist(emp.left$promotion_last_5years,col="rosybrown2",main="Promoted in the last 5 years?")
hist(emp.left$Work_accident,col="rosybrown2",main="Work Accident")
barplot(table(emp.left$department),col="rosybrown2",main="Department")
barplot(table(emp.left$salary),col="lightblue",main="Salary")
hist(emp.left$average_montly_hours,col="lightblue",main="Avergae monthly hours")
emp.left$time_spend_company <- as.numeric(emp.left$time_spend_company)
hist(emp.left$time_spend_company,col="lightblue",main="Time spent in company")
hist(emp.left$number_project,col="lightblue",main="Number of projects")

# Visualizing the density of employee left against different variables
library(ggplot2)

ggplot(data = emp.left, aes(x= emp.left$satisfaction_level, y=..density..)) +
        geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) +
        geom_density()   # Satisfacion level

ggplot(data = emp.left, aes(x= emp.left$last_evaluation, y=..density..)) +
        geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) +
        geom_density()   # last_evaluation



# Data Exploration <- Why are good employees leaving?

# Setting a criteria for good Employees 
# Filtering the good employees that left and selecting only important rows
emp.left$time_spend_company <- as.numeric(emp.left$time_spend_company)
emp.good.left <- emp.left %>% 
        filter(last_evaluation >= 0.65 | time_spend_company >= 4 | number_project >= 4)

# Number of good employees who left
nrow(emp.good.left) 

# visaulize each variable in the dataset pertaining to "good employees"
# Correlation matrix for good employees
empData$employeeNumber <- rownames(empData) # adding a new column 
empData$left = as.integer(empData$left) # converting column 'left' to integer 
empData$time_spend_company = as.numeric(empData$time_spend_company) # Numeric conversion

emp.good <- empData %>% 
        filter(last_evaluation >= 0.65 | time_spend_company >= 4 | number_project >= 4)

hr_good_people_select <- emp.good %>% 
        select(satisfaction_level, number_project: promotion_last_5years)
M <- cor(hr_good_people_select)
corrplot(M, method="circle")

# Visualize correlation matrix for good employees using corrgram
corrgram(emp.good, lower.panel = panel.shade, 
         upper.panel = panel.pie, text.panel = panel.txt, 
         main = "Corrgram of all  variables")


#################################
#                               #
#  Natural Language Processing  #
#                               #
#################################

# subsetting the text attributes for Sentiment Analysis
Emp_reviews <- hr.data[12:14]
names(Emp_reviews)

#perform nlp on pros, cons, advice.to.mgmt columns

# TEXT EXTRACTION     
# to convert text into corpus we use tm package
library(tm)
library(SnowballC)
corpus_pros =Corpus(VectorSource(Emp_reviews$pros))
corpus_cons =Corpus(VectorSource(Emp_reviews$cons))
corpus_atm =Corpus(VectorSource(Emp_reviews$advice.to.mgmt))

#convert to lower case
library(tm)
library(SnowballC)
corpus_pros =tm_map(corpus_pros, tolower)
corpus_cons = tm_map(corpus_cons, tolower)
corpus_atm =tm_map(corpus_atm, tolower)

#remove punctuation
library(tm)
library(SnowballC)
corpus_pros =tm_map(corpus_pros, removePunctuation)
corpus_cons = tm_map(corpus_cons, removePunctuation)
corpus_atm =tm_map(corpus_atm, removePunctuation)

#remove stop words
library(tm)
library(SnowballC)
corpus_pros =tm_map(corpus_pros, removeWords, stopwords("english"))
corpus_pros = tm_map(corpus_pros, removeWords , c("work","compani","smart","get","manag","free","amaz"))
corpus_cons = tm_map(corpus_cons, removeWords, stopwords("english"))
corpus_atm =tm_map(corpus_atm, removeWords, stopwords("english"))
corpus_atm = tm_map(corpus_atm, removeWords , c("none"))

## Stem document
library(tm)
library(SnowballC)
corpus_pros =tm_map(corpus_pros, stemDocument)
corpus_cons = tm_map(corpus_cons, stemDocument)
corpus_atm =tm_map(corpus_atm, stemDocument)

##Viewing the corpus content
corpus_pros
corpus_cons
corpus_atm

# Converting Document Term Matrix to Term Document Matrix
pros_dtm <- DocumentTermMatrix(corpus_pros)
pros_tdm <- TermDocumentMatrix(corpus_pros)

cons_dtm <- DocumentTermMatrix(corpus_cons)
cons_tdm <- TermDocumentMatrix(corpus_cons)

atm_dtm <- DocumentTermMatrix(corpus_atm)
atm_tdm <- TermDocumentMatrix(corpus_atm)


# Convert TDM to matrix
pros_m <- as.matrix(pros_tdm)
cons_m <- as.matrix(cons_tdm)
atm_m <- as.matrix(atm_tdm)

# Sum rows and frequency data frame
pros_term_freq <- rowSums(pros_m)
cons_term_freq <- rowSums(cons_m)
atm_term_freq <- rowSums(atm_m)

# Sort term_frequency in descending order
pros_term_freq  <- sort(pros_term_freq, decreasing = T)
cons_term_freq <-  sort(cons_term_freq, decreasing = T)
atm_term_freq <- sort(atm_term_freq, decreasing = T)


# View the top 10 most common words
pros_term_freq[1:50]
cons_term_freq[1:10]
atm_term_freq[1:10]

# Plot a barchart of the 20 most common words
barplot(pros_term_freq[1:20], col = "steel blue", las = 2)
barplot(cons_term_freq[1:20], col = "steel blue", las = 2)
barplot(atm_term_freq[1:20], col = "steel blue", las = 2)

# Converting Term Document Matrix into Data Frame
pros_word_freq <- data.frame(term = names(pros_term_freq),num = pros_term_freq)
cons_word_freq <- data.frame(term = names(cons_term_freq),num = cons_term_freq)
atm_word_freq <- data.frame(term = names(atm_term_freq),num = atm_term_freq)


# Create a wordcloud for the values in word_freqs
library(wordcloud)
wordcloud(pros_word_freq$term, pros_word_freq$num,max.words=200, 
                random.order=F, colors=brewer.pal(8,"Paired")) 
wordcloud(cons_word_freq$term, cons_word_freq$num,max.words=200, 
                random.order=F, colors=brewer.pal(8,"Spectral"))
wordcloud(atm_word_freq$term, atm_word_freq$num,max.words=200, 
                random.order=F, colors=brewer.pal(8,"Dark2"))


######################
#                    #
#      Modeling      #
#                    # 
######################

#######################
# Logistic Regression # 
#######################

# Convert 'left' to factor variable
library(dplyr)
empData$employeeNumber <- rownames(empData)
empData$time_spend_company = as.numeric(empData$time_spend_company)

emp.good <- empData %>% 
        filter(last_evaluation >= 0.65 | time_spend_company >= 4 | number_project >= 4)

emp.good$left = factor(emp.good$left, labels=c('Remained', 'Left'))
emp.good$salary = factor(emp.good$salary)

# implement cross validation
library(caret)
train_control<- trainControl(method="cv", number=10, repeats=5)
train_control

# training logistic regression model
modelglm <- train(left ~ . - department 
                  - employeeNumber 
                  - Work_accident,
                  data=emp.good, 
                  trControl=train_control, 
                  method='glm', 
                  family='binomial')

# Summary of the glm model
summary(modelglm)

# make predictions
predsglm <- predict(modelglm, emp.good)

# Confusion Matrix for Logistic Regression Model
cMatrixglm<- confusionMatrix(predsglm, emp.good$left)
cMatrixglm

# sensitivity
glmSensitivity <- cMatrixglm$byClass[[1]]

# Convert confusion matrix to markdown format
library(formattable)
glmDF <- data.frame("Remained" = cMatrixglm$table[,1], 
                    "Left" = cMatrixglm$table[,2], 
                    row.names = c("Predicted to stay", "Predicted to leave"))

glmDFformattable <- formattable(glmDF, align = "c", 
                                list("Predicted to stay", "Predicted to leave"))

# do summary(modelglm) to view variable significance
as.htmlwidget(glmDFformattable, width='40%')

#PLOT ROC CURVE FOR LOGISTIC REGRESSION
library(ROSE)
Log.predict <- predict(modelglm, newdata = emp.good)
roc.curve(emp.good$left, Log.predict, col = 'darkviolet')


#################
# Decision Tree #
#################

# Decision Tree model 
library(rpart)
modelrpart<- train(left~. - employeeNumber, 
                   data=emp.good, 
                   trControl=train_control, 
                   method="rpart")

# make predictions
predsrpart<- predict(modelrpart, emp.good)

# summarize results
cMatrixrpart <- confusionMatrix(predsrpart, emp.good$left)
# Confusion Matrix for Decision Tree Model
cMatrixrpart

rpartSensitivity <- cMatrixrpart$byClass[[1]]

# Convert confusion matrix to markdown format
library(formattable)
rpartDF <- data.frame("Remained" = cMatrixrpart$table[,1], 
                      "Left" = cMatrixrpart$table[,2], 
                      row.names = c("Predicted to stay", "Predicted to leave"))

rpartDFformattable <- formattable(rpartDF, align = "c", 
                                  list("Predicted to stay", "Predicted to leave"))

as.htmlwidget(rpartDFformattable, width='30%')


# plotting the decision trees
library(rpart.plot)
rpart.plot(modelrpart$finalModel, type = 2, fallen.leaves = F, cex = 1, extra = 2)

# Plot ROC curve for decision tree model
library(ROSE)
Tree.predict <- predict(modelrpart, newdata = emp.good)
roc.curve(emp.good$left, Tree.predict, col = 'darkviolet')

##################################
# Random Forest Prediction Model #
##################################

hr <- read.csv("HR.csv")
empData <- hr[,1:10]

library(dplyr)
emp.good <- empData %>% 
        filter(last_evaluation >= 0.65 | time_spend_company >= 4 | number_project >= 4)

library(caret)
train_control<- trainControl(method="cv", number=10, repeats=5)
set.seed(1234)

mtry <- sqrt(ncol(emp.good))
tunegrid <- expand.grid(.mtry=mtry)

# Random Forest Model
# Execution takes approx 2 minutes
# Please Wait...
modelrf <- train(as.factor(left)~., 
                 data=emp.good, 
                 method="rf", 
                 metric="Accuracy", 
                 tuneGrid=tunegrid, 
                 trControl=train_control) 
# Execution takes approx 2 minutes
# Please Wait...

# Summary of Random Forest Model
summary(modelrf)
print(modelrf)
modelrf$bestTune$mtry

# Maximum attained Accuracy for RF model
max(modelrf$results$Accuracy)

# Make predictions
emp.good_rf_pred <- predict(modelrf, emp.good)

# Confusion Matrix for Random Forest Prediction Model
confusionMatrix(emp.good_rf_pred, as.factor(emp.good$left))
cMatrixrf <- confusionMatrix(emp.good_rf_pred, as.factor(emp.good$left))

# Convert confusion matrix to markdown format
library(formattable)
rfDF <- data.frame("Remained" = cMatrixrf$table[,1], 
                      "Left" = cMatrixrf$table[,2], 
                      row.names = c("Predicted to stay", "Predicted to leave"))

rfDFformattable <- formattable(rfDF, align = "c", 
                                  list("Predicted to stay", "Predicted to leave"))

as.htmlwidget(rfDFformattable, width='30%')

# Plot ROC curve for Random Forest Model
library(ROSE)
R.predict <- predict(modelrf, newdata = emp.good)
roc.curve(emp.good$left, R.predict, col = 'darkviolet')

########### Random Forest Deployment #############

empData$employeeNumber <- rownames(empData)

emp.good <- empData %>% 
        filter(last_evaluation >= 0.65 | time_spend_company >= 4 | number_project >= 4)
emp.good$left = factor(emp.good$left, labels=c('Remained', 'Left'))

# The most valued employees have the highest evaluations
# The employees about to leave have leaving probabilities closest to 1
leavingProbability <- predict(modelrf, emp.good, type='prob')

# Adding relevant columns to the new data frame for leaving probability
# add the RF prediction model to the employee data
leavingProbability$employeeNumber <- emp.good$employeeNumber
leavingProbability$department <- emp.good$department
leavingProbability$salary <- emp.good$salary
leavingProbability$hasLeft <- emp.good$left
leavingProbability$last_evaluation <- emp.good$last_evaluation
leavingProbability <- subset(leavingProbability, hasLeft == 'Remained')[2:7]

# Renaming columns and rounding off the probabilities to 3 decimal places
colnames(leavingProbability)[c(1, 3)] <- c("probLeaving", 'department')
leavingProbability$probLeaving <- round(leavingProbability$probLeaving, 3)

# filtering top 200 employees
# arranging them in descending order of their probability to leave company
leavingProbability %>%
        arrange(desc(probLeaving)) %>%
        select(employeeNumber, department, salary, probLeaving, last_evaluation) %>%
        head(200) -> ProbTable
head(ProbTable)


# Output of below code will open in HTML viewer with 200 entries
library(DT)
datatable(ProbTable, options = list(
        columnDefs = list(list(className = 'dt-center', targets = c(1, 4, 5)))
))
