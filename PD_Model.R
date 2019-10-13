install.packages("auc")
install.packages("pROC")
install.packages("ggplot2")
install.packages("gmodels")
install.packages("PerformanceAnalytics")
install.packages("ROCR")
install.packages("ROSE")
install.packages("rpart")
install.packages("car")
install.packages("plyr")
install.packages("dplyr")
install.packages("rpart.plot")
install.packages("corrplot")
install.packages("RColorBrewer")


library("auc")
library("pROC")
library("ggplot2")
library("gmodels")
library("PerformanceAnalytics")
library("ROCR")
library("ROSE")
library("rpart")
library("car")
library("plyr")
library("dplyr")
library("rpart.plot")
library("corrplot")
library("RColorBrewer")

# Reading the data
credit <- readRDS("credit_data.rds")

#Checking the inial values & structure of data
head(credit)
str(credit)

# Calculating default rate
default_rate = (nrow(subset(credit1,loan_status==1))/nrow(credit1))*100
# 11.254% 


#Missing values treatment
per <- (count(subset(credit, ir_cat== "Missing"))/nrow(credit))*100 # 9.54% >5% therefore we cannot remove them directly
# Dataset is already treated for missing values 

#EDA

#Univariate Analysis
CrossTable(credit$grade) # maximum number of observations in grade A 
plot(credit$grade)

CrossTable(credit$home_ownership)# proportion of on mortgage and on rent category is maximum
plot(credit$home_ownership)

CrossTable(credit$emp_cat) # proportion of 0-15 employee category is maximum
plot(credit$emp_cat)

n_breaks <- sqrt(nrow(credit))
# Very less loan takers above annual_income above 1000000$
plot(credit$annual_inc,ylab="annual income")

n_breaks <- sqrt(nrow(credit))
#  maximum number of loan takers have loan amount around 10000$
hist(credit$loan_amnt,breaks = n_breaks,xlab = "Loan_amnt", main = "Histogram of Loan Amount")

n_breaks <- sqrt(nrow(credit))
# Number of loan takers are decreasing as the age is increasing
hist(credit$age,breaks = n_breaks,xlab = "Age", main = "Histogram of Age")
plot(credit$age,ylab="age")

#Bivariate Analysis

CrossTable(credit$home_ownership, credit$loan_status, prop.r = TRUE,  prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
# borrowers having homes on rent and mortgaes are more probable to default
ggplot(data= credit, aes(x =home_ownership,  fill = factor(loan_status))) + geom_bar()+labs(x="Home Ownership",colour = 'Loan Status' )


CrossTable(credit$grade, credit$loan_status, prop.r = TRUE,  prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
# proportion of defaults are increasing as the grade is decreasing from A to G
ggplot(data= credit, aes(x =grade,  fill = factor(loan_status))) + geom_bar(position = "fill") #Proportion of deafults are increasing as the grade is decreasing

CrossTable(credit$emp_cat, credit$loan_status, prop.r = TRUE,  prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
# proportion of deafaulters from rmp_cat 45+ is high
ggplot(data= credit, aes(x =emp_cat,  fill = factor(loan_status))) + geom_bar(position = "fill") 

CrossTable(credit$ir_cat, credit$loan_status, prop.r = TRUE,  prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
#proportion of default for ir_cat 13.5+ is high 
ggplot(data= credit, aes(x =ir_cat,  fill = factor(loan_status))) + geom_bar(position = "fill") # Maximum proportion of defaults in 13.5+ followed by 11-13.5 category

n_breaks <- sqrt(nrow(credit))
# Hardly any default above 500000$     
ggplot(data= credit, aes(x = annual_inc,fill = factor(loan_status)))+ geom_histogram()+xlab("annual_inc")  +scale_x_continuous(n_breaks)+xlim(c(0,500000)) +labs(x='Annual income',y='Count',colour = 'Loan Status' )

# Number of loan takers are decreasing as the age is increasing
ggplot(data= credit, aes(x = age,fill = factor(loan_status)))+labs(x='Age',y='Count',colour = 'Loan Status' )+ geom_histogram() +scale_x_continuous(n_breaks)


# We can see here that as age is increasing annual income is decreasing. Also the defaults are concentrated for lower income groups having lesser age 
ggplot(data=credit, aes(y=age,x=annual_inc,color=factor(loan_status))) +labs(x = 'Annual Income' , y = 'Age' , color = 'Default Status') +geom_point()+geom_smooth(se = F , color = 'darkred' , method = 'loess')

# We can conclude that defaults are concentrated for lower income and higher loan amount group.
# Also we can see as the annual income is increasing the loan amount is also increasing. Also there are few extreme cases where annual income is high and the loan amount is less 
ggplot(data=credit, aes(y=loan_amnt,x=annual_inc,color=factor(loan_status))) +geom_point()+geom_smooth(se = F , color = 'darkred' , method = 'loess') + labs(x = 'Annual Income' , y = 'Loan Amount' , color = 'Default Status')


#outliers removal

# Based on EDA we are building our model on the following logic & those data points that contradict this logic will be treated as outlier:-
# Probability of default is increasing as grade decreases from A to G
# As the age is incresing, annual income is increasing and therefore probability of default is decreasing
# As the annual income is increasing the Loan amount is also increasing. And probability of default is decreasing

#Checking for outliers first

#from the plot we can see the presence of outliers in the annual income & age
# Outliers can only be present in the numerical variable

#Visualizing distribution through box plot
ggplot(data = credit, aes(x = factor(loan_status), y = annual_inc)) +  geom_boxplot()+labs(x="Loan Status",y = "Annual Income")
# We can see that whiskers are not present on one side but we cannot say that how many of these data points do not conform to our model therefore using IQR concept.
outlier_cutoff <- quantile(credit$annual_inc,0.75) + 1.5*IQR(credit$annual_inc)
# Outlier cutoff is comming to be 140000 but from EDA we found that above 1000000$ annual income the loan amount was decreasing also there were no deafults in this range so we can safely remove these points
(nrow(subset(credit,annual_inc>1000000))/nrow(credit))*100 # 0.02% of the data will be removed
index_oulier <- which(credit$annual_inc>1000000)
credit_new <-  credit[-index_oulier,]
hist(credit_new$annual_inc,  sqrt(nrow(credit_new)), xlab =  "Annual income rule of thumb" )
ggplot(data = credit_new, aes(x = factor(loan_status), y = annual_inc)) +  geom_boxplot()

# age 
ggplot(data = credit_new, aes(x = factor(loan_status), y = age)) +  geom_boxplot()+labs(x= "Loan Status", y="age")
outlier_cutoff2 <- quantile(credit_new$age,0.75) + 1.5*IQR(credit_new$age)
# Cutoff by IQR is coming out to be 40.5 but we cannot remove data points above 40.5 years age as it will create bias is the model
#Therefore removing only values greater than 80 years of age
nrow(subset(credit_new,age>80)) # Only two observations will be removed
index_oulier2 <- which(credit_new$age>80)
credit2 <- credit_new[-index_oulier2,]  
ggplot(data = credit2, aes(x = factor(loan_status), y = age)) +  geom_boxplot()+labs(x= "Loan Status", y="age")

#Standardizing the data
data <- cbind(as.data.frame(scale(credit2[,c(2,5,6)])),as.data.frame(credit2[,c(1,3,4,7,8)]))

# test and training set
set.seed(567)
index_train <- sample(1:nrow(data), 2 / 3 * nrow(data))
training_set <- data[index_train, ]
test_set <- data[-index_train, ]


#Model Development
# correlation matrix

M <-cor(mtcars)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
cor(data[,c(1,2,3)])# Annual_inc and loan_amnt have weak correlation

#Since its a classification model we will be using logistic and decision tree model
# Logistic Regression Model

#model using all variables
log_mod1 <- glm(loan_status ~. ,family= "binomial",data=training_set)
summary(log_mod1)# p_value of loan_amnt, home_ownership, emp_cat >>0.05 and p_value of age 0.15 
predict_log_mod1 <- predict(log_mod1, newdata = test_set,type = "response")
vif(log_mod1) # vif of grade and ir_cat was found to be high so first removing grade
simple_summary(predict_log_mod1,0.1,0.48)
#"best_accuracy 0.891376108933361"
#"best_auc 0.607329750291599"
#"best_sensitivity 0.737891737891738"
#"best accuracy cutoff 0.39"
#"best auc cutoff 0.11"
#"best sensitivity cutoff 0.1"


#Removing the insignificant variables & building model on grade, annual_inc & ir_cat
log_mod2 <- glm(loan_status ~ grade+annual_inc+ir_cat,family= "binomial",data=training_set)
summary(log_mod2)
predict_logmod2 <- predict(log_mod2, newdata = test_set,type = "response")
vif(log_mod2) # vif of grade and ir_cat found to be significantly high
simple_summary(predict_logmod2,0.1,0.43)
#"best_accuracy 0.891479265525067"
#"best_auc 0.608456525807913"
#"best_sensitivity 0.730294396961064"
#"best accuracy cutoff 0.41"
#"best auc cutoff 0.11"
#"best sensitivity cutoff 0.1"

# We found that the AUC has improved for model when we included grade, annual_inc and ir_cat

# Removing grade and building the model on rest two
log_mod3 <- glm(loan_status ~ ir_cat+annual_inc ,family= "binomial",data=training_set)
summary(log_mod3)
predict_logmod3 <- predict(log_mod3, newdata = test_set,type = "response")
vif(log_mod3)
simple_summary(predict_logmod3,0.1,0.23)
#"best_accuracy 0.890757169383124"
#"best_auc 0.601166472304072"
#"best_sensitivity 0.743589743589744"
#"best accuracy cutoff 0.22"
#"best auc cutoff 0.1"
#"best sensitivity cutoff 0.1"

#Since auc decreased therefore including grade and removing ir_cat
log_mod4 <- glm(loan_status ~ grade+annual_inc,family= "binomial",data=training_set)
summary(log_mod4)
predict_logmod4 <- predict(log_mod4, newdata = test_set,type = "response")
vif(log_mod4) # vif of grade and ir_cat found to be significantly high
simple_summary(predict_logmod4,0.1,0.44)
#"best_accuracy 0.891582422116773"
#"best_auc 0.612465549683464"
#"best_sensitivity 0.760683760683761"
#"best accuracy cutoff 0.39"
#"best auc cutoff 0.12"
#"best sensitivity cutoff 0.1"

#AUC has improved in the model by including only grade and annual income 

#In our hypothesis we concluded that age could be one of default determining factor Therefore, including age 
log_mod5 <- glm(loan_status ~ grade+annual_inc+age ,family= "binomial",data=training_set)
summary(log_mod5)
predict_logmod5 <- predict(log_mod5, newdata = test_set,type = "response")
vif(log_mod5) # vif of grade and ir_cat found to be significantly high
simple_summary(predict_logmod5,0.1,0.45)
#"best_accuracy 0.891582422116773"
#"best_auc 0.61366876239769"
#"best_sensitivity 0.760683760683761"
#"best accuracy cutoff 0.39"
#"best auc cutoff 0.12"
#"best sensitivity cutoff 0.1"

# AUC has increased


# Selecting the cutoff
# At 0.12 cutoff log_mod5 (using grade and annual_inc) we are getting an optimized value of accuracy & sensitivity. After this
# the sensitivity is decreasing and our aim should be keeping the sensitivity high
#"accuracy 0.601918712605735"
#"cutoff 0.12"
#"auc 0.61366876239769"
#"sensitivity 0.628679962013295"

#function for determining cutoff
simple_summary <- function(predictions,lower,upper) {
  cutoff <- lower
  max_acc <- 0
  auc_max <- 0
  sensitivity_max <- 0
  accuracy <- rep(NA,50)
  sensitivity <- rep(NA,50)
  specificity <- rep(NA,50)
  for(i in 1:50){
    predictions_all_full <-  ifelse(predictions>cutoff,1,0)
    auc_new <- auc(test_set$loan_status,predictions_all_full)
    table_mat <- table(test_set$loan_status, predictions_all_full)
    accuracy[i] <- sum(diag(table_mat)) / sum(table_mat)
    sensitivity[i] <- table_mat[2,2]/sum(table_mat[2,1:2])
    specificity[i] <- table_mat[1,1]/sum(table_mat[1,1:2])
    print(paste("accuracy",accuracy_Test))
    print(paste("cutoff",cutoff))
    print(paste("auc",auc_new))
    print(paste("sensitivity",sensitivity))
    if( accuracy_Test>max_acc){
      max_acc = accuracy_Test
      best_cutoff <- cutoff
    }
    if(auc_new>auc_max){
      auc_max = auc_new
      best_cutoff1 <- cutoff
    }
    if(sensitivity>=sensitivity_max){
      sensitivity_max = sensitivity
      best_cutoff2 <- cutoff
    }
    cutoff <- cutoff+0.01
  }
  print(paste("best_accuracy",max(accuracy)))
  print(paste("best_auc",auc_max))
  print(paste("best_sensitivity",max(sensitivity)))
  print(paste("best accuracy cutoff",best_cutoff))
  print(paste("best auc cutoff",best_cutoff1))
  print(paste("best sensitivity cutoff",best_cutoff2))
}

#decision tree model
# Not a good model
mod9 <- rpart(loan_status ~ .,method="class",data = training_set)
summary(mod9)
predict9 <-predict(mod9, test_set, type = 'p')
printcp(mod9)

#We have to provide a control parameter 
dec_mod1 <- rpart(loan_status ~ .,method="class",data = training_set,control = rpart.control(cp = 0.001))
summary(dec_mod1)
predict_dec_mod1 <-predict(dec_mod1, test_set, type = 'p')
printcp(dec_mod1)

#Including only annual income , grade and age
dec_mod1 <- rpart(loan_status ~ grade+annual_inc+age,method="class",data = training_set,control = rpart.control(cp = 0.001))
summary(dec_mod1)
predict_dec_mod1 <-predict(dec_mod1, test_set, type = 'p')
printcp(dec_mod1)

# We have to undersample the data first 

#Undersampling
data_balanced_under <- ovun.sample(loan_status ~ ., data = training_set, method = "under", N = 6000, seed = 1)$data
table(data_balanced_under$loan_status)
dec_mod3 <- rpart(loan_status ~ .,method="class",data =data_balanced_under,control = rpart.control(cp = 0.001))
predict_dec_mod3 <-predict(dec_mod3,binned_test, type = 'p')
# We can find that minimum xerror is achieved for cp = 0.003104876
printcp(dec_mod3) 
plotcp(dec_mod3)
plot(dec_mod3)
text(dec_mod3,use.n = TRUE,extra =1)

# Pruning the above decision tree model based on cp=0.003104876
cp_critical <- dec_mod3$cptable[which.min(dec_mod3$cptable[,"xerror"]),"CP"]
dec_mod3_pruned <- prune(dec_mod3,cp =cp_critical)
predict_dec_mod3_pruned <- predict(dec_mod3_pruned,test_set,type = 'p')
printcp(dec_mod3_pruned) 
plotcp(dec_mod3_pruned)
plot(dec_mod3_pruned)
text(dec_mod3_pruned,use.n = TRUE,extra =1)
prp(dec_mod3_pruned)

# Creating another model using only annual income grade and age variables 
set.seed(400)
dec_mod4 <- rpart(loan_status ~ grade+annual_inc+age,method="class",data =data_balanced_under,control = rpart.control(cp = 0.001))
predict_dec_mod4 <-predict(dec_mod4,binned_test, type = 'p')
# We can find that minimum xerror is achieved for cp = 0.003104876
printcp(dec_mod4) 
plotcp(dec_mod4)
plot(dec_mod4)
text(dec_mod4,use.n = TRUE,extra =1)


# Pruning the above decision tree model based on cp=0.003104876
cp_critical <- dec_mod4$cptable[which.min(dec_mod4$cptable[,"xerror"]),"CP"]
dec_mod4_pruned <- prune(dec_mod4,cp =cp_critical)
predict_dec_mod4_pruned <- predict(dec_mod4_pruned,test_set,type = 'p')
printcp(dec_mod4_pruned) 
plotcp(dec_mod4_pruned)
plot(dec_mod4_pruned)
text(dec_mod4_pruned,use.n = TRUE,extra =1)
prp(dec_mod4_pruned,extra = 1)

#Trying another method of undersampling by giving prior probability

#Strategy curve

strategy_cutoff <- function(prob_of_def){
  cutoff <- rep(NA, 26)
  bad_rate <- rep(NA, 26)
  accept_rate <- seq(0.75,0.5,by=-0.01)
  acc <- rep(NA,26)
  sensit <- rep(NA,26)
  auc_new <- rep(NA,26) 
  for (i in 1:26){
    cutoff[i]=quantile(prob_of_def,accept_rate[i])
    pred_i=ifelse(prob_of_def> cutoff[i], 1, 0)
    pred_as_good=test_set$loan_status[pred_i==0]
    bad_rate[i]=sum(pred_as_good)/length(pred_as_good)
    auc_new[i] <- auc(test_set$loan_status,pred_i)
    table_mat <- table(test_set$loan_status, pred_i)
    acc[i] <- sum(diag(table_mat)) / sum(table_mat)
    sensit[i] <- table_mat[2,2]/sum(table_mat[2,1:2]) }
    table=cbind(accept_rate,cutoff=round(cutoff,4),bad_rate=round(bad_rate,4),accuracy=round(acc,4),sensitivity=round(sensit,4),AUC = round(auc_new,4))
    return(list(table=table,bad_rate=bad_rate, accept_rate=accept_rate, cutoff=cutoff,accuracy=acc,sensitivity=sensit))
}
# Strategy table for decision tree model
strategy_dec <- strategy_cutoff(predict_dec_mod4_pruned[,2])
strategy_dec$table

#Strategy table for logistic model
strategy <- strategy_cutoff(predict_logmod5)
strategy$table
strategy_data <- data.frame(strategy)
ggplot(data = strategy_data, aes(x= accept_rate,y=bad_rate))+geom_line()

