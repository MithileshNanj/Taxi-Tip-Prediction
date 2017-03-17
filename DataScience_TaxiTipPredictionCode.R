install.packages("caret")
install.packages("randomForest")
install.packages("mlbench")
install.packages("mice")
install.packages("mboost")
install.packages("anytime")
library("anytime")
library("mice")
library("caret")
library("randomForest")
library("pbkrtest")
library("mlbench")
library("anytime")
library("mboost")

############################################################################################################
#Solution 1

#Function to read the file and return rows and columns
Question_1 <- function(x)
{
  data <- read.csv(x,header = TRUE)
  column_count <- ncol(data)
  row_count <- nrow(data)
  q1 <- list("data" = data, "column_count" = column_count, "row_count" = row_count)
  return(q1)
}

#Main function for solution 1
x <- "green_tripdata_2015-09.csv"
#Function call
list <- Question_1(x)
data <- list$data
cat(sprintf(" Number of columns in the data set = %d \n Number of rows in the data set = %d",list$column_count,list$row_count)) 

############################################################################################################


############################################################################################################
#Solution 2

#Function to plot histograms and boxplots with and without outliers

Question_2 <- function(TD)
{
  bp_wo <- boxplot(TD, main = "Boxplot of Trip Distance with outliers", xlab = "Trip Distance")
  hp_wo <- hist(TD, main = "Histogram plot of Trip Distance with outliers", xlab = "Trip Distance")
  #Outliers from boxplot statistics
  outlier <- boxplot.stats(TD)$out
  #replacing outliers with NAs
  TD_1  <- ifelse(TD %in% outlier, NA, TD)
  #Plotting without outliers
  bp_woo <- boxplot(TD_1, main="Boxplot of Trip Distance without outliers",xlab = "Trip Distance")
  hp_woo <- hist(TD_1, main="Histogram plot of Trip Distance without outliers",xlab = "Trip Distance")
  dp_wo  <- plot(density(TD, na.rm=TRUE),main="Density plot of Trip distance with outliers",xlab = "Trip Distance")
  dp_woo <- plot(density(TD_1, na.rm=TRUE),main="Density plot of Trip distance without outliers",xlab = "Trip Distance")
}

#Main function for Solution 2
Trip_distance <- data$Trip_distance
#Function call for solution 2
Question_2(Trip_distance)

############################################################################################################



############################################################################################################
#Solution 3

#subsetting pickup time and trip distance from the parent data set
subset_data      <- data[,c("lpep_pickup_datetime", "Trip_distance")]
#Creating a derived variable hour by extracting hour from pickup time
subset_data$hour <- as.POSIXlt(subset_data$lpep_pickup_datetime)$hour
#Grouping Trip distance by hour and summarizing by mean and median
agg_data <- aggregate(subset_data$Trip_distance ~ hour, data = subset_data, FUN = function(x) c(mean_TD = signif(mean(x),3), median_TD = signif(median(x),3)))
#Plotting median trip distance
plot(agg_data[,1], agg_data$`subset_data$Trip_distance`[,2], type = 'l', main = "Median trip distance by hour of day", xlab = "Hour of day", ylab = "Median trip distance")

#Creating a duplicate data set to work on
data_airport <- data
#Checking latitude longitude co-ordinates for airports origin or destination
data_airport$Airport <- 

  ifelse(((data_airport$Pickup_longitude > -73.79 & data_airport$Pickup_longitude < -73.76) & 
        (data_airport$Pickup_latitude  >  40.62 & data_airport$Pickup_latitude  < 40.69))
       | ((data_airport$Dropoff_longitude > -73.79 & data_airport$Dropoff_longitude < -73.76) & 
          (data_airport$Dropoff_latitude > 40.62 & data_airport$Dropoff_latitude < 40.69)),"JFK", 
  ifelse(((data_airport$Pickup_longitude > -73.89 & data_airport$Pickup_longitude < -73.84) & 
            (data_airport$Pickup_latitude  >  40.76 & data_airport$Pickup_latitude  < 40.81))
         | ((data_airport$Dropoff_longitude > -73.89 & data_airport$Dropoff_longitude < -73.84) & 
              (data_airport$Dropoff_latitude > 40.76 & data_airport$Dropoff_latitude < 40.81)),"LG",
  ifelse(((data_airport$Pickup_longitude > -74.30 & data_airport$Pickup_longitude < -74.10) & 
            (data_airport$Pickup_latitude  >  40.60 & data_airport$Pickup_latitude  < 40.75))
         | ((data_airport$Dropoff_longitude > -74.30 & data_airport$Dropoff_longitude < -74.10) & 
              (data_airport$Dropoff_latitude > 40.60 & data_airport$Dropoff_latitude < 40.75)),"N","O")))
data_airport$Airport <- as.factor(data_airport$Airport)
#count of transaction, average fair, average time taken, average time taken at time of day
subset_airport <- subset(data_airport, data_airport$Total_amount > 0)
subset_airport$travel_time <- as.double(difftime(subset_airport$Lpep_dropoff_datetime, subset_airport$lpep_pickup_datetime, units = "hours"))
subset_airport$hour <- format(as.POSIXct(subset_airport$lpep_pickup_datetime),"%H")
airport_data <- as.data.frame(table(subset_airport$Airport))
#This table gives the number of transactions and average travel time by airports
airport_data$mean <- aggregate(cbind(subset_airport$Total_amount,subset_airport$travel_time) ~ subset_airport$Airport, data = subset_airport, FUN = "mean")
airport_data
#This table gives average travel time by hour of day for all airports
airport_data_time_of_day <- aggregate(subset_airport$travel_time ~ subset_airport$Airport + subset_airport$hour, data = subset_airport, FUN = "mean")
airport_data_time_of_day

############################################################################################################


############################################################################################################

#Solution_4


#All Functions
#Functions for pre-processing
#Function to impute data
impute_data<-function(n_data,c_data,categoricalvars)
{
  #numerical variables are imputed using pmm 
  missingdata<-mice(n_data,m=1,method = "pmm",maxit=1,seed=500)
  imputed_data_n<-complete(missingdata,1)
  #in case of categorical variables, another level or class 
  #is introduced for missing values (blanks = class M)
  #replacing blanks with "M"
  for (i in categoricalvars){levels(c_data[[i]])[is.na(c_data[[i]]) == TRUE] <- "M"}
  imputed_data<-cbind(c_data,imputed_data_n)
  return(imputed_data)
}


#Function for feature Selection
feature_selection<-function(imputed_data)
{
  set.seed(10)
  #Cross validation
  control<-rfeControl(functions=rfFuncs, method="cv", number=10,verbose = FALSE)
  #Feature elimination by random forest
  output <- rfe(imputed_data[,names(imputed_data)!= "Tip_percentage"], imputed_data$Tip_percentage, sizes = c(1:10), metric = "RMSE", maximize = FALSE, rfeControl=control)
  featuresselected<-predictors(output)
  return(featuresselected)
}

#Functions for predictive model
#function for multiple linear regression
linear_regression<-function(data1, test)
{
  lr_model<-train(Tip_percentage~.,data=data1,method="lm",
                  trControl=trainControl(method="repeatedcv",repeats=5))
  print(lr_model)
  pred<-predict(lr_model,test)
  MSE_LM <- mean((test$Tip_percentage - pred)^2)
  p <- ggplot(aes(x=actual, y=pred),
              data=data.frame(actual=test$Tip_percentage, pred=pred))
  print(p + geom_point() +
          geom_abline(color="red") +
          ggtitle(paste("Linear Regression in R MSE=", MSE_LM, sep="")))
  return(pred)
}


#Function for Boosted generalized linear model
boosted_generalized_linear_model<-function(trainingpredict,test)
{
  #training a boosted generlized linear model
  glm1<-glmboost(Tip_percentage~.,
                 data=trainingpredict,center = FALSE,control=boost_control(mstop = 100))
  coef(glm1,which="")
  plot(glm1,off2int = TRUE,main= "Boosted GLM Co-efficients")
  print(glm1)
  #predict method
  predict(glm1,type='response')
  #target containing predicted values
  test$predicted<-predict(glm1,test)
  MSE_GBLM <- mean((test$Tip_percentage - test$predicted)^2)
  p <- ggplot(aes(x=actual, y=pred),
             data=data.frame(actual=test$Tip_percentage, pred=test$predicted))
  print(p + geom_point() +
       geom_abline(color="red") +
       ggtitle(paste("Gradient Boosted Linear Moldel in R MSE=", signif(MSE_GBLM,4), sep="")))
  return(test[,(colnames(test) %in% c("Tip_percentage","predicted"))])
}


#function for Random Forest prediction 
random_forest<-function(train)
{
  rf <- randomForest(Tip_percentage~ ., data=train, ntree=50)  
  print(rf)
  #predo<-predict(rf, validate[,names(validate)!="target"])  
  # mse <- mean((validate$target - predo)^2)
  # 
  # p <- ggplot(aes(x=actual, y=pred),
  #             data=data.frame(actual=validate$target, pred=predo))
  # print(p + geom_point() +
  #   geom_abline(color="red") +
  #   ggtitle(paste("RandomForest Regression in R MSE=", mse, sep="")))
  return(predo)
}


#Main function for Solution 4

#Data Pre-processing
#Check for duplicates
data<-unique(data)

#Drop the column which had all missing values
drop <- "Ehail_fee"
#Create a derived target variable Tip_percentage
data$Tip_percentage <- ifelse(is.na(signif((data$Tip_amount/data$Total_amount)*100,3)) == TRUE,0,signif((data$Tip_amount/data$Total_amount)*100,3))
TipPrediction_data  <- data[,!(names(data) == drop)]

#Eliminate outliers in Tip percentage
outlier_tip_percentage <- boxplot.stats(TipPrediction_data$Tip_percentage)$out
TipPrediction_data$Tip_percentage    <-
  ifelse(TipPrediction_data$Tip_percentage %in% outlier_tip_percentage, NA, TipPrediction_data$Tip_percentage)

TipPrediction_data <- TipPrediction_data[complete.cases(TipPrediction_data$Tip_percentage),]
head(TipPrediction_data)

#Impute data
#Split data into numeric and categorical for impute function to process
nums <- sapply(TipPrediction_data, is.numeric)
#Convert to POSIXct class
TipPrediction_data$lpep_pickup_datetime <- anytime(as.factor(TipPrediction_data$lpep_pickup_datetime))
TipPrediction_data$Lpep_dropoff_datetime <- anytime(as.factor(TipPrediction_data$Lpep_dropoff_datetime))
categoricalvars<-names(TipPrediction_data)[sapply(TipPrediction_data, class) == "factor"]
categoricalvars<-as.vector(categoricalvars)
numericdata <- TipPrediction_data[,nums]
categoricaldata<- subset(TipPrediction_data, select = categoricalvars)
#Function call for imputing data
imputed_data_n<-impute_data(numericdata,categoricaldata,categoricalvars)
#binding all data
imputed_data <-cbind(TipPrediction_data[,!(colnames(TipPrediction_data) %in% c(colnames(numericdata),categoricalvars))],imputed_data_n) 


#Ignoring highly correlated features
set.seed(10)
#numeric and categorical data split after imputation - training data
nums_2 <- sapply(imputed_data, is.numeric)
numericdata_imp<- imputed_data[,nums_2]
#finding correlation matrix for numeric data
correlationMatrix <- cor(numericdata_imp[,names(numericdata_imp)!="Tip_percentage"])
#any features having a correlation greater than or equal to 0.7 will be removed
highlyCorrelatedMatrix<- findCorrelation(correlationMatrix, cutoff=0.7)
highlyCorrelatedattributes <- as.vector(colnames(numericdata_imp[highlyCorrelatedMatrix]))
feature_selected_ndata <- numericdata_imp[,!(colnames(numericdata_imp) %in% highlyCorrelatedattributes)] 
feature_selected_data  <- cbind(imputed_data[,!(colnames(imputed_data) %in% c(colnames(numericdata_imp)))],feature_selected_ndata)

#function call for feature selection
#predictors_data <- imputed_data[,which(colnames(imputed_data) != "Tip_percentage")]
#features_selected<-feature_selection(imputed_data)
#features_target<-c(features_selected,"target")

#Running predictive algorithms
#Splitting data into 80/20 train and test
smp_size <- floor(0.80 * nrow(feature_selected_data))
set.seed(400)
train_ind <- sample(seq_len(nrow(feature_selected_data)), size = smp_size)
feature_selected_train <- feature_selected_data[train_ind, ]
feature_selected_test <- feature_selected_data[-train_ind, ]

#Function call for Linear regression
predicted_values_LR<-linear_regression(feature_selected_train,feature_selected_test)
#Function call for Gradient boosted linear models
predicted_data_GLM<-boosted_generalized_linear_model(feature_selected_train,feature_selected_test)
#predicted_values_RF<-random_forest(feature_selected_sample)

############################################################################################################


############################################################################################################
#Solution 5

#Function to pre-process and construct derived variable for anova test
pre_processing <- function(imputed_data)
{
  imputed_data$lpep_pickup_datetime <- anytime(as.factor(imputed_data$lpep_pickup_datetime))
  imputed_data$Lpep_dropoff_datetime <- anytime(as.factor(imputed_data$Lpep_dropoff_datetime))
  #Derived variable Travel time
  imputed_data$travel_time <- as.double(difftime(imputed_data$Lpep_dropoff_datetime, imputed_data$lpep_pickup_datetime, units = "hours"))
  #Derived variable Average speed
  imputed_data$averageSpeed <- ifelse(is.nan(imputed_data$Trip_distance/imputed_data$travel_time) == TRUE | imputed_data$travel_time == 0, 0, imputed_data$Trip_distance/imputed_data$travel_time)
  #Derived variable week number
  imputed_data$week <- (as.numeric(difftime(as.Date(imputed_data$lpep_pickup_datetime), as.Date(imputed_data$lpep_pickup_datetime[1]), units = "days"))) %/% 7
  imputed_data$week <- as.factor(imputed_data$week)
  #Derived variable hour and minute
  imputed_data$hour <- format(as.POSIXct(imputed_data$lpep_pickup_datetime),"%H:%M")
  imputed_data$hour <- as.factor(imputed_data$hour)
  #Removing outliers
  outlier <- boxplot.stats(imputed_data$averageSpeed)$out
  imputed_data$averageSpeed_woo <- ifelse(imputed_data$averageSpeed %in% outlier, NA, imputed_data$averageSpeed) 
  hist(imputed_data$averageSpeed_woo)
  week_data <- imputed_data[,colnames(imputed_data) %in% c("week","hour","averageSpeed_woo")]  
}

#Function to test the assumptions of anova
anova_test <- function(week_data)
{
  week_list <- levels(week_data$week)
  
  for(i in week_list)
  {
    boxplot(subset(week_data, week_data$week == i)$averageSpeed_woo, main = paste("boxplot of average speed in week ",i))
    qqnorm(subset(week_data, week_data$week == i)$averageSpeed_woo, main = paste("Normal Q-Q plot of average speed in week ",i))
    print(paste("sd of average speed week ",i,sd(subset(week_data, week_data$week == i)$averageSpeed_woo, na.rm = TRUE)))
    hist(subset(week_data, week_data$week == i)$averageSpeed_woo, main = paste("histogram of average speed in week ",i), xlab = "Average Speed")
  }
  
}

#boxplot(week_0$averageSpeed_woo,week_1$averageSpeed_woo,week_2$averageSpeed_woo,week_3$averageSpeed_woo,week_4$averageSpeed_woo, names = c("Week 0","Week 1", "Week 2", "Week 3", "Week 4"), ylab = "Average Speed", main = "Histogram plot of average speed")

#Function call pre-processing
week_data <- pre_processing(imputed_data)
anova_data <- week_data[complete.cases(week_data[,3]),c(1,3)]

#Function call ANOVA
anova_test(anova_data)
anova_model <- aov(anova_data$averageSpeed_woo ~ anova_data$week)
summary(anova_model)

#Follow up test
follow_up <- TukeyHSD(anova_model, anova_data$week, conf.level = 0.95)
#t.test(subset(week_data, week_data$week == 0)$averageSpeed_woo,subset(week_data, week_data$week == 4)$averageSpeed_woo)
#


#Average speed as a function of time of day
aggregate_data <- as.data.frame(aggregate(week_data$averageSpeed_woo ~ hour, data = week_data, FUN = mean))
plot(aggregate_data[,1], aggregate_data[,2], type = "l", main = "Average Speed as a function of time of day", xlab = "Time", ylab = "Average Speed" )

############################################################################################################

