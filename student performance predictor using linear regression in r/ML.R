################################ Read CSV, note the delimiter (sep)##############################
df <- read.csv('student-mat.csv',sep=';')
################################### CLEAN THE DATA #################################
any(is.na(df))
##############################  IMPORT LIBRARIES ##################
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
library(caTools)
############################## EDA ######################################
###########################  # Grab only numeric columns   ###########################
num.cols <- sapply(df, is.numeric)
#####################################   Filter to numeric columns for correlation ######################
cor.data <- cor(df[, num.cols])
print(cor.data)
################### USE CORPLOT TO PLOT CORELATION ############################
print(corrplot(cor.data,method = 'color'))
################## WE CAN ALSO DO THIS WITH CORGRAM ##########################
corrgram(df)
corrgram(df,order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)
################################  PLOT HISTOGRAM ####################################         
ggplot(df,aes(x=G3)) + geom_histogram(bins=20,alpha=0.5,fill='blue') + theme_minimal()
################## BUILD A MODEL ################################
##################### TRAIN AND TEST DATA ###########################
########### SET A RANDOM SEED #################
set.seed(101)
#############################   Split up the sample, basically randomly assigns a booleans to a new column "sample"  ####################
sample <- sample.split(df$G3,SplitRatio = 0.7)
#######################   70% is train data  ######################################
train <- subset(df,sample==TRUE)
##########################   30% will be test  #########################
test <- subset(df,sample==FALSE)
######################   train and build model  ################################
model <- lm(G3 ~ .,train)
#################   VISUALIZE OUR MODEL ###############################
########################### Grab residuals  ###############################
res <- residuals(model)

###################### Convert to DataFrame for gglpot  ##########################
res <- as.data.frame(res)

head(res)
######################   Histogram of residuals  ##############################
ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5)
plot(model)
##########################   predictions  ####################################
G3.predictions <- predict(model,test)
####################  combine cols  ############################
results <- cbind(G3.predictions,test$G3) 
########################  rename cols  ###############################
colnames(results) <- c('pred','real')
######################  set results as df  ############################
results <- as.data.frame(results)
head(results)
#######################  take care of negative values   ##########################
to_zero <- function(x){
  if  (x < 0){
    return(0)
  }else{
    return(x)
  }
}
###################  apply zero function  #############################
results$pred <- sapply(results$pred,to_zero)

#######################  mean squared error  ##############################
mse <- mean((results$real-results$pred)^2)
print(mse)
#######################   square root of mse  #############################
print(mse^0.5)
SSE = sum((results$pred - results$real)^2)
SST = sum( (mean(df$G3) - results$real)^2)

R2 = 1 - SSE/SST
print(R2)
