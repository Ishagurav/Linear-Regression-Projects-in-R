library(ggplot2)
library(data.table)
##################  Read in bikeshare.csv file and set it to a dataframe called bike. ######################
bike <- fread('bikeshare.csv')
print(head(bike))
########################3 Exploratory Data Analysis  #####################
################### Create a scatter plot of count vs temp  ########################
pl <- ggplot(bike,aes(x=temp,y=count)) + geom_point(alpha=0.3,aes(color=temp)) + theme_bw()
print(pl)
########################   convert the datetime column into POSIXct  #############################
bike$datetime <- .POSIXct(bike$datetime)
##########################  Plot count versus datetime as a scatterplot with a color gradient based on temperature. #######
pl2 <- ggplot(bike,aes(x=datetime,y=count)) + geom_point(alpha=0.5,aes(color=temp)) + theme_bw() + scale_color_continuous(low='blue',high='red')
print(pl2) 
###############  to find correlation  ############################
x <- cor(bike[,c('temp','count')])
print(x)
################  Create a boxplot, with the y axis indicating count and the x axis begin a box for each season. ###########
pl3 <- ggplot(bike,aes(x=factor(season),y=count)) + geom_boxplot(aes(fill=factor(season)))
print(pl3)
#####################   feature engineering  #####################################
########################  Create an "hour" column that takes the hour from the datetime column  #####################
bike$hour <- sapply(bike$datetime,function(x){format(x,"%H")})
bike$hour <- sapply(bike$datetime,function(x){format(x,"%H")})
bike$hour <- sapply(bike$hour,as.numeric)
print(head(bike))
################    create a scatterplot of count versus hour, with color scale based on temp. Only use bike data where workingday==1. ########################
library(dplyr)
pl <- ggplot(filter(bike, workingday ==1),aes(x=hour,y=count)) + geom_point(alpha=0.3,aes(color=temp)) + theme_bw()
pl4 <- pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.5)
pl5 <- pl4 + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red')) + theme_bw()
print(pl5)
 ##################  create a scatterplot of count versus hour, with color scale based on temp. Only use bike data where workingday==0 ##########
pl <- ggplot(filter(bike,workingday==0),aes(hour,count)) 
pl <- pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.8)
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
print(pl)
###############   build the model  #################################
############ Use lm() to build a model that predicts count based solely on the temp feature, name it temp.model ###########
temp.model <- lm(count~temp,bike)
model <- lm(count ~ . -casual - registered -datetime -atemp,bike )
print(summary(temp.model))
print(summary(model))
###########################    prediction  ###########################
temp.test <- data.frame(temp=c(25))
result <- predict(temp.model,temp.test)
print(result)