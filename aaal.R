
\pagebreak 


library(tidyverse)
library(psych) #para corplot install.packages("psych")
library(corrplot) #para corrplot install.packages("psych")
library(randomForest)
library(tree) #For tree
library(forcats)
library(caret)#for confusionMatrix
 

#### CSV & Variable Names ####

#https://archive.ics.uci.edu/ml/datasets/Beijing+PM2.5+Data
car_ev <- c("https://archive.ics.uci.edu/ml/machine-learning-databases/00381/PRSA_data_2010.1.1-2014.12.31.csv")
e <- read.delim(car_ev, sep = ",")
e


  
  #No: row number
  
  #year: year of data in this row
  
  #month: month of data in this row
  
  #day: day of data in this row
  
  #hour: hour of data in this row
  
  #pm2.5: PM2.5 concentration (ug/m^3)
  
  #DEWP: Dew Point (â„ƒ)
  
  #TEMP: Temperature (â„ƒ)
  
  #PRES: Pressure (hPa)
  
  #cbwd: Combined wind direction
  
  #Iws: Cumulated wind speed (m/s)
  
  #Is: Cumulated hours of snow
  
  #Ir: Cumulated hours of rain



# (43748 rows)
# Remove all NA´s. Data reduced now to 41681 rows

  head(e)
  summary(e)
  str(e)

  delete.na <- function(e, n=0) {
  e[rowSums(is.na(e)) <= n,]
  }

  e <- delete.na(e)
  summary(e)


 
   
# change month numerical to categorical
  unique(e$month)
  unique(e$cbwd)

  month.name[unique(e$month)]
  month.abb[unique(e$month)]
  e$month <- month.abb[e$month]
  unique(e$day)



# Correlation Matrix
  corPlot(e %>% select(No, year, day, hour, pm2.5, DEWP, TEMP, PRES, Iws, Is, Ir), 
        cex = 0.5, main = "Matriz de correlación")

  corrplot(cor(e%>% select(No, year, day, hour, pm2.5, DEWP, TEMP, PRES, Iws, Is, Ir)),
         method = "circle",       
         order = "hclust",         
         hclust.method = "ward.D", 
         addrect = 2,              
         rect.col = 3,             
         rect.lwd = 3)             

  
  
    
# Barplot (PM2.5 per month and PM2.5 per day)
  e %>% ggplot(aes(x = month, y = pm2.5 ))+
    geom_col() + 
    ggtitle("PM2.5 per Month Distribution")+
    ylab("PM2.5")+
    xlab("MONTH")
  
  e %>% ggplot(aes(x = hour, y = pm2.5 ))+
    geom_col()+
    ggtitle("PM2.5 per Days Distribution")+
    ylab("PM2.5")+
    xlab("Days")

  
  

#### Scatter Plots ####
  #TEMP Temperature based on month & TEMP Temperature versus windspeed Iws
  #Temperature based on month
  ggplot(data = e)+
    geom_point(aes(x = TEMP, y = month, color = month), size = 5)+
    ggtitle("Temperature per Month Distribution")+
    ylab("Month")+
    xlab("Temperature")
    
  
#### Temperature versus windspeed ####
  ggplot(data = e)+
    geom_point(aes(x = TEMP, y = Iws, color = TEMP), size = 0.8)+
    ggtitle("Temperature per LWS Level Wind Speed")+
    ylab("LWS")+
    xlab("Temperature")

  
  
?varImpPlot()
  
#### Linear model ####
  elm <- lm(TEMP~., data = e)
  summary(elm)
  
  elm_predict <- predict(elm, newdata = e)
  
  elm_mse <- mean((elm_predict - e$TEMP)^2) #[1] 23.18344
  elm_COR <- cor(elm_predict, e$TEMP) #[1] 0.9184774
  print(paste("Linear Regression Accuracy:", elm_COR)) #[1] "Linear Regression Accuracy: 0.918477359195698"
  
  

  
  
###       HERE        ###
  
  
#### THE RANDOM FOREST #### 
  
  summary(e$month) 
  unique(data$month) # Jan to december

## The Data  
  #Based on >35.5 pm2.5 unhealthy conditions
  data = e %>% filter(pm2.5 >=35.5 & month == factor(month)) 
  str(data)
  
## The ThePM  
  thePM = unique(e$pm2.5)
  summary(thePM)
  str(thePM)
  
## The Training Model  
  training_u = sample(thePM, length(thePM)*0.80, replace = F)
  summary(training_u)
  str(training_u)
  
## The Testing Model  
  testing_u = thePM[-training_u]
  testing_u
  summary(testing_u)
  str(testing_u)

## Training 
  
  training = data %>% filter(pm2.5 %in% training_u) %>% 
    select(No, year, month, day, hour, pm2.5, DEWP, TEMP, PRES, Iws, Is, Ir, cbwd) %>% 
    mutate(month = factor(month))
  
  summary(training)
  str(training)
  
## Model
  
  ### Model con RandomForest###

  set.seed(0)  
  Model = randomForest(month ~ ., data = training, ntree = 10, method = "class", norm.votes = FALSE, do.trace = 10, proximity = FALSE, importance = TRUE)
  summary(Model)
  str(Model)
  
## Model Out of BAG
  
  muestra = list(testing_u, Model)
  Model = muestra[[2]]
  testing_u = muestra[[1]]
  testing = data %>% filter(pm2.5 %in% testing_u)
  
  Model
  
## Testing
  
  unique(testing$pm2.5)
  
  testingair = testing %>% filter(pm2.5 %in% (40:42)) %>% select(No, year, month, day, hour, pm2.5, DEWP, TEMP, PRES, Iws, Is, Ir, cbwd)  
  head(testingair)
  
  set.seed(0)
  predictionair <- predict(Model, newdata = testingair, type = "class")
  str(predictionair)
  
  
  #Now month as a factor
  
  predictionair = as.factor(predictionair)
  testingair$month = as.factor(testingair$month)
  
  predictions.rf <- predict(Model, newdata = testing)
  head(predictions.rf)
  confusionMatrix(predictions.rf, as.factor(testing$month))
  
  plot(predictions.rf, main = "Error rate of Random Forest")
  
  varImpPlot(Model, pch = 0, main = "Importance of Variables")
  
## The prediction Air (PM2.5 Air aquality)
  
  table(predictionair)
  

## Recommendation
  
  suggestion = names(sort(table(predictionair), decreasing = T))
  suggestion = names(sort(table(predictionair), decreasing = T)[1:3]) 
  suggestion  
  
  # So these months are predicted.
  
  # And Here it´s the head of the prediction, in a filter taking the suggestion:
  head(testingair %>% filter(month %in% suggestion))
  

  # And finish the suggestions have many rows
  testingair %>% filter(month %in% suggestion) %>% group_by(month)
  
  
  # How is the Temperature in 2010, in pm2.5 mayor or equal than 35.5 in the month of June 
  i1.1 = data %>% filter(month %in% "Jun" & pm2.5 >=35.5 & year == 2010) %>% distinct(TEMP)#Se repiten porque tienen distintos géneros, por eso usamos distinct
  i1.1
  
  

#### Objective reached ~ Thank you ####
