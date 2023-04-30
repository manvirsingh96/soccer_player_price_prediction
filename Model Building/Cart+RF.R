##-------------------------------------- CART KEEPING RANK VARIABLES AS NUMERIC -------------------------------
getwd()
setwd("C:/Users/U370867.l/iCloudDrive/GL//Capstone/Interim Report/Model Building")
fifa_data=read.csv(file.choose()) ## open fifa data cleaned
data.frame(colnames(fifa_data))
#fifa_data=fifa_data[,-19] # Red cards were 0 for all after outlier treatment so the variable has been removed
str(fifa_data)

fifa_data$S.No.=as.character(fifa_data$S.No.)
fifa_data$sofifa_id=as.character(fifa_data$sofifa_id)
fifa_data$short_name=as.character(fifa_data$short_name)
fifa_data$long_name=as.character(fifa_data$long_name)
str(fifa_data)

x = c('Player_Continent','Club_Continent','preferred_foot','Position_Final')

y=c('age','Market_Value','Wages','pace','shooting','passing','dribbling','defending','physic','Mins_Total'
    ,'Shots_per_game_total','Aerial_Battles_Won_Per_Game_Total','Club_Int_Prestige','Club_Domestic_Prestige',
    'Player_international_reputation','weak_foot')

fifa_data[x]=lapply(fifa_data[x],as.factor)
fifa_data[y]=lapply(fifa_data[y],as.numeric)
str(fifa_data)

fifa_data_backup=fifa_data



## splitting data into train and test ##
set.seed(1234)

pd=sample(2,nrow(fifa_data),replace = T , prob=c(.7,.3))

fifa_train_data=fifa_data[pd==1,]
fifa_test_data = fifa_data[pd==2,]   

#write.csv(fifa_train_data,'train_data_original_non_normaized.csv')
#write.csv(fifa_test_data,'test_data_original_outliers_non_normaized.csv')


#### ----Making CART model for mkt_value TAKING wages as iv------ ###

data.frame(colnames(fifa_train_data))
nrow(fifa_train_data)

#5.specify conrol parameters for CART
r_ctrl_mkt_value=rpart.control(minsplit = 500,minbucket = 50,cp=0.0045,xval=5)## cp was initially set to 0 to see
## at what value is the change in error insignificant ( from observing the plot drawmn by plotcp below)
## then from the plot the cp value was change to get the number of optimal nodes beyond which decrease in error is not big
## this method was followed for all cart models 



cart_mkt_val=rpart(data= fifa_train_data,
                   formula = Market_Value ~ Wages +age +pace+shooting + defending + dribbling
                       +passing  +physic +Mins_Total
                   +Goals_total +Assists_Total +Shots_per_game_total +
                       Yellow_Cards_Total 
                       +Red_Cards_Total 
                   +Aerial_Battles_Won_Per_Game_Total 
                   +Man_of_The_Match_Total 
                   +Player_international_reputation 
                   +weak_foot
                   +Club_Int_Prestige +
                         Club_Domestic_Prestige 
                       +Player_Continent +Club_Continent +preferred_foot +Position_Final,
                   method = 'anova',control = r_ctrl_mkt_value)

fancyRpartPlot(model =  cart_mkt_val,tweak=0.8,palettes = 'Purples')
title("Decision Tree for Market Value\n(After outlier treatment for all variables)", cex.main=1.3,adj=0,font.main=4,col.main='Dark Blue')


## printing and plotting complexity parameter and the error
printcp(cart_mkt_val)
plotcp(cart_mkt_val)



#now we need to check the accuracy of the model
fifa_train_data$predicted_mkt_val_1=predict(cart_mkt_val,newdata = fifa_train_data,interval='predict')
fifa_test_data$predicted_mkt_val_1 = predict(cart_mkt_val,newdata = fifa_test_data,interval = 'predict')

## train data accuracy measures
rmse(actual = fifa_train_data$Market_Value,predicted = fifa_train_data$predicted_mkt_val_1)
mape(actual = fifa_train_data$Market_Value,predicted = fifa_train_data$predicted_mkt_val_1)


## test data accuracy measures
rmse(actual = fifa_test_data$Market_Value,predicted = fifa_test_data$predicted_mkt_val_1)
mape(actual = fifa_test_data$Market_Value,predicted = fifa_test_data$predicted_mkt_val_1)


#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

#-------------------------------------CART CONVERTING RANK VARIABLES INTO FACTORS------------------------------

fifa_data=fifa_data_backup
str(fifa_data)

x=c('Player_Continent','Club_Continent','Club_Domestic_Prestige','preferred_foot',
    'Position_Final','Player_international_reputation','weak_foot','Club_Int_Prestige')

y=c('age','Market_Value','Wages','','','pace','shooting','passing','dribbling','defending','physic','Mins_Total'
    ,'Shots_per_game_total','Aerial_Battles_Won_Per_Game_Total')

fifa_data[x]=lapply(fifa_data[x], as.factor)

fifa_data[y]=lapply(fifa_data[y], as.numeric)

str(fifa_data)

data.frame(colnames(fifa_data))



### dividing data into train and test sets
set.seed(1236)
pd=sample(2,nrow(fifa_data),replace = T , prob=c(.7,.3))

fifa_train_data_2=fifa_data[pd==1,]
fifa_test_data_2 = fifa_data[pd==2,]      


#### ------------ model for market value  taking wages as an IV----------------###

data.frame(colnames(fifa_train_data_2))
str(fifa_train_data_2)
r_ctrl_mkt_value_2=rpart.control(minsplit = 500,minbucket = 50,cp=0.0045,xval=5)



cart_mkt_val_2=rpart(data= fifa_train_data_2,formula = Market_Value~
                     age+pace+passing+shooting+dribbling+defending+physic+Mins_Total+Goals_total+Assists_Total+
                     Shots_per_game_total+Yellow_Cards_Total+Red_Cards_Total+Aerial_Battles_Won_Per_Game_Total+
                     Man_of_The_Match_Total++Player_international_reputation+weak_foot+Club_Int_Prestige+
                     Club_Domestic_Prestige+Player_Continent+Club_Continent+preferred_foot+Position_Final,
                   method = 'anova',control = r_ctrl_mkt_value_2)

fancyRpartPlot(model =  cart_mkt_val_2,palettes = 'YlOrRd',tweak=0.8) 
title("Decision Tree for Market Value(Rank Variables as Factors)", cex.main=1.2,adj=0,font.main=4,col.main='maroon')
 

## printing and plotting complexity parameter and the error
printcp(cart_mkt_val_2)
plotcp(cart_mkt_val_2)



#now we need to check the accuracy of the model
fifa_train_data_2$predicted_mkt_val_2=predict(cart_mkt_val_2,newdata = fifa_train_data_2,interval='predict')
fifa_test_data_2$predicted_mkt_val_2 = predict(cart_mkt_val_2,newdata = fifa_test_data_2,interval = 'predict')

## train data accuracy measures
rmse(actual = fifa_train_data_2$Market_Value,predicted = fifa_train_data_2$predicted_mkt_val_2)
mape(actual = fifa_train_data_2$Market_Value,predicted = fifa_train_data_2$predicted_mkt_val_2)


## test data accuracy measures
rmse(actual = fifa_test_data_2$Market_Value,predicted = fifa_test_data_2$predicted_mkt_val_2)
mape(actual = fifa_test_data_2$Market_Value,predicted = fifa_test_data_2$predicted_mkt_val_2)


    ##-------------------------------------- RANDOM FOREST KEEPING RANK VARIABLES AS NUMERIC -------------------------------


fifa_data=read.csv(file.choose())
str(fifa_data)

## splitting data into train and test ##
set.seed(1234)

pd=sample(2,nrow(fifa_data),replace = T , prob=c(.7,.3))

fifa_train_data=fifa_data[pd==1,]
fifa_test_data = fifa_data[pd==2,]   


#### ----Making RF model for mkt_value TAKING wages as iv------ ###

data.frame(colnames(fifa_train_data))

colnames(fifa_train_data[c(1:5)])
colnames(fifa_train_data[-c(1:5)])
length(colnames(fifa_train_data[-c(1:5)]))
## 24 vars in the model after removing uneccessary vars (including red cards)

str(fifa_train_data)

mtry_mkt_value_1=tuneRF(fifa_train_data[-c(1:5)],fifa_train_data$Market_Value,ntreeTry = 500,stepFactor = 1.5,
                        improve = 0.01,trace = T,plot = T)



rf_mkt_value_1 = randomForest(Market_Value ~ Wages +age +pace+shooting + defending + dribbling
                              +passing  +physic +Mins_Total
                              +Goals_total +Assists_Total +Shots_per_game_total +
                                  Yellow_Cards_Total 
                              #+Red_Cards_Total 
                              +Aerial_Battles_Won_Per_Game_Total 
                              +Man_of_The_Match_Total 
                              +Player_international_reputation 
                              +weak_foot
                              +Club_Int_Prestige +
                                  Club_Domestic_Prestige 
                              +Player_Continent +Club_Continent +preferred_foot +Position_Final,
                              data = fifa_train_data,mtry=10,importance=T,ntree=500)

rf_mkt_value_1
rf_mkt_value_1$importance

varImpPlot(rf_mkt_value_1,main = 'Variable importance for Market Value',sort = T)

## train data accuracy measures
rmse(actual = fifa_train_data$Market_Value,predicted = rf_mkt_value_1$predicted)
mape(actual = fifa_train_data$Market_Value,predicted = rf_mkt_value_1$predicted)


## test data accuracy measures
fifa_test_data$predicted_mkt_value_1=predict(rf_mkt_value_1,newdata = fifa_test_data,interval='predict')
rmse(actual = fifa_test_data$Market_Value,predicted = fifa_test_data$predicted_mkt_value_1)
mape(actual = fifa_test_data$Market_Value,predicted = fifa_test_data$predicted_mkt_value_1)


#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################


#-------------------------------------RF CONVERTING RANK VARIABLES INTO FACTORS------------------------------

fifa_data=fifa_data_backup
str(fifa_data)

x=c('Player_Continent','Club_Continent','Club_Domestic_Prestige','preferred_foot',
    'Position_Final','Player_international_reputation','weak_foot','Club_Int_Prestige')

y=c('age','Market_Value','Wages','','','pace','shooting','passing','dribbling','defending','physic','Mins_Total'
    ,'Shots_per_game_total','Aerial_Battles_Won_Per_Game_Total')

fifa_data[x]=lapply(fifa_data[x], as.factor)

fifa_data[y]=lapply(fifa_data[y], as.numeric)

str(fifa_data)


### dividing data into train and test sets
set.seed(1234)
pd=sample(2,nrow(fifa_data),replace = T , prob=c(.7,.3))

fifa_train_data_2=fifa_data[pd==1,]
fifa_test_data_2 = fifa_data[pd==2,]      

str(fifa_train_data_2)

#### ------------ model for market value  taking wages as an IV----------------###

data.frame(colnames(fifa_train_data_2))

mtry_mkt_value_2=tuneRF(fifa_train_data_2[-c(1:6,22)],fifa_train_data_2$Market_Value,ntreeTry = 500,stepFactor = 1.5,
                        improve = 0.01,trace = T,plot = T)


rf_mkt_value_2 = randomForest(Market_Value~
                                age+pace+passing+shooting+dribbling+defending+physic+Mins_Total+Goals_total+Assists_Total+
                                Shots_per_game_total+Yellow_Cards_Total+Red_Cards_Total+Aerial_Battles_Won_Per_Game_Total+
                                Man_of_The_Match_Total++Player_international_reputation+weak_foot+Club_Int_Prestige+
                                Club_Domestic_Prestige+Player_Continent+Club_Continent+preferred_foot+Position_Final,
                              data = fifa_train_data_2,mtry=18,importance=T,ntree=500)

rf_mkt_value_2
rf_mkt_value_2$importance

varImpPlot(rf_mkt_value_2,sort=T , main ='Variable importance for Market Value with Rank Variables as Factors')

## train data accuracy measures
rmse(actual = fifa_train_data_2$Market_Value,predicted = rf_mkt_value_2$predicted)
mape(actual = fifa_train_data_2$Market_Value,predicted = rf_mkt_value_2$predicted)


## test data accuracy measures
fifa_test_data_2$predicted_mkt_value_2=predict(rf_mkt_value_2,newdata = fifa_test_data_2,interval='predict')
rmse(actual = fifa_test_data_2$Market_Value,predicted = fifa_test_data_2$predicted_mkt_value_2 )
mape(actual = fifa_test_data_2$Market_Value,predicted = fifa_test_data_2$predicted_mkt_value_2)


#### ----Making RF model for wages TAKING mkt_value as iv------ ###

data.frame(colnames(fifa_train_data))


colnames(fifa_train_data[c(1:6,23)])
colnames(fifa_train_data[-c(1:6,23)])
length(colnames(fifa_train_data[-c(1:6,23)]))
## 24 vars to be used in the model

mtry_wages_2=tuneRF(fifa_train_data_2[-c(1:6,23)],fifa_train_data_2$Wages,ntreeTry = 500,stepFactor = 1.5,
                    improve = 0.01,trace = T,plot = T)


rf_wages_2 = randomForest(Wages~
                            age+pace+passing+shooting+dribbling+defending+physic+Mins_Total+Goals_total+Assists_Total+
                            Shots_per_game_total+Yellow_Cards_Total+Red_Cards_Total+Aerial_Battles_Won_Per_Game_Total+
                            Man_of_The_Match_Total++Player_international_reputation+weak_foot+Club_Int_Prestige+
                            Club_Domestic_Prestige+Player_Continent+Club_Continent+preferred_foot+Position_Final,
                          data = fifa_train_data_2,mtry=18,importance=T,ntree=500)

rf_wages_2
rf_wages_2$importance

varImpPlot(rf_wages_2,sort = T,main = 'Variable importance for Wages with Rank Variables as Factors')

## train data accuracy measures
rmse(actual = fifa_train_data_2$Wages,predicted = rf_wages_2$predicted)
mape(actual = fifa_train_data_2$Wages,predicted = rf_wages_2$predicted)


## test data accuracy measures
fifa_test_data_2$predicted_wages_2=predict(rf_wages_2,newdata = fifa_test_data_2,interval='predict')
rmse(actual = fifa_test_data_2$Wages,predicted = fifa_test_data_2$predicted_wages_2)
mape(actual = fifa_test_data_2$Wages,predicted = fifa_test_data_2$predicted_wages_2)

rsq.rpart(cart_mkt_val)
