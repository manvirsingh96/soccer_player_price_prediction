##-------------------------------------- NN KEEPING RANK VARIABLES AS NUMERIC -------------------------------
getwd()
setwd("/Users/manvir/Library/Mobile Documents/com~apple~CloudDocs/GL/Capstone/Interim Report/Model Building")
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

#### ---------ENCODING FACTOR VARIABLES-----------###

library(fastDummies)
fifa_data_encoded=dummy_cols(fifa_data,select_columns = x, ## by default it converts all character and factor cols into dummies
                             remove_first_dummy = T, ## to keep n-1 vars
                             remove_selected_columns = T) ## keep only dummy var cols and remove original cols
str(fifa_data_encoded)
glimpse(fifa_data_encoded)

### -------normalizing variables using min-max scaling------------ ###

fifa_data_normalized = fifa_data_encoded
names(fifa_data_normalized) = make.names(names(fifa_data_normalized),unique = T)
data.frame(colnames(fifa_data_normalized))
normalize_len = 6:20
normalize_len

for (i in normalize_len) {fifa_data_normalized[,i]=
  (fifa_data_normalized[,i]-min(fifa_data_normalized[,i]))/(max(fifa_data_normalized[,i])-min(fifa_data_normalized[,i]))
}

summary(fifa_data_normalized)

## splitting data into train and test ##

set.seed(1234)

pd=sample(2,nrow(fifa_data_normalized),replace = T , prob=c(.7,.3))

fifa_train_data=fifa_data_normalized[pd==1,]
fifa_test_data = fifa_data_normalized[pd==2,]   

str(fifa_train_data)

data.frame(colnames(fifa_train_data))
colnames(fifa_train_data[c(1:4)])

#### ----Making model for mkt_value TAKING wages as iv------ ###




nn_mkt_value_1=neuralnet(formula = Market_Value~Wages +age +pace+shooting + defending + dribbling
                           +passing  +physic +Mins_Total
                         +Goals_total +Assists_Total +Shots_per_game_total +
                           Yellow_Cards_Total 
                          # +Red_Cards_Total 
                         +Aerial_Battles_Won_Per_Game_Total 
                         +Man_of_The_Match_Total 
                         +Player_international_reputation 
                         +weak_foot
                         +Club_Int_Prestige +
                             Club_Domestic_Prestige 
                           +Player_Continent_Asia +Player_Continent_Europe 
                         +Player_Continent_North.America +Player_Continent_Oceania +Player_Continent_South.America 
                         +Club_Continent_Europe.Tier.1 +Club_Continent_Europe.Tier.2 +Club_Continent_Europe.Tier.3 
                         +Club_Continent_Europe.Tier.4 +Club_Continent_North.America +Club_Continent_South.America
                         +preferred_foot_Right +Position_Final_Attacking.Midfield.Forward +Position_Final_Defense 
                         +Position_Final_Defensive.Midfield +Position_Final_Defensive.Midfield.Defense +Position_Final_Forward
                         +Position_Final_Midfield..Attacking.Defensive.
                           ,
                         data = fifa_train_data,
                         err.fct = "sse", lifesign = 'full',
                         hidden=1,lifesign.step = 1000 , threshold =0.001,linear.output = T,stepmax = 10000000000)

write.csv(fif)

plot(nn_mkt_value_1)
### makin predictions 

fifa_train_data$Predicted_Mkt_Val_1 = predict(nn_mkt_value_1,newdata = fifa_train_data,interval = 'predict')

fifa_test_data$Predicted_Mkt_Val_1 = predict(nn_mkt_value_1,newdata = fifa_test_data,interval = 'predict')



data.frame(colnames(fifa_train_data))

## train accuracy measures
rmse(actual = fifa_train_data$Market_Value,predicted = fifa_train_data$Predicted_Mkt_Val_1)
mape(actual = fifa_train_data$Market_Value,predicted = fifa_train_data$Predicted_Mkt_Val_1)


## test accuracy measures
rmse(actual = fifa_test_data$Market_Value,predicted = fifa_test_data$Predicted_Mkt_Val_1)
mape(actual = fifa_test_data$Market_Value,predicted = fifa_test_data$Predicted_Mkt_Val_1)

nn_mkt_value_1$result.matrix
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

#-------------------------------------NN CONVERTING RANK VARIABLES INTO FACTORS------------------------------

fifa_data=fifa_data_backup
str(fifa_data)

x=c('Player_Continent','Club_Continent','Club_Domestic_Prestige','preferred_foot',
    'Position_Final','Player_international_reputation','weak_foot','Club_Int_Prestige')

y=c('age','Market_Value','Wages','pace','shooting','passing','dribbling','defending','physic','Mins_Total'
    ,'Shots_per_game_total','Aerial_Battles_Won_Per_Game_Total')

fifa_data[x]=lapply(fifa_data[x], as.factor)

fifa_data[y]=lapply(fifa_data[y], as.numeric)

str(fifa_data)

data.frame(colnames(fifa_data))


## the categorical variables need to be converted into dummy variables                


library('fastDummies')
fifa_data_encoded_2 = dummy_cols(fifa_data, select_columns = x, ## by default it converts all character and factor cols into dummies
                                 remove_selected_columns =T ## this will remove original cols and keep only dummy cols
                                 ,remove_first_dummy = T)

fifa_data_encoded_2
str(fifa_data_encoded_2)
data.frame(colnames(fifa_data_encoded_2))

colnames(fifa_data_encoded_2)=make.names(colnames(fifa_data_encoded_2),unique = T)
str(fifa_data_encoded_2)

fifa_data_normalized_2=fifa_data_encoded_2

normalize_len = 7:23
normalize_len

for (i in normalize_len) {fifa_data_normalized_2[,i]=
  (fifa_data_normalized_2[,i]-min(fifa_data_normalized_2[,i]))/(max(fifa_data_normalized_2[,i])-min(fifa_data_normalized_2[,i]))
}

summary(fifa_data_normalized_2)


### dividing data into train and test sets
set.seed(1234)
pd=sample(2,nrow(fifa_data_normalized_2),replace = T , prob=c(.7,.3))

fifa_train_data_2=fifa_data_normalized_2[pd==1,]
fifa_test_data_2 = fifa_data_normalized_2[pd==2,]

data.frame(colnames(fifa_train_data_2))
colnames(fifa_train_data_2[c(1:4,6,22)])
fifa_train_data_subset_mkt_value = fifa_train_data_2[-c(1:4,6,22)]
fifa_train_data_subset_wages = fifa_train_data_2[-c(1:4,5,23)]



#### ------------ model for market value  taking wages as an IV----------------###

#### ----Making model for mkt_value TAKING wages as iv------ ###



nn_mkt_value_2=neuralnet(formula = Market_Value~
                           ,data = fif,
                         err.fct = "sse", lifesign = 'full',stepmax = 10000000,
                         hidden=1,lifesign.step = 1000 , threshold =0.01,linear.output = T)

plot(nn_mkt_value_2)
### makin predictions 

fifa_train_data_2$Predicted_Mkt_Val_2 = predict(nn_mkt_value_2,newdata = fifa_train_data_2,interval = 'predict')

fifa_test_data_2$Predicted_Mkt_Val_2 = predict(nn_mkt_value_2,newdata = fifa_test_data_2,interval = 'predict')



data.frame(colnames(fifa_train_data))

## train accuracy measures
rmse(actual = fifa_train_data_2$Market_Value,predicted = fifa_train_data_2$Predicted_Mkt_Val_2)
mape(actual = fifa_train_data_2$Market_Value,predicted = fifa_train_data_2$Predicted_Mkt_Val_2)


## test accuracy measures
rmse(actual = fifa_test_data_2$Market_Value,predicted = fifa_test_data_2$Predicted_Mkt_Val_2)
mape(actual = fifa_test_data_2$Market_Value,predicted = fifa_test_data_2$Predicted_Mkt_Val_2)

nn_mkt_value_2$result.matrix
#### ----Making model for wages TAKING mkt value as iv------ ###


str(fifa_train_data_subset_wages)

nn_wages_2=neuralnet(formula = Wages~.  ,data = fifa_train_data_subset_wages,
                     err.fct = "sse", lifesign = 'full',stepmax = 100000000000000,
                     hidden=1,lifesign.step = 1000 , threshold =.01,linear.output = T)

plot(nn_wages_2)
### makin predictions 

fifa_train_data_2$Predicted_Wages_2 = predict(nn_wages_2,newdata = fifa_train_data_2,interval = 'predict')

fifa_test_data_2$Predicted_Wage_2 = predict(nn_wages_2,newdata = fifa_test_data_2,interval = 'predict')


data.frame(colnames(fifa_train_data))

## train accuracy measures
rmse(actual = fifa_train_data_2$Wages,predicted = fifa_train_data_2$Predicted_Wages_2)
mape(actual = fifa_train_data_2$Wages,predicted = fifa_train_data_2$Predicted_Wages_2)


## test accuracy measures
rmse(actual = fifa_test_data_2$Wages,predicted = fifa_test_data_2$Predicted_Wage_2)
mape(actual = fifa_test_data_2$Wages,predicted = fifa_test_data_2$Predicted_Wage_2)

nn_wages_2$result.matrix

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################