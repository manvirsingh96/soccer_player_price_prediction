##-------------------------------------- MLR KEEPING RANK VARIABLES AS NUMERIC -------------------------------
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

fifa_data_encoded=dummy_cols(fifa_data,select_columns = x, ## by default it converts all character and factor cols into dummies
                             remove_first_dummy = T, ## to keep n-1 vars
                             remove_selected_columns = T) ## keep only dummy var cols and remove original cols
str(fifa_data_encoded)
glimpse(fifa_data_encoded)

 ### -------normalizing variables using z score------------ ###

fifa_data_normalized = fifa_data_encoded
data.frame(colnames(fifa_data_normalized))
normalize_len = 6:21
normalize_len

for (i in normalize_len) {fifa_data_normalized[,i]=
  (fifa_data_normalized[,i]-mean(fifa_data_normalized[,i]))/(sd(fifa_data_normalized[,i]))
}

summary(fifa_data_normalized)

set.seed(1234)

pd=sample(2,nrow(fifa_data_normalized),replace = T , prob=c(.7,.3))

names(fifa_data_normalized) = make.names(names(fifa_data_normalized),unique = T)
names(fifa_data_normalized)


fifa_train_data=fifa_data_normalized[pd==1,]
fifa_test_data = fifa_data_normalized[pd==2,]   

str(fifa_train_data)
data.frame(colnames(fifa_train_data))


## splitting data into train and test ##
set.seed(1234)

pd=sample(2,nrow(fifa_data_encoded),replace = T , prob=c(.7,.3))

names(fifa_data_encoded) = make.names(names(fifa_data_encoded),unique = T)
names(fifa_data_encoded)

fifa_train_data=fifa_data_encoded[pd==1,]
fifa_test_data = fifa_data_encoded[pd==2,]   


#### ----Making model for mkt_value TAKING wages as iv------ ###

data.frame(colnames(fifa_train_data))
str(fifa_train_data)
           


## model was first run with all variables but then vars were dropped based on VIF and significance
## the following variables were dropped one by one due to high VIFs in the above model 
##1st `Club_Continent_Europe Tier 1` 2nd - Position_Final_Defense + 3rd dribbling

##model was made again after removing multicollinearity and then insignificant vars were removed one by one based on business knowledge and p value
#Yellow_Cards_Total , weak foot , club dom prestige , aerial battles won per game ,player continent all except asia
## , club continent all except europe tier 2,3 , preferred foot right , Position_Final_Attacking Midfield,Forward , 
##, Position_Final_Forward 

lm_mktvalue=lm(data=fifa_train_data,
               Market_Value~Wages +age +#pace#+shooting + defending + dribbling
             +passing  +physic +Mins_Total
               +Goals_total +Assists_Total +Shots_per_game_total +
               #Yellow_Cards_Total 
              +Red_Cards_Total 
               #+Aerial_Battles_Won_Per_Game_Total 
            +Man_of_The_Match_Total 
             +Player_international_reputation 
             #+weak_foot
             +Club_Int_Prestige +
             #  Club_Domestic_Prestige 
             +Player_Continent_Asia +Player_Continent_Europe 
               +Player_Continent_North.America +Player_Continent_Oceania +Player_Continent_South.America 
               +Club_Continent_Europe.Tier.1 +Club_Continent_Europe.Tier.2 +Club_Continent_Europe.Tier.3 
               +Club_Continent_Europe.Tier.4 +Club_Continent_North.America +Club_Continent_South.America
               +preferred_foot_Right +Position_Final_Attacking.Midfield.Forward +Position_Final_Defense 
               +Position_Final_Defensive.Midfield +Position_Final_Defensive.Midfield.Defense +Position_Final_Forward
               +Position_Final_Midfield..Attacking.Defensive.)

data.frame(vif(lm_mktvalue))
summary(lm_mktvalue)
## even though shooting and assists_total are insignificant , they have not been removed because they are important vars

fifa_train_data$Predicted_Mkt_Val = predict(lm_mktvalue,newdata = fifa_train_data,interval = 'predict')

fifa_test_data$Predicted_Mkt_Val = predict(lm_mktvalue,newdata = fifa_test_data,interval = 'predict')
View(fifa_test_data)



rmse(actual = fifa_train_data$Market_Value,predicted = fifa_train_data$Predicted_Mkt_Val)
mape(actual = fifa_train_data$Market_Value,predicted = fifa_train_data$Predicted_Mkt_Val)


rmse(actual = fifa_test_data$Market_Value,predicted = fifa_test_data$Predicted_Mkt_Val)
mape(actual = fifa_test_data$Market_Value,predicted = fifa_test_data$Predicted_Mkt_Val)


## testing assumptions ##

plot(lm_mktvalue$fitted.values,lm_mktvalue$residuals,pch=19 , col='blue',
     xlab = 'Fitted Values',ylab = 'Residuals',main='Residuals vs Fitted for Market Value')##  assumption  of homoscedacity is violated
shapiro.test(lm_mktvalue$residuals) ## errors are not normally distributed
dwtest(lm_mktvalue) ## there is autocorrelation


#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

#-------------------------------------MLR CONVERTING RANK VARIABLES INTO FACTORS------------------------------

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


## the categorical variables need to be converted into dummy variables                


library('fastDummies')
fifa_data_encoded_2 = dummy_cols(fifa_data, select_columns = x, ## by default it converts all character and factor cols into dummies
                               remove_selected_columns =T ## this will remove original cols and keep only dummy cols
                               ,remove_first_dummy = T)

fifa_data_encoded_2
str(fifa_data_encoded_2)
data.frame(colnames(fifa_data_encoded_2))


'''fifa_data_normalized = fifa_data_encoded

data.frame(colnames(fifa_data_normalized))
normalize_len = 7:22
normalize_len

for (i in normalize_len) {fifa_data_normalized[,i]=
  (fifa_data_normalized[,i]-mean(fifa_data_normalized[,i]))/(sd(fifa_data_normalized[,i]))
}

summary(fifa_data_normalized)
'''

### dividing data into train and test sets
set.seed(1234)
pd=sample(2,nrow(fifa_data_encoded_2),replace = T , prob=c(.7,.3))

fifa_train_data_2=fifa_data_encoded_2[pd==1,]
fifa_test_data_2 = fifa_data_encoded_2[pd==2,]      


#########################################################################################################
#########################################################################################################
#########################################################################################################


##SINCE ASSUMPTIONS ARE NOT BEING SATISFIED , WE CAN MAKE THE MODEL WITHOUT HAVING TO DEAL WITH MULTICOLLINEARITY AND KEEPING ALL VARIABLES IN THE MODEL

### making model again with all vars with rank vars as continuous ###


#### ----Making model for mkt_value TAKING wages as iv------ ###

data.frame(colnames(fifa_train_data))

lm_mktvalue_3=lm(data=fifa_train_data,
                Market_Value~ 
                age+	pace+	shooting+	passing+	dribbling+	defending+	physic+	Mins_Total+	Goals_total+	Assists_Total+
                  Shots_per_game_total+Yellow_Cards_Total+	Red_Cards_Total+	Aerial_Battles_Won_Per_Game_Total+
                  Man_of_The_Match_Total+	+Player_international_reputation+	weak_foot+	Club_Int_Prestige+	
                  Club_Domestic_Prestige+	Player_Continent_Asia+Player_Continent_Europe+	`Player_Continent_North America`+	
                  Player_Continent_Oceania+`Player_Continent_South America`+`Club_Continent_Europe Tier 1`+	
                  `Club_Continent_Europe Tier 2`+`Club_Continent_Europe Tier 3`+	`Club_Continent_Europe Tier 4`+
                  `Club_Continent_North America`+`Club_Continent_South America`+	preferred_foot_Right+
                  `Position_Final_Attacking Midfield,Forward`+Position_Final_Defense+	`Position_Final_Defensive Midfield`+
                  `Position_Final_Defensive Midfield,Defense`+Position_Final_Forward+`Position_Final_Midfield (Attacking/Defensive)`
 )
 
 
 summary(lm_mktvalue_3)
 
 fifa_test_data$Predicted_Mkt_Val_2 = predict(lm_mktvalue_3,newdata = fifa_test_data,interval = 'predict')
 View(fifa_test_data)
 
 
 rmse(actual = fifa_test_data$Market_Value,predicted = fifa_test_data$Predicted_Mkt_Val_2)
 mape(actual = fifa_test_data$Market_Value,predicted = fifa_test_data$Predicted_Mkt_Val_2)
 
 
 #### ------------ model for wages  TAKING  mkt value as an IV----------------###
 
 

 lm_wages_3=lm(data=fifa_train_data,Wages~
               age+	pace+	shooting+	passing+	dribbling+	defending+	physic+	Mins_Total+	Goals_total+	Assists_Total+
               Shots_per_game_total+Yellow_Cards_Total+	Red_Cards_Total+	Aerial_Battles_Won_Per_Game_Total+
               Man_of_The_Match_Total+	+Player_international_reputation+	weak_foot+	Club_Int_Prestige+	
               Club_Domestic_Prestige+	Player_Continent_Asia+Player_Continent_Europe+	`Player_Continent_North America`+	
               Player_Continent_Oceania+`Player_Continent_South America`+`Club_Continent_Europe Tier 1`+	
               `Club_Continent_Europe Tier 2`+`Club_Continent_Europe Tier 3`+	`Club_Continent_Europe Tier 4`+
               `Club_Continent_North America`+`Club_Continent_South America`+	preferred_foot_Right+
               `Position_Final_Attacking Midfield,Forward`+Position_Final_Defense+	`Position_Final_Defensive Midfield`+
               `Position_Final_Defensive Midfield,Defense`+Position_Final_Forward+`Position_Final_Midfield (Attacking/Defensive)`
 
 )
 summary(lm_wages_3)
 
 fifa_test_data$Predicted_wages_2= predict(lm_wages_3,newdata = fifa_test_data,interval = 'predict')
 
 rmse(actual = fifa_test_data$Wages,predicted = fifa_test_data$Predicted_wages_2)
 mape(actual = fifa_test_data$Wages,predicted = fifa_test_data$Predicted_wages_2)
 
 
 #########################################################################################################
 #########################################################################################################
 #########################################################################################################
 #########################################################################################################
 
 ### making model again with all vars with rank vars as factors ###
 
 
 #### ----Making model for mkt_value TAKING wages as iv------ ###
 
 data.frame(colnames(fifa_train_data))
 
 lm_mktvalue_4=lm(data=fifa_train_data_2,
                  Market_Value~ 
                  age+	pace+	shooting+	passing+	dribbling+	defending+	physic+	Mins_Total+	Goals_total+	Assists_Total+	
                    Shots_per_game_total+	Yellow_Cards_Total+	Red_Cards_Total+	Aerial_Battles_Won_Per_Game_Total+	
                    Man_of_The_Match_Total+	+	Player_Continent_Asia+	Player_Continent_Europe+	
                    `Player_Continent_North America`+	Player_Continent_Oceania+	`Player_Continent_South America`+	
                    `Club_Continent_Europe Tier 1`+	`Club_Continent_Europe Tier 2`+	`Club_Continent_Europe Tier 3`+	
                    `Club_Continent_Europe Tier 4`+	`Club_Continent_North America`+	`Club_Continent_South America`+	
                    Club_Domestic_Prestige_2+	Club_Domestic_Prestige_3+	Club_Domestic_Prestige_4+	Club_Domestic_Prestige_5+	
                    Club_Domestic_Prestige_6+	Club_Domestic_Prestige_7+	Club_Domestic_Prestige_8+	Club_Domestic_Prestige_9+
                    Club_Domestic_Prestige_10+	preferred_foot_Right+	`Position_Final_Attacking Midfield,Forward`+
                    Position_Final_Defense+	`Position_Final_Defensive Midfield`+	`Position_Final_Defensive Midfield,Defense`+	
                    Position_Final_Forward+	`Position_Final_Midfield (Attacking/Defensive)`+	Player_international_reputation_2+
                    Player_international_reputation_3+	Player_international_reputation_4+	Player_international_reputation_5+
                    weak_foot_2+	weak_foot_3+	weak_foot_4+	weak_foot_5+	Club_Int_Prestige_2+	Club_Int_Prestige_3+
                    Club_Int_Prestige_4+	Club_Int_Prestige_5+	Club_Int_Prestige_6+	Club_Int_Prestige_7+	Club_Int_Prestige_8+
                    Club_Int_Prestige_9+	Club_Int_Prestige_10
                  
 )
 
 
 summary(lm_mktvalue_4)
 
 fifa_test_data_2$Predicted_Mkt_Val_2 = predict(lm_mktvalue_4,newdata = fifa_test_data_2,interval = 'predict')
 View(fifa_test_data_2)
 
 
 rmse(actual = fifa_test_data_2$Market_Value,predicted = fifa_test_data_2$Predicted_Mkt_Val_2)
 mape(actual = fifa_test_data_2$Market_Value,predicted = fifa_test_data_2$Predicted_Mkt_Val_2)
 
 
 #### ------------ model for wages  TAKING  mkt value as an IV----------------###
 
 
 
 lm_wages_4=lm(data=fifa_train_data_2,
                  Wages~ 
                    age+	pace+	shooting+	passing+	dribbling+	defending+	physic+	Mins_Total+	Goals_total+	Assists_Total+	
                    Shots_per_game_total+	Yellow_Cards_Total+	Red_Cards_Total+	Aerial_Battles_Won_Per_Game_Total+	
                    Man_of_The_Match_Total+	+	Player_Continent_Asia+	Player_Continent_Europe+	
                    `Player_Continent_North America`+	Player_Continent_Oceania+	`Player_Continent_South America`+	
                    `Club_Continent_Europe Tier 1`+	`Club_Continent_Europe Tier 2`+	`Club_Continent_Europe Tier 3`+	
                    `Club_Continent_Europe Tier 4`+	`Club_Continent_North America`+	`Club_Continent_South America`+	
                    Club_Domestic_Prestige_2+	Club_Domestic_Prestige_3+	Club_Domestic_Prestige_4+	Club_Domestic_Prestige_5+	
                    Club_Domestic_Prestige_6+	Club_Domestic_Prestige_7+	Club_Domestic_Prestige_8+	Club_Domestic_Prestige_9+
                    Club_Domestic_Prestige_10+	preferred_foot_Right+	`Position_Final_Attacking Midfield,Forward`+
                    Position_Final_Defense+	`Position_Final_Defensive Midfield`+	`Position_Final_Defensive Midfield,Defense`+	
                    Position_Final_Forward+	`Position_Final_Midfield (Attacking/Defensive)`+	Player_international_reputation_2+
                    Player_international_reputation_3+	Player_international_reputation_4+	Player_international_reputation_5+
                    weak_foot_2+	weak_foot_3+	weak_foot_4+	weak_foot_5+	Club_Int_Prestige_2+	Club_Int_Prestige_3+
                    Club_Int_Prestige_4+	Club_Int_Prestige_5+	Club_Int_Prestige_6+	Club_Int_Prestige_7+	Club_Int_Prestige_8+
                    Club_Int_Prestige_9+	Club_Int_Prestige_10
)
 
 
 summary(lm_wages_4)
 
 fifa_test_data_2$Predicted_Wages_2 = predict(lm_wages_4,newdata = fifa_test_data_2,interval = 'predict')
 View(fifa_test_data_2)
 
 
 rmse(actual = fifa_test_data_2$Wages,predicted = fifa_test_data_2$Predicted_Wages_2)
 mape(actual = fifa_test_data_2$Wages,predicted = fifa_test_data_2$Predicted_Wages_2)
 
 
 
 #########################################################################################################
 #########################################################################################################
 #########################################################################################################
 #########################################################################################################