# soccer_player_price_prediction
Repository for a non-github project to predict market value of soccer players.


## Project Overview

The trade of football players between clubs involve huge sums of money. Therefore, the problem at hand is a misevaluation of a player’s worth, that can result in huge financial losses for the clubs. The main objective of the project is therefore to use a suitable machine learning algorithm and come up with a model that can predict the market value of football players with maximum accuracy. Apart from predicting the market value, the project will also attempt to analyse and find out which features significantly determine the value of a footballer. 

The project involves usage of various game related as well as real life statistics of  4705 players across the top 10 soccer leagues across the world. The attributes cover the attacking and defending skillsets of a player as per the game EA FIFA 20 along with certain real-life performance metrics in the 2018-2019 season. (Please note that the stats provided to the player in the FIFA 20 game are based on the performance in the 2018-2019 season). Further the project focuses only on outfield players, meaning players playing in all positions except goalkeepers. This is because goalkeepers have a completely different set of attributes and cannot be directly compared with outfield players). 

## Data Source and Description

 - Part of the data has been sourced from the Kaggle (https://www.kaggle.com/stefanoleone992/fifa-20-complete-player-dataset#players_20.csv). The data on Kaggle was in turn obtained from the website www.sofifa.com.  This part of the data contains each player’s gaming attributes as in the game of EA FIFA 20. 
- Apart from the variables from sofifa already available in the Kaggle data set , some additional variables were sourced from www.sofifa.com  as well as www.whoscored.com. The data was scraped using a python script with the help of Selenium for python. 

- The data obtained from the two sources was then combined using an Alteryx workflow to get the final dataset which contains the details 4705 unique football players. The below table captures the different variables in the dataset and a description of each variable. The variable “Value” (initially read as “value_eur”) is the response / dependent variable.


Variable Name|	Variable Description|Data Type|
-------------|---------------------|----------|
|S.No.	|Serial number of the observation|Character|
sofifa_id	|Unique identifier of the player on www.sofifa.com |Character|
short_name|	Player’s truncated name|Character|
long_name	|Player’s full name|Character|
age	|Age of the player|Numeric|
nationality|	Nationality of the player |Nominal Categorical|
Player_Continent*|	The geographical continent to which the player belongs|Nominal Categorical|
Club_Continent**|	The geographical continent to which the player’s club belongs|Nominal Categorical|
Value/Market_Value|	Market value of the player (in million Euros) as of 1st Oct 2019|Numeric|
Wages|	Per week wage of the player (in thousand Euros) for the season 2018-19 (ending in May’19)|Numeric|
Position_Final|	The position in which the player plays|Nominal Categorical|
preferred_foot|	Preferred foot of the player for kicking the ball|Nominal Categorical|
weak_foot|	Weak foot rating on a scale of 1 to 5 as per the FIFA 20 Game |Oridnal Numeric|
pace|	The pace of the player on a scale of 100 as per the FIFA 20 game|Numeric|
shooting|	The shooting accuracy of the player on a scale of 100 as per the FIFA 20 game |Numeric|
passing|	The passing accuracy of the player on a scale of 100 as per the FIFA 20 game |Numeric|
dribbling|	The dribbling skills of the player on a scale of 100 as per the FIFA 20 game |Numeric|
defending|	The defending skills of the player on a scale of 100 as per the FIFA 20 game|Numeric|
physic|	The physical strength of the player on a scale of 100 as per the FIFA 20 game |Numeric|
Club_Int_Prestige	|The player's club's prestige rated from 1 to 10 amongst all the international clubs in world football, with 10 being the best rating possible|Oridnal Numeric|
Club_Domestic_Prestige|	The player's clubs prestige rated from 1 to 10 amongst all the clubs within the league to which the club belongs, with 10 being the best rating possible |Oridnal Numeric|
Player_international_reputation	| Each player’s reputation on an international scale from 1 to 5 with 5 being the highest rating |Oridnal Numeric|
Mins_Total|	Total minutes of football played during the 2018-19 season |Numeric|
Goals_Total	|No. of goals scored by the player during the 2018-19 season|Numeric|
Assists_Total|	No. of assists provided by the player during the 2018-19 season. As assist is a pass that directly results in a goal|Numeric|
Shots_per_game_total|	No. of shots attempted by a player per game during the 2018-19 season|Numeric|
Yellow_Cards_Total|	No. of yellow cards obtained by the player for committing fouls during the 2018-19 season. A yellow card in a game represents a warning for foul play.|Numeric|
Aerial_Battles_Won_Per_Game_Total|	No. of headers won by a player per game throughout the 2019-19 season |Numeric|
Red_Cards_Total	| No. of red cards obtained by the player for committing fouls during the 2018-19 season. A red card may be obtained directly or by obtaining 2 yellow cards in the same game. While both kind of red cards involve the player being sent off in the current game , the former involves a suspension for 3 subsequent games whereas the latter involves a suspension for 1 subsequent game|Numeric|
Man_of_The_Match_Total	| No. of man of the match accolades obtained by the player during the 2018-19 season |Numeric|

\* Player_Contintent was obtained based on player nationalities so as to reduce the number of factor levels. 

\** Club_Contintent was obtained based on the country to which the clubs belong so as to reduce the number of factor levels



