fifa_data=read.csv(file.choose())
str(fifa_data)
fifa_data
fifa_data=data.frame(fifa_data)
data.frame(colnames(fifa_data))
str(fifa_data_new)
fifa_data=fifa_data[,-c(1:4)]


colnames(fifa_data)
numeric_vars=c('age','Value','Market_Value','Wages','pace','shooting','passing','dribbling','defending','physic','Mins_Total',
                  'Shots_per_game_total','Aerial_Battles_Won_Per_Game_Total','Goals_total','Assists_Total','
               Yellow_Cards_Total','Red_Cards_Total','Man_of_The_Match_Total','weak_foot',"Club_Int_Prestige",
               'Club_Domestic_Prestige','Player_international_reputation')

fifa_data_numeric = select_if(fifa_data,(colnames(fifa_data)%in%c(numeric_vars)))

colnames(fifa_data_numeric)



cor_numeric=cor(fifa_data_numeric,method = 'kendall')

cor_numeric = data.frame(cor_numeric)

write.csv(cor_numeric,'1.cor_all_numeric_pearson.csv')

corrplot(as.matrix(cor_numeric),method="color",addCoef.col = "blue",type = 'lower',
         title = "Correlation between all numeric variables (Kendall Method)",
         tl.cex = 1,tl.srt = 20,mar=c(0,0,2,0),tl.col = 'black',number.cex = .8,diag = F,tl.offset = .45,
         col=colorRampPalette(c("darkgreen ","white","darkred"))(100))

  
