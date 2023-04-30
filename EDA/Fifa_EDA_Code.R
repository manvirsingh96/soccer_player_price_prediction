####### IMPORTING and PPREPARING DATA####
fifa_data = read_excel('fifa_data_for_eda.xlsx')
fifa_data= read.csv('fifa_data_for_eda.csv')
str(fifa_data)
fifa_data=as.data.frame(fifa_data)

getwd()

'''class(fifa_data)
fifa_data=data.frame(fifa_data)
head(fifa_data)

data.frame(colnames(fifa_data))

attach(fifa_data)
fifa_data=fifa_data[,-c(8:13,31,33)]


data.frame(colnames(fifa_data))
fifa_data=fifa_data[,-c(11,12)]
fifa_data=fifa_data[,-c(13)]


data.frame(colnames(fifa_data))



str(fifa_data)
fifa_data$Club_Domestic_Prestige=as.factor(fifa_data$Club_Domestic_Prestige)
fifa_data$Club_Int_Prestige=as.factor(fifa_data$Club_Int_Prestige)
fifa_data$weak_foot=as.factor(fifa_data$weak_foot)
fifa_data$Player_international_reputation=as.factor(fifa_data$Player_international_reputation)




fifa_data_chr=select_if(fifa_data,is.character)
data.frame(colnames(fifa_data_chr))


'''
str(fifa_data)
data.frame(colnames(fifa_data))

write.csv(fifa_data,"fifa_data_for_eda.csv") ## later converted to excel

options(scipen=999) ### converts scientific notation of numbers to normal notation '''

str(fifa_data)

x = c("nationality","Player_Continent" ,"Club_Continent","preferred_foot" ,"Position_Final",'weak_foot',
      'Player_international_reputation','Club_Int_Prestige','Club_Domestic_Prestige')
fifa_data[x]=lapply(fifa_data[x],as.factor) ## converting the necessary variables into factors (ordinal variables converted to factors for now as their univariate analysis will different than other numeric vars)


fifa_data$S.No.=as.character(fifa_data$S.No.)
fifa_data$short_name=as.character(fifa_data$short_name)
fifa_data$long_name=as.character(fifa_data$long_name)
fifa_data$sofifa_id=as.character(fifa_data$sofifa_id)

str(fifa_data)
summary(fifa_data)

###-----------------------------#### CHECKING DATA FOR NULLS#######--------------------------------------------####

sum(is.na(fifa_data)) ## no nulls in data


###--------------------------------------#### UNIVARIATE ANALYSIS#####--------------------------------------------##
dev.off()
par(mfrow=c(2,2))


######################################### NUMERIC VARIABLES UNIVARIATE ANALYSIS###################################


num_fifa =select_if(fifa_data,is.numeric)
data.frame(colnames(num_fifa))
num_fifa=num_fifa[,c(1,5,8:21)]

data.frame(colnames(num_fifa))

num_fifa = num_fifa[, c(1,3:16,2)]## reordering columns

ncol(num_fifa)
head(num_fifa[1])
colnames(num_fifa[1])

seq(1:15)

i=seq(1,ncol(num_fifa))
i=seq(1,16)
i
names(num_fifa)[14]='Aerial_Battles_Won'
########################PLOTTING BOXPLOTS ('FOR' LOOP USED TO DRAW MULTIPLE BOXPLOTS TOGETHER ) #######################


for (val in i) {
  boxplot(num_fifa[val] , horizontal = T ,main=paste("Boxplot for",colnames(num_fifa[val]),sep = " ") , col = 'green',
          cex.main=1.5,cex.lab=1.5,cex.axis=1.2)
}



dev.off()
par(mfrow=c(2,2))

################# PLOTTING HISTOGRAMS for IVs ('FOR' LOOP USED TO DRAW MULTIPLE HISTOGRAMS TOGETHER ) #############


for (val in i ) {
  hist(num_fifa[,val] ,main=paste("Histogram for",colnames(num_fifa[val]),sep = " ") , col = 'light blue',
       xlab=colnames(num_fifa[val]) , cex.lab=1.5, cex.axis=1.5,cex.main=1.8 ,
       xlim = c(min(num_fifa[,val]),max(num_fifa[,val])))
}

hist(num_fifa$Wages ,main=paste("Histogram for ",colnames(num_fifa[2]),sep = " ") , col = 'light blue',
     xlab=colnames(num_fifa[2])) 


############################################### PLOTTING HISTOGRAMS for DVs########################################


ggplot(fifa_data,aes(x=Value))+
geom_histogram(fill='maroon') +
 ggtitle("Histogram of DV 'Value'")+
  theme(axis.title.x = element_text(size=15))+
  theme(axis.title.y = element_text(size=15))+
  theme(plot.title = element_text(size=20,face='bold',hjust = 0.5))



ggplot(fifa_data,aes(x=log(Value)))+geom_histogram(fill='maroon') +
  ggtitle("Histogram of log of DV 'Value'")+
  theme(axis.title.x = element_text(size=15))+
  theme(axis.title.y = element_text(size=15))+
  theme(plot.title = element_text(size=20,face='bold',hjust = 0.5))




ggplot(fifa_data,aes(x=Wages))+geom_histogram(fill='navy') +
  ggtitle("Histogram of DV 'Wages'")+
  theme(axis.title.x = element_text(size=15))+
  theme(axis.title.y = element_text(size=15))+
  theme(plot.title = element_text(size=20,face='bold',hjust = 0.5))



ggplot(fifa_data,aes(x=log(Wages)))+geom_histogram(fill='navy') +
  ggtitle("Histogram of log of DV 'Wages'")+
  theme(axis.title.x = element_text(size=15))+
  theme(axis.title.y = element_text(size=15))+
  theme(plot.title = element_text(size=20,face='bold',hjust = 0.5))


shapiro.test(fifa_data$Value)
shapiro.test(log(fifa_data$Value))
shapiro.test(fifa_data$wages)
shapiro.test(log(fifa_data$wages))


#######################################CATEGORICAL VARIABLES UNIVARIATE ANALYSIS ###################################

non_num_fifa = select_if(fifa_data,Negate(is.numeric))
data.frame(colnames(non_num_fifa))



non_num_fifa=non_num_fifa[,5:13]
colnames(non_num_fifa)


table(Club_Continent)

display.brewer.all()




ggplot(non_num_fifa,
       aes(x=reorder(Club_Continent,-table(Club_Continent)[Club_Continent])))+
  geom_bar(fill="Maroon") + 
  ggtitle("Count of Clubs by Continent")+xlab("\nContinent")+ylab("Count")+
  theme(plot.title = element_text(hjust = 0.5,face='bold',size=30),
  axis.title.x = element_text(size=25,colour = 'dark red'),
  axis.title.y = element_text(size=25,colour = 'dark red'),
  axis.text.x = element_text(size=20,colour = 'dark blue',angle = 90),
  axis.text.y = element_text(size=20,colour = 'dark blue'))




ggplot(non_num_fifa,
       aes(x=reorder(preferred_foot,-table(preferred_foot)[preferred_foot])))+
  geom_bar(fill="tomato") + 
  ggtitle("Preferred foot of players")+xlab("Preferred Foot")+ylab("Count")+
  theme(plot.title = element_text(hjust = 0.5,face='bold',size=30),
        axis.title.x = element_text(size=25,colour = 'dark red'),
        axis.title.y = element_text(size=25,colour = 'dark red'),
        axis.text.x = element_text(size=20,colour = 'dark blue',angle = 0),
        axis.text.y = element_text(size=20,colour = 'dark blue'))

t =table(non_num_fifa$nationality) # frequency of values in nationality

t=data.frame(t)
names(t)[1]="Nationality"

head(t)  

t=t[order(-t$Freq),]
t=t[1:10,]

t

table(nationality)
ggplot(t,
       aes(x=reorder(Nationality,-Freq),y=Freq))+
  scale_fill_brewer(palette = "Blues")+
  geom_col(fill='sea green') + 
  ggtitle("Top 10 Nationalities")+xlab("Nationality")+ylab("Count")+
  theme(plot.title = element_text(hjust = 0.5,face='bold',size=30),
        axis.title.x = element_text(size=25,colour = 'dark red'),
        axis.title.y = element_text(size=25,colour = 'dark red'),
        axis.text.x = element_text(size=20,colour = 'dark blue',angle = 90),
        axis.text.y = element_text(size=20,colour = 'dark blue'))


ggplot(non_num_fifa,
       aes(x=reorder(Player_Continent,table(Player_Continent)[Player_Continent])))+
  geom_bar(fill='steel blue') + 
  ggtitle("Count of Players by continent")+xlab("Continent")+ylab("Count")+
  theme(plot.title = element_text(hjust = 0.5,face='bold',size=30),
        axis.title.x = element_text(size=25,colour = 'dark red'),
        axis.title.y = element_text(size=25,colour = 'dark red'),
        axis.text.x = element_text(size=20,colour = 'dark blue',angle = 0),
        axis.text.y = element_text(size=20,colour = 'dark blue'))+coord_flip()

ggplot(non_num_fifa,
       aes(x=reorder(Position_Final,-table(Position_Final)[Position_Final])))+
  geom_bar(fill='orange') +xlab("Position")+ylab("Count")+
  ggtitle("Count of Players by Position")+xlab("Player positions")+ylab("Count")+
  theme(plot.title = element_text(hjust = 0.5,face='bold',size=30),
        axis.title.x = element_text(size=25,colour = 'dark red',vjust=15),
        axis.title.y = element_text(size=25,colour = 'dark red'),
        axis.text.x = element_text(size=15,colour = 'dark blue',vjust=1,angle=89),
        axis.text.y = element_text(size=20,colour = 'dark blue'))


ggplot(non_num_fifa,
       aes(x=reorder(Position_Final,-table(Position_Final)[Position_Final])))+
  geom_bar(fill='purple') +xlab("Position")+ylab("Count")+
  ggtitle("Count of Players by Position")+xlab("Player positions")+ylab("Count")+
  theme(plot.title = element_text(hjust = 0.5,face='bold',size=30),
        axis.title.x = element_text(size=25,colour = 'dark red',vjust=15),
        axis.title.y = element_text(size=25,colour = 'dark red'),
        axis.text.x = element_text(size=15,colour = 'dark blue',vjust=1,angle=89),
        axis.text.y = element_text(size=20,colour = 'dark blue'))


ggplot(non_num_fifa,
       aes(x=reorder(Player_international_reputation,-table(Player_international_reputation)[Player_international_reputation])))+
  geom_bar(fill='dark green') +xlab("Internationa Reputation")+ylab("Count")+
  ggtitle("Count of Players by International Reputation")+xlab("International Reputation")+ylab("Count")+
  theme(plot.title = element_text(hjust = 0.5,face='bold',size=30),
        axis.title.x = element_text(size=25,colour = 'dark red'),
        axis.title.y = element_text(size=25,colour = 'dark red'),
        axis.text.x = element_text(size=15,colour = 'dark blue'),
        axis.text.y = element_text(size=20,colour = 'dark blue'))

ggplot(non_num_fifa,
     aes(x=reorder(Club_Int_Prestige,-table(Club_Int_Prestige)[Club_Int_Prestige])))+
  geom_bar(fill='purple') +xlab("Club International Prestige")+ylab("Count")+
  ggtitle("Count of Players by Club International Prestige")+
  theme(plot.title = element_text(hjust = 0.5,face='bold',size=30),
        axis.title.x = element_text(size=25,colour = 'dark red'),
        axis.title.y = element_text(size=25,colour = 'dark red'),
        axis.text.x = element_text(size=15,colour = 'dark blue'),
        axis.text.y = element_text(size=20,colour = 'dark blue'))


ggplot(non_num_fifa,
       aes(x=reorder(Club_Domestic_Prestige,table(Club_Domestic_Prestige)[Club_Domestic_Prestige])))+
  geom_bar(fill='dark red') +xlab("Club Domestic Prestige")+ylab("Count")+
  ggtitle("Count of Players by Club Domestic Prestige")+
  theme(plot.title = element_text(hjust = 0.5,face='bold',size=30),
        axis.title.x = element_text(size=25,colour = 'dark red'),
        axis.title.y = element_text(size=25,colour = 'dark red'),
        axis.text.x = element_text(size=15,colour = 'dark blue'),
        axis.text.y = element_text(size=20,colour = 'dark blue'))+coord_flip()


ggplot(non_num_fifa,
       aes(x=reorder(weak_foot,table(weak_foot)[weak_foot])))+
  geom_bar(fill='royal blue') +xlab("Weak Foot Rating")+ylab("Count")+
  ggtitle("Count of Players by Weak Foot Rating")+
  theme(plot.title = element_text(hjust = 0.5,face='bold',size=30),
        axis.title.x = element_text(size=25,colour = 'dark red'),
        axis.title.y = element_text(size=25,colour = 'dark red'),
        axis.text.x = element_text(size=15,colour = 'dark blue'),
        axis.text.y = element_text(size=20,colour = 'dark blue'))+coord_flip()


####--------------------------------------#### BIVARIATE ANALYSIS#####--------------------------------------------###

## correlation

x = c('weak_foot','Player_international_reputation','Club_Int_Prestige','Club_Domestic_Prestige')
fifa_data[x]=lapply(fifa_data[x],as.numeric)

str(fifa_data)
colnames(fifa_data)
numeric_vars=c('age','Value','Market_Value','Wages','pace','shooting','passing','dribbling','defending','physic','Mins_Total',
               'Shots_per_game_total','Aerial_Battles_Won_Per_Game_Total','Goals_total','Assists_Total','
               Yellow_Cards_Total','Red_Cards_Total','Man_of_The_Match_Total','weak_foot',"Club_Int_Prestige",
               'Club_Domestic_Prestige','Player_international_reputation')

fifa_data_numeric = select_if(fifa_data,(colnames(fifa_data)%in%c(numeric_vars)))

colnames(fifa_data_numeric)

str(fifa_data_numeric)

write.csv(fifa_data_numeric,'fifa_numeric.csv')

fifa_data_numeric_2 = read.csv('fifa_numeric.csv')

head(fifa_data_numeric_2)

fifa_data_numeric_2=fifa_data_numeric_2[,c(2:21)]

## correlation for continuous variables only 

ord_vars=c('weak_foot',"Club_Int_Prestige",
           'Club_Domestic_Prestige','Player_international_reputation')
cont_vars = c('age','Value','Market_Value','Wages','pace','shooting','passing','dribbling','defending','physic','Mins_Total',
              'Shots_per_game_total','Aerial_Battles_Won_Per_Game_Total','Goals_total','Assists_Total','
               Yellow_Cards_Total','Red_Cards_Total','Man_of_The_Match_Total')

fifa_data_cont= select_if(fifa_data_numeric_2,
                          (colnames(fifa_data_numeric_2)%in%c(cont_vars)))

cor_numeric=cor(fifa_data_cont,method = 'pearson')

cor_numeric = data.frame(cor_numeric)

corrplot(as.matrix(cor_numeric),method="color",addCoef.col = "blue",type = 'lower',
         title = "Correlation between all numeric variables (Pearson Method)",
         tl.cex = 1,tl.srt = 20,mar=c(0,0,2,0),tl.col = 'black',number.cex = .8,diag = F,tl.offset = .45,
         col=colorRampPalette(c("darkgreen ","white","darkred"))(100))

## correlation for ordinal as well as continous variables 

cor_all=cor(fifa_data_numeric_2,method = 'pearson')

cor_all = data.frame(cor_all)



corrplot(as.matrix(cor_all),method="color",addCoef.col = "blue",type = 'lower',
         title = "Correlation between all numeric variables (Spearman Method)",
         tl.cex = 1,tl.srt = 20,mar=c(0,0,2,0),tl.col = 'black',number.cex = .8,diag = F,tl.offset = .45,
         col=colorRampPalette(c("darkgreen ","white","darkred"))(100))

library("car")


### calculating VIF after running regression 

### regressing 'Value' with numeric IVs

str(fifa_data)
data.frame(colnames(fifa_data))

fifa_data_2=fifa_data[,-c(1:4,6)]

str(fifa_data_2)

fifa_data_2=fastDummies::dummy_cols(fifa_data_2,remove_first_dummy = T,remove_selected_columns = T)
  
names(fifa_data_2)=make.names(names(fifa_data_2),unique = T)
str(fifa_data_2)

lm1 =lm(Value~Wages +age + pace +shooting + #defending + #dribbling
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
          +Player_Continent_Asia +Player_Continent_Europe 
        +Player_Continent_North.America +Player_Continent_Oceania +Player_Continent_South.America 
        +Club_Continent_Europe.Tier.1 +Club_Continent_Europe.Tier.2 +Club_Continent_Europe.Tier.3 
        +Club_Continent_Europe.Tier.4 +Club_Continent_North.America +Club_Continent_South.America
        +preferred_foot_Right +Position_Final_Attacking.Midfield.Forward +Position_Final_Defense 
        +Position_Final_Defensive.Midfield +Position_Final_Defensive.Midfield.Defense +Position_Final_Forward
        +Position_Final_Midfield..Attacking.Defensive. , data = fifa_data_2)


vif_results=data.frame(vif(lm1))
names(vif_results)[1]='VIF'

summary(lm1)
vif_results
### regressing 'Wages' with numeric IVs

num_fifa_3= select_if(fifa_data,is.numeric)

data.frame(colnames(num_fifa_3))

num_fifa_3=num_fifa_3[-c(1,2,4)]


wages_lm=lm(Wages~. , data = num_fifa_3)
b = vif(wages_lm)

vif_results_wages=data.frame(b)
vif_results_wages

names(vif_results_wages)[1]='VIF'

vif_results_wages

write.csv(vif_results_wages,"VIF_Wages.csv")




######################################### DRAWING SCATTERPLOTS####################################################

##################### USING NESTED FOR LOOPS TO MAKE ALL SCATTERPLOTS TOGETHER #################################


col_len=1:length(num_fifa)
col_len

colnames(num_fifa)


dev.off()
par(mfrow=c(2,2))

## pdf('scatterplots') to save all plots as pdfs

for (i in col_len){
  for (j in col_len){
    x=plot(num_fifa[,1], y=num_fifa[,2], xlab = colnames(num_fifa[i]), ylab=colnames(num_fifa[j]), 
           main = paste("Scatterplot between ",colnames(num_fifa[i]),"and ",colnames(num_fifa[j]))
           ,pch = 19,col='blue') ## changing shape of dots to be filled instead of hollow(pch) with color blue
    abline(lm(num_fifa[,j]~num_fifa[,i]),col='red',lwd=2) ## lwd = line width 
    legend("bottomright",bg='yellow',text.font=4, ## making font of legend bold and italic 
           legend = paste ("R = " ,round(cor(num_fifa[,i],num_fifa[,j]),3))) ## adding legend to show correlation and rounding it to 3 digits
  }}


  plot(y=fifa_data$Value, x=fifa_data$Wages, xlab = "Market Value", ylab="Wages", 
     main = paste("Scatterplot between Market Value and Wages "),pch = 19,col='blue') ## changing shape of dots to be filled instead of hollow(pch) with color blue
abline(lm(formula =Value~Wages,data = fifa_data),col='red',lwd=2) ## lwd = line width 
legend("bottomright",bg='yellow',text.font=4, ## making font of legend bold and italic 
       legend = paste ("R = " ,round(cor(fifa_data$Value,fifa_data$Wages),3)))

## dev.off()run this at the end if you are saving all plots as pdfs





#######################################CATEGORICAL VARIABLES BIVARIATE ANALYSIS##################################


ggplot(non_num_fifa,
       aes(x=reorder(Position_Final,-table(Position_Final)[Position_Final]),
           fill=preferred_foot))+ 
  labs(fill="Preferred Foot")+
  geom_bar() + 
  ggtitle("Preferred foot of players by position")+xlab("Position")+ylab("Count")+
  theme(plot.title = element_text(hjust = 0.5,face='bold',size=30),
        axis.title.x = element_text(size=25,colour = 'dark red'),
        axis.title.y = element_text(size=25,colour = 'dark red',vjust = -5),
        axis.text.x = element_text(size=15,colour = 'dark blue',angle = 0),
        axis.text.y = element_text(size=15,colour = 'dark blue',angle=30),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))+
  coord_flip()


ggplot(non_num_fifa,
       aes(x=reorder(Player_Continent,table(Player_Continent)[Player_Continent]),
           fill=Position_Final))+ 
  labs(fill="Position")+
  scale_fill_manual(values = c('dark green','lime green','royal blue','navy blue','blue','green','yellow'))+
  geom_bar() + 
  ggtitle("Player Positions by Geography")+xlab("Continent")+ylab("Count")+
  theme(plot.title = element_text(hjust = 0.5,face='bold',size=30),
        axis.title.x = element_text(size=25,colour = 'dark red'),
        axis.title.y = element_text(size=25,colour = 'dark red'),
        axis.text.x = element_text(size=15,colour = 'dark blue',angle = 0),
        axis.text.y = element_text(size=15,colour = 'dark blue',angle=0),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))+
  coord_flip()



plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)

file.copy(from=plots.png.paths, to="C:\\Users\\U370867.l\\Desktop\\New folder")





#### CATEGORICAL AND NUMERIC VARS BIVARIATE ANALYSIS ####
####### IMPORTING and PPREPARING DATA####
fifa_data = read_excel('fifa_data_for_eda.xlsx')
fifa_data %>% explore()
attach(fifa_data)


x=c('S.No.','sofifa_id')
y=c('nationality','Player_Continent','Club_Continent','Club_Int_Prestige','Club_Domestic_Prestige','preferred_foot'
    ,'Player_international_reputation','weak_foot','Position_Final')

fifa_data[x]=lapply(fifa_data[x],as.character)
fifa_data[y]=lapply(fifa_data[y],as.factor)

options(scipen = 999)
str(fifa_data)

par(mfrow=c(2,2))

ggplot(data = fifa_data,aes(x=Value)) + 
  geom_density(aes(fill=preferred_foot),alpha=0.3)+ ## fill is used to specify the hue
  labs(fill = 'Preferred Foot')## change legend for Hue

ggplot(data = fifa_data,aes(x=Value)) + 
  geom_density(aes(fill=preferred_foot),alpha=0.3)+ ## fill is used to specify the hue
  labs(fill = 'Preferred Foot')## change legend for Hue

ggplot(data = fifa_data,aes(x=Value)) + 
  geom_density(aes(fill=Player_Continent),alpha=0.3)+ ## fill is used to specify the hue
  labs(fill = 'Player_Continent')## change legend for Hue


ggplot(data = fifa_data,aes(x=Value)) + 
  geom_density(aes(fill=Club_Continent),alpha=0.3)+ ## fill is used to specify the hue
  labs(fill = 'Club_Continent')## change legend for Hue


ggplot(data = fifa_data,aes(x=Value)) + 
  geom_density(aes(fill=Club_Int_Prestige),alpha=0.3)+ ## fill is used to specify the hue
  labs(fill = 'Club_Int_Prestige')## change legend for Hue


ggplot(data = fifa_data,aes(x=Value)) + 
  geom_density(aes(fill=Club_Domestic_Prestige),alpha=0.3)+ ## fill is used to specify the hue
  labs(fill = 'Club_Domestic_Prestige')## change legend for Hue


ggplot(data = fifa_data,aes(x=Value)) + 
  geom_density(aes(fill=Player_international_reputation),alpha=0.3)+ ## fill is used to specify the hue
  labs(fill = 'Player_international_reputation')## change legend for Hue

ggplot(data = fifa_data,aes(x=Value)) + 
  geom_density(aes(fill=weak_foot),alpha=0.3)+ ## fill is used to specify the hue
  labs(fill = 'weak_foot')## change legend for Hue

ggplot(data = fifa_data,aes(x=Value)) + 
  geom_density(aes(fill=Position_Final),alpha=0.3)+ ## fill is used to specify the hue
  labs(fill = 'Position_Final')## change legend for Hue


