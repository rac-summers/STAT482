rm(list=ls())
library(readxl)

pres = read_excel("1976-2020-president-cleaned.xlsx")
library(dplyr)
library(tidyr)

rep_pres = read_excel("1976-2020-president-cleaned-republican.xlsx")
rep_pres = rep_pres%>%filter(year != 1976 & year != 1980 & year != 1984)
rep_pres = rep_pres[rep_pres$state!="DISTRICT OF COLUMBIA",]
election_years = seq(1988,2020,by=4)

per_capita_income = read_excel("Per Capita Income Clean.xlsx")
per_capita_income$GeoName[per_capita_income$GeoName=="Alaska *"] = "Alaska"
per_capita_income$GeoName[per_capita_income$GeoName=="Hawaii *"] = "Hawaii"
per_capita_income = per_capita_income[(tolower(per_capita_income$GeoName) %in% tolower(rep_pres$state)),]



urban = read_excel("% Urban Population Clean.xlsx")
urban = urban%>%pivot_longer(!'Area Name',names_to="Year",values_to="UrbanPopulation")
urban = urban%>%filter(`Area Name` != "United States")
urban = urban%>%filter(`Area Name` != "District of Columbia")

unemployment = read_excel("clean_unemployment.xlsx")
unemployment = unemployment%>%filter(Area != "District of Columbia")

senate = read_excel("1984-2020-senate-cleaned.xlsx")
senate = senate%>%filter(year>=1988)


############################################################
## Fitting Model                                  
############################################################


rep_pres1 = rep_pres[order(rep_pres$year,rep_pres$state),]
per_capita_income1 = per_capita_income[order(per_capita_income$Year,per_capita_income$GeoName),]
senate1 = senate[order(senate$year,senate$state),]
unemployment1 = unemployment[order(unemployment$Year,unemployment$Area),]
urban1 = urban[order(urban$Year,urban$`Area Name`),]

income_x = c()
unemp_x = c()
urban_x = c()
senate_x = c()
state_x = c()
y = rep_pres1$Proportion_of_Votes
state_x = rep_pres1$state
for (i in election_years){
  income_x = c(income_x,per_capita_income1$persincome[per_capita_income1$Year == (i-1)])
  unemp_x = c(unemp_x,unemployment1$Unemployment[unemployment1$Year == (i-1)])
  urban_x = c(urban_x,urban1$UrbanPopulation[urban1$Year == max(urban1$Year[urban1$Year<=i])])
  senate_x = c(senate_x,senate1$senator_status[senate1$year==i])
}

state_x = as.factor(state_x)
state_x = relevel(state_x,ref="OHIO")
model_whole_world = lm(y~income_x + unemp_x + urban_x + senate_x + state_x)
summary(model_whole_world)

######################################
#### Predictions #####################
#######################################
per_capita_income_2023 = c(71964, 98811, 84380, 69357, 111622,100933,
                           115337, 92308,83104,85691,107348,77399,95115,76984,
                           80316,82103,72318,73759,78301,114236,115964,
                           80803,96814,65156,78194,76834,82306,84350,
                           101292,117868,70241,105304,79620,85506,78797,
                           74195,88137,87262,92427,76390,77932,76937,89506,
                           94452,83767,106023,103669,65332,82757,83583)

unemployment_2023 = c(2.5, 4.2, 3.9, 3.3, 4.8, 3.2, 3.8, 4.0, 2.9, 3.2, 3.0, 3.1, 
                      4.5, 3.3, 2.9, 2.7, 4.2, 3.7, 2.9, 2.1, 3.4, 3.9, 2.8, 3.2, 3.0, 2.9, 2.3,
                      5.1, 2.2, 4.4, 3.8, 4.2, 3.5, 1.9, 3.5,3.2, 3.7, 3.4, 3.0, 3.0, 2.0, 3.3,
                      3.9, 2.6, 2.0, 2.9, 4.1, 3.9, 3.0, 2.9)

states_2023 = rep_pres1$state[rep_pres$year==2016]

urban_2023 = urban1$UrbanPopulation[urban1$Year==2020]

senate_2023 = c("Republicans","Republicans","Mixed","Republicans","Democrats","Democrats",
                "Democrats","Democrats","Republicans","Democrats","Democrats","Republicans",
                "Democrats","Republicans","Republicans","Republicans","Republicans",
                "Republicans","Mixed","Democrats","Democrats","Democrats","Democrats",
                "Republicans","Republicans","Mixed","Republicans","Democrats","Democrats",
                "Democrats","Democrats","Democrats","Republicans","Republicans",
                "Mixed","Republicans","Democrats","Democrats","Democrats","Republicans",
                "Republicans","Republicans","Republicans","Republicans","Mixed","Democrats",
                "Democrats","Mixed","Mixed","Republicans")
data_2023 = cbind(states_2023,",",senate_2023,",",urban_2023,",",unemployment_2023,",",per_capita_income_2023)
write.matrix(data_2023,"Data 2023.csv")


predict(model_whole_world,newdata=
          data.frame(income_x=71964,unemp_x=2.5,urban_x=57.7,
                     senate_x="Republicans",state_x="ALABAMA"))

predictions = c()
for (i in 1:50){
  predictions = c(predictions,predict(model_whole_world,
                                      newdata=data.frame(income_x=per_capita_income_2023[i],
                                                         unemp_x=unemployment_2023[i],
                                                         urban_x=urban_2023[i],
                                                         senate_x=senate_2023[i],
                                                         state_x=states_2023[i])))
}
pred_table = cbind(states_2023,",",predictions)

hist(predictions)

library(MASS)
write.matrix(pred_table,file="prediction_table.csv")

min(predictions)
