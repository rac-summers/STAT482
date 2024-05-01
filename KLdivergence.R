rm(list=ls())

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

#######################################
#### KL Divergence ####################
#######################################

#KL formula for two normal distributions
kl_div <- function(mu1, sigma1, mu2, sigma2) {
  return((log(sigma2/sigma1) + ((sigma1^2) + (mu1 - mu2)^2) / (2 * (sigma2^2))) - 0.5)
}

#Create df with beta estimates and their intervals
results <- summary(model_whole_world)
results_estimates <- results$coefficients[, "Estimate"]
results_se <- results$coefficients[, "Std. Error"]

results_df <- data.frame(estimates = results_estimates, se=results_se)
library(dplyr)
library(stringr)
library(tibble)
results_df <- results_df %>%  #remove variables that aren't states
  rownames_to_column("state") %>%
  filter(str_starts(state, "state_")) 
results_df <- results_df %>%    #change names to clearly be states
  mutate(state = str_replace(state, "state_x", ""))


#Calculate KL divergence between two states
ALABAMA_mu <- results_df %>%
  filter(state == "ALABAMA") %>%
  pull(estimates)
ALABAMA_se <- results_df %>%
  filter(state == "ALABAMA") %>%
  pull(se)
ALASKA_mu <- results_df %>%
  filter(state == "ALASKA") %>%
  pull(estimates)
ALASKA_se <- results_df %>%
  filter(state == "ALASKA") %>%
  pull(se)
kl_div(ALABAMA_mu, ALABAMA_se, ALASKA_mu, ALASKA_se) 

state_list <- c()
for (my_state in (results_df$state)) {
  state_list <- c(state_list, my_state)  #missing Ohio
}

kl_df <- data.frame(state = state_list)
kl_ALABAMA <- c() #create vectors/df to fill

for (second_state in (results_df$state)) {
  state_mu <- results_df %>%
    filter(state == second_state) %>%
    pull(estimates)
  state_se <- results_df %>%
    filter(state == second_state) %>%
    pull(se)
  
  kl_ALABAMA <- c(kl_ALABAMA, kl_div(ALABAMA_mu, ALABAMA_se, state_mu, state_se)) # Calculate KL divergence
  }
kl_df <- cbind(kl_df, kl_ALABAMA)

#Now iterate through two layers
kl_matrix <- matrix(NA, nrow = length(results_df$state), ncol = length(results_df$state))
rownames(kl_matrix) <- unique(results_df$state)
colnames(kl_matrix) <- unique(results_df$state)

for (first_state in state_list) {
  first_state_mu <- results_df %>%  #first state estimates
    filter(state == first_state) %>%
    pull(estimates)
  first_state_se <- results_df %>%
    filter(state == first_state) %>%
    pull(se)

  for (second_state in state_list) {
    second_state_mu <- results_df %>%  #second state estimates
      filter(state == second_state) %>%
      pull(estimates)
    second_state_se <- results_df %>%
      filter(state == second_state) %>%
      pull(se)
    
    kl_matrix[first_state, second_state] <- kl_div(first_state_mu, first_state_se, second_state_mu, second_state_se)  #kl divergence
  }
}

symmetric_kl_matrix <- (kl_matrix + t(kl_matrix)) / 2  #takes the average between the two sides for one matrix

#######################################
#### Clustering #######################
#######################################

#ChatGPT
distance_matrix <- as.dist(kl_matrix)  # Convert to a distance object
hc <- hclust(distance_matrix)         # Perform hierarchical clustering

#determine optimal number of clusters
#library(factoextra)
#dend <- as.dendrogram(hc)
#fviz_nbclust(kl_matrix, FUN = hcut, method = "wss")
#fviz_nbclust(kl_matrix, FUN = hcut, method = "silhouette")
#k_optimal <- which.max(sil_width) 
clusters <- cutree(hc, k = 3)  #split into 3 clusters

state_clusters <- data.frame(state = rownames(kl_matrix), cluster = clusters) #assign clusters to states

library(maps)
library(ggplot2)
us_map <- map_data("state")
state_clusters$state <- tolower(state_clusters$state)
map_clusters <- merge(us_map, state_clusters, by.x = "region", by.y = "state", all.x = TRUE) #merge data
map_clusters <- map_clusters[order(map_clusters$group, map_clusters$order), ]

clustered_plot <- ggplot(map_clusters, aes(x = long, y = lat, group = group, fill = factor(cluster))) +
  geom_polygon(color = "black") +
  coord_fixed(1.3) +
  labs(fill = "Cluster", title = "Spatial Clustering of States Based on Average KL Divergence") +
  theme_void() +
  theme(plot.background = element_rect(fill = "white"),  # Set plot background to white
        legend.position = "right") +
  scale_fill_manual(values = c("deepskyblue4","lightblue1","aquamarine"))

ggsave(filename = "KL_Divergence_Clustering.png", plot = clustered_plot, width = 10, height = 8)

#######################################
#### Mapping #######################
#######################################
library(maps)
library(ggplot2)
#ALABAMA
ALABAMA_divergence <- data.frame(state = rownames(symmetric_kl_matrix), divergence = symmetric_kl_matrix["ALABAMA", ])
us_map <- map_data("state") #get US map
us_map$region <- toupper(us_map$region)
map_divergence <- merge(us_map, ALABAMA_divergence, by.x = "region", by.y = "state", all.x = TRUE) # Merge KL divergence data with spatial data
map_divergence <- map_divergence[order(map_divergence$group, map_divergence$order), ] #put in correct order for clean mapping

ggplot(map_divergence, aes(x = long, y = lat, group = group, fill = divergence)) +
  geom_polygon(color = "white") +
  scale_fill_gradient(low = "midnightblue", high = "white", na.value = "grey50", name = "KL Divergence") +
  coord_fixed(1.3) +
  labs(title = "Heatmap of KL Divergence from Alabama") +
  theme_void() +
  theme(plot.background = element_rect(fill = "white"),  # Set plot background to white
        legend.position = "right")

#CALIFORNIA
CALIFORNIA_divergence <- data.frame(state = rownames(symmetric_kl_matrix), divergence = symmetric_kl_matrix["CALIFORNIA", ])
us_map <- map_data("state") #get US map
us_map$region <- toupper(us_map$region)
map_divergence <- merge(us_map, CALIFORNIA_divergence, by.x = "region", by.y = "state", all.x = TRUE) # Merge KL divergence data with spatial data
map_divergence <- map_divergence[order(map_divergence$group, map_divergence$order), ] #put in correct order for clean mapping
ggplot(map_divergence, aes(x = long, y = lat, group = group, fill = divergence)) +
  geom_polygon(color = "gray") +
  scale_fill_gradient(low = "midnightblue", high = "white", na.value = "grey50", name = "KL Divergence") +
  coord_fixed(1.3) +
  labs(title = "Heatmap of KL Divergence from Calfornia") +
  theme_void() +
  theme(legend.position = "right")

#NEW YORK
NEWYORK_divergence <- data.frame(state = rownames(symmetric_kl_matrix), divergence = symmetric_kl_matrix["NEW YORK", ])
us_map <- map_data("state") #get US map
us_map$region <- toupper(us_map$region)
map_divergence <- merge(us_map, NEWYORK_divergence, by.x = "region", by.y = "state", all.x = TRUE) # Merge KL divergence data with spatial data
map_divergence <- map_divergence[order(map_divergence$group, map_divergence$order), ] #put in correct order for clean mapping
ggplot(map_divergence, aes(x = long, y = lat, group = group, fill = divergence)) +
  geom_polygon(color = "gray") +
  scale_fill_gradient(low = "midnightblue", high = "white", na.value = "grey50", name = "KL Divergence") +
  coord_fixed(1.3) +
  labs(title = "Heatmap of KL Divergence from New York") +
  theme_void() +
  theme(legend.position = "right")

#WYOMOING
WYOMING_divergence <- data.frame(state = rownames(symmetric_kl_matrix), divergence = symmetric_kl_matrix["WYOMING", ])
us_map <- map_data("state") #get US map
us_map$region <- toupper(us_map$region)
map_divergence <- merge(us_map, WYOMING_divergence, by.x = "region", by.y = "state", all.x = TRUE) # Merge KL divergence data with spatial data
map_divergence <- map_divergence[order(map_divergence$group, map_divergence$order), ] #put in correct order for clean mapping
ggplot(map_divergence, aes(x = long, y = lat, group = group, fill = divergence)) +
  geom_polygon(color = "gray") +
  scale_fill_gradient(low = "midnightblue", high = "white", na.value = "grey50", name = "KL Divergence") +
  coord_fixed(1.3) +
  labs(title = "Heatmap of KL Divergence from Wyoming") +
  theme_void() +
  theme(legend.position = "right")

#TEXAS
TEXAS_divergence <- data.frame(state = rownames(symmetric_kl_matrix), divergence = symmetric_kl_matrix["TEXAS", ])
us_map <- map_data("state") #get US map
us_map$region <- toupper(us_map$region)
map_divergence <- merge(us_map, TEXAS_divergence, by.x = "region", by.y = "state", all.x = TRUE) # Merge KL divergence data with spatial data
map_divergence <- map_divergence[order(map_divergence$group, map_divergence$order), ] #put in correct order for clean mapping
ggplot(map_divergence, aes(x = long, y = lat, group = group, fill = divergence)) +
  geom_polygon(color = "gray") +
  scale_fill_gradient(low = "midnightblue", high = "white", na.value = "grey50", name = "KL Divergence") +
  coord_fixed(1.3) +
  labs(title = "Heatmap of KL Divergence from Texas") +
  theme_void() +
  theme(legend.position = "right")

#Loop
library(ggplot2)
library(maps)
library(stringr)

for (my_state in unique(results_df$state)) {
  my_state <- as.character(my_state)
  state_divergence <- data.frame(state = rownames(symmetric_kl_matrix), 
                                 divergence = kl_matrix[my_state, ])
  us_map <- map_data("state") #get US map
  us_map$region <- toupper(us_map$region)
  map_divergence <- merge(us_map, state_divergence, by.x = "region", by.y = "state", all.x = TRUE)
  map_divergence <- map_divergence[order(map_divergence$group, map_divergence$order), ]
  my_state_title <- str_to_title(my_state)
  state_plot <- ggplot(map_divergence, aes(x = long, y = lat, group = group, fill = divergence)) +
    geom_polygon(color = "gray") +
    scale_fill_gradient(low = "midnightblue", high = "white", na.value = "grey50", name = "KL Divergence") +
    coord_fixed(1.3) +
    labs(title = paste("Heatmap of KL Divergence from", my_state_title)) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white"),  # Set plot background to white
          legend.position = "right")
  ggsave(filename = paste("KL_Divergence_Map_", my_state_title, ".png", sep = ""), plot = state_plot, width = 10, height = 8)
}
