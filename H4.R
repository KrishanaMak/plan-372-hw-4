#loading in libraries necessary for this assignment
library(tidyverse)
library(tidycensus)

#reading in data
data = read_csv("airport_pairs.csv")

# QUESTION ONE

#filtered the original dataset to only include origin and destination of RDU 
#with more than 10k passengers. Then, I joined together these tables.
rdu_dataset = data
rdu_origin = filter(rdu_dataset, origin == 'RDU')
rdu_origin = filter(rdu_origin, passengers >= 10000)
rdu_dest = filter(rdu_dataset, dest == 'RDU')
rdu_dest = filter(rdu_dest, passengers >= 10000)
rdu_data = rbind(rdu_origin,rdu_dest)

#What is the most popular non-stop destination from RDU?
#I sorted the rdu_origin table in descending order and then tried to assign popular_location
#as a value for the most popular non-stop destination from RDU, however, it saved
#as a table
rdu_origin[order(rdu_origin$passengers, decreasing=TRUE),]
#ANS: Atlanta

# QUESTION TWO

#First I got the census data using a geography of cbsa and getting the total populations column along with the median household income.
#Then, I removed two extra columns that I did not think would be useful. 
census_data = get_acs(geography = "cbsa",  variables = c('total_pop' ='B01003_001', 'median_household_income'= 'B19013_001E'),output = 'wide', year = 2019)
census_data = census_data[,-4]
census_data = census_data[,-5]

#I then created two new columns for origin and destination so that I could differentiate the data.
census_origin = census_data
colnames(census_origin) = c('origin_cbsa','orign_cbsa_name_2','origin_pop', 'origin_mhi')

census_dest = census_data
colnames(census_dest) = c('dest_cbsa','dest_cbsa_name_2','dest_pop', 'dest_mhi')

#Next, I merged the tables I created to the all_airport_cbsa table that I had made using a left join.
#I also removed NA values within the columns of dest_cbsa and origin_cbsa. 
all_data = data
all_airport_cbsa = merge(x=all_data, y=census_origin, by="origin_cbsa", all.x=TRUE)
all_airport_cbsa = merge(x=all_airport_cbsa, y=census_dest, by="dest_cbsa", all.x=TRUE)
all_airport_cbsa = filter(all_airport_cbsa, !is.na(dest_cbsa) & !is.na(origin_cbsa))

#Using the new table that I created, all_cbsa_volumes , I grouped and summarized all_airport_cbsa so
#so that the data was more concise. 
all_cbsa_volumes = all_airport_cbsa %>%
  group_by(origin_cbsa, dest_cbsa) %>%
  summarise(origin_population = mean(origin_pop), dest_population = mean(dest_pop), origin_mhi = mean(origin_mhi), dest_mhi = mean(dest_mhi), distancemiles = round(mean(distancemiles), digit = 0), total_passengers = sum(passengers)) 

#The following is my code to make the three required scatterplots. 
ggplot(all_cbsa_volumes, aes(x=origin_population, y=total_passengers)) +
  geom_point(size=0.1)+
  xlab("Origin Population") +
  ylab("Total Passengers") +
  ggtitle("Scatterplot of Origin Population and Total Passengers ")

ggplot(all_cbsa_volumes, aes(x=dest_population, y=total_passengers)) +
  geom_point(size=0.1)+
  xlab("Dest Population") +
  ylab("Total Passengers") +
  ggtitle("Scatterplot of Dest Population and Total Passengers ")

ggplot(all_cbsa_volumes, aes(x=distancemiles, y=total_passengers)) +
  geom_point(size=0.1)+
  xlab("Distance of Miles") +
  ylab("Total Passengers") +
  ggtitle("Scatterplot of Distance of Miles and Total Passengers")

#Here is my scatterplot for EC
ggplot(all_cbsa_volumes, aes(x=dest_mhi, y=total_passengers)) +
  geom_point(size=0.1)+
  xlab("Dest Median Household Income") +
  ylab("Total Passengers") +
  ggtitle("EC:Scatterplot of Dest Median Household Income and Total Passengers")

ggplot(all_cbsa_volumes, aes(x=origin_mhi, y=total_passengers)) +
  geom_point(size=0.1)+
  xlab("Origin Median Household Income") +
  ylab("Total Passengers") +
  ggtitle("EC:Scatterplot of Origin Median Household Income and Total Passengers")

# QUESTION THREE
#Time to run regressions
#regression of total passenger volumes on origin CBSA population, destination CBSA 
#population, distance between cities, and any other Census variables you wish to include
regression = lm(total_passengers ~ origin_population + dest_population + distancemiles + origin_mhi + dest_mhi, all_cbsa_volumes)
summary(regression)

# QUESTION FOUR
# Create a table for RDU, PDX, ELP, TLH, and SAN with origin, destination, origin_cbsa, dest_cbsa, and distance miles
destinations = data.frame(origin = c("RDU","RDU","RDU","RDU","PDX", "ELP", "TLH", "SAN"), destination = c("PDX", "ELP", "TLH", "SAN","RDU","RDU","RDU","RDU"),origin_cbsa = c('39580','39580','39580','39580','38900','21340','45220','41740'), dest_cbsa = c('38900','21340','45220','41740','39580','39580','39580','39580'),distancemiles = c(2363,1606,496,2193,2363,1606,496,2193))

# Merge the two tables to get population data for each destination airport
destination_census = merge(x=destinations, y=census_origin, by="origin_cbsa", all.x=TRUE)
destination_census = merge(x=destination_census, y=census_dest, by="dest_cbsa", all.x=TRUE)

#Re-formated the dataframe to make it match the variable in the regression equation. 
destination_census = destination_census[, c(3,2,4,1,5,7,10,8,11)]
colnames(destination_census) = c('origin','orign_cbsa','dest', 'dest_cbsa', 'distancemiles', 'origin_population','dest_population','origin_mhi', 'dest_mhi')

#predict function
destination_census$estimated_passengers = predict(regression, destination_census)