
#Loading required libraries
library(tidyverse)
library(lubridate)
library(plyr)

#Loading the COVID-19 data
covid_data<- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")

#Converting date variable to date format
covid_data$date <- ymd(covid_data$date)

#Filter data to include only from Jan 30-March 1, 2023 and valid data
covid_data_filtered <- covid_data %>% 
  filter(date < as.Date("2023-03-01"), date > (as.Date("2023-03-01")- 30), total_cases_per_million != "NA", population_density != "NA", new_cases_per_million != "NA")

#Dividing into regions
asia <- covid_data_filtered %>%
  filter(continent == "Asia")
africa <- covid_data_filtered %>%
  filter(continent == "Africa")
n_america <- covid_data_filtered %>%
  filter(continent == "North America")
s_america <- covid_data_filtered %>%
  filter(continent == "South America") 
europe <- covid_data_filtered %>%
  filter(continent == "Europe") 
oceania <- covid_data_filtered %>%
  filter(continent == "Oceania") 
continents_lbls <- c("Africa", "Asia", "Europe", "North America", "Oceania", "South America")

#Helper function to group the data from the continents by date, combining total and new cases for each country. 
group_by_date <-function(df) {
  grouped_by_date <- ddply(df, .(df$date), summarise, total_cases_per_million = sum(total_cases_per_million), new_cases_per_million = sum(new_cases_per_million))
  return(grouped_by_date)
}

#PART 1 - New cases per million
#1a - graphing new cases per million
#Function to plot the total number of cases per million since January 30, 2023. 
plot_cases <- function(df, color, first) {
  df_since_20 <- df$date - (as.Date("2023-03-01"))
  df_grouped <- group_by_date(df)
  if(first) {
    plot(df_grouped$`df$date`, df_grouped$new_cases_per_million, xlab="Date", ylab="New Cases Per Million", type="l", col=color, main="Figure 1 - New Cases Per Million By Continent Since Jan 30, 2023", ylim = c(0, 13000))
    #plot(df_grouped$`df$date`, df_grouped$total_cases_per_million, xlab="Date", ylab="Total Cases Per Million", type="l", col=color, main="Cases Per Million By Continent Since Jan 30, 2023")
  } else {
    #lines(df_grouped$`df$date`, df_grouped$total_cases_per_million, col=color, yaxt="n")
    plot(df_grouped$`df$date`, df_grouped$new_cases_per_million, col=color, yaxt="n", ylim = c(0, 13000), type="l", ylab="", xlab="")
    #lines(df_grouped$`df$date`, df_grouped$total_cases_per_million, xlab="Date", type="l", ylab="Total Cases Per Million", col=color, yaxt="n")
  }
}

#Plotting the case increases on the same plot.
plot_cases(asia, color="sienna1", TRUE)
par(new=TRUE)
plot_cases(africa, "red4", FALSE)
par(new=TRUE)
plot_cases(europe, "gold", FALSE)
par(new=TRUE)
plot_cases(n_america, "seagreen4", FALSE)
par(new=TRUE)
plot_cases(s_america, "purple", FALSE)
par(new=TRUE)
plot_cases(oceania, "blue", FALSE)
legend('topright', legend=continents_lbls, col=c("red4", "sienna1", "gold", "seagreen4", "blue", "purple"), lty=1)

#Determining the average number of new cases through this time period.
asia_avg <- mean(group_by_date(asia)$new_cases_per_million)
afr_avg <- mean(group_by_date(africa)$new_cases_per_million)
n_a_avg <-  mean(group_by_date(n_america)$new_cases_per_million)
s_a_avg <-  mean(group_by_date(s_america)$new_cases_per_million)
oca_avg <- mean(group_by_date(oceania)$new_cases_per_million)
eur_avg <- mean(group_by_date(europe)$new_cases_per_million)

# 1b - Creating and displaying table
tab <- as.table(rbind(afr_avg, asia_avg, eur_avg, n_a_avg, oca_avg, s_a_avg))
dimnames(tab) <- list(Continent = continents_lbls,
                      New_Cases_By_Continent = c("Average New Cases"))
tab


#PART 2 - Considering the percentage of new cases
#Function to plot the percentage for the given dataframe.
plot_percent_new <- function(df, color, first) {
  df_grouped <- group_by_date(df)
  prop_new <- (df_grouped$new_cases_per_million / df_grouped$total_cases_per_million) * 100
  if(first) {
    plot(df_grouped$`df$date`, prop_new, xlab="Date", ylab="Percentage of New Cases Per Million", type="l", col=color, main="Figure 2 - Percentage of New Cases By Continent Since Jan 30, 2023", ylim = c(0, 0.35))
  } else {
    plot(df_grouped$`df$date`, prop_new, col=color, yaxt="n", ylim = c(0, 0.35), type="l", ylab="", xlab="")
  }
}
#Calls to the function to plot each continent. 
par(new=FALSE)
plot_percent_new(asia, color="sienna1", TRUE)
par(new=TRUE)
plot_percent_new(africa, "red4", FALSE)
par(new=TRUE)
plot_percent_new(europe, "gold", FALSE)
par(new=TRUE)
plot_percent_new(n_america, "seagreen4", FALSE)
par(new=TRUE)
plot_percent_new(s_america, "purple", FALSE)
par(new=TRUE)
plot_percent_new(oceania, "blue", FALSE)
legend('topright', legend=continents_lbls, col=c("red4", "sienna1", "gold", "seagreen4", "blue", "purple"), lty=1)

