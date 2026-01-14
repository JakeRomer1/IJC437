# INTRO R SCRIPT 12/12/25

rm(list = ls())
install.packages("tidyverse")
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)
library(readxl)

# get the excel file
trains <- read_excel("~/RTraindata.xlsx")

# divide 'trains' into regions
london <- subset(trains, Operator == "Elizabeth Line" |
                   Operator == "London Overground")
north <- subset(trains, Operator == "Avanti West Coast" |
                  Operator == "Merseyrail" |
                  Operator == "Northern Rail" |
                  Operator == "TransPennine Express")
scotland <- subset(trains, Operator == "Caledonian Sleeper" |
                     Operator == "Scot Rail")
wales <- subset(trains, Operator == "TfW Rail")
midlands <- subset(trains, Operator == "Chiltern Railways" |
                     Operator == "East Midlands" |
                     Operator == "West Midlands")
south <- subset(trains, Operator == "c2c" |
                  Operator == "Govia Thameslink" |
                  Operator == "Great Western" |
                  Operator == "Greater Anglia" |
                  Operator == "South Western" |
                  Operator == "Southeastern")
long_distance <- subset(trains, Operator == "CrossCountry" |
                          Operator == "London North Eastern")
average_one <- subset(trains, Operator == "Average")

# assign each train operator their region
london$region <- "london"
north$region <- "north"
scotland$region <- "scotland"
wales$region <- "wales"
midlands$region <- "midlands"
south$region <- "south"
long_distance$region <- "long distance"
average_one$region <- "average"

# use 'rbind' to create an 'all_region' variable to be used in graphs
all_regions <- rbind(london, north, scotland, wales, midlands, south, long_distance)

# use 'rbind' to create an 'all_region_avg' variable to be used later
all_regions_avg <- rbind(london, north, scotland, wales, midlands, south, long_distance, average_one)

# code to allow graphs to be ordered by region
all_regions$Operator <- factor(
  all_regions$Operator,
  levels = all_regions$Operator[order(all_regions$region)])

# create variable including both income and expenses for dumbbell plot
inc_exp_dum <- all_regions %>%
  mutate(direction = ifelse(Total_income > Total_operating_expenditure,
                            "Income is greater",
                            "Expenditure is greater"))

# code to order the legend
inc_exp_dum$direction <- factor(
  inc_exp_dum$direction,
  levels = c("Income is greater",
             "Expenditure is greater"))

# graph 1 - dumbbell plot to display total income and total expenditure
ggplot(inc_exp_dum, aes(x = Operator)) +
  geom_segment(aes(x = Operator,
                   xend = Operator,
                   y = Total_operating_expenditure,
                   yend = Total_income,
                   colour = direction),
               linewidth = 1) +
  geom_point(aes(y = Total_operating_expenditure,
                 colour = "Expenditure"),
             shape = 16,
             size = 2) +
  geom_point(aes(y = Total_income,
                 colour = "Income"),
             shape = 16,
             size = 2) +
  scale_colour_manual(values = c("Income" = "blue",
                                 "Expenditure" = "red",
                                 "Income is greater" = "#00BFC4",
                                 "Expenditure is greater" = "#F8766D"),
                      labels = c("Income" = "Income",
                                 "Expenditure" = "Expenditure",
                                 "Income is greater" = "Income is Greater",
                                 "Expenditure is greater" = "Expenditure is Greater"),
                      breaks = c("Income",
                                 "Expenditure",
                                 "Income is greater",
                                 "Expenditure is greater"),
                      name = "Legend") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.4,
                                   hjust = 1)) +
  labs(title = "Income vs Expenditure for Each Train Operator",
       x = "Train Operator",
       y = "Amount (pencer per passenger KM)")

# create a profit group and a loss group
profit <- subset(all_regions_avg, Operator == "Avanti West Coast" |
                   Operator == "Elizabeth Line" |
                   Operator == "Greater Anglia")
loss <- subset(all_regions_avg, Operator == "Northern Rail" |
                 Operator == "Scot Rail" |
                 Operator == "TfW Rail")
average_two <- subset(all_regions_avg, Operator == "Average")
no_usage <- subset(all_regions_avg, Operator == "London Overground" |
                     Operator == "CrossCountry" |
                     Operator == "London North Eastern" |
                     Operator == "Chiltern Railways" |
                     Operator == "East Midlands" |
                     Operator == "West Midlands" |
                     Operator == "Merseyrail" |
                     Operator == "Transpennine Express" |
                     Operator == "Caledonian Sleeper" |
                     Operator == "c2c" |
                     Operator == "Govia Thameslink" |
                     Operator == "Great Western" |
                     Operator == "South Western" |
                     Operator == "Southeastern")

# assign values to use for the graph
profit$profitability <- "positive"
loss$profitability <- "negative"
average_two$profitability <- "average"
no_usage$profitability <- "no usage"

# use r bind to create 'aorta' variable that can help filter values to be used
aorta <- rbind(profit, loss, average_two, no_usage)

# create a new variable including only 'positive', 'negative' and 'average' to use in the graphs
cava <- subset(aorta, profitability == "positive" |
                 profitability == "negative" |
                 profitability == "average")

# code to order the group in a custom manner
cava$profitability <- factor(cava$profitability,
                             levels = c("positive", "average", "negative"))

# code to order graph together by the clusters I am using
cava$Operator <- factor(
  cava$Operator,
  levels = cava$Operator[order(cava$profitability)])

# create a bar chart for staff costs using 'cava'
ggplot(cava, aes(x = Operator,
                 y = Staff_costs,
                 fill = profitability)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.4,
                                   hjust = 1)) +
  scale_fill_manual(values = c("positive" = "blue",
                               "average" = "black",
                               "negative" = "red"),
                    labels = c("Profitable Train Operators",
                               "Average Staff Costs \nAcross All Train Operators",
                               "Unprofitable Train Operators"),
                    name = "Legend") +
  labs(title = "Comparison of Staff Costs Between 3 Profitable \nand 3 Unprofitable Train Operators",
       x = "Train Operator",
       y = "Staff Costs (pence per passenger KM)")

# create a bar chart for rolling stock costs using 'cava'
ggplot(cava, aes(x = Operator,
                 y = Rolling_stock_costs,
                 fill = profitability)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.4,
                                   hjust = 1)) +
  scale_fill_manual(values = c("positive" = "blue",
                               "average" = "black",
                               "negative" = "red"),
                    labels = c("Profitable Train Operators",
                               "Average Rolling Stock Costs \nAcross All Train Operators",
                               "Unprofitable Train Operators"),
                    name = "Legend") +
  labs(title = "Comparison of Rolling Costs Between \n3 Profitable and 3 Unprofitable Train Operators",
       x = "Train Operator",
       y = "Rolling Stock Costs (pence per passenger KM)")

# create a bar chart for fare income using 'cava'
ggplot(cava, aes(x = Operator,
                 y = Fare_income,
                 fill = profitability)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.4,
                                   hjust = 1)) +
  scale_fill_manual(values = c("positive" = "blue",
                               "average" = "black",
                               "negative" = "red"),
                    labels = c("Profitable Train Operators",
                               "Average Fare Income \nAcross All Train Operators",
                               "Unprofitable Train Operators"),
                    name = "Legend") +
  labs(title = "Comparison of Fare Income Between \n3 Profitable and 3 Unprofitable Train Operators",
       x = "Train Operator",
       y = "Fare Income (pence per passenger KM)")

# create a bar chart for passengers per train using 'cava'
ggplot(cava, aes(x = Operator,
                 y = Number_of_passengers_per_train,
                 fill = profitability)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.4,
                                   hjust = 1)) +
  scale_fill_manual(values = c("positive" = "blue",
                               "average" = "black",
                               "negative" = "red"),
                    labels = c("Profitable Train Operators",
                               "Average Number of \nPassengers Per Train \nAcross All Train Operators",
                               "Unprofitable Train Operators"),
                    name = "Legend") +
  labs(title = "Comparison of the Average Number of \nPassengers Per Train Between 3 Profitable \nand 3 Unprofitable Train Operators",
       x = "Train Operator",
       y = "Average Number of Passengers Per Train")

# create a bar chart for trains per KM of operation using 'cava'
ggplot(cava, aes(x = Operator,
                 y = Trains_per_KM_of_operation,
                 fill = profitability)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.4,
                                   hjust = 1)) +
  scale_fill_manual(values = c("positive" = "blue",
                               "average" = "black",
                               "negative" = "red"),
                    labels = c("Profitable Train Operators",
                               "Average Number of Trains \nPer KM of Operation \nAcross All Train Operators",
                               "Unprofitable Train Operators"),
                    name = "Legend") +
  labs(title = "Comparison of the Number of Trains Per \nKM of Operation Between 3 Profitable \nand 3 Unprofitable Train Operators",
       x = "Train Operator",
       y = "Number of Trains Per KM of Operation")

#create a bar chart for government subsidy
ggplot(cava, aes(x = Operator,
                 y = Net_franchise_subsidy,
                 fill = profitability)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.4,
                                   hjust = 1)) +
  scale_fill_manual(values = c("positive" = "blue",
                               "average" = "black",
                               "negative" = "red"),
                    labels = c("Profitable Train Operators",
                               "Average Government Subsidy \nAcross All Train Operators",
                               "Unprofitable Train Operators"),
                    name = "Legend") +
  labs(title = "Comparison of the Amount of Government \nSubsidy Between 3 Profitable and \n 3 Unprofitable Train Operators",
       x = "Train Operator",
       y = "Subsidy Amount (pence per passenger KM)")

