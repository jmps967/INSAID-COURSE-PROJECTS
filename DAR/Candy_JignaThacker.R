# Jigna Thacker: April 2019 Batch
# Term:7:DAR : Project on Candy data

# Table of Content: -	
  # 1. Brief About the project
  # 2. Install and Load Packages
  # 3. Load the Database from local machine
  # 4. View the Database
  # 5. Sugar % and Price % 
    #  5.1 Scatterplot
    #  5.2 Scatterplot with Encircling
    #  5.3 Scatterplot with Text
  # 6. Identifying Candy Features
    #  6.1 Chocolate Bar Chart
    #  6.2 Chocolate and Caramel Bar Chart
    #  6.3 Chocolate Candy Features Grid Arrange
  # 7. Ranking with Lollipop Chart


##********************************* SECTION:1 ********************************* ##

# 1. Brief About the project
  # Dataset information : "candy-data.csv" Information collected to get the most popular Halloween candy
  # Collection Methodology : Online collection (http://walthickey.com/2017/10/18/whats-the-best-halloween-candy )
  # Attribute information : 
    # Various candy features: Chocolate , Fruity , Caramel , Peanutyalmondy , Nougat , Crispedricewafer , Hard , Bar and Pluribus
    # Various % points: Sugar % , Price % and Win %  
  # Sample : Overall 269,000 matchups were collected from 8,371 different IP addresses. 
    # CANDY-DATA.CSV contains 85 records with 9 candy features and 3 % points

##********************************* SECTION:2 ********************************* ##

# 2. Install and Load Packages
  # Below are the packages and libraries that we required to perform "Exploratory Data Analysis (EDA)"

install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggalt")
install.packages("gridExtra")
install.packages("grid")
install.packages("png")
library(dplyr)
library(ggplot2)
library(ggalt)
library(gridExtra)
library(grid)
library(png)

##********************************* SECTION:3 ********************************* ##

# 3. Load the Database from local machine

setwd("C:\\Users\\Jigna.thacker\\Documents\\Project-Term7-DAR")
getwd()
candy <- read.csv("candy-data.csv", stringsAsFactors = FALSE)

##********************************* SECTION:4 ********************************* ##

# 4. View the Database

# To View the database:-
View(candy)

# To see and view what all heads are there in the database:-
head(candy)

# To display internal structure of the dataset:-
str(candy)

# To dispaly internal structure with all data details of the database:-
summary(candy)

## Observations : There are many other variables in the dataset to compare the types of candy such as chocolate, fruity, caramel, and hard. 
#                 We will create plots on the Sugar % vs the Price % 
#                 Candy totals with respect to the Win percentage.

##********************************* SECTION:5 ********************************* ##

# 5. Sugar % and Price % 

#  5.1 Scatterplot
   # Let's see Sugar Percentage and Price Percentage Scatterplot

ggplot(data = candy, aes(x = sugarpercent, y = pricepercent  )) +
  geom_point(color='red')

## Observations:
# - This scatterplot is created to see how the amount of sugar has on the cost of candy. 
# - Each point in the plot is determined by the value of the variable on the x-axis (sugar percentage) and on the y-axis (price percentage).

#  5.2 Scatterplot with Encircling
    # Let's see Sugar Percentage and Price Percentage Scatterplot with Encircling

options(scipen = 999)

candy_select <- candy[candy$sugarpercent > 0.01 & 
                candy$sugarpercent <= 1 & 
                candy$pricepercent > 0.01 & 
                candy$pricepercent <= 0.1, ]

ggplot(candy, aes(x = sugarpercent, y = pricepercent)) + 
  geom_point(aes(col = sugarpercent, size = pricepercent)) +
  geom_smooth(method ="loess") + 
  xlim(c(0, 1)) + 
  ylim(c(0, 1)) + 
  geom_encircle(aes(x = sugarpercent, y = pricepercent), 
                data = candy_select, 
                color = "purple", 
                size = 3, 
                expand = 0.05,
                position = "identity") +
  labs(title = "Candy Power Ranking - Scatterplot with Encircling",
       subtitle="Sugarpercentage vs Pricepercentage", 
       y = "Sugarpercentage", 
       x = "Priceercentage") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

## Observations:
# - This graph is created to encircle group of points in the scattered plot to draw attention with respect to Sugarpercentage and Pricepercentage

#  5.3 Scatterplot with Text
   # Let's see Sugar Percentage and Price Percentage Scatterplot with Text

ggplot(data = candy, aes(x = sugarpercent, y = pricepercent, 
                                   label = competitorname)) +
  geom_point(color ="green") + 
  geom_smooth(method = "lm") + 
  geom_text(check_overlap = T,
            vjust = "bottom", 
            nudge_y = 0.01, 
            angle = 35,
            size = 2,
            color ="purple") +
  labs(title = "Candy Power Ranking : Scatterplot with Text",
       y = "Sugarpercentage", 
       x = "Pricepercentage") +
  theme(plot.title = element_text(hjust = 0.5))

## Observations:
# - In this graph adding the candy names to the plot will provide even more benefit to the analysis to see name of the candy wrt Sugarpercentage and Pricepercentage. 

##********************************* SECTION:6 ********************************* ##

# 6. Identifying Candy Features

# Candy Features
candy_features <- candy %>% select(2:10)
candy_features[] <- lapply(candy_features, as.logical)

#  6.1 Candy Chocolate Bar Chart

ggplot(candy_features, aes(x = chocolate)) + 
  geom_bar()

## Observation:
# - Here in this graph we can see
#   - 37 brands has chocolate feature
#   - 48 brands are non-chocolate feature
# - Like chocolate , all feature has such distribution 
# - Few Candies has more than one  feature. For eg: Tootsie Pop has chocolate , fruity and hard features.

#  6.2 Candy Chocolate and Caramel Bar Chart

ggplot(candy_features, aes(x = chocolate, fill = caramel)) + 
  geom_bar()

## Observation:
# - Here in this graph we can see:
#   - 10 brands has chocolate and Caramel combination
#   - 27 brands has non-Caramel and chocolate feature 
#   - 4 brands has Caramel and non-chocolate feature
#   - 44 brands has non-Caramel and non-chocolate feature

#  6.3 Chocolate Candy Features Grid Arrange
#      6.3.1 First we must create variables for each chocolate and feature as show below:
#      - Chocolate and bar
#      - Chocolate and caramel
#      - Chocolate and crispedricewafer
#      - Chocolate and fruity
#      - Chocolate and hard
#      - Chocolate and nougat
#      - Chocolate and peanutyalmondy
#      - Chocolate and pluribus

chocolate_bar <- ggplot(candy_features, aes(x = chocolate, fill = bar)) + 
  geom_bar() + scale_fill_manual(values = c('navy', 'orangered2'))

chocolate_bar

chocolate_caramel <- ggplot(candy_features, aes(x = chocolate, fill = caramel)) + 
  geom_bar() + scale_fill_manual(values = c('navy', 'orangered2'))

chocolate_caramel

chocolate_crispedricewafer <- ggplot(candy_features, aes(x = chocolate, fill = crispedricewafer)) + 
  geom_bar() + scale_fill_manual(values = c('navy', 'orangered2'))

chocolate_crispedricewafer

chocolate_fruity <- ggplot(candy_features, aes(x = chocolate, fill = fruity)) + 
  geom_bar() + scale_fill_manual(values = c('navy', 'orangered2'))

chocolate_fruity

chocolate_hard <- ggplot(candy_features, aes(x = chocolate, fill = hard)) + 
  geom_bar() + scale_fill_manual(values = c('navy', 'orangered2'))

chocolate_hard

chocolate_nougat <- ggplot(candy_features, aes(x = chocolate, fill = nougat)) + 
  geom_bar() + scale_fill_manual(values = c('navy', 'orangered2'))

chocolate_nougat

chocolate_peanutyalmondy <- ggplot(candy_features, aes(x = chocolate, fill = peanutyalmondy)) + 
  geom_bar() + scale_fill_manual(values = c('navy', 'orangered2'))

chocolate_peanutyalmondy

chocolate_pluribus <- ggplot(candy_features, aes(x = chocolate, fill = pluribus)) + 
  geom_bar() + scale_fill_manual(values = c('navy', 'orangered2'))

chocolate_pluribus

#      6.3.2 Grid arrange:

chocolate_features_grid <- grid.arrange(chocolate_bar, chocolate_caramel, chocolate_crispedricewafer,
                                        chocolate_fruity, chocolate_hard, chocolate_nougat,  
                                        chocolate_peanutyalmondy, chocolate_pluribus, 
                                        top = "Chocolate Candy Features Grid Arrange",
                                        ncol = 2, nrow = 4)

chocolate_features_grid


##********************************* SECTION:7 ********************************* ##

# 7. Ranking with Lollipop Chart

candy_order <- candy[order(candy$winpercent, rev(candy$winpercent), decreasing = TRUE), ]

head(candy_order)

# Identify top 25 Candy:-

candy_top_25 <- head(candy_order, 25)

head(candy_top_25)

# Pictorial presentaion of top 25 Candy:-

  reeses_background <- png::readPNG("candy_power_ranking_reeses_peanut_butter_cup.png")

ggplot(candy_top_25,  aes(x = reorder(competitorname, sort(as.numeric(winpercent))), y = winpercent)) +
  annotation_custom(rasterGrob(reeses_background, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_point(size = 4) + 
  geom_segment(aes(x = competitorname, 
                   xend = competitorname, 
                   y = 0, 
                   yend = winpercent)) +
  labs(title = "Candy Power Ranking with Lollipop Chart",
       y = "Win Percentage", 
       x = "Candy Names",
       subtitle = "Top 25 Candy Power Rankings") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

##********************************* THANK YOU ********************************* ##
