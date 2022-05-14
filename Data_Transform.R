library(tidyverse)
library (readr)


data() #to see the types of built indata sets

indata <- CO2   #importing the picked data (CO2 dataset: Carbon Dioxide Uptake inGrass Plants)
                   # Results:  84 observations and 5 variables
 
dim(indata) # this also shows the observations, in Python it is shape

head(indata) #the display

is.data.frame(indata) #checking if data frame is in matrix  
                      #Dataframe is TRUE
                      # Is it a data frame or a tibble? It's a data frame 
is_tibble(indata) #data frame is not on tibble
indata_tibble <- as_tibble(indata)
is_tibble(indata_tibble)

# Summary of the data

indata_tibble
summary(indata_tibble)


#checking for quality

cleaned_data <- na.omit(indata) #remove the missing data

dim(cleaned_data) #how many rows did it remove 

print(paste("Number of rows dropped due to missing values: ", dim(indata)[1] - dim (cleaned_data)[1]))
#notice how the row starts with 1 while in Python starts with 0.

#Results for checking quality 
#There was no missing data. This can be seen comparing indata and cleaned_data.

#checking for duplicates

duplicated(cleaned_data) #will show in boolean

sum(duplicated(cleaned_data)) #there are 0 duplicated rows

#Descriptive Statistics

#get certain stats for variables

#summary of all numeric columns

summary(cleaned_data)


cleaned_data <- as_tibble(cleaned_data) #tibble has more features than the regular base package it is enchanced dataframe,it is a table format rather than dataframe. Tibble has more information.

is_tibble(cleaned_data)#checking if an object is a table or not 

#1.Plot data by using ggplot - gives more control how we want the plot to look 

ggplot(data = cleaned_data, aes(x=uptake)) + facet_wrap(~Treatment, scales ='free_x') + geom_histogram(binwidth = function(x) 2*IQR(x)/ length(x)^(1/3))

#filter
cleaned_data %>% filter(Treatment == "chilled" ) %>% ggplot() +geom_histogram(mapping = aes(x=uptake), binwidth = function(x) 2*IQR(x)/ length(x)^(1/3)) #took dataset perform some manipulation on it and use it the resulting data object and use it for the plotting
# Boxplot
ggplot(data = cleaned_data, mapping=aes(x= Type, y= uptake)) + geom_boxplot() + coord_flip()


# arrange

cleaned_data %>% arrange(conc, desc(uptake)) %>% head()

# select 

cleaned_data %>% select(conc,uptake,Treatment, Plant) %>% head()

# group_by

cleaned_data %>% group_by(conc) %>% summarise( count=n(), avguptake = mean(uptake), stddevuptake = sd(uptake), maxuptake = max(uptake), minuptake= min (uptake))

#Arrange 

cleaned_data %>% arrange(Plant,desc(conc)) %>% head()

#Summarize
summary(cleaned_data)

