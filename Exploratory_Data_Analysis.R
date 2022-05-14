

library(tidyverse)

indata <- read.csv("C:\\Users\\josel\\Documents\\Downloads\\grad-students.csv")

is.data.frame(indata)
is_tibble(indata)

indata_tibble <- as_tibble(indata)
is_tibble(indata_tibble)

indata_tibble
summary(indata_tibble)


indata_tibble$Grad_unemployed = factor(indata_tibble$Grad_unemployed)
indata_cleaned <-select(indata_tibble,-(Grad_P25:Grad_premium))
summary(indata_cleaned)

indata_filled <- indata_cleaned %>% group_by(Grad_unemployed) %>% fill(Grad_unemployment_rate, .direction="downup") %>% ungroup()

indata_filled %>% filter(is.na(Grad_unemployment_rate)) %>% group_by(Grad_unemployed) %>% summarise(count=n())

indata_filled <- indata_filled %>% filter(!is.na(Grad_unemployment_rate))

#checking for quality

cleaned_data <- na.omit(indata_tibble) #remove the missing data

dim(cleaned_data) #how many rows did it remove 

print(paste("Number of rows dropped due to missing values: ", dim(indata)[1] - dim (cleaned_data)[1]))
#notice how the row starts with 1 while in Python starts with 0.

#Results for checking quality 
#There were 0 rows that were dropped because they had missing values.

#checking for duplicates

duplicated(cleaned_data) #will show in boolean

sum(duplicated(cleaned_data)) #there are 0 duplicated rows



#Question
#Are the number Grads Unemployed and Grad Unemployment rate related to each other?

ggplot(data = indata_filled) + geom_point(mapping =aes(x=Grad_unemployed, y =Grad_unemployment_rate), position = "jitter", alpha = 0.1) + geom_smooth(mapping = aes(x=Grad_unemployed, y =Grad_unemployment_rate)) + labs(x="Grad Unemployment", y= "Grad Unemployment Rate", title ="# of Grad Unemployment vs. Unemployment Rate ") 

#Results
#Data points of Grad unemployment rate vs Number of grads unemployed are scattered, that means that there is no linear correlation between the two variables.

#Question
#Does unemployment in grads occur often?

ggplot(data = indata_tibble) + geom_point(mapping = aes (x= Grad_total, y=Grad_unemployed)) + labs(x="Grad total", y="Grad unemployed", title="Grads unemployed vs Grad total")

#Results
#As the number of grads increase, the number of grads unemployed also increase; highest unemployment number is 35,718 with about 1,184,158 total students 


#Question
##What does the probability function of grads employed look like?
ggplot(data= indata_tibble)+ geom_histogram(mapping = aes(x= Grad_employed), binwidth =  50000)

#Results
#Histogram shows a decaying probability distribution for grads employed. 
