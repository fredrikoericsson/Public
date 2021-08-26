
####################################################################################################
#                                             R-document
####################################################################################################
# Prepared by Fredrik Eriksson
# Last updated on 18/11/2020


##################################################
# Clean the space,set the library and install packages
##################################################


#For basic manipulation
install.packages('tidyverse')
install.packages('dplyr')
install.packages("reshape")
install.packages("reshape2")
install.packages("forcats")
install.packages("stringr")
install.packages("lubridate")

#Plots and maps
install.packages('ggplot2')
install.packages("scales")
install.packages("plotly")


#For API and SQL
install.packages("httr")
install.packages("jsonlite")
install.packages("rlang")
install.packages("rjson")

# For sQL
install.packages("RODBC")

# Stat packages
install.packages("corrplot")
install.packages("MASS")

# Other
install.packages("pxweb") # SCB library
install.packages("RJSONIO")

# NLP
install.packages("wordcloud")
install.packages("topicmodels")
install.packages("RTextTools")
install.packages("e1071")
install.packages("tm")
install.packages("tidytext")
install.packages("magrittr")
install.packages("RColorBrewer")


#Install GGMap for Google API
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap")

#For basic manipulation
library(tidyverse)
library(dplyr)
library(reshape)
library(reshape2)
library(stringr)
library(forcats)
library(lubridate)

#Plots and maps
library(ggplot2)
library(scales)
library(plotly)

#For API
require("httr")
require("jsonlite")
library(rjson)
library(rlang)

# For sQL
library(RODBC)

# Stat packages
library(corrplot)
library(MASS)

# Other
library(pxweb) # SCB library
library(RJSONIO)
library(ggmap) # Google API

# NLP
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(RTextTools)
library(e1071)
library(tm)
library(tidytext)
library(magrittr)

# List objects
ls()

# Remove all objects
rm(list=ls()) 

# Set working directory
setwd('C:/Users/fredr/OneDrive/Documents/R')  

##################################################
# Read in data
##################################################

# Select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)

# Read in the CSV data and store it in a variable 
origAddress <-read.table(fileToLoad,header=T, stringsAsFactors=F, sep=";")

# Read CSV Dataset
Data1 <- read.csv("MobileAnalyticsInterviewQuestionData1.csv", header = TRUE, stringsAsFactors = FALSE) #Data sheet 1 including sales

# Pull data from SQL server
library(RODBC)
bdt<-odbcDriverConnect(connection="Driver=SQL Server; Server=Delhi; Database=BDT_Migration; UID=bdtids; PWD=bdt[IDS]A((ess;")
bdt_tablelist<-sqlTables(bdt)
wticols<-sqlColumns(bdt, 'vDataWTI')

# Create a new dataframe
test2 <- data_frame()
emptydataframe <- data.frame(stringsAsFactors = FALSE)

##################################################
# Export data
##################################################

# export the dataset with weights
write.csv(IntUserEst3, file = "Testweights.csv")

# Loop to create separate files
Prima_20132018 <- Prima %>%
  filter(FBAAR >= 2013)
str(Prima_20132018)

for(i in 2013:2018)
{
  name <- paste("Prima",i,sep="_")
  x <- Prima %>% filter(FBAAR == i)  
  # Create dataframes
  assign(name,x)
  # Export datasets
  write.csv(x, paste0("Prima", i,".csv"))
}

write.csv(Prima_20132018, "Prima_2013_2018.csv")


##################################################
# Checking dataset
##################################################

# Check dataset
summary(Data1)

# Get a little glimpse of the data
glimpse(polisen_json)

# Check first 10 rows of the dataset
head(escalator,10)

# Check variables
str(Data1)

# Check variable names
names(pirates)

# Check missing values
colSums(is.na(Data1))

# Check duplicated values
duplicated(x)

# Check true values
which(sex == "m")

# Check if the entries are numbers
is.finite(x)

# Set missing values to X
Data1$Super.Region[Data1$Super.Region=="NA"] <- "NOA" # To avoid R assuming NA as missing value

allcountries <- mutate_if(allcountries, is.numeric, funs(replace(., is.na(.), 0))) # Set NAs to 0

head(x) # Print the first few rows
tail(x)	# Print the last few rows
View(x)	# Open the entire object in a new window
nrow(x) #	Count the number of rows
ncol(x) #	Count the number of columns
dim(x) 
rownames() # Show the row names
colnames() # Show the column names
names()	


##################################################
# Variable creation and combining datasets
##################################################

# Creating new variable from scratch
employee <- c('John Doe','Peter Gynn','Jolie Hope')
salary <- c(21000, 23400, 26800)
startdate <- as.Date(c('2010-11-1','2008-3-25','2007-3-14'))

# Creating a sequence
sequence1 <- seq(from = 1, to = 10, by = 1) # Sequence from 1 to 10
sequence2 <- seq(from = 1, to = 100, by = 10)
sequence3 <- seq(from = 0, to = 100, length.out = 3) # 3 numbers with equal distance from 1 to 100 (0, 50 and 100)

# Repeat the same value
repetition1 <- rep(x = 3, times = 10) #(3 3 3 3 3 3 3 3 3 3)
repetition2 <- rep(x = c(1, 2), each = 3) #(1 1 1 2 2 2)
repetition3 <- rep(x = 1:3, length.out = 10) #(1 2 3 1 2 3 1 2 3 1)
repetition4 <- c(rep("gold", 20), rep("silver", 30), rep("bronze", 50)) # Combine different types

# Random data
set.seed(100) # set generator number
sample1 <- sample(x = 1:10, size  = 5) # 5 random numbers from 1 to 10 without replacement
sample2 <- sample(x = 1:10, size  = 5, replace = TRUE) # 5 random numbers from 1 to 10 with replacement
sample3 <- sample(x = c("a", "b"), prob = c(.9, .1), size = 10, replace = TRUE) # Setting probabilities
sample4 <- rnorm(n = 5, mean = 0, sd = 1) # normal distribution
sample5 <- runif(n = 5, min = 0, max = 1) # uniform distribution

# Create a new data frame
survey <- data.frame("index" = c(1, 2, 3, 4, 5),
                     "sex" = c("m", "m", "m", "f", "f"),
                     "age" = c(99, 46, 23, 54, 23))

# Create a matrix
matrix(data = 1:10,
       nrow = 5,
       ncol = 2)

# Set column names
colnames(polisen_df_gps5) <- c("lat", "lon")

# Create new variable from other variables
CountrySum$Net <- CountrySum$V1 / CountrySum$V2

# Create new variable based on substring of a string
Data1$YearSale <- substr(Data1$Week,1,4)

# Change variable type
escalator$Tid2 <- as.character(escalator$Tid)
escalator_model$Rost <- as.numeric(escalator_model$Rost)
property_map2$gaia_bcom_nrn2 <- as.integer(property_map2$gaia_bcom_nrn)

# Rename the variable
selection <- rename(selection, destination_country_code = destination_country_code.x)


##################################################
# Combining datasets
##################################################

# Combine datasets
Combine <- cbind(PropertySum3, DesktopSum, MobileAppSum, MobileWebSum) # Combine columns
Combine2 <- rbind(x, y) # Combine rows

# Concatenate variables
paste(as.character(escalator$Year),"-","0",as.character(escalator$Month))

# Merge two datasets
YearChartNew <- merge(YearChart,New,by="Booking.Window.Group")

df3 = merge(df1, df2, by ="ID") # inner join
df4 = merge(df1, df2, by ="ID", all.x = TRUE) # Left Join
df5 = merge(df1, df2, by ="ID", all.y = TRUE) # Right Join
df6 = merge(df1, df2, by ="ID", all = TRUE) # Full (Outer) Join
df7 = merge(df1, df2, by = NULL) # Cross Join

# Or 
all_properties <- left_join(property_view, property_map2, by = c("expe_property_id" = "expedia_id")) # Left join
all_properties <- right_join(property_view, property_map2, by = c("expe_property_id" = "expedia_id")) # Right join
all_properties <- inner_join(property_view, property_map2, by = c("expe_property_id" = "expedia_id")) # Inner join


##################################################
# Data cleaning
##################################################

# Slicing
##################################
# Get rows 3 and 24 and all columns
mtcars[c(3, 24),]

# Get column 1
mtcars[,1] 

# Row 1 to 5 and column 2
df[1:5, 2]

DesktopSum<-DesktopSum[c(3:4)]
names(DesktopSum)[1]<-"DesktopValue"

# Get rows 1 to 3 and columns 1 and 3 only
ToothGrowth[1:3, c(1,3)]

# Order data
y <- x[order(x)]

# Dplyr piping examples
###########################
y<- test %>%
    filter(Rost == 1)

selection <- filter(allcountries,
                    property_count_2017 >=1,
                    super_region_name %in% SuperRegion)

polisen_df_types <- polisen_df %>% 
  group_by(type) %>%
  summarize(incidenter = n()) %>%
  arrange(desc(incidenter))

polisen_agg_score <- polisen_df %>%
  group_by(lon, lat) %>%
  summarize(sum_score = sum(score)) %>%
  arrange(desc(sum_score))
polisen_agg_score

MC %>%
  group_by(posacountry, card_type2, year2) %>%
  summarise(NoOrders = sum(orders),
            gbv_total = sum(gbv),
            cd_total = sum(coupndiscamt)) %>%
  mutate(abv = (gbv_total - cd_total) / NoOrders) 

Referals <- Referals %>%
  select(-Email, -Email2)

# If else 
trafikolycka <- c("Trafikolycka","Trafikolycka, singel","Trafikolycka, vilt","Trafikolycka, personskada")
polisen_df$broad_type <- ifelse(polisen_df$type %in% trafikolycka,"Trafikolycka", polisen_df$type)

# Remove blanks or other text
escalator$month_year <- gsub(" ", "", escalator$month_year) 

# Subset data (dataset, variable == X)
YearChart <- subset(Data1, YearSale == "2015")
PropertySum3 <- subset(PropertySum2, V2 > 20000)
YearChart2 <- subset(YearChart, YearChart$Property.Country %in% TopCountries)

# Aggregate data
###########################
CountrySum <- aggregate(cbind(Data1$Net.Gross.Booking.Value.USD, Data1$Net.Orders) ~ Data1$YearSale + Data1$Country.Name, data = Data1, sum, na.rm = TRUE)
# or
attach(mtcars) 
#attach() function in R Language is used to access the variables present in the data framework without calling the data frame.
aggdata <-aggregate(mtcars, by=list(cyl,vs), FUN=mean, na.rm=TRUE)

# Adding a variable showing net value per order
CountrySum$Net <- CountrySum$V1 / CountrySum$V2



# Create a new variable to get the correct order of booking window
Order <- c(1,6,3,4,7,8,5,2,9,0)
Booking.Window.Group <- unique(YearChart$Booking.Window.Group)
New <- data.frame(Order, Booking.Window.Group)


New$Booking.Window.Group2<-paste0(as.character(New$Order),". ", as.character(New$Booking.Window.Group))

# Use which
IntUserEstModel <- IntUserEst3[ which(IntUserEst3$IntUsers!=''),]
nrow(IntUserEstModel[IntUserEstModel$IntUsers > 0,])

# Split a string into two variables in a dataframe
polisen_df_gps3<-strsplit(polisen_df_gps,",")
polisen_df_gps3<-unlist(polisen_df_gps3)
polisen_df_gps4  <- matrix(unlist(polisen_df_gps3), ncol=2, byrow=TRUE)
polisen_df_gps5   <- as.data.frame(polisen_df_gps4)



##################################################
# Charts
##################################################

# Create scatterplot with model
############################
plot(x = pirates$height,        # X coordinates
     y = pirates$weight,        # y-coordinates
     main = 'My first scatterplot of pirate data!',
     xlab = 'Height (in cm)',   # x-axis label
     ylab = 'Weight (in kg)',   # y-axis label
     pch = 16,                  # Filled circles
     col = gray(.0, .1))        # Transparent gray

grid() # Add gridlines

# Create a linear regression model
model <- lm(formula = weight ~ height, data = pirates)

abline(model, col = 'blue')      # Add regression to plot

# Bar charts
##############################
escalator %>% 
  filter(Plats == 1) %>%
  group_by(Rulltrappa, Year) %>%
  summarize(Personer = sum(Personer)) %>%
  ggplot() +
  geom_bar(aes(y=Personer, x=Rulltrappa), stat = "identity") + 
  scale_x_continuous(breaks = c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title="Antalet resenärer per rulltrappa \n", 
       subtitle="2017 and 2018", 
       x = "Rulltrappa i systemet",
       y = "Antalet personer under året",
       caption="Source: SL") +
  facet_wrap(~Year)

# Plot
ggplot(aes(x = posacountry, y = NoOrders, fill = card_type2)) +
  geom_bar(stat="identity", position = 'dodge') +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "1. Number of bookings made by payment method",
    subtitle = paste("Customers who made a booking",startdate, "-", enddate," 2017/18 and 2018/19."),
    x = "Country",
    y = "Number of bookings",
    fill = "Type of payment method") +
  theme(
    legend.position = "bottom",
    legend.justification = c(0, 0),
    legend.direction = "horizontal",
    #legend.key.size = unit(0.8, "cm"),
    legend.background = element_rect(colour = 'transparent', fill = NA)) +
  facet_grid(~year2)

# Create Chart1 - Largest country markets by value in 2015
theme_set(theme_classic())
Chart1 <- ggplot(YearChart, aes(Country.Name, Net.Gross.Booking.Value.USD))
Chart1 + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Largest markets by value", 
       subtitle="Total Net Gross Booking Value (USD) in 2015", 
       caption="Source: Expedia") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

# Line chart with points
ggplot(aes(x = bk_date, y = NoOrders, group=1)) +
  #geom_bar(stat="identity") +
  geom_line() +
  geom_point() +
  scale_x_discrete(breaks=c(2018-12-15,2018-12-20,2018-12-25,2018-12-30,2019-01-03)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "18. Number of bookings made with the MC coupon by day.",
    subtitle = paste("Customers who made a booking with MC coupon ",startdate, "-", enddate," 2018/19."),
    x = "Day of booking",
    y = "Number of bookings") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.justification = c(0, 0),
    legend.direction = "horizontal",
    #legend.key.size = unit(0.5, "cm"),
    legend.background = element_rect(colour = 'transparent', fill = NA)) +
  facet_grid(~ posacountry)

# Another example
POSanalysis %>%
  filter(destination_country_code %in% EMEA) %>%
  ggplot(aes(x = cl_coverage_2017_2017, y = cvr_2017,
             color = we5_flag, size = visits_2017)) +
  geom_point() + geom_text(aes(label=destination_country_code),hjust=-0.1, vjust=-1, size=3) +
  scale_x_continuous(limits = c(0.05, 0.7),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7), labels = scales::percent) +
  scale_y_continuous(limits = c(0.02, 0.1), breaks=c(0.02,0.04,0.06,0.08,0.1),labels = scales::percent) +
  #scale_y_continuous(limits = c(-0.01, 0.023), breaks=c(-0.01,0,0.01,0.02),labels = scales::percent) +
  scale_color_hcom(name = "") +
  scale_size(name = "Number of visits",labels = scales::comma) +
  scale_linetype_discrete(name = "") +
  labs(
    title = "Baseline CVR and CL coverage 2017 by destination country",
    subtitle = "All POSa. CVR for Sep 2017. CL coverage measured in Oct 2017.",
    x = "CL coverage compared to Bcom",
    y = "CVR",
    size = "Number of visits in 2017"
  ) +
  theme(
    legend.position = "bottom",
    legend.justification = c(0, 0),
    legend.direction = "horizontal",
    #legend.key.size = unit(0.8, "cm"),
    #legend.background = element_rect(colour = 'transparent', fill = NA)
  )

# Other examples
POSanalysis_int_dom_cvr %>%
  filter(destination_country_code %in% WE5) %>%
  #filter(destination_country_code %in% EMEA) %>%
  ggplot(aes(x = destination_country_code, y = value, fill = variable)) +
  geom_bar(stat="identity",position = "dodge") +
  geom_point(mapping = aes(x=destination_country_code, y=Total), size=3) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Change in CVR 2017-2018 by WE5 destination country and domestic/international POSa",
    subtitle = "All POSa included. CVR compared Sep-Sep. The dot represents total CVR change.",
    x = "Country",
    y = "Change in CVR (percentage points)",
    fill = "Origin of visits") +
  theme(
    legend.position = "bottom",
    legend.justification = c(0, 0),
    legend.direction = "horizontal",
    #legend.key.size = unit(0.8, "cm"),
    legend.background = element_rect(colour = 'transparent', fill = NA)) +
  coord_flip()

##################################################
# Loop example
##################################################

# Create a vector filled with random normal values
u1 <- rnorm(30)
print("This loop calculates the square of the first 10 elements of vector u1")

# Initialize `usq`
usq <- 0

for(i in 1:10) {
  # i-th element of `u1` squared into `i`-th position of `usq`
  usq[i] <- u1[i]*u1[i]
  print(usq[i])
}

print(i)






##################################################
# Create formula (to calculate Outliers)
##################################################
calculate_outlier <- function(vec){
  
  lower_range <- quantile(vec, na.rm = T)[2]-1.5*IQR(vec, na.rm = T)
  upper_range <- quantile(vec, na.rm = T)[4]+1.5*IQR(vec, na.rm = T)
  
  outlier_range <- c(lower_range, upper_range)
  
  return(outlier_range)
}

cvr_outlier <- list(
  cvr_2017 = calculate_outlier(selection$cvr_2017),
  cvr_2018 = calculate_outlier(selection$cvr_2018)
)

cvr_outlier


##################################################
# create a training and test dataset
##################################################


escalator_model <- escalator
head(escalator_model)

set.seed(121)  #100 used for estimations
index<-sample(1:nrow(escalator_model), size=0.8*nrow(escalator_model))
# subset train to include only the elements in the index
train<-escalator_model[index,]
# subset test to include all but the elements in the index
test<-escalator_model[-index,]


nrow(train)
nrow(test)


##################################################
# Statistics
##################################################

# T-text
t.test(formula = age ~ headband,
       data = pirates,
       alternative = 'two.sided')

# Get ANOVA table
anova(tat.sword.lm)

# Correlation
cor.test(formula = ~ height + weight,
         data = pirates)

correlations <- cor(escalator[,5:14])

corrplot(correlations, method="square")
ggcorrplot(correlations)

min(x)
max(x)
mean(x)
unique(x)
table(x)

a.z <- (a - mean(a)) / sd(a) # Standardisation

##################################################
# Logistic regression example
##################################################

# Determining weights Using logistic regression
glm.fit<-glm(Reporter~Africa+Asia+Europe+America+CIS+Arab,
             data=IntUserEst, family = binomial)

# inspect model
summary(glm.fit)

# apply model to the testing data (e.g. make predictions)
predict <- predict(glm.fit, type = 'response',data=IntUserEst)
predict2 <- 1-predict

# remove data points if they are blank
#IntUserEst2 <- IntUserEst[ which(IntUserEst$Electricity!=''),]

# append the predictions to the dataset
IntUserEst3 <- cbind(IntUserEst, predict2)


# logistic regression stepwise procedure
#####################################################################
glm.fit<-glm(Rost ~ Rulltrappa + Av_luftfuktighet + Temperatur + Plats + Medeltemperatur + Personer + Korrosivitetsklass + Sensor1 + Sensor2 + Year,
             data=train, family = binomial)


# Evaluate models:
step <- stepAIC(glm.fit, direction="both",trace = FALSE)

# Display results
step$anova 


# logistic regression stepwise determined with multiplication
####################################################################
glm.fit2<-glm(Rost~Av_luftfuktighet * Personer * Korrosivitetsklass,
              data=train, family = binomial)

# inspect model
summary(glm.fit2)

# apply model to the testing data (e.g. make predictions)
glm.probs2 <- predict(glm.fit2, type = 'response',test)
glm.pred2 <- ifelse(glm.probs2 > 0.5, "Up", "Down")

# append the predictions to the dataset
test <- cbind(test, glm.probs2)
test <- cbind(test, glm.pred2)

test %>%
  filter(Rost == 1)

#Check correct
table(glm.pred2,test$Rost)

##################################################
# Create a linear model on the training set
##################################################

# Basic model - only coverage change
summary(lm(cvr_change ~ log_cl_coverage_change_delta  
           , data = selection_model3,
           , weights = converted_2017))

# Standardised variables
LM1 <- lm(cvr_change ~ log_cl_coverage_change_delta  
          , data = selection_model3,
          , weights = converted_2017)

LM1.beta <- lm.beta(LM1)
summary(LM1.beta)


# Load stepwise Regression package
library(MASS)

# Stepwise models With Weights Regression
fit <- lm(LogitIntUsers~
            Africa+America+Asia+Europe+LDC+LIC+	
            LnPop+SmallPop+HouseholdSize+LnIncomeAtlas+FixBB+
            Network+UrbanPer+MBSubs+MYS+Tertiary+Electricity
          ,data=na.omit(train), weights=predict2)


# Stepwise models Without weights Regression
fit3 <- lm(LogitIntUsers~
             Africa+America+Asia+Europe+LDC+LIC+	
             LnPop+SmallPop+HouseholdSize+LnIncomeAtlas+FixBB+
             Network+UrbanPer+MBSubs+MYS+Tertiary+Electricity
           ,data=na.omit(train))

# Evaluate models:
step <- stepAIC(fit, direction="both",trace = FALSE)
step3 <- stepAIC(fit3, direction="both",trace = FALSE)

# Display results
step$anova 
step3$anova


# Evaluate models from Stepwise regression

# Model 1: Model from Stepwise regression LogitIntUsers weights
lin.reg1<-lm(LogitIntUsers~
               Africa + America + Asia + LIC + SmallPop + LnIncomeAtlas + 
               Network + MBSubs + MYS + Tertiary + Electricity,
             data=train, weights=predict2)

# Model 3: Model from Stepwise regression - LogitIntUsers no weights
lin.reg3<-lm(LogitIntUsers~
               Africa + SmallPop + FixBB + Network + MBSubs + 
               MYS + Electricity,
             data=train)

# inspect model
summary(lin.reg1)
summary(lin.reg3)

# apply model to the testing data (e.g. make predictions)
test.pred.lin1<-predict(lin.reg1,test)-1
test.pred.lin3<-predict(lin.reg3,test)-1

# Evaluate the models using RMSE
RMSE.lin.reg1<-sqrt(mean((test.pred.lin1-test$IntUsers)^2,na.rm=TRUE)) 
RMSE.lin.reg3<-sqrt(mean((test.pred.lin3-test$IntUsers)^2,na.rm=TRUE)) 

RMSE.lin.reg1
RMSE.lin.reg3

# Apply models and append to the test dataset

# Append the predictions to the dataset with all predictions
test_all <- test.pred.lin1 # Append the first predictions from reg1

# Append predictions from reg2-regX
test_all <- cbind(test_all , test.pred.lin3) 



# Estimate the full dataset
##


IntUserEstData <- IntUserEst[is.na(IntUserEst$IntUsers),]
nrow(IntUserEst[is.na(IntUserEst$IntUsers),])
IntUserEstData.pred.lin5<-predict(lin.reg5,IntUserEstData)-1

# append the predictions to the dataset with all predictions
IntUserEstData2 <- cbind(IntUserEstData, IntUserEstData.pred.lin5)






##################################################
# PCA
##################################################

# read Dataset
IntUserPCA <- read.csv("DatasetforR_PCA.csv")

set.seed(121)
index<-sample(1:nrow(IntUserPCA), size=0.8*nrow(IntUserEstModel))
# subset IntUserEst to include only the elements in the index
pca.train<-IntUserPCA[index,]
# subset IntUserEst to include all but the elements in the index
pca.test<-IntUserPCA[-index,]

nrow(pca.train)
nrow(pca.test)

#principal component analysis - 
#see https://www.analyticsvidhya.com/blog/2016/03/practical-guide-principal-component-analysis-python/
prin_comp <- prcomp(pca.train, scale. = T)
names(prin_comp)

#outputs the mean of variables
prin_comp$center

#outputs the standard deviation of variables
prin_comp$scale

prin_comp$rotation

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:10]

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

#scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")


#add a training set with principal components
train.data <- data.frame(Item_Outlet_Sales = train$Item_Outlet_Sales, prin_comp$x)

#we are interested in first 30 PCAs
pca.train.data <- train.data[,1:31]

#run a decision tree
install.packages("rpart")
library(rpart)
rpart.model <- rpart(Item_Outlet_Sales ~ .,data = pca.train.data, method = "anova")
rpart.model

#transform test into PCA
test.data <- predict(prin_comp, newdata = pca.test)
test.data <- as.data.frame(test.data)

#select the first 30 components
test.data <- test.data[,1:30]

#make prediction on test data
rpart.prediction <- predict(rpart.model, test.data)

#For fun, finally check your score of leaderboard
sample <- read.csv("SampleSubmission_TmnO39y.csv")
final.sub <- data.frame(Item_Identifier = sample$Item_Identifier, Outlet_Identifier = sample$Outlet_Identifier, Item_Outlet_Sales = rpart.prediction)
write.csv(final.sub, "pca.csv",row.names = F)



##################################################
# Use SCB open API
##################################################


x <- pxweb_interactive(x = "http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/")
x <- pxweb_interactive(x = "http://api.scb.se/OV0104/v1/doris/en/ssd/HE/HE0110")

pxweb_query_list <- 
  list("Region"=c("00","01","0114","0115","0117","0120","0123","0125","0126","0127","0128","0136","0138","0139","0140","0160","0162","0163","0180","0181","0182","0183","0184","0186","0187","0188","0191","0192","03","0305","0319","0330","0331","0360","0380","0381","0382","04","0428","0461","0480","0481","0482","0483","0484","0486","0488","05","0509","0512","0513","0560","0561","0562","0563","0580","0581","0582","0583","0584","0586","06","0604","0617","0642","0643","0662","0665","0680","0682","0683","0684","0685","0686","0687","07","0760","0761","0763","0764","0765","0767","0780","0781","08","0821","0834","0840","0860","0861","0862","0880","0881","0882","0883","0884","0885","09","0980","10","1060","1080","1081","1082","1083","12","1214","1230","1231","1233","1256","1257","1260","1261","1262","1263","1264","1265","1266","1267","1270","1272","1273","1275","1276","1277","1278","1280","1281","1282","1283","1284","1285","1286","1287","1290","1291","1292","1293","13","1315","1380","1381","1382","1383","1384","14","1401","1402","1407","1415","1419","1421","1427","1430","1435","1438","1439","1440","1441","1442","1443","1444","1445","1446","1447","1452","1460","1461","1462","1463","1465","1466","1470","1471","1472","1473","1480","1481","1482","1484","1485","1486","1487","1488","1489","1490","1491","1492","1493","1494","1495","1496","1497","1498","1499","17","1715","1730","1737","1760","1761","1762","1763","1764","1765","1766","1780","1781","1782","1783","1784","1785","18","1814","1860","1861","1862","1863","1864","1880","1881","1882","1883","1884","1885","19","1904","1907","1960","1961","1962","1980","1981","1982","1983","1984","20","2021","2023","2026","2029","2031","2034","2039","2061","2062","2080","2081","2082","2083","2084","2085","21","2101","2104","2121","2132","2161","2180","2181","2182","2183","2184","22","2260","2262","2280","2281","2282","2283","2284","23","2303","2305","2309","2313","2321","2326","2361","2380","24","2401","2403","2404","2409","2417","2418","2421","2422","2425","2460","2462","2463","2480","2481","2482","25","2505","2506","2510","2513","2514","2518","2521","2523","2560","2580","2581","2582","2583","2584"),
       "Fodelseregion"=c("SAMT","FSv","UtrF","NexSv","EU28exN","EuexEU28","A","NA","SA","ABB","UUU"),
       "VistelsetidUF"=c("SAMT"),
       "Alder"=c("SAMT"),
       "ContentsCode"=c("000002X8","000002XA","000002X9","000002XB"),
       "Tid"=c("2017"))

# Download data 
px_data <- 
  pxweb_get(url = "http://api.scb.se/OV0104/v1/doris/en/ssd/HE/HE0110/HE0110F/Tab5DispInkB",
            query = pxweb_query_list)

# Convert to data.frame 
px_data <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

head(px_data)

px_data %>%
  filter(region == 'Vallentuna')


##################################################
# Use Google API for geocoding
##################################################

# Geocoding a csv column of "addresses" in R

#load ggmap
library(ggmap)


#temporarily
register_google(key = 'AIzaSyBpildq0jail2BtHsIFYKlbWsH4iqa60Uk')
#permanently
register_google(key = "AIzaSyBpildq0jail2BtHsIFYKlbWsH4iqa60Uk", write = TRUE)


# Select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)

# Read in the CSV data and store it in a variable 
origAddress <-read.table(fileToLoad,header=T, stringsAsFactors=F, sep=";")

head(origAddress,5)

# Initialize the data frame
geocoded <- data.frame(stringsAsFactors = FALSE)
geocoded

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon
for(i in 1:nrow(origAddress))
{
  # Print("Working...")
  result <- geocode(origAddress$addresses[i], output = "latlona", source = "google")
  origAddress$lon[i] <- as.numeric(result[1])
  origAddress$lat[i] <- as.numeric(result[2])
  origAddress$geoAddress[i] <- as.character(result[3])
}
# Write a CSV file containing origAddress to the working directory
write.csv(origAddress, "geocoded.csv", row.names=FALSE)



##################################################
# Polisen API
##################################################

base <- "https://polisen.se/api/events"
call1 <- paste(base,"?","ticker","=", sep="")

polisen <- GET(call1)

polisen_text <- content(polisen, "text")

#Converting it from JSON to a list you can use. This actually gives you a list, one item of which is the data, with the rest is information about the API call
polisen_json <- jsonlite::fromJSON(polisen_text, flatten = TRUE)

glimpse(polisen_json)

#This grabs just the data you want and makes it a data frame
polisen_df <- as.data.frame(polisen_json)

#Check the data
glimpse(polisen_df)
head(polisen_df,1)
polisen_df$id
str(polisen_df)

polisen_df_types <- polisen_df %>% 
  group_by(type) %>%
  summarize(incidenter = n()) %>%
  arrange(desc(incidenter))

head(polisen_df_types,20)

trafikolycka <- c("Trafikolycka","Trafikolycka, singel","Trafikolycka, vilt","Trafikolycka, personskada")
stöld <- c('Stöld/inbrott','Stöld','Motorfordon, anträffat stulet','Häleri','Motorfordon, stöld','Stöld, försök','Inbrott','Inbrott, försök')
sammanfattning <- c('Sammanfattning natt','Sammanfattning kväll och natt','Sammanfattning kväll','Uppdatering')
trafikbrott <- c('Trafikkontroll','Trafikbrott','Rattfylleri','Trafikhinder','Trafikolycka, smitning från')
våldsbrott <- c('Misshandel','Rån','Vapenlagen','Mord/dråp, försök','Olaga hot','Bråk','Detonation','Rån, försök','Larm Överfall','Våld/hot mot tjänsteman','Knivlagen','Rån övrigt','Misshandel, grov','Skottlossning','Våldtäkt','Farligt föremål, misstänkt')
mindrebrott <- c('Fylleri/LOB','Skadegörelse','Narkotikabrott','Ofredande/förargelse','Bedrägeri','Alkohollagen','Åldringsbrott','Sedlighetsbrott','Anträffat gods','Skyddslagen','Olaga intrång','Polisinsats/kommendering')
olycka <- c('Arbetsplatsolycka','Försvunnen person','Fjällräddning','Räddningsinsats','Sjukdom/olycksfall')


polisen_df$broad_type <- ifelse(polisen_df$type %in% trafikolycka,"Trafikolycka", 
                                ifelse(polisen_df$type %in% stöld,"Stöld",
                                       ifelse(polisen_df$type %in% sammanfattning,"Sammanfattning",
                                              ifelse(polisen_df$type %in% trafikbrott,"Trafikbrott",
                                                     ifelse(polisen_df$type %in% våldsbrott,"Våldsbrott",
                                                            ifelse(polisen_df$type %in% mindrebrott,"Mindre brott",
                                                                   ifelse(polisen_df$type %in% olycka,"Olycka",
                                                                          polisen_df$type)))))))

#Create lon and lat coordinates
polisen_df_gps <- polisen_df$location.gps
polisen_df_gps3<-strsplit(polisen_df_gps,",")
polisen_df_gps3<-unlist(polisen_df_gps3)
polisen_df_gps4  <- matrix(unlist(polisen_df_gps3), ncol=2, byrow=TRUE)
polisen_df_gps5   <- as.data.frame(polisen_df_gps4)
colnames(polisen_df_gps5) <- c("lat", "lon")
polisen_df_gps5$lat <- as.numeric(as.character(polisen_df_gps5$lat), digits=4)
polisen_df_gps5$lon <- as.numeric(as.character(polisen_df_gps5$lon), digits=4)

# Get final dataset
polisen_df <- cbind(polisen_df,polisen_df_gps5)


# Create crime score
polisen_df$score <- 
  ifelse(polisen_df$broad_type == "Våldsbrott", 4, 
         ifelse(polisen_df$broad_type == "Stöld", 3, 
                ifelse(polisen_df$broad_type == "Trafikbrott", 2, 
                       ifelse(polisen_df$broad_type == "Mindre brott", 1,        
                              0
                       ))))

polisen_agg_score <- polisen_df %>%
  group_by(lon, lat) %>%
  summarize(sum_score = sum(score)) %>%
  arrange(desc(sum_score))
polisen_agg_score

#All of Sweden
polisen_agg_score2 <- polisen_agg_score %>%
  filter(lat>55, lat<69)
polisen_agg_score2

#Map
qmplot(lon, lat, data = polisen_agg_score2,  
       zoom = 7, maptype = "toner-lite", darken = .2, legend = "bottom") +
  geom_point(aes(size = sum_score)) +
  stat_density_2d(aes(fill =  stat(level)), geom = "polygon", alpha = .2, color = NA) +
  
  scale_fill_gradient2("Antal brott", low = "white", mid = "yellow", high = "red", midpoint = 50) 

# polisen_df %>% 
#   filter(broad_type == "Other") %>%
#   group_by(type) %>%
#   summarize(incidenter = n()) %>%
#   arrange(desc(incidenter))

# Aggregation to see total crime statistics
################################################
polisen_agg <- polisen_df %>%
  group_by(broad_type) %>%
  summarize(incidenter = n()) %>%
  arrange(desc(incidenter))

# Visualise total crime statistics
polisen_agg %>%
  ggplot() +
  geom_bar(aes(y=incidenter, x=broad_type), stat = "identity") + 
  # scale_x_continuous(breaks = c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30)) +
  # scale_y_continuous(labels = scales::comma) +
  labs(title="Kategorisering av brott \n",
       subtitle="De senaste 500 incidenter",
       x = "Typ av brott",
       y = "Antalet incidenter av de senaste 500",
       caption="Polisen")


#Filter on type of crime
#############################
polisen_df_type <- polisen_df %>%
  filter(broad_type == 'Trafikolycka'
         # | broad_type =='Stöld' | broad_type == 'Våldsbrott'| broad_type == 'Trafikbrott'
  )
polisen_df_type

# Different types of maps
?get_stamenmap
qmplot(lon, lat, data = polisen_df_type, maptype = "toner-lite", color = I("red"))

#contour map
qmplot(lon, lat, data = polisen_df_type, maptype = "toner-lite", geom = "density2d", color = I("red"))

#All of Sweden
#################################
polisen_df_type_sweden <- polisen_df_type %>%
  filter(lat>55, lat<69)

#Map
qmplot(lon, lat, data = polisen_df_type_sweden,  
       zoom = 7, maptype = "toner-lite", darken = .3, legend = "bottom") +
  stat_density_2d(aes(fill =  stat(piece)), geom = "polygon", alpha = .3, color = NA) +
  geom_point() +
  scale_fill_gradient2("Antal brott", low = "white", mid = "yellow", high = "red", midpoint = 10) 

polisen_df_type_sweden %>%
  filter(lat>67)



##################################################
# WORD CLOUD FUNCTION
##################################################
visualizeWordcloud <- function(term, freq, title = "", min.freq = 50, max.words = 200){
  mypal <- brewer.pal(8,"Dark2")
  wordcloud(words = term,
            freq = freq, 
            colors = mypal, 
            scale=c(2,.2),
            rot.per=.15,
            min.freq = min.freq, max.words = max.words,
            random.order = F)
}



##################################################
# NLP
##################################################

# Separate each text by row #  
test3 <- data_frame (text = capture.output(cat(paste(test2, collapse='\n'), '\n')))

# Separate words #  
test4 <- test3 %>%
  unnest_tokens(word, text)


# Remove stop words #  
#English
data(stop_words)

#Spanish
custom_stop_words <- bind_rows(stop_words,
                               data_frame(word = tm::stopwords("spanish"),
                                          lexicon = "custom"))

#French
custom_stop_words <- bind_rows(custom_stop_words,
                               data_frame(word = tm::stopwords("french"),
                                          lexicon = "custom"))

#Portuguese
custom_stop_words <- bind_rows(custom_stop_words,
                               data_frame(word = tm::stopwords("portuguese"),
                                          lexicon = "custom"))

#German
custom_stop_words <- bind_rows(custom_stop_words,
                               data_frame(word = tm::stopwords("german"),
                                          lexicon = "custom"))

#Extra words added manually
list_words <- data.frame("word" = c("Ã","DÃ","RÃ","ã","rã","dã","mã","null",
                                    "1","2","3","4","5","6","7","8","9","0",
                                    "gã","sã","2013","2014","2015","2016","tti"
                                    ,"2","tã","eã","l'ã","development","project","support"
                                    ,"countries","services","public","based","afdb","country"
                                    ,"program","developing","projects","provide","veloppement"
                                    ,"programs","activities","related","providing","aims"
                                    ,"projet","hands")
                         , "lexicon"=c("custom"))
list_words



custom_stop_words <- bind_rows(custom_stop_words,
                               list_words)


# remove stop words #
test4 <- test4 %>%
  anti_join(custom_stop_words)

# Count the top words #  
test4 %>%   count(word, sort = TRUE) 
word_frequencies <- test4 %>% count(word, sort=TRUE)

# Filter for the top words used #  
word_frequencies %>%
  filter(n > 80) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x= word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


# Visualise using wordcloud #
visualizeWordcloud(term = word_frequencies$word, freq = word_frequencies$n)
