#Name: Shruti B. Kamble
#Course: Intermediate Analytics
#Title: Chi Square and ANOVA
#Date: 04/24/2021


#State the hypotheses and identify the claim.
#Find the critical value.
#Compute the test value.
#Make the decision.
#Summarize the results.

#Section 11-1

#A medical researcher wishes to see if hospital patients in a large hospital have the same blood type distribution as those in the general population. The distribution for the general population is as follows: type A, 20%; type B, 28%; type O, 36%; and type AB = 16%. He selects a random sample of 50 patients and finds the following: 12 have type A blood, 8 have type B, 24 have type O, and 6 have type AB blood.

#At α = 0.10, can it be concluded that the distribution is the same as that of the general population?
#Blood types
#creating vectors
data <- tibble(pop_dist = c(0.2, 0.28, 0.36, 0.16),
                      obs_samp = c(12, 8, 24, 6),
                      exp_n = 50 * pop_dist)

#stating the hypothesis
#H0: Blood type distribution is same as general population
#H1: Blood type distribution is not same as stated in null hypotheses

exp_n <- 50
alpha <- 0.10
LoSig1 <- 1- alpha
k <- nrow(data)
deg_freedom <- k-1

#Critical value
Critical_Value <-qchisq(p= LoSig1, deg_freedom, lower.tail=TRUE)
cat("Critical value = ",Critical_Value)

#test value
chisq01 =chisq.test(data$obs_samp,
                  p = data$pop_dist,  ## Values in Probability 
                  correct = FALSE) # not to apply continuity correction

cat("Chi-Square Statistic = ",chisq01$statistic)


-------------------------------------------------------------------------------------------------
  
#According to the Bureau of Transportation Statistics, on-time performance by the airlines is described as follows:
  
#Action	% of Time
#On time	70.8
#National Aviation System delay	8.2
#Aircraft arriving late	9.0
#Other (because of weather and other conditions)	12.0
#Records of 200 randomly selected flights for a major airline company showed that 125 planes were on time; 40 were delayed because of weather, 10 because of a National Aviation System delay, and the rest because of arriving late. At α = 0.05, do these results differ from the government’s statistics?

#on-time performance by airlines
#Goodness of Fit
Air_Per <- data.frame(Action = c("On time","National Aviation System delay","Aircraft arriving late","Other Conditions"),
                         percentage_time = c(70.8,8.2,9.0,12.0),
                         observed_airlines = c(125,10,NA,40),
                      record = 200)

Record <- 200
alpha_01 <- 0.05
LoSig <- 1- alpha_01
k <- nrow(Air_Per)
deg_freedom <- k-1


#Computing missing value(Aircraft arriving late)
Air_Per$observed_airlines[3] <- Record - sum(Air_Per$observed_airlines, na.rm = TRUE)
Air_Per

#General values are in Percentage and Observed values are in count. 
Air_Per$percentage_time <- Air_Per$percentage_time/100
Air_Per

#Stating the hypothesis
#H0: The Observed distribution is the same as that of the government stats
#H1: The Observed distribution is the different as that of the government stats

#Critical value
critical_value <- qchisq(p = LoSig, deg_freedom, lower.tail = TRUE)
cat("Critical Value = " , critical_value)

#test value
Result_01 <- chisq.test(x = Air_Per$observed_airlines, p = Air_Per$percentage_time, correct = FALSE)
cat("Chi-Square Statistic = ",Result_01$statistic)


# Comparing p-value to alpha
ifelse(Result_01$p.value > alpha_01, "Retain the null hypothesis", "Reject the null hypothesis") 

---------------------------------------------------------------------------------------------------------

#Section 11-2
  
 # Are movie admissions related to ethnicity? A 2014 study indicated the following numbers of admissions (in thousands) for two different years. At the 0.05 level of significance, can it be concluded that movie attendance by year was dependent upon ethnicity?
  
# Caucasian	Hispanic	African American	Other
#2013	724	335	174	107
#2014	370	292	152	140
  
#Ethnicity and movie admissions
Movie_Admissions <- data.frame(Caucasian =c(724,370),
                           Hispanic=c(335,292),
                           African_American =c(174,152),
                           Other =c(107,140),
                           row.names = c(2013,2014)
                           )

alpha_02   = 0.05
LoSig_02   = 1- alpha_02
deg_freedom_02  = (nrow(Movie_Admissions) -1)*(ncol(Movie_Admissions) -1 )

#stating the hypothesis
#H0: The movie attendance is independent to ethnicity
#H1: The movie attendance is dependent upon ethnicity

#Critical Value
critical_value_02 <-round(qchisq(p= LoSig_02, deg_freedom_02), 3)  
cat("Critical value =  ", critical_value_02)

#test value
Result_02 = chisq.test(Movie_Admissions)
cat("The computed p-value is: ",Result_02$p.value, " and the alpha is, ",alpha_02)

# Comparing p-value to alpha
ifelse(Result_02$p.value > alpha_02, "Retain the null hypothesis", "Reject the null hypothesis") 

-----------------------------------------------------------------------------------------------

  #This table lists the numbers of officers and enlisted personnel for women in the military. At α = 0.05, is there sufficient evidence to conclude that a relationship exists between rank and branch of the Armed Forces?
  
  #Action	Officers	Enlisted
#Army	10,791	62,491
#Navy	7,816	42,750
#Marine Corps	932	9,525
#Air Force	11,819	54,344

#Women in the military
Military <- data.frame(Officers =c(10791,7816, 932, 11819),
                            Enlisted  =c(62491,42750,9525, 54344))

alpha_03 <- 0.05
LoSig_03 <- 1- alpha_03
deg_freedom_03  = (nrow(Military) -1)*(ncol(Military) -1 )

#Stating the hypothesis
#H0:There is a relationship between ranks and branch of armed forces
#H1: There is no relationship between ranks and branch of armed forces

#Critical value
critical_value_03 <-round(qchisq(p=LoSig_03, deg_freedom_03), 3) 
cat("Critical value = ", critical_value_03)

#test value
Result_03 = chisq.test(Military)
cat("Chi-Square Statistic = ", Result_03$statistic)

#cross-verifying using the p-value
cat("p-value = ",Result_03$p.value, " and alpha = ",alpha_03)

--------------------------------------------------------------------------------------------------
  
#Section 12-1
  
 # The amount of sodium (in milligrams) in one serving for a random sample of three different kinds of foods is listed. At the 0.05 level of significance, is there sufficient evidence to conclude that a difference in mean sodium amounts exists among condiments, cereals, and desserts?
  
 # Condiments	Cereals	Desserts
#270	260	100
#130	220	180
#230	290	250
#180	290	250
#80	200	300
#70	320	360
#200	140	300
#       160

#Sodium contents of food
sodium_contents <- data.frame(Condiments <- c(270,130, 230, 180, 80, 70, 200,NA),
                          Cereals  = c(260,220,290,290,200,320,140,NA),
                          Desserts = c(100,180,250,250,300,360,300, 160))

sodium_contents <- sodium_contents %>% gather(key = food_types, value = sodium_contents, na.rm = TRUE)
sodium_contents

alpha_04 <- 0.05
LoSig_04 <- 1- alpha_04
deg_freedom_04 <- (nrow(sodium_contents) - 1)*(ncol(sodium_contents)-1)


#H0:The mean of Sodium content among condiments, cereals and desserts are same
#H1:The mean is different of sodium content among condiments, cereals and desserts


#ANOVA test
Anova_test <- oneway.test(sodium_contents ~ food_types, data = sodium_contents, var.equal = TRUE, na.action = "na.omit")
cat("ANOVA p-value = ", Anova_test$p.value)

--------------------------------------------------------------------------------------------------------------------

#Section 12-2
 # The sales in millions of dollars for a year of a sample of leading companies are shown. At α = 0.01, is there a significant difference in the means?
  
  #Cereal	Chocolate Candy	Coffee
#578	311	261
#320	106	185
#264	109	302
#249	125	689
#237	173	
  
#Sales for leading companies
Sales <- data.frame(Cereals          <- c(578,320,264,249,237),
                    Chocolate.Candy  <- c(311,106,109,125,173),
                    Coffee           <- c(261,185,302,689,NA)
                    )
Sales <- Sales %>% gather(key = food_types, value = Sales, na.rm = FALSE)
Sales

alpha_05 <- 0.01

#H0:The mean of all food types are equal
#H1:There is significant difference in the mean

#Running ANOVA test
Anova_test_01 <- aov(Sales ~ food_types, data = Sales)
summary(Anova_test_01)
TukeyHSD(Anova_test_01)

--------------------------------------------------------------------------------------
 # The expenditures (in dollars) per pupil for states in three sections of the country are listed. Using α = 0.05, can you conclude that there is a difference in means?
  
 # Eastern third	Middle third	Western third
#4946	6149	5282
#5953	7451	8605
#6202	6000	6528
#7243	6479	6911
#6113		

#Per-Pupil Expenditures
Eastern_third <- c(4946,5953,6202,7243,6113)
Middle_third <- c(6149,7451,6000,6479)
Western_third <- c(5282,8605,6528,6911)

#Creating matrix
Expenditures <- matrix(c(rep("Eastern_third", 5), 
                   rep("Middle_third", 4), 
                   rep("Western_third", 4),
                   Eastern_third , 
                   Middle_third, Western_third),
                 ncol=2)

Expenditures <- data.frame(Expenditures, stringsAsFactors = FALSE)

#Renaming columns
names(Expenditures) <- c("Region", "Expenditures")

alpha_06 <- 0.05

#H0: The mean of all variables is same
#H1:There is a difference in mean of the three variables

Anova_test_02 <- aov(Expenditures ~ Region, data = Expenditures)
summary(Anova_test_02)

--------------------------------------------------------------------------------------------
 # A gardening company is testing new ways to improve plant growth. Twelve plants are randomly selected and exposed to a combination of two factors, a “Grow-light” in two different strengths and a plant food supplement with different mineral supplements. After a number of days, the plants are measured for growth, and the results (in inches) are put into the appropriate boxes.

#Grow-light 1	Grow-light 2
#Plant food A	9.2, 9.4, 8.9	8.5, 9.2, 8.9
#Plant food B	7.1, 7.2, 8.5	5.5, 5.8, 7.6
#Can an interaction between the two factors be concluded? Is there a difference in mean growth with respect to light? With respect to plant food? Use α = 0.05.

  #Increasing of Plant Growth
  
Plant_Growth <- data.frame(Plant_group = c("A","A","A","A","A","A","B","B","B","B","B","B"),
                           Plant_Group_Growth=c(9.2, 9.4, 8.9,8.5, 9.2, 8.9,7.1, 7.2, 8.5,5.5, 5.8, 7.6))   

Sample <- nrow(Plant_Growth)
alpha <- 0.05
k       = n_distinct(Plant_Growth$Plant_group)  ## Number of Observations
Df_N    = k-1
Df_D    = Sample- k

#H0:The mean of group A and B is same
#H1:There is significant difference in the mean of group

#Critical value
critical_value_04 <- qf(p= alpha, df1 = Df_N, df2 = Df_D, lower.tail = FALSE )
cat("F_Critical value = ",critical_value_04)  

#test value
f_test <- var.test(Plant_Group_Growth ~ Plant_group, data = Plant_Growth, alternative = "two.sided")
f_test <- summary(aov(Plant_Group_Growth ~ Plant_group, data = Plant_Growth))
cat("F_statistic value = ",f_test[[1]][["F value"]][[1]])


--------------------------------------------------------------------------------------------------

#On Your Own
#1. Download the file ‘baseball.csv’ from the course resources and import the file into R.
library(readr)
library(tidyverse)
library(dplyr)
baseball <- read_csv("DataSet/baseball.csv")
baseball <- as.data.frame(read.csv(file = "DataSet/baseball.csv"))
headTail(baseball,top = 5,bottom = 5)

#2. Perform EDA on the imported data set.
#Write a paragraph or two to describe the data set using descriptive statistics and plots.
#Are there any trends or anything of interest to discuss?
str(baseball)
summary(baseball)
table <- psych::describe(baseball)
write.csv(table, "Table.csv")

Teams <- as.factor(baseball$Team)
League <- as.factor(baseball$League)

#Plot
boxplot(baseball$RS ~ baseball$W, xlab = "W", ylab = "RS", main = "Baseball Data")

colnames(baseball)

#Assuming the expected frequencies are equal, perform a Chi-Square Goodness-of-Fit test to determine if there is a difference in the number of wins by decade. 
#Be sure to include the following:

baseball$Decade <- baseball$Year - (baseball$Year %% 10)

Win <- baseball %>% dplyr::group_by(baseball$Decade) %>% summarise(Win = sum(baseball$W)) %>% as.tibble()

alpha   <- 0.05
LoSig_05   <- 1- alpha
k_05       <- nrow(Win) ## Number of Observations
Deg_Freedom_05  <- k_05 -1

#Assuming the expected frequencies are equal 1/6th
Win$Expected <- 1/k_05
Win

#3. 
#a. State the hypotheses and identify the claim
#H0:The Observed distribution is the same as that of the general population
#H1:The Observed distribution is different as that of the general population

#b. Critical value
Critical_Val <-qchisq(p=LoSig_05, Deg_Freedom_05, lower.tail=TRUE)
cat("Critical value = ",Critical_Val)

#c. test value

chisq =chisq.test(Win$Win,
                  p = Win$Expected,  ## Values in Probability 
                  correct = FALSE) # not to apply continuity correction

cat("Chi-Square Statistic =  ",chisq$statistic)
cat("p-value = ",chisq$p.value)


#4. Download the file ‘crop_data.csv’ from the course resources and import the file into R.
library(readr)
crop_data_1 <- read_csv("DataSet/crop_data-1.csv")
crop_data <- as.data.frame(read_csv("DataSet/crop_data-1.csv"))
headTail(crop_data, top = 5, bottom = 5)

#5. Perform a Two-way ANOVA test using yield as the dependent variable and fertilizer and density as the independent variables. 
#Explain the results of the test. Is there reason to believe that fertilizer and density have an impact on yield?

str(crop_data)
summary(crop_data)

# H0: Fertilizer and crop does not have impact on yield
# HA: Fertilizer and crop have impact on yield

alpha_07 <- 0.05
colnames(crop_data)

#Plot
colnames(crop_data)
ggplot(data = crop_data, aes(x = Fertilizer, y = Yield, fill = Crop))+ geom_boxplot()+ stat_boxplot(geom = "errorbar")

#Two-way ANOVA test
Anova = summary(aov(Yield ~ Fertilizer + Crop, data = crop_data))
cat("p-value of fertilizer = ", Anova[[1]][["Pr(>F)"]][[1]], "and crop = ", Anova[[1]][["Pr(>F)"]][[2]])




