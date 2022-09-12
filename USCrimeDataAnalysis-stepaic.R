# US Crime Data Analysis
#load Mass library
library(MASS)
#load dplyr to do data modifications
library(dplyr)
#load lattice to run splom plot
library(lattice)
#attach the UScrime dataset
attach(UScrime)
#pdf(file="UScrimedataanalysis.pdf", pointsize=9)
options(width=65, digits=5)

#Rename the variables to meaningful names
names(UScrime)<-c("Percent_Male","is_South","mean_Education","pol_exp60","pol_exp59","labour_fp","no_mal_1000_f","state_pop",
                  "nonWhites_per1000","unemploy_urb_m24","unemploy_urb_m39","gdp","inequality","prob_imp","time_state_pri","crime_rate")

#write the original dataset to csv file
#write.csv(UScrime,file = "USCrimeDataSet.csv")
#Remove the variable is_south categorical variable
#Uscrime.new<-subset(UScrime,select = -c(is_South))
#Uscrime.new1<-UScrime

uscrime.fac<-UScrime
uscrime.fac$is_South<-as.factor(UScrime$is_South)
str(uscrime.fac)



#Explore the correlation between each individual varaibles
UScrime.cor<-cor(UScrime)
UScrime.cor
#write the correlation to csv file
#write.csv(UScrime.cor,file = "USCrimeCorrelation.csv")


#Since the correlation between pol_exp60 and pol_exp59 is almost equal to 1 combine both to a single variable
UScrime.dt <- UScrime %>%
  mutate(pol_exp = pol_exp60+pol_exp59) %>% 
  select(-c(pol_exp60,pol_exp59))
UScrime.dt
par(mfrow=c(2,2))
#Data Verification for outliers and cleaning data
plot(UScrime.dt$pol_exp,UScrime.dt$crime_rate)

boxplot(UScrime.dt$pol_exp)

#Clean data by removing outliers
UScrime.data_clean<-UScrime.dt %>% filter(pol_exp<320)

UScrime.dt.modint<-aov(crime_rate~pol_exp*gdp*prob_imp*state_pop,data = UScrime.data_clean)
summary(UScrime.dt.modint)

uscrime.dt.mlm<-lm(crime_rate~pol_exp*gdp*prob_imp*state_pop,data = UScrime.data_clean)
summary(uscrime.dt.mlm)

stepAIC(uscrime.dt.mlm,direction = "both")

uscrimestepaic1<-stepAIC(uscrime.dt.mlm,direction = "both")
uscrimestepaic1$anova
uscrimestepaic1


UScrime.dt.modint.m1<-lm(crime_rate~pol_exp*gdp*prob_imp*state_pop - (gdp:state_pop),data = UScrime.data_clean)
summary(UScrime.dt.modint.m1)
stepAIC(UScrime.dt.modint.m1,direction = "both")
uscrimestepaic2<-stepAIC(UScrime.dt.modint.m1,direction = "both")
uscrimestepaic2$anova
uscrimestepaic2

anova(uscrime.dt.mlm,UScrime.dt.modint.m1)
anova(uscrimestepaic1,uscrimestepaic2)


UScrime.dt.modint.m2<-lm(crime_rate~pol_exp*gdp*prob_imp*state_pop - (gdp:state_pop) - (gdp),data = UScrime.data_clean)
summary(UScrime.dt.modint.m2)
stepAIC(UScrime.dt.modint.m2,direction = "both")
uscrimestepaic3<-stepAIC(UScrime.dt.modint.m2,direction = "both")
uscrimestepaic3$anova
uscrimestepaic3

anova(uscrime.dt.mlm,UScrime.dt.modint.m1,UScrime.dt.modint.m2)


library(lmtest)

lrtest(uscrime.dt.mlm, UScrime.dt.modint.m1,UScrime.dt.modint.m2)
lrtest(uscrimestepaic1,uscrimestepaic2)



uscrime.dt.mlm2<-glm(crime_rate~pol_exp*gdp*prob_imp*state_pop,data = UScrime.data_clean)
summary(uscrime.dt.mlm2)

uscrime.dt.mlm2aic<-stepAIC(uscrime.dt.mlm2,direction = "both")
uscrime.dt.mlm2aic$anova
dropterm(uscrime.dt.mlm2aic, test = "Chisq")

uscrime.dt.mlm3aic <-update(uscrime.dt.mlm2aic, . ~ pol_exp*gdp*prob_imp*state_pop,data - gdp:state_pop)
anova(uscrime.dt.mlm2aic,uscrime.dt.mlm3aic,test="Chisq")


#Plot the best model
par(mfrow=c(2,2))
plot(uscrime.dt.mlm)

#Model Validation
#Plot Histogram of residuals
hist(uscrime.dt.mlm$residuals,breaks = 5)
#Run Shapiro test
shapiro.test(uscrime.dt.mlm$residuals)

shapiro.test(UScrime.data_clean)

#Plot the residuals
plot(UScrime.data_clean$crime_rate, uscrime.dt.mlm$residuals)
abline(h = 0, col = "green")

UScrime.dt.modint<-aov(crime_rate~pol_exp*gdp*prob_imp*state_pop,data = UScrime.data_clean)
summary(UScrime.dt.modint)

