# US Crime Data Analysis
#load Mass library
library(MASS)
#load dplyr to do data modifications
library(dplyr)
#load lattice to run splom plot
library(lattice)
#attach the UScrime dataset
attach(UScrime)
pdf(file="UScrimedataanalysis.pdf", pointsize=9)
options(width=65, digits=5)

#Rename the variables to meaningful names
names(UScrime)<-c("Percent_Male","is_South","mean_Education","pol_exp60","pol_exp59","labour_fp","no_mal_1000_f","state_pop",
                  "nonWhites_per1000","unemploy_urb_m24","unemploy_urb_m39","gdp","inequality","prob_imp","time_state_pri","crime_rate")

#write the original dataset to csv file
#write.csv(UScrime,file = "USCrimeDataSet.csv")
#Remove the variable is_south categorical variable
Uscrime.new<-subset(UScrime,select = -c(is_South))

#Explore the correlation between each individual varaibles
UScrime.cor<-cor(Uscrime.new)
UScrime.cor
#write the correlation to csv file
#write.csv(UScrime.cor,file = "USCrimeCorrelation.csv")


#Since the correlation between pol_exp60 and pol_exp59 is almost equal to 1 combine both to a single variable
UScrime.dt <- Uscrime.new %>%
  mutate(pol_exp = pol_exp60+pol_exp59) %>% 
  select(-c(pol_exp60,pol_exp59))
UScrime.dt

#Data Verification for outliers and cleaning data
plot(UScrime.dt$pol_exp,UScrime.dt$crime_rate)

boxplot(UScrime.dt$pol_exp)

#Clean data by removing outliers
UScrime.data_clean<-UScrime.dt %>% filter(pol_exp<320)

boxplot(UScrime.data_clean$pol_exp)

plot(UScrime.data_clean$pol_exp,UScrime.data_clean$crime_rate)

#Simple Regression crime rate vs Police Expenditure of 2 years with and without outlier
UScrime.pol_o.lm<-lm(crime_rate~pol_exp,data = UScrime.dt)
UScrime.pol.lm<-lm(crime_rate~pol_exp,data = UScrime.data_clean) # without outlier

plot(UScrime.dt$pol_exp, UScrime.dt$crime_rate)
abline(UScrime.pol_o.lm$coefficients[1],UScrime.pol_o.lm$coefficients[2], col = "red")
abline(UScrime.pol.lm$coefficients[1],UScrime.pol.lm$coefficients[2], col = "blue") # without outlier

#Summary with outlier
summary(UScrime.pol_o.lm)

#Summary without outlier
summary(UScrime.pol.lm)

#Run Pairs
pairs(UScrime.data_clean,upper.panel = NULL)


#Splom Plot to explore the correlation
splom(~ UScrime.data_clean, aspect = "fill",
      panel = function(x, y, ...) {
        panel.xyplot(x, y, ...); panel.loess(x, y, ...)
      }
)
#Based on splom the 4 parameters have correlation to the dependent variable crime rate. subset of splom is performed on the four
UScrime.splom<-subset(UScrime.data_clean,select = c(state_pop,gdp,prob_imp,pol_exp,crime_rate))
UScrime.splom
#Perform splome plot on this subset
#Splom Plot to explore the correlation
splom(~ UScrime.splom, aspect = "fill",
      panel = function(x, y, ...) {
        panel.xyplot(x, y, ...); panel.loess(x, y, ...)
      }
)

#Simple Regression on the 4 independent variables selected based on the splom plot
# Simple Regression for crime rate vs state population
UScrime.sp.lm<-lm(crime_rate~state_pop,data = UScrime.data_clean)
summary(UScrime.sp.lm)
anova(UScrime.sp.lm)

# Simple Regression for crime rate vs gdp
UScrime.gdp.lm<-lm(crime_rate~gdp,data = UScrime.data_clean)
summary(UScrime.gdp.lm)
anova(UScrime.gdp.lm)

# Simple Regression for crime rate vs probability of imprisonment
UScrime.probimp.lm<-lm(crime_rate~prob_imp,data = UScrime.data_clean)
summary(UScrime.probimp.lm)
anova(UScrime.probimp.lm)

# Simple Regression for crime rate vs Police expense for the 2 years 1959 and 1960 combined
UScrime.polex.lm<-lm(crime_rate~pol_exp,data = UScrime.data_clean)
summary(UScrime.polex.lm)
anova(UScrime.polex.lm)

#Now we will run Anova for multiple models
#model 1 crime rate vs police expenditure+gdp
UScrime.dt.mod1<-lm(crime_rate~pol_exp+gdp,data = UScrime.data_clean)
summary(UScrime.dt.mod1)
anova(UScrime.dt.mod1)

#model 2 crime rate vs police expenditure+prob_imp
UScrime.dt.mod2<-lm(crime_rate~pol_exp+prob_imp,data = UScrime.data_clean)
summary(UScrime.dt.mod2)
anova(UScrime.dt.mod2)

#model 3 crime rate vs police expenditure+state_pop
UScrime.dt.mod3<-lm(crime_rate~pol_exp+state_pop,data = UScrime.data_clean)
summary(UScrime.dt.mod3)
anova(UScrime.dt.mod3)

#model 4 crime rate vs gdp+prob_imp
UScrime.dt.mod4<-lm(crime_rate~gdp+prob_imp,data = UScrime.data_clean)
summary(UScrime.dt.mod4)
anova(UScrime.dt.mod4)

#model 5 crime rate vs gdp+state_pop
UScrime.dt.mod5<-lm(crime_rate~gdp+state_pop,data = UScrime.data_clean)
summary(UScrime.dt.mod5)
anova(UScrime.dt.mod5)

#model 6 crime rate vs prob_imp+state_pop
UScrime.dt.mod6<-lm(crime_rate~prob_imp+state_pop,data = UScrime.data_clean)
summary(UScrime.dt.mod6)
anova(UScrime.dt.mod6)

#Find best model on the 2 parameters
anova(UScrime.dt.mod1,UScrime.dt.mod2,UScrime.dt.mod3,UScrime.dt.mod4,UScrime.dt.mod5,UScrime.dt.mod6)

#Based on AIC
library(AICcmodavg)
model.set<-list(UScrime.dt.mod1,UScrime.dt.mod2,UScrime.dt.mod3,UScrime.dt.mod4,UScrime.dt.mod5,UScrime.dt.mod6)
model.names<-c("1,2","1,3","1,4","2,3","2,4","3,4")
aictab(model.set,modnames = model.names)

#Run anova for 3 parameters
# Model 7 - crime rate vs Police expenditure + Gdp+probability of imprisonment
UScrime.dt.mod7<-lm(crime_rate~pol_exp+gdp+prob_imp,data = UScrime.data_clean)
summary(UScrime.dt.mod7)
anova(UScrime.dt.mod7)

# Model 8 - crime rate vs Police expenditure + Gdp+state of imprisonment
UScrime.dt.mod8<-lm(crime_rate~pol_exp+gdp+state_pop,data = UScrime.data_clean)
summary(UScrime.dt.mod8)
anova(UScrime.dt.mod8)

# Model 9 - crime rate vs Police expenditure + probability of imprisonment +state of imprisonment
UScrime.dt.mod9<-lm(crime_rate~pol_exp+prob_imp+state_pop,data = UScrime.data_clean)
summary(UScrime.dt.mod9)
anova(UScrime.dt.mod9)

# Model 10 - crime rate vs gdp + probability of imprisonment +state of imprisonment
UScrime.dt.mod10<-lm(crime_rate~gdp+prob_imp+state_pop,data = UScrime.data_clean)
summary(UScrime.dt.mod10)
anova(UScrime.dt.mod10)

#Find best model on the 3 parameters
anova(UScrime.dt.mod7,UScrime.dt.mod8,UScrime.dt.mod9,UScrime.dt.mod10)

#Based on AIC
model.set<-list(UScrime.dt.mod7,UScrime.dt.mod8,UScrime.dt.mod9,UScrime.dt.mod10)
model.names<-c("1,2,3","1,2,4","1,3,4","2,3,4")
aictab(model.set,modnames = model.names)

#Anova on all 4 parameters
# Model 11 - crime rate vs Police expenditure + Gdp+probality of imprisonment + State population
UScrime.dt.mod11<-lm(crime_rate~pol_exp+gdp+prob_imp+state_pop,data = UScrime.data_clean)
summary(UScrime.dt.mod11)
anova(UScrime.dt.mod11)

#Find Best model from 2 ,3,4 parameters
anova(UScrime.dt.mod1,UScrime.dt.mod7,UScrime.dt.mod11)

#Based on AIC
model.set<-list(UScrime.dt.mod1,UScrime.dt.mod7,UScrime.dt.mod11)
model.names<-c("1,2","1,2,3","1,2,3,4")
aictab(model.set,modnames = model.names)

#Just to confirm run anova and AIC on all models together
anova(UScrime.dt.mod1,UScrime.dt.mod2,UScrime.dt.mod3,UScrime.dt.mod4,UScrime.dt.mod5,UScrime.dt.mod6,UScrime.dt.mod7,UScrime.dt.mod8,UScrime.dt.mod9,UScrime.dt.mod10,UScrime.dt.mod11)
#Based on AIC
model.set<-list(UScrime.dt.mod1,UScrime.dt.mod2,UScrime.dt.mod3,UScrime.dt.mod4,UScrime.dt.mod5,UScrime.dt.mod6,UScrime.dt.mod7,UScrime.dt.mod8,UScrime.dt.mod9,UScrime.dt.mod10,UScrime.dt.mod11)
model.names<-c("1,2","1,3","1,4","2,3","2,4","3,4","1,2,3","1,2,4","1,3,4","2,3,4","1,2,3,4")
aictab(model.set,modnames = model.names)

#Plot the best model
par(mfrow=c(2,2))
plot(UScrime.dt.mod7)

#Model Validation
#Plot Histogram of residuals
hist(UScrime.dt.mod7$residuals,breaks = 5)
#Run Shapiro test
shapiro.test(UScrime.dt.mod7$residuals)

#Plot the residuals
plot(UScrime.data_clean$crime_rate, UScrime.dt.mod7$residuals)
abline(h = 0, col = "green")

