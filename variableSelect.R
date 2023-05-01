#dimensions of Park and Parkclean
dim(park)
dim(parkclean)

#Installing the olsrr package
install.packages("olsrr")
library(olsrr)

#backward selection using p value
fullmodel <- lm(formula = total_UPDRS ~ age + sex + test_time + motor_UPDRS + Jitter + Jitter.Abs + Jitter.RAP +
                  Jitter.PPQ5 + Jitter.DDP + Shimmer + Shimmer.dB + Shimmer.APQ3 + Shimmer.APQ5 +
                  Shimmer.APQ11 + Shimmer.DDA + NHR + HNR + RPDE + DFA + PPE, data= parkclean)
model1<-ols_step_backward_p(fullmodel,prem=.05, details = TRUE)
model1

#forward selection using p value
fullmodel <- lm(formula = total_UPDRS ~ age + sex + test_time + motor_UPDRS + Jitter + Jitter.Abs + Jitter.RAP +
                  Jitter.PPQ5 + Jitter.DDP + Shimmer + Shimmer.dB + Shimmer.APQ3 + Shimmer.APQ5 +
                  Shimmer.APQ11 + Shimmer.DDA + NHR + HNR + RPDE + DFA + PPE, data= parkclean)
model2<-ols_step_forward_p(fullmodel,penter=.05, details = TRUE)
model2

#step wise selection using p value
fullmodel <- lm(formula = total_UPDRS ~ age + sex + test_time + motor_UPDRS + Jitter + Jitter.Abs + Jitter.RAP +
                  Jitter.PPQ5 + Jitter.DDP + Shimmer + Shimmer.dB + Shimmer.APQ3 + Shimmer.APQ5 +
                  Shimmer.APQ11 + Shimmer.DDA + NHR + HNR + RPDE + DFA + PPE, data= parkclean)
model3<-ols_step_both_p(fullmodel,pent=.05,prem=.05, details = TRUE)
model3

#final model of the three methods
finalmodel <-lm(formula = total_UPDRS ~ age + sex + test_time + motor_UPDRS + Jitter + Jitter.Abs + Jitter.RAP
                + Shimmer + Shimmer.APQ5 +Shimmer.APQ11 + HNR + RPDE + DFA + PPE, data= parkclean)
summary(finalmodel)

#calculating AIC and BIC for model1,2 and 3
AIC(finalmodel)
BIC(finalmodel)

#cor matrix for the stepwise model
colli <- parkclean
colli[ , c('X','subject','sex', 'test_time_hr', 'test_time_min', 'Jitter.PPQ5','Jitter.DDP',
           'Shimmer.dB','Shimmer.APQ3','Shimmer.DDA', 'total_UPDRS', 'NHR')] <- list(NULL)
library(ggpubr)
library(cowplot)
library(corrplot)
M <- cor(colli)
corrplot(M, method = 'number')

#VIF for the stepwise model
library(car)
vif(finalmodel)

#summary of final model for identifying High stanbdard errors
summary(finalmodel)