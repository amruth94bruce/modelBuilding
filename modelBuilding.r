psatable <- read.table("C:/Users/Madhura/Desktop/utd/prob/prostate_cancer.csv", sep=",", header=T)
head(psatable)
# we have to remove the subject id because it wont help in predicting the response
psatable <- psatable[,-1]
head(psatable)
str(psatable)
#we see that vesinv and gleason are taken as integers. We need to change it to factors
psatable$vesinv <- factor(psatable$vesinv)
psatable$gleason <- factor(psatable$gleason)
str(psatable)

#checking the data of various columns
table(psatable$cancervol)
table(psatable$weight)
table(psatable$age)
table(psatable$benpros)
table(psatable$vesinv)
table(psatable$capspen)
table(psatable$gleason)

#selecting the best modification for response
y3<- psatable$psa
y1<- (psatable$psa)^ (1/3)
y <- sqrt(psatable$psa)
y2 <- log(psatable$psa)
#we find that taking square root of psa is better

plot(psatable$cancervol,y)
fit1 <- lm(y ~ cancervol, data = psatable)
abline(fit1)
anova(fit1)
#Response: y
#Df Sum Sq Mean Sq F value    Pr(>F)    
#cancervol  1 340.63  340.63  93.536 8.323e-16 ***
#  Residuals 95  97469    1026
#since p value is really low, we reject the null hypothsis and select this field for predicting

plot(psatable$weight,y)
fit2 <- lm(y ~ weight, data = psatable)
abline(fit2)
anova(fit2)
#Response: y
#Df Sum Sq Mean Sq F value Pr(>F)
#weight     1   2.96   2.962  0.4116 0.5227
#Residuals 95 683.62   7.196 
#since p value is high we accept the null hypothesis which is the slope is zero. So this predictor
#has no effect on response varIABLE

plot(psatable$age,y)
fit3 <- lm(y ~ age, data = psatable)
abline(fit3)
anova(fit3)
#Response: y
#Df Sum Sq Mean Sq F value Pr(>F)
#age        1   4.29  4.2852  0.5967 0.4418
#Residuals 95 682.30  7.1821 
#since p value is high we accept the null hypothesis which is the slope is zero. So this predictor
#has no effect on response varIABLE

plot(psatable$benpros,y)
fit4 <- lm(y ~ benpros, data = psatable)
abline(fit4)
anova(fit4)
#Response: y
#Df Sum Sq Mean Sq F value Pr(>F)
#benpros    1   1.38  1.3792  0.1912 0.6629
#Residuals 95 685.20  7.2126  
#since p value is high we accept the null hypothesis which is the slope is zero. So this predictor
#has no effect on response varIABLE

plot(psatable$vesinv,y)
fit5 <- lm(y ~ vesinv, data = psatable)
abline(fit5)
anova(fit5)
#Response: y
#Df Sum Sq Mean Sq F value    Pr(>F)    
#vesinv     1 252.48 252.483  55.255 4.616e-11 ***
#Residuals 95 434.10   4.569   
#since p value is really low, we reject the null hypothsis and select this field for predicting

plot(psatable$capspen,y)
fit6 <- lm(y ~ capspen, data = psatable)
abline(fit6)
anova(fit6)
#Response: y
#Df Sum Sq Mean Sq F value    Pr(>F)    
#capspen    1 229.77 229.766  47.782 5.444e-10 ***
#Residuals 95 456.81   4.809
#since p value is really low, we reject the null hypothsis and select this field for predicting

plot(psatable$gleason,y)
fit7 <- lm(y ~ gleason, data = psatable)
anova(fit7)
#Response: y
#Df Sum Sq Mean Sq F value    Pr(>F)    
#gleason    2 226.89  113.44  23.198 6.478e-09 ***
#Residuals 94 459.69    4.89   
#since p value is really low, we reject the null hypothsis and select this field for predicting

# So we are choosing cancer volume,vesinv,capspen and gleason as our predictors
#we start with adding volume and vesinv to our model
fit8 <- lm(y ~ cancervol + vesinv,data=psatable)
anova(fit8)
#Response: y
#Df Sum Sq Mean Sq F value    Pr(>F)    
#cancervol  1  62202   62202 64.3543 2.869e-12 ***
 #vesinv     1  40.14   40.14  12.337 0.0006843 ***
 # Residuals 94 305.82    3.25  
#sine p value is low we can add these predictors to our model

fit25 <- lm(y ~ capspen + gleason,data=psatable)
AIC(fit8,fit25)
#      df      AIC
#fit8   4 394.6569
#fit20  5 417.3879
#Hence fit 8 is better. So we start with cancervol and vesinv

fit26 <- lm(y ~ capspen + vesinv,data=psatable)
AIC(fit8,fit26)
#      df      AIC
#fit8   4 394.6569
#fit20  5 420.4316
#Hence fit 8 is better. So we start with cancervol and vesinv

fit9 <- lm(y ~ cancervol + vesinv+weight,data=psatable)
anova(fit8,fit9)
#Model 1: y ~ cancervol + vesinv
#Model 2: y ~ cancervol + vesinv + weight
#Res.Df   RSS Df Sum of Sq      F Pr(>F)
#1     94 90857                           
#2     93 303.03  1    2.7848 0.8546 0.3576
#we knew adding weight to our model is not nedded. Here we accept the null hypothsesis,
#so we reject weight

fit10 <- lm(y ~ cancervol + vesinv+age,data=psatable)
anova(fit8,fit10)
#Model 1: y ~ cancervol + vesinv
#Model 2: y ~ cancervol + vesinv + age
#Res.Df   RSS Df Sum of Sq      F Pr(>F)
#1     94 90857                           
#2     93 305.44  1   0.37689 0.1148 0.7356
#we knew adding age to our model is not nedded. Here we accept the null hypothsesis,
#so we reject age

#now we add capspen to fit8 which has volume and vesinv
fit12 <- lm(y ~ cancervol + vesinv+capspen,data=psatable)
anova(fit8,fit12)
#Model 1: y ~ cancervol + vesinv
#Model 2: y ~ cancervol + vesinv + capspen
#Res.Df   RSS Df Sum of Sq      F Pr(>F)
#1     94 90857                           
#2     93 305.71  1   0.11004 0.0335 0.8552
#since p value is high we accept the null hypothsis meaning capspen has no effect on response 
#so we remove it from our model

#now we add gleason to our model which has voulme and vesinv
fit13 <- lm(y ~ cancervol + vesinv+gleason,data=psatable)
anova(fit8,fit13)
#Model 1: y ~ cancervol + vesinv
#Model 2: y ~ cancervol + vesinv + gleason
#Res.Df   RSS Df Sum of Sq      F Pr(>F)
#1     94 90857                           
#2     92 282.66  2    23.159 3.7689 0.02672 *
#since p value is low we add gleason to our model

fit17 <- lm(y ~ cancervol + vesinv+gleason+weight,data=psatable)
anova(fit13,fit17)
#Model 1: y ~ cancervol + vesinv + gleason
#Model 2: y ~ cancervol + vesinv + gleason + weight
#Res.Df    RSS Df Sum of Sq      F Pr(>F)
#1     92 282.66                           
#2     91 280.14  1    2.5209 0.8189 0.3679
#Since p value is high we dont add weight to our model

fit18 <- lm(y ~ cancervol + vesinv+gleason+age,data=psatable)
anova(fit13,fit18)
#Model 1: y ~ cancervol + vesinv + gleason
#Model 2: y ~ cancervol + vesinv + gleason + age
#Res.Df    RSS Df Sum of Sq     F Pr(>F)
#1     92 282.66                          
#2     91 282.58  1   0.07439 0.024 0.8773
#Since p value is high we dont add weight to our model

fit19 <- lm(y ~ cancervol + vesinv+gleason+benpros,data=psatable)
anova(fit13,fit19)
#Model 1: y ~ cancervol + vesinv + gleason
#Model 2: y ~ cancervol + vesinv + gleason + benpros
#Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
#1     92 282.66                              
#2     91 268.73  1    13.933 4.7183 0.03244 *
#Since p value is low we add benpros to our model 
#hence our model predictors are cancer volume and vesinv and gleason and benpros

fit20 <- lm(y ~ cancervol + vesinv+gleason+benpros+weight,data=psatable)
anova(fit19,fit20)
#Model 1: y ~ cancervol + vesinv + gleason + benpros
#Model 2: y ~ cancervol + vesinv + gleason + benpros + weight
#Res.Df    RSS Df Sum of Sq      F Pr(>F)
#1     91 268.73                           
#2     90 268.62  1     0.106 0.0355 0.8509
#Since p value is high we dont add weight to our model

#Forward selection based on AIC
fit14.forward <- step(lm(y ~ 1, data = psatable), 
                      scope = list(upper = ~cancervol + vesinv + gleason +capspen),
                      direction = "forward")

# Backward elimination based on AIC
fit15.backward <- step(lm(y ~ cancervol + vesinv + gleason +capspen, data = psatable), 
                       scope = list(lower = ~1), direction = "backward")


# Both forward/backward
fit16.both <- step(lm(y ~ 1, data = psatable), 
                   scope = list(lower = ~1, upper = ~cancervol + vesinv + gleason +capspen),
                   direction = "both")

#We see that:
#forward direction picks: cancervol + vesinv + gleason
#backward direction picks: cancervol + vesinv + gleason
#both direction picks: cancervol + vesinv + gleason
#Here all three models give the same result
#Whereas, we came up with the following model (fit19):
#cancervol + vesinv + gleason + benpros

#We compare our model with this model
anova(fit19,fit16.both)
#Model 1: y ~ cancervol + vesinv
#Model 2: y ~ cancervol + vesinv + gleason
#Res.Df   RSS Df Sum of Sq      F Pr(>F)
#1     91 268.73                         
#2     92 282.66 -1   -13.933 4.7183 0.03244 *
#Since p value is less we can add benpros and hence our final model has:
#cancervol + vesinv + gleason + benpros

AIC(fit19,fit16.both)
#           df      AIC
#fit19       7 388.1149
#fit16.both  6 391.0183
#Since AIC value of our model is less, our model is better.

summary(fit19)
Call:
  lm(formula = y ~ cancervol + vesinv + gleason + benpros, data = psatable)

Residuals:
  Min      1Q  Median      3Q     Max 
-4.7138 -0.9340 -0.1334  0.6718  7.0183 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.92287    0.35128   5.474 3.86e-07 ***
  cancervol    0.14936    0.03076   4.856 4.95e-06 ***
  vesinv1      1.80134    0.53646   3.358  0.00115 ** 
  gleason7     0.17643    0.41140   0.429  0.66904    
gleason8     1.47272    0.57866   2.545  0.01261 *  
  benpros      0.12841    0.05911   2.172  0.03244 *  
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.718 on 91 degrees of freedom
Multiple R-squared:  0.6086, Adjusted R-squared:  0.5871 
F-statistic:  28.3 on 5 and 91 DF,  p-value: < 2.2e-16


