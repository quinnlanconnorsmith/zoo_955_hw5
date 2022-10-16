#Lecture 5 - Additive modeling Zurr et al. Ch 3 
#Quinn and Amanda 
#Import ISIT dataset from text 

View(ISIT)
#3.2.2 GAM in gam w/LOESS
#Generating fig 3.1A from the chapter 
op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2)) #Changing some basic parameters for graphing  
Sources16 <- ISIT$Sources[ISIT$Station == 16] #Filtering sources from 16 
Depth16 <- ISIT$SampleDepth[ISIT$Station == 16] #Filtering depths from 16
plot(Depth16, Sources16, type = "p")
#Fig. 3.1B
library(gam)
M1 <- gam(Sources16 ~ lo(Depth16, span = 0.5))
plot(M1, se = TRUE) 
#The LOESS (locally weighted smoothing (LOWESS)/local regression smoother) curve is a way of predicting values 
#fig 3.1C
M2 <- predict(M1, se = TRUE) #I think there was actually a typo in the book? 
plot(Depth16, Sources16, type = "p")
I1 <- order(Depth16)
lines(Depth16[I1], M2$fit[I1], lty = 1)
lines(Depth16[I1], M2$fit[I1] + 2 * M2$se[I1], lty = 2)
lines(Depth16[I1], M2$fit[I1] - 2 * M2$se[I1], lty = 2)
par(op)
#This is just plotting predicted points and overlaying LOESS
#Fit and SE can be pulled with above code, which is nice
#LOESS works by applying a local linear regression within a window of the x axis 
#You can change window size, but it is expressed as a percentage in R 
lo(Depth, span = 0.5)
#Do we need to specify data here? 
#Anyway, by changing the window size we can analyze data over and underfitting, 
#Figure 3.3 shows this, however there is a tradeoff of bias-variance 
#Small spans will have greater variances 
#You can also use AIC! 
#OR the mgcv package will automatically smooth lines 
#Fig 3.4 shows residual patterns, heterogeneity, and non-normality 

#3.2.3 GAM in mgcv with Cubic Regression Splines 
#Doing similar things but now with mgcv 
library(mgcv)
op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2)) #Messing with parameters 
Sources16 <- ISIT$Sources[ISIT$Station == 16]
Depth16 <- ISIT$SampleDepth[ISIT$Station == 16]
plot(Depth16, Sources16, type = "p")
M3 <- gam(Sources16 ~ s(Depth16, fx = FALSE, k=-1,
                          bs = "cr"))  #New mgcv object 
#We can apply smoothing factor to explanatory variable 
#We can also specify that the smoothing is not fixed to a preset value 
#CR refers to the cubic regression slpine 
plot(M3, se = TRUE) #New wavy loess lines 
M3pred <- predict(M3, se = TRUE, type = "response")
plot(Depth16, Sources16, type = "p")
I1 <- order(Depth16)
lines(Depth16[I1], M3pred$fit[I1], lty=1)
lines(Depth16[I1], M3pred$fit[I1]+2*M3pred$se[I1],lty=2)
lines(Depth16[I1], M3pred$fit[I1]-2*M3pred$se[I1],lty=2)
#Individually adding lines based on fit and se 

#3.3 Technical details of GAM in mgcv 
#Techy discussion on regression splines 
#For cubic polynomials, x axis is split into segments, then regression is applied 
#Sometimes though, the segments will appear 'Broken' 
# cubic regression spline is applied to segments to smooth things out using 1st and 2nd order derivatives 
#You can choose segments or 'knots'. Keep in mind you may reach a saturation point (can use AIC here)

#3.3.2
#The cubic spline regression gives the best fit with the least amount of errors 
#We've talked about knots, but what is the optimal amount of smoothing? 

#3.3.3
#Beta and lamba both unknown at this point 
#Going to use generalised cross-validation 
#Be aware of violating assumptions of collinerity and independence as well as using on small datasets 

#3.3.5 
#DoF and other types of smoothers 
#Useful - cyclic cubic regression spline, shrinkage smoothers (0 smoothing, useful for backwards model selection)
#Expressed as effective degrees of freedom for a smoother 

#3.4 
#GAM example 1, looking at different smoothers for seperate stations 

S8 <- ISIT$Sources[ISIT$Station == 8]
D8 <- ISIT$SampleDepth[ISIT$Station == 8]
S13 <- ISIT$Sources[ISIT$Station == 13]
D13 <- ISIT$SampleDepth[ISIT$Station == 13]
So <- c(S8, S13); De <- c(D8, D13)
ID <- rep(c(8, 13), c(length(S8), length(S13)))
mi <- max(min(D8), min(D13))
ma <- min(max(D8), max(D13))
I1 <- De > mi & De < ma
op <- par(mfrow = c(1, 2))
plot(D8[I1], S8[I1], pch = 16, xlab = "Depth",
       ylab = "Sources", col = 1, main = "Station 8",
       xlim = c(500, 3000), ylim = c(0, 40))
plot(D13[I1], S13[I1], pch = 16, xlab = "Depth",
       ylab = "Sources", col = 1, main = "Station 13",
       xlim = c(500, 3000), ylim = c(0, 40))
par(op)
#Filtering data - concatenating data usong De and So (probably easier in dplyr?)

#Now lets get on the peering into the models 
library(mgcv)
M4 <- gam(So ~ s(De) + factor(ID), subset = I1)
#Similar to linear regression
summary(M4)
anova(M4)
#Explained deviance is 72%, smoother for depth is significant, you can use the intercepts to make estimated curves 
gam.check(M4)
#Sweet diagnostics check, but doesn't look great for this model 
par(mar = c(2, 2, 2, 2))
vis.gam(M4, theta = 120, color = "heat")
#This doesn't seem to work? 

#3.4.1
#Interactions within GAM 
#Making a new model 
M5<-gam(So ~ s(De)+
          s(De, by = as.numeric(ID == 13)) +
          factor(ID), subset = I1)
anova(M5)
#2nd depth smoother is real significant 
#s(De) applies a smoother along depth for all data points 
#2nd smoother has a by argument that applies to section 13 
gam.check(M5)
#Comapre using AIC 
AIC(M4)
AIC(M5)
#Can also use the generalised cross validation score from summary 
#Can also use F-stat and p-value 
#OR - 
anova(M4, M5, test = "F")
#These are similar results to what we saw with other tests 
#Let's apply smoothers to each of these: 
M6 <- gam(So ~ s(De, by = as.numeric(ID == 8)) +
            s(De, by = as.numeric(ID == 13)) +
            factor(ID), subset = I1)

#3.4
#GAM example with collinearity violation 
#As explanatory variables increase, it becomes important to avoid using collinear explanatory variables within the GAM
#I don't think we have this dataset but I put the code below: 

library(mgcv)
M7 <- gam(Richness ~ s(ROCK, bs = "cs") +
              s(LITTER, bs = "cs") + s(BARESOIL, bs = "cs") +
              s(FallPrec, bs = "cs") + s(SprTmax, bs = "cs"),
            data = Vegetation)
#Below ANOVA output is with shrunkage smoothers "cs" above
#Approximate significance of smooth terms:
#  edf Est.rank F p-value
#s(ROCK) 1.750 4.000 4.634 0.00367
#s(LITTER) 1.865 4.000 2.320 0.07376
#s(BARESOIL) 4.167 9.000 2.991 0.00820
#s(FallPrec) 4.699 9.000 2.216 0.04150
#s(SprTmax) 5.103 9.000 3.508 0.00286
#After dropping insignificant terms, we're left with ROCK, BARESOIL, and SprTmax