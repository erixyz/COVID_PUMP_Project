df = olsdf_wdum <- read.csv("~/covid-counties-documents/county-percentiles/olsdf_wdum.csv")

library(car)
library(MASS)
library(AER)
library(foreign)
library(cAIC4)

df$exp_rate <- round(log(2)/df$exp_rate)
names(df)[names(df) == 'exp_rate'] <-'d_time'
df1 = subset(df, select = -c(county, state, S_dum, pos))

#initial model has R^2 of 0.19
mod1 = lm(d_time~., data=df1)
summary(mod1)

cor(df1, method = 'pearson')
vif(mod1)

mod2 = update(mod1, .~. -households, data = df1)
summary(mod2)
vif(mod2)

mod3 = update(mod2, .~. -medfam_inc, data = df1)
summary(mod3)
vif(mod3)
plot(mod3)

no.bad <- df1[-c(650,893,1803), ]
mod4 = update(mod3, data = no.bad)
summary(mod4)

#in this case, the worst leverage points are counties that didn't get hit by COVID as bad as other places.
attach(no.bad)
summary(powerTransform((cbind(d_time, pc_income, medfam_inc, households))))

mod5 = lm(I(d_time^(-0.8))~log(pc_income)+log(medfam_inc)+log(households)+NE_dum+MW_dum+W_dum, data = no.bad)
summary(mod5)

detach(no.bad)
plot(mod5)

poiss.mod = glm(d_time~pc_income+medfam_inc+households+NE_dum+MW_dum+W_dum, family = 'poisson', data = no.bad)
summary(poiss.mod)
dispersiontest(poiss.mod) #dispersion is about ~5.23

quasi.mod = glm(d_time~pc_income+medfam_inc+households+NE_dum+MW_dum+W_dum, family = quasipoisson(link = 'log'), data = no.bad)
summary(quasi.mod)

nb.mod <- glm.nb(d_time~pc_income+medfam_inc+households+NE_dum+MW_dum+W_dum, data = no.bad)
summary(nb.mod)
AIC(mod5)
