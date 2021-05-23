df = olsdf_wdum <- read.csv("~/covid-counties-documents/county-percentiles/olsdf_wdum.csv")

library(car)
library(MASS)
library(foreign)

df1 = subset(df, select = -c(county, state, S_dum, pos))

#initial model has R^2 of 0.19
mod1 = lm(exp_rate~., data=df1)
summary(mod1)

cor(df1, method = 'pearson')
vif(mod1)

mod2 = update(mod1, .~. -pop, data = df1)
summary(mod2)
vif(mod2)

mod3 = update(mod2, .~. -medhouse_inc, data = df1)
summary(mod3)
vif(mod3)


no.bad <- df1[-c(497), ]
mod4 = update(mod3, data = no.bad)
summary(mod4)

#counties with the worst leverage points include:
#los angeles, california; cook, illinois; maricopa, arizona; harris, texas
attach(no.bad)
summary(powerTransform((cbind(exp_rate, pc_income, medfam_inc, households))))

mod5 = lm(I(exp_rate^0.8)~log(pc_income)+log(medfam_inc)+log(households)+NE_dum+MW_dum+W_dum, data = no.bad)
summary(mod5)
plot(mod5)


#analysis of data with ethnic groups implemented
df2 = covid_exp <- read.csv("~/covid-counties-documents/county-percentiles/covid_exp.csv")
df2 = subset(df2, select = -c(pos,county,state,S_dum))
newmod1 = lm(exp_rate~., data = df2)
newmod2 = update(newmod1, .~. -total_pop -medfam_inc -white_only_total -black_only_total -amer_indian_alaskanative_only_total
                 -asian_only_total -pacisland_only_total -other_only_total -two_or_more_races_total -not_hislat_total -hislat_total
                 -whiteonly_pct)
newmod3 = update(newmod1, .~. -whiteonly_pct -blackonly_pct -indianonly_pct -asianonly_pct -pacislandonly_pct -otheronly_pct
                 -hislatonly_pct -total_pop -white_only_total)

#continuing on with newmod2 because it has the better fit.
newmod4 = update(newmod2, .~. -twoplusonly_pct)
newmod5 = update(newmod4, .~. -NE_dum -MW_dum -W_dum)
newmod6 = update(newmod4, .~. -NE_dum -MW_dum -pop) #focusing on just the west coast shows statistical significant. +0.008 R^2... pop has high vif

attach(df2)
summary(powerTransform(cbind(exp_rate, pc_income, medhouse_inc, households)))

newmod7 = lm(I(exp_rate^0.8)~log(pc_income)+I(medhouse_inc^-0.2)+log(households)+W_dum+
               blackonly_pct+indianonly_pct+asianonly_pct+pacislandonly_pct+otheronly_pct+hislatonly_pct, data = df2)
plot(newmod7)

newmod8 = update(newmod7, .~. +I(log(pc_income)^2) +I(log(households)^2))



#analysis with ethnic groups and population densities.
fin <- read.csv("~/covid-counties-documents/county-percentiles/final_covid_pls.csv")
fin1 = subset(fin, select = -c(pos, county, state, S_dum, white_only_total, black_only_total,
                               amer_indian_alaskanative_only_total, asian_only_total, pacisland_only_total,
                               other_only_total, two_or_more_races_total, not_hislat_total, hislat_total, total_pop,
                               twoplusonly_pct, otheronly_pct))
finmod1 = lm(exp_rate~., data = fin1)
summary(finmod1)

vif(finmod1)
finmod2 = update(finmod1, .~. -pop -medfam_inc -households -medhouse_inc -doubdays -pop_density)
summary(finmod2)
AIC(finmod2)

vif(finmod2)
finmod3 = update(finmod2, .~. -NE_dum -MW_dum -W_dum)
summary(finmod3)
anova(finmod2, finmod3, test = 'LRT')

cor(fin1$medhouse_inc, fin1$pc_income)
cor(fin1$pc_income, fin1$medfam_inc)
finmod4 = update(finmod2, .~. -medhouse_inc -medfam_inc)
summary(finmod4)

attach(fin1)
summary(powerTransform(cbind(exp_rate, pc_income, households, avg_house, pop_density)))
finmod5 = lm(I(exp_rate^0.82)~NE_dum+MW_dum+W_dum+I(pc_income^(-1/3))+log(households)+whiteonly_pct+blackonly_pct+
               indianonly_pct+asianonly_pct+pacislandonly_pct+hislatonly_pct+I(avg_house^-2.28), data = fin1)
summary(finmod5)

#DISCRETE MODELS:
fin1$doubdays <- round((log(2)/fin1$exp_rate))

fin2 <- subset(fin1, select = -c(exp_rate))
finmod6 <- glm(doubdays~., family = poisson(),data = fin2)
summary(finmod6)
finmod7 <- update(finmod6, as.formula(paste(".~. -medhouse_inc -medfam_inc -pop -households")))
summary(finmod7) #best poisson model.

finmod8 <- glm.nb(doubdays~., data = fin2)
summary(finmod8)
finmod9 <- update(finmod8, as.formula(paste(".~.-medhouse_inc -medfam_inc -pop -pop_density -households")))
summary(finmod9)


finmod10 <- lm(doubdays~., data = fin2)
summary(finmod10)
finmod11 = update(finmod10, .~. -medhouse_inc -medfam_inc -pop -households -pop_denisty)
summary(finmod11) #in case anyone asks.
AIC(finmod11)

attach(fin2)
summary(powerTransform(cbind(doubdays, pc_income, households, avg_house)))
finmod12 <- lm(I(doubdays^-0.8)~I(pc_income^-0.33)+log(households)+I(avg_house^-2)+NE_dum+MW_dum+W_dum+
                 whiteonly_pct+blackonly_pct+indianonly_pct+asianonly_pct+pacislandonly_pct+hislatonly_pct, data = fin2)
summary(finmod12)
AIC(finmod12)
