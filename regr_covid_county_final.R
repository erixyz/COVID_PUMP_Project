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
