#To open the csv file:
setwd(dirname(file.choose()))
getwd()

stud <- read.csv("u2563353_DS7006_CW2_data.csv", stringsAsFactors = FALSE)
head(stud)
str(stud)


# visualize the missing values:
# check for missing data
apply(stud, MARGIN = 2, FUN = function(x) sum(is.na(x)))
library(Amelia)
missmap(stud, col = c("black", "red"), legend = FALSE)
str(stud)

# remove any missing data
#stud <- na.omit(stud)#


boxplot(stud$total_Death_Per, xlab = "p_tot_deaths")
qqnorm(stud$total_Death_Per)
qqline(stud$total_Death_Per, col = 2)
ks.test(stud$total_Death_Per, mean(stud$total_Death_Per), sd(stud$total_Death_Per))

library(rcompanion)
plotNormalHistogram(stud$total_Death_Per)


boxplot(stud$male_Per, xlab = "male")
qqnorm(stud$male_Per)
qqline(stud$male_Per, col = 2)
ks.test(stud$male_Per, mean(stud$male_Per), sd(stud$male_Per))

plotNormalHistogram(stud$male_Per)

stud$log_male_per <- log(stud$male_Per)
boxplot(stud$log_male_per, xlab = "male")
qqnorm(stud$log_male_per)
qqline(stud$log_male_per, col = 2)
ks.test(stud$log_male_per, mean(stud$log_male_per), sd(stud$log_male_per))

plotNormalHistogram(stud$log_male_per)
# hist(stud$log_male_per) 



boxplot(stud$female_Per)
qqnorm(stud$female_Per)
qqline(stud$female_Per, col = 2)
ks.test(stud$female_Per, mean(stud$female_Per), sd(stud$female_Per))

plotNormalHistogram(stud$female_Per)


stud$log_female_Per<- log(stud$female_Per)
boxplot(stud$log_female_Per , xlab = "female")
qqnorm(stud$log_female_Per)
qqline(stud$log_female_Per, col = 2)
ks.test(stud$log_female_Per, mean(stud$log_female_Per), sd(stud$log_female_Per))

plotNormalHistogram(stud$log_female_Per, xlab = "female")


boxplot(stud$child_Per)
qqnorm(stud$child_Per)
qqline(stud$child_Per, col = 2)
ks.test(stud$child_Per, mean(stud$child_Per), sd(stud$child_Per))

plotNormalHistogram(stud$child_Per)



boxplot(stud$teen_Per, xlab = "teen")
qqnorm(stud$teen_Per)
qqline(stud$teen_Per, col = 2)
ks.test(stud$teen_Per, mean(stud$teen_Per), sd(stud$teen_Per))

plotNormalHistogram(stud$teen_Per, xlab = "teen")


stud$log_teen_Per <- log(stud$teen_Per)
boxplot(stud$log_teen_Per, xlab = "teen")
qqnorm(stud$log_teen_Per)
qqline(stud$log_teen_Per, col = 2)
ks.test(stud$log_teen_Per, mean(stud$log_teen_Per), sd(stud$log_teen_Per))

plotNormalHistogram(stud$log_teen_Per, xlab = "teen")



boxplot(stud$adult_Per)
qqnorm(stud$adult_Per, xlab = "adult")
qqline(stud$adult_Per, col = 2)
ks.test(stud$adult_Per, mean(stud$adult_Per), sd(stud$adult_Per))

plotNormalHistogram(stud$adult_Per, xlab = "adult")

####################################

############## Elder Normal ###############

boxplot(stud$elder_Per, xlab = "elder")
qqnorm(stud$elder_Per)
qqline(stud$elder_Per, col = 2)
ks.test(stud$elder_Per, mean(stud$elder_Per), sd(stud$elder_Per))

plotNormalHistogram(stud$elder_Per, xlab = "elder")



boxplot(stud$Very_Good.Health_Per, xlab = "Very_Good.Health_Per")
qqnorm(stud$Very_Good.Health_Per)
qqline(stud$Very_Good.Health_Per, col = 2)
ks.test(stud$Very_Good.Health_Per, mean(stud$Very_Good.Health_Per), sd(stud$Very_Good.Health_Per))

plotNormalHistogram(stud$Very_Good.Health_Per, xlab = "Very_Good.Health_Per")



stud$log_Vg_healPer <- log(stud$Very_Good.Health_Per)
boxplot(stud$log_Vg_healPer)
qqnorm(stud$log_Vg_healPer)
qqline(stud$log_Vg_healPer, col = 2)
ks.test(stud$log_Vg_healPer, mean(stud$log_Vg_healPer), sd(stud$log_Vg_healPer))

plotNormalHistogram(stud$log_Vg_healPer)




boxplot(stud$good_Health_Per, xlab = "good_Health_Per")
qqnorm(stud$good_Health_Per)
qqline(stud$good_Health_Per, col = 2)
ks.test(stud$good_Health_Per, mean(stud$good_Health_Per), sd(stud$good_Health_Per))

plotNormalHistogram(stud$good_Health_Per, xlab = "good_Health_Per")


stud$log_g_healPer <- log(stud$good_Health_Per)
boxplot(stud$log_g_healPer, xlab = "good_Health_Per")
qqnorm(stud$log_g_healPer)
qqline(stud$log_g_healPer, col = 2)
ks.test(stud$log_g_healPer, mean(stud$log_g_healPer), sd(stud$log_g_healPer))

plotNormalHistogram(stud$log_g_healPer, xlab = "good_Health_Per")




boxplot(stud$normal_Health_Per, xlab = "normal_Health_Per")
qqnorm(stud$normal_Health_Per)
qqline(stud$normal_Health_Per, col = 2)
ks.test(stud$normal_Health_Per, mean(stud$normal_Health_Per), sd(stud$normal_Health_Per))

plotNormalHistogram(stud$normal_Health_Per, xlab = "normal_Health_Per")


stud$log_nor_healPer <- log(stud$normal_Health_Per)
boxplot(stud$log_nor_healPer, xlab = "normal_Health_Per")
qqnorm(stud$log_nor_healPer)
qqline(stud$log_nor_healPer, col = 2)
ks.test(stud$log_nor_healPer, mean(stud$log_nor_healPer), sd(stud$log_nor_healPer))

plotNormalHistogram(stud$log_nor_healPer, xlab = "normal_Health_Per")

#########################################################


############## bad Health is Normal ###############

boxplot(stud$bad_Health_Per, xlab = "bad_Health_Per")
qqnorm(stud$bad_Health_Per)
qqline(stud$bad_Health_Per, col = 2)
ks.test(stud$bad_Health_Per, mean(stud$bad_Health_Per), sd(stud$bad_Health_Per))

plotNormalHistogram(stud$bad_Health_Per, xlab = "bad_Health_Per")


stud$bad_healPer <- log(stud$bad_Health_Per)
boxplot(stud$bad_healPer, xlab = "bad_Health_Per")
qqnorm(stud$bad_healPer)
qqline(stud$bad_healPer, col = 2)
ks.test(stud$bad_healPer, mean(stud$bad_healPer), sd(stud$bad_healPer))

plotNormalHistogram(stud$log_bad_healPer, xlab = "bad_Health_Per")




boxplot(stud$very_Bad_Per, xlab = "very_Bad_Per")
qqnorm(stud$very_Bad_Per)
qqline(stud$very_Bad_Per, col = 2)
ks.test(stud$very_Bad_Per, mean(stud$very_Bad_Per), sd(stud$very_Bad_Per))

plotNormalHistogram(stud$very_Bad_Per, xlab = "very_Bad_Per")


stud$very_Bad_Pe <- log(stud$very_Bad_Per)
boxplot(stud$very_Bad_Pe, xlab = "very_Bad_Per")
qqnorm(stud$very_Bad_Pe)
qqline(stud$very_Bad_Pe, col = 2)
ks.test(stud$very_Bad_Pe, mean(stud$very_Bad_Pe), sd(stud$very_Bad_Pe))

plotNormalHistogram(stud$very_Bad_Pe, xlab = "very_Bad_Per")




#converting the All the Attributes into proportion by 1000:
#putting into Relevant data frame:
co <- data.frame(stud$child_Per, stud$teen_Per, stud$adult_Per, stud$elder_Per, 
                 stud$male_Per, stud$female_Per, 
                 stud$Very_Good.Health_Per, stud$good_Health_Per, stud$normal_Health_Per, stud$bad_Health_Per, stud$very_Bad_Per, 
                 stud$total_Death_Per)

colnames(co) <- c("chPer", "tePer", "aduPer", "eldPer",
                  "mPer", "fPer", 
                  "vGHealth", "gHealth","nHealth", "bHealth", "vBHealth",
                  "total_P")
str(co)

# Significance testing for normality

# Kolmogorov-Smirnov Tests of normality
# ks.test(x, y, ...,
#        alternative = c("two.sided", "less", "greater"),
#        exact = NULL)


# Kolmogorov-Smirnov Tests of normality
ks.test(co$total_P, "pnorm", mean(co$total_P), sd(co$total_P))
ks.test(co$chPer, "pnorm", mean(co$chPer), sd(co$chPer))
ks.test(co$tePer, "pnorm", mean(co$tePer), sd(co$tePer))
ks.test(co$aduPer, "pnorm", mean(co$aduPer), sd(co$aduPer))
# D = 0.042642, p-value = 0.5997
ks.test(co$eldPer, "pnorm", mean(co$eldPer), sd(co$eldPer))
ks.test(co$fPer, "pnorm", mean(co$fPer), sd(co$fPer))
ks.test(co$mPer, "pnorm", mean(co$mPer), sd(co$mPer))
ks.test(co$vGHealth, "pnorm", mean(co$vGHealth), sd(co$vGHealth))
ks.test(co$gHealth, "pnorm", mean(co$gHealth), sd(co$gHealth))
# D = 0.033815, p-value = 0.8539
ks.test(co$nHealth, "pnorm", mean(co$nHealth), sd(co$nHealth))
ks.test(co$bHealth, "pnorm", mean(co$bHealth), sd(co$bHealth))
ks.test(co$vBHealth, "pnorm", mean(co$vBHealth), sd(co$vBHealth))

# An alternative is the Shapiro-Wilk's test
shapiro.test(co$total_P)


cor.test(co$total_P, co$chPer)
cor.test(co$total_P, co$tePer)
cor.test(co$total_P, co$aduPer)
cor.test(co$total_P, co$eldPer)
cor.test(co$total_P, co$fPer)
cor.test(co$total_P, co$mPer)
cor.test(co$total_P, co$vGHealth)
cor.test(co$total_P, co$gHealth)
cor.test(co$total_P, co$nHealth)
cor.test(co$total_P, co$bHealth)
cor.test(co$total_P, co$vBHealth)


# Correlation matrix for all variables
cor.matrix <- cor(co, use = "pairwise.complete.obs", method = "spearman")
cor.df <- as.data.frame(round(cor.matrix, 2))
View(cor.df)
round(cor.df,2)


library(corrplot)
corrplot(cor.matrix, type = "upper", tl.col = "black", tl.srt = 45)

library(psych)
pairs.panels(co, method = "spearman", hist.col = "grey", col = "blue", main = "Spearman")

library(corrgram)
# corrgram works best with Pearson correlation
corrgram(co, order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="Variables")

# Assuming 'data' is your dataset containing variables X, Y, Z, etc.
# Install and load the ppcor package (if not installed)
install.packages("ppcor")
library(ppcor)

pcor.test(co$total_P, co$vGHealth, co$eldPer)
pcor.test(co$total_P, co$eldPer, co$vGHealth)
pcor.test(co$total_P, co$eldPer, co$vBHealth)
pcor.test(co$total_P, co$vBHealth, co$eldPer)
pcor.test(co$total_P, co$chPer, co$vBHealth)
pcor.test(co$total_P, co$vBHealth, co$chPer)
pcor.test(co$total_P, co$chPer, co$bHealth)
pcor.test(co$total_P, co$bHealth, co$chPer)


#correlation matrix and correlation plot for all dependent and independent variables
myvars <- names(co) %in% c("LA_name", "LA_code","total_Population")
deaths3 <- co[!myvars]
str(deaths3)
rm(myvars)


# correlation matrix for all variables
cor.matrix <- cor(deaths3, use = "pairwise.complete.obs", method = "spearman")
cor.df <- as.data.frame(round(cor.matrix, 2))
View(cor.df)

library(corrplot)
corrplot(cor.matrix, type = "upper", tl.col = "black", tl.srt = 45)

library(psych)
pairs.panels(co, method = "spearman", hist.col = "grey", col = "blue", main = "Spearman")


# corrgram works best with Pearson correlation
library(corrgram)
corrgram(deaths3, order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="Variables")



# Kaiser-Meyer-Olkin statistics: if overall MSA > 0.6, proceed to factor analysis
#i have a very few variable so i'm using old data(Co)
# Linear Regression
library(nFactors)
library(psych)
# Linear Regression
model1 <- lm(total_P ~ chPer + tePer + aduPer + mPer + vGHealth + gHealth + nHealth + bHealth, data = co)

# add regression line to scatter plot
plot(co$chPer + co$tePer + co$aduPer + co$fPer + co$vGHealth + co$gHealth + co$nHealth + co$bHealth, co$total_P, main = "Scatterplot",
     xlab = "Variable", ylab = "Total Deaths")
abline(model1, col = "red",lwd=1)

summary(model1)

hist(model1$residuals)
rug(model1$residuals)
# consider normality of residuals
plot(model1$residuals ~ model1$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model1$residuals, "pnorm", mean(model1$residuals), sd(model1$residuals))



# select variables by excluding dependent variables and others not required
myvars <- names(co) %in% c("LA_name", "LA_code","total_Population","total_P")
deaths4 <- co[!myvars]
str(deaths4)
rm(myvars)

#Kaiser-Meyer-Olkin factor adequacy
KMO(cor(deaths4))

# get eigenvalues
ev <- eigen(cor(co))
ev$values
# plot a scree plot of eigenvalues
plot(ev$values, type="b", col= "blue", xlab="variables")

# calculate cumulative proportion of eigenvalue and plot
ev.sum <- 0
for(i in 1:length(ev$value)){
  ev.sum <- ev.sum+ev$value[i]
}
ev.list1 <- 1:length(ev$value)
for(i in 1:length(ev$value)){
  ev.list1[i] <- ev$value[i]/ev.sum
}
ev.list2 <- 1:length(ev$value)
ev.list2[1] <- ev.list1[1]
for(i in 2:length(ev$value)){
  ev.list2[i] <- ev.list2[i-1]+ev.list1[i]
}
plot (ev.list2, type = "b", col = "red", xlab = "number of components", ylab = "cumulative proportion")

# Varimax Rotated Principal Components
# retaining 'nFactors' components
library(GPArotation)

fit <- principal(deaths4, nfactors = 3, rotate = "varimax")
fit

# get eigenvalues
ev <- eigen(cor(deaths4))
ev$values
# plot a scree plot of eigenvalues
plot(ev$values, type = "b", col = "blue", xlab = "variables")

fit <- principal(deaths4, nfactors = 3, rotate = "varimax")
fit

# create four variables to represent the rorated components
fit$scores
fit.data <- data.frame(fit$scores)

# check new variables are uncorrelated
cor.matrix2 <-cor(fit.data, method = "spearman")
cor.df2 <- as.data.frame(cor.matrix2)
round(cor.df2, 2)


# Multiple Regression

# model with all variables
model2 <- lm(total_P ~ chPer + tePer + aduPer + mPer + vGHealth + gHealth + nHealth + bHealth, data = co)

summary(model2)
# calculate variance inflation factor
library(car)
vif(model2)
sqrt(vif(model2)) > 2  # if > 2 vif too high

# Assuming 'co' is your dataset
cor_matrix <- cor(co, method = "spearman")  # Calculating the correlation matrix


# Visualizing the correlation matrix using corrplot
library(corrplot)
corrplot(cor_matrix, type = "upper", tl.col = "black", tl.srt = 45)

model3 <- lm(total_P ~ chPer + aduPer + mPer + gHealth + nHealth + bHealth, data = co)
summary(model3)
sqrt(vif(model3)) > 2


#calculate partial correlation
library(ppcor)

model3a <- lm(total_P ~ vGHealth + gHealth + nHealth + bHealth, data = co)
summary(model3a)
sqrt(vif(model3a)) > 2

# relative importance of variables
library(relaimpo)
calc.relimp(model3a, type = c("lmg"), rela = TRUE)

# use a stepwise approach to search for a best model
library(RcmdrMisc)

# add other correlating variables not in components
cor3b <- cor(co[, c("gHealth", "nHealth", "bHealth")], method = "spearman")
corrplot(cor3b, type = "upper", tl.col = "black", tl.srt = 45)



model3b <- lm (total_P ~ gHealth + nHealth + bHealth,data = co)
summary(model3b)
sqrt(vif(model3b)) > 2
calc.relimp(model3b, type = c("lmg"), rela = TRUE)

  
# forward stepwise selection
model4 <- stepwise(model3b, direction = "forward")
summary(model4)
hist(model4$residuals)
rug(model4$residuals)
plot(model4$residuals ~ model4$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model4$residuals, "pnorm", mean(model4$residuals), sd(model4$residuals))

summary(model4)

library(lmtest)
het_test <- bptest(model4)

outlier_test <- outlierTest(model4)

co$interaction_term <- co$gHealth * co$bHealth
model_interact <- lm(total_P ~ nHealth + bHealth + interaction_term, data = co)
summary(model_interact)

anova(model4, model_interact)


#-----Section 05-------------------------------------------

detach(deaths)

