
##1 libraries####

library(readxl)
library(sciplot)
library(multcomp)
library(epiDisplay)
library(blorr)

library(arm)
##2 set wd####

setwd("C:/Users/cel/Horizon/EU_research/4 Papers/DECIDE_Mona_Therm_Trials/data_analysis")


##3. load datasets####


df1 <- read_excel("000_round1.xlsx")
df2 <- read_excel("000_round2.xlsx")

##4. round 1 analysis####

df <- as.data.frame(df1)


table(df$letter)
table(df$IV)


####

table(df$block,df$DV)
table(df$letter, df$DV, useNA = "always")
table(df$letter,df$DV_long)

bargraph.CI(letter,DV,data=df,ylim=c(0,1), xlab = "Lettertype", 
            ylab = "Installation success", col = c("grey", "darkgreen"))

###chi square
chisq.test(table(df$DV,df$letter)) 


### logistic regression


m1 <- glm(DV~letter, data=df, family=binomial(link="logit"))
summary(m1)

# Obtain standardized coefficients
standardized_coefs <- standardize(m1)
print(standardized_coefs)

##odd's ratio and conf intervals 
logistic.display(m1)


##variance accounted for in m1
pscl::pR2(m1)['McFadden']


##means and sds
aggregate( DV ~ letter, df, mean )
aggregate( DV ~ letter, df, sd )

rm(ci,standardized_coefs,coef,odds_ratio,se)


##5. round 2 analysis####

df <- df2

df <- subset(df, !df$status == "Installation technically not possible")

table(df$letter, df$status)

table(df$letter, df$success)
table(df$letter, df$success, df$block)


bargraph.CI(block,success,letter,data=df,ylim=c(0,1), 
            xlab = "letter", 
            ylab = "Installation success", 
            col=c("grey", "darkgreen"))


chisq.test(table(df$success,df$letter))


m2 <- glm(success~letter, data=df, family=binomial(link="logit"))
summary(m2)

# Obtain standardized coefficients
standardized_coefs <- standardize(m2)
print(standardized_coefs)

coef <- coef(m2)
se <- summary(m2)$coefficients[, 2]
odds_ratio <- exp(coef)
ci <- exp(confint.default(m2))
odds_ratio
ci

pscl::pR2(m2)['McFadden']


m3 <- glm(success~letter * block, data=df, family=binomial(link="logit"))
summary(m3)


# Obtain standardized coefficients
standardized_coefs <- standardize(m3)
print(standardized_coefs)


coef <- coef(m3)
se <- summary(m3)$coefficients[, 2]

# Calculate the odds ratios and their 95% confidence intervals
odds_ratio <- exp(coef)
ci <- exp(confint.default(m3))

# Combine the results into a data frame
results <- data.frame(
  odds_ratio = odds_ratio,
  ci_lower = ci[,1],
  ci_upper = ci[,2],
  se = se,
  z = coef/se,
  p_value = 2*pnorm(abs(coef/se), lower.tail=FALSE)
)

results

aggregate(success ~ Lettertype, df, mean)
aggregate(success ~ Lettertype, df, sd)


### fit of models through adjusted McFadden’s R-Squared for a logistic regression model --> Model2 fits way better
pscl::pR2(m2)['McFadden']



###6. script for data cleaning before analysis above


###replace the instances in df$apartment with unique identifiers
df$block <- as.factor(df$block)
# Create a lookup table that maps each unique name to a unique letter
lookup <- setNames(LETTERS[seq_along(unique(df$block))], unique(df$block))

# Replace blocks with letters A, B, C, etc.
df$block <- lookup[match(df$block, names(lookup))]
table(df$block)
#writexl::write_xlsx(df, "000_round2.xlsx")

