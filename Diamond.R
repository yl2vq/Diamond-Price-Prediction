setwd("/Users/beautifulmonster/Desktop/STAT6021")
diamond <- read.csv('clean_diamond_data.csv')
head(diamond)
library(MASS)
library(ggplot2)

##summary for response variable vs each predictors regression
#carat
summary(lm(price~carat, data=diamond))
#cut
summary(lm(price~cut, data=diamond))
#color 
summary(lm(price~color, data=diamond))
#clarity
summary(lm(price~clarity, data=diamond))

##plot for price vs carat
ggplot(diamond, aes(x=carat, y=price)) + geom_point() + theme_bw()

## model after transformation (price --> (price)^0.5)
#response variable transformation
diamond$price.trans <- (diamond$price)^(0.5)
#fit the straight line model
basicfit = lm(price.trans ~ carat, data = diamond)
summary(basicfit)
# plot log(price) vs carat
ggplot(diamond, aes(x=carat, y=price.trans)) + geom_point() + theme_bw()

##plots for Assumption check
#external studentized residual calculation
extern_s_resids <- studres(basicfit)
#residual vs predicted response variable plot(linearity, constant variance check)
plot(fitted.values(basicfit), extern_s_resids)
#QQplot(normality)
qqnorm(extern_s_resids)
qqline(extern_s_resids)
#histogram(normality)
hist(extern_s_resids)

## boxcox procedure to the orifinal model
bc_res <- boxcox(price ~ carat, data = diamond)
best_lambda <- bc_res$x[bc_res$y == max(bc_res$y)] 
cat(best_lambda)

## apply best lambda to the response variable and fit the model
diamond$transformed_y <- (diamond$price^best_lambda - 1)/best_lambda
final_mod <- lm(transformed_y ~ carat, data=diamond)

##Assumption check
extern_s_resids <- studres(final_mod)
hist(extern_s_resids)
plot(fitted.values(final_mod), extern_s_resids)
hist(extern_s_resids)
qqnorm(extern_s_resids)


## the second transformation: price --> log(price), carat --> log(carat)
diamond$price.trans <- log(diamond$price)
diamond$carat.trans <- log(diamond$carat)
#fit the model
diamondfit <- lm(price.trans ~ carat.trans , data=diamond)
summary(diamondfit)
#Assumption test with the new model
extern_s_resids <- studres(diamondfit)
qqnorm(extern_s_resids)
hist(extern_s_resids)
plot(fitted.values(diamondfit), extern_s_resids)
plot(diamondfit)

## Partial F test
big.mod <- lm(price.trans ~ carat.trans + color + cut + clarity, data=diamond)
lil.mod <- lm(price.trans ~ carat.trans, data=diamond)
anova(lil.mod, big.mod)
summary(anova(lil.mod, big.mod))

#fit the final model 
final_model <- lm(price.trans ~ carat.trans + color + cut + clarity, data=diamond)
summary(final_model)
## Final Assumption check
extern_s_resids <- studres(final_model)
qqnorm(extern_s_resids)
qqline(extern_s_resids)
hist(extern_s_resids)
plot(fitted.values(final_model), extern_s_resids)




