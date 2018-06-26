library(tidyverse)
library(magrittr)

raw.data <- read_csv('https://raw.githubusercontent.com/brian-cuny/621week3/master/crime-training-data.csv') %>%
  mutate(chas = factor(chas))

#No missing data
raw.data %>%
  map_dbl(~sum(is.na(.))/nrow(raw.data))

sum(raw.data$target) / nrow(raw.data)
part <- caret::createDataPartition(raw.data$target, p=0.8, list=FALSE)
training <- raw.data %>%
  filter(row_number() %in% part)
testing <- raw.data %>%
  filter(!row_number() %in% part)
#use these to train/test

# Explore Individual Predictors -------------------------------------------

Predictor.Disp <- function(x){
  require(gridExtra)
  plot.1 <- ggplot(raw.data, aes_string(x)) +
    geom_density()
  
  plot.2 <- 
    raw.data %>%
      mutate(target = factor(target)) %>%
      ggplot(aes_string('target', x)) +
      geom_boxplot() +
      geom_point(alpha=0.2)
  
  grid.arrange(plot.1, plot.2, ncol=2)
}

Predictor.Disp('zn')
# When conducting a binary regression with a skewerd predictor, it is often easiest to asses the need for x and log(x) by inclding them both in the model

Predictor.Disp('indus')
# Bimodal distribtuion. Split into categorical data? 

#Chas is categorical
#When x is a dummy varaiable, it can be shown that the log odds are also a linear function of x.

Predictor.Disp('nox')
#Roughly normal with different variance. Quadraditic?

Predictor.Disp('rm')
#Roughly normal distribution with roughly similar distribution. NO CHANGE

Predictor.Disp('age')
#Very skewed, include log?

Predictor.Disp('dis')
#Skewed, include log

Predictor.Disp('rad')
#Highly bimodal, split into two?

Predictor.Disp('tax')
#Highly bimodal, split into two?

Predictor.Disp('ptratio')
#Skew, include log?

Predictor.Disp('black')
#Highly, highly skew, include log

Predictor.Disp('lstat')
#Roughly normal distribution with roughly similar distirubtion. NO CHANGE

Predictor.Disp('medv')
#Roughly normal distribution with roughly similar distirubtion. NO CHANGE

raw.data.cat <- raw.data %>%
  mutate(chas.cat = factor(chas),
         zn.cat = factor(ifelse(zn == 0, 0, 1)),
         indus.cat = factor(ifelse(indus < 14, 0, 1)),
         rad.cat = factor(ifelse(rad <= 15, 0, 1)),
         tax.cat = factor(ifelse(tax <= 540, 0, 1)),
         black.cat = factor(ifelse(black <= 390, 0, 1))) %>%
  select(-chas, -zn, -indus, -rad, -tax, -black)



# Model 1: Only added categorical -----------------------------------------
model.all <- glm(target ~ . , data=raw.data.cat, family=binomial)
summary(model.all)
#tax.cat, rad.cat, indus.cat, rm
model.all <- glm(target ~ . -tax.cat -rad.cat -indus.cat -rm -lstat, data=raw.data.cat, family=binomial)

Marginal.Model.One <- function(p, yhat){
  ggplot(raw.data, aes_string(p, 'target')) +
    geom_point() + 
    geom_smooth(method='loess', se=FALSE) +
    geom_smooth(aes(y=yhat), method='loess', se = FALSE, color='red')
}

yhat <- predict(model.all, type='response')
Marginal.Model.One('nox', yhat)
#marginal for nox is poor

Marginal.Model.One('age', yhat)
#marginal for age is poor

Marginal.Model.One('dis', yhat)
#very poor

Marginal.Model.One('ptratio', yhat)
#poor

Marginal.Model.One('medv', yhat)
#good

#Due to the poor match on the marginal model plot, this model is not valid. There are a  number of suggested components to add to account for the difference, as
#suggested by the text book


# Model 2: Added categorical and suggested modifications from book --------
raw.data.log <- raw.data.cat %>%
  mutate(l.nox = log(nox),
         l.age = log(age),
         l.dis = log(dis),
         l.ptratio = log(ptratio))

model.2 <- glm(target ~ ., data=raw.data.log, family=binomial)
summary(model.2)
MASS::stepAIC(model.2)

model.2 <- glm(target ~ nox + age + ptratio + medv + chas.cat + rad.cat + tax.cat + black.cat + l.age + l.dis + ptratio*l.dis, data=raw.data.log, family=binomial)

yhat <- predict(model.2, type='response')

Marginal.Model.Two <- function(p, yhat){
  ggplot(raw.data.log, aes_string(p, 'target')) +
    geom_point() + 
    geom_smooth(method='loess', se=FALSE) +
    geom_smooth(aes(y=yhat), method='loess', se = FALSE, color='red')
}

Marginal.Model.Two('nox', yhat)
Marginal.Model.Two('age', yhat)
Marginal.Model.Two('ptratio', yhat)
Marginal.Model.Two('medv', yhat)
Marginal.Model.Two('l.age', yhat)
Marginal.Model.Two('l.dis', yhat) #poor

#Is there a relationship between ptratio and dis?
ggplot(raw.data.log, aes(ptratio, dis, group=factor(target))) +
  geom_point() +
  geom_smooth(method='lm')

#Added relationship between two variables to the data


#  -------------------------------------------------------------
model.3 <- glm(target ~ nox + age + ptratio + medv + chas.cat + rad.cat + tax.cat + black.cat + l.age + l.dis, data=raw.data.log, family=binomial)

car::vif(model.3)

sum(raw.data.log$rad.cat == raw.data.log$tax.cat)

#Since rad and tax are identical, we will drop 1

model.3 <- glm(target ~ nox + age + ptratio + medv + chas.cat + tax.cat + black.cat + l.dis, data=raw.data.log, family=binomial)
summary(model.3)

MASS::stepAIC(model.3)

yhat <- predict(model.3, type='response')

Marginal.Model.Three <- function(p, yhat){
  ggplot(raw.data.log, aes_string(p, 'target')) +
    geom_point() + 
    geom_smooth(method='loess', se=FALSE) +
    geom_smooth(aes(y=yhat), method='loess', se = FALSE, color='red')
}

Marginal.Model.Three('nox', yhat)
Marginal.Model.Three('age', yhat)
Marginal.Model.Three('ptratio', yhat)
Marginal.Model.Three('medv', yhat)
Marginal.Model.Three('l.dis', yhat)


# rad, ptratio and tax all super highly correlated with a group of elements that are all identical

car::mmps(model.all)

# moded glm ---------------------------------------------------------------
moded.all <- glm(target ~ ., data=raw.data, family=binomial)
summary(moded.all)

ggplot(raw.data, aes(zn, target)) +
  geom_point() + 
  geom_smooth(method='loess', se=FALSE) +
  geom_smooth(aes(zn, predict(model.all, type='response')), method='loess', se = FALSE, color='red')



#Initial GLM with all attributes
model.all <- glm(target ~ ., data=raw.data, family=binomial)
summary(model.all)
probabilities <- predict(moded.all, type='response')

car::residualPlots(moded.all)

numeric.raw.data <- moded.raw.data %>%
  select_if(is.numeric)

predictors <- colnames(numeric.raw.data)

numeric.raw.data <- raw.data.log %>%
  select(-l.age, -dis, -l.nox, -l.ptratio, -lstat, -rm) %>%
  select_if(is.numeric) %>%
  mutate(logit = log(probabilities / (1 - probabilities))) %>%
  gather(key='predictors', value='predictor.value', -logit)

ggplot(numeric.raw.data, aes(logit, predictor.value)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method='loess') +
  theme_bw() + 
  facet_wrap(~predictors, scale='free_y')


arm::binnedplot(predict(model.3), resid(model.3))
plot(model.3)
faraway::halfnorm(hatvalues(model.3))

table <- table(predicted = ifelse(predict(model.3, type='response') < .5, 0, 1), actual = raw.data$target)

caret::confusionMatrix(table)















