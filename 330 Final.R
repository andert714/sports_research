library(tidyverse)
knitr::opts_chunk$set(echo = FALSE, message = FALSE)

df <- read_csv("Data/PineBeetle2.csv") %>% 
  mutate(Infested = ifelse(Infested == "Yes", 1, 0)) %>% 
  mutate_if(is.character, as.factor)

df %>% 
  pivot_longer(c(January, August_max, Slope, Elev, Precip)) %>% 
  ggplot(aes(x = value, y = Infested)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~name, scales = "free")

best_model <- df %>% 
  as.data.frame %>% 
  bestglm::bestglm(
    family = binomial,
    IC = "AIC",
    method = "exhaustive"
  )

glm <- glm(
  Infested ~ January + August_max + Slope + Elev + Precip + NC + SE + SW,
  family = binomial,
  data = df
)

df %>% 
  pivot_longer(c(January, August_max, Slope, Elev, Precip)) %>% 
  ggplot(aes(x = value, y = Infested)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~name, scales = "free")

glm %>% 
  confint %>% 
  as.data.frame %>% 
  rownames_to_column("Variable") %>% 
  transmute(
    Variable = Variable,
    Lower = `2.5 %`,
    Point = glm$coefficients,
    Upper = `97.5 %`
  ) %>% 
  knitr::kable(
    digits = 4,
    caption = "95% confidence intervals for model parameters"
  )

jan_effect <- tibble(
  lwr = confint(glm)["January", "2.5 %"],
  point = coefficients(glm)["January"],
  upr = confint(glm)["January", "97.5 %"]
) %>% 
  mutate_all(exp) %>% 
  mutate_all(round, 4)

fitted <- predict.glm(glm, type="response")
roc <- pROC::roc(df$Infested, fitted)
auc <- roc$auc %>% 
  round(4)
thresh <- seq(from=0, to=1, length=1000) 
misclass <- rep(NA,length=length(thresh))
for(i in 1:length(thresh)) {
  my.classification <- ifelse(fitted>thresh[i], 1, 0)
  misclass[i] <- mean(my.classification!=df$Infested)
}
thresh_point <- thresh[which.min(misclass)]

ggplot() + 
  geom_line(aes(x = 1 - roc$specificities, y = roc$sensitivities)) + 
  geom_abline(slope = 1, intercept = 0) +
  labs(
    title = "ROC curve",
    x = "1 - Specificity",
    y = "Sensitivity"
  )

table(
  df$Infested,
  ifelse(fitted > thresh_point, 1, 0)
) %>% 
  addmargins %>% 
  `row.names<-`(c("Actual No", "Actual Yes", "Sum")) %>% 
  knitr::kable(
    col.names = c("Predicted No", "Predicted Yes", "Sum"),
    caption = "Confusion Matrix"
  )

## Choose number of CV studies to run in a loop & test set size
n.cv <- 100
n.test <- round(.1*nrow(df))

## Initialize matrices to hold CV results
sens <- rep(NA, n.cv)
spec <- rep(NA, n.cv)
ppv <- rep(NA, n.cv)
npv <- rep(NA, n.cv)
auc <- rep(NA, n.cv)

## Begin for loop
for(cv in 1:n.cv){
  ## Separate into test and training sets
  test.obs <- sample(1:nrow(df), n.test)
  
  test.set <- df[test.obs,]
  train.set <- df[-test.obs,]
  
  ## Fit best model to training set
  train.model <- glm(
    Infested ~ January + August_max + Slope + Elev + Precip + NC + SE, 
    data=train.set, 
    family=binomial
  )
  
  ## Use fitted model to predict test set
  pred.probs <- predict.glm(train.model,newdata=test.set, type="response") #response gives probabilities
  
  ## Classify according to threshold
  test.class <- ifelse(pred.probs>thresh_point, 1, 0)
  
  ## Create a confusion matrix
  conf.mat <- addmargins(table(factor(test.set$Infested), factor(test.class)))
  
  ## Pull of sensitivity, specificity, PPV and NPV using bracket notation
  sens[cv] <- conf.mat[2,2]/conf.mat[2,3]
  spec[cv] <- conf.mat[1,1]/conf.mat[1,3]
  ppv[cv] <- conf.mat[2,2]/conf.mat[3,2]
  npv[cv] <- conf.mat[1,1]/conf.mat[3,1]
  
  ## Calculate AUC
  auc[cv] <- pROC::auc(pROC::roc(test.set$Infested, pred.probs))
}

cv_df <- tibble(
  Sensitivity = sens,
  Specificity = spec,
  `Positive Predicted Value` = ppv,
  `Negative Predicted Value` = npv,
  `Area under curve` = auc
)

cv_df %>% 
  pivot_longer(everything()) %>% 
  ggplot() +
  geom_density(aes(x = value)) +
  facet_wrap(~name, ncol = 2, scales = "free")

cv_df %>% 
  summarize_all(mean) %>% 
  round(4) %>% 
  t %>% 
  knitr::kable(
    col.names = "Measure",
    caption = "Cross Validation Metrics"
  )
future <- tibble(
  January = c(-13.98, -17.80, -17.27, -12.52, -15.99, -11.97, -15.75, -16.19, -17.87, -12.44),
  August_max = c(15.89, 18.07, 16.74, 18.06, 18.23, 15.81, 16.85, 16.51, 17.84, 16.96),
  Slope = 18.07,
  Elev = 1901.95,
  Precip = c(771.13, 788.54, 677.63, 522.77, 732.32, 615.96, 805.90, 714.57, 740.50, 801.22),
  NC = factor("No"),
  NW = factor("No"),
  EC = factor("No"),
  WC = factor("No"),
  SE = factor("Yes"),
  SC = factor("No"),
  SW = factor("No")
)

predict.glm(glm, newdata = future, type="response") %>% 
  enframe() %>% 
  transmute(
    Year = 2018:2027,
    Prediction = round(value, 4)
  ) %>% 
  knitr::kable(
    caption = "Predictions"
  )

