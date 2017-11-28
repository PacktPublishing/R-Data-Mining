library(dplyr)

multiple_regression_new$residuals
multiple_regression_new$residuals^2 %>% 
  mean() %>% 
  sqrt()


multiple_regression_new %>% summary()

r2.0 <- function(sig){
  x <- seq(1,10,length.out = 100)        # our predictor
  y <- 2 + 1.2*x + rnorm(100,0,sd = sig) # our response; a function of x plus some random noise
  summary(lm(y ~ x))$r.squared
  #sum((summary(lm(y~x))$residuals)^2)# print the R-squared value
}

x <- seq(1,100)
set.seed(29)
y <- 2 + 1.2*x + rnorm(100,0,sd = 100)
regression <- lm(y ~ x)
durbinWatsonTest(regression)
summary(regression) -> summary_regression
summary_regression$r.squared


View(table(iris$Petal.Length,iris$Sepal.Width,iris$Species))

outcome <- c(TRUE,FALSE)
set.seed(4)
data <- data.frame(id=seq(1:100),
                   observed = sample(outcome , size= 100, replace = TRUE),
                   predicted= sample(outcome, size = 100, replace = TRUE))

table(data$observed,data$predicted)

table(data$observed,data$predicted) %>% as.matrix()
table(data$observed,data$predicted) %>% as.data.frame()

table(data$observed,data$predicted) %>% 
  as.data.frame() %>% 
  rename(observed = Var1,
         predicted = Var2)-> confusion_matrix

confusion_matrix %>% 
  mutate(label = c("true_negative","false_negative","false_positive","tru_negative"))

confusion_matrix %>% 
  mutate(label = c("true_negative","false_negative","false_positive","true_positive")) %>% 
  dplyr::select(label,Freq) %>% 
  spread(key = label, value = Freq) -> casted_confusion_matrix


casted_confusion_matrix %>% 
  mutate(accuracy = (true_negative+true_positive)/ (true_negative+
                                                      true_positive+
                                                      false_negative + 
                                                      false_positive))

casted_confusion_matrix %>% 
  mutate(sensitivity = true_positive/(true_positive + false_negative))

casted_confusion_matrix %>% 
  mutate(specificity = true_negative/(true_negative + false_positive))
