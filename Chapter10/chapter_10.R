
library(MASS)
library(e1071)
library(ggplot2)
library(dplyr)
library(lmtest)
clean_casted_stored_data_validated_complete %>% 
  dplyr::select(-default_flag) %>% 
  dplyr::select(-customer_code) %>% 
   dplyr::select(-c(customer_agreement, subsidiary)) %>% 
  dplyr::mutate(default_numeric = as.numeric(default_numeric))-> training_data

education <- data.frame(n = c(1,1,2,2,2,1,3),
                        degree = c("high school","high school", "university",
                                   "university", "university","high school","doctor of philosophy"),
                        family_income = c(93922,63019,51787,31954,42681,50378,66107),
                        parents_highest_degree = c("university","high school", "university" , "high school" , "high school" ,  "doctor of philosophy", "university"))

lm_model <- lm(n~ family_income + parents_highest_degree, data = education)


logistic <- glm(as.numeric(default_numeric)~ . ,data = training_data, family ="binomial")

logistic$coefficients['company_revenues']

logistic$model %>% 
  dplyr::select(company_revenues) %>% # we call select this way to avoid overlapping issues with the select function from MASS package which is also loa
  cbind(probability = logistic$fitted.values,.)-> dataviz_dataset

dataviz_dataset %>% 
  ggplot(aes(x = company_revenues,y = probability)) +
  geom_point()

# validating assumption related to the linearity of log odds

logistic_quadratic <- glm(as.numeric(default_numeric) ~ . + 
                            cost_income^2 + 
                            ROE^2 + 
                            employees^2 + 
                            ROS^2 + 
                            company_revenues^2 , 
                          data = training_data, family = "binomial")

lrtest(logistic_quadratic ,logistic)



linear_glm <- glm(as.numeric(default_numeric)~.,data = training_data, family = "gaussian")
summary(linear_glm)

linear_glm$coefficients
multiple_regression_new$coefficients

predict(lm_model,newdata = data.frame(family_income = 140000, parents_highest_degree = "university"))

support_vector_machine_linear <- svm(default_numeric ~ ., data = training_data, kernel = "linear")
weights <- t(support_vector_machine_linear$coefs) %*% support_vector_machine_linear$SV


