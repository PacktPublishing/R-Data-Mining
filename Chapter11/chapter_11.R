library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(e1071)

#random forest

training_data %>% 
  mutate(commercial_portfolio = as.factor(commercial_portfolio),
         business_unit = as.factor(business_unit))-> training_data_factor

set.seed(11)
random_forest <- randomForest::randomForest(formula = as.factor(default_numeric)~., 
                                            data = training_data_factor,
                                            ntree = 400, 
                                            importance = TRUE)

random_forest

plot(random_forest)

random_forest$err.rate %>% tail()

varImpPlot(random_forest)

## ensemble learning and confusion matrix

#logistic regression

logistic_df <- data.frame(y = logistic$y, fitted_values = logistic$fitted.values)

logistic_df %>% 
  mutate(default_threshold = case_when(as.numeric(fitted_values)>0.5 ~ 1,
                                       TRUE ~ 0)) %>% 
  dplyr::select(y, default_threshold)-> logistic_table


confusionMatrix(as.factor(logistic_table$default_threshold), as.factor(logistic_table$y))

# support vector machine

support_vector_data <- data.frame(predicted = support_vector_machine_linear$fitted,
                                  truth = as.numeric(training_data$default_numeric))

support_vector_data %>% 
  mutate(predicted_threshold = case_when(as.numeric(predicted)>0.5 ~ 1,
                                        TRUE ~ 0))-> support_vector_table
confusionMatrix(as.factor(support_vector_table$predicted_threshold), 
                as.factor(support_vector_table$truth))

# random forest

random_forest

confusionMatrix(as.factor(random_forest$predicted), as.factor(random_forest$y))


## comparing precision metric

data <- data.frame(model = c("logistic",
                             "support_vector",
                             "random_forest"), 
                   precision= c(8352/(164+8352),
                                8356/(160+8356),
                                7923/(7923+593)))

ggplot(data = data, aes(x = model,y = precision, label = round(precision,2)))+
  geom_bar(stat = 'identity')+
  geom_text()

# ensemble learning

ensemble_dataset <- data.frame(svm = (support_vector_table$predicted_treshold),
                               logistic = (logistic_table$default_threshold),
                               random_forest = as.numeric(as.character(random_forest$predicted)),
                               observed = as.numeric(training_data$default_numeric))

ensemble_dataset %>% 
  mutate(majority = case_when(svm + logistic + random_forest >= 2~ 1,TRUE ~ 0))-> ensemble_dataframe

confusionMatrix(as.factor(ensemble_dataframe$majority), as.factor(ensemble_dataframe$observed))


# predicting on new data

# predicting on new data
me_customer_list <- import("middle_east_customer_list.xlsx")

str(me_customer_list)
me_customer_list %>%
  select(corporation,
         previous_default,
         multiple_country,
         cost_income,
         ROE,
         employees,
         economic_sector,
         ROS,
         company_revenues,
         commercial_portfolio,
         business_unit) %>% 
  mutate(commercial_portfolio = as.factor(commercial_portfolio),
         business_unit = as.factor(business_unit))->wrangled_me_customer_list


levels(wrangled_me_customer_list$commercial_portfolio) <- levels(training_data_factor$commercial_portfolio) 
levels(wrangled_me_customer_list$business_unit) <- levels(training_data_factor$business_unit) 

wrangled_me_customer_list$logistic <- predict.glm(logistic,newdata = wrangled_me_customer_list)
set.seed(11)
wrangled_me_customer_list$random_forest <-predict(random_forest,newdata = wrangled_me_customer_list)
wrangled_me_customer_list$svm <- predict(support_vector_machine_linear,newdata = wrangled_me_customer_list)


wrangled_me_customer_list %>% 
  mutate(logistic_threshold = case_when(as.numeric(logistic)>0.5 ~ 1,
                                        TRUE ~ 0),
         svm_threshold = case_when(as.numeric(svm)>0.5 ~ 1,
                                   TRUE ~ 0)) %>% 
  mutate(ensemble_prediction = case_when(logistic_threshold+svm_threshold+ as.numeric(as.character(random_forest)) >=2 ~ 1,
                                       TRUE ~ 0)) ->  me_customer_list_complete

me_customer_list_complete %>% filter(ensemble_prediction == 1) -> defaulted_companies 

defaulted_companies %>% export("defaulted_companies.xlsx")

 
# save(defaulted,file ="../r_datamining/visure/defaults.rdata")

