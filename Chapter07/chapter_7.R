library(rio)
library(dplyr)
library(ggplot2)
library(car)

colnames(clean_casted_stored_data_validated_complete)
colnames(clean_casted_stored_data_validated_complete) <- c("corporation",
                                                           "subsidiary" ,
                                                           "previous_default" ,
                                                           "customer_agreement",
                                                           "multiple_country",
                                                           "default_numeric",     
                                                            "default_flag" ,
                                                            "customer_code",
                                                           "cost_income",       
                                                            "ROE",
                                                            "employees",
                                                           "economic_sector",
                                                           "ROS" ,
                                                           "company_revenues",
                                                           "commercial_portfolio",
                                                           "business_unit")


  linear_regression_economic_sector <- lm(as.numeric(default_numeric) ~ economic_sector, clean_casted_stored_data_validated_complete)
  ncvTest(linear_regression_economic_sector)
  durbinWatsonTest(linear_regression_economic_sector)
  
  linear_regression_revenues <- lm(as.numeric(default_numeric) ~ company_revenues, clean_casted_stored_data_validated_complete)
  ncvTest(linear_regression_revenues)
  durbinWatsonTest(linear_regression_revenues)
  
  clean_casted_stored_data_validated_complete %>% 
    dplyr::select(-default_flag) %>% 
    dplyr::select(-customer_code) %>% 
    dplyr::select(-c(customer_agreement, subsidiary)) %>% 
    dplyr::mutate(default_numeric = as.numeric(default_numeric))-> training_data
  
#training_data %>% 
 # mutate(default_numeric = as.numeric(default_numeric))%>% 
  #select(- default_flag, - customer_code)-> training_data 

mode(linear_regression_revenues)
str(linear_regression_revenues)

linear_regression_economic_sector %>% str(1)

show_lm <- data.frame("y"    = c(linear_regression_revenues$model$`as.numeric(default_numeric)`,
                                 as.numeric(linear_regression_revenues$fitted.values)),
                      "type" = c(rep("observed",nrow(clean_casted_stored_data_validated_complete)),
                               rep("fitted",nrow(clean_casted_stored_data_validated_complete))),
                      "revenues"    = c(rep(linear_regression_revenues$model$company_revenues,2
                      )))

show_lm %>% 
  ggplot(aes(x = revenues, y = y))+
  geom_point()

show_lm %>% 
  ggplot(aes(x = revenues, y = y, colour =type))+
  geom_point()

show_lm_residuals <- data.frame(residuals = linear_regression_revenues$residuals,
                                x  = seq(1:length(linear_regression_revenues$residuals)))

