library(pls)
library(car)
library(MASS)

x1 <- c(119,134,183,126,177,107,119,165,156)
x2 <- c(328.5,406,460.5,390,434.5,362.5,325.5,387.5,371)
x3 <- c(715.8,792.8,981.6,734.2,951.4,688.4,715.8,904,876.2)

cor_data <- data.frame(x1,x2,x3)

round(cor(cor_data),3)

summary(lm(x1~x3)) -> f

clean_casted_stored_data_validated_complete %>% 
  dplyr::select(-default_flag) %>% 
  dplyr::select(-customer_code) -> training_data

multiple_regression <- lm(as.numeric(default_numeric)~., data= training_data)
summary(multiple_regression)  


clean_casted_stored_data_validated_complete$customer_agreement %>% unique()
clean_casted_stored_data_validated_complete$subsidiary %>% unique()

clean_casted_stored_data_validated_complete %>% 
  dplyr::select(-default_flag) %>% 
  dplyr::select(-customer_code) %>% 
  dplyr::select(-c(customer_agreement, subsidiary))-> training_data

multiple_regression_new <- lm(as.numeric(default_numeric)~., data= training_data)

vif(multiple_regression_new)

training_data %>% 
  mutate(ln_cost_income = log(cost_income),
         ln_ROE         = log(ROE),
         ln_employees   = log(employees),
         ln_ROS         = log(ROS),
         ln_company_revenues = log(company_revenues)) %>% 
  select(-c(cost_income,ROE,employees,ROS,company_revenues))-> training_log_data

training_data %>% select(cost_income, ROE,ROS) %>% summary()


training_data %>% 
  mutate(ln_employees   = log(employees),
         ln_company_revenues = log(company_revenues)) %>% 
  select(-c(cost_income,ROE,employees,ROS,company_revenues))-> training_log_data

lm(as.numeric(default_numeric)~.,data = training_log_data) %>% durbinWatsonTest()
lm(as.numeric(default_numeric)~.,data = training_log_data) %>% ncvTest()

training_data[20,]-> row
row %>% mutate(company_revenues = round(company_revenues*1000,2),
               cost_income = round(cost_income,1)) -> new_data

predict.lm(multiple_regression_new, newdata = new_data)
## principal component regression

pcr_regression <- pcr(as.numeric(default_numeric)~., data = training_data)
summary(pcr_regression)
plot(R2(pcr_regression))
R2(pcr_regression)
## stepwise regression


stepwise_regression <- stepAIC(multiple_regression_new, direction = "both", trace = TRUE)
stepwise_regression$anova
summary(stepwise_regression)






