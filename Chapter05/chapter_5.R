# importing data

library(rio)
library(dplyr)
library(tidyr)

cash_flow_report <- import("data/cash_flow.csv")
customer_list    <- import("data/customer_list.txt")
stored_data      <- import("data/stored_data.rds")

# analysing the structure of your data

str(cash_flow_report)
str(customer_list)
str(stored_data)

Hmisc::describe(cash_flow_report)
Hmisc::describe(customer_list)
Hmisc::describe(stored_data)

# every row is a record

stored_data$customer_code %>% 
  length()

stored_data$customer_code %>% 
  unique() %>% 
  length()

# the tidyr package

city        <- c(rep("london",4),rep("moscow",4),rep("new_york",4), rep("rome",4))
date        <- c(rep(c("june_2016","july_2016","august_2016","september_2016"),4))
temperature <- c(15,18,19,18,17,20,19,11,22,26,26,23,23,27,26,22) #source : wunderground.com
temperature_table <- data.frame(city,date,temperature)

temperature_table %>% 
  spread(key = date, value= temperature)-> wide_temperature

wide_temperature %>% 
  gather(key = "date", value = "temperature", -city)

wide_temperature %>% 
  gather(key = "date", value = "temperature", 2:5)

time <- c('15/05/217 12:25:33',
          '15/05/217 13:25:33',
          '15/05/217 14:25:33',
          '15/05/217 15:25:33')
temperature <- c(16,
                 16,
                 17,
                 15)
really_dirty_data <- data.frame(time,temperature)

really_dirty_data %>% 
  separate(time, sep  = " ", into = c("date", "hour"))

stored_data %>% 
  spread(key = parameter, value = value) -> clean_stored_data
  
# performing data validation on our data
  str(clean_stored_data)
  
  clean_stored_data$attr_5 %>% 
    as.numeric() %>% 
    mode()
  
  clean_stored_data %>% 
    mutate(attr_5 = as.numeric(attr_5)) %>% 
    str()
  
  
  clean_stored_data %>% 
    mutate(attr_5 = as.numeric(attr_5)) -> clean_casted_stored_data
  
  is.data.frame(clean_stored_data$attr_5)
  
  seq(1:10) %>% 
    range() %in% seq(1:10)
  
  
  domain_test <- seq(1:10) < 12 & seq(1:10) > 2  
    
  domain_test %>% 
    table()
  
  attr_domain_test <- clean_casted_stored_data$attr_8 > 0 
  
  attr_domain_test %>% 
    table()
  
c("A", "C","F", "D", "A") %in% c("A", "D", "E")

str(clean_casted_stored_data)

(clean_casted_stored_data$default_flag) %>%
table()

(clean_casted_stored_data$default_numeric) %>%
  table()

clean_casted_stored_data %>% 
  filter(default_flag == 0 |  default_flag == 1 ) %>% 
  nrow()

clean_casted_stored_data %>% 
  filter(default_flag == 0 |  default_flag == 1 ) -> clean_casted_stored_data_validated

clean_casted_stored_data %>% 
  filter(default_flag != 0 &  default_flag != 1 ) -> question_mark_records

left_join(clean_casted_stored_data_validated,customer_list, by = "customer_code") %>% 
  str()


left_join(clean_casted_stored_data_validated,customer_list, by = "customer_code") ->
  clean_casted_stored_data_validated_complete


