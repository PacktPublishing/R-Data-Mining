install.packages("rio")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
library(rio)
library(lubridate)
library(dplyr)
library(ggplot2)
movements <- rio::import(file = "data/banking.xls")
movements %>% head() %>% View()

movements %>% 
  mutate(date_new = as.Date(movements$date, origin = "1899-12-30")) %>% 
  mutate(day_of_week = wday(date_new)) %>% 
  mutate(month = month(date_new)) -> movements_clean


week_decode <- data.frame(
  day_of_week      = c(1:7),
  name_of_the_day  = c("sunday", "monday", "tuesday", "wednesday","thursday","friday","saturday" ),
  weekend          = c("weekend","workday","workday","workday","workday","workday","weekend")
)

movements_clean <- merge.data.frame(movements_clean,week_decode, by = "day_of_week")
# alternative dplyr code: movements_clean <- inner_join(movements_clean,week_decode, by = "day_of_week")

movements_clean %>% 
  group_by(month) %>% 
  summarise(number_of_movements = n(),
            sum_of_entries = sum(income, na.rm = TRUE),
            sum_of_expenses = sum(expenditure, na.rm = TRUE)) -> monthly_summary

movements_clean %>% 
  group_by(name_of_the_day) %>% 
  summarise(number_of_movements = n(),
            sum_of_entries = sum(income, na.rm = TRUE),
            sum_of_expenses = sum(expenditure, na.rm = TRUE)) -> daily_summary

 View(daily_summary ) 
 
 monthly_summary %>% 
   ggplot(aes( x = month)) +
   geom_bar(aes(y = sum_of_entries), stat = 'identity')+
   geom_bar(aes( y = sum_of_expenses), stat = 'identity')+
   coord_flip()
 
#basic viz = movements per day
 
 daily_summary %>% 
   ggplot(aes(x = name_of_the_day,y = number_of_movements)) +
   geom_bar(stat = 'identity')

 #better to rotate bars 
daily_summary %>% 
  ggplot(aes(x = name_of_the_day,y = number_of_movements)) +
  geom_bar(stat = 'identity') +
  coord_flip()

#advanced viz = movements per day, size of the point from total absolute amount

daily_summary %>% 
  ggplot(aes(x = name_of_the_day, y = number_of_movements, label = number_of_movements)) +
  geom_linerange(aes(ymin = 0, ymax = number_of_movements)) +
  geom_point(aes(size = (sum_of_entries + sum_of_expenses)/number_of_movements))+
  geom_text(nudge_y  = 1.7)+
  coord_flip()



