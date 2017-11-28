#library loading
library(rio)
library(dplyr)
library(ggplot2)
library(energy)
# import cash_flows dataset
cash_flow_report <- import("data/cash_flow.csv") 

cash_flow_report %>% 
  select(cash_flow) %>% 
  unlist() %>% 
  fivenum()


cash_flow_report %>% 
  select(cash_flow) %>% 
  unlist() %>% 
  IQR()


cash_flow_report %>% 
  select(cash_flow) %>% 
  unlist() %>% 
  mean() -> mean


cash_flow_report %>% 
  select(cash_flow) %>% 
  unlist() %>% 
  median() -> median

mean - median

cash_flow_report %>% 
  select(cash_flow) %>% 
  unlist() %>% 
  var()

cash_flow_report %>% 
  select(cash_flow) %>% 
  unlist() %>% 
  sd()

# skewness

cash_flow_report %>% 
  select(cash_flow) %>% 
  unlist() %>% 
  fivenum()-> quartiles

q <- quartiles[2:4]

skewness <- ((q[3]- q[2])-(q[2]- q[1]))/(q[3]-q[1])

#relation among variables

cor (x =cash_flow_report$y,  y = cash_flow_report$cash_flow)
#time variable transformation: we have to assign a progressive number to each unique value of quarter

oldest <- min(cash_flow_report$y)

cash_flow_report %>% 
  mutate(delays = difftime(cash_flow_report$y, oldest, units = "days")) -> cash_flow_report_mutation

cor(x = as.numeric(cash_flow_report_mutation$delays),
    y = cash_flow_report_mutation$cash_flow)

dcor(cash_flow_report_mutation$delays, cash_flow_report_mutation$cash_flow)

# graphical eda

ggplot(data = cash_flow_report,aes(x))+
  geom_histogram(stat = 'count')

ggplot(data = cash_flow_report,aes(y)) +
  geom_histogram(stat = 'count')

ggplot(data = cash_flow_report,aes(cash_flow))+
  geom_histogram()

ggplot(data = cash_flow_report,aes(cash_flow))+
  geom_histogram(bins = 70)



boxplot(x = cash_flow_report$cash_flow, horizontal = TRUE)

boxplot.stats(x = cash_flow_report$cash_flow)

stats <- boxplot.stats(x = cash_flow_report$cash_flow)
outliers <- stats$out
cash_flow_report %>% 
  filter(cash_flow == outliers[3])

#scatterplots

cash_flow_report %>% 
  ggplot(aes(x = y, y = cash_flow))+
  geom_point()

cash_flow_report %>% 
  ggplot(aes(x = y, y = cash_flow, group = x, colour = x))+
  geom_point()

cash_flow_report %>% 
  ggplot(aes(x = y, y = cash_flow, group = x, colour = x))+
  geom_point()+
  geom_line()+
  labs(title = "cash flows over time by region", 
       subtitle="quarterly data from 2014 to Q2 2017",
       caption = "source: cash_flow_report")+
  xlab("quarter of reporting")+
  ylab("recorded cash flows (euro)")+
  annotate("text", label = "the middle east cash flow series \n shows a unprecedent drop on the Q2 2017",
           x = "2017-07-01" , y= 40000 ,hjust = 1, vjust =0)

cash_flow_report %>% 
  ggplot(aes(x = y, y = cash_flow, group = x, colour = x == "middle_east"  ))+
  geom_point()+
  geom_line( alpha = .2)+
  labs(title = "cash flows over time by region", 
       subtitle="quarterly data from 2014 to Q2 2017, middle east data in red",
       caption = "source: cash_flow_report")+
  xlab("quarter of reporting") +
  ylab("recorded cash flows (euro)") +
  annotate("text", label = "the middle east cash flow series \n shows a unprecedent drop on the Q2 2017",
           x = "2017-07-01" , y= 40000 ,hjust = 1, vjust =0) +
  scale_colour_manual(values = c("grey70", "red")) + 
  theme_minimal()+
  theme(legend.position = "none")


cash_flow_report %>% 
  group_by( y) %>% 
  summarise(cash_flow = sum(cash_flow)) %>%
  rename(date = y) %>% 
  ggplot(aes(x = date, y = cash_flow, group = 1))+
  geom_line()+
  geom_point()+
  labs(title = "cash flows by quarter")




