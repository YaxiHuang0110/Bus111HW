# Question 1
# a)
library("coronavirus")
# b)
data = read.csv("/Users/annie/Desktop/coronavirus.csv")
first_100_elements = head(data,100)
# c)
# date - The date of the summary
# province - The province or state, when applicable
# country - The country or region name
# lat - Latitude point
# long - Longitude point
# type - the type of case (i.e., confirmed, death)
# cases - the number of daily cases (corresponding to the case type)

# Question 2
# a) 
library("dplyr")
summary_df = coronavirus %>%
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarize(total_cases = sum(cases)) %>%
  arrange(-total_cases)
summary_df %>% head(20)

# b) bar graph
library(ggplot2)
top_5_countries = summary_df %>%
  arrange(desc(total_cases)) %>%
  top_n(5)
top_5_countries_plot = ggplot(data = top_5_countries, aes(x=country, y=total_cases))+
  geom_bar(stat = "identity", fill="red",color="black")+
  
  # c) horizontal barplot
  coord_flip()+
  
  # d) add title
  ggtitle("Top 5 countries by total cases")

# 1. By adding color="black", the bar graph's color of lines is set directly to black (extra point)
# 2. By adding fill="red", the color of filled objects, here the bars become red (extra point)

# Question 3
# a)
library(tidyr)
recent_cases = coronavirus %>%
  group_by(date) %>%
  summarize(daily_cases = sum(cases)) %>%
  arrange(date)

# b)
recent_caes_line_graph = ggplot(data = recent_cases, aes(x=date, y=daily_cases))+
  geom_line(stat = "identity")+
  
  # By adding labs function which gives names for the  line graph and x,y axis (extra credit)
  labs(x="Date", y="Total Confirmed Cases", title="Recent Cases")+
  
  # By implementing color function in ggplot, the line changes the color into purple (extra credit)
  geom_line(color="purple")+
  
  # By adding linetype="dashed", the line graph becomes dashed (extra credit)
  geom_line(linetype="dashed")+
  
  # I add geom_point and put the size as 0.8 which shows the highest or lowest point clearer by having a small bullet point(extra credit)
  geom_point(size=0.8)



