library(readr)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(plyr)
library(tidyr)
data = read_csv("C:\\Users\\Kartik\\Desktop\\school\\mfe\\project-R\\startup_funding.csv", na=c("N/A", "Undisclosed", "NaN", "undisclosed", "unknown"))
print(spec(data))
#Data Cleaning
data$`Industry Vertical`[data$`Industry Vertical` == 'ECommerce'] = "eCommerce"
data$`Industry Vertical`[data$`Industry Vertical` == 'E-Commerce'] = "eCommerce"
data$`Industry Vertical`[data$`Industry Vertical` == 'E-commerce'] = "eCommerce"
data$`Industry Vertical`[data$`Industry Vertical` == 'ecommerce'] = "eCommerce"
data$`Industry Vertical`[data$`Industry Vertical` == 'Ecommerce'] = "eCommerce"
data$`City  Location`[data$`City  Location`== 'Bangalore'] = "Bengaluru"
data$`Investors Name`[data$`Investors Name` == 'Undisclosed investors'] = "Undisclosed Investors"
data$`Investors Name`[data$`Investors Name` == 'Undisclosed Investor'] = "Undisclosed Investors"
data$`Investors Name`[data$`Investors Name` == 'undisclosed investors'] = "Undisclosed Investors"
data$`Startup Name`[data$`Startup Name` == 'Flipkart.com'] = "Flipkart"
data$`Startup Name`[data$`Startup Name` == 'Ola Cabs'] = "Ola"
data$`Startup Name`[data$`Startup Name` == 'Olacabs'] = "Ola"

#Number of Investment Per Year
data$`Date dd/mm/yyyy` = as.Date(data$`Date dd/mm/yyyy`,format("%d/%m/%Y"))
data$year = as.numeric(format(data$`Date dd/mm/yyyy`,"%Y"))
data$year[data$year == "15"] = 2015
yeartable = table(data$year)
print(yeartable)
df = as.data.frame(yeartable)
print(df)
ggplot(df, aes(x= Var1, y= Freq, group=1)) + geom_line() + ylab("Number of investments") + xlab("Year") + theme_economist()

# Most favored cities by investors
by_city = data %>%
  group_by(`City  Location`) %>%
  filter(!(`City  Location` == "nan")) %>%
  dplyr::summarize(cnt=n()) %>%
  arrange(desc(cnt)) %>%
  head(10)
print(as.data.frame(by_city))
ggplot(as.data.frame(by_city), aes(x = "", y = cnt, fill = `City  Location`)) +
  geom_col() + coord_polar(theta = "y") +
  geom_text(aes(label = cnt), position = position_stack(vjust = 0.46))


#Industry Vertical wise Investments
ind = data %>%
  group_by(`Industry Vertical`)%>%
  filter(!(`Industry Vertical` == "nan")) %>%
  dplyr::summarise(n = n()) %>%
  drop_na(`Industry Vertical`)%>%
  arrange(desc(n)) %>%
  head(n = 10)

ind %>%
  ggplot(aes(x = reorder(`Industry Vertical` , n) , y = n)) +
  geom_bar(stat='identity',colour="white", fill = c("red")) +
  theme_solarized() +
  labs(x = 'Industry Verticals', y = 'Number of Startups Funded', title = 'Industry Verticals wise Startups Funding') +
  coord_flip()

#Top Disclosed investors
investors = data %>%
  group_by(`Investors Name`)%>%
  filter((!`Investors Name` == 'N/A') && !`Investors Name` == "Undisclosed Investors") %>%
  dplyr::summarise(n=n()) %>%
  arrange(desc(n)) %>%
  head(10)

print(investors)
investors %>%
  ggplot(aes(x = reorder(`Investors Name` , n) , y = n)) +
  geom_bar(stat='identity',colour="black", fill = c("#E75227")) +
  labs(x = 'Industry Name', y = 'Number of Startups Funded', title = 'Top Disclosed Investors') +
  coord_flip() + 
  theme_minimal()
data$`Amount in USD` = as.numeric(gsub(",","",data$`Amount in USD`))

#Amount invested by year
amount = data %>%
  group_by(year)%>%
  dplyr::summarise(n=sum(`Amount in USD`,na.rm=TRUE))%>%
  drop_na()
  
print(amount)
ggplot(as.data.frame(amount), aes(x=year, y=n)) + geom_line() + labs(title="Amount invested by year", y="Amount in USD") + theme_economist()

#Amount invested into startups
startups = data %>%
  group_by(`Startup Name`)%>%
  dplyr::summarise(n=sum(`Amount in USD`,na.rm=TRUE))%>%
  arrange(desc(n)) %>%
  drop_na() %>%
  head(10)

startups = as.data.frame(startups)
startups %>%
  ggplot(aes(x = reorder(`Startup Name` , n) , y = n)) +
  geom_bar(stat='identity',colour="black", fill = c("#E75227")) +
  labs(x = 'Startup Name', y = 'Amount Recieved', title = 'Startups with most funding') +
  coord_flip() + 
  theme_minimal()

# check if industry vertical influences Amount invested
data = na.omit(data)
consumer = data$`Amount in USD`[data$`Industry Vertical` == "Consumer Internet"]
technology = data$`Amount in USD`[data$`Industry Vertical` == "Technology"]
res = t.test(consumer, technology, var.equal = FALSE)
if(res$p.value > 0.05){
  print("Accept NULL hypothesis")
}else{
  print("Reject NULL hypothesis")
}