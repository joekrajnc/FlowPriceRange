library(tidyverse)
library(scales)
#library(forcats)
#closedListings<-read.csv("MonthlyClosedListings.csv", TRUE, ",")
newListings<-read.csv("MonthlyClosedListings.csv", TRUE, ",")
pendings<-read.csv("MonthlyClosedListings.csv", TRUE, ",")

View(closedListings)
#class(closedListings)

# Convert the amount column to numeric, removing the dollar sign and commas
closedListings$Close.Price <- as.numeric(gsub("[$,]", "", closedListings$Close.Price))
#closedListingsNoNa <- drop_na(closedListings$Close.Price) 
#filtered_data <- data %>% filter(Close.Price < )
closedListings2 <- closedListings %>% filter(Close.Price <= 1500000)
View(closedListings2)

breaks_vector <- seq(from = 0, to = 1500000, by = 100000) 


closedListings2 %>% 
  drop_na(Close.Price) %>% 
  ggplot(aes(Close.Price))+
  #geom_bar(color="black", linewidth=.75)+ 
  geom_histogram(breaks = breaks_vector, alpha =.2)+ 
  #geom_text(stat='count', aes(label=after_stat(count), vjust=-1))+
  theme_minimal()+
  labs(title= "Market Bin - Closed Listings",y= "Total")+
  #scale_fill_brewer(palette = "Dark2")
  scale_fill_brewer(palette = "Paired")+
  theme(axis.title.x = element_blank(),
        axis.line.x = 
          element_line(linewidth = .75,
                       color="grey0"),
        axis.line.y = 
          element_line(linewidth=.75,
                       color="grey0"))+
  #geom_text(
  #  stat = "bin", aes(label = after_stat(count)),
  #  vjust = -1, breaks = breaks_vector)+
  #scale_y_continuous(expand = expansion(mult = c(0, .1))+
  scale_x_continuous(breaks = breaks_vector,labels = scales::label_currency())# No space below the bars but 10% above them

  df <- closedListings %>% filter(Close.Price >= 200000  & Close.Price <= 300000)
  