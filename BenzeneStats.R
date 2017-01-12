library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

limit <- c(4.5, 9) # change this variable to play with diff. regulatory limits (9 ug/m3 for EPA and 4.5 ug/m3 for TCEQ)
conversion_fact <- 3.19 # ug/M3 per 1 ppb Benzene @ STP

raw2010 <- read.csv("ben+2010.csv") 
raw2011 <- read.csv("ben+2011.csv") 
raw2012 <- read.csv("ben+2012.csv") 
raw2013 <- read.csv("ben+2013.csv")
raw2014 <- read.csv("ben+2014.csv")
raw2015 <- read.csv("ben+2015.csv")
raw2016 <- read.csv("ben+2016.csv")

raw <- bind_rows(raw2010, raw2011, raw2012, raw2013, raw2014, raw2015, raw2016)

data <- raw %>% 
  mutate(Date = as.Date(as.character(Date), '%Y%m%d'),     # Convert variable format to 'Date'
         Year = year(Date)) %>%                            # Get Year variable for grouping our averages 
  gather(Monitor, Conc, -Date, -Time, -Year) %>%                # Put in long format, easier for processing
  mutate(Conc = as.numeric(Conc)*conversion_fact)                        # Make numeric and make NAs for everyhting that's not a number

# # Histograms
# data %>% 
#   ggplot(aes(Conc, fill = Monitor)) +            # this is where I choose the variables
#   geom_histogram(alpha = .8) +                   # THis is where I choose histogram, with tranparency of .8
#   facet_wrap(~Monitor) +                         # THis is where I make 12 plots, one for each monitor
#   xlim(c(0,round(limit))) + ylim(c(0,5000)) +    # The rest is just making the plot look good
#   ggtitle("Histogram of Benzene Concentrations, 2012") +
#   xlab("Benzene Concentration (ppb)") +
#   ylab("Frequency")

# Calculating Exceedances
Exceedances <- data %>% 
  mutate(TCEQ = ifelse(Conc < limit[1], 0, 1),   # Here we create a column called TCEQ that is 1 if exceeds, 0 if not
         EPA = ifelse(Conc < limit[2], 0, 1),    # Same but for thr EPA limit
         Year = factor(Year)) %>%                # We make the year a factor (plotting technicalities)
  gather(Agency, Exceeds, TCEQ:EPA) %>%          # We turn the TCEQ and EPA column into one column called 'Agency'
  group_by(Agency, Year, Monitor) %>%            # In the next two steps we get the percentages from the 'Agency' column, 
  summarise(Exceeded = mean(Exceeds, na.rm = T)*100)  # making sure to average by groups of Agency, Year and Monitoring site

# This is the exceedance plot
Exceedances %>%    
  ggplot(aes(x = Monitor, y = Exceeded, fill = Year)) +
  geom_bar(position = 'dodge', stat = 'identity', alpha = 0.8) +
  facet_grid(~Agency) +
  ylab(paste0("Percentage Hours Exceeded Regulatory Limit")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle("Houston Benzene Regulatory Limit Exceedances by Site and Regulatory Agency")

# THis is where I cacluate the average values
Avg_Vals <- data %>%                        
  mutate(Year = factor(Year)) %>%         
  group_by(Year, Monitor) %>%    
  summarise(Average = mean(Conc, na.rm = T)) 

# This is the avg. value plot
limits <- data.frame( x = rep(c(-Inf, Inf), 2), y = limit, limit = factor(c('TCEQ', 'EPA')))
Avg_Vals %>% 
  ggplot(aes(x = Monitor, y = Average, fill = Year)) +
  geom_bar(position = 'dodge', stat = 'identity', alpha = 0.8) +
  #geom_line(aes(x, y, linetype = limit), limits) +    # tried to show the reg. limits, but it didn't work nicely
  # geom_hline(yintercept=limit, color = limit, show.legend = T) +
  ylab(paste0("Average Yearly Concentrations (ppb)")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle("Houston Benzene Concentrations by Site")

data %>% 
  mutate(Year = factor(Year)) %>% 
  ggplot(aes(Monitor, Conc, fill = Year)) +
  geom_boxplot(outlier.alpha = .03, outlier.fill = "black") +
  geom_hline(yintercept=limit, color = limit, show.legend = T) +
  ylim(0, 10) +
  ylab("Concentration (ug/m3)") +
  xlab("Monitoring Site") +
  ggtitle("Benzene Concentration at Houston Monitoring Sites") 

# exporting the files
write.csv(Avg_Vals, "Benzene Averages 1-10.csv")
write.csv(Exceedances, "Benzene Percentage Exceedances 1-10.csv")



#### 1,3-Butadiene
but15 <- read.csv("2015_13But.csv")
but16 <- read.csv("2016_13But.csv")

but <- bind_rows(but15, but16)

conversion_fact <- 2.21 # ug/M3 per 1 ppb 1,3-Butadiene @ STP

data <- but %>% 
  mutate(Date = as.Date(as.character(Date), '%Y%m%d'),     # Convert variable format to 'Date'
         Year = year(Date)) %>%                            # Get Year variable for grouping our averages 
  gather(Monitor, Conc, -Date, -Time, -Year) %>%                # Put in long format, easier for processing
  mutate(Conc = as.numeric(Conc)*conversion_fact)                        # Make numeric and make NAs for everyhting that's not a number

# THis is where I cacluate the average values
Avg_Vals <- data %>% 
  mutate(Year = factor(Year)) %>%         
  group_by(Year, Monitor) %>%    
  summarise(Average = mean(Conc, na.rm = T)) 

# This is the avg. value plot
limits <- data.frame( x = rep(c(-Inf, Inf), 2), y = limit, limit = factor(c('TCEQ', 'EPA')))
Avg_Vals %>% 
  ggplot(aes(x = Monitor, y = Average, fill = Year)) +
  geom_bar(position = 'dodge', stat = 'identity', alpha = 0.8) +
  #geom_line(aes(x, y, linetype = limit), limits) +    # tried to show the reg. limits, but it didn't work nicely
  geom_hline(yintercept=limit, color = limit, show.legend = T) +
  ylab(paste0("Average Yearly Concentrations (ppb)")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle("Houston 1,3-Butadiene Concentrations by Site")

data %>% 
  mutate(Year = factor(Year)) %>% 
  ggplot(aes(Monitor, Conc, fill = Year)) +
  geom_boxplot(outlier.alpha = .03, outlier.fill = "black") +
  ylim(0, 10) +
  ylab("Concentration (ug/m3)") +
  xlab("Monitoring Site") +
  ggtitle("1,3-Butadiene Concentration at Houston Monitoring Sites") 

# exporting the files
write.csv(Avg_Vals, "13But Averages 1-10.csv")


