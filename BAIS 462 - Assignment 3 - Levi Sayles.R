
                                  ##### R Assignment 3

##### My Data

# Load libraries and Import Data
library(tidyverse)

laptop_data <-
  read_csv("https://myxavier-my.sharepoint.com/:x:/g/personal/saylesl_xavier_edu/EdbkG2PX59ZCsdKngXGMszoB4UVh1V21MraGZbimqwDULA?download=1")

# This dataset contains information about laptops sold including their specs, OS,
# Price, and Manufacturers before the year 2020. 


## Question 1 : OS version and price (grouping Mac OS and Windows 10 
# variations, excluding Windows 7 & No OS)

#  I want to examine whether the operating system (OS) of a laptop has a 
# significant effect on its price. It is interesting because operating systems 
# like MacOS are often associated with premium pricing compared to Windows. I'll 
#  group similar OS types (e.g., Mac OS X, Windows 10) and exclude less common
# ones like Windows 7 or those without an OS.


euro_to_usd <- 1.12 # left this separate to be adjusted as currency values change

laptop_data %>%
  filter(!(OS %in% c("Windows 7", "No OS"))) %>%
  mutate(
    OS_grouped = case_when(
      OS %in% c("Mac OS X", "macOS") ~ "Mac OS",
      OS %in% c("Windows 10", "Windows 10 S") ~ "Windows 10",
      TRUE ~ OS
    ),
    Price_usd = Price_euros * euro_to_usd
  ) %>%
  group_by(OS_grouped) %>%
  summarise(AvgPrice_usd = mean(Price_usd)) %>%
  ggplot(aes(x = OS_grouped, y = AvgPrice_usd, fill = OS_grouped)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Price by Operating System", 
       x = "Operating System", y = "Average Price (USD)",
       fill = "OS") +
  theme_minimal()

# The analysis reveals the following average prices for laptops based on their 
# operating systems:
  # Mac OS: $1,720.62
  # Windows 10: $1,299.63
  # Linux: $684.05
  # Chrome OS: $608.94
  # Android: $477.40

# Interpretation: Mac OS laptops are significantly more expensive on average 
# compared to other systems, particularly Windows 10. Chrome OS and Linux 
# laptops are relatively affordable, while Android-based laptops are the least 
# expensive. This supports the common perception that Mac OS devices are premium
# products.



## Question 2 : Does laptop weight significantly impact price?

# I aim to determine if lighter laptops are more expensive than heavier ones. 
#Lightweight laptops often use advanced materials and designs, which might drive
# up the price, while heavier models may have more basic components.


laptop_data %>%
  mutate(Price_usd = Price_euros * euro_to_usd,
         WeightCategory = case_when(
           Weight < 1.5 ~ "Light",
           Weight >= 1.5 & Weight <= 2.5 ~ "Medium",
           Weight > 2.5 ~ "Heavy")) %>%
  group_by(WeightCategory) %>%
  summarise(AvgPrice_usd = mean(Price_usd)) %>%
  ggplot(aes(x = factor(WeightCategory, levels = c("Heavy", "Medium", "Light")), 
             y = AvgPrice_usd, fill = WeightCategory)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Price by Weight Category", x = "Weight Category", y = "Average Price (USD)") +
  theme_minimal()


#The analysis of average laptop prices based on weight categories reveals:
  # Heavy laptops (above 2.5 kg): $1,813.83
  # Medium laptops (1.5 - 2.5 kg): $1,003.45
  # Light laptops (below 1.5 kg): $1,453.22

# Interpretation: Heavier laptops are, on average, more expensive than both 
# light and medium-weight laptops. Interestingly, light laptops are also priced 
# higher than medium-weight laptops. This suggests that both very lightweight 
# designs and high-performance heavy models may command higher prices, likely 
# due to premium materials in lightweight laptops and higher-end components in 
# heavy models.



## Question 3 :  Are laptops with Intel CPUs more expensive than those with 
# AMD CPUs?

# This question investigates if laptops with Intel processors have a price 
# premium compared to AMD processors, as Intel processors have been historically
# marketed as high-performance, potentially leading to higher prices.

laptop_data %>%
  filter(CPU_company !="Samsung" ) %>% 
  mutate(Price_usd = Price_euros * euro_to_usd) %>%
  group_by(CPU_company) %>%
  summarise(AvgPrice_usd = mean(Price_usd)) %>%
  ggplot(aes(x = CPU_company, y = AvgPrice_usd, fill = CPU_company)) +
  geom_bar(stat = "identity") +
  labs(title = bquote(bold("Average Price by CPU Manufacturer")),
       x = bquote(bold("CPU Manufacturer")),
       y = bquote(bold("Average Price (USD)")),
       fill = bquote(bold("CPU Manufacturer"))) +
  theme_minimal()

# The analysis of average laptop prices based on CPU manufacturers reveals:
  # Intel CPUs: $1,504.67
  # AMD CPUs: $1,125.32

# Interpretation: Laptops equipped with Intel CPUs are, on average, more 
# expensive than those with AMD CPUs. This price difference may reflect consumer
# perceptions of brand value, performance capabilities, and overall market 
# positioning. Intel is often associated with higher-end performance, which can 
# drive up prices due to demand for premium features and build quality. On the 
# other hand, AMD laptops, while generally more affordable, may still offer 
# competitive performance, especially in budget-friendly segments. This suggests
# that while Intel dominates in terms of price, AMD provides a viable option 
# for cost-conscious consumers, making it an interesting dynamic in the laptop 
# market.







##### Flight Data:
flight_data <- 
  read_csv("https://myxavier-my.sharepoint.com/:x:/g/personal/malloyc5_xavier_edu/EdehjmF6CyxAk54Th0dlFgUBIqhpHPgQ2aCtQhDFFX90dQ?download=1")

# coding a satisfaction
flight_data_clean <- flight_data %>%
  select(Class, `Flight Distance`, `Inflight wifi service`, satisfaction) %>%
  mutate(satisfaction = ifelse(satisfaction == "satisfied", 1, 0)) %>% 
  mutate(DistanceCategory = case_when(
    `Flight Distance` <= 1000 ~ "Short",
    `Flight Distance` > 1000 & `Flight Distance` <= 2000 ~ "Medium",
    `Flight Distance` > 2000 ~ "Long"
  ))


## Question 1 :  Does customer satisfaction differ significantly based on the 
# class of travel (Economy, Eco Plus, Business)? 

# Hypothesis: Business class travelers are more likely to be satisfied compared 
# to Economy and Eco Plus travelers.

flight_data_clean %>%
  group_by(Class) %>%
  summarise(Average_Satisfaction = mean(satisfaction)) %>%
  ggplot(aes(x = Class, y = Average_Satisfaction, fill = Class)) +
  geom_bar(stat = "identity") +
  labs(title = "Satisfaction by Class of Travel",
       x = "Class",
       y = "Average Satisfaction") +
  theme_minimal()

# Results:
  # Economy Class: Average satisfaction is the lowest.(0.694)
  # Eco Plus Class: Satisfaction is higher than Economy, but lower than Business.
  # (	0.758)
  # Business Class: Highest satisfaction among all travel classes. (0.872)

# Interpretation: Business class passengers report the highest satisfaction, 
# followed by Eco Plus and Economy. This supports the idea that premium services
# and comfort in Business class lead to higher satisfaction. Airlines should 
# consider improving services in lower classes to boost overall satisfaction.



## Question 2 :  Do longer flight distances result in higher dissatisfaction 
# due to possible delays or discomfort? 

# Hypothesis: Passengers on longer flights tend to be more dissatisfied, possibly
# due to extended flight times or associated delays.

flight_data_clean %>%
  group_by(DistanceCategory) %>%
  summarise(Average_Satisfaction = mean(satisfaction)) %>%
  ggplot(aes(x = DistanceCategory, y = Average_Satisfaction, fill = DistanceCategory)) +
  geom_bar(stat = "identity") +
  labs(title = "Satisfaction by Flight Distance Category", 
       x = "Flight Distance Category",
       y = "Average Satisfaction") +
  theme_minimal()

# Results:
  # Short Flights (≤ 1000 miles): 0.783
  # Medium Flights (1001 to 2000 miles): 0.735
  # Long Flights (> 2000 miles): 	0.692

# Interpretation: Short flights have the highest satisfaction, with satisfaction
# decreasing as flight distance increases. Long flights likely result in lower 
# comfort due to fatigue and extended time in flight. This suggests that airlines
# should focus on improving services for long-haul flights to maintain customer 
# satisfaction.



## Question 3 :  Does in flight WiFi service quality correlate with overall 
# customer satisfaction

# Hypothesis: Better inflight wifi service will positively correlate with higher 
# satisfaction levels.

flight_data_clean %>%
  group_by(`Inflight wifi service`) %>%
  summarise(Average_Satisfaction = mean(satisfaction)) %>%
  ggplot(aes(x = factor(`Inflight wifi service`), y = Average_Satisfaction, fill = factor(`Inflight wifi service`))) +
  geom_bar(stat = "identity") +
  labs(title = "Inflight Wifi Service vs Satisfaction",
       x = "Inflight Wifi Service Rating",
       y = "Average Satisfaction") +
  theme_minimal()

# Results:
  # Passengers rating inflight wifi service as "5" (highest quality): Highest 
  # average satisfaction.

  # Satisfaction decreases consistently with lower wifi service ratings, with 
  # the lowest average satisfaction corresponding to a rating of "1."

# Interpretation: There is a clear relationship between wifi quality and 
# satisfaction. Passengers with excellent wifi service (rating 5) report the 
# highest satisfaction, while those with poor wifi (rating 1) are least satisfied. 
# Reliable wifi is a key factor in enhancing the inflight experience, especially 
# for long flights.