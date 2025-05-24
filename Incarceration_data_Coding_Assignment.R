#TASK 1
library(tidyverse)
library(readxl)
setwd("C:/Users/utkar/Downloads/R-programming/Harris Coding Assignment")
incarceration_data <- read_excel("incarceration_counts_and_rates_by_type_over_time.xlsx", range = "A7:CO10") %>%
  rename("type" = ...1) %>%
  pivot_longer(`1925`:`2016`, names_to = "year", values_to = "counts")
view(incarceration_data)
head(incarceration_data, 5)
tail(incarceration_data,7)
#summary
summary(incarceration_data)
summary(incarceration_data$year)
summary(incarceration_data$counts)
#missing items
sum(is.na(incarceration_data))
#skimming information
library(skimr)
skim(incarceration_data)
#group data by species and perform skim
incarceration_data %>%
  dplyr::group_by(type) %>%
  skim()

#TASK 2
state_data <- incarceration_data %>%
  mutate(year = as.numeric(year),
         counts = as.numeric(counts),
         decade = 10 * as.numeric(year) %/% 10)
print(state_data)

#Filter data 
state_data %>%
  select(type, counts, decade, year) %>%
  filter(type=="State prisons", .preserve = TRUE)%>%
#Mean and Standard Deviation
  summarise(
    mean_count = mean(counts, na.rm = TRUE),
    sd_count = sd(counts, na.rm = TRUE)
  ) 

#TASK 3
growth_by_decade_state_prisons <- state_data %>%
  filter(type == "State prisons") %>%
  group_by(decade) %>%
  summarise(
    start_count = first(counts, order_by = year),
    end_count = last(counts, order_by = year),
    percent_growth = (end_count - start_count) / start_count
  ) %>%
  arrange(-desc(decade))
print(growth_by_decade_state_prisons)

#TASK 4
library(ggplot2)
library(dplyr)

# Assuming your data frame is named 'incarceration_data'
# and it has columns 'year', 'counts', and 'type'

incarceration_data <- incarceration_data %>%
  mutate(
    year = as.numeric(year),
    counts = as.numeric(counts)
  )

# Create the line graph
ggplot(incarceration_data, aes(x = year, y = counts, color = type)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::comma) + # Format y-axis with commas
  labs(
    title = "Incarceration counts (total population on a single day) over time",
    x = "year",
    y = "counts",
    color = "type"
  ) +
  theme_minimal()

#TASK 5
library(dplyr)
addr <- data.frame(
  name = c("Alice","Bob", "Carol","Dave", "Eve"), 
  email = c("alice@company.com", "bob@company.com", "carol@company.com", "dave@company.com", "eve@company.com"), 
  stringsAsFactors = FALSE)
phone <- data.frame(
  fullname = c("Bob","Carol", "Dave","Eve", "Frank"), 
  phone = c("919 555-1111", "919 555-2222", "919 555-3333", "310 555-4444", "919 555-5555"), 
  stringsAsFactors = FALSE)

#Left-join data set
left_joined_data <- left_join(addr, phone, by = c("name" = "fullname"))
print("Result of left join:")
print(left_joined_data)
cat("\nAre there any missing values in the Left Join result?\n")
print(any(is.na(left_joined_data)))

#Inner-join
inner_joined_data <- inner_join(addr, phone, by = c("name" = "fullname"))
print("Result of inner join:")
print(inner_joined_data)
cat("\nAre there any missing values in the Inner join result?\n")
print(any(is.na(inner_joined_data)))

#Full join
full_joined_data <- full_join(addr, phone, by = c("name" = "fullname"))
print("Result of full join:")
print(full_joined_data)
cat("\nAre there any missing values in the Full join result?\n")
print(any(is.na(full_joined_data)))

#TASK 6

library(tidyverse)
#Create the 'numbers' vector
numbers <- rep(seq(-9, 10, 1), 10)
print("numbers vector:")
print(head(numbers))
print(paste("Length of numbers:", length(numbers)))

#Calculate the square of each number using a for-loop
numbers_squared <- numeric(length(numbers)) # Initialize an empty numeric vector
for (i in 1:length(numbers)) {
  numbers_squared[i] <- numbers[i]^2
}
print("\nnumbers_squared vector (first few elements):")
print(head(numbers_squared))
print(paste("Length of numbers_squared:", length(numbers_squared)))

#Calculate the square with added random noise using a for-loop
noisy_numbers_squared <- numeric(length(numbers)) # Initialize an empty numeric vector
for (i in 1:length(numbers)) {
  noise <- rnorm(1, sd = 5) # Generate one random number from a normal distribution with sd = 5
  noisy_numbers_squared[i] <- (numbers[i]^2) + noise
}
print("\nnoisy_numbers_squared vector (first few elements):")
print(head(noisy_numbers_squared))
print(paste("Length of noisy_numbers_squared:", length(noisy_numbers_squared)))

numbers_data <- tibble(numbers = numbers,
                       noisy_numbers_squared = noisy_numbers_squared)
#Grapgh Plot
numbers_data %>%
  ggplot(aes(x = numbers, y = noisy_numbers_squared)) +
  geom_point() +
  geom_smooth(color = "steelblue", size = 1.5) + # Customize the smoothed line
  labs(
    title = "Relationship between Numbers and Noisy Squared Values",
    x = "numbers",
    y = "noisy_numbers_squared"
  ) +
  theme_bw() # Use a different theme for better contrast

#TASK 7

notice_gpa <- function(gpa) {
  if (gpa < 2) {
    print(paste0("Your GPA is ", gpa, ". You are on academic probation."))
  } else if (gpa >= 3.5) {
    print(paste0("Your GPA is ", gpa, ". You made the Dean’s list. Congrats!"))
  } else {
    print(paste0("Your GPA is ", gpa))
  }
}
notice_gpa(1.9)
notice_gpa(3.5)
notice_gpa(3.0)
