library(tidyverse)
install.packages("nycflights13")
library(nycflights13)
data <- library(nycflights13)
head(airlines)
head(airports)
head(airlines)
nycflights13::airlines
library(flights)
glimpse(flights)
flights <- flights
glimpse(flights)
summary(flights)
dim(flights)
head(flights)
tail(flights)

flights %>% 
  summarise_all(n_distinct)

flights %>%
  summarise_all(n_distinct)

sapply(flights, function(x) sum(is.na(x)))

subset_flights <- subset(flights, month == 1 & day == 1)       
subset_flights

names(flights)[names(flights) == "year"] <- "flight_year"
names(subset_flights) [names(subset_flights) == "year"] <- "flight_year"

flights$gain <- flights$sched_dep_time - flights$dep_time
flights$gain
summary(flights$gain)

selected_flights <- flights[c("flight_year", "month", "day", "gain")]
summary(selected_flights)

selected_flights2 <- flights[c(1,2,3,20)]
head(selected_flights)
head(selected_flights2)

avg_delay_by_carrier_base <- aggregate(dep_delay ~ carrier, data = flights, FUN = mean, na.rm = TRUE)

summary(avg_delay_by_carrier_base)

apply(flights[, 1:5], 1, mean, na.rm = TRUE)
te <- apply(flights[,1:5], 1, mean, na.rm = TRUE)
te
show(te)
summary(te)

filtered_flight <- flights %>% filter(month == 1, day == 1)
head(filtered_flight)
summary(filtered_flight)
summary(flights)
filtered_flight_aug <- flights %>% filter(month == 8)
summary(filtered_flight_aug)
filtered_flight_jan <- flights %>% filter (month == 1)
summary(filtered_flight_jan)
renamed_flights <- flights %>% rename(year == flight_year)

renamed_flights <- flights %>% dplyr::rename(year = flight_year)

flights <- flights %>% mutate(gain = sched_dep_time - dep_time)

selected_flights3 <- flights %>% select(flight_year, month, day, gain)
summary(selected_flights3)
summary(selected_flights2)





avg_delay_by_carrier <- flights %>%
  group_by(carrier) %>%
  summarize(mean(dep_delay, na.rm = TRUE))

print(avg_delay_by_carrier)


hist(flights$dep_delay, breaks = 30, main = "Histogram of Departure Delays", xlab = "Departure Delay (minutes)")
hist(flights$dep_delay, breaks = 30, main = "Departure Delays", xlab = "Verspätungen (Minuten)")

boxplot(arr_delay ~ carrier, data = flights, xlab = "Carrier", ylab = "Arrival Delay (minutes)", main = "Departure Delays by Carrier")


plot(flights$dep_delay, flights$arr_delay, xlab = "Startverspätung in Minuten", ylab = "Ankunftsverspätung in Minuten", main = "Relation Verspätung Abflug und Ankunft")

ggplot(data = flights, aes(x = dep_delay, y = arr_delay)) +
  geom_point(alpha = 0.9, color = "green") +
  labs(title = "Abflugs und Ankunftsverspätung", x = "Abflugsverspätung in Minuten", y = "Ankunftsverspätung in Minuten")

ggplot(data = flights, aes(x = dep_delay, y = arr_delay)) +
  geom_point(alpha = 0.2, color = "purple") +
  labs(title = "Departure vs. Arrival Delays", x = "Departure Delay (min)", y = "Arrival Delay (min)") +
  geom_smooth(method = "lm", color = "blue")

flights %>%
  group_by(carrier) %>%
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = carrier, y = avg_delay)) +
  geom_bar(stat = "identity", fill = "tomato") +
  labs(title = "Average Departure Delay by Carrier", x = "Carrier", y = "Average Delay (min)")

flights %>%
  group_by(carrier) %>%
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = carrier, y = avg_delay)) +
  geom_bar (stat = "identity", fill = "grey") + 
  labs(title = "Durchschnittliche Verspätung", x = "Fluggesellschaft", y = "Durchschnittliche Verspätung in Minuten")


flights %>%
  group_by(carrier) %>%
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = carrier, y = avg_delay, fill = carrier)) + # Added fill aesthetic
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired") + # This line applies the color palette
  labs(title = "Average Departure Delay by Carrier", x = "Carrier", y = "Average Delay (min)")

flights %>%
  group_by(carrier) %>%
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = carrier, y = avg_delay, fill = carrier)) + 
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = "Spectral") + 
  labs(title = "Durchschnittliche Verspätung", x = "Fluggesellschaft", y = "Durchschnittliche Verspätung in Minuten")

?palette

flights %>%
  group_by(carrier) %>%
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = carrier, y = avg_delay, fill = carrier)) + # Added fill aesthetic for demonstration
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired") + # Applies the color palette
  labs(title = "Durchschnittliche Verspätung", x = "Fluggesellschaft", y = "Durchschnittliche Verspätung in Minuten") +
  theme_minimal() + # Applies the minimal theme
  theme(text = element_text(size = 10), # Customizes global text size
        title = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold")) # Boldens the x-axis title

ggplot(data = flights, aes(x = dep_time, y = arr_delay)) +
  geom_point(alpha = 0.1) +
  facet_wrap(~month)

