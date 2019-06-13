library(dplyr)
library(hflights)

data(hdflights)
head(hflights)

flights <- tbl_df(hflights)
flights
print(flights, n=20)

data.frame(head(flights))

flights[flights$Month==1 & flights$DayofMonth==1, ]
filter(flights, Month==1, DayofMonth==1)
filter(flights, UniqueCarrier=="AA" | UniqueCarrier=="UA")

flights[ , c("DepTime", "ArrTime", "FlightNum")]
select(flights, DepTime, ArrTime, FlightNum)

flights %>% 
  select(UniqueCarrier, DepDelay) %>% 
  filter(DepDelay > 60)


x1 <- 1:5; x2 <- 2:6
sqrt(sum((x1-x2)^2))
(x1-x2)^2 %>%  sum() %>% sqrt()

flights$Speed <- flights$Distance/flights$AirTime*60
flights[, c("Distance", "AirTime", "Speed")]

flights %>% 
  select(Distance, AirTime) %>% 
  mutate(Speed = Distance/AirTime*60)

flights <- flights %>% mutate(Speed = Distance/AirTime*60)
flights

flights %>% 
  group_by(Dest) %>% 
  summarise(avg_delay = mean(ArrDelay, na.rm=TRUE))

flights %>% 
  group_by(UniqueCarrier) %>% 
  summarise_each(funs(mean), Cancelled, Diverted)

flights %>% 
  group_by(UniqueCarrier) %>% 
  summarise_each(funs(min(., na.rm=TRUE), max(., na.rm=TRUE)), matches("Delay"))

flights %>% 
  group_by(Month, DayofMonth) %>% 
  summarise(flight_count = n()) %>% 
  arrange(desc(flight_count))

flights %>% 
  group_by(Month, DayofMonth) %>% 
  tally(sort = TRUE)

flights %>% 
  group_by(Dest) %>% 
  summarise(flight_count = n(), plane_count = n_distinct(TailNum))

flights %>% 
  group_by(Dest) %>% 
  select(Cancelled) %>% 
  table() %>% 
  head()

flights %>% 
  group_by(UniqueCarrier) %>% 
  select(Month, DayofMonth, DepDelay) %>% 
  filter(min_rank(desc(DepDelay)) <= 2) %>% 
  arrange(UniqueCarrier, desc(DepDelay))

flights %>% 
  group_by(UniqueCarrier) %>% 
  select(Month, DayofMonth, DepDelay) %>% 
  top_n(2) %>% 
  arrange(UniqueCarrier, desc(DepDelay))

flights %>% 
  group_by(Month) %>% 
  summarise(flight_count = n()) %>% 
  mutate(change = flight_count - lag(flight_count))

glimpse(flights)