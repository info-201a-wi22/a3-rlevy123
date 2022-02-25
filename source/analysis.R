#load dataset ----
incarceration <- read.csv("../source/incarceration_trends.csv", header = T)

#incarceration data cleaned up ----
incarceration_fixed <- incarceration %>%
  arrange(year) %>%
  select(year,
         state,
         county_name,
         total_jail_pop,
         black_jail_pop,
         aapi_jail_pop,
         latinx_jail_pop,
         native_jail_pop,
         white_jail_pop,
         black_prison_pop,
         aapi_prison_pop,
         latinx_prison_pop,
         native_prison_pop,
         white_prison_pop,
         black_pop_15to64,
         aapi_pop_15to64,
         latinx_pop_15to64,
         native_pop_15to64,
         white_pop_15to64,
         total_pop_15to64) %>%
  drop_na() %>%
  mutate(black_incarceration_pop = black_jail_pop + black_prison_pop,
         aapi_incarceration_pop = aapi_jail_pop + aapi_prison_pop,
         latinx_incarceration_pop = latinx_jail_pop + latinx_prison_pop,
         native_incarceration_pop = native_jail_pop + native_prison_pop,
         white_incarceration_pop = white_jail_pop+white_prison_pop,
         poc_incarceration_pop = black_incarceration_pop + 
           aapi_incarceration_pop + 
           latinx_incarceration_pop + 
           native_incarceration_pop,
         total_incarceration_pop = black_incarceration_pop + 
           aapi_incarceration_pop + 
           latinx_incarceration_pop + 
           native_incarceration_pop +
           white_incarceration_pop,
         poc_pop_15to64 = aapi_pop_15to64 + 
           black_pop_15to64 + 
           latinx_pop_15to64 + 
           native_pop_15to64
         )%>%
  select(year,
         state, 
         county_name,
         black_incarceration_pop,
         aapi_incarceration_pop,
         latinx_incarceration_pop,
         native_incarceration_pop,
         white_incarceration_pop,
         poc_incarceration_pop,
         total_incarceration_pop,
         poc_pop_15to64,
         white_pop_15to64,
         total_pop_15to64)

# summary ----
## 1) Which race has the most incarceration population total for all the years? ----
highest_total_incarceration <- incarceration_fixed%>%
  summarize(black_incarceration_pop = sum(black_incarceration_pop), 
            aapi_incarceration_pop = sum(aapi_incarceration_pop),
            latinx_incarceration_pop = sum(latinx_incarceration_pop),
            native_incarceration_pop = sum(native_incarceration_pop),
            white_incarceration_pop = sum(white_incarceration_pop)
            )%>%
  pivot_longer("black_incarceration_pop":"white_incarceration_pop", 
               names_to = "race_incarceration", 
               values_to = "pop_total") %>%
  filter(pop_total == max(pop_total)) %>%
  pull(race_incarceration)
highest_total_incarceration #black_incarceration_pop

## 2) Which race is currently facing the highest amount of incarceration ----
current_highest_incarceration_rate <- incarceration_fixed %>%
  filter(year == max(year)) %>%
  summarise(black_incarceration_pop = sum(black_incarceration_pop), 
            aapi_incarceration_pop = sum(aapi_incarceration_pop),
            latinx_incarceration_pop = sum(latinx_incarceration_pop),
            native_incarceration_pop = sum(native_incarceration_pop),
            white_incarceration_pop = sum(white_incarceration_pop)) %>%
  pivot_longer("black_incarceration_pop":"white_incarceration_pop", 
               names_to = "race_incarceration", 
               values_to = "pop_total") %>%
  filter(pop_total == max(pop_total)) %>%
  pull(race_incarceration)
current_highest_incarceration_rate #black_incarceration_pop

## 3) Which race is currently facing the lowest amount of incarceration? ----

current_lowest_incarceration_rate <- incarceration_fixed %>%
  filter(year == max(year)) %>%
  summarise(black_incarceration_pop = sum(black_incarceration_pop), 
            aapi_incarceration_pop = sum(aapi_incarceration_pop),
            latinx_incarceration_pop = sum(latinx_incarceration_pop),
            native_incarceration_pop = sum(native_incarceration_pop),
            white_incarceration_pop = sum(white_incarceration_pop)) %>%
  pivot_longer("black_incarceration_pop":"white_incarceration_pop", 
               names_to = "race_incarceration", 
               values_to = "pop_total") %>%
  filter(pop_total == min(pop_total) )%>%
  pull(race_incarceration)
current_lowest_incarceration_rate #aapi_incarceration_pop

## 4) Which state currently has the highest incarceration rate? ----

current_state_highest <- incarceration_fixed %>%
  filter(year==max(year)) %>%
  group_by(state) %>%
  summarize(black_pop_ratio = black_incarceration_pop/total_pop_15to64,
            aapi_pop_ratio = aapi_incarceration_pop / total_pop_15to64,
            latinx_pop_ratio = latinx_incarceration_pop / total_pop_15to64,
            native_pop_ratio= native_incarceration_pop / total_pop_15to64,
            white_pop_ratio = white_incarceration_pop / total_pop_15to64) %>%
  group_by(state) %>%
  summarize(black_pop_ratio = sum(black_pop_ratio),
            aapi_pop_ratio = sum(aapi_pop_ratio),
            latinx_pop_ratio = sum(latinx_pop_ratio),
            native_pop_ratio = sum(native_pop_ratio),
            white_pop_ratio = sum(white_pop_ratio),
            total_ratio = sum(black_pop_ratio, 
                              aapi_pop_ratio, 
                              latinx_pop_ratio, 
                              native_pop_ratio, 
                              white_pop_ratio)) %>%
  filter(total_ratio==max(total_ratio)) %>%
  pull(state)
current_state_highest #TX

## 5) What group in `current_highest_state` is facing a higher amount of incarceration?

current_highest_group_in_highest_state <- incarceration_fixed %>%
  filter(year==max(year)) %>%
  group_by(state) %>%
  summarize(black_pop = black_incarceration_pop/total_pop_15to64,
            aapi_pop = aapi_incarceration_pop / total_pop_15to64,
            latinx_pop = latinx_incarceration_pop / total_pop_15to64,
            native_pop= native_incarceration_pop / total_pop_15to64,
            white_pop = white_incarceration_pop / total_pop_15to64) %>%
  group_by(state) %>%
  summarize(black_pop = sum(black_pop),
            aapi_pop = sum(aapi_pop),
            latinx_pop = sum(latinx_pop),
            native_pop = sum(native_pop),
            white_pop = sum(white_pop),
            poc_pop = sum(black_pop, aapi_pop, latinx_pop, native_pop),
            total = sum(black_pop, aapi_pop, latinx_pop, native_pop, white_pop))%>%
  filter(total==max(total)) %>%
  pivot_longer("poc_pop":"white_pop", 
               names_to = "demographic", 
               values_to = "incarceration_ratio") %>%
  filter(incarceration_ratio == max(incarceration_ratio)) %>%
  pull(demographic)
current_highest_group_in_highest_state #poc_pop_ratio

## 6) Which county in Washington State has the largest incarceration population 
# currently? ----

# create a dataframe that shows the current total incarceration population in 
#total in each Washington County. 

washington <- incarceration_fixed %>%
  filter(year==max(year), 
         state == "WA")

# find highest county in WA

highest_wa_county <- washington %>%
  filter(total_incarceration_pop == max(total_incarceration_pop)) %>%
  pull(county_name)
highest_wa_county #King County


#Charts ----
## Chart 1: total incarceration in highest WA county throughout all of the years ----

wa_counties <- incarceration_fixed %>%
  group_by(county_name) %>%
  filter(state == "WA")

#use
wa_counties <- incarceration_fixed %>%
  group_by(county_name)%>%
  filter(state == "WA", 
         county_name == "Clark County" | 
           county_name =="King County" | 
           county_name =="Yakima County" |
           county_name =="Pierce County") %>%
  group_by(county_name, year)%>%
  summarize(ratio=total_incarceration_pop/total_pop_15to64)

chart_1 <- ggplot(wa_counties)+
  geom_point(mapping= aes(x = year, y = ratio, color = county_name))+
  geom_line(mapping= aes(x = year, y = ratio, color = county_name))+
  labs(title = "Incarceration Rates in Four WA Counties (1990-2016)",
       x = "Year",
       y = "Incarceration Rate")
chart_1 #final chart 1
ggplotly(chart_1)

## Chart 2: Comparing POC incarceration population to White Persons incarceration population ---
wa_counties_2 <- incarceration_fixed %>%
  group_by(state) %>%
  filter(state == "WA",
         year == max(year)) %>%
  group_by(year) %>%
  summarize(poc_incarceration_pop = sum(poc_incarceration_pop),
            white_incarceration_pop = sum(white_incarceration_pop)) %>%
  mutate(incarceration_total= sum(poc_incarceration_pop, 
                                  white_incarceration_pop)) %>%
  pivot_longer(poc_incarceration_pop:white_incarceration_pop, 
               names_to = "race_incarceration", 
               values_to = "incarceration_pop")%>%
  group_by(race_incarceration)%>%
  mutate(ratio = incarceration_pop / incarceration_total)

pie_chart <- ggplot(wa_counties_2, aes(x="", y=ratio, fill = race_incarceration))+
  geom_col(color= "black")+
  geom_text(aes(label = race_incarceration),
            position = position_stack(vjust = 0.5)) +
  labs(title = "POC VS White WA Most Recent Incarceration Demographic Pie Chart", 
       subtitle = "Shows the ratio of people incarcerated in WA over total people incarcerated in WA") +
  coord_polar(theta = "y")
pie_chart #chart 2

## Chart 3: map of the ratio of poc incarceration pop to poc total pop in WA stat --- 
map_white <- incarceration_fixed %>%
  filter(year==max(year)) %>%
  group_by(state)%>%
  summarize(white_incarceration_pop = sum(white_incarceration_pop),
            poc_incarceration_pop = sum(poc_incarceration_pop),
            total_incarceration_pop = white_incarceration_pop + poc_incarceration_pop,
            ratio = white_incarceration_pop/total_incarceration_pop)


map_1 <- plot_usmap(
  regions = c("states"),
  data = map_white,
  values = "ratio",
  label = T,
  label_color = "black"
  )+
  scale_fill_continuous(low = "white",
                        high = "red", 
                        name = "POC Ratio", 
                        label = scales::comma) + 
  labs(title = "White Incarceration Over Total Incarceration Population Ratio in Each State")
map_1

map_poc <- incarceration_fixed %>%
  filter(year==max(year)) %>%
  group_by(state)%>%
  summarize(white_incarceration_pop = sum(white_incarceration_pop),
            poc_incarceration_pop = sum(poc_incarceration_pop),
            total_incarceration_pop = white_incarceration_pop + poc_incarceration_pop,
            ratio = poc_incarceration_pop/total_incarceration_pop)

map_2 <- plot_usmap(
  regions = c("states"),
  data = map_poc,
  values = "ratio",
  label = T,
  label_color = "black"
)+
  scale_fill_continuous(low = "white",
                        high = "red",
                        name = "POC Ratio", 
                        label = scales::comma) + 
  labs(title = "Poc Incarceration Over Total Incarceration Population Ratio in Each State")
map_2



