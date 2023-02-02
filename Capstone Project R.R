library(tidyverse)

vg_data <- read_csv('vgsales.csv')
View(vg_data)

vg_data <- vg_data[!(vg_data$Year == "N/A"), ]
View(vg_data)


# create a column called decade that holds the decade in which the game was released
vg_data <- vg_data %>%
  mutate(Decade = case_when(Year < 1990 ~ '1980-89',
                            Year >= 1990 & Year < 2000 ~ '1990-99',
                            Year >= 2000 & Year < 2010 ~ '2000-09',
                            Year >= 2010 & Year < 2020 ~ '2010-19'))
         
View(vg_data)


# Create a pie chart showing which genre was most popular in the eighties
eighties_by_genre <- vg_data %>%
  filter(Decade == '1980-89') %>%
  group_by(Genre)
eighties_by_genre

eighties_pie <- ggplot(data = eighties_by_genre)
eighties_pie + geom_bar(mapping = aes(x = Genre, fill = Genre)) +
  labs(x = 'Genre', y = 'Count',
       title = 'Games per Genre in the Eighties') +
  coord_polar()


# Create a pie chart showing which genre was most popular in the nineties
nineties_by_genre <- vg_data %>%
  filter(Decade == '1990-99') %>%
  group_by(Genre)

nineties_pie <- ggplot(data = nineties_by_genre)
nineties_pie + geom_bar(mapping = aes(x = Genre, fill = Genre)) +
  labs(y = 'Count',
       title = 'Games per Genre in the Nineties') +
  coord_polar()

# Create a pie chart showing which genre was most popular in the 2000's
two_thousands_by_genre <- vg_data %>%
  filter(Decade == '2000-09') %>%
  group_by(Genre)

two_thousands_pie <- ggplot(data = two_thousands_by_genre)
two_thousands_pie + geom_bar(mapping = aes(x = Genre, fill = Genre)) +
  labs(y = 'Count',
       title = "Games per Genre in the 2000's") +
  coord_polar()

# Create a pie chart showing which genre was most popular in the 2010's
tens_by_genre <- vg_data %>%
  filter(Decade == '2010-19') %>%
  group_by(Genre)

tens_pie <- ggplot(data = tens_by_genre) 
tens_pie + geom_bar(mapping = aes(x = Genre, fill = Genre)) +
  labs(y = 'Genre',
       title = "Games per Genre in the 2010's") +
  coord_polar()

# Create a chart showing the best selling games across all platforms
best_selling_game <- vg_data %>%
  group_by(Name) %>%
  summarize(Sales = sum(Global_Sales)) %>%
  arrange(desc(Sales))
best_selling_game

write.table(best_selling_game, file = 'best_selling_game.csv', sep = ',',
            col.names = NA, qmethod = 'double')

# Create a graph showing avg global sales per year
sales_per_year <- vg_data %>%
  group_by(Year) %>%
  summarize(Sales_NA = sum(NA_Sales),
            Sales_EU = sum(EU_Sales),
            Sales_JP = sum(JP_Sales),
            Sales_OT = sum(Other_Sales))

sales_per_year_line <- ggplot(data = sales_per_year)
sales_per_year_line + 
  geom_line(mapping = aes(x = Year, y = Sales_NA,
                          color = 'green', group = 1)) +
  geom_line(mapping = aes(x = Year, y = Sales_EU,
                          color = 'red', group = 2)) + 
  geom_line(mapping = aes(x = Year, y = Sales_JP,
                          color = 'blue', group = 3)) + 
  geom_line(mapping = aes(x = Year, y = Sales_OT,
                          color = 'purple', group = 4)) + 
  labs(x = 'Year', y = 'Total Sales',
       title = "Total Sales Per Year") + 
  guides(x = guide_axis(angle = 45)) +
  scale_fill_discrete(name = 'Region',
                      breaks = c("green", "purple", "red", "blue"),
                      labels = c("North America", "Europe", "Japan", "Other"))

# Most popular sports games in the 1990's
sports_nineties <- vg_data %>%
  filter(Decade == '1990-99', Genre == 'Sports') %>%
  group_by(Name) %>%
  summarize(Sales = sum(Global_Sales)) %>%
  arrange(desc(Sales))
sports_nineties

write.table(sports_nineties, file = 'sports_nineties.csv', sep = ',',
            col.names = NA, qmethod = 'double')
  