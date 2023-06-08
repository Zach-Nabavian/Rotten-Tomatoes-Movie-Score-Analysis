library(tidyverse)
library(lubridate)

movies <- read_csv('/Users/Honors/Desktop/Portfolio Projects/Normalized Movie Scores.csv')

str(movies)

## Scatter plot with average scores and fresh percentage, with no filtering
movies %>% 
  ggplot(aes(x = avg_score, y = percent_fresh)) +
  geom_point(position = "jitter") +
  labs(title = 'Rotten Tomatoes Rating vs Average Critic store',
       x = 'Average Score',
       y = 'Rotten Tomatoes Rating') +
  ggsave("scatter.png")



## Second scatter plot removing erroneous datapoints
movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  ggplot(aes(x = avg_score, y = percent_fresh)) +
  geom_point(position = "jitter") +
  labs(title = 'Rotten Tomatoes Rating vs Average Critic store',
       x = 'Average Score',
       y = 'Rotten Tomatoes Rating')


movies %>% 
  ggplot(aes(y = num_reviews)) +
  geom_boxplot() +
  labs(y = 'Number of Reviews')

# Filter for well known movies: movies with more than 100 reviews, then do a scatter plot  
movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(num_reviews >= 100) %>% 
  ggplot(aes(x = avg_score, y = percent_fresh)) +
  geom_point(position = "jitter") +
  labs(title = 'Rotten Tomatoes Rating vs Average Critic store',
       x = 'Average Score',
       y = 'Rotten Tomatoes Rating') +
  geom_smooth(method=lm, se=FALSE) +
  ggsave("scatter_popular.png")


# Narrow the scatter plot down to where there is less of a relationship between
# average review score and rotten tomatoes score
movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(num_reviews >= 100) %>% 
  filter(avg_score >= 60 & percent_fresh >= 50) %>% 
  ggplot(aes(x = avg_score, y = percent_fresh)) +
  geom_point(position = "jitter") +
  geom_smooth(method=lm, se=FALSE) +
  labs(title = 'Rotten Tomatoes Rating vs Average Critic store',
       x = 'Average Score',
       y = 'Rotten Tomatoes Rating')
  
# Narrow the scatter plot's range even farther, then categorize the four kinds of movies
# among the highest rated ones
movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(num_reviews >= 100) %>% 
  filter(avg_score >= 60  & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
      avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
      avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
      avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
      avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>% 
    ggplot(aes(x = avg_score, y = percent_fresh)) +
    stat_smooth(method = lm, se = FALSE) +
    geom_point(position = "jitter", aes(color = consensus)) +
    scale_color_discrete(breaks=c('Universally Loved', 'Loved by Some', 'Universally Liked', 'Not as Well Liked')) +
  labs(title = 'Rotten Tomatoes Rating vs Average Critic store',
       x = 'Average Score',
       y = 'Rotten Tomatoes Rating') +
  ggsave("scatter_categorize.png")

# Check the size of each movie category
movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(num_reviews >= 100) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>% 
  group_by(consensus) %>% 
  summarize(n())

# Check the highest rated movies in the 'Universally loved' category
universally_loved <- movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(num_reviews >= 100) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>% 
  filter(consensus == 'Universally Loved') %>% 
  select(movie_title, genres, original_release_date, runtime, audience_rating, avg_score, percent_fresh) %>% 
  arrange(desc(avg_score), desc(percent_fresh))

write_csv(universally_loved, 'universally_loved.csv')

# Check the top ten movies among the 'loved by some' category
loved_by_some <- movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(num_reviews >= 100) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>% 
  filter(consensus == 'Loved by Some') %>% 
  select(movie_title, genres, original_release_date, runtime, audience_rating, avg_score, percent_fresh) %>% 
  arrange(desc(avg_score), desc(percent_fresh))

write_csv(universally_loved, 'loved_by_some.csv')
  
# Check the top ten movies among the 'Universally liked' category
universally_liked <- movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(num_reviews >= 100) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>% 
  filter(consensus == 'Universally Liked') %>% 
  select(movie_title, genres, original_release_date, runtime, audience_rating, avg_score, percent_fresh) %>% 
  arrange(desc(avg_score), desc(percent_fresh))

write_csv(universally_liked, 'universally_liked.csv')

# Check the top ten movies among the 'Not as well liked' category
not_as_well_liked <- movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(num_reviews >= 100) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>% 
  filter(consensus == 'Not as Well Liked') %>% 
  select(movie_title, genres, original_release_date, runtime, audience_rating, avg_score, percent_fresh) %>% 
  arrange(desc(avg_score), desc(percent_fresh))

write_csv(not_as_well_liked, 'not as well liked.csv')

movies %>% 
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>% 
  filter(str_detect(movie_title, 'Avengers')) %>% 
  select(movie_title, original_release_date, avg_score, percent_fresh, consensus) %>% 
  arrange(desc(avg_score), desc(percent_fresh))

movies %>% 
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>% 
  filter(movie_title == 'Finding Nemo') %>% 
  select(movie_title, original_release_date, avg_score, percent_fresh, consensus) %>% 
  arrange(desc(avg_score), desc(percent_fresh))

# Bar graph of how many top-rated movies were released each year
movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(num_reviews >= 100) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>%
  filter(original_release_date != 'NULL') %>% 
  mutate(original_release_date = ymd(original_release_date)) %>%
  ggplot(aes(x = year(original_release_date))) +
  geom_bar(fill = 'orange') +
  labs(title = 'Number of Movies Released Each Year',
       x = 'year',
       y = 'count') 

movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(num_reviews >= 100) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>%
  mutate(original_release_date = ymd(original_release_date)) %>%
  count(year(original_release_date) < 2000)

movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(num_reviews >= 100) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>%
  mutate(original_release_date = ymd(original_release_date)) %>%
  filter(consensus == 'Universally Loved') %>% 
  count(year(original_release_date) < 2000)
  

# Another bar graph just for the universally loved movies
movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(num_reviews >= 100) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>%
  filter(original_release_date != 'NULL') %>% 
  mutate(original_release_date = ymd(original_release_date)) %>%
  filter(consensus == 'Universally Loved') %>% 
  ggplot(aes(x = year(original_release_date))) +
  geom_bar(fill = 'purple') +
  labs(title = 'Number of Movies Released Each Year',
       x = 'year',
       y = 'count')

# Same graph for universally liked movies
movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(num_reviews >= 100) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>%
  filter(original_release_date != 'NULL') %>% 
  mutate(original_release_date = ymd(original_release_date)) %>%
  filter(consensus == 'Universally Liked') %>% 
  ggplot(aes(x = year(original_release_date))) +
  geom_bar(fill = 'blue') +
  labs(title = 'Number of Movies Released Each Year',
       x = 'year',
       y = 'count')

# Same graph for 'loved by some' movies
movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(num_reviews >= 100) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>%
  filter(original_release_date != 'NULL') %>% 
  mutate(original_release_date = ymd(original_release_date)) %>%
  filter(consensus == 'Loved by Some') %>% 
  ggplot(aes(x = year(original_release_date))) +
  geom_bar(fill = 'red') +
  labs(title = 'Number of Movies Released Each Year',
       x = 'year',
       y = 'count')

# Same graph for the 'not as well liked' movies
movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(num_reviews >= 100) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>%
  filter(original_release_date != 'NULL') %>% 
  mutate(original_release_date = ymd(original_release_date)) %>%
  filter(consensus == 'Not as Well Liked') %>% 
  ggplot(aes(x = year(original_release_date))) +
  geom_bar(fill = 'green') +
  labs(title = 'Number of Movies Released Each Year',
       x = 'year',
       y = 'count')


# Check the average number of reviews each movie recieved each year
movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>%
  filter(original_release_date != 'NULL') %>% 
  mutate(original_release_date = ymd(original_release_date)) %>% 
  group_by(year = year(original_release_date)) %>% 
  summarize(reviews = mean(num_reviews)) %>% 
  ggplot(aes(x = year, y = reviews)) +
  geom_col(fill = 'red') +
  labs(title = 'Average Number of Reviews Per Year') +
  ggsave("Average Reviews.png")

# See the proportion of universally loved movies each year
movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>%
  filter(original_release_date != 'NULL') %>% 
  mutate(original_release_date = ymd(original_release_date)) %>% 
  group_by(year = year(original_release_date)) %>% 
  summarize(proportion =  sum(consensus == 'Universally Loved')/n()) %>% 
  ggplot(aes(x = year, y = proportion)) +
  geom_col(fill = 'purple') +
  labs(title = 'Proportion of Universally Loved Movies Each Year') +
  ggsave('proportion consensus.png')

# See the proportion of loved by some movies each year
movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>%
  filter(original_release_date != 'NULL') %>% 
  mutate(original_release_date = ymd(original_release_date)) %>% 
  group_by(year = year(original_release_date)) %>% 
  summarize(proportion =  sum(consensus == 'Loved by Some')/n()) %>% 
  ggplot(aes(x = year, y = proportion)) +
  geom_col(fill = 'red') +
  labs(title = 'Proportion of "Loved by Some" Movies Each Year') +
  ggsave('proportion consensus loved by some.png')

# See the proportion of universally liked movies each year
movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>%
  filter(original_release_date != 'NULL') %>% 
  mutate(original_release_date = ymd(original_release_date)) %>% 
  group_by(year = year(original_release_date)) %>% 
  summarize(proportion =  sum(consensus == 'Universally Liked')/n()) %>% 
  ggplot(aes(x = year, y = proportion)) +
  geom_col(fill = 'blue') +
  labs(title = 'Proportion of Universally Liked Movies Each Year') +
  ggsave('proportion consensus universally liked.png')

# See the proportion of "not as well liked" movies each year
movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>%
  filter(original_release_date != 'NULL') %>% 
  mutate(original_release_date = ymd(original_release_date)) %>% 
  group_by(year = year(original_release_date)) %>% 
  summarize(proportion =  sum(consensus == 'Not as Well Liked')/n()) %>% 
  ggplot(aes(x = year, y = proportion)) +
  geom_col(fill = 'green') +
  labs(title = 'Proportion of Less Liked Movies Each Year') +
  ggsave('proportion consensus less liked.png')

# Area graph plotting all four categories

movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>%
  filter(original_release_date != 'NULL') %>% 
  mutate(original_release_date = ymd(original_release_date)) %>% 
  group_by(year = year(original_release_date), consensus) %>% 
  summarize(count =  n()) %>% 
  ggplot(aes(x = year, y = count, fill = consensus)) +
  geom_area(position = 'fill') +
  labs(title = 'Proportion of Less Liked Movies Each Year', y = 'proportion') +
  ggsave('proportion consensus area graph.png')

# Bar graph showing the number of movies in each category
movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(num_reviews >= 100) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>% 
  mutate(consensus = factor(consensus, levels = c('Universally Loved', 'Loved by Some', 'Universally Liked', 'Not as Well Liked'))) %>% 
  ggplot(aes(x = consensus)) +
  geom_bar(fill = 'orange') +
  labs(title = 'Movies by Consensus Category') +
  ggsave("categories bar plot.png")

# Now plot same graph while filtering for genre
movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(num_reviews >= 100) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>% 
  filter(grepl('Comedy', genres)) %>% 
  mutate(consensus = factor(consensus, levels = c('Universally Loved', 'Loved by Some', 'Universally Liked', 'Not as Well Liked'))) %>% 
  ggplot(aes(x = consensus)) +
  geom_bar(fill = 'red') +
  labs(title = 'Movies by Consensus Category')

movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(num_reviews >= 100) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>% 
  filter(grepl('Action & Adventure', genres)) %>% 
  mutate(consensus = factor(consensus, levels = c('Universally Loved', 'Loved by Some', 'Universally Liked', 'Not as Well Liked'))) %>% 
  ggplot(aes(x = consensus)) +
  geom_bar(fill = 'green') +
  labs(title = 'Movies by Consensus Category')

movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(num_reviews >= 100) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>% 
  filter(grepl('Classics', genres)) %>% 
  mutate(consensus = factor(consensus, levels = c('Universally Loved', 'Loved by Some', 'Universally Liked', 'Not as Well Liked'))) %>% 
  ggplot(aes(x = consensus)) +
  geom_bar(fill = 'blue') +
  labs(title = 'Movies by Consensus Category')

movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(num_reviews >= 100) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>% 
  filter(grepl('Mystery & Suspense', genres)) %>% 
  mutate(consensus = factor(consensus, levels = c('Universally Loved', 'Loved by Some', 'Universally Liked', 'Not as Well Liked'))) %>% 
  ggplot(aes(x = consensus)) +
  geom_bar(fill = 'purple') +
  labs(title = 'Movies by Consensus Category')


movies %>% 
  filter(!(avg_score < 50 & percent_fresh == 100)) %>% 
  filter(!(avg_score > 70 & percent_fresh == 0)) %>% 
  filter(num_reviews >= 100) %>% 
  filter(avg_score >= 60 & percent_fresh >= 80) %>%
  mutate(consensus = case_when(
    avg_score >= 80 & percent_fresh >= 90 ~ 'Universally Loved',
    avg_score >= 80 & percent_fresh <= 90 ~ 'Loved by Some',
    avg_score <= 80 & percent_fresh >= 90 ~ 'Universally Liked',
    avg_score <= 80 & percent_fresh <= 90 ~ 'Not as Well Liked')) %>% 
  filter(grepl('Drama', genres)) %>% 
  mutate(consensus = factor(consensus, levels = c('Universally Loved', 'Loved by Some', 'Universally Liked', 'Not as Well Liked'))) %>% 
  ggplot(aes(x = consensus)) +
  geom_bar(fill = 'yellow') +
  labs(title = 'Movies by Consensus Category')
