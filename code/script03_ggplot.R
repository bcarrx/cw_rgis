library(tidyverse)



# point figure -------------ctrl + shift + R-----------------------------------

## ctrl shift m for pipe 
iris %>% 
  ggplot(
    aes(x = Sepal.Length, 
        y = Sepal.Width)
  ) + geom_point()
## point information with data, color in aes
iris %>% 
  ggplot(
    aes(x = Sepal.Length, 
        y = Sepal.Width,
        color = Species)
  ) + geom_point()
## point information without data, color in geom
iris %>% 
  ggplot(
    aes(x = Sepal.Length, 
        y = Sepal.Width)
  ) + geom_point( color = "maroon")


# line figure -------------------------------------------------------------

df_x <- tibble(x = 1:50,
               y = x * 2)
df_x %>%
  ggplot(
    aes(x = x, 
        y = y)
  ) + geom_line()


# histogram ---------------------------------------------------------------

iris %>% 
  ggplot(
    aes(x = Sepal.Length)
  ) + geom_histogram(fill = "purple")


# boxplot -----------------------------------------------------------------
## bottom up, min?, 25%, median, 75% max?


iris %>% 
  ggplot(
    aes(x = Species, y = Sepal.Length)
  ) + geom_boxplot()

iris %>% 
  ggplot(
    aes(x = Species, y = Sepal.Length, fill = Species)
  ) + geom_boxplot()


# exercise ---------------------------------------------------------------

# Q1 Using iris, identify the longest Sepal.Length using arrange()
iris %>%
  arrange(desc(Sepal.Length))
# A1 7.9

#Q2 Using iris, filter individuals with Sepal.Width > 3.0

iris %>% 
  filter(Sepal.Width > 3.0)

#Q3 Using iris, select the columns Petal.Length/Width
# and arrange the order of rows by Petal.Length
# Assign result to object "df_petal"

df_petal <- iris %>% 
  select(Petal.Length, Petal.Width) %>% 
  arrange(desc(Petal.Length))
view(df_petal)

#Q4 Calculate mean Sepal Width by Species

df_mean <- iris %>%
  group_by(Species) %>% 
  summarize(mean(Sepal.Width)) %>% 
ungroup()
view(df_mean)

#Q5 Create a point figure of petal width (y) and sepal width (x)
# with colors distinguishing species

iris %>% 
  ggplot(aes(x = Sepal.Width,
             y = Petal.Width,
             color = Species)) + 
  geom_point()