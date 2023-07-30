library(tidyverse)

View(mpg) #command to view the dataset mpg 
?mpg

?mean #information about mean()

glimpse(mpg) #gives a glimpse into the dataset and datavalue of mpg dataset

?filter
filter(mpg, cty >= 20) #used to filter mpg column cty, where cty >= 20

mpg_efficient <- filter(mpg, cty >= 20) #assigning the variable name to the filtered column values

View(mpg_efficient) #can view the column we stored 

mpg_ford <- filter(mpg, manufacturer == "ford") #stores all the rows with manu ford

mpg_metric <- mutate(mpg, cty_metric = 0.425144 * cty) # mp/g to km/l

mpg_metric <- mpg %>%
  mutate(cty_metric = 0.425144 * cty) #mutate the column values of cty from mp/g to km/l

mpg %>%
  group_by(class) %>%
  summarise(mean(cty),
            median(cty)) #https://uc-r.github.io/pipe

#Data vizualization with ggplot

#histogram
ggplot(mpg, aes(x = cty)) + #aesthetic is the way of saying what sort of variables are going to be discussed in the plot: x axis is the cty
  geom_histogram() + 
  labs(x = "City mileage") #changing the cty label to city mileage

#frequency polygon
ggplot(mpg, aes(x = cty)) + 
  geom_freqpoly() + 
  labs(x = "City mileage")

#histogram and frrequency polygon together
ggplot(mpg, aes(x = cty)) + 
  geom_freqpoly() +
  geom_histogram() +
  labs(x = "City mileage")

#scatterplot cty vs hwy
ggplot(mpg, aes(x = cty,
                y = hwy,
                color = class)) + #differentiate colorwise according to the the class column 
  geom_point() + 
  geom_smooth(method = "lm") + #lm: linear (regression line)
  scale_color_brewer(palette = "Dark2") #used to change color palette of the graph




