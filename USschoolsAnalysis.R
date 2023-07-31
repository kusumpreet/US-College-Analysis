library(tidyverse)
library(ISLR2) #Introduction to statistical learning with applications in R, second edition

?College
glimpse(College)


ggplot(College, aes(x = Grad.Rate)) + 
  geom_histogram()
#good: its got a general bell shape, which is good
#bad: there is a outlier, which is a red flag because grad-rates should not go above 100%

#trying to filter out the suspicious row data: where the grad-rate is > 100
suspicious <- filter(College, Grad.Rate >= 100) 
View(suspicious)

ggplot(College, aes(x = log10(F.Undergrad), #log10 is just a scaling factor, here we are scaling our F.undergrad data by a log of 10 so that it is eaaasier to read
                    y = Grad.Rate)) +
  geom_point()

#since it is more easier to visualize F.Undergrad by log10 we should just mutate the column to log10
#and make a new database 
College_sm <- College %>% 
  mutate(log_full = log10(F.Undergrad)) %>% 
  select(Grad.Rate,
         log_full,
         Private,
         Top25perc)
View(College_sm)


ggplot(College, aes(x = Grad.Rate)) + 
  geom_histogram() +
  geom_smooth(method = "lm")
#the line is pretty straight and horizontal, which means that full time undergraduate population is not a good predictor of grad rate

model_undergrad <- lm(Grad.Rate ~ log_full,
                      data = College_sm)
summary(model_undergrad)
plot(model_undergrad)

#Residuals vs fitted: it seems to be relatively linear 
#Normal Q-Q: shows that there is a general skew in the data
#Scale-Location: gving us the information about the homoskeadestity of the data. 
#residuals vs leverage: it talks about the outliers(if there is an outlier, it would be present beyond the line of cooks ratio)

ggplot(College, aes(x = log10(F.undergrad),
                    y = Grad.Rate,
                    color = Private)) + #the graph is plotted according to full time undergraduate student vs grad rate, while the difference between private and public schools is shown using different colors
  geom_point() + #scatterplot
  geom_smooth(method = "lm",
              se = FALSE) + #se: confidence interval around smooth
  scale_color_brewer(palette = "Dark2")
#from the linear line and the scatter plot we can make out that the scatter plot of private schools is higher and a bit towards the left
#which could be inferred as it having lower amount of participants as compared to public schools and also a higher graduating rate,
#wheraeas public school is lower and towards the right

#smaller p values indicate higher statistical significance:
#therefore, the full time undergraduate enrollment is not a meaningful explaination to graduation rate.


model_private <- lm(Grad.Rate ~ Private + log_full, data = College_sm)
summary(model_private)
#the r values of both Private and log_full are very small which indicates that those values are very significant


#interaction: privat and F.Undergrad
model_private_in <- lm(Grad.Rate ~ Private * log_full, data = College_sm)
summary(model_private_in)

anova(model_private_in)

#what about top25percent?
model_top <- lm(GradRate ~ Private + log_full + Top25perc, data = College_sm)
summary(model_top)
#R^2 for this model is 0.33 substantially explaining more than any of the other models
plot(model_top)







