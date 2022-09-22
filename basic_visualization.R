install.packages("tidyverse")
library(tidyverse)
View(diamonds)
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))



count(diamonds, cut)

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

ggplot(data = diamonds) +
  geom_density(mapping = aes(x = carat), fill = "blue")

smaller <- diamonds %>%
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_density()

ggplot(diamonds, mapping = aes(x = carat, y =  price)) +
  geom_point() +
  geom_smooth(method = "lm")

mod1<- lm(price ~ carat, diamonds)
summary(mod1)

