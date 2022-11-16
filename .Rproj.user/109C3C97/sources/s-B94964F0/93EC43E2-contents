
library(tidyverse)
library(openintro)
yrbss <- na.omit(yrbss)

head(yrbss)
str(yrbss)

# Inference for Categorical Data
yrbss %>%
  group_by(school_night_hours_sleep) %>%
  summarize(count = n())

#Exercise 1

yrbss %>%
  group_by(text_while_driving_30d) %>%
  summarize(count = n())

#Exercise 2

no_helmet <- yrbss %>%
  filter(helmet_12m == "never")

no_helmet <- no_helmet %>%
  mutate(text_every_day = ifelse(text_while_driving_30d == "30", "yes", "no"))

no_helmet %>%
  group_by(text_every_day) %>%
  summarize(count = n()) %>%
  mutate(prop=count/sum(count))

#Exercise 3

p <- 332/(332+4012)

n <- 332+4012

upper.bound <- p + 1.96*sqrt(p*(1-p)/n)

lower.bound <- p - 1.96*sqrt(p*(1-p)/n)

lower.bound
upper.bound



#Exercise 4

1.96*sqrt(p*(1-p)/n)

#Exercise 5 (a)

no_helmet <- no_helmet %>%
  mutate(less5_hours_sleep = ifelse(school_night_hours_sleep == "<5", "yes", "no"))

no_helmet %>%
  group_by(less5_hours_sleep) %>%
  summarize(count = n())

p <- 322/(322+4022)

n <- 322+4022

upper.bound <- p + 1.96*sqrt(p*(1-p)/n)

lower.bound <- p - 1.96*sqrt(p*(1-p)/n)

lower.bound
upper.bound

#Exercise 5 (b)

no_helmet <- no_helmet %>%
  mutate(train_every_day = ifelse(physically_active_7d==7,"yes","no"))

no_helmet %>%
  group_by(train_every_day) %>%
  summarize(count = n())

p <- 1330/(1330+3014)

n <- 1330+3014

upper.bound <- p + 1.96*sqrt(p*(1-p)/n)

lower.bound <- p - 1.96*sqrt(p*(1-p)/n)

lower.bound
upper.bound

# How does the proportion affect the margin of error?

n <- 1000
p <- seq(from = 0, to = 1, by = 0.01)
me <- 2 * sqrt(p * (1 - p)/n)

dd <- data.frame(p = p, me = me)

ggplot(data = dd, aes(x = p, y = me)) +
  geom_line() +
  labs(x = "Population Proportion", y = "Margin of Error")


# Success-failure condition

#Exercise 6

p <- 0.15
n <- 10

data1 <- rnorm(n=n, mean=p, sd=sqrt(p*(1-p)/n))

data <- as.data.frame(data1)

p1 <- ggplot(data, aes(x = data1)) +
  geom_histogram(color = "white") +
  xlim(0, 0.3) +
  labs(y ="Count", x = "p-hats", title = "n=10")

p <- 0.15
n <- 100

data1 <- rnorm(n=n, mean=p, sd=sqrt(p*(1-p)/n))

data <- as.data.frame(data1)

p2 <- ggplot(data, aes(x = data1)) +
  geom_histogram(color = "white") +
  xlim(0, 0.3) +
  labs(y ="Count", x = "p-hats", title = "n=100")

p <- 0.15
n <- 1000

data1 <- rnorm(n=n, mean=p, sd=sqrt(p*(1-p)/n))

data <- as.data.frame(data1)

p3 <- ggplot(data, aes(x = data1)) +
  geom_histogram(color = "white") +
  xlim(0, 0.3) +
  labs(y ="Count", x = "p-hats", title = "n=1000")

grid.arrange(p1,p2,p3, ncol=1)



#Exercise 7

p <- 0.05
n <- 100

data1 <- rnorm(n=n, mean=p, sd=sqrt(p*(1-p)/n))

data <- as.data.frame(data1)

p1 <- ggplot(data, aes(x = data1)) +
  geom_histogram(color = "white") +
  xlim(0, 1) +
  labs(y ="Count", x = "p-hats", title = "p=0.05")

p <- 0.15
n <- 100

data1 <- rnorm(n=n, mean=p, sd=sqrt(p*(1-p)/n))

data <- as.data.frame(data1)

p2 <- ggplot(data, aes(x = data1)) +
  geom_histogram(color = "white") +
  xlim(0, 1) +
  labs(y ="Count", x = "p-hats", title = "p=0.15")

p <- 0.35
n <- 100

data1 <- rnorm(n=n, mean=p, sd=sqrt(p*(1-p)/n))

data <- as.data.frame(data1)

p3 <- ggplot(data, aes(x = data1)) +
  geom_histogram(color = "white") +
  xlim(0, 1) +
  labs(y ="Count", x = "p-hats", title = "p=0.35")

grid.arrange(p1,p2,p3, ncol=1)


# Group Tasks

download.file("http://www.openintro.org/stat/data/atheism.RData", destfile = "atheism.RData")
load("atheism.RData")
str(atheism)
