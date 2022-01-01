#following https://www.r-bloggers.com/2021/05/getting-started-in-rtistry/

# Load packages ----
library(tidyverse)
library(viridis)
library(ggdark)

# write a parametric equation ----
circleFun <- function(center = c(0, 0), diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
dat <-
  circleFun(c(1, -1), 2.3, npoints = 100)


ggplot(dat,aes(x, y)) +
  geom_path() +
  coord_equal()

#  function to create parametric equation ----


# center: the center of the equation
# npoints: the number of points used to evaluate the function.
# c1, c2, c3, c4: the coefficients for the equation

genFun <- function(center = c(0, 0), 
                   npoints = 500, 
                   c1 = 2.5, c2 = -5, c3 = 4.28, c4 = 2.3){
  t <- seq(0, 2*pi, length.out = npoints)
  xx <- center[1] + c1*(sin(c2*t)*sin(c2*t))*(2^cos(cos(c3*c4*t)))
  yy <- center[2] + c1*sin(sin(c2*t))*(cos(c3*c4*t)*cos(c3*c4*t))
  a <- data.frame(x = xx, y = yy)
  return(a)
}

dat <-
  genFun(c(1,-1), npoints = 100) %>% 
  mutate(index = 1:n())

ggplot(dat, aes(x, y)) +
  geom_path(aes(color = index)) +
  coord_equal()

dat <-
  genFun(c(1,-1), npoints = 500, c1 = 5, c2 = -3, c3 = 5, c4 = 2)
ggplot(dat, aes(x, y)) +
  geom_path()+ coord_equal()


# graphing ----

dat <- genFun(c(1,-1), npoints = 5000)

#basic plot definition
p <- ggplot(dat, aes(x,y)) +
  coord_equal()

# different geoms
p + geom_path()

p + geom_line()

p + geom_point()

# define some geom parameters
set.seed(1111)
dat <-
  genFun(c(1,-1), npoints = 5000) %>%
  mutate(rand_w = sample(n())/3000) %>% 
  mutate(seq_w = seq(from = 0.1, to = 3, length.out = n())) %>% 
  mutate(rand_c = sample(n())) %>% 
  mutate(seq_c = seq(from = 0.1, to = 3, length.out = n()))


# basic plot definition
p <- ggplot(dat, aes(x, y)) +
  theme_void() +
  coord_equal()


# start changing params
p + geom_point(size = dat$rand_w) 

p + geom_point(size = dat$x^(1.01),
               shape = 9) 

p + geom_point(size = sin(dat$seq_w)*2) 

p + geom_point(aes(color = dat$seq_c, 
                   size = 1/dat$seq_w)) + 
  scale_color_viridis("viridis") +
  coord_flip() +
  dark_theme_void() +
  theme(legend.position = "none")


#  add more params to generative function ----

genFun <- function(center = c(0, 0), npoints = 500, c1 = 2.5, c2 = -5, c3 = 4.28, c4 = 2.3, size_denom = 1, opacity_denom = 1, color_denom = 1){
  t <- seq(0, 2*pi, length.out = npoints)
  xx <- center[1] + c1*(sin(c2*t)*sin(c2*t))*(2^cos(cos(c3*c4*t)))
  yy <- center[2] + c1*sin(sin(c2*t))*(cos(c3*c4*t)*cos(c3*c4*t))
  rand_w <- sample(0:20, npoints, replace = TRUE)/size_denom
  rand_o <- sample(1:100, npoints, replace = TRUE)/opacity_denom
  rand_c <- sample(1:100, npoints, replace = TRUE)/color_denom
  a <- data.frame(x = xx, y = yy, rand_w = rand_w, rand_o = rand_o, rand_c = rand_c)
  return(a)
}

set.seed(1111)
dat <-
  genFun(c(0, 0), npoints = 5000, c1 = 5, c2 = -3, c3 = 5, c4 = 2, size_denom = 1.5, opacity_denom = 50)
dat %>%
  ggplot(aes(x, y, color = rand_c)) +
  geom_point(size = dat$rand_w,
             alpha = dat$rand_o) +
  scale_color_viridis(option = "magma") +
  dark_theme_void() +
  theme(legend.position = "none") # remove legend

# same but with added rotation

dat %>%
  ggplot() +
  geom_point(aes(x, y, color = rand_c),
             size = dat$rand_w,
             alpha = dat$rand_o) +
  geom_point(aes(-x, -y, color = rand_c),
             size = dat$rand_w,
             alpha = dat$rand_o) +
  geom_point(aes(-y, x, color = rand_c),
             size = dat$rand_w,
             alpha = dat$rand_o) +
  geom_point(aes(-y, -x, color = rand_c),
             size = dat$rand_w,
             alpha = dat$rand_o) +
  scale_color_viridis(option = "magma") +
  dark_theme_void() +
  theme(legend.position = "none")


# trying a more complex one

genFun <- function(center = c(0, 0), npoints = 500, c1 = 7, c2 = 1.84, c3 = 1.92, 
                   size_denom = 1, opacity_denom = 1, color_denom = 1){
  t <- seq(from = -3, to = 3, length.out = npoints)
  xx <- center[1] + c1 * (cos(cos(c2 * round(t)))^2) * (1 + cos(c3 * t)^4)
  yy <- center[2] + c1 * (sin(sin(c2 * round(t)))^2) * sin(sin(c3 * t))
  rand_w <- sample(0:20, npoints, replace = TRUE)/size_denom
  rand_o <- sample(1:100, npoints, replace = TRUE)/opacity_denom
  rand_c <- sample(1:100, npoints, replace = TRUE)/color_denom
  a <- data.frame(x = xx, y = yy, rand_w = rand_w, rand_o = rand_o, rand_c = rand_c)
  return(a)
}

set.seed(1111)
dat <-
  genFun(c(0, 0), npoints = 5000, c1 = 7, c2 = 1.84, c3 = 1.92, 
         size_denom = 1.5, opacity_denom = 50)
dat <- data.frame(x = seq(from = 0, to = 5, length.out = 100), 
                  y = seq(from = 0, to = 5, length.out = 100))

dat %>%
  ggplot() +
  # geom_path(aes(x, y)) +
  geom_path(aes(-x, y)) +
  # geom_path(aes(-x, -y)) +
  # geom_path(aes(x, -y)) +
  geom_path(aes(x, y)) +
  # geom_path(aes(-x, y)) +
  # geom_path(aes(-x, -y)) +
  # geom_path(aes(x, -y)) +
  coord_equal() +
#  scale_color_viridis(option = "magma") +
#  theme_void() +
  theme(legend.position = "none") # remove legend

