##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Pipes in R
##########################################################################

x <- c(0.109, 0.359, 0.63, 0.996, 0.515, 0.142, 0.017, 0.829, 0.907)

# f(x) can be rewritten as x %>% f

# Compute the logarithm of `x` 
log(x)

# Compute the logarithm of `x` 
library(magrittr)
x %>% log()



# f(x, y) can be rewritten as x %>% f(y)
# Round pi
round(pi, 6)

# Round pi 
pi %>% round(6)



# Another example

# Compute the logarithm of `x`, return suitably lagged and iterated differences, 
# compute the exponential function and round the result
round(exp(diff(log(x))), 1)

# Exercise: repeat the previous formula using pipes



# The %<>%  pipe

x <- seq(-10,10)
x %<>% abs %>% sort
