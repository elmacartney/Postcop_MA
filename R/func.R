# functions

##########
# 2 groups
##########

group2 <- function(m1, m2, sd1, sd2, n1, n2){ # m2 = higher/larger group
  n12 <- n1 + n2
  h <- n12/n1 + n12/n2
  p <- n1/n12 # prop for n1
  q <- n2/n12 # prop for n2
  s_pool <- sqrt( ((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n12 - 2) )
  j <- 1 - (3 / (4*n12 - 9))
  d <- ((m2 - m1) / s_pool) * j
  r_pb <-  d / sqrt(d^2 + h)
  r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
  r_b #r_b = r
}

# test
#group2(3,5, 2,2, 30, 60)

# 4 groups

group4 <- function(m1, m2, m3, m4, # m1 - smallest & m4 biggest
                   sd1, sd2, sd3, sd4, 
                   n1, n2, n3, n4,
                   sim = 100000){
  
  ordering <- c(rep(1, n1), rep(2, n2), rep(3, n3), rep(4, n4))
  
  vec <- numeric(length = sim) 
  
  for(i in 1:sim) {
  vec1 <- rnorm(n1, m1, sd1)
  vec2 <- rnorm(n2, m2, sd2)
  vec3 <- rnorm(n3, m3, sd3)
  vec4 <- rnorm(n4, m4, sd4)
  y <- c(vec1, vec2, vec3, vec4)
  vec[i] <- cor(ordering, y)
  }
  r <- mean(vec)
  r
}

# this takes a while
#group4(3, 6, 10, 12, 2, 2, 2, 2, 30, 30, 30, 30)

# estimate

est_se <- function(est, se, n1, n2){ # m2 = higher/larger group
  n12 <- n1 + n2
  #h <- n12/n1 + n12/n2
  p <- n1/n12 # prop for n1
  q <- n2/n12 # prop for n2
  t <- est/se
  r_pb <- t/sqrt(t^2+ n12 -2)
  
  r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
  r_b #r_b = r
}

# test
#est_se(3,1, 20 ,20)


# t values

t_vals <- function(t, n1, n2){ # m2 = higher/larger group
  n12 <- n1 + n2
  #h <- n12/n1 + n12/n2
  p <- n1/n12 # prop for n1
  q <- n2/n12 # prop for n2
  #t <- est/se
  r_pb <- t/sqrt(t^2+ n12 -2)
  
  r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
  r_b #r_b = r
}

# test
#t_vals(3, 30, 30)

# F values (sign required)

F_vals <- function(F_val, n1, n2, reverse = FALSE){ # m2 = higher/larger group
  n12 <- n1 + n2
  #h <- n12/n1 + n12/n2
  p <- n1/n12 # prop for n1
  q <- n2/n12 # prop for n2
  #t <- est/se
  r_pb <- sqrt(F_val)/sqrt(F_val + n12 -2)
  
  r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
  if(reverse == TRUE){
  r_b = r_b*(-1)}
  r_b
}

# test
#F_vals(9, 20, 20, reverse = TRUE)

# p value (sign required)

p_vals <- function(p_val, n1, n2, reverse = FALSE){ # m2 = higher/larger group
  n12 <- n1 + n2
  #h <- n12/n1 + n12/n2
  p <- n1/n12 # prop for n1
  q <- n2/n12 # prop for n2
  t <- qt(1 - p_val, n12 - 2)
  #t <- est/se
  r_pb <- t/sqrt(t^2 + n12 -2)
  
  r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
  if(reverse == TRUE){
    r_b = r_b*(-1)}
  r_b
}

# test
#p_vals(0.004, 20, 20, reverse = TRUE)

