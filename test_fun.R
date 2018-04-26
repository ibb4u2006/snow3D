#This is the first R script
test_fun <- function(a, b){
  c <- a*b
  return(c)
}

test_fun(3, 5)

test_fun(a = 3, b = 5)

test <- test_fun(b = 5, a = 3)

# this is such a cool function