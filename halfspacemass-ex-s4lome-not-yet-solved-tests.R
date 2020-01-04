set.seed(1337)

## testing for input transformation 
# Two dimensional Matrix with n = 100, filled up with standard normal distribution 
# expect that bla für sampling 
test_data_2_dim <- matrix(data = rnorm(200), nrow = 100, ncol = 2)

test_df <- as.data.frame(test_data_2_dim)

sample(x = test_df, size = 10, replace = FALSE )

sample(as.matrix(test_df), size = 10, replace = FALSE)


# testing for random direction 
# schreib expect that norm = 1 bla 
for (i in 1:100) {
test <- random_direction(dimensions = 4)
print(test)

print(norm(as.matrix(test), type = "F"))
}
