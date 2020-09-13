# Coding in R
# Question 1
# It generates 1000 uniform random numbers between -10 to 10
vector_of_random_numbers = runif(n=1000, min =-10, max=10)
vector_of_random_numbers

# mean_vrn is the mean of these 1000 random numbers
mean_vrn = mean(vector_of_random_numbers)
mean_vrn

# Question 2
# You can get a full sentence by entering your name and age
name = readline(prompt = "Enter your name: ")
age = readline(prompt = "Enter your age: ")
print(paste('Your name is ', name, 'and your age is ', age))

# Question 3
# Getting your working directory
getwd()
directory = readline(prompt = "Enter your working directory: ")
# Changing working directory
setwd(directory)

# Question 4
# environment() tells you the current working environment
environment()
# ls() tells you what are the variables are in your current working environment
ls()

# Question 5
vector_of_numbers = c(0:150)
vector_of_numbers
# mean_vn is the mean of all numbers from 0 to 150
mean_vn = mean(vector_of_numbers)
mean_vn
# vector_of_numbers_divisible_by3 is all numbers which are divisible by 3 from 0 to 150
vector = which(0:150 %% 3 == 0)
new_vector = vector_of_numbers[c(vector)]
mean(new_vector)


# Question 6
vector_of_random_integer = as.integer(runif(n=10, min=-50, max=50))
vector_of_random_integer
# sum_vri is the sume of all 10 random integers between -50 to 50
sum_vri = sum(vector_of_random_integer)
sum_vri
# mean_vri is the mean of all 10 random integers between -50 to 50
mean_vri = mean(vector_of_random_integer)
mean_vri
# prod_vri is the product of all 10 random integers between -50 to 50
prod_vri = prod(vector_of_random_integer)
prod_vri

# Question 7
initial = as.integer(readline(prompt = "Initial value: "))
final = as.integer(readline(prompt = "Final value: "))
denom = as.integer(readline(prompt = "Denominator: "))
# Get all integers from initial to final
all_vector = initial:final
# Pick all integers which fit the condition of divisible by denom
new_vector = all_vector[all_vector %% denom == 0]
sum(new_vector)
mean(new_vector)
prod(new_vector)


# Math with R
# Question 1
number_sqrt = as.integer(readline(prompt = "Enter any positive number: "))
squareroot_number = sqrt(number_sqrt)
squareroot_number
# We cannot compute the square root of -1, since only numbers larger or equal to 0 can have
# square root numbers.

# Question 2
number_exp = as.integer(readline(prompt = "Enter a number: "))
exponential_number = exp(number_exp)
exponential_number

# Question 3
number_natural_log = as.integer(readline(prompt = "Enter a number: "))
natural_logarithm_number = log(number_natural_log)
natural_logarithm_number


# Calculus Questions
# Question 1
# The statement is true that if the derivative of a function is positive at each point in
# an interval, then the function is said to be continuously increasing in this interval. 
# This also applies when vice versa. 

# Question 2
# a). The domain is x >= -1
# b). The domain is all real numbers
