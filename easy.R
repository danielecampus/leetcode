# create a fibonacci series

fibonacci <- function(n){
  x <- c(1:n)
  x[2] <- 1
  for (i in 3:n){
    x[i] <- x[i-1]+x[i-2]
  }
  return(x)
}

n <- 10
fibonacci_values <- fibonacci(n)

################################################################

# ordering data without using an in-built function
order_data <- function(x) {
  n <- length(x)  
  for (i in 1:(n-1)) {
    for (j in 1:(n-i)) {    # Compare each element with the next one
      if (x[j] > x[j+1]) {  # Swap if the current element is greater than the next
        temp <- x[j]  
        x[j] <- x[j+1]
        x[j+1] <- temp
      }
    }
  }
  return(x) 
}

n <- runif(20)
m <- sample(1:100, size = 20)
data_sorted <- list(
  n_sorted <- order_data(n),
  m_sorted <- order_data(m)
)

################################################################

# compute the factorial of an integer without using the built-in function

factorial_fun <- function(n){
  x <- c(1:n) %>% sort(, decreasing = T)
  y <- n
  for (i in 2:n){
    z <- y * x[i]
    y <- z
  }
  return(y)
}

n <- 5
print(factorial_fun(n))

################################################################

# Given an array of integers nums and an integer target, 
# return the INDICES of the two numbers such that they add up to target

two_sum <- function(nums, n_target) {
  hashmap <- list()
  for (i in 1:length(nums)) {
    # Calculate the complement of the current number
  complement <- n_target - nums[i]
    # Check if the complement exists
    if (complement %in% names(hashmap)) {
      return(c(hashmap[[as.character(complement)]], i))  # Return indices
    }
    hashmap[[as.character(nums[i])]] <- i 
  }
  return(NULL)  # Return NULL if no solution found
}

nums <- c(11, 7, 4, 2, 15)
n_target <- 9
print(two_sum(nums, n_target))

################################################################

# Given an integer x, return true if x is a palindrome, and false otherwise.

is_palindrome <- function(x) {
  # Convert to string, split the string, reverse it and check if it is equal to the first string
  x_str <- as.character(x)
  return(x_str == paste(rev(strsplit(x_str, NULL)[[1]]), collapse = "")) 
}

x <- "anna"
print(is_palindrome(x)) 

################################################################

# Given a sorted array nums, remove the duplicates in place 
# such that each element appears only once and returns the new length.

remove_duplicates <- function(nums) {
  if (length(nums) == 0) return(0)
  unique_pos <- 1
  output <- nums
  
  for (i in 2:length(nums)) {
    if (nums[i] != output[unique_pos]) {
      unique_pos <- unique_pos + 1
      output[unique_pos] <- nums[i]
    }
  }
  nums[1:unique_pos] <<- output[1:unique_pos]
  return(unique_pos)  # Return the new length
}

nums <- c(1, 1, 2, 3, 3)
new_length <- remove_duplicates(nums)
print(new_length)  
print(nums[1:new_length]) 

################################################################

# Merge two sorted linked lists into one sorted list.

merge_sorted_lists <- function(l1, l2) {
  result <- c()
  i <- 1
  j <- 1
  
  while (i <= length(l1) && j <= length(l2)) {
    if (l1[i] < l2[j]) {
      result <- c(result, l1[i])
      i <- i + 1
    } else {
      result <- c(result, l2[j])
      j <- j + 1
    }
  }
  
  if (i <= length(l1)) result <- c(result, l1[i:length(l1)])
  if (j <= length(l2)) result <- c(result, l2[j:length(l2)])
  
  return(result)
}

l1 <- c(1, 2, 4)
l2 <- c(1, 3, 4)
print(merge_sorted_lists(l1, l2)) 

################################################################

# Given an integer array nums, find the contiguous subarray 
# (containing at least one number) which has the largest sum and return its sum.

max_subarray_sum <- function(nums) {
  max_sum <- nums[1]
  current_sum <- nums[1]
  
  for (i in 2:length(nums)) {
    current_sum <- max(nums[i], current_sum + nums[i])
    max_sum <- max(max_sum, current_sum)
  }
  
  return(max_sum)
}

nums <- c(-2, 1, -3, 4, -1, 2, 1, -5, 4)
print(max_subarray_sum(nums))

################################################################

# Given a non-empty array of integers nums, every element appears twice,
# except for one. Find that single one.

single_number <- function(nums) {
  freq <- table(nums)
  result <- as.numeric(names(freq)[freq == 1])
  return(result)
}

nums <- c(2, 1, 3, 1, 3, 5, 5)
print(single_number(nums))  

################################################################
