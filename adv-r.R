# the book is here
# https://adv-r.hadley.nz/ 
library(tidyverse)

# the books on solutions are here 
# https://advanced-r-solutions.rbind.io/ 
# the repository on github here 
# https://github.com/Tazinho/Advanced-R-Solutions 
  
?# data structures
#Atomic vectors are usually created with c(), short for combine:
dbl_var <- c(1, 2.5, 4.5)
# With the L suffix, you get an integer rather than a double
int_var <- c(1L, 6L, 10L)
# Use TRUE and FALSE (or T and F) to create logical vectors
log_var <- c(TRUE, FALSE, T, F)
chr_var <- c("these are", "some strings")

#Atomic vectors are always flat, even if you nest c()’s:
c(1, c(2, c(3, 4)))
# the same as
c(1, 2, 3, 4)

#Vectors
#Type, typeof(), what it is.
#Length, length(), how many elements it contains.
#Attributes, attributes(), additional arbitrary metadata.
typeof(int_var)
is.integer(int_var)
is.atomic(int_var)
is.double(dbl_var)
is.numeric(int_var)

#Coercion
x <- c(FALSE, FALSE, TRUE)
as.numeric(x)
sum(x)
mean(x)

#Lists
x <- list(1:3, "a", c(TRUE, FALSE, TRUE), c(2.3, 5.9))
str(x)

#Lists are sometimes called recursive vectors, because a list can contain other lists.
#This makes them fundamentally different from atomic vectors.

x <- list(list(list(list())))
str(x)
is.recursive(x)

#c() will combine several lists into one. If given a combination of atomic vectors and lists,
#c() will coerce the vectors to lists before combining them. Compare the results of list() and c():

x <- list(list(1, 2), c(3, 4))
y <- c(list(1, 2), c(3, 4))
str(x)
str(y)

#Attributes
#  attr(x, which) <- value 
#  with attributes() as a list
y <- 1:10
attr(y, "my_attribute") <- "This is a vector"
attr(y, "my_attribute")
str(attributes(y))
#The structure() function returns a new object with modified attributes:
structure(1:10, my_attribute = "This is a vector")

#Each of these attributes has a specific accessor function to get and set values.
#When working with these attributes, 
#use names(x), dim(x), and class(x), not attr(x, "names"), attr(x, "dim"), and attr(x, "class").

#Names
#You can name a vector in three ways:
x <- c(a = 1, b = 2, c = 3)
x <- 1:3; names(x) <- c("a", "b", "c")
x <- setNames(1:3, c("a", "b", "c"))
# removing names from a vector
names(x) <- NULL
#Factors
# A factor is a vector that can contain only predefined values, and is used to store categorical data.

x <- factor(c("a", "b", "b", "a"))
x 
class(x)
levels(x)
# You can't use values that are not in the levels
x[2] <- "c"
#Factors are useful when you know the possible values a variable may take, even if you don’t see all values
#in a given dataset. Using a factor instead of a character vector makes it obvious when some groups contain 
# no observations:

sex_char<-c("m", "m", "m")
sex_factor<-factor(sex_char, levels = c("m","f"))
table(sex_char)
table(sex_factor)
# Reading in "text" instead of from a file here:
z <- read.csv(text = "value\n12\n1\n.\n9")
typeof(z$value)
as.double
# Oops, that's not right: 3 2 1 4 are the levels of a factor, 
# not the values we read in!
class(z$value)
as.double<-as.character
# Or change how we read it in:
z <- read.csv(text = "value\n12\n1\n.\n9", na.strings=".")
typeof(z$value)
#While factors look (and often behave) like character vectors, they are actually integers. 
#Be careful when treating them like strings. 

#Matrices and arrays

#Matrices and arrays are created with matrix() and array(), or by using the assignment form of dim():
# Two scalar arguments to specify rows and columns
a <- matrix(1:6, ncol = 3, nrow = 2)
# One vector argument to describe all dimensions
b <- array(1:12, c(2, 3, 2))

# You can also modify an object in place by setting dim()
c <- 1:6
dim(c) <- c(3, 2)
c
rownames(a) <- c("A", "B")
colnames(a) <- c("a", "b", "c")

#data frames

df <- data.frame(
  x = 1:3,
  y = c("a", "b", "c"),
  stringsAsFactors = FALSE)
str(df)
typeof(df)
class (df)
is.data.frame(df)
# Combining data frames
# You can combine data frames using cbind() and rbind():
cbind(df, data.frame(z = 3:1))
rbind(df, data.frame(x = 10, y = "z"))
bad <- data.frame(cbind(a = 1:2, b = c("a", "b")))
str(bad)
good<-data.frame(cbind(a=1:2, b=c("a","b")),stringsAsFactors = FALSE)
str(good)
#Special columns
df <- data.frame(x = 1:3)
df$y<-list(1:2,1:3,1:4)
dfl <- data.frame(x = 1:3, y = I(list(1:2, 1:3, 1:4)))
str(dfl)
  
# Subsetting

#4.2.3 Matrices and arrays

a <- matrix(1:9, nrow = 3)
colnames(a) <- c("A", "B", "C")
a[1:2, ]
a[c(TRUE, FALSE, TRUE), c("B", "A")]


#4.3 Selecting a single element

x <- list(1:3, "a", 4:6)
x[[3]]

x<-1:5
x[c(1,2)]<-2:3
x<-2:3
x

x[-1] 
x[-1] <-4:1
x

x[c(1, 1)] <-2:3
x

mtcars[]<-lapply(mtcars, as.integer)
mtcars <-lapply(mtcars, as.integer)

# 4.5.1 Lookup tables (character subsetting)

x <- c("m", "f", "u", "f", "f", "m", "m")
lookup <- c(m = "Male", f = "Female", u = NA)
lookup[x]
unname(lookup[x])

# 4.5.2 Matching and merging by hand (integer subsetting

grades <- c(1, 2, 2, 3, 1)

info <- data.frame(
  grade = 3:1,
  desc = c("Excellent", "Good", "Poor"),
  fail = c(F, F, T)
)

id <- match(grades, info$grade)
id

info[id, ]

rownames(info) <- info$grade
info[as.character(grades),]

# 4.5.4 Ordering (integer subsetting)

x <- c("b", "c", "a")
order(x)

x[order(x)]

df <- data.frame(x = c(1, 2, 3, 1, 2), y = 5:1, z = letters[1:5])

# Randomly reorder df
df2 <- df[sample(nrow(df)), 3:1]
df2
df2[order(df2$x), ]
df2[, order(names(df2))]

#4.5.5 Expanding aggregated counts (integer subsetting)

df <- data.frame(x = c(2, 4, 1), y = c(9, 11, 6), n = c(3, 5, 1))
df
rep(1:nrow(df), df$n)
df[rep(1:nrow(df), df$n), ]

# 4.5.6 Removing columns from data frames (character )

# You can set individual columns to NULL:
df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df$z <- NULL
df

# Or you can subset to return only the columns you want:

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df[c("x", "y")]

df[setdiff(names(df), "z")]


# 4.5.7 Selecting rows based on a condition (logical subsetting)
# https://adv-r.hadley.nz/subsetting.html#selecting-rows-based-on-a-condition-logical-subsetting 

head(mtcars)

mtcars[mtcars$gear == 5, ]
mtcars[mtcars$gear == 5 & mtcars$cyl == 4, ]

subset(mtcars, gear == 5)
subset(mtcars, gear ==5 & cyl == 4)

# 4.5.8 Boolean algebra versus sets (logical and integer )

x <- sample(10) < 4
which(x)
#> [1] 2 3 4
? which()

unwhich <- function(x, n) {
  out <- rep_len(FALSE, n)
  out[x] <- TRUE
  out
}
unwhich(which(x), 10)
#>  [1] FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE

# Let’s create two logical vectors and their integer equivalents, and then explore the relationship between
# Boolean and set operations.

# vector 1 to 10 which divides on 2 (5) without decimal fraction (ostatka) or 
(x1 <- 1:10 %% 2 == 0)  
(x2 <- which(x1))
(y1 <- 1:10 %% 5 == 0)
(y2 <- which(y1))

x1 & y1
intersect(x2, y2)

# union
x1 | y1
x2
y2
union(x2, y2)

#setdiff
x1 & !y1
setdiff(x2, y2)

?xor()
# xor produces a compound expression that gives the EXCLUSIVE-OR of two expressions.
xor(x1, y1)
x1
y1

## Chapter 6  Functions  -------------------------------------------------------------
# https://adv-r.hadley.nz/functions.html#functions 

x <- 10
f1 <- function(x) {
  function() {
    x + 10
  }
}
f1(x)()

# install package pryr to explore what happens when modifying vectors in place

install.packages("pryr")

library(pryr)

#6.2 Function fundamentals
# To understand functions in R you need to internalise two important ideas:
  
#  - Functions can be broken down into three components: -- arguments,  --- body, and  --environment.
#  - There are exceptions to every rule, and in this case, there is a small selection of “primitive” base
# functions that are implemented purely in C.


# !!!Functions are objects, just as vectors are objects

# 6.2.1 Function components

# the body () - the code inside the function 

# the formals()  - the list of arguments

# the environment ()  - the "map" of the location of function variables


f02 <- function(x, y) {
  # A comment
  x + y
}

formals(f02)
body(f02)
environment(f02)

f02(3, 5)

attr(f02, "srcref")

# 6.2.4 Invoking a function
# what happens if you have the arguments already in a data structure?

# You can instead use do.call(): it has two arguments

args <- list(1:10, na.rm = TRUE)
do.call(mean, args)

# 6.3 Function composition

square <- function(x) x^2
deviation <- function(x) x - mean(x)

# runif generates random deviates
x<-runif(100, min = 1, max = 100)
square(x)
deviation(x)
hist(x)

# trying to build mine

f03 <- function(x) { 
      3* x + 7
}


body(f03)
formals(f03)
f03(5)

