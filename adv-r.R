# the book is here
# https://adv-r.hadley.nz/ 

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

  