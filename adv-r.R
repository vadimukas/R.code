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
#Factor
