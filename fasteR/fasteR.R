#https://github.com/matloff/fasteR

#First R steps
#working with Nile dataset (bild-in in R)
mean(Nile)
#printing out Nile
Nile
Start = 1871 
End = 1970 
Frequency = 1
#making histogram of Nile dataset with default bins and 20 bins
hist(Nile)
hist(Nile,breaks = 20)

#First, note that a set of numbers such as Nile is called a vector. 
# Individual elements can be accessed using subscripts 
#or indices (singular is index),which are specified using brackets, e.g:
Nile[2]

#The c ("concatenate") function builds a vector, stringing several numbers together. 
#E.g. we can get the 2nd, 5th and 6th:
Nile[c(2,4,6)]

#And 80:100 means all the numbers from 80 to 100. 
#So, to answer the above question on the mean flow during 1951-1971, we can do
mean (Nile [80:100])

#we can make a copy of the period:
n80100<-Nile[80:100]
mean(n80100)
sd(n80100)

#On to data frames!
# with ToothGrowth dataset 

head(ToothGrowth)
head(ToothGrowth$len)
#copy ToothGroth to a shorter name
tg <- ToothGrowth
# what is the size of dataset
dim (tg)

mean(tg$len)
tg$len[3]

#What is the mean of the first row of tg dataset
mean(tg[,1])
# One can extract subsets of data frames, using either some of the rows or some of the columns, e.g.: 
z <- tg[2:5,c(1,3)]
# To extract those columns but keep all rows, do:
y <- tg[ ,c(1,3)]

#R Factor Class 
class(tg)
class(tg$supp)
# We can find the list of categories for tg$supp as follows:
levels(tg$supp)
#Let's compare mean tooth length for the two types of supplements:
tgoj <- tg[tg$supp == 'OJ',]
tgvc <- tg[tg$supp == 'VC',]
mean(tgoj$len)
mean(tgvc$len)

#magical tapply: "Split tg$len into two groups, according to the value of tg$supp, then apply mean to each group." 
tapply(tg$len,tg$supp,mean)
# or assign the results to vector
z <- tapply(tg$len,tg$supp,mean)

# Next: Data cleaning (Pima Diabetes dataset)
pima <- read.csv('http://heather.cs.ucdavis.edu/FasteR/data/Pima.csv',header=TRUE)
# we look at glucose variable
table(pima$glucose) 
#Let's consider a version of the glucose data that excludes these 0s:
pg<-(pima$glucose)
pg1<-pg[pg>0]
length(pg1)

# R has a special code for missing values, NA, for situations like this. Rather than removing the 0s, it's better 
# to recode them as NAs. Let's do this, back in the original dataset so we keep all the variables in one object:

pima$glucose[pima$glucose==0]<-NA 
sum(is.na(pima$glucose))
mean(pima$glucose)
mean(pima$glucose, na.rm = TRUE)

#near the end:R List Class wit mtcars dataset
mtmpg<-mtcars$mpg
mt1<-split(mtmpg, mtcars$cyl)
mt1
# access each vector 
mt1$`4`
# or
mt1[[1]]
head(mtcars$cyl)
# create separate vectors
m4 <- mt1[[1]]
m6 <- mt1[[2]]
m8 <- mt1[[3]]

#finally Another Look at the Nile Data
plot(Nile)
which(Nile<600)

