# the books is avalable online https://r4ds.had.co.nz/ 

# the answers to exercises here 
#https://github.com/maxconway/r4ds_solutions/blob/master/transform_solutions.Rmd

# first install.packages(c("nycflights13", "gapminder", "Lahman"))
# and tidyverse

# install tidyverse
install.packages("tidyverse")

# load the required packages
library(nycflights13)
library(tidyverse)

#explore nycflights 
nycflights13::flights
# can view View(flights)

# ggplot2 + dataframe mpg
library (ggplot2)
ggplot2::mpg

# two ways to code ggplot 
ggplot(data = mpg,mapping = aes(x = displ, y = hwy)) + 
  geom_point ()

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))

# a graphing template
#ggplot(data = <DATA>) +
 # <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

#exploring mpg with gg
ggplot(mpg, aes(x=cyl,y=hwy))+
  geom_point()

ggplot(mpg, aes(x=class,y=drv))+
  geom_point()

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, colour = class))

# Top 
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# Bottom

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# blue colour of dots
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), colour = "blue")

#facets
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(drv ~ cyl)

# What do the empty cells in a plot with facet_grid(drv ~ cyl) mean? How do they relate to this plot?

ggplot(data = mpg) + geom_point(mapping = aes(x = drv, y = cyl))+ facet_grid(drv ~ cyl)

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(drv ~ .)

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(. ~ cyl)

?facet_wrap

# left 
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))

# right
ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy))

# Here geom_smooth() separates the cars into three lines based on their drv value, which describes a car’s drivetrain.
ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))
       
# we can make it more clear by overlaying the lines on top of the raw data and then coloring everything according to drv.
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, linetype = drv, color=drv))+
geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, color=drv))

ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))
ggplot(data = mpg) + geom_smooth( mapping = aes(x = displ, y = hwy, color = drv), show.legend = FALSE)

# To display multiple geoms in the same plot, add multiple geom functions to ggplot():
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + geom_smooth(mapping = aes(x = displ, y = hwy))

# more efficient code for the same plot
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point() + geom_smooth()

# to display different aesthetics in different layers:
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point(mapping = aes(color = class)) + geom_smooth()

# Here, our smooth line displays just a subset of the mpg dataset, the subcompact cars
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point(mapping = aes(color = class)) + 
  geom_smooth( 
    data = filter(mpg, class == "subcompact"), se = FALSE
    )

#Will these two graphs look different? Why/why ( the same)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point() + geom_smooth()

ggplot() + geom_point( data = mpg, mapping = aes(x = displ, y = hwy)
) +
  geom_smooth( data = mpg, mapping = aes(x = displ, y = hwy)
  )

# Re-create the R code necessary to generate the following graphs.
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point() + geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, linetype = drv))+
  geom_smooth(se=FALSE, size=2)+
  geom_point(size=4)

ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy, linetype = drv, color=drv, size=2))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv))+
  geom_point(size=3)+
  geom_smooth(se=FALSE)

ggplot() + geom_point( data = mpg, mapping = aes(x = displ, y = hwy, color=drv, size=2)
) +
  geom_smooth( data = mpg, mapping = aes(x = displ, y = hwy, size=1)
               , se=FALSE)

ggplot() + geom_point( data = mpg, mapping = aes(x = displ, y = hwy, color=drv, size=1)
) +
  geom_smooth( data = mpg, mapping = aes(x = displ, y = hwy,linetype=drv, size=1)
               , se=FALSE)

# statstical transformations
# exploring diamonds dataset with bar charts
library (ggplot2)
ggplot2::diamonds
ggplot(data = diamonds) + geom_col(mapping = aes(x = cut))
# using stat_count instead of geom_bar
ggplot(data = diamonds) + stat_count(mapping = aes(x = cut))

#you might want to display a bar chart of proportion, rather than count:
ggplot(data = diamonds) + geom_bar( mapping = aes(x = cut, y = ..prop.., group = 1))

#you might use stat_sum mary(), which summarizes the y values for each unique x value
ggplot(data = diamonds) + stat_summary( mapping = aes(x = cut, y = depth), 
                                        fun.ymin = min, fun.ymax = max, fun.y = median) 

#the other way to do the same as above but a boxplot
ggplot(data = diamonds) + geom_boxplot(mapping = aes(x=cut, y=depth))

# You can color a bar chart using either the color aesthetic, or more usefully, fill:
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, color = cut))
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = cut))

#Note what happens if you map the fill aesthetic to another variable, like clarity: the
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity))

#If you don’t want a stacked bar chart, you can use one of three other options: 
#"identity", "dodge" or "fill":

# position = "identity"
ggplot( data = diamonds, mapping = aes(x = cut, fill = clarity))
+ geom_bar(alpha = 1/5, position = "identity")
ggplot( data = diamonds, mapping = aes(x = cut, color = clarity)) 
+ geom_bar(fill = NA, position = "identity")

# position = "fill"
ggplot(data = diamonds) + geom_bar( mapping = aes(x = cut, fill = clarity), 
                                    position = "fill")
# position = "dodge" places
ggplot(data = diamonds) + geom_bar( mapping = aes(x = cut, fill = clarity), 
                                    position = "dodge")
# position = "jitter" 
ggplot(data = mpg) + geom_point( mapping = aes(x = displ, y = hwy), position = "jitter", size=1)
# ggplot2 comes with a shorthand for geom_point(posi tion = "jitter"): geom_jitter()

#excercise 1
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + geom_point(position = "jitter")

# excercise 4
ggplot(data = mpg) + geom_boxplot(mapping = aes(x = drv, y = hwy), position = "dodge")

# Boxplots on mpg dataset
ggplot2::mpg
ggplot(mpg, aes(class, hwy)) + 
  geom_boxplot()

ggplot(mpg, aes(class, hwy))+ 
  geom_boxplot() + geom_jitter(width = 0.2)

ggplot(mpg, aes(class, hwy))+ 
  geom_boxplot() + coord_flip()

ggplot(mpg, aes(class, hwy))+ 
  geom_boxplot(notch = TRUE)

ggplot(mpg, aes(class, hwy))+ 
  geom_boxplot(varwidth = TRUE)

ggplot(mpg, aes(class, hwy))+ 
geom_boxplot(aes(colour = drv))

# coordinate system
nz <- map_data("nz")
ggplot(nz, aes(long, lat, group = group)) + geom_polygon(fill = "white", color = "black")
ggplot(nz, aes(long, lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_quickmap()

#coord_polar() uses polar coordinates. Polar coordinates reveal an interesting connection between a bar chart and a Coxcomb chart:

bar <- ggplot(data = diamonds) + 
  geom_bar( mapping = aes(x = cut, fill = cut), show.legend = FALSE, width = 1
) +
  theme(aspect.ratio = 1) + labs(x = NULL, y = NULL)
bar + coord_flip() 
bar + coord_polar()

# Data Transformation with dplyr
# prerequisites 
library(nycflights13)
library(tidyverse) 

nycflights13::flights
View (flights)

# 5.2 Filter rows with filter()

filter(flights, month==1, day==1)
jan1<-filter(flights, month==1, day==1) 
# to print and save them to the variable 
(dec25 <- filter(flights, month==12, day==25)) 
# filter nov and Dec flights
filter(flights, month==11 | month==12)
# or using (x %in% y) expression
nov_dec<-filter(flights, month %in% c(11,12))

filter(flights, !(arr_delay > 120 | dep_delay > 120)) 
filter(flights, arr_delay <= 120, dep_delay <= 120)

# 5.2.3 Missing values 
# if you want to determine if a value is missing, use is.na()

is.na(jan1)

#filter() only includes rows where the condition is TRUE; it excludes both FALSE and NA values. 

#5.2.4 Exercises
#Find all flights that
# Had an arrival delay of two or more hours
filter(flights, (arr_delay >= 120 | dep_delay >= 120)) 
# Flew to Houston (IAH or HOU)
filter(flights, dest=="IAH")
#Were operated by United, American, or Delta
filter(flights, carrier %in% c("UA", "AA", "DL"))
#Departed in summer (July, August, and September)
filter(flights, month %in% c(7,8,9))
# Arrived more than two hours late, but didn’t leave late 
filter(flights, (arr_delay > 120 | dep_delay <= 0)) 
#Were delayed by at least an hour, but made up over 30 minutes in flight
filter(flights, (dep_delay >= 60 | arr_delay >= 30)) 

#Arrange Rows with arrange() https://r4ds.had.co.nz/transform.html#arrange-rows-with-arrange
arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))
#Missing values are always sorted at the end:
df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))
# Exercises
arrange(flights, desc(dep_delay))
arrange(flights, desc(dep_time-sched_dep_time))
arrange(flights, desc(sched_arr_time-arr_time))
arrange(flights, desc(air_time))
arrange(flights, air_time)
arrange(flights, !is.na(arr_time))
#5.4 Select columns with select
# Select columns by name
select(flights, year, month, day)
# Select all columns between year and day (inclusive)
select(flights, year:day)
# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))

#use rename(), which is a variant of select() to rename the variables
rename(flights, tail_num=tailnum)

#use select() in conjunction with the everything() helper 
# here select - time_hour, air_time, is moved to the start of the dataframe everthing else remains the same
select(flights, time_hour, air_time, everything())
#5.4.1 Exercises https://r4ds.had.co.nz/transform.html#exercises-9  
select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, c(dep_time,  dep_delay, arr_time, arr_delay))
flights %>% select(dep_time,  dep_delay, arr_time, arr_delay)
select(flights, dep_time, dep_time)
# 3.What does the `one_of()` function do? Why might it be helpful in conjunction with this vector?
#It returns all the variables you ask for, for example ones stored in a vector.
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
flights %>% select(one_of(vars))
select(flights, one_of(vars))
select(flights, contains("TIME"))

#5.5 Add new variables with mutate()
# it’s often useful to add new columns that are functions of existing columns. That’s the job of mutate()
# mutate() always adds new columns at the end of your dataset 


mutate(flights_sml,
       gain = dep_delay-arr_delay,
       speed = distance/air_time*60
)

mutate(flights_sml,
       gain = dep_delay-arr_delay,
       hours = air_time/60,
       gain_per_hour = gain/hours
)
# if you want to keep the new variables use transmutate
transmute(flights,
       gain = dep_delay - arr_delay,
       hours = air_time/60,
       gain_per_hour = gain/hours
)
#5.5.1 Useful creation functions 
# Modular arithmetic: %/% (integer division) and %% (remainder), where x == y * (x %/% y) + (x %% y)
transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
)
# Logs: log(), log2(), log10() - Logarithms
# Offsets: lead() and lag() allow you to refer to leading or lagging values (offset them)
(x <- 1:10)
lead(x)
lag(x)

# Cumulative and rolling aggregates: R provides functions for running sums, products, mins and maxes: 
#cumsum(), cumprod(), cummin(), cummax(); and dplyr provides cummean() for cumulative means.
cumsum(x)
cummean(x)
# Logical comparisons, <, <=, >, >=, !=, and ==
# Ranking - start with min_rank()
y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
min_rank(desc(y))
# the other ranking variants row_number(), dense_rank()
row_number(y)
dense_rank(y)
View(flights)

#5.5.2 Exercises
#1
mutate(flights,
       dep_time = (dep_time %/% 100) * 60 + (dep_time %% 100),
       sched_dep_time=(sched_dep_time%/%100)*60+(sched_dep_time%%100))
#2
flights %>% 
  mutate(dep_time = (dep_time %/% 100) * 60 + (dep_time %% 100),
         sched_dep_time = (sched_dep_time %/% 100) * 60 + (sched_dep_time %% 100),
         arr_time = (arr_time %/% 100) * 60 + (arr_time %% 100),
         sched_arr_time = (sched_arr_time %/% 100) * 60 + (sched_arr_time %% 100)) %>%
  transmute((arr_time - dep_time) %% (60*24) - air_time)

#3
flights %>% 
  mutate(dep_time = (dep_time %/% 100) * 60 + (dep_time %% 100),
         sched_dep_time = (sched_dep_time %/% 100) * 60 + (sched_dep_time %% 100),
         arr_time = (arr_time %/% 100) * 60 + (arr_time %% 100),
         sched_arr_time = (sched_arr_time %/% 100) * 60 + (sched_arr_time %% 100)) %>%
  transmute(near((sched_dep_time + dep_delay) %% (60*24), dep_time, tol=1))
# 4 
filter(flights, min_rank(desc(dep_delay))<=10)
flights %>% top_n(n = 10, wt = dep_delay)
# 5
1:3+1:10
#6
?Trig

# 5.6 Grouped summaries with summarise()
#summarise a dataframe to a single row
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

# summarise() is not terribly useful unless we pair it with group_by()
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

# 5.6.1 Combining multiple operations with the pipe
by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")

ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

# There’s another way to tackle the same problem with the pipe, %>%: 

delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")
#5.6.2 Missing values
flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay, na.rm = TRUE))
# remove values with cancelted flights
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))
# then summirize 
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

# 5.6.3 Counts
#Whenever you do any aggregation, it’s always a good idea to include either a count (n()), 
# or a count of non-missing values (sum(!is.na(x))). 

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )

ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)
# scatterplot of number of flights vs average delay
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

# filtering out the groups with smalest number of observations

delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

# Convert to a tibble so it prints nicely
batting <- as_tibble(Lahman::Batting)

batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() + 
  geom_smooth(se = FALSE)
#> `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

# 5.6.4 Useful summary functions
# It’s sometimes useful to combine aggregation with logical subsetting
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
)
#Measures of spread: sd(x), IQR(x), mad(x)
# Why is distance to some destinations more variable than to others?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))
# Measures of rank: min(x), quantile(x, 0.25), max(x)
# When do the first and last flights leave each day?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )
#Measures of position: first(x), nth(x, 2), last(x) 
#For example, we can find the first and last departure for each day: 
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = first(dep_time), 
    last_dep = last(dep_time)
  )
#These functions are complementary to filtering on ranks
not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))
#Counts: You’ve seen n(), which takes no arguments, and returns the size of the current group
#To count the number of non-missing values, use sum(!is.na(x)). 
#To count the number of distinct (unique) values, use n_distinct(x)

# Which destinations have the most carriers?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

#Counts are so useful that dplyr provides a simple helper if all you want is a count:
not_cancelled %>% 
  count(dest)
# You can optionally provide a weight variable
not_cancelled %>% 
  count(tailnum, wt = distance)

# How many flights left before 5am? (these usually indicate delayed
# flights from the previous day)
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))

# What proportion of flights are delayed by more than an hour?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_perc = mean(arr_delay > 60))

#5.6.5 Grouping by multiple variables
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))

#5.6.6 Ungrouping
daily %>% 
  ungroup() %>%             # no longer grouped by date
  summarise(flights = n())  # all flights

#5.6.7 Exercises
# 4
flights %>%
  mutate(dep_date = lubridate::make_datetime(year, month, day)) %>%
  group_by(dep_date) %>%
  summarise(cancelled = sum(is.na(dep_delay)), 
            n = n(),
            mean_dep_delay = mean(dep_delay,na.rm=TRUE),
            mean_arr_delay = mean(arr_delay,na.rm=TRUE)) %>%
  ggplot(aes(x= cancelled/n)) + 
  geom_point(aes(y=mean_dep_delay), colour='blue', alpha=0.5) + 
  geom_point(aes(y=mean_arr_delay), colour='red', alpha=0.5) + 
  ylab('mean delay (minutes)')

#5.7 Grouped mutates (and filters)
#Grouping is most useful in conjunction with summarise(), 
#but you can also do convenient operations with mutate() and filter():

#Find the worst members of each group:
flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

# Find all groups bigger than a threshold:
popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)
popular_dests

#Standardise to compute per group metrics:
popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)


#7 Exploratory Data Analysis
#https://r4ds.had.co.nz/exploratory-data-analysis.html 
library(tidyverse)
library (ggplot2)

#7.3.1 Visualising distributions
ggplot2::diamonds
ggplot(data=diamonds)+
geom_bar(mapping = aes(x=cut))

diamonds %>%
  count(cut)
# the continuous variable distributiion can be examined by histogram:
ggplot(data = diamonds)+
  geom_histogram(mapping = aes(x=carat), binwidth = 0.5)
# with dplyr::count()
diamonds %>%
  count(cut_width(carat, 0.5))
#diamonds of less than three carats
smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

#to overlay multiple histograms in the same plot use frequepoly
ggplot(data=smaller, mapping = aes(x=carat, color=cut))+
  geom_freqpoly(binwidth=0.1)
#Typical values+
ggplot(data=smaller, mapping=aes(x=carat))+
  geom_histogram(binwidth = 0.01)
#unusual values
ggplot(data = diamonds)+
  geom_histogram(mapping = aes(x=y), binwidth = 0.5)
#and zoom into data
ggplot(diamonds)+
  geom_histogram(mapping = aes(x=y), binwidth = 0.5)+
  coord_cartesian(ylim=c(0,50))
#filtering unusual values with dplyr
unusual <- diamonds%>%
  filter(y<3|y>20)%>%
  arrange(y)
unusual
# 7.3.4 Exercises
#1
ggplot(data = diamonds)+
  geom_histogram(mapping = aes(x=x), binwidth = 0.5)+
  facet_grid(.~cut)
ggplot(data = diamonds)+
  geom_histogram(mapping = aes(x=y), binwidth = 0.5, color="blue")+
  facet_grid(. ~ cut)
ggplot(data = diamonds)+
  geom_histogram(mapping = aes(x=z), binwidth = 0.5, color="red")+
  facet_grid(. ~ cut)
#2
ggplot(data = diamonds)+
  geom_line(mapping = aes(x=price), binwidth = 0.5, color="yellow")

# 7.4 Missing values
# replacing the unusual values with missing values
diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()
# Other times you want to understand what makes observations with missing values different 
# to observations with recorded values.

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

# 7.5 Covariation
# 7.5.1 A categorical and continuous variable
ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)
# It’s hard to see the difference in distribution because the overall counts differ so much:
ggplot(diamonds)+
  geom_bar(mapping = aes(x=cut))

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

# Boxplots
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()

ggplot(data = mpg) +
  geom_violin(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))+
  facet_wrap(~ class, nrow = 2)
ggplot(data = mpg) +
  geom_jitter(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))+
  facet_wrap(~ class, nrow = 2)

# 7.5.2 Two categorical variables

ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

# Another approach is to compute the count with dplyr:
diamonds %>% 
  count(color, cut)
diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

# 7.5.3 Two continuous variables

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))
# to overcome overplottling

ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 100)

ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

# use hexbin package 
install.packages("hexbin")

ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))

# or use boxplots with binned data
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

# scatterplot
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y)) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

#7.6 Patterns and models

 # A scatterplot of Old Faithful eruption lengths versus the wait time between eruptions shows a pattern:

ggplot(data=faithful)+
  geom_point(mapping=aes(x=eruptions, y=waiting))

mod<-lm(data=faithful, waiting~eruptions)
plot(faithful$eruptions, faithful$waiting, col='yellow')
abline(mod, col="red", lty=2)

#Models are a tool for extracting patterns out of data. 

library(modelr)

mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid))

ggplot(data = diamonds2) + 
  geom_boxplot(mapping = aes(x = cut, y = resid))

#7.7 ggplot2 calls

ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_freqpoly(binwidth = 0.25)

# more concise
ggplot(faithful, aes(eruptions)) + 
  geom_freqpoly(binwidth = 0.25)

diamonds %>% 
  count(cut, clarity) %>% 
  ggplot(aes(clarity, cut, fill = n)) + 
  geom_tile()

# more about ggplot2 book "ggplot2: Elegant Graphics for Data Analysis"

