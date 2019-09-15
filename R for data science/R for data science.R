# the books is avalable online https://r4ds.had.co.nz/ 

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
