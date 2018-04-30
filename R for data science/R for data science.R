# first install.packages(c("nycflights13", "gapminder", "Lahman"))
# and tidyverse

# install tidyverse
install.packages("tidyverse")

# load the required packages
library(nycflights13)
library(tidyverse)

#explore nycflights 
nycflights13::flights
View(flights)

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

