# http://www.jvcasillas.com/tidyr_tutorial/
library(tidyr)
library(dplyr)

set.seed(1)
tidyr.ex <- data.frame(
  participant = c("p1", "p2", "p3", "p4", "p5", "p6"), 
  info = c("g1m", "g1m", "g1f", "g2m", "g2m", "g2m"),
  day1score = rnorm(n = 6, mean = 80, sd = 15), 
  day2score = rnorm(n = 6, mean = 88, sd = 8)
)
print(tidyr.ex)

#gather(), reverse of spread()
new_data = tidyr.ex %>% gather(day, score, c(day1score, day2score))
print(new_data)

#spread()
new_data %>% spread(day,score)

#separate(), takes values inside a column and separates them
# separate(df, col, into, sep)
new_data = tidyr.ex %>% separate(col = info, into = c("group", "gender"), sep = 2)

#unite(), opposite of separate
#unite(data, col, ..., sep = "_", remove = TRUE)
new_data %>% unite(info,group,gender)
new_data %>% unite(info,group,gender,sep="",remove=FALSE)

#Other functions: extract() and unite() which use regex