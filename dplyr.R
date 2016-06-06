# http://genomicsclass.github.io/book/pages/dplyr_tutorial.html

library(dplyr)
library(downloader)

# - Load files
setwd("H:/My Documents/R/stockSelector")
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- "msleep_ggplot2.csv"
if (!file.exists(filename)) download(url,filename)
msleep <- read.csv("msleep_ggplot2.csv")
head(msleep)

# - Deplyr functions
#select()
#http://www.r-bloggers.com/the-complete-catalog-of-argument-variations-of-select-in-dplyr/
select(msleep, name, sleep_total)
head(select(msleep,-order),n=10)
head(select(msleep, name:order))
head(select(msleep, starts_with("sl")))
head(select(msleep, ends_with("re")))
head(select(msleep, contains("nus")))
head(select(msleep, matches("nus|der")))
select(msleep, matches("^(dep|arr)_"))
oneof <- c("name", "genus", "order")
head(select(msleep, one_of(oneof)))

#filter()
filter(msleep, sleep_total >= 16)
filter(msleep, sleep_total >= 16, bodywt >= 1) #equivalent to and
filter(msleep, order %in% c("Perissodactyla", "Primates"))
filter(msleep, sleep_total >= 16 & bodywt >= 1)
filter(msleep, sleep_total >= 16 | bodywt >= 1)

#Pipe operator: %>%
msleep %>% select(name, sleep_total) %>% head %>% filter(sleep_total < 14)

#arrange()
msleep %>% arrange(order) %>% head
msleep %>% select(name, order, sleep_total) %>% arrange(order, sleep_total)
msleep %>% select(name, order, sleep_total) %>% arrange(order, desc(sleep_total)) %>% filter(sleep_total >= 16)

#mutate()
msleep %>% mutate(rem_proportion = sleep_rem / sleep_total) %>% head
msleep %>% mutate(rem_proportion = sleep_rem / sleep_total, bodywt_grams = bodywt * 1000) %>% head

#summarise()
msleep %>% summarise(avg_sleep = mean(sleep_total))
msleep %>% summarise(avg_sleep = mean(sleep_total), min_sleep = min(sleep_total),max_sleep = max(sleep_total),total = n(),distinct=n_distinct(sleep_total))

#group_by()
msleep %>% 
  group_by(order) %>%
  summarise(avg_sleep = mean(sleep_total), 
            min_sleep = min(sleep_total), 
            max_sleep = max(sleep_total),
            total = n())
#rename()
msleep %>% rename(new_order = order)