# Variables that can be accessed by server and UI automatically

# - Create Global variables

#write.csv(colnames(temp),"header.csv")
headers = read.csv(file="header.csv", header=FALSE,stringsAsFactors=FALSE)
headers2 = setNames(headers,c("Name","NewName"))
headers2 = spread(headers2,Name,NewName)
#View(headers)

# Create a dataframe with one entry and renamed header. 
# You need one entry so you know what types of variable each column is.
temp = DATA[1,]
temp = select(temp,-measure_currency)
IndicatorDropDown = colnames(temp[lapply(temp,class)=="character"])
IndicatorDropDown = IndicatorDropDown[2:length(IndicatorDropDown)]
NumericalDropDown = colnames(temp[lapply(temp,class)=="numeric"])
# notice [[]] instead of []
IndicatorDropDown = lapply(IndicatorDropDown, function(x) return(headers2[[x]])) %>% unlist
NumericalDropDown = lapply(NumericalDropDown, function(x) return(headers2[[x]])) %>% unlist

# Global Functions
get_min = function(measure) {
  return(min(measure,na.rm = TRUE))
}

get_max = function(measure) {
  return(max(measure,na.rm = TRUE))
}