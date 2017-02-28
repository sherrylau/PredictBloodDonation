library(ggplot2)
library(dplyr)
library(gsubfn)

#### Functions #### 
individual_explore = function(xVar, data){
  print(paste("The variable", xVar,"is of type", typeof(data[xVar])))
  print(summary(data[xVar]))
  p = ggplot(aes_string(xVar), data=data) + geom_bar()
  print(p)
}

# Need to be improved for other data type
univariate_explore = function(xVar, yVar, data){
  if (class(data[[yVar]]) == 'factor'){
    if (class(data[[xVar]]) == "integer" | class(data[[xVar]]) == "numeric"){
      summary_table = data %>% group_by_(yVar) %>% summarise_(.dots = fn$list(freq = "length($xVar)", 
                                                                              min = "min($xVar)", 
                                                                              mean = "mean($xVar)", 
                                                                              max = "max($xVar)",
                                                                              sd = "sd($xVar)"))
      print(data.frame(summary_table))
      p = ggplot(aes_string(x=yVar, y=xVar, fill=yVar), data=data) + geom_boxplot() + scale_fill_brewer(guide = FALSE, palette = 'BrBG')
      print(p)
    } else{
      summary_table = data %>% group_by_(yVar) %>% summarise_(.dots = fn$list(freq = "length($xVar"))
      print(data.frame(summary_table))
    }
  }
}

#### Read Data ####

train_data = read.csv("./data/train_data.csv")
train_data = train_data[-which(names(train_data) %in% "X")]
test_data = read.csv("./data/test_data.csv")
sample_submit = read.csv("./data/BloodDonationSubmissionFormat.csv")

train_data$Made.Donation.in.March.2007 = as.factor(train_data$Made.Donation.in.March.2007)
summary(train_data)

#### Individual Variables Exploration ####

ggplot(train_data, aes(x=Made.Donation.in.March.2007)) + 
  geom_bar(aes(fill=Made.Donation.in.March.2007)) + 
  scale_fill_brewer(guide = FALSE, palette = "BrBG")

for (i in 1:ncol(train_data)){
  var = names(train_data)[i]
  individual_explore(var, train_data)
}

#### Univariate Exploration ####
xTrainData = train_data[-which(names(train_data) %in% "Made.Donation.in.March.2007")]
xVars = names(xTrainData)

for (i in 1:length(xVars)){
  xVar = xVars[i]
  yVar = 'Made.Donation.in.March.2007'
  univariate_explore(xVar, yVar, train_data)
}

