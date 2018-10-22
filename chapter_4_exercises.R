library(AppliedPredictiveModeling)
library(caret)
library(tidyverse)

# Exercise 4.2
data("permeability")
fingerprints = as.tibble(fingerprints)

set.seed(72)
repeatedCV <- createMultiFolds(permeability, k = 10, times = 25)
str(repeatedCV)


# Exercise 4.3
data("ChemicalManufacturingProcess")

# (a)
0.545-0.0308 # => > 0.5142 and simpler than 4 components => 3 components
 
# (b)
0.545*0.9 # => > 0.4905 and simpler than 4 components => 2 components


# Exercise 4.4
data(oil)

# (a)
prop.table(table(oilType))*100
prop.table(table(sample(oilType, 60)))*100

# (b)
prop.table(table(oilType))*100
oilType %>% 
  as.tibble() %>% 
  slice(createDataPartition(oilType, p = 0.625, list = F)) %>% 
  table() %>% 
  prop.table()*100
# Always the same distribution

# (c)


















