library(tidyverse)
        
dummy <- data.frame(age = c(20,30,50,60,70,90,40,20,40,30),
                    sex = c(rep("male", 5), rep("female", 5)),
                    treatment = c(rep("control", 5), rep("treatment", 5)),
                    dose = c(seq(1:5), seq(1:5)))
  
t <- t(dummy %>%
  group_by(treatment) %>%
  summarise(mean = mean(age), sd = sd(age)))


colnames(t) <- t[1,]
t <- t[-1, ] 

test <- dummy %>%
  summarise(N = n(),
            mean = mean(age),
            `(SD)` = sd(age),
            median = median(age),
            IQR = "TODO",
            min = min(age),
            max = max(age))
t(test)

dummy %>% summarise(N = n())

data.frame(Variable = "Age", total = (dummy %>% summarise(N = n())))

dummy %>%
  group_by(age) %>%
  summarize(count = n())

dummy %>% group_by(  ) %>% summarize(mean = mean( age ), sd = sd( age ) )
