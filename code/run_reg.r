library(tidyverse)
library(estimatr)
library(broom)

# import data
data <- read_csv("./output/interm_data/data.csv")

# select ELYEAR1 == 1991
data1991 <- data |>
    filter(ELYEAR1 == 1991)

# create avg_peer_gpa
data1991 <- data1991 |>
    mutate(avg_peer_gpa = (sum(EAOGPA1) - EAOGPA1) / (nrow(data1991) - 1))

# run regression
model <- lm_robust(EAOGPA1 ~ PVTSTD3C + EREXITST + BST90P12 + BST90P16 + BST90P21 + avg_peer_gpa, data = data1991)

# save model
saveRDS(model, "./output/model/reg.rds")
