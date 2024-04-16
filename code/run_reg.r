library(tidyverse)
library(estimatr)

# import data
data <- read_csv("./output/interm_data/data.csv")

# select year == 1991
data1991 <- data |>
    filter(year == 1991)

# create avg_peer_gpa
data1991 <- data1991 |>
    mutate(avg_peer_gpa = (sum(gpa) - gpa) / (nrow(data1991) - 1))

# run regression
model <- lm_robust(gpa ~ pvt + grad + fam_mig + fam_inc + fam_edu + avg_peer_gpa, data = data1991)

# save model
saveRDS(model, "./output/model/reg.rds")
