library(ggplot2)
library(broom)

data1991 <- read_csv("./output/interm_data/data1991.csv")
model <- readRDS("./output/model/reg.rds")

data1991$fitted_gpa <- model$fitted.values

ggplot(data1991, aes(x=avg_peer_gpa, y=fitted_gpa)) +
  geom_line(color = "blue")
