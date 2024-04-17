library(tidyverse)
library(data.table)
library(ggplot2)
library(patchwork)

# import data
data <- read_csv("./output/interm_data/data.csv")
data1991 <- read_csv("./output/interm_data/data1991.csv")
data1992 <- read_csv("./output/interm_data/data1992.csv")
data1993 <- read_csv("./output/interm_data/data1993.csv")
data1994 <- read_csv("./output/interm_data/data1994.csv")
data1995 <- read_csv("./output/interm_data/data1995.csv")
data1996 <- read_csv("./output/interm_data/data1996.csv")

# import model
model1991 <- readRDS("./output/model/reg1991.rds")
model1992 <- readRDS("./output/model/reg1992.rds")
model1993 <- readRDS("./output/model/reg1993.rds")
model1994 <- readRDS("./output/model/reg1994.rds")
model1995 <- readRDS("./output/model/reg1995.rds")
model1996 <- readRDS("./output/model/reg1996.rds")

# define fitted value function
fitted_func <- function(data, model) {
  # get coefficients
  alpha <- model$coefficients["(Intercept)"]
  beta1 <- model$coefficients["pvt"]
  beta2 <- model$coefficients["grad"]
  beta3 <- model$coefficients["fam_mig"]
  beta4 <- model$coefficients["fam_inc"]
  beta5 <- model$coefficients["fam_edu"]
  phi <- model$coefficients["avg_peer_gpa"]
  # calculate fitted gpa
  fitted_gpa <- alpha + beta1 * data$pvt + beta2 * data$grad + beta3 * data$fam_mig + beta4 * data$fam_inc + beta5 * data$fam_edu + phi * data$avg_peer_gpa
  # add fitted gpa to data
  data$fitted_gpa <- fitted_gpa
  return (data)
}

# run fitted value function and clean data
data1991 <- fitted_func(data1991, model1991) |> select(year,avg_peer_gpa, fitted_gpa)
data1992 <- fitted_func(data1992, model1992) |> select(year,avg_peer_gpa, fitted_gpa)
data1993 <- fitted_func(data1993, model1993) |> select(year,avg_peer_gpa, fitted_gpa)
data1994 <- fitted_func(data1994, model1994) |> select(year,avg_peer_gpa, fitted_gpa)
data1995 <- fitted_func(data1995, model1995) |> select(year,avg_peer_gpa, fitted_gpa)
data1996 <- fitted_func(data1996, model1996) |> select(year,avg_peer_gpa, fitted_gpa)

# define figure function
fig_func <- function(data) {
  # generate figure
  fig <- ggplot(data, aes(x=avg_peer_gpa, y=fitted_gpa)) +
    geom_line() +
    ggtitle(paste("Year", data$year))
  # save figure
  plot_name <- paste0("fig", data$year, ".pdf")
  path <- paste0("./output/figure/", plot_name)
  ggsave(path, fig, width = 4, height = 6, units = "in")
  return (fig)
}

# make figure
fig1991 <- fig_func(data1991)
fig1992 <- fig_func(data1992)
fig1993 <- fig_func(data1993)
fig1994 <- fig_func(data1994)
fig1995 <- fig_func(data1995)
fig1996 <- fig_func(data1996)

summary_fig <- (fig1991 | fig1992 | fig1993) / (fig1994 | fig1995 | fig1996)
ggsave("./output/figure/summary_fig.png", summary_fig)
