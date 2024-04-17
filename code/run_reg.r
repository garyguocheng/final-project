library(data.table)
library(estimatr)

# import data
data <- read_csv("./output/interm_data/data.csv")

# define model function
model_func <- function(data = data, year) {
    # select year
    data_yr <- data[data$year == year, ]
    # create avg_peer_gpa
    data_yr$avg_peer_gpa <- (sum(data_yr$gpa) - data_yr$gpa) / (nrow(data_yr) - 1)
    # run regression
    model <- lm_robust(gpa ~ pvt + grad + fam_mig + fam_inc + fam_edu + avg_peer_gpa, data = data_yr)
    # save data
    data_name <- paste0("data", year, ".csv")
    fwrite(data_yr, paste0("./output/interm_data/", data_name))
    # save model
    model_name <- paste0("reg", year, ".rds")
    saveRDS(model, paste0("./output/model/", model_name))
}

# run model function
for (i in unique(data$year)) {
    model_func(data, i)
}
