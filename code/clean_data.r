library(tidyverse)

# import data
data1 <- haven::read_sav("./data/w3educatn_dvn.sav")
data2 <- haven::read_sav("./data/w3pvt_dvn.sav")
data3 <- haven::read_sav("./data/w3graduatn_dvn_3.sav")
data4 <- haven::read_sav("./data/w1context_dvn.sav")

# merge data
data <- data1 |>
    inner_join(data2, by = "AID") |>
    inner_join(data3, by = join_by(AID == aid)) |>
    inner_join(data4, by = "AID")

# select columns
data <- data |>
    select(AID, ELYEAR1, EAOGPA1, PVTSTD3C, erexitst, BST90P12, BST90P16, BST90P21) |>
    rename(EREXITST = erexitst)

# clean columns
data <- data |>
    filter(ELYEAR1 >= 1991 & ELYEAR1 <= 1996) |>
    filter(EAOGPA1 != 9995) |>
    filter(PVTSTD3C != 996) |>
    filter(EREXITST != "92") |>
    filter(BST90P12 != 9998 & BST90P12 != 9999) |>
    filter(BST90P16 != 9998 & BST90P16 != 9999) |>
    filter(BST90P21 != 9998 & BST90P21 != 9999)

data$EREXITST <- replace(data$EREXITST, data$EREXITST %in% c("A", "B", "C", "E", "I", "J"), "1")
data$EREXITST <- replace(data$EREXITST, data$EREXITST %in% c("F", "G"), "0")
data$EREXITST <- as.numeric(as.character(data$EREXITST))

# export data
write_csv(data, "./output/interm_data/data.csv")
