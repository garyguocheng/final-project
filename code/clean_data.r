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
    rename(aid = AID) |>
    rename(year = ELYEAR1) |>
    rename(gpa = EAOGPA1) |>
    rename(pvt = PVTSTD3C) |>
    rename(grad = erexitst) |>
    rename(fam_mig = BST90P12) |>
    rename(fam_inc = BST90P16) |>
    rename(fam_edu = BST90P21)

# clean columns
data <- data |>
    filter(year >= 1991 & year <= 1996) |>
    filter(gpa != 9995) |>
    filter(pvt != 996) |>
    filter(grad != "92") |>
    filter(fam_mig != 9998 & fam_mig != 9999) |>
    filter(fam_inc != 9998 & fam_inc != 9999) |>
    filter(fam_edu != 9998 & fam_edu != 9999)

data$grad <- replace(data$grad, data$grad %in% c("A", "B", "C", "E", "I", "J"), "1")
data$grad <- replace(data$grad, data$grad %in% c("F", "G"), "0")
data$grad <- as.numeric(as.character(data$grad))

# export data
write_csv(data, "./output/interm_data/data.csv")