## ---- software-impact
library(cranlogs)
library(tidyverse)
library(lubridate)

my_pkgs <- c("sugrrants", "tsibble", "mists")

## ---- software-downloads
gravitas_dl <- cran_downloads(my_pkgs[1], from = "2020-01-01", to = "2021-11-01")
tsibble_dl <- cran_downloads(my_pkgs[2], from = "2018-01-09", to = "2019-09-20")
