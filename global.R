# BSD_2_clause

library(dplyr)
library(googleVis)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyr)

load("data/FWS_s7_03May2016.rda")
load("data/spp_lookup.rda")

# make the spp list
all_spp <- unique(unlist(full$spp_ev_ls, recursive = TRUE))
all_spp <- c("All", all_spp)

# # make spp lookup df
# spp_lookup <- data_frame(
#                 idx = seq(1, length(full$activity_code)),
#                 act = full$activity_code,
#                 spp = full$spp_ev_ls) %>%
#   unnest(spp)

# save(spp_lookup, file = "data/spp_lookup.rda")
