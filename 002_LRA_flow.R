library(tidyverse)
library(plater)
library(stringr)

# read in flow data ------------------------------------------------------------
# use readr::read_tsv because here is a weird NA symbol later in the table and 
# this way it gets turned into an NA instead of making all the columns factors
d <- read_tsv("../002 LRA/data/002 LRA gating-Table", na = "¥")

# better column names
colnames(d)[2:3] <- c("Well", "Plate")

# sample column is useless
d <- select(d, -Sample)

# read in plate layout data ----------------------------------------------------
dir <- "../002 LRA/plate_layout_"
mapping_files <- paste0(dir, c("combo_and_cntls.csv", "single_FD.csv", 
  "single_np.csv"))

# plate_mappings_combo has some character concentrations where the others
# have only numeric concentrations, which doesn't work with read_plates / 
# bind_rows, so have to do separately and use rbind
plate_mappings <- read_plates(mapping_files[2:3], plate_names = 
    c("free_drug_single", "nanoparticle_single"), well_ids_column = "Well")

plate_mappings_combo <- read_plate(mapping_files[1], 
    well_ids_column = "Well") %>% 
  mutate(Plate = "fd_combo_np_combo_cntls_np_only")

plate_mappings <- rbind(plate_mappings, plate_mappings_combo)

# data is ready ----------------------------------------------------------------
d <- inner_join(d, plate_mappings)


to_plot <- d %>% 
  # for combos just use concentration of one drug
  mutate(nM = as.numeric(str_replace(Concentration_nM, " .*", ""))) %>% 
  # omit nanoparticle alone
  filter(Donor != "Nanoparticles alone")

helper <- function(y) {
  ggplot(to_plot, aes_string(x = "log10(nM)", y = y, color = "Donor")) + 
    geom_point() + facet_wrap(~ Drug)
}

helper("LivePercent")
helper("log10(CD4Count)") + geom_hline(yintercept = log10(500))
helper("log10(CD8Count)") + geom_hline(yintercept = log10(500))
helper("CD4CD28")
helper("CD4CD69")
helper("CD4CXCR4")
helper("CD4HLADR")
helper("CD8CD28")
helper("CD8CD69")
helper("CD8CXCR4")
helper("CD8HLADR")
