# DATA:
# https://datadryad.org/stash/downloads/file_stream/26912

# ARTICLE:
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0191290

library(readxl)
library(tidyverse)
library(labelled)

d <- read_excel('CRRTstudy_1144patients_revised.xls', skip = 34)

# set up factors ----------------------------------------------------------

crrt <- d %>% 
  mutate(Sex = factor(Sex, labels = c("Male", "Female")),
         MV_CRRT = factor(MV_CRRT, labels = c("No", "Yes")),
         Death_90D = factor(Death_90D, labels = c("No", "Yes")),
         Death_28D = factor(Death_28D, labels = c("No", "Yes")),
         P0h_3groups = factor(P0h_3groups, labels = c("hypophosphatemic", 
                                                      "normophosphatemic",
                                                      "hyperphosphatemic")),
         P24h_3groups = factor(P24h_3groups,labels = c("hypophosphatemic", 
                                                         "normophosphatemic",
                                                         "hyperphosphatemic")),
         AKIcause = factor(AKIcause, labels = c("Sepsis", "Nephrotoxic",
                                                "Ischemia", "Surgery",
                                                "Others")),
         CRRTcause = factor(CRRTcause, labels = c("Volume overload", 
                                                  "metablic acidosis",
                                                  "hyperkalemia", "uremia",
                                                  "oliguria", "others")),
         AKIN = factor(AKIN, labels = c("stage 2", "stage 3")))


# label variables ---------------------------------------------------------

# use my version of codebook
vlabs <- read_excel('CRRT_codebook_CF.xlsx')

# remove coding instructions and (0 h)
vlabs <- mutate(vlabs, Description = str_remove(Description, " \\(.*\\)"))

# add labels
var_label(crrt) <- vlabs$Description


# save file ---------------------------------------------------------------

saveRDS(crrt, file = "crrt.rds")
