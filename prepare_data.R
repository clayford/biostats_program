# Source: https://archive.ics.uci.edu/ml/datasets/chronic_kidney_disease#

# import data
library(farff)
d <- readARFF('Chronic_Kidney_Disease/chronic_kidney_disease.arff',
              na = c("?","","NA"))

# add variable labels
library(labelled)

dlabels <- c("Age",
            "Blood Pressure in mm/Hg",
            "Specific Gravity",
            "Albumin",
            "Sugar",
            "Red Blood Cells",
            "Pus Cell",
            "Pus Cell clumps",
            "Bacteria",
            "Blood Glucose Random",
            "Blood Urea in mgs/dl",
            "Serum Creatinine in mgs/dl",
            "Sodium in mEq/L",
            "Potassium in mEq/L",
            "Hemoglobin in gms",
            "Packed Cell Volume",
            "White Blood Cell Count in cells/cumm",
            "Red Blood Cell Count in millions/cmm",
            "Hypertension",
            "Diabetes Mellitus",
            "Coronary Artery Disease",
            "Appetite",
            "Pedal Edema",
            "Anemia",
            "Class")

var_label(d) <- dlabels

saveRDS(d, file = "kidney_disease.rds")
