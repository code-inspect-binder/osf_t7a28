## load packages
library(foreign)
library(lubridate)
library(data.table)

## load full dataset
data <- read.spss(file = "../data/MEHQ_Welle123.sav", use.value.labels = FALSE, to.data.frame = TRUE)

## recode missing values
data[data == 999] <- NA
data[, "O2new_3"][data$O2new_3 == -99] <- NA

## prepare target variables

# discretize sick days at t1 for both waves
data$O2cat_2 <- cut(data$O2new, breaks = c(-Inf, 0, 5, 10, 15, 20, Inf), labels = FALSE)

# compute combined discretized sick days variable at t1
data$O2cat_12 <- rowSums(data[, c("O2", "O2cat_2")], na.rm = TRUE)

# discretize sick days at t2 to make it comparable to t1
data$O2cat_3 <- cut(data$O2new_3, breaks = c(-Inf, 0, 5, 10, 15, 20, Inf), labels = FALSE)

# compute sumscores of strain items (FBF) for both timepoints
data$SBF_12 <- rowSums(data[, c("OM11", "OM15", "OM8", "OM12", "OM5", "OM16", 
                                "OM10", "OM2", "OM18", "OM14", "OM3", "OM7", "OM9")])
data$SBF_3 <- rowSums(data[, c("OM11_3", "OM15_3", "OM8_3", "OM12_3", "OM5_3", "OM16_3", 
                               "OM10_3", "OM2_3", "OM18_3", "OM14_3", "OM3_3", "OM7_3", "OM9_3")])

## prepare covariates

# gender
data$SP1_a <- factor(data$SP1_a, levels = 1:2, labels = c("female", "male"))
# age
data$SP2_a <- factor(data$SP2_a)
levels(data$SP2_a) <- c("3", "3", "4", "5", "6", "7", "7") # combine age levels
data$SP2_a <- factor(data$SP2_a, levels = 3:7, 
                     labels = c("<=24", "25-34", "35-44", "45-54", ">=55"))
# length of tenure
data$SA4 <- factor(data$SA4, labels = c("<1", "1-<3", "3-<5", "5-<10", ">=10"))

# elapsed time between both measurements in days
data$Duration <- as.numeric(ymd(data$Timings_yyyymmdd_3) - ymd(data$Timings_yyyymmdd))

# rename variables
names(data)[names(data) == "SP1_a"] <- "Gender"
names(data)[names(data) == "SP2_a"] <- "Age"
names(data)[names(data) == "SA4"] <- "Tenure"

## prepare administrative variables

data <- as.data.table(data) # use data.table

##speeders
# wave 1
data[WAVE == 1, TimeSec_W1 := Timings_dateFinish - Timings_dateFirstEntry]
data[WAVE == 1 & TimeSec_W1 < 230, Speeder_W1 := 1]
# wave 2
data[WAVE == 2, TimeSec_W2 := Timings_dateFinish - Timings_dateFirstEntry]
data[WAVE == 2 & TimeSec_W2 < 230, Speeder_W2 := 1]
# wave 3
data[WAVE_3 == 3, TimeSec_W3 := Timings_dateFinish_3 - Timings_dateFirstEntry_3]
data[WAVE_3 == 3 & TimeSec_W3 < 230, Speeder_W3 := 1]
# in wave 1 or 2
data[Speeder_W1 == 1 | Speeder_W2 == 1, Speeder_W12 := 1]
# in wave 1 or 2 or 3
data[Speeder_W1 == 1 | Speeder_W2 == 1 | Speeder_W3 == 1, Speeder_all := 1]

## straightliners
# wave 1
data[WAVE == 1 & A1 == A2 & A2 == A3 & A3 == A5 & A5 == A6 & A1_3 > 0, Straight_A_W1 := 1]
data[WAVE == 1 & B1 == B2 & B2 == B3 & B3 == B5 & B5 == B6 & B6 == B7 & B7 == B8 &
    B8 == B11 & B11 == B12 & B12 == B13 & B13 == B14 & B14 == B15 & B15 == B16 &
    B16 == B17 & B17 == B19 & B19 == B20 & B20 == B21 & B1 > 0, Straight_B_W1 := 1]
data[WAVE == 1 & C1 == C2 & C2 == C3 & C3 == C4 & C4 == C5 & 
    C5 == C6 & C6 == C7 & C1 > 0, Straight_C_W1 := 1]
data[WAVE == 1 & D1 == D2 & D2 == D3 & D3 == D4 & D4 == D5 &
    D1 > 0, Straight_D_W1 := 1]
data[WAVE == 1 & E1 == E2 & E2 == E3 & E3 == E4 & E4 == E5 & E5 == E6 & 
    E6 == E7 & E7 == E8 & E1 > 0, Straight_E_W1 := 1]
data[WAVE == 1 & F1 == F2 & F2 == F3 & F3 == F5 & F5 == F6 & F6 == F7 & 
    F7 == F9 & F9 == F10 & F10 == F12 & F12 == F13 & F13 == F14 &
    F14 == F15 & F15 == F16 & F1 > 0, Straight_F_W1 := 1]
data[WAVE == 1 & G2_1 == G2_2 & G2_2 == G2_3 & G2_3 == G2_4 & G2_4 == G2_5 &
    G2_5 == G2_6 & G2_6 == G2_8 & G2_8 == G2_9 & G2_9 == G2_10 &
    G2_10 == G2_11 & G2_11 == G2_7 & G2_7 == G2_12 & G2_12 == G2_13 &
    G2_13 == G2_15 & G2_15 == G2_16 & G2_16 == G2_17 & G2_17 == G2_19 &
    G2_19 == G2_20 & G2_1 > 0, Straight_G_W1 := 1]
data[WAVE == 1 & H1 == H2 & H2 == H3 & H3 == H4 & H4 == H5 & H5 == H6 & H6 == H7 &
    H7 == H8 & H8 == H9 & H9 == H10 & H10 == H11 & H1 > 0, Straight_H_W1 := 1]
data[WAVE == 1 & I1 == I2 & I2 == I3 & I3 == I4 & I4 == I6 & I6 == I7 & I7 == I10 & 
    I10 == I12 & I12 == I15 & I15 == I16 & I16 == I17 & I1 > 0, Straight_I_W1 := 1]
data[WAVE == 1 & OM1 == OM2 & OM2 == OM3 & OM3 == OM5 & OM5 == OM6 & OM6 == OM7 &
    OM7 == OM8 & OM8 == OM9 & OM9 == OM10 & OM10 == OM11 & OM11 == OM12 & OM12 == OM13 &
    OM13 == OM14 & OM14 == OM15 & OM15 == OM16 & OM16 == OM18 & OM18 == OM19 & 
    OM19 == OM21 & OM1 > 0, Straight_OM_W1 := 1]
data[, Sum_Straight_W1 := rowSums(.SD, na.rm = TRUE), 
  .SDcols = c("Straight_A_W1", "Straight_B_W1", "Straight_C_W1", "Straight_D_W1", "Straight_E_W1", 
    "Straight_F_W1", "Straight_G_W1", "Straight_H_W1", "Straight_I_W1", "Straight_OM_W1")]
data[Sum_Straight_W1 > 7, SL_W1 := 1]
# wave 2 
data[WAVE == 2 & A1 == A2 & A2 == A3 & A3 == A5 & A5 == A6 & A1_3 > 0, Straight_A_W2 := 1]
data[WAVE == 2 & B1 == B2 & B2 == B3 & B3 == B5 & B5 == B6 & B6 == B7 & B7 == B8 &
    B8 == B11 & B11 == B12 & B12 == B13 & B13 == B14 & B14 == B15 & B15 == B16 &
    B16 == B17 & B17 == B19 & B19 == B20 & B20 == B21 & B1 > 0, Straight_B_W2 := 1]
data[WAVE == 2 & C1 == C2 & C2 == C4 & C4 == C5 & 
    C5 == C6 & C1 > 0, Straight_C_W2 := 1]
data[WAVE == 2 & D1 == D2 & D2 == D3 & D3 == D4 & D4 == D5 &
    D1 > 0, Straight_D_W2 := 1]
data[WAVE == 2 & E1 == E2 & E2 == E3 & E3 == E4 & E4 == E5 & E5 == E6 & 
    E6 == E8 & E1 > 0, Straight_E_W2 := 1]
data[WAVE == 2 & F1 == F3 & F3 == F5 & F5 == F6 & F6 == F7 & 
    F7 == F9 & F9 == F12 & F12 == F13 & F13 == F14 &
    F14 == F15 & F15 == F16 & F1 > 0, Straight_F_W2 := 1]
data[WAVE == 2 & G2_1 == G2_2 & G2_2 == G2_3 & G2_3 == G2_4 & G2_4 == G2_5 &
    G2_5 == G2_6 & G2_6 == G2_8 & G2_8 == G2_9 & G2_9 == G2_10 &
    G2_10 == G2_11 & G2_11 == G2_7 & G2_7 == G2_12 & G2_12 == G2_13 &
    G2_13 == G2_15 & G2_15 == G2_16 & G2_16 == G2_17 & G2_17 == G2_19 &
    G2_19 == G2_20 & G2_1 > 0, Straight_G_W2 := 1]
data[WAVE == 2 & H1 == H2 & H2 == H3 & H3 == H4 & H4 == H5 & H5 == H6 & H6 == H7 &
    H7 == H8 & H8 == H9 & H9 == H10 & H10 == H11 & H1 > 0, Straight_H_W2 := 1]
data[WAVE == 2 & OM2 == OM3 & OM3 == OM5 & OM5 == OM6 & OM6 == OM7 &
    OM7 == OM8 & OM8 == OM9 & OM9 == OM10 & OM10 == OM11 & OM11 == OM12 & OM12 == OM14 &
    OM14 == OM15 & OM15 == OM16 & OM16 == OM18 & OM18 == OM19 & OM19 == OM21 & OM2 > 0, 
    Straight_OM_W2 := 1]
data[, Sum_Straight_W2 := rowSums(.SD, na.rm = TRUE), 
  .SDcols = c("Straight_A_W2", "Straight_B_W2", "Straight_C_W2", "Straight_D_W2", 
    "Straight_E_W2", "Straight_F_W2", "Straight_G_W2", "Straight_H_W2", "Straight_OM_W2")]
data[Sum_Straight_W2 > 6, SL_W2 := 1]
# wave 3 
data[WAVE_3 == 3 & A1_3 == A2_3 & A2_3 == A3_3 & A3_3 == A5_3 & A5_3 == A6_3 & A1_3 > 0, 
  Straight_A_W3 := 1]
data[WAVE_3 == 3 & B1_3 == B5_3 & B5_3 == B6_3 & B6_3 == B7_3 & B7_3 == B11_3 & 
    B11_3 == B12_3 & B12_3 == B13_3 & B13_3 == B14_3 & B14_3 == B15_3 & B15_3 == B20_3 &
    B1_3 > 0, Straight_B_W3 := 1]
data[WAVE_3 == 3 & C1_3 == C2_3 & C2_3 == C4_3 & C4_3 == C5_3 & 
    C5_3 == C6_3 & C1_3 > 0, Straight_C_W3 := 1]
data[WAVE_3 == 3 & D1_3 == D2_3 & D2_3 == D3_3 & D3_3 == D5_3 &
    D1_3 > 0, Straight_D_W3 := 1]
data[WAVE_3 == 3 & E1_3 == E2_3 & E2_3 == E4_3 & E4_3 == E5_3 & E5_3 == E6_3 & 
    E6_3 == E8_3 & E1_3 > 0, Straight_E_W3 := 1]
data[WAVE_3 == 3 & F1_3 == F5_3 & F5_3 == F6_3 & F6_3 == F7_3 & 
    F7_3 == F9_3 & F9_3 == F12_3 & F12_3 == F13_3 & F13_3 == F14_3 &
    F14_3 == F15_3 & F15_3 == F16_3 & F1_3 > 0, Straight_F_W3 := 1]
data[WAVE_3 == 3 & G2_1_3 == G2_2_3 & G2_2_3 == G2_3_3 & G2_3_3 == G2_4_3 & 
    G2_4_3 == G2_5_3 & G2_5_3 == G2_6_3 & G2_6_3 == G2_8_3 & G2_8_3 == G2_9_3 &
    G2_9_3 == G2_10_3 & G2_10_3 == G2_11_3 & G2_11_3 == G2_7_3 & 
    G2_7_3 == G2_13_3 & G2_13_3 == G2_15_3 & G2_15_3 == G2_16_3 & G2_16_3 == G2_17_3 &
    G2_17_3 == G2_19_3 & G2_1_3 > 0, Straight_G_W3 := 1]
data[WAVE_3 == 3 & H1_3 == H2_3 & H2_3 == H3_3 & H3_3 == H4_3 & H4_3 == H5_3 & 
    H5_3 == H6_3 & H6_3 == H7_3 & H7_3 == H8_3 & H8_3 == H9_3 & H9_3 == H10_3 & 
    H10_3 == H11_3 & H1_3 > 0, Straight_H_W3 := 1]
data[WAVE_3 == 3 & OM2_3 == OM3_3 & OM3_3 == OM5_3 & OM5_3 == OM7_3 &
    OM7_3 == OM8_3 & OM8_3 == OM9_3 & OM9_3 == OM10_3 & OM10_3 == OM11_3 & OM11_3 == OM12_3 &
    OM12_3 == OM14_3 & OM14_3 == OM15_3 & OM15_3 == OM16_3 & OM16_3 == OM18_3 & 
    OM2_3 > 0, Straight_OM_W3 := 1]
data[WAVE_3 == 3, Sum_Straight_W3 := rowSums(.SD, na.rm = TRUE), 
  .SDcols = c("Straight_A_W3", "Straight_B_W3", "Straight_C_W3", "Straight_D_W3", "Straight_E_W3",
    "Straight_F_W3", "Straight_G_W3", "Straight_H_W3", "Straight_OM_W3")]
data[WAVE_3 == 3 & Sum_Straight_W3 > 6, SL_W3 := 1]
# in wave 1 and wave 2
data[SL_W1 == 1 | SL_W2 == 1, SL_W12 := 1]
# in all waves
data[SL_W1 == 1 | SL_W2 == 1 | SL_W3 == 1, SL_all := 1]

## speeders or straightliners
# in wave 1 or wave 2
data[Speeder_W12 == 1 | SL_W12 == 1, SPSL_W12 := 1]
# in wave 3
data[Speeder_W3 == 1 | SL_W3 == 1, SPSL_W3 := 1]
# in wave 1 or wave 2 or wave 3
data[SPSL_W12 == 1 | SPSL_W3 == 1, SPSL_W123 := 1]

## changejob
data[SA4_3 == 1 | V1_3_Code1 %in% c(2,5,7,16), ChangeJob := 1]

data <- as.data.frame(data) # retransform into data.frame

# code missing values in administrative variables as 0
data$SPSL_W12[is.na(data$SPSL_W12)] <- 0
data$SPSL_W123[is.na(data$SPSL_W123)] <- 0
data$ChangeJob[is.na(data$ChangeJob)] <- 0

## collect variables

items_list <- list(
  SAA = c("B12", "B11", "B5", "B7", "B20", "B15", "B13", "B6"),
  SFS = c("G2_10_rec", "G2_11_rec", "G2_16_rec", "G2_8_rec", "G2_17_rec"),
  SKB = c("G2_6", "G2_19", "G2_15", "G2_7", "G2_3", "G2_9", "G2_13", "F1", "F13"),
  SAU = c("A5", "A2", "A3", "A6", "A1"),
  SINF = c("E6_rec", "E5_rec", "E8_rec"),
  SSU = c("G2_4_rec", "G2_2_rec", "G2_5_rec", "G2_1_rec"),
  SUN = c("E2", "E4", "E1"),
  SHS = c("C5_rec", "C1", "C2"),
  SIM = c("F7_rec", "F14_rec", "F9_rec", "D3_rec", "D2_rec"),
  SRESI = c("H8", "H7", "H1", "H9", "H2"),
  SRIS = c("H11", "H6", "H10", "H5_rec", "H4"),
  BelastIndex = c("B14", "B1", "F16_rec", "D5", "C6_rec", "D1_rec", "C4_rec", "F12_rec", "H3", "F15", "F6")
)
covariates_cross <- c("Gender", "Age", "Tenure")
covariates_long <- c("Gender", "Age", "Tenure", "Duration", "SBF_12", "O1_rec", "F5", "O2cat_12")
administratives <- c("Respondent_ID", "SPSL_W12", "SPSL_W123", "ChangeJob", "WAVE")

items <- as.character(unlist(items_list))
sumscores <- names(items_list)

targets_cross_sectional <- c("SBF_12", "O1_rec", "F5", "O2cat_12")
targets_longitudinal <- c("SBF_3", "O1_3_rec", "F5_3", "O2cat_3")

# compute sumscores and add to data
data <- cbind(data, lapply(items_list, function(x) rowSums(data[, x], na.rm = FALSE)))

## indices with complete cases
i_cross <- 
  data$SPSL_W12 == 0 & complete.cases(data[, c(items, covariates_cross, targets_cross_sectional)]) 
i_long_strain_health_turnover <- 
  data$SPSL_W123 == 0 & data$ChangeJob == 0 & complete.cases(data[, c(items, covariates_long, "SBF_3", "O1_3_rec", "F5_3")]) 
i_long_sickdays <- 
  data$SPSL_W123 == 0 & data$ChangeJob == 0 & complete.cases(data[, c(items, covariates_long, "O2cat_3")])
