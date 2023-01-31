#  .-.    __
# |   |  /\ \  ValidR
# |   |  \_\/      __        .-.
# |___|        __ /\ \      /:::\
# |:::|       / /\\_\/     /::::/
# |:::|       \/_/        / `-:/
# ':::'__   _____ _____  /    /
#     / /\ /     |:::::\ \   /
#     \/_/ \     |:::::/  `"`
#           `"""""""""`

{require(readr)
require(dplyr)
require(modelr)
require(data.table)
require(ggplot2)
require(kableExtra)
require(plotly)
require(viridis)
require(ggpmisc)
require(writexl)
require(readxl)}

#------------------------------------------------------------------------------#
# Common / global variables
#------------------------------------------------------------------------------#



# il y a un problème avec le fichier RMD qui n'arrive pas à trouver le fichier script

############################################################## 
# ========= LOAD GLOBAL VARIABLES ! (before running) ========# 
##############################################################

# write_xlsx(generate_template(nSERIE, nCAL_LEVEL, nCAL_REP, nVAL_LEVEL, nVAL_REP),"TEMPLATE.xlsx")
# TEMPLATE <- read_excel("data_valid.xlsx")


# RAW_DATA <- read_delim("data_valid2.csv",
#   ";",
#   escape_double = FALSE,
#   col_types = cols(
#     TYPE = col_factor(levels = c("CAL", "VAL")),
#     SERIE = col_factor(levels = c("1", "2", "3"))
#   ),
#   trim_ws = TRUE
# ) # User defined

# data_valid2 <- read_delim("data_valid.csv",
#   ";",
#   escape_double = FALSE, col_types = cols(SERIE = col_factor(levels = c(
#     "1",
#     "2", "3"
#   ))), trim_ws = TRUE
# )

#dfRMDdfRMD <← read_excel("data_valid.xlsx")

#dfRMDdfRMD <← as.data.table(dfRMD)

#nBETA <- 0.9 # User defined
#cUNIT <- "(ng/mL)"
#nACC_LIMIT <- 10




# =========================== #
##### CALIBRATION CURVES ######
# =========================== #

# CAL : Values (signal) for calibration standards SIGNAL = f(CONC_LEVEL)
# VAL : Values (signal) for validation standards CONC = f(SIGNALM)

#------------------------------------------------------------------------------#
# This function calculates the parameters of different calibration curves for 
# dfRMD and fills a CALIB_CURVE table (global variable) with this parameters.
#------------------------------------------------------------------------------#

compute_calibration_curves <- function() {
  for (i in levels(as.factor(dfRMD$SERIE))) {

    # MOD LIN
    
    linear_model <- lm(formula = SIGNAL ~ CONC_LEVEL, 
                       data = dfRMD %>% filter(TYPE == "CAL" & SERIE == i))
    ## INSERT CAL RESIDUES
    dfRMD <<- setDT(dfRMD)[(SERIE == i) & TYPE == "CAL", 
                        RES_LIN := (SIGNAL - (coef(linear_model)[2] * CONC_LEVEL + coef(linear_model)[1])) / SIGNAL * 100]
    ## PREDICT VAL CONC_LEVEL BASED ON SIGNAL
    dfRMD <<- setDT(dfRMD)[(SERIE == i) & (TYPE == "VAL"), 
                        LIN := (SIGNAL - coef(linear_model)[1]) / coef(linear_model)[2]]
    ## STORE CALIBRATION dfRMD
    calib_curves <<- calib_curves %>% 
                        add_row(method = "Linear (LM)", 
                                series = i, 
                                intercept = coef(linear_model)[1], 
                                slope = coef(linear_model)[2], 
                                aic = AIC(linear_model), 
                                "r2" = summary(linear_model)$r.squared
                                )

    # MOD LIN 0

    linear_model_0 <- lm(formula = SIGNAL ~ 0 + CONC_LEVEL, 
                         data = dfRMD %>% filter(TYPE == "CAL" & SERIE == i))
    ## INSERT CAL RESIDUES
    dfRMD <<- setDT(dfRMD)[(SERIE == i) & TYPE == "CAL", 
                        RES_LIN_0 := (SIGNAL - coef(linear_model_0)[1] * CONC_LEVEL) / SIGNAL * 100]
    ## PREDICT VAL CONC_LEVEL BASED ON SIGNAL
    dfRMD <-- setDT(dfRMD)[(SERIE == i) & (TYPE == "VAL"), 
                        LIN_0 := SIGNAL / coef(linear_model_0)[1]]
    ## STORE CALIBRATION dfRMD
    calib_curves <<- calib_curves %>% 
                        add_row(method = "Linear 0 (LM)", 
                                series = i, 
                                intercept = 0, 
                                slope = coef(linear_model_0)[1],
                                aic = AIC(linear_model_0), 
                                "r2" = summary(linear_model_0)$r.squared
                                )

    # MOD LIN weighed 1/X

    linear_model_1X <- lm(formula = SIGNAL ~ CONC_LEVEL, 
                          data = dfRMD %>% filter(TYPE == "CAL" & SERIE == i), weights = 1 / CONC_LEVEL)
    ## INSERT CAL RESIDUES
     setDT(dfRMD)[(SERIE == i) & (TYPE == "CAL"), 
                        RES_LIN_1X := (SIGNAL - (coef(linear_model_1X)[2] * CONC_LEVEL + coef(linear_model_1X)[1])) / SIGNAL * 100]
    ## PREDICT VAL CONC_LEVEL BASED ON SIGNAL
     dfRMD <<- setDT(dfRMD)[(SERIE == i) & (TYPE == "VAL"), 
                        LIN_1X := (SIGNAL - coef(linear_model_1X)[1]) / coef(linear_model_1X)[2]]
    ## STORE CALIBRATION dfRMD
    calib_curves <<- calib_curves %>% 
                        add_row(method = "Linear weighed 1/X (LM)", 
                                series = i, 
                                intercept = coef(linear_model_1X)[1], 
                                slope = coef(linear_model_1X)[2],
                                aic = AIC(linear_model_1X), 
                                "r2" = summary(linear_model_1X)$r.squared
                                )

    # MOD LIN weighed 1/Y

    linear_model_1Y <- lm(formula = SIGNAL ~ CONC_LEVEL, 
                          data = dfRMD %>% filter(TYPE == "CAL" & SERIE == i), weights = 1 / SIGNAL)
    ## INSERT CAL RESIDUES
    dfRMD <<- setDT(dfRMD)[(SERIE == i) & TYPE == "CAL", 
                        RES_LIN_1Y := (SIGNAL - (coef(linear_model_1Y)[2] * CONC_LEVEL + coef(linear_model_1Y)[1])) / SIGNAL * 100]
    ## PREDICT VAL CONC_LEVEL BASED ON SIGNAL
    dfRMD <<- setDT(dfRMD)[(SERIE == i) & (TYPE == "VAL"), 
                        LIN_1Y := (SIGNAL - coef(linear_model_1Y)[1]) / coef(linear_model_1Y)[2]]
    calib_curves <<- calib_curves %>% 
                        add_row(method = "Linear weighed 1/Y (LM)", 
                                series = i, intercept = coef(linear_model_1Y)[1], 
                                slope = coef(linear_model_1Y)[2],
                                aic = AIC(linear_model_1Y), 
                                "r2" = summary(linear_model_1Y)$r.squared
                                )

    # MOD LIN 0->max

    linear_model_0max <- lm(formula = SIGNAL ~ 0 + CONC_LEVEL, 
                            data = dfRMD %>% filter(TYPE == "CAL" & SERIE == i & CONC_LEVEL == tail(levels(as.factor(dfRMD$CONC_LEVEL[dfRMD$TYPE == "CAL"])), n = 1)))
    ## INSERT CAL RESIDUES
    dfRMD <<- setDT(dfRMD)[(SERIE == i) & TYPE == "CAL", 
                        RES_LIN_0MAX := (SIGNAL - (coef(linear_model_0max)[1] * CONC_LEVEL)) / SIGNAL * 100]
    ## PREDICT VAL CONC_LEVEL BASED ON SIGNAL
    dfRMD <<- setDT(dfRMD)[(SERIE == i) & (TYPE == "VAL"), 
                        LIN_0MAX := SIGNAL / coef(linear_model_0max)[1]]
    ## STORE CALIBRATION dfRMD
    calib_curves <<- calib_curves %>% 
                        add_row(method = "Linear 0-max (LM)", 
                                series = i, 
                                intercept = 0, 
                                slope = coef(linear_model_0max)[1],
                                aic = AIC(linear_model_0max), 
                                "r2" = summary(linear_model_0max)$r.squared
                                )
  }
}

# Running function compute_calibration_curves()
# compute_calibration_curves(dfRMD)



#------------------------------------------------------------------------------#
# This function plot a curve for each SERIE of the string1 TYPE in dfRMD     
#------------------------------------------------------------------------------#

print_plot_standards <- function(data, string1) {
  p1 <- ggplot(data %>% filter(TYPE == string1) %>% select(SERIE, SIGNAL, CONC_LEVEL), 
               aes(x = CONC_LEVEL, y = SIGNAL, color = as.factor(SERIE))
               ) +
            geom_point() +
            xlab("Concentration levels") +
            ylab("Signal") +
            labs(color = "Serie") +
            scale_color_viridis(discrete = TRUE) +
            theme_light()
  return(p1)
}

# Function testing
#ggplotly(print_plot_standards(dfRMD, "CAL") + geom_smooth(method = "lm", se = TRUE, color = "black", formula = y ~ x, size = 0.25))

#------------------------------------------------------------------------------#
# This function returns a structured table (kable) the string1 TYPE in dfRMD         
# (this is to ease incorporation of raw data values in the report)
#------------------------------------------------------------------------------#

print_table_rawdata <- function(data, string1) {
  string2 <- if_else(string1 == "CAL", "Calibration standards raw data", "Validation standards raw data")
  table <- data %>%
    filter(TYPE == string1) %>%
    select(ID, TYPE, SERIE, SIGNAL, CONC_LEVEL) %>%
    kableExtra::kable(caption = string2) %>%
    kable_styling("hover")
  return(table)
}

# testing
#print_table_rawdata(dfRMD, "CAL")


#------------------------------------------------------------------------------#
# This function returns a plot (ggplot) of relative bias = f(CONC_LEVEL) for the 
# calibration standards according to the type of regression chosen.
# The dfRMD must have been processed with compute_calibration_curves()
#------------------------------------------------------------------------------#

plot_residues <- function(data) {
  v_colors <- viridis(5)
  colors <- c(
    "Linear" = v_colors[1],
    "Linear 0" = v_colors[2],
    "Linear 1/X" = v_colors[3],
    "Linear 1/Y" = v_colors[4],
    "Linear 0 max" = v_colors[5]
  )

  p2 <- ggplot(data = data %>% filter(TYPE == "CAL"), 
               aes(y = SIGNAL, x = CONC_LEVEL)) +
            geom_point(data = data %>% filter(TYPE == "CAL"), 
                       aes(y = RES_LIN, x = CONC_LEVEL, color = "Linear"), 
                       shape = 1) +
            stat_summary(fun = mean, 
                         data = data %>% filter(TYPE == "CAL"), 
                         aes(y = RES_LIN, x = CONC_LEVEL, color = "Linear"), 
                         geom = "line") +
            geom_point(data = data %>% filter(TYPE == "CAL"), 
                       aes(y = RES_LIN_0, x = CONC_LEVEL, color = "Linear 0"), 
                       shape = 2) +
            stat_summary(fun = mean, 
                         data = data %>% filter(TYPE == "CAL"), 
                         aes(y = RES_LIN_0, x = CONC_LEVEL, color = "Linear 0"), 
                         geom = "line") +
            geom_point(data = data %>% filter(TYPE == "CAL"), 
                       aes(y = RES_LIN_1X, x = CONC_LEVEL, color = "Linear 1/X"), 
                       shape = 3) +
            stat_summary(fun = mean, 
                         data = data %>% filter(TYPE == "CAL"), 
                         aes(y = RES_LIN_1X, x = CONC_LEVEL, color = "Linear 1/X"), 
                         geom = "line") +
            geom_point(data = data %>% filter(TYPE == "CAL"), 
                       aes(y = RES_LIN_1Y, x = CONC_LEVEL, color = "Linear 1/Y"), 
                       shape = 4) +
            stat_summary(fun = mean, data = data %>% filter(TYPE == "CAL"), 
                         aes(y = RES_LIN_1Y, x = CONC_LEVEL, color = "Linear 1/Y"), 
                         geom = "line") +
            geom_point(data = data %>% filter(TYPE == "CAL"), 
                       aes(y = RES_LIN_0MAX, x = CONC_LEVEL, color = "Linear 0 max"), 
                       shape = 5) +
            stat_summary(fun = mean, 
                         data = data %>% filter(TYPE == "CAL"), 
                         aes(y = RES_LIN_0MAX, x = CONC_LEVEL, color = "Linear 0 max"), 
                         geom = "line") +
            geom_hline(yintercept = 0, color = "red") +
            ylab("Relative bias (%)") +
            xlab("Concentration levels") +
            labs(color = "Regression types") +
            scale_color_manual(values = colors) +
            theme_light()
  return(p2)
}

#testing
#ggplotly(plot_residues(dfRMD))




# ============================ #
##### VALIDATION STANDARDS #####
# ============================ #

#------------------------------------------------------------------------------#
# This function compute for each calibration type biais, intermediate precision 
# This function calculates for each type of calibration the statistical 
# parameters associated with the validation standards such as reproducibility, 
# repeatability and precision interval  
# The function add element to a list() : listVALID (globale variable) for each 
# calibration type
#------------------------------------------------------------------------------#

# To do -> Trouver les points d'intersections avec les limites (-> Limites de quanti)
# 
compute_validation_data <- function() {

  # MEAN of Inverse prediction by levels
  cols <- c("LIN", "LIN_0", "LIN_1X", "LIN_1Y", "LIN_0MAX")
  dfRMD <<- dfRMD[, paste(cols, "hat", sep = "_") := lapply(.SD, mean, na.rm = TRUE), 
               by = CONC_LEVEL, .SDcols = cols][]

  # SELECT ONLY VALIDATION dfRMD
  DATA_VALID_1 <- dfRMD %>%
                    select(-contains("RES")) %>%
                    filter(TYPE == "VAL")

  for (var in what_to_grep) {
    cVAR <- paste0(var)
    cVAR2 <- paste0(var, "_hat")
    VALID <- DATA_VALID_1 %>% select(SERIE, CONC_LEVEL, SIGNAL, all_of(cVAR), all_of(cVAR2))
    VALID <- setDT(VALID)[, BIAS := get(cVAR2) - CONC_LEVEL, by = CONC_LEVEL][
      , BIAS_pc := 100 * (BIAS) / CONC_LEVEL,
      by = CONC_LEVEL
    ][
      , RECOV_LIN_pc := 100 * (get(cVAR2)) / CONC_LEVEL,
      by = CONC_LEVEL
    ][
      , ERROR_pc := (get(cVAR) - CONC_LEVEL) / CONC_LEVEL * 100
    ]

    for (j in levels(as.factor(VALID$CONC_LEVEL))) {
      VALID <- setDT(VALID)[CONC_LEVEL == j, SQU_DIFF := (get(cVAR) - mean(get(cVAR)))^2, by = SERIE]
      VALID <- setDT(VALID)[CONC_LEVEL == j, SUM_SQU_DIFF := sum(SQU_DIFF), by = SERIE]
      VALID <- setDT(VALID)[CONC_LEVEL == j, SUM_SQU_DIFF_RES := sum(SUM_SQU_DIFF) / (nrow(VALID[CONC_LEVEL == j]) / nlevels(as.factor(VALID$SERIE))) ,] # need to divide as repetition of value by row
      VALID <- setDT(VALID)[CONC_LEVEL == j, VAR_REP := SUM_SQU_DIFF_RES / (nrow(VALID[CONC_LEVEL == j]) - nlevels(as.factor(VALID$SERIE))), ] 
      # REPEATABILITY SD STDEV_REP
      VALID <- setDT(VALID)[CONC_LEVEL == j, STDEV_REP := sqrt(VAR_REP), ]
      VALID <- setDT(VALID)[CONC_LEVEL == j, SQU_DIF_FULL := (get(cVAR) - mean(get(cVAR)))^2]
      VALID <- setDT(VALID)[CONC_LEVEL == j, SUM_SQU_DIF_FULL := sum(SQU_DIF_FULL)]
      VALID <- setDT(VALID)[CONC_LEVEL == j, SUM_SQU_DIF_BS := SUM_SQU_DIF_FULL - SUM_SQU_DIFF_RES]
      VALID <- setDT(VALID)[CONC_LEVEL == j, int_VAR_BS := ((SUM_SQU_DIF_BS / (nlevels(as.factor(VALID$SERIE)) - 1)) - VAR_REP) / (nrow(VALID[CONC_LEVEL == j])/nlevels(as.factor(VALID$SERIE)))] #
      VALID <- setDT(VALID)[CONC_LEVEL == j, VAR_BS := ifelse(int_VAR_BS > 0, int_VAR_BS, 0)]
      # BETWEEN SERIES SD STDEV_BS
      VALID <- setDT(VALID)[CONC_LEVEL == j, STDEV_BS := sqrt(VAR_BS)]
      # INTERMEDIATER PRECISION SD STDEV_FI
      VALID <- setDT(VALID)[CONC_LEVEL == j, STDEV_FI := sqrt(VAR_BS + VAR_REP)]
      # CV
      VALID <- setDT(VALID)[CONC_LEVEL == j, CV_REP := STDEV_REP / CONC_LEVEL * 100] # MEAN(get(CVAR)) ???
      VALID <- setDT(VALID)[CONC_LEVEL == j, CV_IP := STDEV_FI / CONC_LEVEL * 100]
      #
      VALID <- setDT(VALID)[CONC_LEVEL == j, ratio_VAR := VAR_BS / VAR_REP]
      VALID <- setDT(VALID)[CONC_LEVEL == j, B2 := (ratio_VAR + 1) / ((nrow(VALID[CONC_LEVEL == j]) / nlevels(as.factor(VALID$SERIE))) * ratio_VAR + 1)]
      VALID <- setDT(VALID)[CONC_LEVEL == j, CI := sqrt(1 + 1 / (nrow(VALID[CONC_LEVEL == j]) * B2))]
      VALID <- setDT(VALID)[CONC_LEVEL == j, DDL := (ratio_VAR + 1)^2 / ((ratio_VAR + 1 / (nrow(VALID[CONC_LEVEL == j]) / nlevels(as.factor(VALID$SERIE))))^2 / (nlevels(as.factor(VALID$SERIE)) - 1) + (1 - 1 / (nrow(VALID[CONC_LEVEL == j]) / nlevels(as.factor(VALID$SERIE)))) / nrow(VALID[CONC_LEVEL == j]))]
      VALID <- setDT(VALID)[CONC_LEVEL == j, KTOL := qt((1 + nBETA) / 2, DDL) * CI] # nBETA global variable
      VALID <- setDT(VALID)[CONC_LEVEL == j, TVL_ABS := mean(get(cVAR)) - KTOL * STDEV_FI]
      VALID <- setDT(VALID)[CONC_LEVEL == j, TVH_ABS := mean(get(cVAR)) + KTOL * STDEV_FI]
      VALID <- setDT(VALID)[CONC_LEVEL == j, TVL_RELATIVE := BIAS_pc - KTOL * CV_IP]
      VALID <- setDT(VALID)[CONC_LEVEL == j, TVH_RELATIVE := BIAS_pc + KTOL * CV_IP] 
      VALID <- setDT(VALID)[, pass := ifelse(test = TVL_RELATIVE > -nACC_LIMIT & TVH_RELATIVE < nACC_LIMIT, "PASS", "FAIL")]
    } # Calculations verified on Hubert et al. (2007) data
    listVALID[[var]] <<- VALID
  }
}

#testing
#compute_validation_data(dfRMD)
#viewing
#check = listVALID[["LIN"]]

plot_validation <- function(data, string) {
  p <- ggplot(data, aes(x = CONC_LEVEL, y = ERROR_pc, color = as.factor(SERIE))) +
    geom_hline(yintercept = nACC_LIMIT, color = "red", linetype = "dashed") +
    geom_hline(yintercept = -nACC_LIMIT, color = "red", linetype = "dashed") +
    geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
    geom_point() +
    geom_line(data = data %>% select(-SERIE, -pass) %>% 
                group_by(CONC_LEVEL) %>% summarize_all(~ mean(.x, na.rm = TRUE)), 
              aes(x = CONC_LEVEL, y = TVL_RELATIVE), color = "blue") +
    geom_line(data = data %>% select(-SERIE, -pass) %>% 
                group_by(CONC_LEVEL) %>% summarize_all(~ mean(.x, na.rm = TRUE)), 
              aes(x = CONC_LEVEL, y = TVH_RELATIVE), color = "blue") +
    geom_line(data = data %>% select(-SERIE, -pass) %>% 
                group_by(CONC_LEVEL) %>% summarize_all(~ mean(.x, na.rm = TRUE)), 
              aes(x = CONC_LEVEL, y = BIAS_pc), color = "black") +
    theme_light() +
    scale_color_viridis(discrete = TRUE) +
    ggtitle(paste(string)) +
    xlab(paste("Concentration levels ", cUNIT)) +
    ylab("Relative error (%)") +
    labs(color = "Serie")

  return(p)
}

#test
# for (x in 1:length(listVALID)) {
#   print(ggplotly(plot_validation(as.data.frame(listVALID[[x]]), names(listVALID[x]))))
# }

summary_validation_abs <- function(data, string) {
  string2 <- paste(string, "hat", sep = "_")
  t <- data %>%
    group_by(CONC_LEVEL) %>%
    select(-SERIE, -pass) %>%
    summarize_all(~ mean(.x, na.rm = TRUE)) %>%
    select(CONC_LEVEL, string2, BIAS, STDEV_REP, VAR_BS, STDEV_FI, TVL_ABS, TVH_ABS) %>%
    kableExtra::kable(col.names = c(
      paste("Introduced concentrations ", cUNIT),
      paste("Mean calculated concentrations ", cUNIT),
      paste("Bias ", cUNIT),
      paste("Repeatability SD ", cUNIT),
      paste("Between series SD ", cUNIT),
      paste("Intermediate precision SD ", cUNIT),
      paste("Low limit of tolerance ", cUNIT),
      paste("High limit of tolerance ", cUNIT)
    ), caption = paste("Trueness and precision estimators and limits (", string, ")")) %>%
    kable_styling("hover")
  return(t)
}

summary_validation_rel <- function(data, string) {
  string2 <- paste(string, "hat", sep = "_")
  t <- data %>%
    group_by(CONC_LEVEL) %>%
    select(-SERIE, -pass) %>%
    summarize_all(~ mean(.x, na.rm = TRUE)) %>%
    select(CONC_LEVEL, string2, BIAS_pc, RECOV_LIN_pc, CV_REP, CV_IP, TVL_RELATIVE, TVH_RELATIVE) %>%
    mutate(pass = ifelse(TVL_RELATIVE > -nACC_LIMIT & TVH_RELATIVE < nACC_LIMIT, "PASS", "FAIL")) %>%
    kableExtra::kable(col.names = c(
      paste("Introduced concentrations ", cUNIT),
      paste("Mean calculated concentrations ", cUNIT),
      "Bias (%)",
      "Recovery (%)",
      "CV repeatability (%)",
      "CV intermediate precision (%)",
      "Low limit of tolerance (%)",
      "High limit of tolerance (%)",
      "Results"
    ), caption = paste("Trueness and precision estimators and limits (", string, ")")) %>%
    kable_styling("hover")
  return(t)
}

#test
# for (x in 1:length(listVALID)) {
#   print(summary_validation_abs(as.data.frame(listVALID[[x]]), names(listVALID[x])))
# }
# 
# for (x in 1:length(listVALID)) {
#   print(summary_validation_rel(as.data.frame(listVALID[[x]]), names(listVALID[x])))
# }

plot_linearity <- function(data, string) {
  p <- ggplot(data, aes(x = CONC_LEVEL, y = data[, 4])) +
    geom_abline(slope = 1, color = "red", linetype = "dashed") +
    geom_point(aes(color = as.factor(SERIE))) +
    geom_line(data = data %>% select(-SERIE, -pass) %>% group_by(CONC_LEVEL) %>% summarize_all(~ mean(.x, na.rm = TRUE)), aes(x = CONC_LEVEL, y = TVL_ABS), color = "blue") +
    geom_line(data = data %>% select(-SERIE, -pass) %>% group_by(CONC_LEVEL) %>% summarize_all(~ mean(.x, na.rm = TRUE)), aes(x = CONC_LEVEL, y = TVH_ABS), color = "blue") +
    geom_smooth(method = "lm", se = FALSE, color = "black", formula = y ~ x, linewidth=0.5) +
    theme_light() +
    ggtitle(paste(string)) +
    scale_color_viridis(discrete = TRUE) +
    xlab(paste("Concentration levels", cUNIT)) +
    ylab(paste("Calculated concentrations ", cUNIT)) +
    labs(color = "Serie")
  return(p)
}


# test
# for (x in 1:length(listVALID)) {
#   print(ggplotly(plot_linearity(as.data.frame(listVALID[[x]]), names(listVALID[x]))))
# }

linear_model_stat <- function(data) {
  dLIN<-data.frame(data[,2],data[,4])
  names(dLIN)[1]<-"x"
  names(dLIN)[2]<-"y"
  linear_model <- lm(y~x,dLIN)
  return(linear_model)
}
#linear_model_stat(listVALID[["LIN"]])

anova_model_stat <- function(data) {
  
  bp <- bptest(linear_model_stat(data))
  return(bp)
}
#anova_model_stat(listVALID[["LIN"]])
