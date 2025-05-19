



#------------------------------------------------------------------------------#
# Function to generate the template file XLS
#------------------------------------------------------------------------------#

generate_template <- function(nSERIE, nCAL_LEVEL, nCAL_REP, nVAL_LEVEL, nVAL_REP) {
  TEMPLATE_XLXS <- data.frame(
    "ID" = character(),
    "TYPE" = character(),
    "SERIE" = character(),
    "LEVEL" = numeric(),
    "REPLICATE" = numeric(),
    "CONC_LEVEL" = numeric(),
    "SIGNAL" = numeric()
  )
  i <- 1

  while (i < nSERIE + 1) {
    j <- 1
    while (j < nCAL_LEVEL + 1) {
      k <- 1
      while (k < nCAL_REP + 1) {
        TEMPLATE_XLXS <- TEMPLATE_XLXS %>% add_row(
          ID = paste("CAL", i, j, k, sep = "_"),
          TYPE = "CAL",
          SERIE = paste(i),
          LEVEL = j,
          REPLICATE = k,
          CONC_LEVEL = NULL,
          SIGNAL = NULL
        )
        k <- k + 1
      }
      j <- j + 1
    }
    i <- i + 1
  }

  i <- 1

  while (i < nSERIE + 1) {
    j <- 1
    while (j < nVAL_LEVEL + 1) {
      k <- 1
      while (k < nVAL_REP + 1) {
        TEMPLATE_XLXS <- TEMPLATE_XLXS %>% add_row(
          ID = paste("VAL", i, j, k, sep = "_"),
          TYPE = "VAL",
          SERIE = paste(i),
          LEVEL = j,
          REPLICATE = k,
          CONC_LEVEL = NULL,
          SIGNAL = NULL
        )
        k <- k + 1
      }
      j <- j + 1
    }
    i <- i + 1
  }

  return(TEMPLATE_XLXS)
}

#------------------------------------------------------------------------------#
# Function to check upload (same column name, same column type only)
#------------------------------------------------------------------------------#

dfTYPE <- data.frame(
  "ID" = character(),
  "TYPE" = character(),
  "SERIE" = character(),
  "LEVEL" = numeric(),
  "REPLICATE" = numeric(),
  "CONC_LEVEL"=numeric(),
  "SIGNAL" = numeric())

check_upload <- function(dfUPLOAD){
  if (isFALSE(janitor::compare_df_cols_same(dfTYPE, dfUPLOAD, bind_method = "rbind"))) {
    return(FALSE)
  } else if (anyNA(dfUPLOAD)) {
    return(FALSE)
  } else if (length(unique(dfUPLOAD[dfUPLOAD$TYPE == "VAL", ]$LEVEL)) < 2) {
    warning("At least two validation levels are required.")
    return(FALSE)
  } else {
    return(TRUE)
  }
}



#---------------------------------------------------------------#
##### These function check the number of CAL and VAL levels #####
# They should be use to avoid errors in calculation             #
# (e.g. 1 CAL level ?)                                          #
#---------------------------------------------------------------#

check_nb_calibration_level <- function(){
  nb_cal_lvl <<-  length(unique(dfUPLOAD[dfUPLOAD$TYPE == "CAL", ]$LEVEL))
}

check_nb_validation_level <- function(){
  nb_val_lvl <<- length(unique(dfUPLOAD[dfUPLOAD$TYPE == "VAL", ]$LEVEL))
}




