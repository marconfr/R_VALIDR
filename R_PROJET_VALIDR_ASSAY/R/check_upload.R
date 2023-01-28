# This function should check that the uploaded file meet requirements i.e.
# All variable types are corrects
# Sufficient data samples are entered in the files
dfTYPE <- data.frame(
  "ID" = character(),
  "TYPE" = character(),
  "SERIE" = character(),
  "LEVEL" = numeric(),
  "REPLICATE" = numeric(),
  "CONC_LEVEL"=numeric(),
  "SIGNAL" = numeric())

check_upload <- function(dfUPLOAD){
  if(isFALSE(janitor::compare_df_cols_same(dfTYPE,dfUPLOAD, bind_method = "rbind"))){
    return(FALSE)
  } else if(anyNA(dfUPLOAD)){
    return(FALSE)
  } else {
    return(TRUE)
  }
}
  