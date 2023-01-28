library(tidyverse)

##### GLOBAL VARIABLES ####
dfTYPE <- data.frame(
  "ID" = character(),
  "TYPE" = character(),
  "DAY" = numeric(),
  "REPLICATE" = numeric(),
  "CONC_LEVEL"=numeric(),
  "SIGNAL" = numeric())

#=========================#
#### GENERATE TEMPLATE ####
#=========================#

# This function generate an XLS template based on the choice made on the UI

generate_template <- function(siTEXT){

  typeMATRIX<-(c("with","without"))
  if(siTEXT=="Active substance alone"){
    nCAL=3
    typeCAL=c("80pc","100pc","120pc")
  } else {
    nCAL=5
    typeCAL=c("LOD","LOQ","3LOQ","MID","HIGH")
  }
# Beware that counters does not reset in intricate loops !
  
  type<-0
  for(type in typeMATRIX){
    i<-1
    while(i < 3 ){
      j<-1
      for(j in typeCAL){
        k<-1
        while(k < 3){
                dfTYPE <- dfTYPE %>% add_row(
                  ID=paste(j, k, i ,sep = "_"),
                  TYPE=type,
                  DAY=i,
                  REPLICATE=k,
                  CONC_LEVEL=NULL,
                  SIGNAL=NULL)
                k<-k+1
              }
        }
    i<-i+1
    }
  }

return(dfTYPE)
}
#Testing
t<-generate_template("Active substance alone")

#### CHECK Upload ####
check_upload <- function(dfUPLOAD){
  if(isFALSE(janitor::compare_df_cols_same(dfTYPE,dfUPLOAD, bind_method = "rbind"))){
    return(FALSE)
  } else if(anyNA(dfUPLOAD)){
    return(FALSE)
  } else {
    return(TRUE)
  }
}
