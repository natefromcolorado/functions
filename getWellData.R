getWellData = function(new.data = "Y", filled.Data = "Filled"){
  if(new.data == "Y"){
if(filled.Data =="Original"){
  model.domain.data1 = read.csv(paste("C:/all.agency.mdata.csv", sep = ""), header = TRUE) ### all agency mdata
} else {model.domain.data1 = read.csv(paste(mainDir, "data/Filled_stations_stack_All_agency_MaxCorr_0.9_MatchPair_10_MinRegPeriod3.csv", sep = ""), header = TRUE)} ### all agency mdata
  }
return(model.domain.data1)
}