mapModelDomainParameters = function(model.area = NA){
if(model.area == "CSM"){
  map.xlim = c(-84,-80.5) # CSM longitute min and max
  map.ylim = c(27.5,30) # CSM latitude min and max
  county.lines = "Y" # display county lines on map ("Y" or "N")
  png.width = 650
  png.height = 550
  leg.loc = "bottomleft"
} else if(model.area == "NFSEG"){
  map.xlim = c(-85,-80) # NFSEG longitute min and max
  map.ylim = c(28.5,33.5) # NFSEG latitude min and max
  county.lines = "N" # display county lines on map ("Y" or "N")
  png.width = 650
  png.height = 700
  leg.loc = "bottomleft"
} else if(model.area == "All_agency"){
  map.xlim = c(-86,-80) # All agency longitute min and max
  map.ylim = c(25,34) # All agency latitude min and max
  county.lines = "N" # display county lines on map ("Y" or "N")
  png.width = 650
  png.height = 1000
  leg.loc = "bottomleft"
} else if(model.area == "GA"){
  map.xlim = c(-85.5,-80.5) # NFSEG longitute min and max
  map.ylim = c(30.3,33.5) # NFSEG latitude min and max
  county.lines = "N" # display county lines on map ("Y" or "N")
  png.width = 650
  png.height = 550
  leg.loc = "topleft"
} else stop("Select appropriate model.area that has and inventory .csv file 
            with appropriate latDD and lonDD columns or create a model domain in the previous if else statements")
return(mapPar = list("map.xlim" = map.xlim, "map.ylim" = map.ylim, "county.lines" = county.lines, "png.width" = png.width, "png.height" = png.height, "leg.loc" = leg.loc))
}