#aggregateData.R  from S-plus
# initialize y and t
y=0
t=0
n = 1:length(ytmp)
ttmp = as.Date(ttmp)
monthnums = monthNum(ttmp)

# start aggregation by "D"ay, "M"onth, "S"eason, or "Y"ear

if (mode == "D") {
  t.julian <- julian(monthnums,fac2num(days(ttmp)),fac2num(years(ttmp)))
  yagg = aggregate(ytmp,t.julian,FUN=mean) # averages repeated measurements
  y = yagg$x
  t <- unique(ttmp)
  tdd <- fac2num(years(t))+(yeardays(t)-0.5)/365
  tdd.perf <- seq(min(tdd),max(tdd),(1.0/365.0))  # Need time vector with no gaps
  dates.agg <- ttmp
  t.out <- t
  n.pds <- 365
  mode.name = "Day"
}

if (mode == "M") {
  mvec <- months(ttmp)
  yvec <- year(ttmp)
  agg.field <- "month.numeric"
  yagg <- aggregate(ytmp,list(mvec,yvec),FUN=mean)
#   yagg <- aggregate(ytmp,list(ttmp),FUN=mean) trying to figure out a better way to aggregate. 
  names(yagg)[1:3] = c("month","year", "x")
  y = yagg$x
  month.numeric <- fac2num(match(as.vector(yagg$month),month.name))
    #t <- dates(c(mvec,1,yvec), format = "mm/dd/yyyy") # **** doesnt' work
  tdd <- fac2num(yagg$year) + match(as.vector(yagg$month),month.name)/12
  t = tdd
  tdd.perf <- seq(min(tdd),max(tdd),(1.0/12.0))  # Need time vector with no gaps  
  y.perf <- y[match(round(tdd.perf,2),round(tdd,2))]  	#  Match time vector (no gaps) with observed aggregated values
  n.pds <- 12
  seasonsvec <- match(as.vector(yagg$month),month.abb)
  dd <- paste(yagg$month,"/","1","/",fac2num(yagg$year))
  dates.agg <- timeDate(ttmp)
  t.out <- yagg[1:2]
  n.pds <- 12
  mode.name = "Month"
  
  # Added 7/1/13 by NJohnson (had to sort by time in order to perform kensen function)
  data.00 = data.frame(t,y)
  sortBy(t, dataFrame = data.00, inclusive = T)
  t = data.00$t
  y = data.00$y
}

if (mode == "S")  { 
  mvec <- months(ttmp)
  qvec <- seasons[match(mvec,month.name)]
  agg.field <- "season.numeric"
  m.num.vec <- match(mvec,month.name)
  yvec <- fac2num(year(ttmp))
  yvec.old <- fac2num(year(ttmp))
  if (seasons[1]==seasons[12]) {
    loop.month <- seasons[1]
    ct <- 12
    while (seasons[ct]==loop.month) {
      yvec[m.num.vec==ct] <- yvec[m.num.vec==ct]+1
      ct=ct-1
    }
  }
  yagg <- aggregate(ytmp,list(qvec,yvec),FUN=mean) 
  magg <- aggregate(m.num.vec,list(qvec,yvec),FUN=min) 
  names(yagg)[1:3] = c("season","year","x") 
  names(magg)[1:3] = c("season","year","x") 
  y <- yagg$x
  season.numeric <- fac2num(yagg$season)
  seasonsvec <- yagg$season
  mm <- fac2num(magg$x) ## use month corresponding to first month of each season
  tdd <- fac2num(yagg$year) + (mm -1) / 12
  t=tdd 
  tdd.perf <- seq(min(tdd),max(tdd),1/(max(seasons)))  # Need time vector with no gaps
  y.perf <- y[match(round(tdd.perf,2),round(tdd,2))]  	#  Match time vector (no gaps) with observed aggregated values
  n.pds <- max(seasons)
  dd <- paste(mm,"/","1","/",fac2num(yagg$year), sep = "") # date is set to first day of each season
  dates.agg <- timeDate(dd)
  t.out <- yagg[1:2]
  n.pds <- max(seasons)
  mode.name = "Season"
}

if (mode == "Y")  { 
  yvec <- year(ttmp)
  if(y.label == "Rainfall in"){
    yagg <- aggregate(ytmp,by=list(yvec),FUN = "sum")
  } else {yagg <- aggregate(ytmp,by=list(yvec),FUN = "mean")}
  names(yagg)[c(1,2)] = c("year", "x")
  y <- yagg$x 
  tdd <- fac2num(yagg$year) 
  t=tdd 
  tdd.perf <- seq(min(tdd),max(tdd),1)  # Need time vector with no gaps
  dd <- paste("1","/","1","/",fac2num(yagg$year), sep = "")
  dates.agg <- timeDate(dd, format = "%m/%d/%Y")
  t.out <- as.array(tdd)
  n.pds <- 1
  mode.name = "Year"
  y.perf <- y[match(round(tdd.perf,2),round(tdd,2))]  	#  Match time vector (no gaps) with observed aggregated values
#   y.perf <- interpNA(y.perf0)	# Use average value to fill missing observations 
}

# Added 7/1/13 by NJohnson (had to sort by time in order to perform kensen function)
data.00 = data.frame(t,y)
sortBy(t, dataFrame = data.00, inclusive = T)
t = data.00$t
y = data.00$y

# Linear regression of y vs time
lm.yt <- lm(y ~ tdd)

# plot(lm.yt)