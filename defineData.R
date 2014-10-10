#defineData
# select data for the specified subset
if(subset.label != "") {  # if a subset label is defined
  eval(parse(text=paste("data.1=data.0[data.0$",dataset.label,"==subset.label,]",sep="")))
}
if(subset.label == "") {	# if no subset label is defined
  data.1 <- data.0
}

eval(parse(text=paste("t.0 <- data.1$",tvar.label,sep="")))
eval(parse(text=paste("y.0 <- data.1$",y.variable.label,sep="")))
data <- data.frame(t.0,y.0)
names(data)[1:2] <- c(tvar.label,y.variable.label)

# the following options are available to handle NAs
data <- na.omit(data)				# omit NAs
#data <- na.gam.replace(data)		# replace NAs with mean of available data
#data <- na.exclude(data)			# same as na.omit
#data <- na.fail(data)				# produces an error if missing values are present

eval(parse(text=paste("ytmp.0=data$", y.variable.label, sep="")))
eval(parse(text=paste("ttmp.0=data$", tvar.label, sep="")))

ttmp.0 = data[,1]
ttmp.0 = as.Date(ttmp.0, format="%m/%d/%Y")
ttmp.0 = timeDate(ttmp.0)
data$date <- as.POSIXct(strptime(data$date, format = "%m/%d/%Y", "GMT"))
ytmp <- ytmp.0[ttmp.0 >= start.date & ttmp.0 < end.date]
ttmp <- ttmp.0[ttmp.0>=start.date & ttmp.0<=end.date]
#data.ts = as.timeSeries(ttmp,ytmp)
if(length(ytmp) < 5) # too few data points
{
  next("Not enough data points (N < 5).  Please verify set/subset name and verify that there is enough data.")
}
