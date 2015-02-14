
# load the crsp weekly data sets and save to the data/ folder for the GARMPFRM package

library(xts)
Sys.setenv(TZ="GMT")
# wherever the data folder is saved
dir <- "~/Downloads/crsp data weekly 1997-2010/"
file <- c("largecap_weekly.csv", "midcap_weekly.csv", "smallcap_weekly.csv", "microcap_weekly.csv")

largecap_weekly <- read.csv(paste(dir, file[1], sep=""), header=TRUE, sep=",", as.is=TRUE)
largecap_weekly <- xts(largecap_weekly[,-1], order.by = as.POSIXct(largecap_weekly[,1], tz = Sys.getenv("TZ"), format="%m/%d/%Y"))

midcap_weekly <- read.csv(paste(dir, file[2], sep=""), header=TRUE, sep=",", as.is=TRUE)
midcap_weekly <- xts(midcap_weekly[,-1], order.by = as.POSIXct(midcap_weekly[,1], tz = Sys.getenv("TZ"), format="%m/%d/%Y"))

smallcap_weekly <- read.csv(paste(dir, file[3], sep=""), header=TRUE, sep=",", as.is=TRUE)
smallcap_weekly <- xts(smallcap_weekly[,-1], order.by = as.POSIXct(smallcap_weekly[,1], tz = Sys.getenv("TZ"), format="%m/%d/%Y"))

microcap_weekly <- read.csv(paste(dir, file[4], sep=""), header=TRUE, sep=",", as.is=TRUE)
microcap_weekly <- xts(microcap_weekly[,-1], order.by = as.POSIXct(microcap_weekly[,1], tz = Sys.getenv("TZ"), format="%m/%d/%Y"))

save(largecap_weekly, midcap_weekly, smallcap_weekly, microcap_weekly, file="data/crsp_weekly.rda")
