library(radiotrackingeu)
library(doParallel)
library(mapview)
library(rgdal)
library(doParallel)


#read raw radiotracking data 
raw_data<-readRDS("data/raw/radiotracking_data_2019_06_27.rds")
#read antenna information
receiver<-read.table("N:/data/rteu/mofrts/data/table/Antennas2019.csv", header = TRUE, sep = "\t", dec = ".")

#filter for tagged Myotis bechsteinii (frequency=150.233)
bat<-filter_data_freq(log_dat_1,freq =150233, freq_error = 2 )

#Match timestamps of antennas of the same station for bearing calculation
tm<-time_match_signals(bat,station_time_error = 0.6, progress = F)



#register kernels
registerDoParallel(cores=detectCores())
#calculate bearings
bearings <- doa(tm, receiver,dBLoss=14)

#-->Exclude bearings calculated based on pointing direction of strongest antenna
#If incoming signals are very weak, one antenna might give a rough estimate 
#of the whereabouts of the tagged individua, but these bearings should not be
#included in the triangulation procedure. Bearings based on pointing directions of
#single antennas will be implemented as an option in the bearing function. For now
# all bearings with values equal to pointing directions are excluded from triangulation.
bearings2<-bearings[!(bearings$angle)==0,]
bearings2<-bearings2[!(bearings2$angle)==90,]
bearings2<-bearings2[!(bearings2$angle)==180,]
bearings2<-bearings2[!(bearings2$angle)==270,]

triangulations <- na.omit(triangulate(receiver, na.omit(bearings2), 
                                      time_error_inter_station=1,#Time distance between stations
                                      only_one=F,
                                      angles_allowed=c(30,150),#allowed intersection angle
                                      tm_method="tm",
                                      tri_option="two_strongest",
                                      spar=0.1, 
                                      progress=F))
stopImplicitCluster()


#plot triangulations on a map

df_coords<-cbind(triangulations$pos.X,triangulations$pos.Y)
colnames(df_coords)<-c("x","y")
df_sp<-SpatialPointsDataFrame(coords=df_coords, data=triangulations, proj4string = CRS("+proj=longlat +datum=WGS84"))
mapview(df_sp)
