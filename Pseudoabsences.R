source("PA_paper/LoadPackages_PA.R")
source("PA_paper/PseudoFunctions.R")

out.dir='output.Predicted/'
in.csv='tags.csv'

# read in tag data
results<-readRDS("20231128fit_ssm_manualALL.Rdata")
filtered_results <- list()
for (i in 1:length(results)) {
  result <- results[[i]]
  if (!is.null(result)) {
    filtered_results[[result$id]] <- result
  }
}
floc <- lapply(filtered_results, grab, what = "predicted")
floc.df<-do.call(rbind.data.frame, floc)

tags = floc.df

#removing tracks that departed the area, indicating non-foraging
depart.ids<-factor(c("K93087*_96777_0", "K93087*_96777_1", "K93087*_96777_2", "K93087*_96777_4", "K93087*_96777_6", "K93087*_96777_8", "QA64318*_133758_0"))

FGPS.stay<- tags%>% filter(!g>0.8) |> 
  filter(!(grepl("K93087*", id) & date > ymd_hms('2010-09-20 23:13:44'))) |> 
  filter(!(grepl("QA33394", id) & date > ymd_hms('2017-08-31 13:37:28'))) |> 
  filter(!(grepl("QA64318", id) & date > ymd_hms('2017-08-31 13:37:28'))) |> 
  droplevels()

  
FGPS.stay |>   ggplot(aes(x=x, y=y))+
  geom_point()
write.csv(FGPS.stay, file="20231129SSMoutput.csv")

tags<-FGPS.stay
#tags$date <- paste(gsub(" ","",paste(tags$Month,tags$Day,tags$Year,sep="/"), fixed=TRUE), " 12:00:00 GMT")
tags$dTime = as.POSIXct(strptime(as.character(tags$date), "%Y-%m-%d %H:%M", tz="GMT"))

# clear sim.alltags if exists in workspace
if (exists('sim.alltags')) rm(sim.alltags)
if (exists('sim.allbufftags')) rm(sim.allbufftags)
if (exists('sim.allbacktags')) rm(sim.allbacktags)

#tags to be run:
#tagsleft<-unique(tags$eventid)[7:64]

tags$tag<-tags$id
ids<-unique(tags$tag)
tags$long<-tags$lon
#first.tag<-tags |> filter(tag==first(unique(tag)))

for (tagid in unique(tags$tag)[147:length(unique(tags$tag))]){   
  #Simulate CRW
  sim.alldata <- createCRW(tags, tagid, n.sim=100)
  #Simulate background
  sim.background.alldata <- createbackgroundabsence(tags, tagid, n.sim=100)  
}  # end for (tagid in unique(tags$ptt)){

#saveRDS(sim.background.alldata, file="background.Fitted.Rds")
#saveRDS(sim.alldata, file="CRW.Fitted.Rds")
#saveRDS(sim.background.alldata, "background.Predicted.Rds")
#saveRDS(sim.alldata, "CRW.Predicted.Rds")
