source("PA_paper/LoadPackages_PA.R")
source("PA_paper/PseudoFunctions.R")

out.dir='PA_paper/output.Predicted2024/'
in.csv='tags.csv'

# read in tag data
results<-readRDS("PA_paper/20240704ssm.Rds") # replace with "20240704ssm_24h.Rdata" for 24h individuals

floc <- grab(results, what = "predicted")
#floc.df<-do.call(rbind.data.frame, floc)

tags = floc


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

for (tagid in unique(tags$tag)[2:length(unique(tags$tag))]){   
  #Simulate CRW
  sim.alldata <- createCRW(tags, tagid, n.sim=100)

  #Simulate background
  sim.background.alldata <- createbackgroundabsence(tags, tagid, n.sim=100)  
}  # end for (tagid in unique(tags$ptt)){

# saveRDS(sim.background.alldata, file="background.Fitted.Rds")
# saveRDS(sim.alldata, file="CRW.Fitted.Rds")
# saveRDS(sim.background.alldata, "background.Predicted.Rds")
# saveRDS(sim.alldata, "CRW.Predicted.Rds")