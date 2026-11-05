# Synthetic test producer, not the real Biomass_core module.
defineModule(sim,list(
  name="Biomass_core",description="Synthetic event-order fixture",keywords="test",
  authors=person("Project","Contributors",role=c("aut","cre")),
  childModules=character(),version=list(Biomass_core=numeric_version("0.0.1")),
  timeframe=as.POSIXlt(c(NA,NA)),timeunit="year",citation=list(),documentation=list(),
  reqdPkgs=list("data.table","terra"),
  parameters=rbind(defineParameter("successionTimestep","numeric",2,0,NA,"Test interval"),
                   defineParameter("removeInitialZeroBiomass","logical",FALSE,NA,NA,"Test initial growth ordering")),
  inputObjects=rbind(expectsInput("cohortData","data.table","Fixture cohorts",sourceURL=NA_character_)),
  outputObjects=rbind(createsOutput("cohortData","data.table","Updated fixture cohorts"))
))
doEvent.Biomass_core <- function(sim,eventTime,eventType,debug=FALSE) {
  if(eventType=="init") {
    if (P(sim)$removeInitialZeroBiomass) {
      sim <- scheduleEvent(sim,start(sim),"Biomass_core","mortalityAndGrowth",eventPriority=6)
      sim <- scheduleEvent(sim,start(sim),"Biomass_core","summaryBGM",eventPriority=8.25)
    }
    sim <- scheduleEvent(sim,start(sim)+P(sim)$successionTimestep,"Biomass_core","update",eventPriority=7)
  } else if (eventType == "mortalityAndGrowth") {
    dead <- sim$cohortData[B <= 0, pixelGroup]
    sim$cohortData <- sim$cohortData[B > 0]
    sim$pixelGroupMap[terra::values(sim$pixelGroupMap) %in% dead] <- 0
  } else if (eventType == "summaryBGM") {
    sim$fixtureSummaryCompleted <- TRUE
  } else {
    species <- if(time(sim)==start(sim)+P(sim)$successionTimestep) "Popu_tre" else "Lari_lar"
    sim$cohortData$speciesCode <- if (is.factor(sim$cohortData$speciesCode)) factor(rep(species, nrow(sim$cohortData))) else rep(species, nrow(sim$cohortData))
    nextTime <- time(sim)+P(sim)$successionTimestep
    if(nextTime<=end(sim)) sim <- scheduleEvent(sim,nextTime,"Biomass_core","update",eventPriority=7)
  }
  invisible(sim)
}
