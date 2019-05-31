#libraries----
library(jsonlite)
library(data.table)
library(ggplot2)
library(stringr)
library(scales)
#constants----
# heroes <- c("ana"="support","bastion"="defense", "brigitte"="support", "doomfist"="dps", "dva"="tank", "genji"="dps","hanzo"="defense", "junkrat"="defense",
# 						"lucio"="support", "mccree"="dps", "mei"="defense", "mercy"="support", "moira"="support", "orisa"="tank", "pharah"="dps", "reaper"="dps",
# 						"reinhardt"="tank", "roadhog"="tank", "soldier76"="dps", "sombra"="dps","symmetra"="support", "tracer"="dps", "torbjorn"="defense",
# 						"widowmaker"="defense", "winston"="tank", "wrecking ball"="tank", "zarya"="tank", "zenyatta"="support")

heroes <- c("ana"="support", "ashe"= "damage", "bastion"="damage", "baptiste"="support", "brigitte"="support", "doomfist"="damage", "dva"="tank", "genji"="damage","hanzo"="damage", 
            "junkrat"="damage", "lucio"="support", "mccree"="damage", "mei"="damage", "mercy"="support", "moira"="support", "orisa"="tank", 
            "pharah"="damage", "reaper"="damage", "reinhardt"="tank", "roadhog"="tank", "soldier76"="damage", "sombra"="damage","symmetra"="damage", 
            "tracer"="damage", "torbjorn"="damage", "widowmaker"="damage", "winston"="tank", "wrecking_ball"="tank", "zarya"="tank", "zenyatta"="support")

heroColors <- c("ana"="#6a84ab", "ashe"= "#615d5c", "bastion"="#5a7250", "baptiste"="#4ba9c4", "brigitte"="#bf6b63", "doomfist"="#81493e", "dva"="#e382bc", "genji"="#95f034","hanzo"="#bab17f",
									 "junkrat"="#ebb643", "lucio"="#77bf41", "mccree"="#b15050", "mei"="#68a6e8", "mercy"="#f3eab7", "moira"="#7e3345", "orisa"="#398031",
									 "pharah"="#2968bc", "reaper"="#793343", "reinhardt"="#929b9c", "roadhog"="#ae6e16", "soldier76"="#636e89", "sombra"="#6f4fb2",
									 "symmetra"="#8dbac9", "tracer"="#dc8f35", "torbjorn"="#b98283", "widowmaker"="#a0a1b5", "winston"="#9799b2", 
									 "wrecking_ball"="#dc8f36", "zarya"="#ed79b0", "zenyatta"="#f0e376")
DamageHeroes 		<- names(heroes[which(heroes=="damage")])
TankHeroes		<- names(heroes[which(heroes=="tank")])
SupportHeroes	<- names(heroes[which(heroes=="support")])

# CompStack1 <- c("HelpX-2656", "McLovin-22252", "SatyrSaint-2705", "Shodrei-2156", "Simply-21778", "Dumbeldore-2156", "Emanuelo-21306")
# 
# Me <- c("Wonderbrah-21110")


#functions----
ChooseFromFactorLevels <- function(Choices, prompt="Selected from this list:", ExcludeFrom=c(), ReturnIndex=T){
	Chosen <- F
	while(!Chosen){
		message(prompt)
		if (!("factor" %in% class(Choices))) { Choices <- factor(Choices) }
		ChoiceLevels <- levels(Choices)
		for (i in 1:length(ChoiceLevels)) {
			if(ReturnIndex) { ExcludeValue <- i } else { ExcludeValue <- ChoiceLevels[i]}
			if(ExcludeValue %in% ExcludeFrom) { next; }
			message(paste(i, ChoiceLevels[i], sep=" "))
		}
		groupIndex <- NA;
		tryCatch(
			expr = {groupIndex <- as.numeric(readline("Select a group number and confirm with enter: ")); Chosen <- T; },
			error = function(err) { message(paste0("Error: ", err,
																						 ". Likely not a valid number. Please select from the indices offered.")); Chosen <- F; },
			warning = function(wrn) { message(paste0("Warning: ", wrn,
																							 ". Likely not a valid number. Please select from the indices offered.")); Chosen <- F; }
		)
		if (is.na(groupIndex) || groupIndex <= 0 || groupIndex > length(ChoiceLevels)){
			message("Invalid value. Select from the numerical indices offered."); Chosen <- F; }
	}
	if(!ReturnIndex){ groupIndex <- ChoiceLevels[groupIndex] }
	return(groupIndex)
}
ReorderColumns<-function(DT, LeadingCols){
	clnms <- colnames(DT)
	if (!(all(union(LeadingCols, clnms) %in% clnms))) {
		stop("One or more LeadingCols are not part of colnames - cannot assign new columns or change names, only reorder")
	}
	setcolorder(DT, c(LeadingCols, clnms[!(clnms %in% LeadingCols)]))
}
OWStatTableAddAndRBind<-function(DTQP, DTCP, TargetDT, player, hero=NA){
  if (nrow(DTQP) == 0 & nrow(DTCP) == 0) { return(data.table()) }
  #DTQP=StatsQP.OS; DTCP=StatsComp.OS; TargetDT=PlayerStats$OverallStats; player="Bartie"
	if(!is.null(DTQP)) {
		cln <- colnames(DTQP)
		cln <- cln[!(cln %in% c("tier", "avatar", "rank_image"))] #HACK
		for (col in cln) if (!(class(DTQP[,get(col)])=="(list)")) { set(DTQP, j = col, value = as.numeric(DTQP[[col]])) }
		DTQP[,Mode:="QuickPlay"]
		DTQP[,Player:=player]
		if (!is.na(hero)){ DTQP[,Hero:=hero] }
	}

	if(!is.null(DTCP)) {
		cln <- colnames(DTCP)
		cln <- cln[!(cln %in% c("tier", "avatar", "rank_image"))] #HACK
		for (col in cln) if (!(class(DTCP[,get(col)])=="(list)")) { set(DTCP, j = col, value = as.numeric(DTCP[[col]])) }
		DTCP[,Mode:="Competitive"]
		DTCP[,Player:=player]
		if (!is.na(hero)) { DTCP[,Hero:=hero] }
	}
	if(!is.null(DTQP)) { TargetDT <- rbind(TargetDT, DTQP, fill=T)}
	if(!is.null(DTCP)) {TargetDT <- rbind(TargetDT, DTCP, fill=T)	}
	return(TargetDT)
}
ParsePlayerStats <- function(ChosenFile=NA, region="eu"){
  #region="eu"; ChosenFile="./190424/190424_LGBTracer-2875.json"
	PlayerStats							<- list()
	PlayerStats$GeneralStats<- data.table()
	PlayerStats$HeroAverages<- data.table()
	PlayerStats$HeroSpecific<- data.table()
	PlayerStats$Playtime 		<- data.table()
	PlayerStats$OverallStats<- data.table()
	PlayerStats$OverallAverages<- data.table()
	PlayerStats$LifetimeStats<- data.table()

	if(is.na(ChosenFile)) {
		setwd("E:/Users/User/Projects/00 Finished/OWStats Team Trolltel/")
		AvailableFiles<- list.files(pattern = ".json", recursive = T)
		ChosenFile 		<- ChooseFromFactorLevels(AvailableFiles, "Choose file to extract:", ReturnIndex = F)
		rm(AvailableFiles)
	}
	PlayerName		<- str_match(ChosenFile, "\\d{6}_(\\w+)-\\d{4,6}\\.json")[,2]

	#RawData 			<-stream_in(file(ChosenFile))
	RawData 			<-fromJSON(ChosenFile)
	if (region=="eu") { RawData 			<-RawData$eu }	else { RawData 			<-RawData$us }

	#Parse per-hero stats----
	HeroData 			<- RawData$heroes
	HeroStats 		<- HeroData$stats
	HeroStats.QP	<- HeroStats$quickplay
	HeroStats.CP	<- HeroStats$competitive
	
	for (hero in names(heroes)) {
	  #hero="mercy"
		if (hero %in% names(HeroStats.CP)) { 
		  DTCP <- getElement(HeroStats.CP, hero)
		  subset.cp.gs		<- data.table(t(getElement(DTCP, "general_stats")))          
		  subset.cp.rs		<- data.table(t(getElement(DTCP, "rolling_average_stats")))    
		} else {
		  subset.cp.gs		<- data.table()
		  subset.cp.rs		<- data.table()
		}
		
	  if (hero %in% names(HeroStats.QP)) { 
	    DTQP <- getElement(HeroStats.QP, hero)
	    subset.qp.gs		<- data.table(t(getElement(DTQP, "general_stats")))          
	    subset.qp.rs		<- data.table(t(getElement(DTQP, "rolling_average_stats")))
	  } else { 
	    subset.qp.gs <- data.table()
	    subset.qp.rs <- data.table()
	  }
		PlayerStats$GeneralStats <- OWStatTableAddAndRBind(subset.qp.gs, subset.cp.gs, PlayerStats$GeneralStats, PlayerName, hero)
		PlayerStats$HeroAverages <- OWStatTableAddAndRBind(subset.qp.rs, subset.cp.rs, PlayerStats$HeroAverages, PlayerName, hero)
	}

	if ("damage_done" %in% colnames(PlayerStats$GeneralStats)) { setnames(PlayerStats$GeneralStats, "damage_done", "all_damage_done")}

	ReorderColumns(PlayerStats$GeneralStats, c("Player", "Hero", "Mode")) #, "shots_fired", "shots_hit"
	ReorderColumns(PlayerStats$HeroAverages, c("Player", "Hero", "Mode"))
	#DBG1 <- PlayerStats$GeneralStats
	#DBG2 <- PlayerStats$HeroAverages
	#DBG3 <- PlayerStats$HeroSpecific
	rm(list = grep("^(?=^subset)", ls(), value = T, perl = T)) #removes ONLY items ENDING in .FIBlood
	rm(HeroStats, HeroStats.QP, HeroStats.CP, hero, DTCP, DTQP)

	#Parse per-hero playtime ----
	HeroPlayTime 				<- HeroData$playtime
	PlayerStats$Playtime <- OWStatTableAddAndRBind(data.table(t(HeroPlayTime$quickplay)), data.table(t(HeroPlayTime$competitive)),
																								 PlayerStats$Playtime, PlayerName)

	PlayerStats$Playtime <- melt.data.table(PlayerStats$Playtime, id.vars = c("Player", "Mode"))
	colnames(PlayerStats$Playtime) <- c("Player", "Mode", "Hero", "Time(Hours)")
	rm(HeroPlayTime, HeroData)
	#DBG4 <- PlayerStats$Playtime

	#Parse other stats----
	pStats 				<- RawData$stats
	#There's a 'competitive' datapoint, but it's just a bool to check mode...
	StatsComp 					<- pStats$competitive 	#average_stats overall_stats game_stats
	StatsComp.AS 				<- data.table(t(StatsComp$average_stats))
	StatsComp.OS 				<- data.table(t(StatsComp$overall_stats))
	#StatsComp.OS[,realLevel:=(ifelse(!is.null(prestige), as.numeric(prestige)*100, 0))+as.numeric(level)]
	StatsComp.GS 				<- data.table(t(StatsComp$game_stats))

	StatsQP							<- pStats$quickplay 		#average_stats overall_stats game_stats
	StatsQP.AS 					<- data.table(t(StatsQP$average_stats))
	StatsQP.OS 					<- data.table(t(StatsQP$overall_stats))
	#StatsQP.OS[,realLevel:=(as.numeric(prestige)*100)+as.numeric(level)]
	StatsQP.GS 					<- data.table(t(StatsQP$game_stats))
	rm(StatsComp, StatsQP, pStats)
  
	StatsQP.OS[,prestige:=NULL]
	StatsComp.OS[,prestige:=NULL]
	PlayerStats$OverallStats 			<- OWStatTableAddAndRBind(StatsQP.OS, StatsComp.OS, PlayerStats$OverallStats, PlayerName)
	
	#PlayerStats$OverallAverages 	<- OWStatTableAddAndRBind(StatsQP.AS, StatsComp.AS, PlayerStats$OverallAverages, PlayerName)
	PlayerStats$LifetimeStats 		<- OWStatTableAddAndRBind(StatsQP.GS, StatsComp.GS, PlayerStats$LifetimeStats, PlayerName)

	ReorderColumns(PlayerStats$OverallStats, c("Player", "Mode"))
	#ReorderColumns(PlayerStats$OverallAverages, c("Player", "Mode"))
	ReorderColumns(PlayerStats$LifetimeStats, c("Player", "Mode"))

	return(PlayerStats)
	rm(ChosenFile, RawData)
	}
ParseAllStats <- function(StatsDate=NA, Players=AllPlayers, region="eu"){
  #Players = c("Bartie-2639"); StatsDate="181023"; region="eu"
	setwd("E:/Users/User/Projects/00 Finished/OWStats Team Trolltel/")
	if (is.na(StatsDate)){
		AllFiles 	<- str_replace_all(list.dirs(), "\\./", "")
		FileDates <- unique(str_match(AllFiles, "(\\d{6})")[,2])
		FileDates <- FileDates[!(is.na(FileDates))]
		StatsDate <- ChooseFromFactorLevels(FileDates, "Select date to analyse OW stats for:", ReturnIndex = F)
	}
	AvailableFiles <- data.table(filename=dir(path = paste0("./", StatsDate, "/"), pattern = paste0(StatsDate,"_.*\\.json")))
	AvailableFiles[,PlayerName := str_match(filename, "\\d{6}_(\\w+-\\d{4,5})\\.json")[,2]]
	AvailableFiles[,IsChosen := (PlayerName %in% Players)]
	ChosenFiles <- AvailableFiles[IsChosen == TRUE, filename]

	TeamStats							<- list()
	TeamStats$GeneralStats<- data.table()
	TeamStats$HeroAverages<- data.table()
	TeamStats$HeroSpecific<- data.table()
	TeamStats$Playtime 		<- data.table()
	TeamStats$OverallStats<- data.table()
	TeamStats$OverallAverages<- data.table()
	TeamStats$LifetimeStats<- data.table()


	for (afile in ChosenFiles) {
		data <- ParsePlayerStats(paste0("./", StatsDate, "/", afile), region)
		TeamStats$GeneralStats		<- rbind(TeamStats$GeneralStats,   data$GeneralStats	 , fill=T)
		TeamStats$HeroAverages		<- rbind(TeamStats$HeroAverages,   data$HeroAverages	 , fill=T)
		TeamStats$HeroSpecific		<- rbind(TeamStats$HeroSpecific,   data$HeroSpecific	 , fill=T)
		TeamStats$Playtime 				<- rbind(TeamStats$Playtime,       data$Playtime 			 , fill=T)
		TeamStats$OverallStats		<- rbind(TeamStats$OverallStats,   data$OverallStats	 , fill=T)
		TeamStats$OverallAverages	<- rbind(TeamStats$OverallAverages,data$OverallAverages, fill=T)
		TeamStats$LifetimeStats		<- rbind(TeamStats$LifetimeStats,	 data$LifetimeStats	 , fill=T)
	}
	return(TeamStats)
}
ExtractMainByStat <- function(StatName){
  if ( (!StatName %in% colnames(GeneralStats)) ) { stop(paste0("Cannot ExtractMAinByStat(): ", StatName, " is not a column in GeneralStats.")) }
  setnames(GeneralStats, StatName, "TEMP")
  DT = GeneralStats[!is.na(TEMP),.SD[TEMP==max(TEMP), .(Hero, TEMP)], .(Player, Mode)]
  setnames(GeneralStats, "TEMP", StatName)
  setnames(DT, "TEMP", StatName)
  return(DT)
}
completeDT <- function(DT, cols, defs = NULL){
  mDT = do.call(CJ, c(DT[, ..cols], list(unique=TRUE))) 
  #Creates a data.table to Join, over cols, with Unique values, and all value combinations (silent repetition?)
  res = DT[mDT, on=names(mDT)] #joins by names
  if (length(defs)) { res[, names(defs) := Map(replace, .SD, lapply(.SD, is.na), defs), .SDcols=names(defs)] } #idk
  #seems to use a call to Map() to call replace() on all .SDs where is.na, replaceing the present value with that in defs?
  #whilst using .SDcols to restrict the columns being edited to those selected
  return(res)
} 
AllStatsOverTime <- function(region="eu"){
  AllFiles 	<- list.files(path = "./", pattern = "\\d{6}_.*\\.json", recursive = T)

  TimeTeamStats							<- list()
  TimeTeamStats$GeneralStats<- data.table()
  TimeTeamStats$Playtime 		<- data.table()
  TimeTeamStats$OverallStats<- data.table()
  TimeTeamStats$LifetimeStats<-data.table()
  
  for (file in AllFiles){
    PlayerStats							<- list()
    PlayerStats$GeneralStats<- data.table()
    PlayerStats$Playtime 		<- data.table()
    PlayerStats$OverallStats<- data.table()
    PlayerStats$LifetimeStats<-data.table()
    
    date = as.Date.character(str_match(file, "(\\d{6})")[,2], "%y%m%d")
    PlayerName = str_match(file, "\\d{6}_(\\w+)-\\d{4,6}\\.json")[,2]
    RawData 			<-fromJSON(file)
    if (region=="eu") { RawData 			<-RawData$eu }	else { RawData 			<-RawData$us }
    
    #Parse per-hero stats----
    HeroData 			<- RawData$heroes
    HeroStats 		<- HeroData$stats
    HeroStats.QP	<- HeroStats$quickplay
    HeroStats.CP	<- HeroStats$competitive
    
    for (hero in names(heroes)) {
      if (hero %in% names(HeroStats.CP)) { DTCP <- getElement(HeroStats.CP, hero)} else { DTCP <- NA}
      if (hero %in% names(HeroStats.QP)) { DTQP <- getElement(HeroStats.QP, hero)} else { DTQP <- NA}
      
      if (length(DTCP)>1) {subset.cp.gs		<- data.table(t(getElement(DTCP, "general_stats"))) } else { subset.cp.gs <- NULL }
      if (length(DTCP)>1) {subset.cp.as		<- data.table(t(getElement(DTCP, "average_stats"))) } else { subset.cp.as <- NULL }
      if (length(DTCP)>1) {subset.cp.hs		<- data.table(t(getElement(DTCP, "hero_stats"))) 		} else { subset.cp.hs <- NULL }
      
      if (length(DTQP)>1) {subset.qp.gs		<- data.table(t(getElement(DTQP, "general_stats"))) } else { subset.qp.gs <- NULL }
      if (length(DTQP)>1) {subset.qp.as		<- data.table(t(getElement(DTQP, "average_stats"))) } else { subset.qp.as <- NULL }
      if (length(DTQP)>1) {subset.qp.hs		<- data.table(t(getElement(DTQP, "hero_stats"))) 		} else { subset.qp.hs <- NULL }
      
      PlayerStats$GeneralStats <- OWStatTableAddAndRBind(subset.qp.gs, subset.cp.gs, PlayerStats$GeneralStats, PlayerName, hero)
      PlayerStats$GeneralStats[,date:=date]
      PlayerStats$HeroAverages <- OWStatTableAddAndRBind(subset.qp.as, subset.cp.as, PlayerStats$HeroAverages, PlayerName, hero)
      PlayerStats$HeroAverages[,date:=date]
      PlayerStats$HeroSpecific <- OWStatTableAddAndRBind(subset.qp.hs, subset.cp.hs, PlayerStats$HeroSpecific, PlayerName, hero)
      PlayerStats$HeroSpecific[,date:=date]
    }
    
    if ("damage_done" %in% colnames(PlayerStats$GeneralStats)) { setnames(PlayerStats$GeneralStats, "damage_done", "all_damage_done")}
    
    ReorderColumns(PlayerStats$GeneralStats, c("Player", "Hero", "Mode")) #, "shots_fired", "shots_hit"
    ReorderColumns(PlayerStats$HeroAverages, c("Player", "Hero", "Mode"))
    ReorderColumns(PlayerStats$HeroSpecific, c("Player", "Hero", "Mode"))
    #DBG1 <- PlayerStats$GeneralStats
    #DBG2 <- PlayerStats$HeroAverages
    #DBG3 <- PlayerStats$HeroSpecific
    rm(list = grep("^(?=^subset)", ls(), value = T, perl = T)) #removes ONLY items ENDING in .FIBlood
    rm(HeroStats, HeroStats.QP, HeroStats.CP, hero, DTCP, DTQP)
  
    HeroPlayTime 				<- HeroData$playtime
    PlayerStats$Playtime <- OWStatTableAddAndRBind(data.table(t(HeroPlayTime$quickplay)), data.table(t(HeroPlayTime$competitive)),
                                                PlayerStats$Playtime, PlayerName)
    
    PlayerStats$Playtime <- melt.data.table(PlayerStats$Playtime, id.vars = c("Player", "Mode"))
    colnames(PlayerStats$Playtime) <- c("Player", "Mode", "Hero", "Time(Hours)")
    rm(HeroPlayTime, HeroData)
    pStats 				<- RawData$stats
    rm(RawData)
    
    StatsComp 					<- pStats$competitive 	#average_stats overall_stats game_stats
    StatsComp.AS 				<- data.table(t(StatsComp$average_stats))
    StatsComp.OS 				<- data.table(t(StatsComp$overall_stats))
    
    StatsComp.OS[,realLevel:=(as.numeric(ifelse("prestige" %in% colnames(StatsComp.OS), prestige, 0))*100)+as.numeric(ifelse("level" %in% colnames(StatsComp.OS), level, 0))]
    
    StatsComp.GS 				<- data.table(t(StatsComp$game_stats))
    
    StatsQP							<- pStats$quickplay 		#average_stats overall_stats game_stats
    StatsQP.AS 					<- data.table(t(StatsQP$average_stats))
    StatsQP.OS 					<- data.table(t(StatsQP$overall_stats))
    StatsQP.OS[,realLevel:=(as.numeric(ifelse("prestige" %in% colnames(StatsQP.OS), prestige, 0))*100)+as.numeric(ifelse("level" %in% colnames(StatsQP.OS), level, 0))]
    StatsQP.GS 					<- data.table(t(StatsQP$game_stats))
    rm(StatsComp, StatsQP, pStats)
    
    PlayerStats$OverallStats 			<- OWStatTableAddAndRBind(StatsQP.OS, StatsComp.OS, PlayerStats$OverallStats, PlayerName)
    PlayerStats$OverallAverages 	<- OWStatTableAddAndRBind(StatsQP.AS, StatsComp.AS, PlayerStats$OverallAverages, PlayerName)
    PlayerStats$LifetimeStats 		<- OWStatTableAddAndRBind(StatsQP.GS, StatsComp.GS, PlayerStats$LifetimeStats, PlayerName)
    
    PlayerStats$Playtime[,date:=date]
    PlayerStats$OverallStats[,date:=date]
    PlayerStats$OverallAverages[,date:=date]
    PlayerStats$LifetimeStats[,date:=date]
    
    TimeTeamStats$GeneralStats<- rbind( TimeTeamStats$GeneralStats, PlayerStats$GeneralStats, fill = T)
    TimeTeamStats$Playtime 		<- rbind( TimeTeamStats$Playtime, PlayerStats$Playtime, fill = T)
    TimeTeamStats$OverallStats<- rbind( TimeTeamStats$OverallStats, PlayerStats$OverallStats, fill = T)
    TimeTeamStats$LifetimeStats<-rbind( TimeTeamStats$LifetimeStats, PlayerStats$LifetimeStats, fill = T)
  }
  return (TimeTeamStats)
}
#Read and process OWAPI data----
setwd("E:/Users/User/Projects/00 Finished/OWStats Team Trolltel/")
OWTeam <- ParseAllStats(Players = c("Wonderbrah-21110", "LGBTracer-2875", "DorialD-2401"))

GeneralStats		<- OWTeam$GeneralStats
GeneralStats[,games_tied:=games_played-(games_won+games_lost)]
HeroAverages		<- OWTeam$HeroAverages
HeroSpecific		<- OWTeam$HeroSpecific
Playtime 				<- OWTeam$Playtime
Playtime[is.na(`Time(Hours)`), `Time(Hours)`:=0]
Playtime[,Hero:=factor(Hero, levels=names(heroes))]
OverallStats		<- OWTeam$OverallStats
OverallAverages	<- OWTeam$OverallAverages
LifetimeStats		<- OWTeam$LifetimeStats
LifetimeStats[,games_tied:=games_played-(games_won+games_lost)]
LifetimeStats[,win_percentage:=games_won/games_played]
TotalPlaytime 	<- Playtime[, .(total_playtime=sum(`Time(Hours)`)), .(Player,Mode)]
TotalPlaytime[is.na(total_playtime), total_playtime:=0]
LifetimeStats 	<- merge(TotalPlaytime, LifetimeStats, by = c("Player", "Mode"))
#rm(OWTeam)

#Medals----
LifetimeStats.Medals <- melt.data.table(LifetimeStats[,.(medals_bronze, medals_gold, medals_silver, Player, Mode)], id.vars=c("Player", "Mode"))
LifetimeStats.Medals[,variable:=factor(variable, levels=c("medals_gold", "medals_silver", "medals_bronze"), labels=c("Gold", "Silber", "Bronze"))]
# setorder(LifetimeStats.Medals, Mode, variable, -value, Player)
ggplot(LifetimeStats.Medals, aes(x=Player, y=value, fill=variable))+geom_bar(position=position_dodge(0.5), stat="identity")+
	facet_grid(.~Mode)+theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1), legend.position = "none")+
	scale_fill_manual(name="Medaillen", values = c("#CD7F32", "#C0C0C0", "#FCD667"))+ylab("Medaillen")
#Playtime----
Playtime_Pct <- merge(Playtime, TotalPlaytime, by=c("Player", "Mode"))
Playtime_Pct[,TimePct := `Time(Hours)`/total_playtime]
Playtime_Pct[,Mode:=factor(Mode, levels = c("QuickPlay", "Competitive"))]
Playtime_Pct[,fHero:=heroes[Hero]]
Playtime_Pct[,fHero:=factor(fHero, levels = c("damage","tank", "support"))]
Playtime_Pct = merge(Playtime_Pct, unique(OverallStats[,.(Player,comprank)]), by="Player")
Playtime_Pct[,Player:=paste0(Player, "[", comprank, "]")]

ggplot(TotalPlaytime[!is.na(total_playtime)], aes(x=Player, y=total_playtime, fill=Mode))+geom_bar(stat="identity")+xlab("Teammitglied")+
  ylab("Spielzeit (in Stunden)")+scale_color_manual()

#Playtime per class; change position_dodge width to 0.5 for a ~cool overlap~----
ggplot(Playtime_Pct, aes(x=fHero, y=TimePct, fill=Hero))+geom_bar(stat="identity", position = position_dodge(width = 01))+
	facet_grid(Player~Mode)+scale_y_continuous(labels = scales::percent)+scale_fill_manual(values = heroColors)+
	theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3), strip.text.y = element_text(angle=0),
				panel.background = element_rect(fill='grey'), panel.grid.minor.y =  element_line(colour="grey"),
				panel.grid.major.x=element_blank())

#Playtime per class. stacked----
ggplot(Playtime_Pct, aes(x=fHero, y=TimePct, fill=Hero))+geom_bar(stat="identity")+ #, position = position_dodge(width = 0.5)
	facet_grid(Player~Mode)+scale_y_continuous(labels = scales::percent)+scale_fill_manual(values = heroColors)+
	theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3), strip.text.y = element_text(angle=0),
				panel.background = element_rect(fill='grey'), panel.grid.minor.y =  element_line(colour="grey"),
				panel.grid.major.x=element_blank())+ggtitle(paste0("Play time per class, hero and mode"))

#Mains----
MainsByTime 		<- Playtime_Pct[,.SD[TimePct==max(TimePct), .(Hero, `Time(Hours)`)], .(Player,Mode)]
rm(Playtime_Pct)

MainsByHeals = ExtractMainByStat("healing_done")

MainsBySoloKills<- ExtractMainByStat("solo_kills")
MainsByCrits<- ExtractMainByStat("critical_hits")
#MainsByCrits<- ExtractMainByStat("scoped_critical_hits")
#MainsByCrits<- ExtractMainByStat("critical_hits")

MainsByAnyKills<- ExtractMainByStat("eliminations_per_life")
#ExtractMainByStat("eliminations_most_in_game")
#ExtractMainByStat("eliminations_per_life")
#ExtractMainByStat("eliminations")

MainsByTHEPOINT	<- ExtractMainByStat("objective_time")

#Comp Win %----
ggplot(LifetimeStats[Mode=="Competitive"], aes(x=reorder(Player, win_percentage), y=win_percentage, fill=win_percentage))+
	geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), legend.position = "none")+
	coord_cartesian(ylim=c(0, 1))+xlab("Player")+ylab("Win rate in Competitive")+scale_y_continuous(labels = scales::percent)

#per hero
WinData = completeDT(GeneralStats[Mode=="Competitive", .(Player, Hero, games_played, games_tied, games_won, win_percentage)], c("Player", "Hero"), defs = c(win_percentage=NA))
WinData[,class:=heroes[Hero]]
WinData = merge(WinData, unique(OverallStats[,.(Player,comprank)]), by="Player")
WinData[,Player := paste0(Player, "[", comprank ,"]")]
ggplot(WinData, aes(x=Player, y=Hero, fill=win_percentage))+geom_raster()+facet_wrap(~class, scales="free_y")+
  scale_fill_gradient(labels=scales::percent, low = "#ff0000", high = "#00ff00")+geom_text(aes(label=paste(games_played, " game(s) played"), color=Hero))+
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.2))+scale_color_manual(values=heroColors, breaks=NULL)

#Healing----
# NuHeals <- LifetimeStats[,.(heals=healing_done_avg_per_10_min),.(Player,Mode)]
# NuHeals[,Mode:=factor(Mode, levels = c("QuickPlay","Competitive"))]
# ggplot(NuHeals, aes(x=reorder(Player, heals), y=heals, fill=Player))+geom_bar(stat="identity")+facet_wrap(~Mode)+xlab("Player")+
# 	ylab("Average Healing per 10 minutes")+theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

Heals <- GeneralStats[Hero %in% c("ana", "lucio", "mercy", "zenyatta")]
Heals <- Heals[,.(TotalHealing=healing_done, TotalTime=time_played), .(Player, Hero, Mode)]
Heals[,healing_per_hour:=TotalHealing/TotalTime]
Heals[,healing_avg_per_10_min:=(healing_per_hour * 6)/1000]
Heals[,healing_weight:=min(max(TotalTime, 8), 24), .(Player, Hero, Mode)]
ggplot(Heals, aes(x=reorder(Player, -healing_avg_per_10_min), y=healing_avg_per_10_min, fill=Hero))+geom_bar(stat="identity")+
	facet_grid(Hero~Mode)+theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
	xlab("Spieler")+ylab("Heilung pro 10 Minuten")+scale_fill_manual(values = heroColors)+
	geom_text(aes(label=format(TotalTime, digits = 1, nsmall = 1, scientific = F), angle=90, hjust=1, size=healing_weight))

#accuracy====
Accuracy = GeneralStats[Hero %in% DamageHeroes, .(Player, Hero, Mode, weapon_accuracy, secondary_fire_accuracy)]
ggplot(Accuracy, aes(x=Hero, y=weapon_accuracy, color=Hero, shape=Mode))+facet_grid(Player~.)+
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+xlab("Hero")+
  ylab("Weapon accuracy")+geom_point(size=4)+scale_color_manual(values = heroColors, breaks=NULL)

Melee = GeneralStats[, .(Player, Hero, Mode, melee_kills, melee_kills_most_in_game, rocket_hammer_melee_accuracy, quick_melee_accuracy)]

ggplot(Melee[Hero=="reinhardt"], aes(x=Player, y=rocket_hammer_melee_accuracy, color=Hero, shape=Mode))+
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+xlab("Hero")+
  ylab("Hammer accuracy")+geom_point(size=2)+scale_color_manual(values = heroColors, breaks=NULL)

ggplot(Melee, aes(x=Hero, y=quick_melee_accuracy, color=Hero, shape=Mode))+facet_grid(Player~.)+
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+xlab("Hero")+
  ylab("Quick melee accuracy")+geom_point(size=2)+scale_color_manual(values = heroColors, breaks=NULL)

ggplot(LifetimeStats, aes(x=Player, y=objective_time, shape=Mode))+ylab("Time spent on the fucking objective")+geom_point(size=2)+
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+xlab("Hero")
  
