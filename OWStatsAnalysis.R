#libraries----
library(jsonlite)
library(data.table)
library(ggplot2)
library(stringr)
library(scales)
#constants----
heroes <- c("ana"="support", "ashe"= "damage", "bastion"="damage", "baptiste"="support", "brigitte"="support", "doomfist"="damage", "dva"="tank", 
            "genji"="damage","hanzo"="damage", "junkrat"="damage", "lucio"="support", "mccree"="damage", "mei"="damage", "mercy"="support", 
            "moira"="support", "orisa"="tank", "pharah"="damage", "reaper"="damage", "reinhardt"="tank", "roadhog"="tank", "soldier76"="damage", 
            "sombra"="damage","symmetra"="damage", "tracer"="damage", "torbjorn"="damage", "widowmaker"="damage", "winston"="tank", "wrecking_ball"="tank", 
            "zarya"="tank", "zenyatta"="support")
heroRoles = data.table(role=heroes, hero=names(heroes))

heroColors <- c("ana"="#6a84ab", "ashe"= "#615d5c", "bastion"="#5a7250", "baptiste"="#4ba9c4", "brigitte"="#bf6b63", "doomfist"="#81493e", "dva"="#e382bc", 
                "genji"="#95f034","hanzo"="#bab17f", "junkrat"="#ebb643", "lucio"="#77bf41", "mccree"="#b15050", "mei"="#68a6e8", "mercy"="#f3eab7", 
                "moira"="#7e3345", "orisa"="#398031", "pharah"="#2968bc", "reaper"="#793343", "reinhardt"="#929b9c", "roadhog"="#ae6e16", 
                "soldier76"="#636e89", "sombra"="#6f4fb2", "symmetra"="#8dbac9", "tracer"="#dc8f35", "torbjorn"="#b98283", "widowmaker"="#a0a1b5", 
                "winston"="#9799b2", "wrecking_ball"="#dc8f36", "zarya"="#ed79b0", "zenyatta"="#f0e376")
DamageHeroes 		<- names(heroes[which(heroes=="damage")])
TankHeroes		<- names(heroes[which(heroes=="tank")])
SupportHeroes	<- names(heroes[which(heroes=="support")])

#plot theme====
library(extrafont); library(cowplot)
TextSize = 8 #10 for thesis, 20+ for presentation
geom.text.size =  TextSize #/ ggplot2::.pt #geom_text uses "mm", ggplot theme uses point... yikes.
geom.text.size.small = geom.text.size / 2
theme_set(theme_cowplot(line_size = 1))
theme_update( text = element_text(family = windowsFont("Arial"), size = geom.text.size),  line = element_line(size=unit(0.7, "cm")), 
              legend.text.align = 1, plot.margin=margin(10,2,-15,0, "pt")
)
theme_update(
  axis.ticks.length=unit(-0.2, "cm"), strip.background = element_rect(fill = NA, colour = "#000000", linetype = "solid"),
  axis.text.x = element_text(family = "Arial", size = geom.text.size, margin=margin(8,0,0,0,"pt")), 
  axis.text.y = element_text(family = "Arial", size = geom.text.size, margin=margin(0,10,0,0,"pt")), 
  strip.text = element_text(family = "Arial", size = geom.text.size, margin = margin(2,2,2,2, "pt"))
)
#functions----
completeDT <- function(DT, cols, defs = NULL){
  mDT = do.call(CJ, c(DT[, ..cols], list(unique=TRUE))) 
  #Creates a data.table to Join, over cols, with Unique values, and all value combinations (silent repetition?)
  res = DT[mDT, on=names(mDT)] #joins by names
  if (length(defs)) { res[, names(defs) := Map(replace, .SD, lapply(.SD, is.na), defs), .SDcols=names(defs)] } #idk
  #seems to use a call to Map() to call replace() on all .SDs where is.na, replaceing the present value with that in defs?
  #whilst using .SDcols to restrict the columns being edited to those selected
  return(res)
} 
ExtractMainByStat <- function(DT, StatName){
  if ( (!StatName %in% colnames(DT)) ) { stop(paste0("Cannot ExtractMAinByStat(): ", StatName, " is not a column in supplied DT")) }
  setnames(DT, StatName, "TEMP")
  DT2 = DT[!is.na(TEMP),.SD[TEMP==max(TEMP), .(Hero, TEMP)], .(Player, Mode)]
  setnames(DT, "TEMP", StatName)
  setnames(DT2, "TEMP", StatName)
  return(DT2)
}
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
ParseGametypeStats<-function(DT, gametype, playerName){
  stopifnot(gametype %in% c("quickplay", "competitive"))
  DT=getElement(DT, gametype)
  RollingAvgs = as.data.table(getElement(DT, "rolling_average_stats"))
  RollingAvgs[,player:=playerName]
  RollingAvgs[,mode:=gametype]
  
  Overall = as.data.table(getElement(DT, "overall_stats"))
  Overall[,player:=playerName]
  Overall[,mode:=gametype]
  
  MostInGame = as.data.table(getElement(DT, "game_stats"))
  MostInGame[,player:=playerName]
  MostInGame[,mode:=gametype]
  
  Data = list()
  Data$OverallStats = Overall
  Data$RollingPer10Min = RollingAvgs
  Data$MostInGame = MostInGame
  
  return(Data)
}
ParsePlayerStats <- function(ChosenFile=NA, region="eu"){
  if(is.na(ChosenFile)) {
    AvailableFiles<- list.files(pattern = ".json", recursive = T)
    ChosenFile 		<- ChooseFromFactorLevels(AvailableFiles, "Choose file to extract:", ReturnIndex = F)
    rm(AvailableFiles)
  }
  PlayerName		<- str_match(ChosenFile, "\\d{6}_(\\w+)-\\d{4,6}\\.json")[,2]
  RawData 			<-fromJSON(ChosenFile)
  if (region=="eu") { RawData 			<-RawData$eu }	else { RawData 			<-RawData$us }
  
  QPStats   = ParseGametypeStats(RawData$stats, "quickplay", PlayerName)
  CompStats = ParseGametypeStats(RawData$stats, "competitive", PlayerName)
  
  OverallStats = rbind(QPStats$OverallStats, CompStats$OverallStats,       fill=TRUE)
  if(! ("comprank" %in% colnames(OverallStats))) { OverallStats[,comprank:="(Unranked)"] }
  SRName = sprintf("%s\n[%s]", PlayerName, OverallStats[,unique(comprank)])
  OverallStats[,SRName:=SRName]
  
  RollingAvgs  = rbind(QPStats$RollingPer10Min, CompStats$RollingPer10Min, fill=TRUE)
  RollingAvgs[,SRName:=SRName]
  
  MostInGame   = rbind(QPStats$MostInGame, CompStats$MostInGame,           fill=TRUE)
  MostInGame[,SRName:=SRName]
  
  rm(QPStats, CompStats)
  
  Playtime = rbind( melt(as.data.table(RawData$heroes$playtime$quickplay)[,gamemode:="Quickplay"], id.vars="gamemode"), 
                    melt(as.data.table(RawData$heroes$playtime$competitive)[,gamemode:="Competitive"], id.vars="gamemode"))
  setnames(Playtime, c("variable", "value"), c("hero", "playtime"))
  Playtime[,player:=PlayerName]
  ReorderColumns(Playtime, c("player", "hero", "gamemode"))
  setorder(Playtime, player, hero, gamemode)
  Playtime=merge(Playtime, heroRoles, by="hero")
  Playtime[,SRName:=SRName]
  
  heroPool = unique(c(names(RawData$heroes$stats$quickplay), names(RawData$heroes$stats$competitive)))
  RollingStats = data.table()
  HeroStats = data.table()
  for (gamemode_ in c("quickplay", "competitive")) {
    for (hero_ in heroPool) {
      #hero="mercy"
      if (hero_ %in% names(getElement(RawData$heroes$stats, gamemode_))) {
        Stats = getElement(getElement(RawData$heroes$stats, gamemode_), hero_)
        Stats.Rolling = as.data.table(Stats$rolling_average_stats)
        if (nrow(Stats.Rolling)>0) {
          Stats.Rolling[,hero:= hero_]
          Stats.Rolling[,gamemode:=gamemode_] 
          Stats.Rolling[,player:=PlayerName]
          RollingStats = rbind(RollingStats, Stats.Rolling, fill=TRUE)
          }
        
        Stats.HeroGeneral = as.data.table(Stats$general_stats)
        if (nrow(Stats.HeroGeneral)>0) { 
          Stats.HeroGeneral[,gamemode:=gamemode_]
          Stats.HeroGeneral[,hero:=hero_]
          Stats.HeroGeneral[,player:=PlayerName]
          HeroStats = rbind(HeroStats, Stats.HeroGeneral, fill=TRUE)
          
        }
      }
    }
    ReorderColumns(HeroStats, c("player", "hero", "gamemode"))
    ReorderColumns(RollingStats, c("player", "hero", "gamemode"))
  }
  HeroStats[,SRName:=SRName]
  RollingStats[,SRName:=SRName]
  
  GeneralStats = HeroStats[,.(player, hero, gamemode, games_played, games_won, games_lost, deaths, eliminations, final_blows, melee_final_blows,
                              weapon_accuracy, primary_fire_accuracy, secondary_fire_accuracy, quick_melee_accuracy, 
                              healing_done, health_recovered, hero_damage_done, barrier_damage_done, offensive_assists, defensive_assists, 
                              time_played, time_spent_on_fire, medals, medals_gold, medals_silver, medals_bronze)]
  GeneralStats[,SRName:=SRName]
  
  HighScores = HeroStats[,.(player, hero, gamemode, eliminations_most_in_game, eliminations_most_in_life, final_blows_most_in_game, solo_kills_most_in_game, 
                            all_damage_done_most_in_game, all_damage_done_most_in_life, hero_damage_done_most_in_game, hero_damage_done_most_in_life, 
                            barrier_damage_done_most_in_game, critical_hits_most_in_game, critical_hits_most_in_life, damage_blocked_most_in_game, 
                            healing_done_most_in_game, self_healing_most_in_game, objective_kills_most_in_game, objective_time_most_in_game, 
                            offensive_assists_most_in_game, defensive_assists_most_in_game)]
  HighScores[,SRName:=SRName]

  rm(RawData)
  Player = list()
  Player$HeroStats    = HeroStats
  Player$RollingAll   = RollingStats
  Player$RollingAvgs  = RollingAvgs
  Player$OverallStats = OverallStats
  Player$OverallStats[,realLevel:= ((prestige*100) + level)]
  Player$MostInGame   = MostInGame
  Player$HighScores   = HighScores
  Player$Playtime     = Playtime
  Player$SRName       = SRName
  return(Player)
}
ParseAllStats <- function(StatsDate=NA, Players=AllPlayers, region="eu"){
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
  
  Team = list()
  Team$HeroStats   = data.table()
  Team$RollingAvgs = data.table()
  Team$RollingAll  = data.table()
  Team$HighScores  = data.table()
  Team$MostInGame  = data.table()
  Team$Playtime    = data.table()
  Team$OverallStats= data.table()
  
  for (afile in ChosenFiles) {
    data <- ParsePlayerStats(paste0("./", StatsDate, "/", afile), region)
    Team$HeroStats   = rbind(Team$HeroStats,   data$HeroStats,   fill=T)
    Team$RollingAvgs = rbind(Team$RollingAvgs, data$RollingAvgs, fill=T)
    Team$RollingAll  = rbind(Team$RollingAll,  data$RollingAll,  fill=T)
    Team$HighScores  = rbind(Team$HighScores,  data$HighScores,  fill=T)
    Team$MostInGame  = rbind(Team$MostInGame,  data$MostInGame,  fill=T)
    Team$Playtime    = rbind(Team$Playtime,    data$Playtime,    fill=T)
    Team$OverallStats= rbind(Team$OverallStats,data$OverallStats,fill=T)
  }
  return(Team)
}
#script----
OWData = ParseAllStats()
#HeroTime Plot====
ggplot(OWData$Playtime, aes(x=role, y=playtime, fill=hero))+geom_col() + facet_wrap(gamemode~SRName, scales="free_y")+
  scale_fill_manual(values=heroColors)+theme(axis.text.x = element_text(angle=90, vjust = 0.3, hjust=1))+ylab("Playtime (decimal hours)")+xlab("")

#Highest In Game====
HighScores.M = melt(OWData$HighScores, id.vars = c("player", "SRName", "gamemode", "hero"))
HiScorePlot = ggplot(HighScores.M[gamemode=="competitive"], aes(x=SRName, y=value, color=hero, group=hero))+
  geom_text(aes(label=hero), color="black", size=10/ggplot2::.pt)+scale_color_manual(values=heroColors)+facet_wrap(~variable, scales="free_y")+
  theme(axis.text.x = element_text(angle=90, vjust = 0.3, hjust=1))+ggtitle("Competitive")
#ggsave("OWPLOT1.emf", HiScorePlot, device = "emf", units = "cm", width=20, height=25)

#accuracy====
AccuracyData = c("player", "SRName", "gamemode", "hero", colnames(OWData$HeroStats)[grepl("acc", colnames(OWData$HeroStats))])
AccuracyData = AccuracyData[!grepl("best_in_game", AccuracyData)]
AccuracyData = AccuracyData[!grepl("primal", AccuracyData)]
AccuracyData = AccuracyData[!grepl("tesla", AccuracyData)]
AccuracyData = AccuracyData[!grepl("hook", AccuracyData)]
AccuracyData = AccuracyData[!grepl("direct", AccuracyData)]
AccuracyData = OWData$HeroStats[, ..AccuracyData]
AccuracyData[!is.na(unscoped_accuracy), primary_fire_accuracy:=unscoped_accuracy]
AccuracyData[!is.na(rocket_hammer_melee_accuracy), quick_melee_accuracy:=rocket_hammer_melee_accuracy]
AccuracyData[!is.na(healing_accuracy), primary_fire_accuracy:=healing_accuracy]
set(AccuracyData, j = which(colnames(AccuracyData) %in% c("rocket_hammer_melee_accuracy", "weapon_accuracy", "healing_accuracy")), value = NULL)
setkey(AccuracyData, "player", "SRName", "hero", "gamemode")
setkey(OWData$Playtime, "player", "SRName", "hero", "gamemode")

AccuracyData = OWData$Playtime[AccuracyData]
AccuracyData.M = melt(AccuracyData, id.vars=c("player", "SRName", "hero", "gamemode"))
ggplot(AccuracyData.M[!is.na(value)], aes(x=paste0(player, " (", gamemode, ")"), y=value, color=hero, group=hero))+geom_jitter(width=0.1, size=2)+
  scale_color_manual(values=heroColors)+facet_grid(~variable)+geom_text(aes(label=hero), size=geom.text.size/ggplot2::.pt, nudge_x = 0.05)+
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust=0.33))
