
#### add_play_by_play_data ####
# Pull in new play by play data using nflscrapr
add_play_by_play_data <- function(season) {
  print(paste0("Checking for ",season - 1," Play by Play Data..."))
  pbp_old <- read.csv("play_data.csv", stringsAsFactors = FALSE)
  if (nrow(pbp_old[pbp_old$season == (season-1),]) > 0) {
    print(paste0("Data for ",season - 1," season has already been scraped..."))  
  } else {
    print(paste0("Scraping Play by Play Data for ",season - 1," season..."))
    pbp_new <- season_play_by_play(season - 1)
    
    # Match column structure from old to new
    names(pbp_new) <- tolower(names(pbp_new))
    names(pbp_new)[names(pbp_new) == "date"] <- "game_date"
    names(pbp_new)[names(pbp_new) == "yards.gained"] <- "yards_gained"
    names(pbp_new)[names(pbp_new) == "challenge.replay"] <- "challenge_replay"
    names(pbp_new)[names(pbp_new) == "accepted.penalty"] <- "accepted_penalty"
    pbp_new$play_id <- NULL
    
    # Combine old and new
    play_data <- plyr::rbind.fill(pbp_new,pbp_old)
    rm(pbp_new, pbp_old)
    
    print(paste0("Cleaning Play by Play Data for ",season - 1," season..."))
    
    # Clean
    play_data$hometeam[play_data$hometeam == "LA"]  <- "LAR"
    play_data$awayteam[play_data$awayteam == "LA"]  <- "LAR"
    play_data$game_date <- as.Date(play_data$game_date,"%Y-%m-%d")
    play_data$playtype <- ifelse(play_data$playtype=="Sack",
                                 "Pass",
                                 play_data$playtype)
    play_data$spike <- ifelse(play_data$playtype=="Spike",
                              1,
                              0)
    play_data$passoutcome <- ifelse(play_data$playtype=="Spike",
                                    "Spike",
                                    play_data$passoutcome)
    play_data$playtype <- ifelse(play_data$playtype=="Spike",
                                 "Pass",
                                 play_data$playtype)
    
    write.csv(play_data,"play_data.csv",row.names = FALSE)
  }
}

#### instr ####
instr <- function(str1,str2,startpos=1,n=1){
  aa=unlist(strsplit(substring(str1,startpos),str2))
  if(length(aa) < n+1 ) return(0);
  return(sum(nchar(aa[1:n])) + startpos+(n-1)*nchar(str2) )
}  

scrape_ffa <- function(season) {
  new_data <- scrape_data(src = c("CBS", "FantasyPros", "FantasySharks", "NumberFire", 
                                  "Yahoo","NFL", "RTSports", "Walterfootball"),
                          pos = c("QB","RB","WR","TE","DST","K"), 
                                  season = season, 
                                  week = 0)
  
  new_proj <- plyr::rbind.fill(new_data$QB,
                               new_data$RB,
                               new_data$WR,
                               new_data$TE,
                               new_data$DST,
                               new_data$K)
  new_proj <- new_proj %>% 
    add_player_info()
  
  write.csv(new_proj, paste0("raw projections ",season,".csv"),row.names = FALSE)
}

clean_player_name <- function(column_name) {
  column_name <- gsub("'","",column_name)
  column_name <- gsub("\\.","",column_name)
  column_name <- gsub("-","",column_name)
  column_name <- gsub(" IV","",column_name)
  column_name <- gsub(" III","",column_name)
  column_name <- gsub(" II","",column_name)
  column_name <- gsub(" Jr","",column_name)
  column_name <- gsub(" Sr","",column_name)
  return(column_name)
}

clean_trim_name <- function(trim_name_col, position_col) {
  trim_name_col[trim_name_col=="BENWATSON"] <- "BENJAMINWATSON"
  trim_name_col[trim_name_col=="MITCHTRUBISKY"] <- "MITCHELLTRUBISKY"
  trim_name_col[trim_name_col=="WILLFULLERV"] <- "WILLFULLER"
  trim_name_col[trim_name_col=="RYANGRIFFIN" & position_col=="TE"] <- "RYANGRIFFINTE"
  trim_name_col[trim_name_col=="CHRISTHOMPSON" & position_col=="RB"] <- "CHRISTHOMPSONRB"
  trim_name_col[trim_name_col=="ALEXANDERARMAH"] <- "ALEXARMAH"
  trim_name_col[trim_name_col=="BUCKYHODGES"] <- "TEMUCHINHODGES"
  trim_name_col[trim_name_col=="CHARLESJOHNSON"] <- "CHARLESDJOHNSON"
  trim_name_col[trim_name_col=="DANVITALE"] <- "DANNYVITALE"
  trim_name_col[trim_name_col=="CEDRICKWILSON"] <- "CEDWILSON"
  trim_name_col[trim_name_col=="DAVIDWILLIAMS"] <- "DAVEWILLIAMS"
  trim_name_col[trim_name_col=="JEFFERYWILSON"] <- "JEFFWILSON"
  trim_name_col[trim_name_col=="JOSHUAPERKINS"] <- "JOSHPERKINS"
  trim_name_col[trim_name_col=="MATTHEWDAYES"] <- "MATTDAYES"
  trim_name_col[trim_name_col=="MATTHEWMCCRANE"] <- "MATTMCCRANE"
  trim_name_col[trim_name_col=="MATTSLATER"] <- "MATTHEWSLATER"
  trim_name_col[trim_name_col=="ROOSEVELTNIX"] <- "ROOSEVELTNIXJONES"
  trim_name_col[trim_name_col=="STEVENHAUSCHKA"] <- "STEPHENHAUSCHKA"
  trim_name_col[trim_name_col=="PATMAHOMES"] <- "PATRICKMAHOMES"
  
  trim_name_col[trim_name_col=="CHICAGODEFENSE"] <- "CHICAGOBEARS"
  trim_name_col[trim_name_col=="LARAMSDEFENSE"] <- "LOSANGELESRAMS"
  trim_name_col[trim_name_col=="JACKSONVILLEDEFENSE"]  <- "JACKSONVILLE"
  trim_name_col[trim_name_col=="BALTIMOREDEFENSE"] <- "JACKSONVILLEJAGUARS"
  trim_name_col[trim_name_col=="MINNESOTADEFENSE"] <- "MINNESOTAVIKINGS"
  trim_name_col[trim_name_col=="CLEVELANDDEFENSE"] <- "CLEVELANDBROWNS"
  trim_name_col[trim_name_col=="LACHARGERSDEFENSE"] <- "LOSANGELESCHARGERS"
  trim_name_col[trim_name_col=="HOUSTONDEFENSE"] <- "HOUSTONTEXANS"
  trim_name_col[trim_name_col=="DALLASDEFENSE"] <- "DALLASCOWBOYS"
  trim_name_col[trim_name_col=="DENVERDEFENSE"] <- "DENVERBRONCOS"
  trim_name_col[trim_name_col=="PHILADELPHIADEFENSE"] <- "PHILADELPHIAEAGLES"
  trim_name_col[trim_name_col=="NEWENGLANDDEFENSE"] <- "NEWENGLANDPATRIOTS"
  trim_name_col[trim_name_col=="NYGIANTSDEFENSE"] <- "NEWYORKGIANTS"
  trim_name_col[trim_name_col=="NEWORLEANSDEFENSE"] <- "NEWORLEANSSAINTS"
  trim_name_col[trim_name_col=="BUFFALODEFENSE"] <- "BUFFALOBILLS"
  trim_name_col[trim_name_col=="PITTSBURGHDEFENSE"] <- "PITTSBURGHSTEELERS"
  trim_name_col[trim_name_col=="GREENBAYDEFENSE"] <- "GREENBAYPACKERS"
  trim_name_col[trim_name_col=="WASHINGTONDEFENSE"] <- "WASHINGTONREDSKINS"
  trim_name_col[trim_name_col=="TENNESSEEDEFENSE"]  <- "TENNESSEETITANS"
  trim_name_col[trim_name_col=="INDIANAPOLISDEFENSE"] <- "INDIANAPOLISCOLTS"
  trim_name_col[trim_name_col=="SEATTLEDEFENSE"] <- "SEATTLESEAHAWKS"
  trim_name_col[trim_name_col=="SANFRANCISCODEFENSE"] <- "SANFRANCISCO49ERS"
  trim_name_col[trim_name_col=="ARIZONADEFENSE"] <- "ARIZONACARDINALS"
  trim_name_col[trim_name_col=="NEWYORKDEFENSE"] <- "NEWYORKJETS"
  trim_name_col[trim_name_col=="ATLANTADEFENSE"] <- "ATLANTAFALCONS"
  trim_name_col[trim_name_col=="DETROITDEFENSE"] <- "DETROITLIONS"
  trim_name_col[trim_name_col=="CAROLINADEFENSE"] <- "CAROLINAPANTHERS"
  trim_name_col[trim_name_col=="MIAMIDEFENSE"] <- "MIAMIDOLPHINS"
  trim_name_col[trim_name_col=="TAMPABAYDEFENSE"] <- "TAMPABAYBUCCANEERS"
  trim_name_col[trim_name_col=="OAKLANDDEFENSE"] <- "OAKLANDRAIDERS"
  trim_name_col[trim_name_col=="CINCINNATIDEFENSE"] <- "CINCINNATIBENGALS" 
  trim_name_col[trim_name_col=="KANSASCITYDEFENSE"] <- "KANSASCITYCHIEFS"  
  
  
  trim_name_col[trim_name_col=="BEARS"] <- "CHICAGOBEARS"
  trim_name_col[trim_name_col=="RAMS"] <- "LOSANGELESRAMS"
  trim_name_col[trim_name_col=="JAGUARS"]  <- "JACKSONVILLEJAGUARS"
  trim_name_col[trim_name_col=="RAVENS"] <- "BALTIMORERAVENS"
  trim_name_col[trim_name_col=="VIKINGS"] <- "MINNESOTAVIKINGS"
  trim_name_col[trim_name_col=="BROWNS"] <- "CLEVELANDBROWNS"
  trim_name_col[trim_name_col=="CHARGERS"] <- "LOSANGELESCHARGERS"
  trim_name_col[trim_name_col=="TEXANS"] <- "HOUSTONTEXANS"
  trim_name_col[trim_name_col=="COWBOYS"] <- "DALLASCOWBOYS"
  trim_name_col[trim_name_col=="BRONCOS"] <- "DENVERBRONCOS"
  trim_name_col[trim_name_col=="EAGLES"] <- "PHILADELPHIAEAGLES"
  trim_name_col[trim_name_col=="PATRIOTS"] <- "NEWENGLANDPATRIOTS"
  trim_name_col[trim_name_col=="SAINTS"] <- "NEWORLEANSSAINTS"
  trim_name_col[trim_name_col=="BILLS"] <- "BUFFALOBILLS"
  trim_name_col[trim_name_col=="STEELERS"] <- "PITTSBURGHSTEELERS"
  trim_name_col[trim_name_col=="PACKERS"] <- "GREENBAYPACKERS"
  trim_name_col[trim_name_col=="REDSKINS"] <- "WASHINGTONREDSKINS"
  trim_name_col[trim_name_col=="TITANS"]  <- "TENNESSEETITANS"
  trim_name_col[trim_name_col=="COLTS"] <- "INDIANAPOLISCOLTS"
  trim_name_col[trim_name_col=="SEAHAWKS"] <- "SEATTLESEAHAWKS"
  trim_name_col[trim_name_col=="49ERS"] <- "SANFRANCISCO49ERS"
  trim_name_col[trim_name_col=="CARDINALS"] <- "ARIZONACARDINALS"
  trim_name_col[trim_name_col=="JETS"] <- "NEWYORKJETS"
  trim_name_col[trim_name_col=="FALCONS"] <- "ATLANTAFALCONS"
  trim_name_col[trim_name_col=="LIONS"] <- "DETROITLIONS"
  trim_name_col[trim_name_col=="PANTHERS"] <- "CAROLINAPANTHERS"
  trim_name_col[trim_name_col=="DOLPHINS"] <- "MIAMIDOLPHINS"
  trim_name_col[trim_name_col=="BUCCANEERS"] <- "TAMPABAYBUCCANEERS"
  trim_name_col[trim_name_col=="RAIDERS"] <- "OAKLANDRAIDERS"
  trim_name_col[trim_name_col=="BENGALS"] <- "CINCINNATIBENGALS"  
  trim_name_col[trim_name_col=="GIANTS"] <- "NEWYORKGIANTS"  
  trim_name_col[trim_name_col=="CHIEFS"] <- "KANSASCITYCHIEFS"  
  
  
  return(trim_name_col)
}


visualize_rf_imp <- function(model, importance, pos) {
  imp <- importance(rf, type=1)
  featureImportance <- data.frame(Feature=row.names(imp), 
                                  Importance=imp[,1])
  non_neg_imp <- featureImportance %>% 
    filter(Importance > 1) %>% 
    arrange(-Importance)
  
  print(ggplot(featureImportance, 
                aes(x=reorder(Feature, Importance), 
                    y=Importance)) + 
           geom_bar(stat="identity", fill="#d40511") +
           coord_flip() +
           theme_light(base_size=10) + 
           xlab("") +
           ylab("Importance") + 
           ggtitle(paste0("Random Forest Feature Importance - ",pos,"\n")) +
           theme(plot.title=element_text(size=18)))
  return(non_neg_imp)
}
