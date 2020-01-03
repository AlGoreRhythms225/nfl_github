
adp_scrape <- function(season) {
  
  print("Getting latest ADP values...")
  
  # Get ADP & St.Dev
  adp_url <- paste0("https://fantasyfootballcalculator.com/api/v1/adp/standard?teams=12&year=",
                    season)
  adp_json <- fromJSON(adp_url)
  adp <- adp_json$players
  adp$position[adp$position=="DEF"] <- "DST" 
  
  # Get Backup #s from FFPros for missing players
  adp_url <- "https://www.fantasypros.com/nfl/adp/overall.php"
  adp_html <- GET(adp_url)
  adp_backup <- data.frame(readHTMLTable(rawToChar(adp_html$content),
                                         as.data.frame = TRUE,
                                         stringsAsFactors=FALSE)
                           $data)
  adp_backup <- adp_backup[,c(2,3,7)]
  names(adp_backup) <- c("name","pos","adp")
  adp_backup$pos_par <- str_locate(adp_backup$name, "\\(")[, 1]
  adp_backup$name <- substr(adp_backup$name,1,adp_backup$pos_par-2)
  adp_backup$team <- word(adp_backup$name, -1)
  adp_backup$pos_par <- nchar(paste0(" ",adp_backup$team))
  adp_backup$name <- substr(adp_backup$name,1,nchar(adp_backup$name)-adp_backup$pos_par)
  adp_backup$position <- ifelse(substr(adp_backup$pos,1,1)=="D","DST",
                                ifelse(substr(adp_backup$pos,1,1)=="K","K",
                                       substr(adp_backup$pos,1,2)))
  adp$name <- clean_player_name(adp$name)
  adp$trim_name <- adp$name
  adp$trim_name <- gsub(" ", "",adp$trim_name)
  adp$trim_name <- toupper(adp$trim_name)
  adp$trim_name <- clean_trim_name(adp$trim_name, adp$position)
  
  adp_backup$name <- clean_player_name(adp_backup$name)
  adp_backup$trim_name <- adp_backup$name
  adp_backup$trim_name <- gsub(" ", "",adp_backup$trim_name)
  adp_backup$trim_name <- toupper(adp_backup$trim_name)
  adp_backup$trim_name <- clean_trim_name(adp_backup$trim_name, adp_backup$pos)
  
  # Add missing players from backup to main
  
  adp_final <- merge(adp[,c("trim_name", "name","position","team","adp","stdev")],
                     adp_backup[,c("trim_name","name","position","team","adp")],
                     by = c("trim_name","position"),
                     all.y = TRUE)
  adp_final$adp <- ifelse(is.na(adp_final$adp.x),
                          adp_final$adp.y,
                          adp_final$adp.x)
  adp_final$adp.x <- NULL
  adp_final$adp.y <- NULL
  adp_final$adp <- as.numeric(adp_final$adp)
  adp_final$name <- ifelse(is.na(adp_final$name.x),
                           adp_final$name.y,
                           adp_final$name.x)
  adp_final$name.x <- NULL
  adp_final$name.y <- NULL
  adp_final$team <- ifelse(is.na(adp_final$team.x),
                           adp_final$team.y,
                           adp_final$team.x)
  adp_final$team.x <- NULL
  adp_final$team.y <- NULL
  adp_final$stdev[is.na(adp_final$stdev)] <- 20
  
  adp_final <- adp_final[!is.na(adp_final$trim_name),]
  
  adp_final <- adp_final[,c(1,5,2,6,4,3)]
  write.csv(adp_final, "adp.csv", row.names = FALSE)
  
}
