###########################################################################################################
# SWISS FOOTBALL TRANSFERS
###########################################################################################################

# Loading Packages ---------------------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(rvest, dplyr, tidyr, ggplot2, circlize)

# Scraping transfers FROM Swiss football clubs (loans excluded) ------------------------------------------

# Defining parameters 
url_base <- "https://www.transfermarkt.ch/transfers/transfertagedetail/statistik/top/plus/0?land_id_ab=148&land_id_zu=&leihe=true&datum="
days <- seq(as.Date("2012-07-01"), as.Date("2017-11-09"), "days")
transfers_from_ch <- NULL

# Loop over days
for(i in 1:length(days)){
  
  transfers_per_day <- NULL
  
  # Loop over pages per day
  for(j in 1:100){
    
    tab <- read_html(paste0(url_base, as.character(days[i]), "&page=", j)) %>%
      html_node(xpath='//*[@id="yw1"]/table')
    
    if(length(tab)>0){
      
      tab <- tab %>%
        html_table(fill=TRUE, header=FALSE) %>%
        filter(X2!="Alter", !is.na(X4)) %>%
        select(X3, X8, X9, X12, X13, X14) %>%
        mutate(date=days[i])
      
      transfers_per_day <- rbind.data.frame(transfers_per_day, tab)
    
    } else {
      
      break
      
    }
    
    if(nrow(tab)<25) break
    rm(tab)
    
  }
  
  print(paste0(days[i], " done"))
  transfers_from_ch <- rbind.data.frame(transfers_from_ch, transfers_per_day)
  
}

# Tidying up
rm(list=setdiff(ls(), c("transfers_from_ch", "days")))

# Scraping transfers TO Swiss football clubs (loans excluded) --------------------------------------------

# Defining parameters 
url_base <- "https://www.transfermarkt.ch/transfers/transfertagedetail/statistik/top/plus/0?land_id_ab=&land_id_zu=148&leihe=true&datum="
transfers_to_ch <- NULL

# Loop over days
for(i in 1:length(days)){
  
  transfers_per_day <- NULL
  
  # Loop over pages per day
  for(j in 1:100){
    
    tab <- read_html(paste0(url_base, as.character(days[i]), "&page=", j)) %>%
      html_node(xpath='//*[@id="yw1"]/table')
    
    if(length(tab)>0){
      
      tab <- tab %>%
        html_table(fill=TRUE, header=FALSE) %>%
        filter(X2!="Alter", !is.na(X4)) %>%
        select(X3, X8, X9, X12, X13, X14) %>%
        mutate(date=days[i])
      
      transfers_per_day <- rbind.data.frame(transfers_per_day, tab)
      
    } else {
      
      break
      
    }
    
    if(nrow(tab)<25) break
    rm(tab)
    
  }
  
  print(paste0(days[i], " done"))
  transfers_to_ch <- rbind.data.frame(transfers_to_ch, transfers_per_day)
  
}

# Tidying up
rm(list=setdiff(ls(), c("transfers_from_ch", "transfers_to_ch")))

# Scraping information on all leagues (region, country, tier, name) --------------------------------------

# Defining parameters 
url_base <- "https://www.transfermarkt.ch/wettbewerbe/"
regions <- c("europa", "asien", "amerika", "afrika")
leagues <- NULL

# Loop over regions
for(i in 1:length(regions)){
  
  leagues_per_region <- NULL
  
  # Loop over pages per region
  for(j in 1:100){
    
    site <- read_html(paste0(url_base, regions[i], "?page=", j)) %>%
      html_node(xpath='//*[@id="yw1"]/table')
    
    if(length(site)>0){

      tab <- site %>%
        html_table(fill=TRUE, header=FALSE) %>%
        filter(X1!="Wettbewerb", X1!="") %>%
        select(X1, X4) %>%
        mutate(
          pos=row_number(),
          tier="",
          country="") 
      
      levels <- tab %>%
        filter(!X4=="")
      
      # Extracting tier
      for(k in 1:nrow(levels)){
        
        tab$tier[tab$pos>levels$pos[k]] <- levels$X1[k]
        
      }
      
      tab <- tab %>%
        filter(X4=="")
      
      # Extracting country
      for(l in 1:nrow(tab)){
        
        tab$country[l] <- site %>% 
          html_node(xpath=paste0('//*[@id="yw1"]/table/tbody/tr[', tab$pos[l], ']/td[2]/img')) %>%
          html_attr("title")
      
      }
      
      tab <- tab %>%
        mutate(
          name=X1,
          region=regions[i]) %>%
        select(region, country, tier, name)

      leagues_per_region <- rbind.data.frame(leagues_per_region, tab)
      
    } else {
      
      break
      
    }
    
    if(nrow(tab)<20) break
    rm(tab)
    
  }
  
  print(paste0(regions[i], " done"))
  leagues <- rbind.data.frame(leagues, leagues_per_region)
  
}

# Tidying up
rm(list=setdiff(ls(), c("transfers_from_ch", "transfers_to_ch", "leagues")))

# Preparing data set -------------------------------------------------------------------------------------

# Combining data sets
transfers <- union(transfers_from_ch, transfers_to_ch) %>%
  mutate(
    name=X3,
    club_from=X8,
    league_from=X9,
    club_to=X12,
    league_to=X13,
    fee=X14
  ) %>%
  select(date, name, club_from, league_from, club_to, league_to, fee) %>% 
  left_join(leagues, by = c("league_from" = "name")) %>% 
  left_join(leagues, by = c("league_to" = "name"), suffix = c("_from", "_to"))

# Permeability between Swiss football leagues ------------------------------------------------------------

# Transfers between Swiss football leagues
permeability <- transfers %>%
  mutate(
    country_from = ifelse(league_from=="Schweiz", "Schweiz", country_from),
    country_to = ifelse(league_to=="Schweiz", "Schweiz", country_to)) %>%
  filter(country_from == "Schweiz" & country_to == "Schweiz") %>%
  filter(!is.na(tier_to) & !is.na(tier_from)) %>%
  mutate(
    league_from = gsub(" Gr\\. [0-9]{1}", "", league_from),
    league_from = gsub("Inter -", "Inter", league_from),
    league_from = gsub("Swiss U18 Elite League", "U-18", league_from),
    league_to = gsub(" Gr\\. [0-9]{1}", "", league_to),
    league_to = gsub("Inter -", "Inter", league_to),
    league_to = gsub("Swiss U18 Elite League", "U-18", league_to),
    from_to = paste0(league_from, "_", league_to)) %>%
  group_by(from_to) %>%
  summarise(transfers = n()) %>%
  separate(from_to, c("from", "to"), sep = "_") %>%
  filter(!from==to)

# Chord Diagramm: Preparation
leagues <- c("Super League", "Challenge League", "Promotion League", "1. Liga", "2. Liga Inter", "U-18")
col <- c("#7AACCF", "#FDB26E", "#81C481", "#E87E80", "#C0A5D8", "#BB9A93")

# Chord Digramm 1: all
circos.par(start.degree = 90, clock.wise = T)
chordDiagram(permeability, order = leagues, grid.col = col, transparency = 0.1, annotationTrack = c("name","grid"),
             annotationTrackHeight = c(0.01, 0.05))
circos.clear()

# Plots 2-7: highlighting inflows for all season per league
par(mfrow=c(2,3))

for(i in 1:length(leagues)){
  
  tra_mat <- as.matrix(rep(0.2, nrow(permeability)))
  tra_mat[!permeability$to==leagues[i]] <- 0.8
  
  circos.par(start.degree = 90, clock.wise = T)
  chordDiagram(permeability, order = leagues, grid.col = col, transparency = tra_mat, 
               annotationTrack = c("name","grid"), annotationTrackHeight = c(0.01, 0.05))
  circos.clear()
  
}

rm(tra_mat, i, col, leagues)

# Plots 8-13: all for each season
years <- seq(2012, 2018)
par(mfrow=c(2,3))

for(i in 1:(length(years)-1)){
  
  permeability2 <- transfers %>%
    mutate(
      country_from = ifelse(league_from=="Schweiz", "Schweiz", country_from),
      country_to = ifelse(league_to=="Schweiz", "Schweiz", country_to)) %>%
    filter(country_from == "Schweiz" & country_to == "Schweiz") %>%
    filter(!is.na(tier_to) & !is.na(tier_from)) %>%
    mutate(
      league_from = gsub(" Gr\\. [0-9]{1}", "", league_from),
      league_from = gsub("Inter -", "Inter", league_from),
      league_from = gsub("Swiss U18 Elite League", "U-18", league_from),
      league_to = gsub(" Gr\\. [0-9]{1}", "", league_to),
      league_to = gsub("Inter -", "Inter", league_to),
      league_to = gsub("Swiss U18 Elite League", "U-18", league_to),
      from_to = paste0(league_from, "_", league_to)) %>%
    filter(date>=as.Date(paste0(years[i], "-07-01"))&date<=as.Date(paste0(years[i+1], "-06-30"))) %>%
    group_by(from_to) %>%
    summarise(transfers = n()) %>%
    separate(from_to, c("from", "to"), sep = "_") %>%
    filter(!from==to)
  
  if(years[i]<2014){
    
    leagues <- c("Super League", "Challenge League", "Promotion League", "1. Liga", "2. Liga Inter")
    col <- c("#7AACCF", "#FDB26E", "#81C481", "#E87E80", "#C0A5D8")
    
  } else {
    
    leagues <- c("Super League", "Challenge League", "Promotion League", "1. Liga", "2. Liga Inter", "U-18")
    col <- c("#7AACCF", "#FDB26E", "#81C481", "#E87E80", "#C0A5D8", "#BB9A93")
    
    
  }

  circos.par(start.degree = 90, clock.wise = T)
  chordDiagram(permeability2, order = leagues, grid.col = col, transparency = 0.2, 
               annotationTrack = c("name","grid"), annotationTrackHeight = c(0.01, 0.05))
  circos.clear()
  
}

rm(col, i, leagues, years)