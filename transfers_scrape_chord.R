###########################################################################################################
# SWISS FOOTBALL TRANSFERS
###########################################################################################################

# Loading Packages ---------------------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(rvest, dplyr, tidyr, ggplot2, circlize, countrycode)

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
      html_node(xpath = '//*[@id="yw1"]/table')
    
    if(length(tab)>0){
      
      tab <- tab %>%
        html_table(fill = TRUE, header = FALSE) %>%
        filter(!X2=="Alter", !is.na(X4)) %>%
        select(X3, X8, X9, X12, X13, X14) %>%
        mutate(date = days[i])
      
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
rm(list = setdiff(ls(), c("transfers_from_ch", "days")))

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
      html_node(xpath = '//*[@id="yw1"]/table')
    
    if(length(tab)>0){
      
      tab <- tab %>%
        html_table(fill = TRUE, header = FALSE) %>%
        filter(!X2=="Alter", !is.na(X4)) %>%
        select(X3, X8, X9, X12, X13, X14) %>%
        mutate(date = days[i])
      
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
rm(list = setdiff(ls(), c("transfers_from_ch", "transfers_to_ch")))

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
      html_node(xpath = '//*[@id="yw1"]/table')
    
    if(length(site)>0){

      tab <- site %>%
        html_table(fill = TRUE, header = FALSE) %>%
        filter(!X1 == "Wettbewerb", !X1 == "") %>%
        select(X1, X4) %>%
        mutate(
          pos = row_number(),
          tier = "",
          country = "") 
      
      levels <- tab %>%
        filter(!X4 == "")
      
      # Extracting tier
      for(k in 1:nrow(levels)){
        
        tab$tier[tab$pos>levels$pos[k]] <- levels$X1[k]
        
      }
      
      tab <- tab %>%
        filter(X4 == "")
      
      # Extracting country
      for(l in 1:nrow(tab)){
        
        tab$country[l] <- site %>% 
          html_node(xpath = paste0('//*[@id="yw1"]/table/tbody/tr[', tab$pos[l], ']/td[2]/img')) %>%
          html_attr("title")
      
      }
      
      tab <- tab %>%
        mutate(
          name = X1,
          region = regions[i]) %>%
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
rm(list = setdiff(ls(), c("transfers_from_ch", "transfers_to_ch", "leagues")))

# Preparing data set -------------------------------------------------------------------------------------

# Combining data sets
transfers <- union(transfers_from_ch, transfers_to_ch) %>%
  mutate(
    name = X3,
    club_from = X8,
    league_from = X9,
    club_to = X12,
    league_to = X13,
    fee = X14
  ) %>%
  select(date, name, club_from, league_from, club_to, league_to, fee) %>% 
  left_join(leagues, by = c("league_from" = "name")) %>% 
  left_join(leagues, by = c("league_to" = "name"), suffix = c("_from", "_to"))

# Permeability between Swiss football leagues ------------------------------------------------------------

# Transfers between Swiss football leagues
permeability <- transfers %>%
  mutate(
    country_from = ifelse(league_from == "Schweiz", "Schweiz", country_from),
    country_to = ifelse(league_to == "Schweiz", "Schweiz", country_to)) %>%
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
  filter(!from == to)

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
  tra_mat[!permeability$to == leagues[i]] <- 0.8
  
  circos.par(start.degree = 90, clock.wise = T)
  chordDiagram(permeability, order = leagues, grid.col = col, transparency = tra_mat, 
               annotationTrack = c("name","grid"), annotationTrackHeight = c(0.01, 0.05))
  circos.clear()
  
}

rm(tra_mat, i, col, leagues, permeability)

# Plots 8-13: all for each season
years <- seq(2012, 2018)
par(mfrow=c(2,3))

for(i in 1:(length(years)-1)){
  
  permeability2 <- transfers %>%
    mutate(
      country_from = ifelse(league_from == "Schweiz", "Schweiz", country_from),
      country_to = ifelse(league_to == "Schweiz", "Schweiz", country_to)) %>%
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
    filter(!from == to)
  
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

rm(col, i, years, permeability2)

# International Swiss football transfers  ------------------------------------------------------------------------------

# Detecting club duplicates (due to leagues with the same name)
duplicates <- bind_rows(transfers %>%
                             mutate(
                               club = club_from,
                               country = country_from) %>%
                             select(club, country),
                           transfers %>%
                             mutate(
                               club = club_to,
                               country = country_to) %>%
                             select(club, country)) %>%
  distinct() %>%
  filter(!is.na(country)) %>%
  group_by(club) %>%
  filter(n() > 1) %>%
  arrange(country)

# Number of rows after correction (distinct clubs in duplicates)
nrow(duplicates %>%
  select(club) %>%
  distinct())

# Not very nice manual work (data is the new oil, they said)
nrow(duplicates %>%
  filter(!(!country == "Ägypten" & club %in% c("Arab Contr.", "Harras Hodoud", "Smouha", "Zamalek", "Wadi Degla", "El Gouna"))) %>%
  filter(!(country == "Ägypten" & !club %in% c("Arab Contr.", "Harras Hodoud", "Smouha", "Zamalek", "Wadi Degla", "El Gouna"))) %>%
  filter(!(!country == "Argentinien" & club %in% c("Boca Juniors", "Unión Santa Fé", "River Plate"))) %>%
  filter(!(country == "Argentinien" & !club %in% c("Boca Juniors", "Unión Santa Fé", "River Plate"))) %>%
  filter(!(!country == "China" & club %in% c("MZ Hakka", "BJ Sinobo Guoan"))) %>%
  filter(!(country == "China" & !club %in% c("MZ Hakka", "BJ Sinobo Guoan"))) %>%
  filter(!(!country == "England" & club %in% c("Leyton Orient", "Newcastle Utd.", "FC Fulham",  "FC Reading", "Aston Villa", "Port Vale", 
                                               "FC Chelsea", "Bradford City", "West Ham Utd.", "Leeds United", "Middlesbrough", "FC Everton",
                                               "AFC Sunderland", "Brighton & Hove", "Hereford Utd.", "Oldham Athletic", "Bournemouth", 
                                               "FC Southampton", "FC Arsenal"))) %>%
  filter(!(country == "England" & !club %in% c("Leyton Orient", "Newcastle Utd.", "FC Fulham",  "FC Reading", "Aston Villa", "Port Vale", 
                                               "FC Chelsea", "Bradford City", "West Ham Utd.", "Leeds United", "Middlesbrough", "FC Everton",
                                               "AFC Sunderland", "Brighton & Hove", "Hereford Utd.", "Oldham Athletic", "Bournemouth", 
                                               "FC Southampton", "FC Arsenal"))) %>%
  filter(!(country == "Fidschi")) %>%
  filter(!(!country == "Ghana" & club == "Liberty Prof.")) %>%
  filter(!(country == "Ghana" & !club == "Liberty Prof.")) %>%
  filter(!(!country == "Griechenland" & club %in% c("Panathinaikos", "Apollon Smyrnis", "Olymp. Piräus", "AOK Kerkyra", "Platanias", 
                                                     "PAE Veria", "APO Levadiakos", "AO Xanthi"))) %>%
  filter(!(country == "Griechenland" & !club %in% c("Panathinaikos", "Apollon Smyrnis", "Olymp. Piräus", "AOK Kerkyra", "Platanias", 
                                                     "PAE Veria", "APO Levadiakos", "AO Xanthi"))) %>%
  filter(!(country == "Indonesien")) %>%
  filter(!(!country == "Irland" & club == "Shamrock Rovers")) %>%
  filter(!(country == "Irland" & !club == "Shamrock Rovers")) %>%
  filter(!(!country == "Kasachstan" & club == "Aktobe")) %>%
  filter(!(country == "Kasachstan" & !club == "Aktobe")) %>%
  filter(!(country == "Libanon")) %>%
  filter(!(!country == "Mazedonien" & club %in% c("Shkendija", "FC Shkupi", "Sileks Kratovo", "Renova", "Rabotn. Skopje", 
                                                  "Bregalnica Stip"))) %>%
  filter(!(country == "Mazedonien" & !club %in% c("Shkendija", "FC Shkupi", "Sileks Kratovo", "Renova", "Rabotn. Skopje", 
                                                  "Bregalnica Stip"))) %>%
  filter(!(country == "Montenegro")) %>%
  filter(!(country == "Nordirland")) %>%
  filter(!(!country == "Ukraine" & club %in% c("Dynamo Kiew", "Vorskla", "Zorya Luhansk"))) %>%
  filter(!(country == "Ukraine" & !club %in% c("Dynamo Kiew", "Vorskla", "Zorya Luhansk"))) %>%
  filter(!(country == "Malta")))

# Preventing special cases (free agents etc.) from being excluded
transfers <- transfers %>%
  mutate(
    country_from = ifelse(club_from %in% c("Vereinslos", "Karriereende", "Sperre", "Unbekannt", "pausiert"), 
                          "special case", country_from),
    country_from = ifelse(is.na(country_from), "special case", country_from),
    country_to = ifelse(club_to %in% c("Vereinslos", "Karriereende", "Sperre", "Unbekannt", "pausiert"), 
                        "special case", country_to),
    country_to = ifelse(is.na(country_to), "special case", country_to))

# Actual correction
transfers <- transfers %>%
  filter(!(!country_to == "Ägypten" & club_to %in% c("Arab Contr.", "Harras Hodoud", "Smouha", "Zamalek", "Wadi Degla", "El Gouna"))) %>%
  filter(!(country_to == "Ägypten" & !club_to %in% c("Arab Contr.", "Harras Hodoud", "Smouha", "Zamalek", "Wadi Degla", "El Gouna"))) %>%
  filter(!(!country_to == "Argentinien" & club_to %in% c("Boca Juniors", "Unión Santa Fé", "River Plate"))) %>%
  filter(!(country_to == "Argentinien" & !club_to %in% c("Boca Juniors", "Unión Santa Fé", "River Plate"))) %>%
  filter(!(!country_to == "China" & club_to %in% c("MZ Hakka", "BJ Sinobo Guoan"))) %>%
  filter(!(country_to == "China" & !club_to %in% c("MZ Hakka", "BJ Sinobo Guoan"))) %>%
  filter(!(!country_to == "England" & club_to %in% c("Leyton Orient", "Newcastle Utd.", "FC Fulham",  "FC Reading", "Aston Villa", "Port Vale", 
                                               "FC Chelsea", "Bradford City", "West Ham Utd.", "Leeds United", "Middlesbrough", "FC Everton",
                                               "AFC Sunderland", "Brighton & Hove", "Hereford Utd.", "Oldham Athletic", "Bournemouth", 
                                               "FC Southampton", "FC Arsenal"))) %>%
  filter(!(country_to == "England" & !club_to %in% c("Leyton Orient", "Newcastle Utd.", "FC Fulham",  "FC Reading", "Aston Villa", "Port Vale", 
                                               "FC Chelsea", "Bradford City", "West Ham Utd.", "Leeds United", "Middlesbrough", "FC Everton",
                                               "AFC Sunderland", "Brighton & Hove", "Hereford Utd.", "Oldham Athletic", "Bournemouth", 
                                               "FC Southampton", "FC Arsenal"))) %>%
  filter(!(country_to == "Fidschi")) %>%
  filter(!(!country_to == "Ghana" & club_to == "Liberty Prof.")) %>%
  filter(!(country_to == "Ghana" & !club_to == "Liberty Prof.")) %>%
  filter(!(!country_to == "Griechenland" & club_to %in% c("Panathinaikos", "Apollon Smyrnis", "Olymp. Piräus", "AOK Kerkyra", "Platanias", 
                                                    "PAE Veria", "APO Levadiakos", "AO Xanthi"))) %>%
  filter(!(country_to == "Griechenland" & !club_to %in% c("Panathinaikos", "Apollon Smyrnis", "Olymp. Piräus", "AOK Kerkyra", "Platanias", 
                                                    "PAE Veria", "APO Levadiakos", "AO Xanthi"))) %>%
  filter(!(country_to == "Indonesien")) %>%
  filter(!(!country_to == "Irland" & club_to == "Shamrock Rovers")) %>%
  filter(!(country_to == "Irland" & !club_to == "Shamrock Rovers")) %>%
  filter(!(!country_to == "Kasachstan" & club_to == "Aktobe")) %>%
  filter(!(country_to == "Kasachstan" & !club_to == "Aktobe")) %>%
  filter(!(country_to == "Libanon")) %>%
  filter(!(!country_to == "Mazedonien" & club_to %in% c("Shkendija", "FC Shkupi", "Sileks Kratovo", "Renova", "Rabotn. Skopje", 
                                                  "Bregalnica Stip"))) %>%
  filter(!(country_to == "Mazedonien" & !club_to %in% c("Shkendija", "FC Shkupi", "Sileks Kratovo", "Renova", "Rabotn. Skopje", 
                                                  "Bregalnica Stip"))) %>%
  filter(!(country_to == "Montenegro")) %>%
  filter(!(country_to == "Nordirland")) %>%
  filter(!(!country_to == "Ukraine" & club_to %in% c("Dynamo Kiew", "Vorskla", "Zorya Luhansk"))) %>%
  filter(!(country_to == "Ukraine" & !club_to %in% c("Dynamo Kiew", "Vorskla", "Zorya Luhansk"))) %>%
  filter(!(country_to == "Malta")) %>%
  filter(!(!country_from == "Ägypten" & club_from %in% c("Arab Contr.", "Harras Hodoud", "Smouha", "Zamalek", "Wadi Degla", "El Gouna"))) %>%
  filter(!(country_from == "Ägypten" & !club_from %in% c("Arab Contr.", "Harras Hodoud", "Smouha", "Zamalek", "Wadi Degla", "El Gouna"))) %>%
  filter(!(!country_from == "Argentinien" & club_from %in% c("Boca Juniors", "Unión Santa Fé", "River Plate"))) %>%
  filter(!(country_from == "Argentinien" & !club_from %in% c("Boca Juniors", "Unión Santa Fé", "River Plate"))) %>%
  filter(!(!country_from == "China" & club_from %in% c("MZ Hakka", "BJ Sinobo Guoan"))) %>%
  filter(!(country_from == "China" & !club_from %in% c("MZ Hakka", "BJ Sinobo Guoan"))) %>%
  filter(!(!country_from == "England" & club_from %in% c("Leyton Orient", "Newcastle Utd.", "FC Fulham",  "FC Reading", "Aston Villa", "Port Vale", 
                                                     "FC Chelsea", "Bradford City", "West Ham Utd.", "Leeds United", "Middlesbrough", "FC Everton",
                                                     "AFC Sunderland", "Brighton & Hove", "Hereford Utd.", "Oldham Athletic", "Bournemouth", 
                                                     "FC Southampton", "FC Arsenal"))) %>%
  filter(!(country_from == "England" & !club_from %in% c("Leyton Orient", "Newcastle Utd.", "FC Fulham",  "FC Reading", "Aston Villa", "Port Vale", 
                                                     "FC Chelsea", "Bradford City", "West Ham Utd.", "Leeds United", "Middlesbrough", "FC Everton",
                                                     "AFC Sunderland", "Brighton & Hove", "Hereford Utd.", "Oldham Athletic", "Bournemouth", 
                                                     "FC Southampton", "FC Arsenal"))) %>%
  filter(!(country_from == "Fidschi")) %>%
  filter(!(!country_from == "Ghana" & club_from == "Liberty Prof.")) %>%
  filter(!(country_from == "Ghana" & !club_from == "Liberty Prof.")) %>%
  filter(!(!country_from == "Griechenland" & club_from %in% c("Panathinaikos", "Apollon Smyrnis", "Olymp. Piräus", "AOK Kerkyra", "Platanias", 
                                                          "PAE Veria", "APO Levadiakos", "AO Xanthi"))) %>%
  filter(!(country_from == "Griechenland" & !club_from %in% c("Panathinaikos", "Apollon Smyrnis", "Olymp. Piräus", "AOK Kerkyra", "Platanias", 
                                                          "PAE Veria", "APO Levadiakos", "AO Xanthi"))) %>%
  filter(!(country_from == "Indonesien")) %>%
  filter(!(!country_from == "Irland" & club_from == "Shamrock Rovers")) %>%
  filter(!(country_from == "Irland" & !club_from == "Shamrock Rovers")) %>%
  filter(!(!country_from == "Kasachstan" & club_from == "Aktobe")) %>%
  filter(!(country_from == "Kasachstan" & !club_from == "Aktobe")) %>%
  filter(!(country_from == "Libanon")) %>%
  filter(!(!country_from == "Mazedonien" & club_from %in% c("Shkendija", "FC Shkupi", "Sileks Kratovo", "Renova", "Rabotn. Skopje", 
                                                        "Bregalnica Stip"))) %>%
  filter(!(country_from == "Mazedonien" & !club_from %in% c("Shkendija", "FC Shkupi", "Sileks Kratovo", "Renova", "Rabotn. Skopje", 
                                                        "Bregalnica Stip"))) %>%
  filter(!(country_from == "Montenegro")) %>%
  filter(!(country_from == "Nordirland")) %>%
  filter(!(!country_from == "Ukraine" & club_from %in% c("Dynamo Kiew", "Vorskla", "Zorya Luhansk"))) %>%
  filter(!(country_from == "Ukraine" & !club_from %in% c("Dynamo Kiew", "Vorskla", "Zorya Luhansk"))) %>%
  filter(!(country_from == "Malta"))

# Special cases integration
transfers <- transfers %>%
  mutate(
    country_from = ifelse(country_from == "special case", 
                          ifelse(club_from %in% c("Vereinslos", "Karriereende", "Sperre", "Unbekannt", "pausiert"),
                                 club_from, league_from), country_from),
    country_to = ifelse(country_to == "special case", 
                          ifelse(club_to %in% c("Vereinslos", "Karriereende", "Sperre", "Unbekannt", "pausiert"),
                                 club_to, league_to), country_to)
    )
                            
# IOC country codes for better readability (https://en.wikipedia.org/wiki/List_of_IOC_country_codes)
countries <- unique(c(transfers$country_from, transfers$country_to))
cntr_abbr <- countrycode(countries, "country.name.de", "ioc")

# Countries without IOC match
cntr_abbr[countries == "England"] <- "ENG"
cntr_abbr[countries == "Wales"] <- "WAL"
cntr_abbr[countries == "Schottland"] <- "SCO"
cntr_abbr[countries == "Moldawien"] <- "MDA"
cntr_abbr[countries == "Kosovo"] <- "KOS"

# Special cases (free agents etc.)
cntr_abbr[countries == "Vereinslos"] <- "Vereinslos"
cntr_abbr[countries == "Unbekannt"] <- "Unbekannt"
cntr_abbr[countries == "pausiert"] <- "Pausiert"
cntr_abbr[countries == "Karriereende"] <- "Karriereende"
cntr_abbr[countries == "Sperre"] <- "Sperre"

# Unmatched (league instead of country name)
cntr_abbr[countries == "Primavera A"] <- "ITA"
cntr_abbr[countries == "Primavera B"] <- "ITA"
cntr_abbr[countries == "Primavera C"] <- "ITA"
cntr_abbr[countries == "Liga 1 - Abstiegsrunde"] <- "ROU"
cntr_abbr[countries == "Liga 1 - Meisterrunde"] <- "ROU"
cntr_abbr[countries == "Liga II - Seria II"] <- "ROU"
cntr_abbr[countries == "NB II - West"] <- "HUN"
cntr_abbr[countries == "NB II - West"] <- "LAT"
cntr_abbr[countries == "1.Liga"] <- "LAT"
cntr_abbr[countries == "Football League Nord"] <- "GRE"
cntr_abbr[countries == "Football League Süd"] <- "GRE"
cntr_abbr[countries == "Réunion"] <- "FRA"
cntr_abbr[countries == "II. Liga Promotion"] <- "SVK"
cntr_abbr[countries == "II. Liga West Relegation"] <- "SVK"
cntr_abbr[countries == "II. Liga West Relegation"] <- "SVK"
cntr_abbr[countries == "A Grupa - Relegation gr."] <- "BUL"
cntr_abbr[countries == "Torneo Final"] <- "ARG"
cntr_abbr[countries == "United Football League"] <- "PHI"
cntr_abbr[countries == "King's Cup"] <- "KSA"
cntr_abbr[countries == "Segunda División - Segunda Fase"] <- "URU"

# Apply to countries
transfers$from <- NULL
transfers$to <- NULL

for(i in 1:length(countries)){
  
  transfers$from[transfers$country_from == countries[i]] <- cntr_abbr[i]
  transfers$to[transfers$country_to == countries[i]] <- cntr_abbr[i]
  
}

rm(duplicates, cntr_abbr, countries, i)

# Generate from-to-pairs for international transfers (without special cases)
int_transfers <- transfers %>%
  filter(
    !nchar(from) > 3,
    !nchar(to) > 3
    ) %>%
  mutate(
    from_to = paste0(from, "_", to)
    ) %>%
  group_by(from_to) %>%
  summarise(transfers = n()) %>%
  ungroup() %>%
  separate(from_to, c("from", "to"), sep = "_") %>%
  filter(!from == to) %>%
  mutate(
    from = ifelse(to == "SUI", ifelse(transfers < 5, "other", from), from), # Minimal amount
    to = ifelse(from == "SUI", ifelse(transfers < 5, "other", to), to), # Minimal amount
    from_to = paste0(from, "_", to)
    ) %>%
  group_by(from_to) %>%
  summarise(transfers = sum(transfers)) %>%
  ungroup() %>%
  separate(from_to, c("from", "to"), sep = "_")

# Preparation Chord Diagram: Sorting order
total_transfers <- bind_rows(int_transfers %>%
          mutate(cntr = from) %>%
          select(cntr, transfers),
          int_transfers %>%
            mutate(cntr = to) %>%
            select(cntr, transfers)) %>%
  group_by(cntr) %>%
  summarise(
    transfers = sum(transfers)
  ) %>% 
  filter(!cntr == "SUI") %>%
  arrange(desc(transfers))

# Chord Diagram of transfers TO Switzerland --------------------------------------------------------
int_transfers_to <- int_transfers %>%
  filter(from %in% total_transfers$cntr) %>%
  arrange(desc(from))

# Country order (alphabetical)
country_order <- NULL
country_order <- sort(int_transfers_to$from, decreasing = TRUE)

# Adding Switzerland
country_order <- c(country_order, "SUI")

# Defining symmetry and gaps for Chord Diagram
small_gap <- 3
big_gap <- 50
row_sector_degree <- 360 - (2*big_gap + (length(country_order)-2)*small_gap)

gaps <- c(rep(small_gap, (length(country_order)-2)), big_gap, big_gap)
start_degree = 272.5 - (180 - row_sector_degree)/2

# Colors
col <- c(rep("steelblue3", (length(country_order)-1)), "#E11A27")
col_mat <- as.matrix(rep("gray70", length(country_order)))

# Chord Digramm
circos.par(gap.after = gaps, start.degree = start_degree)

chordDiagram(int_transfers_to, grid.col = col, col = col_mat, order = country_order, annotationTrack = "grid",
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(int_transfers_to))))))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex = 0.9)
}, bg.border = NA)
circos.clear()

rm(big_gap, col, country_order, gaps, row_sector_degree, small_gap, start_degree, int_transfers_to)
# Chord Diagram of transfers FROM Switzerland --------------------------------------------------------
int_transfers_from <- int_transfers %>%
  filter(to %in% total_transfers$cntr)

country_order <- NULL
country_order <- sort(int_transfers_from$to)

# Adding Switzerland
country_order <- c("SUI", country_order)

# Defining symmetry and gaps for Chord Diagram
small_gap <- 3
big_gap <- 50
row_sector_degree <- 360 - (2*big_gap + (length(country_order)-2)*small_gap)

gaps <- c(big_gap, rep(small_gap, (length(country_order)-2)), big_gap)
start_degree = -135 - (180 - row_sector_degree)/2

# Colors
col <- c("#E11A27", rep("steelblue3", (length(country_order)-1)))
col_mat <- as.matrix(rep("gray70", length(country_order)))

# Chord Digramm
circos.par(gap.after = gaps, start.degree = start_degree)

chordDiagram(int_transfers_from, grid.col = col, col = col_mat, order = country_order, annotationTrack = "grid",
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(int_transfers_from))))))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex = 0.9)
}, bg.border = NA)
circos.clear()

rm(big_gap, col, country_order, gaps, row_sector_degree, small_gap, start_degree)
