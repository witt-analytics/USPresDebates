'Text analysis of 2016 Presidential Debates'

# load file

pacman::p_load(rprojroot)
root <- find_root(is_rstudio_project)

files <- list.files(file.path(root,'R Data Files'), 
                    pattern = '.Rdata',
                    full.names = T)

years <- substr(basename(files), 2, 5)

GOP <- data.frame(message = NULL, year = NULL, stringsAsFactors = F)
DEM <- data.frame(message = NULL, year = NULL, stringsAsFactors = F)

for(i in years){
  
  data <- load(file.path(root,'R Data Files',paste0('E',i,'.Rdata')))
  cand <- subset(get(data), candidate == 1)
  mess <- which(colnames(get(data)) == 'message')
  elec <- which(colnames(get(data)) == 'election')
  if(is.na(elec[1])) cand$election = i
  elec <- which(colnames(cand) == 'election')
  rep  <- subset(cand, party == 'R')
  dem  <- subset(cand, party == 'D')
  Rep  <- subset(rep, substr(debate,5,5) == "P")
  Dem  <- subset(dem, substr(debate,5,5) == "P")

  
  GOP <- rbind(GOP, Rep[,c(mess,elec)])
  DEM <- rbind(DEM, Dem[,c(mess,elec)])
  
}

GOP$party = 'R'
DEM$party = 'D'
comm <- rbind(GOP, DEM)

# Get Sentiments
comm %>% 
     unnest_tokens(word,message) %>%
      group_by(election, party) %>% 
        mutate(word_count = 1:n(),
               index = election)       %>% 
        inner_join(get_sentiments("nrc"))           %>%
        count(party, index = index , sentiment)       %>%
        ungroup()                                    %>%
        spread(sentiment, n, fill = 0)               %>%
        mutate(sentiment = positive - negative) -> test3

test3 %>%
         ggplot(aes(index, sentiment, fill = party)) +
          geom_bar(alpha = 0.5, 
                   stat = "identity", 
                   show.legend = FALSE) +
          facet_wrap(~ party, ncol = 2, scales = "free_x")