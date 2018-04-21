'Text analysis of 2016 Presidential Debates'

# load file

pacman::p_load(rprojroot)
root <- find_root(is_rstudio_project)

year <- 2016 # between 1960 and 2016 in increments of 4

data <- load(file.path(root,'R Data Files',paste0('E',year,'.Rdata')))

debates <- unique(get(data)[['debate']])

#subset all debates by party
Repub <- get(data)[grep('2016R',get(data)[['debate']]),]
Democ <- get(data)[grep('2016D',get(data)[['debate']]),]

# further subset comments made by candidate
# remove comments made not moderators and others
R_comments <- Repub[which(Repub[['candidate']] == 1),]
D_comments <- Democ[which(Democ[['candidate']] == 1),]

# Get Sentiments
R_comm <- tibble(text = R_comments[['message']])
R_comm %>%
        unnest_tokens(word,text) %>%
        inner_join(get_sentiments("bing"))  %>%
        filter(sentiment %in% c("positive", "negative")) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative)
        #mutate(sentiment = positive - negative) 

for(i in 1:nrow(R_comments[1:10,])) {
        
        clean1 <- tibble(text = books[[i]][[1]])
clean2 <- unnest_tokens(clean1, word, text)
clean3 <- setDT(clean2)[, book := titles[i]]
clean4 <- select(clean3, book, everything())

        series <- rbind(series, clean4)
}



R_comments %>%
        mutate(word_count = 1:n())       %>% 
        inner_join(get_sentiments("bing"))           %>%
        count(book, index = index , sentiment)       %>%
        ungroup()                                    %>%
        spread(sentiment, n, fill = 0)               %>%
        mutate(sentiment = positive - negative,
               book = factor(book, levels = titles)) %>%
  
        ggplot(aes(index, sentiment, fill = book)) +
          geom_bar(alpha = 0.5, 
                   stat = "identity", 
                   show.legend = FALSE) +
          facet_wrap(~ book, ncol = 2, scales = "free_x")
