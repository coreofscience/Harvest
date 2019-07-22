cluster <- function(file, stopwords) {
  
  paperCorp_3 <- tm_map(file, 
                        removeWords, 
                        stopwords)
  
  wordcloud(paperCorp_3, 
            min.freq = 1,
            max.words=50, 
            random.order=FALSE, 
            rot.per=0.35, 
            colors=brewer.pal(8, 
                              "Dark2"))
}