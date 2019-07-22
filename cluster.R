cluster <- function(file, stopwords) {
  wordcloud(paperCorp_3, min.freq = 1,
            max.words=50, 
            random.order=FALSE, 
            rot.per=0.35, 
            colors=brewer.pal(8, 
                              "Dark2"))
}