library(bibliometrix)
library(igraph)
library(tidyverse)
library(roadoi) # titles
library(fulltext) # Abstract
library(tm)
library(SnowballC)
library(wordcloud)
library(cluster)
M<- convert2df(D, dbsource="isi",format="plaintext")
M$ID_WOS <- rownames(M)

M$ID_WOS <- paste(M$ID_WOS,M$VL,sep = ", V")
M$ID_WOS <- paste(M$ID_WOS,M$PG,sep = ", P")
M$ID_WOS <- paste(M$ID_WOS,M$DI,sep = ", DOI ")
  enlaces <- data.frame(ID_WOS=character(),
                      CR=character(),
                      stringsAsFactors = FALSE)

for (i in M$ID_WOS) { 
  row1=M[M$ID_WOS==i,c("ID_WOS","CR")]
  df1=data.frame(ID_WOS=i,CR=strsplit(row1$CR,";"))
  colnames(df1)=c("ID_WOS","CR")

  enlaces=rbind(enlaces,df1)
  }
grafo_1 <-graph.data.frame(enlaces,directed = TRUE)
grafo_2 <-delete.vertices(grafo_1,which(degree(grafo_1, mode = "in")==1 & degree(grafo_1, mode = "out")==0))
giant.component <- function(graph) {
cl <- clusters(graph)
induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))}
grafo_3 <- giant.component(grafo_2)
clusters <- cluster_walktrap(grafo_3)
clusters_mx <- cbind(clusters$names, clusters$membership)
clusters_df <- data.frame(clusters_mx, 
                          stringsAsFactors = FALSE)
names(clusters_df) <- c("ID_WOS", "cluster")

clusters_3 <- head(clusters_df %>% count(cluster, sort = TRUE), 3)

df_clusters <- clusters_df[clusters_df$cluster == clusters_3$cluster, ]
df_clusters_1 <- clusters_df[clusters_df$cluster == clusters_3$cluster[1],]
df_clusters_2 <- clusters_df[clusters_df$cluster == clusters_3$cluster[2],]
df_clusters_3 <- clusters_df[clusters_df$cluster == clusters_3$cluster[3],]

raw_data <- 
  df_clusters_1 %>%
  rename(id = "ID_WOS") %>%
  mutate(id = str_to_lower(id))

raw_data_1 <- 
  raw_data %>%
  dplyr::filter(grepl(".*doi", id))

raw_data_1$doi <- sub(".*doi", "", raw_data_1$id)


raw_data_1 <- 
  raw_data_1 %>%
  mutate(doi = str_trim(doi))

df <- data.frame(titulo = as.character(),
                 stringsAsFactors = FALSE)
for (i in raw_data_1$doi) {
  row = try(oadoi_fetch(dois = i,
                   email = correo),
            TRUE)
  if(isTRUE(class(row)=="try-error")) {next} else {
    df_new = data.frame(titulo = row$title, 
                      stringsAsFactors = FALSE)
  }
  
  df = rbind(df_new, df)
}
jeopCorpus <- Corpus(VectorSource(df$titulo %>% na.omit()))

paperCorp <- jeopCorpus
paperCorp <- tm_map(paperCorp, removePunctuation)
paperCorp <- tm_map(paperCorp, removeNumbers)
# added tolower
paperCorp <- tm_map(paperCorp, content_transformer(tolower))
paperCorp <- tm_map(paperCorp, removeWords, stopwords("english"))
# moved stripWhitespace

paperCorp <- tm_map(paperCorp, stripWhitespace)
paperCorp <- tm_map(paperCorp, stemDocument)

paperCorp_1 <- tm_map(paperCorp, removeWords, c("also", "article", "Article", 
                                               "download", "google", "figure",
                                               "fig", "groups","Google", "however",
                                               "high", "human", "levels",
                                               "larger", "may", "number",
                                               "shown", "study", "studies", "this",
                                               "using", "two", "the", "Scholar",
                                               "pubmedncbi", "PubMedNCBI",
                                               "view", "View", "the", "biol",
                                               "via", "image", "doi", "one",
                                              "brand", "corpor"))

nube1 <- wordcloud(paperCorp_1, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

raw_data_2 <- 
  df_clusters_2 %>%
  rename(id = "ID_WOS") %>%
  mutate(id = str_to_lower(id))

raw_data_1_2 <- 
  raw_data_2 %>%
  dplyr::filter(grepl(".*doi", id))

raw_data_1_2$doi <- sub(".*doi", "", raw_data_1_2$id)


raw_data_1_2 <- 
  raw_data_1_2 %>%
  mutate(doi = str_trim(doi))

df_2 <- data.frame(titulo = as.character(),
                 stringsAsFactors = FALSE)
for (i in raw_data_1_2$doi) {
  row = try(oadoi_fetch(dois = i,
                   email = correo),
            TRUE)
  if(isTRUE(class(row)=="try-error")) {next} else {
    df_new = data.frame(titulo = row$title, 
                      stringsAsFactors = FALSE)
  }
  
  df_2 = rbind(df_new, df_2)
}
jeopCorpus_2 <- Corpus(VectorSource(df_2$titulo %>% na.omit()))

paperCorp_2 <- jeopCorpus_2
paperCorp_2 <- tm_map(paperCorp_2, removePunctuation)
paperCorp_2 <- tm_map(paperCorp_2, removeNumbers)
# added tolower
paperCorp_2 <- tm_map(paperCorp_2, content_transformer(tolower))
paperCorp_2 <- tm_map(paperCorp_2, removeWords, stopwords("english"))
# moved stripWhitespace

paperCorp_2 <- tm_map(paperCorp_2, stripWhitespace)
paperCorp_2 <- tm_map(paperCorp_2, stemDocument)

paperCorp_2 <- tm_map(paperCorp_2, removeWords, c("also", "article", "Article", 
                                               "download", "google", "figure",
                                               "fig", "groups","Google", "however",
                                               "high", "human", "levels",
                                               "larger", "may", "number",
                                               "shown", "study", "studies", "this",
                                               "using", "two", "the", "Scholar",
                                               "pubmedncbi", "PubMedNCBI",
                                               "view", "View", "the", "biol",
                                               "via", "image", "doi", "one",
                                               "brand", "corpor"))

nube2 <- wordcloud(paperCorp_2, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

raw_data_3 <- 
  df_clusters_3 %>%
  rename(id = "ID_WOS") %>%
  mutate(id = str_to_lower(id))

raw_data_1_3 <- 
  raw_data_3 %>%
  dplyr::filter(grepl(".*doi", id))

raw_data_1_3$doi <- sub(".*doi", "", raw_data_1_3$id)


raw_data_1_3 <- 
  raw_data_1_3 %>%
  mutate(doi = str_trim(doi))

df_3 <- data.frame(titulo = as.character(),
                 stringsAsFactors = FALSE)
for (i in raw_data_1_3$doi) {
  row = try(oadoi_fetch(dois = i,
                   email = correo),
            TRUE)
  if(isTRUE(class(row)=="try-error")) {next} else {
    df_new = data.frame(titulo = row$title, 
                      stringsAsFactors = FALSE)
  }
  
  df_3 = rbind(df_new, df_3)
}

jeopCorpus_3 <- Corpus(VectorSource(df_3$titulo %>% na.omit()))

paperCorp_3 <- jeopCorpus_3
paperCorp_3 <- tm_map(paperCorp_3, removePunctuation)
paperCorp_3 <- tm_map(paperCorp_3, removeNumbers)
# added tolower
paperCorp_3 <- tm_map(paperCorp_3, content_transformer(tolower))
paperCorp_3 <- tm_map(paperCorp_3, removeWords, stopwords("english"))
# moved stripWhitespace

paperCorp_3 <- tm_map(paperCorp_3, stripWhitespace)
paperCorp_3 <- tm_map(paperCorp_3, stemDocument)

paperCorp_3 <- tm_map(paperCorp_3, removeWords, c("also", "article", "Article", 
                                               "download", "google", "figure",
                                               "fig", "groups","Google", "however",
                                               "high", "human", "levels",
                                               "larger", "may", "number",
                                               "shown", "study", "studies", "this",
                                               "using", "two", "the", "Scholar",
                                               "pubmedncbi", "PubMedNCBI",
                                               "view", "View", "the", "biol",
                                               "via", "image", "doi", "one",
                                               "brand", "corpor"))

nube3 <- wordcloud(paperCorp_3, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

list(df=M,grafo=grafo_3,nube1=nube1,nube2=nube2,nube3=nube3)
}

