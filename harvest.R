library(bibliometrix)
library(igraph)
library(tidyverse)
library(roadoi) # titles
library(fulltext) # Abstract
library(tm)
library(SnowballC)
library(wordcloud)
library(cluster)
D <- readFiles("/cloud/project/EM.txt")

