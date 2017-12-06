
### ---------------------- SINGLE-ITEM PART OF SPEECH ANNOTATOR ---------------------- ###
## This annotator uses openNLP, an R interface which allows you utilize the Apache OpenNLP tools, to tag sentences by part of speech
## OpenNLP uses the Penn Treebank notation style
## Documentation: https://cran.r-project.org/web/packages/openNLP/openNLP.pdf
## Notation Legend: https://cran.r-project.org/web/packages/openNLP/openNLP.pdf 
##
## Author: Josephine Lukito (jlukito@wisc.edu) 

#pkgs <- c("dplyr", "readr", "tidyr", "stringr", "tokenizers", "openNLP", "openNLPmodels.en", "NLP", "gsubfn")
#install.packages(pkgs)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(tokenizers)
library(openNLP)
library(openNLPmodels.en)
library(NLP)
library(gsubfn)

getwd()
# setwd("C:/users/jlukito/Documents/R_Tutorials")

abs <- read.csv("abs.csv", header = TRUE, sep = ",") ##insert a csv file here with your text
text_df <- (data_frame(text = as.character(abs[[1]])))
s <- unlist(text_df)
s2 <- gsub("Â", "", s) ##removes "Â" character (This annotator does not clean data)

### ---------------------- ANNOTATOR ---------------------- ###
sent_token_annotator <-Maxent_Sent_Token_Annotator()
word_token_annotator <-Maxent_Word_Token_Annotator()
a2 <-annotate(s2, list(sent_token_annotator, word_token_annotator)) ##tags sentences and words as individual tokens
pos_tag_annotator <-Maxent_POS_Tag_Annotator() ##calls a part of speech tagger
a3 <-annotate(s2, pos_tag_annotator, a2) #assigns pos tag to word tokens identified
a3w <-subset(a3, type == "word") ##creates a subset of this list that only includes words (sentence tokens are deleted)
tags <-sapply(a3w$features, '[[', "POS") ##produces separate list of POS tags
table(tags) ## produces table of frequency of individual tags
a3w <- a3[a3$type == "word"] ## dataframe of only words (excludes sentences)
POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
s_tr = paste0(s2, collapse = " ") #collapses all the strings in s2
sswl <- unlist(lapply(a3, function(x) substr(s_tr, x$start, x$end))) ## sentence list based on annotator (must occur before next line)
sswl <- unlist(lapply(a3w, function(x) substr(s_tr, x$start, x$end))) ## splits sentence list by words
POStagged <- sprintf("%s/%s", sswl, POStags) ## creates word-POS pairs
a6 <- list(sswl = sswl, POStags = POStags) #creates a matrix which includes each word and its coorresponding POS tag

### ---------------------- EXPORTER ---------------------- ###
writeLines(paste(trialfull), fileConn)
close(fileConn)
write.csv(a6, "abstract_separated.csv") #2 lists, one of words, one of tags
write.csv(POStagged, "abstract_joinedPOS.csv") #joins the list of words and tags together

trialfull <- unlist(POStagged) %>%
  c (collapse = " ")
write.table(trialfull, "abstract.txt", sep = "\t") #produces text file of abstract_joinedPOS