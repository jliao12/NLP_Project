library(dplyr)
library(quanteda)
library(datasets)
library(data.table)


filecon <- file("~/Desktop/datascience_r/Course materials/Capstone/final/en_US/en_US.twitter.txt",encoding = "UTF-8")
US_Twitter <- readLines(filecon,encoding = "UTF-8",skipNul = T)
SMT <- sample(length(US_Twitter), length(US_Twitter) * 0.3)
close(filecon)

filecon <- file("~/Desktop/datascience_r/Course materials/Capstone/final/en_US/en_US.blogs.txt",encoding = "UTF-8")
US_Blogs <- readLines(filecon,encoding = "UTF-8",skipNul = T)
SMB <- sample(length(US_Blogs), length(US_Blogs) * 0.3)
close(filecon)


filecon <- file("~/Desktop/datascience_r/Course materials/Capstone/final/en_US/en_US.news.txt",encoding = "UTF-8")
US_News <- readLines(filecon,encoding = "UTF-8",skipNul = T)
SMN <- sample(length(US_News), length(US_News) * 0.3)
close(filecon)


US_Twitter_Sample <- US_Twitter[SMT]
US_Blogs_Sample <- US_Blogs[SMB]
US_News_Sample <- US_News[SMN]

rm(US_Twitter) 
rm(US_Blogs)
rm(US_News)
rm(SMT)
rm(SMN)
rm(SMB)

#Combine samples and remove some unessary parts
removeConfounders <- function(x) {
  x <- stri_replace_all_regex(x, c("-",":"," -","- ",";"),
                              c(" "," "," "," "," "),vectorize = F)
  
  x <- stri_replace_all_regex(x, c("won't","can't","'re","'ve","what's",
                                   "n't","'d","'ll","'m|’m"),
                              c("will not","cannot"," are"," have","what is",
                                " not"," would"," will"," am"),vectorize = F)
  
  x <- stri_replace_all_regex(x, c("^rt "," rt ","^lol | lol[:punct:]"," lol "),
                              c(""," ",""," "),vectorize = F)
  x <- stri_replace_all_regex(x, c("[^[:alpha:][:space:]]*"),
                              c(""),vectorize = F)
}
master_vector <- c(US_Twitter_Sample, US_Blogs_Sample, US_News_Sample)

rm(US_Twitter_Sample)
rm(US_Blogs_Sample)
rm(US_News_Sample)

master_vector <- str_remove_all(master_vector, "[^[\\da-zA-Z ]]")
master_vector <- removeConfounders(master_vector)
corp <- corpus(master_vector)
master_Tokens <- tokens(
  x = tolower(corp),
  remove_punct = TRUE,
  remove_twitter = TRUE,
  remove_numbers = TRUE,
  remove_hyphens = TRUE,
  remove_symbols = TRUE,
  remove_url = TRUE
)

master_Tokens <- tokens_select(master_Tokens, pattern = stopwords("en"), selection = "remove")

rm(master_vector)
rm(corp)

stemed_words <- tokens_wordstem(master_Tokens, language = "english")

rm(master_Tokens)

bi_gram <- tokens_ngrams(stemed_words, n = 2)
tri_gram <- tokens_ngrams(stemed_words, n = 3)
qua_gram <- tokens_ngrams(stemed_words, n = 4)

uni_DFM <- dfm(stemed_words)

rm(stemed_words)

bi_DFM <- dfm(bi_gram)
tri_DFM <- dfm(tri_gram)
qua_DFM <- dfm(qua_gram)

rm(bi_gram)
rm(tri_gram)
rm(qua_gram)

uni_DFM <- dfm_trim(uni_DFM, 3)
bi_DFM <- dfm_trim(bi_DFM, 3)
tri_DFM <- dfm_trim(tri_DFM, 3)
qua_DFM <- dfm_trim(qua_DFM, 3)

# Create named vectors with counts of words 
sums_U <- colSums(uni_DFM)
sums_B <- colSums(bi_DFM)
sums_T <- colSums(tri_DFM)
sums_Q <- colSums(qua_DFM)

rm(uni_DFM)
rm(bi_DFM)
rm(tri_DFM)
rm(qua_DFM)

# Create data tables with individual words as columns
uni_words <- data.table(word_1 = names(sums_U), count = sums_U)


bi_words <- data.table(
  word_1 = sapply(strsplit(names(sums_B), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(sums_B), "_", fixed = TRUE), '[[', 2),
  count = sums_B)


tri_words <- data.table(
  word_1 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 2),
  word_3 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 3),
  count = sums_T)

qua_words <- data.table(
  word_1 = sapply(strsplit(names(sums_Q), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(sums_Q), "_", fixed = TRUE), '[[', 2),
  word_3 = sapply(strsplit(names(sums_Q), "_", fixed = TRUE), '[[', 3),
  word_4 = sapply(strsplit(names(sums_Q), "_", fixed = TRUE), '[[', 4),
  count = sums_T)

rm(sums_B)
rm(sums_T)
rm(sums_U)
rm(sums_Q)


setkey(uni_words, word_1)
setkey(bi_words, word_1, word_2)
setkey(tri_words, word_1, word_2, word_3)
setkey(qua_words, word_1, word_2, word_3, word_4)

discount_value <- 0.75

######## Finding Bi-Gram Probability #################

# Finding number of bi-gram words
NoBwC <- nrow(bi_words[by = .(word_1, word_2)])

# Dividing number of times word 2 occurs as second part of bigram, by total number of bigrams.  
# ( Finding probability for a word given the number of times it was second word of a bigram)
ckn <- bi_words[, .(Prob = ((.N) /NoBwC)), by = word_2]
setkey(ckn, word_2)

# Assigning the probabilities as second word of bigram, to unigrams
uni_words[, Prob := ckn[word_1, Prob]]
uni_words <- uni_words[!is.na(uni_words$Prob)]
rm(ckn)
# Finding number of times word 1 occurred as word 1 of bi-grams
n1w1 <- bi_words[, .(N = .N), by = word_1]
setkey(n1w1, word_1)

# Assigning total times word 1 occured to bigram cn1
bi_words[, Cn1 := uni_words[word_1, count]]

# Kneser Kney Algorithm
bi_words[, Prob := ((count - discount_value) / Cn1 + discount_value / Cn1 * n1w1[word_1, N] * uni_words[word_2, Prob])]

rm(n1w1)
######## End of Finding Bi-Gram Probability #################

######## Finding Tri-Gram Probability #################

# Finding count of word1-word2 combination in bigram 
tri_words<- as.data.frame(tri_words)
tri_words$word_1_2 <- paste(tri_words$word_1,tri_words$word_2,sep = " ")
bi_list <- bi_words
bi_list$word_1_2 <- paste(bi_list$word_1,bi_list$word_2,sep = " ")
bi_list <- as.data.table(cbind(bi_list$count, bi_list$word_1_2))
setnames(bi_list, c("V1", "V2"), c("Cn2","word_1_2"))
tri_words <- merge(tri_words, bi_list, by="word_1_2")
tri_words <- tri_words[,-1]
tri_words$Cn2 <- as.integer(tri_words$Cn2)
tri_words <- as.data.table(tri_words)
bi_list <- NULL

# Finding count of word1-word2 combination in trigram
n1w12 <- tri_words[, .N, by = .(word_1, word_2)]
setkey(n1w12, word_1, word_2)
setkey(bi_words, word_1, word_2)

# Kneser Kney Algorithm
tri_words[, Prob := (count - discount_value) / Cn2 + discount_value / Cn2 * n1w12[.(word_1, word_2), N] *bi_words[.(word_1, word_2), Prob]]
rm(n1w12)
######## End of Finding Tri-Gram Probability #################

qua_words<- as.data.frame(qua_words)
qua_words$word_1_2_3 <- paste(qua_words$word_1,qua_words$word_2,qua_words$word_3,sep = " ")
tri_list <- tri_words
tri_list$word_1_2_3 <- paste(tri_list$word_1,tri_list$word_2,tri_list$word_3,sep = " ")
tri_list <- as.data.table(cbind(tri_list$count, tri_list$word_1_2_3))
setnames(tri_list, c("V1", "V2"), c("Cn3","word_1_2_3"))
qua_words <- merge(qua_words, tri_list, by="word_1_2_3")
qua_words <- qua_words[,-1]
qua_words$Cn3 <- as.integer(qua_words$Cn3)
qua_words <- as.data.table(qua_words)
tri_list <- NULL

n1w123 <- qua_words[, .N, by = .(word_1, word_2,word_3)]
setkey(n1w123, word_1, word_2, word_3)
setkey(tri_words, word_1, word_2, word_3)
qua_words[, Prob := (count - discount_value) / Cn3 + discount_value / Cn3 * n1w123[.(word_1, word_2,word_3), N] *tri_words[.(word_1, word_2,word_3), Prob]]
rm(n1w12)




# Finding the most frequently used 10 unigrmas
uni_words <- uni_words[order(-Prob)][1:10]

# function to return highly probable previous word given two successive words

quaWords <- function(w1, w2, w3,n = 5) {
  pwords <- qua_words[word_1 == w1 & word_2 == w2 & word_3 == w3][order(-Prob)]
  if (any(is.na(pwords))) {
    return(tri(w1,w2,n))
  }
  else if (nrow(pwords) > n) {
    return(list("qua"=pwords[1:n, word_4]))
  } else {
    count <- nrow(pwords)
    twords <- triWords(w1,w2,n)$tri[1:(n - count)]
    bwords <- biWords(w2, n)$bi[1:(n - count)]
    return(list("qua" = pwords[,word_4],"tri" = twords,"bi" = bwords))
  }
  
}








triWords <- function(w1, w2, n = 5) {
  pwords <- tri_words[word_1 == w1 & word_2 == w2][order(-Prob)]
  if (any(is.na(pwords))) {
    return(biWords(w2, n))
  }
  else if (nrow(pwords) > n) {
    return(list("tri"=pwords[1:n, word_3]))
  } else {
    count <- nrow(pwords)
    bwords <- biWords(w2, n)$bi[1:(n - count)]
    return(list("tri" = pwords[,word_3],"bi" = bwords))
  }
  
}

# function to return highly probable previous word given a word
biWords <- function(w1, n = 5) {
  pwords <- bi_words[w1][order(-Prob)]
  if (any(is.na(pwords))) {
    return(uniWords(n))
  } else if (nrow(pwords) > n) {
    return(list("bi" = pwords[1:n, word_2]))
  } else {
    count <- nrow(pwords)
    unWords <- uniWords(n)$uni[1:(n - count)]
    return(list("bi" = pwords[, word_2], "uni" = unWords))
  }
}

# function to return random words from unigrams
uniWords <- function(n = 5) {  
  return(list("uni" = sample(uni_words[, word_1], size = n)))
}

# The prediction app
getWords <- function(str){
  require(quanteda)
  tokens <- tokens(x = char_tolower(str))
  tokens <- char_wordstem(rev(rev(tokens[[1]])[1:3]), language = "english")
  
  words <- quaWords(tokens[1], tokens[2],tokens[3], 5)
  
  return(words)
}