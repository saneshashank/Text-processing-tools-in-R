
library(tm)
library(slam)
library(RWeka)
library(wordcloud)
library(openNLP)

library(plyr)
library(dplyr)
library(reshape2)
library(lazyeval)
library(tidyr)
library(stringi)
library(stringr)
library(corrplot)

library(caret)

dtm.generate <- function(df,ng,
                         sparse=1,
                         spl_sym = "",
                         my_stop_word_file="",
                         keep.id=FALSE,
                         remove_non_english_char = TRUE,
                         removeNum = TRUE,
                         removePunc = TRUE,
                         removeStpWords = TRUE,
                         doStem = TRUE,
                         doIDF = TRUE,
                         doNormTf = TRUE)
{
  
  if (remove_non_english_char)
  {
    for(c in 1:ncol(df))
    {
      
      df[,colnames(df)[c]] <- gsub("[^\x20-\x7E]", "",
                                   df[,colnames(df)[c]])
    }
  }
  if(keep.id == TRUE)
  {
    # tutorial on rweka - http://tm.r-forge.r-project.org/faq.html
    m <- list(id = "ID", content = colnames(df)[2])
    myReader <- readTabular(mapping = m)
    
    corpus <- VCorpus(DataframeSource(df), readerControl = list(reader = myReader))
    
    # Manually keep ID information from http://stackoverflow.com/a/14852502/1036500
    #       for (i in 1:length(corpus)) {
    #         attr(corpus[[i]], "id") <- df$ID[i]
    #         #corpus[[i]]$meta$ID <- df$ID[i]
    #       }
  } else
  {
    corpus <- Corpus(VectorSource(df[,1])) # create corpus for TM processing
  }
  
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, stripWhitespace)
  if (removeNum) corpus <- tm_map(corpus, removeNumbers) 
  if(length(spl_sym) > 0 & sum(nchar(spl_sym)) > 0)
  {
    corpus <- tm_map(corpus, content_transformer(gsub), 
                     pattern = paste(spl_sym,collapse="|"), replacement = " ")
  }
  if (removePunc) corpus <- tm_map(corpus, removePunctuation)
  if (doStem) corpus <- tm_map(corpus, stemDocument)
  if (removeStpWords) corpus <- tm_map(corpus, removeWords, 
                                       c(stopwords("SMART"),stopwords("english")))
  if(my_stop_word_file !="")
  {
    content_specific_stop_words <- read.csv(my_stop_word_file,
                                            header=F,stringsAsFactors = F)
    corpus <- tm_map(corpus, removeWords, content_specific_stop_words[,1])
  }
  #corpus <- tm_map(corpus, PlainTextDocument)
  if (ng >1)
  {
    options(mc.cores=1) # http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka/20251039#20251039
    # this stopped working in new server environment
    #BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ng, max = ng)) # create n-grams
    nGramTokenizer <-
      function(x)
        unlist(lapply(ngrams(words(x), ng), paste, collapse = " "), use.names = FALSE)
    if(doIDF)
    {
      dtm <- DocumentTermMatrix(corpus, control = list(tokenize = nGramTokenizer,
                                                       weighting = function(x)                                                          
                                                         weightTfIdf(x, normalize = doNormTf))) # create tdm from n-grams
    } else
    {
      dtm <- DocumentTermMatrix(corpus, control = list(tokenize = nGramTokenizer,
                                                       weighting = weightTf)) # create tdm from n-grams
    }
    
  }
  else
  {
    if(doIDF)
    {
      dtm <- DocumentTermMatrix(corpus,control = list(weighting = function(x)                                                          
        weightTfIdf(x, normalize = doNormTf)))
    } else
    {
      dtm <- DocumentTermMatrix(corpus,control = list(weighting = weightTf))
    }
    
  }
  if(sparse != 1)
  {
    dtms <- removeSparseTerms(dtm, sparse)
  }
  else
  {
    dtms <- dtm
  }
  
  dtms
}


remove.unclustered.dtm <- function(dtm)
{
  rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
  dtm.non_uncl <- dtm[rowTotals> 0,] 
  dtm.non_uncl
}


text_column <- c("Subject","Description")
training_file <- "D:/Data Science/BPO/email_classification/processed/train_val1_partial_masked_2018-03-08.csv"
testing_file <- "D:/Data Science/BPO/email_classification/processed/validation2_partial_masked_2018-03-08.csv"
# read training file
train_data_df <- read.csv(training_file,
                        stringsAsFactors = F)

## Data fields formatting

if (length(text_column) >1)
{
  train_data_df[,text_column[1]] <- as.data.frame(apply(train_data_df[,
                                                                      make.names(text_column)],
                                                        1, paste,collapse=" "),
                                                  stringsAsFactors = FALSE) 
}

train_data_df$is_fyi <- ifelse(train_data_df$Category == make.names(tolower("For your information cases")) |
                           train_data_df$Category == make.names(tolower("Internal Team Request to order status team")),
                         "yes","no")

train_data_df$is_fyi <- as.factor(train_data_df$is_fyi)
train_data_df$Category <- make.names(train_data_df$Category)
train_data_df$Category <- as.factor(train_data_df$Category)



train_data_df[,text_column[1]] <- gsub("[x]{2,}"," ",
                                ignore.case = T,
                                train_data_df[,text_column[1]])

data_keywordsDF <- cbind.data.frame(train_data_df$Case.Number,train_data_df[,text_column[1]],
                                    stringsAsFactors = FALSE)

names(data_keywordsDF) <- c("ID","content")
# Stop words
special_symbols <- c("\\/","\\.",":")
stop_words_file <- "D:/Data Science/BPO/email_classification/input/stop_words_not_masked.csv"


## DTM creation

content_DTM_1 <- dtm.generate(data_keywordsDF,
                              1,.995,
                              spl_sym = special_symbols,
                              my_stop_word_file = stop_words_file,
                              keep.id=TRUE,
                              doIDF = FALSE)

#summary(slam::col_sums(content_DTM_1))

term1_tfidf <- tapply(content_DTM_1$v/slam::row_sums(content_DTM_1)[content_DTM_1$i], 
                      content_DTM_1$j, mean) *
  log2(tm::nDocs(content_DTM_1)/slam::col_sums(content_DTM_1 > 0))

summary(term1_tfidf)

term1_tfidf_iqr <- summary(term1_tfidf)[5] - summary(term1_tfidf)[2]

content_DTM_1.reduced <- content_DTM_1[,
                                       term1_tfidf < (summary(term1_tfidf)[5] 
                                                      + 1.5 *  term1_tfidf_iqr) &
                                         term1_tfidf > (summary(term1_tfidf)[2] 
                                                        - 1.5 * term1_tfidf_iqr)]

# content_DTM_1.reduced <- content_DTM_1[,
#                    term1_tfidf >= (summary(term1_tfidf)[2])]

summary(slam::col_sums(content_DTM_1.reduced))


content_DTM_2 <- dtm.generate(data_keywordsDF,
                              2,0.995,
                              spl_sym = special_symbols,
                              my_stop_word_file = stop_words_file,
                              keep.id=TRUE,
                              doIDF = FALSE)

term2_tfidf <- tapply(content_DTM_2$v/slam::row_sums(content_DTM_2)[content_DTM_2$i], 
                      content_DTM_2$j, mean) *
  log2(tm::nDocs(content_DTM_2)/slam::col_sums(content_DTM_2 > 0))

summary(term2_tfidf)

term2_tfidf_iqr <- summary(term2_tfidf)[5] - summary(term2_tfidf)[2]

content_DTM_2.reduced <- content_DTM_2[,
                                       term2_tfidf < (summary(term2_tfidf)[5] 
                                                      + 1.5 *  term2_tfidf_iqr) &
                                         term2_tfidf >= (summary(term2_tfidf)[2] - 
                                                           1.5 * term2_tfidf_iqr)]

# content_DTM_2.reduced <- content_DTM_2[,
#                       term2_tfidf >= (summary(term2_tfidf)[2])]
summary(slam::col_sums(content_DTM_2.reduced))

content_DTM_3 <- dtm.generate(data_keywordsDF,
                              3,0.995,
                              spl_sym = special_symbols,
                              my_stop_word_file = stop_words_file,
                              keep.id=TRUE,
                              doIDF = FALSE)

term3_tfidf <- tapply(content_DTM_3$v/slam::row_sums(content_DTM_3)[content_DTM_3$i], 
                      content_DTM_3$j, mean) *
  log2(tm::nDocs(content_DTM_3)/slam::col_sums(content_DTM_3 > 0))

summary(term3_tfidf)

term3_tfidf_iqr <- summary(term3_tfidf)[5] - summary(term3_tfidf)[2]

content_DTM_3.reduced <- content_DTM_3[,
                                       term3_tfidf < (summary(term3_tfidf)[5] 
                                                      + 1.5 *  term3_tfidf_iqr) &
                                         term3_tfidf >= (summary(term3_tfidf)[2] 
                                                         - 1.5 * term3_tfidf_iqr)]


content_DTM <- cbind(content_DTM_3.reduced,content_DTM_2.reduced,content_DTM_1.reduced)


content_DTM.uncl <- remove.unclustered.dtm(content_DTM)

### Prepare Data for training

con_dtm_df <- as.data.frame.matrix(content_DTM.uncl)
colnames(con_dtm_df) <- make.names(colnames(con_dtm_df))
con_dtm_df$Case.Number <- rownames(con_dtm_df)
con_dtm_df <- train_data_df %>% 
  dplyr::inner_join(con_dtm_df,by = "Case.Number") %>%
  dplyr::select(-one_of(text_column))


### Read testing data
test_data_df <- read.csv(testing_file,
                          stringsAsFactors = F)

## Data fields formatting

if (length(text_column) >1)
{
  test_data_df[,text_column[1]] <- as.data.frame(apply(test_data_df[,
                                                                      make.names(text_column)],
                                                        1, paste,collapse=" "),
                                                  stringsAsFactors = FALSE) 
}

test_data_df$is_fyi <- ifelse(test_data_df$Category == "For your information cases" |
                                 test_data_df$Category == "Internal Team Request to order status team",
                               "yes","no")

test_data_df$is_fyi <- as.factor(test_data_df$is_fyi)
test_data_df$Category <- make.names(test_data_df$Category)
test_data_df$Category <- as.factor(test_data_df$Category)



test_data_df[,text_column[1]] <- gsub("[x]{2,}"," ",
                                       ignore.case = T,
                                       test_data_df[,text_column[1]])

### Prepare Data for testing

test_keywordsDF <- cbind.data.frame(test_data_df$Case.Number,test_data_df[,text_column[1]],
                                    stringsAsFactors = FALSE)

names(test_keywordsDF) <- c("ID","content")

test_DTM_1 <- dtm.generate(test_keywordsDF,
                           1,1,
                           spl_sym = special_symbols,
                           my_stop_word_file = stop_words_file,
                           keep.id=TRUE,
                           doIDF = FALSE)
test_DTM_2 <- dtm.generate(test_keywordsDF,
                           2,1,
                           spl_sym = special_symbols,
                           my_stop_word_file = stop_words_file,
                           keep.id=TRUE,
                           doIDF = FALSE)

test_DTM_3 <- dtm.generate(test_keywordsDF,
                           3,1,
                           spl_sym = special_symbols,
                           my_stop_word_file = stop_words_file,
                           keep.id=TRUE,
                           doIDF = FALSE)

test_DTM <- cbind(test_DTM_3,test_DTM_2,test_DTM_1)

test_dtm_df <- as.data.frame.matrix(test_DTM)
colnames(test_dtm_df) <- make.names(colnames(test_dtm_df))
test_dtm_df$Case.Number <- rownames(test_dtm_df)
test_dtm_df <- test_data_df %>% 
  dplyr::inner_join(test_dtm_df,by = "Case.Number") %>%
  dplyr::select(-one_of(text_column))



### Intermediate Feature files

#intermediate dtm files

interim_dir <- "D:/Data Science/BPO/email_classification/interim/"
train_dtm_file <- paste(interim_dir,"/masked_train_dtm_",Sys.Date(),".csv",sep="")

write.csv(con_dtm_df,train_dtm_file, row.names = F)


#intermediate term to class mapping

# term_class_summary <- function(x)
# {
#   N <- length(x)
#   nt_adj <- (1 + (sum(x != 0,na.rm = T)))
#   idf <- log((N - nt_adj)/nt_adj)
#   sum_value <- sum(x * idf, na.rm = T)/ nt_adj
#   return(sum_value)
# }
# class_prop <- con_dtm_df %>%
#   group_by(Category) %>%
#   summarise(n = n()) %>%
#   mutate(inv_freq = sum(n)/n)
# 
# 
# term_class_mapping <- con_dtm_df %>%
#   select(-one_of("Case.Number")) %>%
#   group_by(Category) %>%
#   summarise_all(term_class_summary)
# 
# term_class_mapping_mat <- as.matrix(term_class_mapping[,-1]) * class_prop$inv_freq
# 
# term_class_mappingDF <- cbind.data.frame(Category = term_class_mapping$Category,
#                                          term_class_mapping_mat)
# 
# train_class_term_file <- paste(interim_dir,"/term_class_mapping.csv",sep="")
# 
# write.csv(term_class_mappingDF,train_class_term_file, row.names = F)



#intermediate term to class mapping only tf

class_prop <- con_dtm_df %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(inv_freq = sum(n)/n)


term_class_mapping <- con_dtm_df %>%
  dplyr::select(-one_of("Case.Number","is_fyi")) %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise_all(sum)

term_class_mapping_mat <- as.matrix(term_class_mapping[,-1]) * class_prop$inv_freq

term_class_TF_prop_mappingDF <- cbind.data.frame(Category = term_class_mapping$Category,
                                                 term_class_mapping_mat)

train_class_term_file <- paste(interim_dir,"/mask_train_term_class_mapping_only_tf_",Sys.Date(),".csv",sep="")

write.csv(term_class_mapping,train_class_term_file, row.names = F)

train_class_term_file <- paste(interim_dir,"/mask_train_term_class_mapping_tf_prop_",Sys.Date(),".csv",sep="")

write.csv(term_class_TF_prop_mappingDF,train_class_term_file, row.names = F)



#intermediate test DTM aligned with train

train_same_cols <- intersect(colnames(con_dtm_df),colnames(test_dtm_df))
train_diff_cols <- setdiff(colnames(con_dtm_df),colnames(test_dtm_df))

test_dtm_int_df <- test_dtm_df[,train_same_cols]
temp_dummy <- read.table(textConnection(""), col.names = train_diff_cols,
                         colClasses = "integer")

test_dtm_int_df <- bind_rows(test_dtm_int_df, temp_dummy)

test_dtm_int_df[is.na(test_dtm_int_df)] <- 0

test_file <- paste(interim_dir,"/mask_testing_dtm_aligned_to_train_",Sys.Date(),".csv",sep="")

write.csv(test_dtm_int_df,test_file, row.names = F)


