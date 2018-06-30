### this file will be called from init_config.R. Don't execute the file alone.

dynamic_require <- function(lib){
  if(eval(parse(text=paste("require(",lib,")")))) 
  {
    return()
  }
  
  install.packages(lib,dep=TRUE,repos='https://cran.cnr.berkeley.edu/')
  if (!eval(parse(text=paste("require(",lib,")")))) 
  {
    stop("Error! library cannot be installed - ",lib)
  }
}

#dynamic_require("xlsx")
dynamic_require("openxlsx")
dynamic_require("plyr")
dynamic_require("dplyr")
dynamic_require("reshape2")
dynamic_require("stringr")

dynamic_require("tm")
dynamic_require("slam")
dynamic_require("RWeka")

dynamic_require("ggplot2")
dynamic_require("wordcloud")

dynamic_require("topicmodels")
dynamic_require("Rmpfr")


dtm.generate <- function(df,ng,sparse=1,my_stop_word_file="",keep.id=FALSE){
  
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
    corpus <- Corpus(VectorSource(df)) # create corpus for TM processing
  }
  
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, c(stopwords("SMART"),stopwords("english")))
  corpus <- tm_map(corpus, removeNumbers) 
  if(length(spl_sym) > 0 & sum(nchar(spl_sym)) > 0)
  {
    corpus <- tm_map(corpus, content_transformer(gsub), 
                     pattern = paste(spl_sym,collapse="|"), replacement = " ", fixed=TRUE)
  }
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stemDocument)
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
    dtm <- DocumentTermMatrix(corpus, 
                              control = list(tokenize = nGramTokenizer,
                                             weighting = weightTf)) # create tdm from n-grams
  }
  else
  {
    dtm <- DocumentTermMatrix(corpus,control = list(weighting = weightTf))
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

wf.generate <- function(dtm,
                        wc_freq,
                        wc_freq_scale,
                        type_string,
                        plot_dir = plot_directory)
{
  
  freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE) 
  wf <- data.frame(word=names(freq), freq=freq)   
  

  wc_freq_filename <- paste(plot_dir,"/",type_string,"_","wc_freq_",wc_freq,".png",sep="")
  png(wc_freq_filename,
      width=5,
      height=5, 
      units='in',
      res=200)
  #par(mfrow = c(1, 1))
  
  set.seed(142)   
  wordcloud(names(freq), freq, min.freq=2,max.words=wc_freq, 
            rot.per=0.15, scale=wc_freq_scale, 
            random.order=FALSE,
            colors=brewer.pal(6, "Dark2")) 
  dev.off()
  wf
}

harmonicMean <- function(logLikelihoods, precision=2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

generate.lda <- function (dtm,topic_start_num = 50,topic_end_num =  100,
                          topic_num_interval = 5,type, max_iteration = 6,
                          burnin = 1000,iter = 1000,keep = 50, plot_dir = plot_directory)
{
  
  old_coeff <- 0
  for (i in 1:max_iteration)
  {
      if(exists("fitted_many")) remove(fitted_many)
      
      sequ <- seq(topic_start_num, topic_end_num, topic_num_interval) # in this case a sequence of numbers from 10 to 50, by 2.
      
      set.seed(123)
      fitted_many <- lapply(sequ, function(k) LDA(dtm, 
                                                  k = k, 
                                                  method = "Gibbs",
                                                  control = list(burnin = burnin, 
                                                                 iter = iter, 
                                                                 keep = keep) ))
      
      # extract logliks from each topic
      logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])
      
      # compute harmonic means
      hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))
      
      plot_filename <- paste(plot_dir,"/",type,
                             "_topic_logLikelyhood_",
                             topic_start_num,"_",
                             topic_end_num,"_",
                             topic_num_interval,
                             ".png",sep="")
      png(plot_filename)
      # inspect
      plot(sequ, hm_many, type = "l")
      lmModel <- lm(hm_many ~ sequ)
      abline(lmModel,col="blue", lty=2, lwd=2)
      dev.off()
      coeff <- (lmModel$coefficients[2]) /((max(hm_many)-min(hm_many))/(max(sequ)-min(sequ)))
      message(paste(Sys.time(),": Iteration ",i," - coefficient: ",coeff, " - # topics:",sequ[which.max(hm_many)]))
      
      change_interval <- 0
      if((sign(old_coeff) != 0) & (sign(coeff) != sign(old_coeff)))
      {
        if(topic_num_interval == 1) break 
        old_start <- topic_start_num
        old_end <- topic_end_num
        old_interval <- topic_num_interval
        change_interval <- 1
        
      }else if(coeff > tan(max_slope_angle * pi/180)) ## this means absolute slope angle is greater than a certain value in degree
      {
        old_start <- topic_start_num
        old_end <- topic_end_num
        if (sequ[which.max(hm_many)] != old_start)
        {
          topic_start_num <- sequ[(which.max(hm_many) - 1)]
        } else break
        
        if (topic_start_num < 2) topic_start_num <- 2
        topic_end_num <- topic_start_num + (old_end - old_start)
        if (old_start == topic_start_num & old_end == topic_end_num)
        {
          old_interval <- topic_num_interval
          change_interval <- 1
        }
        
      }else if (coeff < tan(-1 * max_slope_angle * pi/180)) ## this means absolute slope angle is less than a certain value in degree
      {
        old_start <- topic_start_num
        old_end <- topic_end_num
        if (sequ[which.max(hm_many)] != old_end)
        {
          topic_end_num <- sequ[(which.max(hm_many) + 1)]
        } else break
        topic_start_num <- topic_end_num - (old_end - old_start)
        if (topic_start_num < 2) topic_start_num <- 2
        
        if (old_start == topic_start_num & old_end == topic_end_num)
        {
          old_interval <- topic_num_interval
          change_interval <- 1
        }
      }
      else
      {
        if(topic_num_interval == 1) break 
        old_start <- topic_start_num
        old_end <- topic_end_num
        old_interval <- topic_num_interval
        change_interval <- 1
      }
      
      if(change_interval == 1)
      {
        topic_num_interval <- round(sqrt(topic_num_interval))
        topic_start_num <- sequ[which.max(hm_many)] - 
          round(((old_end - old_start)/(2 * old_interval))*topic_num_interval)
        topic_end_num <- sequ[which.max(hm_many)] + 
          round(((old_end - old_start)/(2 * old_interval))*topic_num_interval)
        if (topic_start_num < 2) topic_start_num <- 2
      }
      old_coeff <- coeff
      message(paste("For next iteration topic_num_interval: ",topic_num_interval,
                  ", topic_start_num: ",topic_start_num,
                  ", topic_end_num: ",topic_end_num))
  }
  
  # compute optimum number of topics
  #print(sequ[which.max(hm_many)])
  
  ldaModel <- fitted_many[[which.max(hm_many)]]
  
  ldaModel
  
}

proc.unclustered <- function(topic_string)
{
  uncl_updated <- sapply(topic_string, function(x){
    if(grepl("^UNCLUSTERED",x))
    {
      x <- "UNCLUSTERED"
    }  else x <- gsub("\\|UNCLUSTERED", "", x)
    x
  })
  
  uncl_updated
}

generate.topic.terms <- function (ldaModel,num_topic_terms = 5)

{
  lda.topics <- as.data.frame(topics(ldaModel))
  names(lda.topics) <- "topic_id"
  lda.terms <- t(as.matrix(terms(ldaModel,num_topic_terms)))
  
  lda.terms.combined <- matrix(apply(as.data.frame(lda.terms),1,
                                     paste,collapse="|"))
  lda.topic.terms <- dplyr::mutate(lda.topics,
                            topics = proc.unclustered(lda.terms.combined[topic_id]))
  
  lda.topic.terms
}

add.unclustered.dtm <- function(dtm)
{
  rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
  rowTotals[rowTotals== 0] <- -1
  rowTotals[rowTotals> 0] <- 0
  rowTotals[rowTotals== -1] <- 1
  
  unclustered <- matrix(rowTotals,nrow=dtm$nrow)
  
  dtm.uncl <- cbind(dtm,unclustered)
  colnames(dtm.uncl)[ncol(dtm.uncl)] <- "UNCLUSTERED"
  dtm.uncl
}

remove.unclustered.dtm <- function(dtm)
{
  rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
  dtm.non_uncl <- dtm[rowTotals> 0,] 
  dtm.non_uncl
}

get.match.score <- function(all_string,words)
{
  score <- sum((str_count(all_string, words)) * 
                 (str_count(words,'\\w+') ^ str_count(words,'\\w+')))
  score
}
get.match.score.v2 <- function(all_string,words)
{
  score <- ((str_count(all_string, words)) * 
                 (str_count(words,'\\w+') ^ str_count(words,'\\w+')))
  score
}


addUniqueTerms.DTM <- function(dtm_dest,dtm_src)
{
  
  if (length(dtm_src$dimnames[[2]]) > 0 & length(dtm_dest$dimnames[[2]]) > 0)
  {
    for(i in 1:length(dtm_src$dimnames[[2]]))
    {
      match_count <- 0
      for(j in 1:length(dtm_dest$dimnames[[2]]))
      {
        if(get.match.score(dtm_dest$dimnames[[2]][j],dtm_src$dimnames[[2]][i]) >= 1)
        {
          match_count <- match_count + 1
        }
      }
      if(match_count == 0)
      {
        #print(dtm_src$dimnames[[2]][i])
        dtm_dest <- cbind(dtm_dest,dtm_src[,dtm_src$dimnames[[2]][i]])
      }
      
    }
  } else if (length(dtm_src$dimnames[[2]]) > 0)
  {
    dtm_dest <- dtm_src
  }
  
  dtm_dest
}

cons.dtms <- function(dtm1,dtm2,dtm3,flag1,flag2,flag3,unique_terms = FALSE)
{
  mul_dtm <- 0
  
  dtm <- NULL
  
  if (!is.null(flag3) & length(flag3) > 0 & 
        (tolower(flag3) == "yes" | 
           tolower(flag3) == "y"))
  {
    if (!is.null(dtm3) & length(dtm3$dimnames[[2]]) > 0)
    {
      dtm <- dtm3
    }
    
    mul_dtm <- 1
  }
  
  
  if (!is.null(flag2) & length(flag2) > 0 & 
        (tolower(flag2) == "yes" | 
           tolower(flag2) == "y"))
  {
    if (mul_dtm == 1)
    {
      #dtm <- addUniqueTerms.DTM(dtm,dtm2)
      if (!is.null(dtm2) & length(dtm2$dimnames[[2]]) > 0 &
          !is.null(dtm) & length(dtm$dimnames[[2]]) > 0)
      {
        
        if(unique_terms)
        {
          dtm <- addUniqueTerms.DTM(dtm,dtm2)
        }else dtm <- cbind(dtm,dtm2)
        
      } else if (!is.null(dtm2) & length(dtm2$dimnames[[2]]) > 0)
      {
        dtm <- dtm2
      }
    } else dtm <- dtm2
    
    mul_dtm <- 1
  }
  
  if (!is.null(flag1) & length(flag1) > 0 & 
        (tolower(flag1) == "yes" | 
           tolower(flag1) == "y"))
  {
    if (mul_dtm == 1)
    {
      #dtm <- addUniqueTerms.DTM(dtm,dtm1)
      if (!is.null(dtm1) & length(dtm1$dimnames[[2]]) > 0 &
            !is.null(dtm) & length(dtm$dimnames[[2]]) > 0)
      {
        if(unique_terms)
        {
          dtm <- addUniqueTerms.DTM(dtm,dtm1)
        }else dtm <- cbind(dtm,dtm1)
      } else if(!is.null(dtm1) & length(dtm1$dimnames[[2]]) > 0)
      {
        dtm <- dtm1
      }
    } else dtm <- dtm1
    mul_dtm <- 1
  }  

  dtm  
  
}


getTermTopPatterns <- function(patternDF,max_pattern_num,score_thres = 0,is_probability_input = FALSE)
{
  pattern_wtDF <- patternDF
  
  pattern_wtDF[pattern_wtDF < score_thres] <- 0
  
  term.top_patterns <- data.frame("top_patterns" = character(nrow(pattern_wtDF)), 
                                  "top_probs" = character(nrow(pattern_wtDF)),
                                  stringsAsFactors=FALSE)
  for(i in 1:nrow(pattern_wtDF))
  {
    
    if(max(pattern_wtDF[i,], na.rm = TRUE) > 0)
    {
      col_indx <- order(pattern_wtDF[i,],decreasing=TRUE)[1:max_pattern_num]
      top_patterns <- c()
      top_probs <- c()
      for (j in 1:length(col_indx))
      {
        if(!is.na(pattern_wtDF[i,(col_indx[j])]) & pattern_wtDF[i,(col_indx[j])] > 0)
        {
          top_patterns <- c(top_patterns,
                            colnames(pattern_wtDF)[col_indx[j]])
          if (is_probability_input)
          {
            top_probs <- c(top_probs,round(pattern_wtDF[i,(col_indx[j])],2))
          }else
          {
            top_probs <- c(top_probs,round((pattern_wtDF[i,(col_indx[j])]/rowSums(patternDF[i,],na.rm=T)),2))
          }
        }
      }
      
    }
    else
    {
      top_patterns <- "No pattern found"
      top_probs <- 0
    }
    
    term.top_patterns[i,"top_patterns"] <- paste(top_patterns,collapse="; ")
    term.top_patterns[i,"top_probs"] <- paste(top_probs,collapse="; ")
  }
  #term.top_patterns[,"ID"] <- rownames(pattern_wtDF)
  term.top_patterns
}
getDocPatternMatchScore <- function(doc_pattern_scoreDF,doc_termsDF,pattern_termsDF)
{
  # weighted by term frequency in docs
  a <- (doc_pattern_scoreDF[as.character(doc_termsDF$variable),as.character(pattern_termsDF$variable)]
         * t(doc_termsDF$value))
  # weighted by term frequency in patterns
  b <- t(t(a) * pattern_termsDF$value)
  # scaled by term frequency in other patterns
  c <- t(t(b)/(pattern_termsDF$tot))
  score <- sum(c)
  score
    
}
getDocPatternMatchScoreV2 <- function(doc_pattern_scoreDF,doc_termsDF,pattern_termsDF)
{
  
  # weighted by term frequency in docs
  if (length(doc_termsDF$value) > 1)
  {
    a <- (diag(doc_termsDF$value) %*% 
          as.matrix(doc_pattern_scoreDF[as.character(doc_termsDF$variable),as.character(pattern_termsDF$variable)]))
  } else 
  {
    a <- ((doc_termsDF$value) %*% 
            as.matrix(doc_pattern_scoreDF[as.character(doc_termsDF$variable),as.character(pattern_termsDF$variable)]))
  }
        
  # weighted by term frequency in patterns
  
  if (length(pattern_termsDF$value) > 1)
  {
    b <- a %*% diag(pattern_termsDF$value)
  } else
  {
    b <- a %*% (pattern_termsDF$value)
  }
  
  # scaled by term frequency in other patterns
  if (length(pattern_termsDF$tot) > 1)
  {
    c <- b %*% diag(1/pattern_termsDF$tot)
  } else 
  {
    c <- b %*% (1/pattern_termsDF$tot)
  }
  score <- sum(c)
  score
  
}
getTermPatternMatchScore <- function(dt,p_pt)
{
  
  p_pt$ID<-seq.int(nrow(p_pt))
  p_pt.mdf <- dplyr::filter(melt(p_pt, id = c("ID"),
                                       na.rm = TRUE),value > 0)
  #remove(p_pt.df)
  p_pt.mdf <- p_pt.mdf[order(p_pt.mdf$ID),]
  p_pt.mdf <- p_pt.mdf %>% 
    dplyr::group_by(variable) %>%
    dplyr::mutate(tot=sum(value) * n())
  
  pt <- as.character(unique(p_pt.mdf$variable))
  
  dt_pt_score <- data.frame(row.names = dt)
  
  for (i in 1:length(pt))
  {
    dt_pt_score <- cbind(dt_pt_score,get.match.score.v2(dt, pt[i]))
  }
  colnames(dt_pt_score) <- pt
  dt_pt_score <- dt_pt_score[,order(names(dt_pt_score))]
  
  p_pt.mdf <- p_pt.mdf %>% dplyr::mutate(prob = value/tot)
  pt_p_prob <- dcast(p_pt.mdf, variable ~ ID, value.var = "prob",row.names=NULL)
  pt_p_prob [is.na(pt_p_prob)] <- 0
  pt_p_prob$variable <- as.character(pt_p_prob$variable)
  
  pto_p_prob <- pt_p_prob[order(pt_p_prob[,"variable"]),] 
  
  dt_p_prob_mat <- as.matrix(dt_pt_score) %*% 
                                         as.matrix(pto_p_prob[,2:ncol(pto_p_prob)])
  
  dt_p_prob <- as.data.frame(dt_p_prob_mat)
  list(dt_p_prob = dt_p_prob,dt_pt_score = dt_pt_score)
  
}
getKeyWordProbMat <- function(dtm4RecDF,dtmPat4RecDF,term.top_pattern)
{
  dtm4RecDF <- dtm4RecDF[,setdiff(names(dtm4RecDF),names(dtmPat4RecDF))]
  dtm4RecDF[,"ID"] <- rownames(dtm4RecDF)
  names(term.top_pattern) <- c("ID","top_patterns")
  
  dtmDocPat4RecDF <- inner_join(dtm4RecDF,term.top_pattern,
                                by = "ID")
  
  remove(dtm4RecDF,term.top_pattern)
  
  pat_term_sum4RecDF <- dtmDocPat4RecDF %>% 
    dplyr::select(- ID) %>%
    dplyr::group_by(top_patterns) %>%
    dplyr::summarise_each(funs(sum))
  
  pat_term_n4RecDF <- dtmDocPat4RecDF %>% 
    dplyr::select(- ID) %>%
    dplyr::group_by(top_patterns) %>%
    dplyr::summarise_each(funs(sum(!is.na(.) & (. > 0))))
  
  tot_term_sum4RecV <- colSums(pat_term_sum4RecDF[,2:ncol(pat_term_sum4RecDF)])
  
  count_term_n4RecV <- colSums(pat_term_n4RecDF[,2:ncol(pat_term_n4RecDF)])
  
  term_recom_weight_mat <- pat_term_sum4RecDF[,2:ncol(pat_term_sum4RecDF)] * 
    pat_term_n4RecDF[,2:ncol(pat_term_n4RecDF)]
  
  term_rec_denom <- tot_term_sum4RecV * count_term_n4RecV
  
  
  term_recom_prob_mat <- as.matrix(term_recom_weight_mat) %*% 
                                            diag(1/term_rec_denom)
  term_recom_prob_df <- as.data.frame(term_recom_prob_mat)
  names(term_recom_prob_df) <- names(term_recom_weight_mat)
  rownames(term_recom_prob_df) <- pat_term_n4RecDF$top_patterns
  
  term_recom_prob_df
}
getMatchedKeywords <- function(docTermFreqDF)
{
  termIndex <- (apply(docTermFreqDF,1,function(x) which(x > 0)))
  matched_keywords <- lapply(termIndex,names)
  matched_keywords <- gsub("c(", "", matched_keywords, fixed = TRUE)
  matched_keywords <- gsub(")", "", matched_keywords, fixed = TRUE)
  matched_keywords <- gsub("\"", "", matched_keywords, fixed = TRUE)
  matched_keywords <- gsub("character(0", "", matched_keywords, fixed = TRUE)
  docMatchedKeywords<- as.data.frame(matched_keywords,row.names = rownames(docTermFreqDF),
                                     stringsAsFactors = "False")
  docMatchedKeywords[,"ID"]<- rownames(docTermFreqDF)
  docMatchedKeywords
}

remove_zero_cols <- function(df) {
  rem_vec <- NULL
  for(i in 1:ncol(df)){
    this_sum <- summary(df[,i])
    zero_test <- length(which(this_sum == 0))
    if(zero_test == 6) {
      rem_vec[i] <- names(df)[i]
    }
  }
  features_to_remove <- rem_vec[!is.na(rem_vec)]
  rem_ind <- which(names(df) %in% features_to_remove)
  df <- df[,-rem_ind]
  return(df)
}

getDocumentPatternMatchScore <- function(docTermScoreList,d_dt,p_pt,min_d_p_t_match = 1)
{
  
  ## doc to pattern term match
  dt_pt_score <- docTermScoreList$dt_pt_score
  dt_pt_score <- dt_pt_score[order(rownames(dt_pt_score)),]
  dt_ptr_score <- remove_zero_cols(dt_pt_score)
  d_ptr_score_mat <- (as.matrix(d_dt))%*% 
                                 (as.matrix(dt_ptr_score))
  ## doc to pattern match freq
  d_ptr_score_mat[d_ptr_score_mat > 0] <- 1
  d_ptr_score <- as.data.frame(d_ptr_score_mat)
  
  
  
  p_pt <- p_pt[,order(colnames(p_pt))]
  p_pt[p_pt > 0] <- 1
  p_ptr <- p_pt[,intersect(colnames(p_pt),colnames(d_ptr_score))]
  
  d_p_freq_mat <- d_ptr_score_mat %*% t(as.matrix(p_ptr))
  remove(d_ptr_score_mat)
  d_p_freq_mat[d_p_freq_mat < min_d_p_t_match] <- 0
  d_p_freq <- as.data.frame(d_p_freq_mat)
  #d_p_freq[d_p_freq < min_d_p_t_match] <- 0
  
  ## doc to pattern weighted score
  dt_p_prob <- docTermScoreList$dt_p_prob
  remove(docTermScoreList)
  dt_p_prob <- dt_p_prob[order(rownames(dt_p_prob)),]
  d_p_prob_mat <- (as.matrix(d_dt))%*% 
                              (as.matrix(dt_p_prob))
  d_p_prob <- as.data.frame(d_p_prob_mat)
  d_p_prob_red <- d_p_prob[,intersect(colnames(d_p_prob),colnames(d_p_freq))]
  d_p_freq_red <- d_p_freq[,intersect(colnames(d_p_prob),colnames(d_p_freq))]
  d_p_prob_calc <- d_p_prob_red * d_p_freq_red
  
  list(d_p_prob = d_p_prob_calc,d_ptr_score = d_ptr_score)
  
}
 
split_config <- function(value)
{
  if(nchar(value) > 0)
  {
    value_list <- str_split(value,",")
    
    values <- value_list[[1]]
    
    values <- gsub("^\\s+|\\s+$", "", values)
    values <- ifelse(grepl("[^0-9.]",values),values,
                     as.numeric(values))
  } else values <- ""
  
  values
}


doIDFWeightReduction <- function(content_DTM)
{
  term_tfidf <- tapply(content_DTM$v/slam::row_sums(content_DTM)[content_DTM$i], 
                        content_DTM$j, mean) *
    log2(tm::nDocs(content_DTM)/slam::col_sums(content_DTM > 0))
  
  
  content_DTM.reduced <- content_DTM[,term_tfidf >= summary(term_tfidf)[3]]
  content_DTM.reduced
}


#### function for reading INI file ####

Parse.INI <- function(INI.filename) 
{ 
  connection <- file(INI.filename) 
  Lines  <- readLines(connection) 
  close(connection) 
  
  connection <- textConnection(Lines) 
  d <- read.table(connection, as.is = TRUE, sep = "=", fill = TRUE,
                  strip.white = TRUE) 
  close(connection) 
  
  ToParse  <- paste("INI.list$", d$V1, " <- '", 
                    d$V2, "'", sep="") 
  
  INI.list <- list() 
  eval(parse(text=ToParse)) 
  
  return(INI.list) 
}