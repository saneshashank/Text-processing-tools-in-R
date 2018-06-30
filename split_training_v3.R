library(dplyr)
library(stringr)
library(caret)
library(stringr)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

truncate_words <- function(text,word_count)
{
  paste(str_split(text,"\\s+|\\n+")[[1]][1:word_count],collapse = " ")
}

truncate_lines <- function(text,line_count)
  {
    paste(str_split(text,"\\n+")[[1]][1:line_count],collapse = " ")
  }

cleaned_file <- "D:/Data Science/BPO/email_classification/input/Email_Dump_OEM_and_Spares_cleaned_Masked_v4.csv"
raw_input_file <- "D:/Data Science/BPO/email_classification/input/Email_Dump_OEM_and_Spares.csv"
manual_tagged_file <- "D:/Data Science/BPO/email_classification/input/data_cleaned_reduced_manual_tagged_reviewed_0226.csv"

out_dir <- "D:/Data Science/BPO/email_classification/interim"

id_column <- "Case.Number"
text_column <- c("Subject", "Text.Body")
target_column <- "Category"

client_name <- c("honeywell","MyAerospace")

PO_regexp <- "[A-Z0-9]+[A-Z]+[0-9]+"
email_regexp <- "[[:alnum:]._-]+@[[:alnum:].-]+"
phone_regexp <- "\\s*(?:\\+?(\\d{1,3}))?[-. (]*(\\d{1,3})[-. )]*(\\d{3,4})[-. ]*(\\d{3,4})(?: *x(\\d+))?\\s*"
#"(\\+\\d{1,2}\\s)?\\(?\\d{3}\\)?[\\s.-]?\\d{3}[\\s.-]?\\d{4}"

line_cut_off <- 14
min_word_count <- 5

cleaned_df <- read.csv(cleaned_file,stringsAsFactors = F)

rwinp_df <- read.csv(raw_input_file,stringsAsFactors = F)
rwinp_df <- rwinp_df %>%
            dplyr::select(one_of(id_column,text_column[1]))
rwinp_df <- rwinp_df[!duplicated(rwinp_df[,id_column]),]

rwinp_df[,text_column[1]] <- gsub(email_regexp, " some_email@address.domain ",
                                       ignore.case = T,
                                       rwinp_df[,text_column[1]])
rwinp_df[,text_column[1]] <- gsub(phone_regexp, " XXXXXXXXXX ",
                                       rwinp_df[,text_column[1]])

rwinp_df[,text_column[1]] <- gsub(paste(client_name,collapse="|"), " client_name ",
                                       ignore.case = T,
                                       rwinp_df[,text_column[1]])

rwinp_df[,text_column[1]] <- gsub(PO_regexp, " PO_NO ",
                                       rwinp_df[,text_column[1]])


manual_tag_df <- read.csv(manual_tagged_file,stringsAsFactors = F)

new_cl_df <- cleaned_df %>%
          inner_join(rwinp_df,by = id_column)

email_split_tokens <- c("From:",
                        "De:")

email_body_split_tokens <- c("Mit freundlichen Gruben",
                             "Thank you for your assistance",
                             "Thanks and Regards",
                             "Many thanks in advance",
                             "Thanks a lot",
                             "Thanks for the support",
                             "Thank you for your support",
                             "Thanks in advance",
                             "Thanks so much",
                             "Thanks again",
                             "Thank you",
                             "Thanks you",
                             "Thanks & Regards",
                             "Many thanks",
                             "Best",
                             "Thank you in advance for your help",
                             "Thx",
                             "TKS",
                             "Thanks",
                             "Kind regards",
                             "Best regards",
                             "Cordialement",
                             "Saludos",
                             "Disclaimer",
                             "Sincerely",
                             "Brgds",
                             "With Regards",
                             "Krgds",
                             "Regards",
                             "Have a Wonderful Day",
                             "Please do not respond to this message",
                             "Visit the exciting new options",
                             "Rgds",
                             "This e-mail message and any attachment(s)",
                             "Please hurry"
)

email_header_terms <- c("from",
                       "to",
                       "cc",
                       "bcc",
                       "subject",
                       "asunto",
                       "sent",
                       "para",
                       "enviado el",
                       "tarih",
                       "konu",
                       "importance",
                       "enviada em"
                       )


email_body_split_regex <- ""
for (i in 1:length(email_body_split_tokens))
{
  email_body_split_regex <- paste0(email_body_split_regex,
                                email_body_split_tokens[i],
                                "[[:punct:]+]|",
                                email_body_split_tokens[i],
                                "\n")
  if(i != length(email_body_split_tokens))
  {
    email_body_split_regex <- paste0(email_body_split_regex,"|")
  }
}

email_split_regex <- paste0(email_split_tokens,collapse = "|")

email_header_terms_regex <- ""
for (i in 1:length(email_header_terms))
{
  email_header_terms_regex <- paste0(email_header_terms_regex,
                                     email_header_terms[i],
                                     "\\s?:")
  if(i != length(email_header_terms))
  {
    email_header_terms_regex <- paste0(email_header_terms_regex,"|")
  }
}

email_split <- str_split(new_cl_df[,text_column[2]],
                                  regex(email_split_regex,
                                        ignore_case = TRUE))
email_texts <- c()
for(i in 1:length(email_split))
{
  email_bodies <- ""
  for(j in 1:length(email_split[[i]]))
  {
    email_body_split <- str_split(paste(email_split[[i]][j]),
                             regex(email_body_split_regex,
                                   ignore_case = TRUE))
    email_body <- email_body_split[[1]][1]
    if (j != 1)
    {
      email_lines <- str_split(email_body,"\n+")
      email_body <- ""
      for(k in 2:length(email_lines[[1]]))
      {
        if(!grepl(email_header_terms_regex,email_lines[[1]][k],ignore.case = T))
        {
          #print(email_lines[[1]][k])
          email_body <- paste(email_body,email_lines[[1]][k],sep = " \n")
        }
      }
    }
    email_body <- trim(email_body)
    email_lines <- str_split(email_body,"\n+")
    if(length(email_lines[[1]]) > line_cut_off)
    {
      print(paste("truncating case:email -",i,":",j ,
                  "as original line count is:",length(email_lines[[1]])))
      email_body <- truncate_lines(email_body,line_cut_off)
    }
    if(j == 1)
    {
      email_bodies = email_body
    } else {
      email_bodies <- paste(email_bodies,email_body,
                            sep = " \n------------------\n")
    }
    text_body_word_count <- str_count(email_body,"\\S+|\\n+")
    if(text_body_word_count > min_word_count) break
  }
  email_texts <- c(email_texts,email_bodies)
}

#new_cl_df[,text_column[2]] <- unlist(lapply(email_split, `[[`, 1))

#new_cl_df[,text_column[2]] <- trim(new_cl_df[,text_column[2]])

new_cl_df[,paste0("cleaned_",text_column[2])] <- email_texts
expr1 <- lazyeval::interp(quote(!(is.na(x) | x == y)), 
                          x = as.name(text_column[2]),y = "")
new_cl_df <- new_cl_df %>%
            filter_(expr1) %>%
            select(-one_of(target_column)) %>%
            inner_join(manual_tag_df,by=id_column)

write.csv(new_cl_df,paste0(out_dir,"/","all_cleaned_and_reduced_",Sys.Date(),".csv"),row.names = F)            

# set.seed(32)
# intrainandval <-createDataPartition(y=new_cl_df$Category,p=0.9,list=FALSE)
# trainandval <- new_cl_df[intrainandval,]
# testing <- new_cl_df[-intrainandval,]
# 
# text_body_line_count <- apply(trainandval,2,str_count,"\\n+")[,"Text.Body"]
# summary(text_body_line_count)
# hist(text_body_line_count,breaks = 50)
# line_cut_off <- ceiling(summary(text_body_line_count)[5] + 1.5 * IQR(text_body_line_count))
# 
# 
# text_body_word_count <- apply(trainandval,2,str_count,"\\S+|\\n+")[,"Text.Body"]
# summary(text_body_word_count)
# hist(text_body_word_count,breaks = 100)
# word_cut_off <- ceiling(summary(text_body_word_count)[5] + 1.5 * IQR(text_body_word_count))
# 
# verify_df <- cbind.data.frame(trainandval,text_body_word_count,text_body_line_count,
#                               stringsAsFactors = F)
# verify_df <- verify_df[text_body_line_count > line_cut_off | text_body_word_count  > word_cut_off,]
# 
# word_truncated_IQR <- c()
# word_truncated_200 <- c()
# line_truncated_IQR <- c()
# line_truncated_50 <- c()
# 
# for(i in 1:nrow(verify_df))
# {
#   
#   if(verify_df[i,"text_body_word_count"] > word_cut_off)
#   {
#     word_truncated_IQR <- c(word_truncated_IQR,
#                         truncate_words(verify_df[i,"Text.Body"],word_cut_off))
#   }
#   else {
#     word_truncated_IQR <- c(word_truncated_IQR,"no change")
#   }
#   
#   
#   if(verify_df[i,"text_body_word_count"] > 200)
#   {
#     word_truncated_200 <- c(word_truncated_200,
#                             truncate_words(verify_df[i,"Text.Body"],200))
#   }
#   else {
#     word_truncated_200 <- c(word_truncated_200,"no change")
#   }
#   
#   
#   if(verify_df[i,"text_body_line_count"] > line_cut_off)
#   {
#     line_truncated_IQR <- c(line_truncated_IQR,
#                         truncate_lines(verify_df[i,"Text.Body"],line_cut_off))
#   }
#   else {
#     line_truncated_IQR <- c(line_truncated_IQR,"no change")
#   }
#   
#   
#   if(verify_df[i,"text_body_line_count"] > 50)
#   {
#     line_truncated_50 <- c(line_truncated_50,
#                             truncate_lines(verify_df[i,"Text.Body"],50))
#   }
#   else {
#     line_truncated_50 <- c(line_truncated_50,"no change")
#   }
#   
# }
# 
# verify_df <- cbind.data.frame(verify_df,
#                               word_truncated_IQR,
#                               word_truncated_200,
#                               line_truncated_IQR,
#                               line_truncated_50,
#                               stringsAsFactors = F)
# 
# 
# write.csv(verify_df,paste0(out_dir,"/","verify_cutoff.csv"),row.names = F)
# 
# text_body_all_line_count <- apply(new_cl_df,2,str_count,"\\n+")[,"Text.Body"]
# summary(text_body_all_line_count)
# hist(text_body_all_line_count,breaks = 50)
# 
# for(i in 1:nrow(new_cl_df))
# {
# 
#   if(text_body_all_line_count[i] > line_cut_off)
#   {
#     new_cl_df[i,"Text.Body"] <- truncate_lines(new_cl_df[i,"Text.Body"],
#                                                line_cut_off)
#   }
# 
# }

new_cl_df <- new_cl_df %>%
          select(-one_of(text_column[2]))

names(new_cl_df) <- c(id_column,text_column,target_column)
new_cl_df[,target_column] <- make.names(trim(tolower(new_cl_df[,target_column])))

set.seed(32)
intrainandval <-createDataPartition(y=new_cl_df[,target_column],p=0.9,list=FALSE)
trainandval <- new_cl_df[intrainandval,]
testing <- new_cl_df[-intrainandval,]

set.seed(32)
intrain <-createDataPartition(y=trainandval[,target_column],p=0.8,list=FALSE)
training <- trainandval[intrain,]
validation <- trainandval[-intrain,]

# n <- 3
# nr <- nrow(training)
# 
# split_dfs <- split(training, rep(1:n, each=ceiling(nr/n), length.out=nr))
# 
# 
# for (i in 1:length(split_dfs))
# {
#   train_file_location <- paste0(out_dir,"/","train_split_",i,".csv")
#   write.csv(split_dfs[i],train_file_location,row.names = F)
# }

write.csv(training,paste0(out_dir,"/","cleaned_training_",Sys.Date(),".csv"),row.names = F)
write.csv(validation,paste0(out_dir,"/","validation_",Sys.Date(),".csv"),row.names = F)
write.csv(testing,paste0(out_dir,"/","blind_test_",Sys.Date(),".csv"),row.names = F)



