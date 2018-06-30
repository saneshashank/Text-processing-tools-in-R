library(dplyr)
library(stringr)
library(caret)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

truncate_words <- function(text,word_count)
{
  paste(str_split(text,"\\s+|\\n+")[[1]][1:word_count],collapse = " ")
}

truncate_lines <- function(text,line_count)
{
  paste(str_split(text,"\\n+")[[1]][1:line_count],collapse = " ")
}

raw_input_file <- "D:/Data Science/BPO/email_classification/input/Email_Dump_OEM_and_Spares.csv"
manual_tagged_file <- "D:/Data Science/BPO/email_classification/input/data_cleaned_reduced_manual_tagged_reviewed_0308.csv"

out_dir <- "D:/Data Science/BPO/email_classification/processed"

top_category_list <- c("Shipment Status",
                       "For your information cases",
                       "Expedite request",
                       "Internal Team Request to order status team",
                       "Quote status"
                      )

id_column <- "Case.Number"
text_column <- c("Subject","Description")
is_text_col_multiple_email <- c(FALSE, TRUE)
target_column <- "Category"
date_column <- "Message.Date"


client_name <- c("honeywell","MyAerospace")
PO_regexp <- "[A-Z0-9]+[A-Z]+[0-9]+"


email_regexp <- "[[:alnum:]._-]+@[[:alnum:].-]+"
phone_regexp <- "\\s*(?:\\+?(\\d{1,3}))?[-. (]*(\\d{1,3})[-. )]*(\\d{3,4})[-. ]*(\\d{3,4})(?: *x(\\d+))?\\s*"
#"(\\+\\d{1,2}\\s)?\\(?\\d{3}\\)?[\\s.-]?\\d{3}[\\s.-]?\\d{4}"

line_cut_off <- 11
min_word_count <- 5

email_split_tokens <- c("From",
                        "De")

email_body_split_tokens <- c("Mit freundlichen Gruben",
                             "Thank you for your assistance",
                             "Thanks and Regards",
                             "Many thanks in advance",
                             "Thanks a lot",
                             "Thanks for the support",
                             "Thanks for your help",
                             "Thanks for all",
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
                             "Caution",
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
                        "enviada em")


email_body_split_regex <- ""
for (i in 1:length(email_body_split_tokens))
{
  email_body_split_regex <- paste0(email_body_split_regex,
                                   email_body_split_tokens[i],
                                   "\\s?[[:punct:]+]|",
                                   email_body_split_tokens[i],
                                   "\\s?\n")
  if(i != length(email_body_split_tokens))
  {
    email_body_split_regex <- paste0(email_body_split_regex,"|")
  }
}

email_split_regex <- paste0(email_split_tokens,collapse = "|")

for (i in 1:length(email_split_tokens))
{
  email_split_regex <- paste0(email_split_regex,
                                     email_split_tokens[i],
                                     "\\s?:")
  if(i != length(email_split_tokens))
  {
    email_split_regex <- paste0(email_split_regex,"|")
  }
}

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


raw_data_df <- read.csv(raw_input_file,
                        stringsAsFactors = F)
raw_data_df[,date_column] <- as.POSIXct(raw_data_df[,date_column],format="%m/%d/%Y %H:%M")

raw_data_df[,target_column] <- trim(raw_data_df[,target_column])

raw_data_df_red <- raw_data_df %>%
  filter_(paste(target_column, "%in%  top_category_list")) %>%
  select(one_of(id_column,target_column,date_column,text_column)) %>%
  filter_(paste("!is.na(",date_column,")"))

print(unique(raw_data_df_red$Category))

raw_data_df_red <- raw_data_df_red[order(raw_data_df_red[,id_column],
                                         raw_data_df_red[,date_column]),]

data_df_final <- raw_data_df_red[!duplicated(raw_data_df_red[,id_column]),]

for (i in 1:length(text_column))
{
  data_df_final[,text_column[i]] <- gsub(email_regexp, " some_email@address.domain ",
                                         ignore.case = T,
                                         data_df_final[,text_column[i]])
  data_df_final[,text_column[i]] <- gsub(phone_regexp, " XXXXXXXXXX ",
                                         data_df_final[,text_column[i]])
  
  data_df_final[,text_column[i]] <- gsub(paste(client_name,collapse="|"), " client_name ",
                                         ignore.case = T,
                                         data_df_final[,text_column[i]])
  
  data_df_final[,text_column[i]] <- gsub(PO_regexp, " PO_NO ",
                                         data_df_final[,text_column[i]])
  
  # email_split <- str_split(data_df_final[,text_column[i]],
  #                          regex(final_email_split_regex,
  #                                ignore_case = TRUE))
  # 
  # data_df_final[,text_column[i]] <- unlist(lapply(email_split, `[[`, 1))
  # 
  # data_df_final[,text_column[i]] <- trim(data_df_final[,text_column[i]])
  if(is_text_col_multiple_email[i])
  {
    email_split <- str_split(data_df_final[,text_column[i]],
                             regex(email_split_regex,
                                   ignore_case = TRUE))
    email_texts <- c()
    for(n in 1:length(email_split))
    {
      email_bodies <- ""
      for(j in 1:length(email_split[[n]]))
      {
        email_body_split <- str_split(paste(email_split[[n]][j]),
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
          print(paste("truncating case:email -",n,":",j ,
                      "as original line count is:",length(email_lines[[1]])))
          email_body <- truncate_lines(email_body,line_cut_off)
        }
        text_body_word_count <- str_count(email_body,"\\S+|\\n+")
        
        if(j == 1)
        {
          email_bodies = email_body
        } else {
          email_bodies <- paste(email_bodies,email_body,
                               sep = " \n------------------\n")
        }
        if(text_body_word_count > min_word_count) break
      }
      email_texts <- c(email_texts,email_bodies)
    }
    
    #new_cl_df[,text_column[2]] <- unlist(lapply(email_split, `[[`, 1))
    
    #new_cl_df[,text_column[2]] <- trim(new_cl_df[,text_column[2]])
    
    data_df_final[,text_column[i]] <- email_texts
  }
}

manual_tag_df <- read.csv(manual_tagged_file,stringsAsFactors = F)
data_df <- data_df_final %>%
  select(one_of(id_column,text_column)) %>%
  inner_join(manual_tag_df,by=id_column)

names(data_df) <- c(id_column,text_column,target_column)
## Data fields formatting

data_df$is_fyi <- ifelse(data_df[,target_column] == "For your information cases" |
                           data_df[,target_column] == "Internal Team Request to order status team",
                         "yes","no")

data_df$is_fyi <- as.factor(data_df$is_fyi)
data_df[,target_column] <- make.names(trim(tolower(data_df[,target_column])))
data_df[,target_column] <- as.factor(data_df[,target_column])

# for(i in 1:length(text_column))
# {
#   
#   data_df[,text_column[i]] <- gsub("[x]{2,}"," ",
#                                  ignore.case = T,
#                                  data_df[,text_column[i]])
# }

# Split in train and test  

# set.seed(32)
# intrainandval <-createDataPartition(y=data_df$Category,p=0.9,list=FALSE)
# trainandval <- data_df[intrainandval,]
# blind_test <- data_df[-intrainandval,]
# 
# text_body_line_count <- apply(trainandval,2,str_count,"\\n+")[,text_column[2]]
# summary(text_body_line_count)
# hist(text_body_line_count,breaks = 50)
# line_cut_off <- ceiling(summary(text_body_line_count)[5] + 1.5 * IQR(text_body_line_count))
# 
# 
# text_body_word_count <- apply(trainandval,2,str_count,"\\S+|\\n+")[,text_column[2]]
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
#                             truncate_words(verify_df[i,text_column[2]],word_cut_off))
#   }
#   else {
#     word_truncated_IQR <- c(word_truncated_IQR,"no change")
#   }
#   
#   
#   if(verify_df[i,"text_body_word_count"] > 200)
#   {
#     word_truncated_200 <- c(word_truncated_200,
#                             truncate_words(verify_df[i,text_column[2]],200))
#   }
#   else {
#     word_truncated_200 <- c(word_truncated_200,"no change")
#   }
#   
#   
#   if(verify_df[i,"text_body_line_count"] > line_cut_off)
#   {
#     line_truncated_IQR <- c(line_truncated_IQR,
#                             truncate_lines(verify_df[i,text_column[2]],line_cut_off))
#   }
#   else {
#     line_truncated_IQR <- c(line_truncated_IQR,"no change")
#   }
#   
#   
#   if(verify_df[i,"text_body_line_count"] > 50)
#   {
#     line_truncated_50 <- c(line_truncated_50,
#                            truncate_lines(verify_df[i,text_column[2]],50))
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
# write.csv(verify_df,paste0(out_dir,"/","verify_cutoff_partial_masked.csv"),row.names = F)
# 
# text_body_all_line_count <- apply(data_df,2,str_count,"\\n+")[,text_column[2]]
# summary(text_body_all_line_count)
# hist(text_body_all_line_count,breaks = 50)
# 
# for(i in 1:nrow(data_df))
# {
#   
#   if(text_body_all_line_count[i] > line_cut_off)
#   {
#     data_df[i,text_column[2]] <- truncate_lines(data_df[i,text_column[2]],
#                                                line_cut_off)
#   }
#   
# }




set.seed(32)
intrainandval <-createDataPartition(y=data_df[,target_column],p=0.9,list=FALSE)
trainandval <- data_df[intrainandval,]
blind_test <- data_df[-intrainandval,]


set.seed(32)
intrain <-createDataPartition(y=trainandval[,target_column],p=0.9,list=FALSE)
trainingandval1 <- trainandval[intrain,]
val2 <- trainandval[-intrain,]

set.seed(32)
intrain <-createDataPartition(y=trainingandval1[,target_column],p=0.8,list=FALSE)
training <- trainingandval1[intrain,]
val1 <- trainingandval1[-intrain,]

print(dim(trainingandval1))
print(dim(training))
print(dim(val1))
print(dim(val2))
print(dim(blind_test))

write.csv(trainingandval1,paste0(out_dir,"/","train_val1_partial_masked_",Sys.Date(),".csv"),row.names = F)
write.csv(training,paste0(out_dir,"/","training_partial_masked_",Sys.Date(),".csv"),row.names = F)
write.csv(val1,paste0(out_dir,"/","validation1_partial_masked_",Sys.Date(),".csv"),row.names = F)
write.csv(val2,paste0(out_dir,"/","validation2_partial_masked_",Sys.Date(),".csv"),row.names = F)
write.csv(blind_test,paste0(out_dir,"/","blind_test_partial_masked_",Sys.Date(),".csv"),row.names = F)
