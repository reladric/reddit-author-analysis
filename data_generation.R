library(data.table)
library(ggplot2)
monnb <-
  function(d) {
    lt <- as.POSIXlt(as.Date(d, origin = "1900-01-01"))
    lt$year * 12 + lt$mon
  }

mondf <- function(d1, d2) {
  monnb(d1) - monnb(d2)
}

start_date = as.Date("2005-06-01",
                     format = "%Y-%m-%d")
end_date = as.Date("2016-07-01",
                   format = "%Y-%m-%d")
month_reference_vector <- seq(1:mondf(end_date, start_date))
sr_reference_vector <- c("Feminism", "MensRights")
month_reference_frame <-
  as.data.frame(expand.grid(month_reference_vector, sr_reference_vector),
                ncol = 2)
colnames(month_reference_frame) <- c("month_num", "subreddit")

get_user_activity <- function(author) {
  current_user_data <- crossposter_all_data[crossposter_all_data$author == author,]
  activity_start_date <-
    min(as.Date(current_user_data$created_date, format = "%Y-%m-%d %H:%M:%S"))
  current_user_data$month_num_from_origin <-
    sapply(
      as.Date(current_user_data$created_date, format = "%Y-%m-%d %H:%M:%S"),
      mondf,
      start_date
    )
  current_user_data$month_num_from_activity <-
    sapply(
      as.Date(current_user_data$created_date, format = "%Y-%m-%d %H:%M:%S"),
      mondf,
      activity_start_date
    )
  current_user_table <- data.table(current_user_data)
  current_user_monthwise_activity <-
    current_user_table[, list(post_count = length(id)), by = list(author, subreddit, month_num_from_origin)]
  activity_data_table<-data.table(current_user_monthwise_activity)
  monthwise_totals<-activity_data_table[,list(total=sum(post_count)), by=month_num_from_origin]
  user_complete_activity <- merge(
    x = month_reference_frame,
    y = current_user_monthwise_activity,
    all.x = TRUE,
    by.x = c("month_num", "subreddit"),
    by.y = c("month_num_from_origin", "subreddit")
  )
  user_complete_activity$post_count[is.na(user_complete_activity$post_count)] <-
    0
  user_complete_activity$author<-author
  user_complete_activity_with_totals<-merge(x=user_complete_activity,y=monthwise_totals, all.x=TRUE,by.x = c("month_num"),
                                            by.y = c("month_num_from_origin"))
  user_complete_activity_with_totals$total[is.na(user_complete_activity_with_totals$total)] <-
    0
  user_complete_activity_with_totals$ratio<-user_complete_activity_with_totals$post_count/user_complete_activity_with_totals$total
  user_complete_activity_with_totals$ratio[is.na(user_complete_activity_with_totals$ratio)]<-user_complete_activity_with_totals$post_count/user_complete_activity_with_totals$total
  return(user_complete_activity_with_totals)
}
ptm <- proc.time()
all_data <- read.csv2(file = "./fem_mr.csv", header = TRUE, sep = ",")
all_data_table <- data.table(all_data)
user_post_group <-
  all_data_table[, list(post_count = length(id)), by = author]
proc.time() - ptm

ptm <- proc.time()
crosstalking_data <-
  read.csv2(file = "./final_file.csv", sep = ",", header = TRUE)
crosstalking_data_table <- data.table(crosstalking_data)
user_cp_data <-
  crosstalking_data_table[, list(crossposter = max(isCrossposterYet)), by =
                            list(author, HomeSubreddit_x)]
user_cp_data$crossposter[user_cp_data$crossposter != 1] <- -1
user_cp_data$crossposter[(user_cp_data$crossposter != 1 &
                            user_cp_data$HomeSubreddit_x == "Feminism")] <-
  -2
print("Load complete")
proc.time() - ptm

user_stat <-
  crosstalking_data_table[, list(
    created = max(account.created),
    post_karma = max(account.post.karma),
    comment_karma = max(account.comment.karma)),
    by = list(author)]
                                                                                                                            
user_crossposting_frame <-
  merge(x = user_post_group,
        y = user_cp_data,
        by = "author",
        all.x = TRUE)

ptm <- proc.time()
list_of_active_user_posting_activity<-pblapply(as.character(user_with_quad$author), get_user_activity)
active_user_posting_activity_frame<-do.call(rbind, list_of_active_user_posting_activity)
proc.time() - ptm

write.table(x=active_user_posting_activity_frame, file = "author_posting_activity.csv", sep = ",", row.names = FALSE)
first_majority_data<-read.csv2(file = "./first_majority.csv", sep = ",", header=TRUE)
