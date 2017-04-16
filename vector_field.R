source("global.R")
first_month_data <-
    plotdata_for_month(
        x = 100, complete_data = score_data, feature = "avg",scale = TRUE, window_size = 5
    )
second_month_data<-plotdata_for_month(
    x = 99, complete_data = score_data, feature = "avg",scale = TRUE, window_size = 5
)

current_month_data <- first_month_data[,c(1,16,17)]
colnames(current_month_data) <- c("user","left_xqval", "left_yqval")

previous_month_data <- second_month_data[,c(1,16,17)]
colnames(previous_month_data) <- c("user","right_xqval", "right_yqval")

common_rows <-
    merge(current_month_data, previous_month_data, by =
              "user")

common_rows$x_movement <-
    common_rows$left_xqval - common_rows$right_xqval
common_rows$y_movement <-
    common_rows$left_yqval - common_rows$right_yqval


