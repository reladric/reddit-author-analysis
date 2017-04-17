source("global.R")
score_data<-compute_data()[[4]]
first_month_data <-
    plotdata_for_month(
        x = 100, complete_data = score_data, feature = "avg",scale = TRUE, window_size = 5
    )
second_month_data<-plotdata_for_month(
    x = 101, complete_data = score_data, feature = "avg",scale = TRUE, window_size = 5
)

current_month_data <- first_month_data[,c(1,16,17)]
colnames(current_month_data) <- c("user","left_xqval", "left_yqval")

previous_month_data <- second_month_data[,c(1,16,17)]
colnames(previous_month_data) <- c("user","right_xqval", "right_yqval")

common_rows <-
    merge(current_month_data, previous_month_data, by =
              "user")
common_rows_table<-data.table(common_rows)

common_rows_agg <-
  common_rows_table[, list(count  = length(user)), by = list(left_xqval, left_yqval, right_xqval, right_yqval)]

common_rows$x_movement <-
    common_rows$left_xqval - common_rows$right_xqval
common_rows$y_movement <-
    common_rows$left_yqval - common_rows$right_yqval

common_rows_table <- data.table(common_rows)


quiver_plot_data <-
  common_rows_agg[(
    common_rows_agg$left_xqval == -1 &
      common_rows_agg$left_yqval ==-1
  ),]
ratio <- .25/(256/5)
ggplot(data = quiver_plot_data,
       aes(x = left_xqval , y = left_yqval + 0.25)) +
  geom_segment(aes(xend = right_xqval , yend = right_yqval - 0.25),
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_hline(yintercept = 2.5, col = "red") +
  geom_vline(xintercept = 2.5, col = "red") +
  geom_hline(yintercept = 1.5, col = "blue") +
  geom_vline(xintercept = 1.5, col = "blue") +
  geom_hline(yintercept = 3.5, col = "blue") +
  geom_vline(xintercept = 3.5, col = "blue") +
  geom_hline(yintercept = 0.5, col = "blue") +
  geom_vline(xintercept = 0.5, col = "blue") +
  geom_hline(yintercept = 4.5, col = "blue") +
  geom_vline(xintercept = 4.5, col = "blue") +
  xlim(c(0.5,4.5))+
  ylim(c(0.5,4.5))+
  geom_text(data = quiver_plot_data,
    aes(
      x =0.2+(left_xqval+right_xqval)/2,
      y =( right_yqval - 0.22),
      label = count
    )
  )
