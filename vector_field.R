source("global.R")
score_data <- compute_data()[[4]]
first_month_data <-
  plotdata_for_month(
    x = 100,
    complete_data = score_data,
    feature = "avg",
    scale = TRUE,
    window_size = 5,
    threshold = 1,
    center = "mean"
  )

second_month_data <- plotdata_for_month(
  x = 101,
  complete_data = score_data,
  feature = "avg",
  scale = TRUE,
  window_size = 5,
  threshold = 2,
  center = "mean"
)

base_month_data <- first_month_data[, c(1, 16, 17)]
colnames(base_month_data) <-
    c("user", "current_xqval", "current_yqval")

previous_month_data <- second_month_data[, c(1, 16, 17)]
colnames(previous_month_data) <-
    c("user", "previous_xqval", "previous_yqval")

common_rows <-
    merge(base_month_data, previous_month_data, by =
              "user")
common_rows_table <- data.table(common_rows)

common_rows_agg <-
    as.data.frame(common_rows_table[, list(count  = length(user)), by = list(current_xqval,
                                                                             current_yqval,
                                                                             previous_xqval,
                                                                             previous_yqval)])

common_rows$x_movement <-
  common_rows$left_xqval - common_rows$right_xqval
common_rows$y_movement <-
  common_rows$left_yqval - common_rows$right_yqval

common_rows_table <- data.table(common_rows)


quiver_plot_data <-
  common_rows_agg[(common_rows_agg$left_xqval == -1 &
                     common_rows_agg$left_yqval == -1),]
ratio <- .25 / (256 / 5)
ggplot(data = source_destination_frame,
       aes(x = previous_xqval , y = previous_yqval )) +
  geom_segment(aes(xend = x_destination , yend = y_destination),
               arrow = arrow(length = unit(0.3, "cm"))) +
  xlim(c(0.5, 4.5)) +
  ylim(c(0.5, 4.5)) +theme(panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank())
get_mean_movement <-
  function(x,
           complete_data,
           feature,
           window_size,
           threshold) {
    wd_1 <-
      get_window_data(
        x - 1,
        complete_data = complete_data,
        feature = feature,
        window_size = window_size,
        threshold = threshold
      )
    wd <-
      get_window_data(
        x,
        complete_data = complete_data,
        feature = feature,
        window_size = window_size,
        threshold = threshold
      )
    
    xstart <- mean(wd_1$fem_avg)
    ystart <- mean(wd_1$mr_avg)
    xend <- mean(wd$fem_avg)
    yend <- mean(wd$mr_avg)
    current_movement <- c(xstart, xend, ystart, yend)
    names(current_movement) <- c("xstart", "xend", "ystart", "yend")
    return (current_movement)
  }
all_movements <-
  lapply(
    seq(min(score_data$month) + 10, max(score_data$month)),
    get_mean_movement,
    feature = "score",
    complete_data = score_data,
    window_size = 10,
    threshold = 2
  )
plot_movements <- as.data.frame(do.call(rbind, all_movements))
vector_data <- plot_movements[complete.cases(plot_movements), ]

point_data_1 <- vector_data[1, c(1, 3)]
colnames(point_data_1) <- c("xend", "yend")
point_data_1$type = "start"
point_data_2 <-  vector_data[c(2:nrow(vector_data) - 1), c(2, 4)]
point_data_2$type = "path"
point_data_3 <-  vector_data[nrow(vector_data), c("xend", "yend")]
point_data_3$type = "end"

point_data <- rbind(point_data_1, point_data_2, point_data_3)
ggplot(data = vector_data,
       aes(x = xstart , y = ystart)) +
  geom_segment(aes(xend = xend   , yend = yend),
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_point(data = point_data,
             aes(
               x = xend,
               y = yend,
               col = type,
               size = type
             )) +
  scale_size_manual (values = c(1.2, 0.7, 1.2)) +
  scale_color_manual(
    values = c(
      "end" = "red",
      "path" = "blue",
      "start" = "green"
    ),
    labels = c("end", "path", "start")
  ) +
  ggtitle(paste("Movement of mean of ",
                "avg",
                "- Fem v MR ",
                sep =
                  " ")) +
  xlab(paste("Mean of Feminsim", "avg", sep = "")) +
  ylab(paste("Mean of Mensrights", "avg", sep = ""))
