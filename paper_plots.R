library(data.table)
library(ggplot2)
zero_df <-
  read.csv(file = "./timeSeries_zeroFiltered_unbiased_majoritySR_withQuantiles.csv", stringsAsFactors = FALSE)

zero_table <- data.table(zero_df)
month_level_details <-
  as.data.frame(zero_table[order(month_num), list(users = length(author)), by =
                             month_num])
month_level_details$percentile_users_reached <-
  100 * cumsum(month_level_details$users) / sum(month_level_details$users)

first_part_df <- zero_df[zero_df$month_num <= 85, ]
first_part_table <- data.table(first_part_df)
second_part_df <- zero_df[zero_df$month_num <= 98, ]
third_part_df <- zero_df[zero_df$month_num > 98, ]

first_part_table <- data.table(first_part_df)
first_user_level_groups <- as.data.frame(first_part_table[, list(
  total_fem_score = sum(feminism_total_score),
  total_mr_score = sum(mensrights_total_score)
), by = list(author)])
first_fem_percentile <-
  ecdf(first_user_level_groups$total_fem_score)
first_user_level_groups$fem_percentile <-
  first_fem_percentile(first_user_level_groups$total_fem_score)
first_mr_percentile <- ecdf(first_user_level_groups$total_mr_score)
first_user_level_groups$mr_percentile <-
  first_mr_percentile(first_user_level_groups$total_mr_score)
first_fem_percentile_of_negative_score <-
  max(first_user_level_groups[first_user_level_groups$total_fem_score == max(first_user_level_groups[first_user_level_groups$total_fem_score <
                                                                                                       0,]$total_fem_score),]$fem_percentile)

first_mr_percentile_of_negative_score <-
  max(first_user_level_groups[first_user_level_groups$total_mr_score == max(first_user_level_groups[first_user_level_groups$total_mr_score <
                                                                                                      0,]$total_mr_score),]$mr_percentile)


# ggsave(filename ="1_From_Jul_06_Till_June_12_percentile_density.png", plot = plot, scale = 1, width = 7, height = 7 )

second_part_table <- data.table(second_part_df)
second_user_level_groups <- as.data.frame(second_part_table[, list(
  total_fem_score = sum(feminism_total_score),
  total_mr_score = sum(mensrights_total_score)
), by = list(author)])
second_fem_percentile <-
  ecdf(second_user_level_groups$total_fem_score)
second_user_level_groups$fem_percentile <-
  second_fem_percentile(second_user_level_groups$total_fem_score)
second_mr_percentile <-
  ecdf(second_user_level_groups$total_mr_score)
second_user_level_groups$mr_percentile <-
  second_mr_percentile(second_user_level_groups$total_mr_score)
second_fem_percentile_of_negative_score <-
  max(second_user_level_groups[second_user_level_groups$total_fem_score == max(second_user_level_groups[second_user_level_groups$total_fem_score <
                                                                                                          0,]$total_fem_score),]$fem_percentile)

second_mr_percentile_of_negative_score <-
  max(second_user_level_groups[second_user_level_groups$total_mr_score == max(second_user_level_groups[second_user_level_groups$total_mr_score <
                                                                                                         0,]$total_mr_score),]$mr_percentile)

# ggsave(filename ="2_From_July_12_till_Aug_13_percentile_density.png", plot = plot2, scale = 1, width = 7, height = 7 )
# png(filename = "2_From_July_12_till_Aug_13_percentile_density.png",
#     width = 800,
#     height = 600)
# plot2
# dev.off()
third_part_table <- data.table(third_part_df)
third_user_level_groups <- as.data.frame(third_part_table[, list(
  total_fem_score = sum(feminism_total_score),
  total_mr_score = sum(mensrights_total_score)
), by = list(author)])
third_fem_percentile <-
  ecdf(third_user_level_groups$total_fem_score)
third_user_level_groups$fem_percentile <-
  third_fem_percentile(third_user_level_groups$total_fem_score)
third_mr_percentile <- ecdf(third_user_level_groups$total_mr_score)
third_user_level_groups$mr_percentile <-
  third_mr_percentile(third_user_level_groups$total_mr_score)
third_fem_percentile_of_negative_score <-
  max(third_user_level_groups[third_user_level_groups$total_fem_score == max(third_user_level_groups[third_user_level_groups$total_fem_score <
                                                                                                       0,]$total_fem_score),]$fem_percentile)

third_mr_percentile_of_negative_score <-
  max(third_user_level_groups[third_user_level_groups$total_mr_score == max(third_user_level_groups[third_user_level_groups$total_mr_score <
                                                                                                      0,]$total_mr_score),]$mr_percentile)


#ggsave(filename ="3_From_Aug_13_till_June_16_percentile_density.png", plot = plot3, scale = 1, width = 7, height = 7 )
hline <-
  data.frame(yint = first_mr_percentile_of_negative_score , lt = 'Percentile of -ve score')
vline <-
  data.frame(xint = first_fem_percentile_of_negative_score, lt = 'Percentile of -ve score')
plot <-
  ggplot(first_user_level_groups,
         aes(x = fem_percentile, y = mr_percentile)) +
  stat_density2d(aes(alpha = ..level.., fill = ..level..), geom = "polygon") +
  scale_fill_gradient(low = "#3ea87a", high = "#181f1c") +
  scale_alpha(range = c(0.00, 0.75)) +
  ggtitle("Jul '06 - Jun '12")+
  theme(
    plot.title = element_text(size = 12,
                              face = "bold",
                              hjust = 0.5),
    legend.position = "bottom",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
  ) +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  geom_hline(
    data = hline,
    aes(yintercept = yint, linetype = lt),
    color = "red",
    size = 0.5
  ) +
  geom_vline(
    data = vline,
    aes(xintercept = xint, linetype = lt),
    color = "red",
    size = 0.5
  ) +
  scale_colour_discrete(guide = "none") +
  scale_linetype_manual(name = 'Legend',
                        values = 1,
                        guide = "legend") +
  guides(fill = F) +
  xlab("Percentile of Feminism Median Score ") +
  ylab("Percentile of Mensrights Median Score")


hline <-
  data.frame(yint = second_mr_percentile_of_negative_score , lt = 'Percentile of -ve score')
vline <-
  data.frame(xint = second_fem_percentile_of_negative_score, lt = 'Percentile of -ve score')
plot2 <-
  ggplot(second_user_level_groups,
         aes(x = fem_percentile, y = mr_percentile)) +
  stat_density2d(aes(alpha = ..level.., fill = ..level..), geom = "polygon") +
  scale_fill_gradient(low = "#3ea87a", high = "#181f1c") +
  scale_alpha(range = c(0.00, 0.75)) +
  ggtitle("Jul '12 - Aug '13")+
  theme(
    plot.title = element_text(size = 12,
                              face = "bold",
                              hjust = 0.5),
    legend.position = "bottom",
    axis.text = element_text(size = 12),
    axis.title =
      element_text(size = 13),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
  ) +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  geom_hline(
    data = hline,
    aes(yintercept = yint, linetype = lt),
    color = "red",
    size = 0.5
  ) +
  geom_vline(
    data = vline,
    aes(xintercept = xint, linetype = lt),
    color = "red",
    size = 0.5
  ) +
  scale_colour_discrete(guide = "none") +
  scale_linetype_manual(name = 'Legend',
                        values = 1,
                        guide = "legend") +
  guides(fill = F) +
  xlab("Percentile of Feminism Median Score ") +
  ylab("")


hline <-
  data.frame(yint = third_mr_percentile_of_negative_score , lt = 'Percentile of -ve score')
vline <-
  data.frame(xint = third_fem_percentile_of_negative_score, lt = 'Percentile of -ve score')
plot3 <-
  ggplot(third_user_level_groups,
         aes(x = fem_percentile, y = mr_percentile)) +
  stat_density2d(aes(alpha = ..level.., fill = ..level..), geom = "polygon") +
  scale_fill_gradient(low = "#3ea87a", high = "#181f1c") +
  scale_alpha(range = c(0.00, 0.75)) +
  ggtitle(
    "Sep '13 - Jun '16"
  )+
  theme(
    plot.title = element_text(size = 12,
                              face = "bold",
                              hjust = 0.5),
    legend.position = "bottom",
    axis.text = element_text(size = 12),
    axis.title =
      element_text(size = 13),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
  ) +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  geom_hline(
    data = hline,
    aes(yintercept = yint, linetype = lt),
    color = "red",
    size = 0.5
  ) +
  geom_vline(
    data = vline,
    aes(xintercept = xint, linetype = lt),
    color = "red",
    size = 0.5
  ) +
  scale_colour_discrete(guide = "none") +
  scale_linetype_manual(name = 'Legend',
                        values = 1,
                        guide = "legend") +
  guides(fill = F) +
  xlab("Percentile of Feminism Median Score ") +
  ylab("")


arrange_plots <- list(plot, plot2, plot3)
ggsave(
  "combined_heatmap.png",
  marrangeGrob(
    grobs = arrange_plots,
    nrow = 1,
    ncol = 3,
    top = NULL
  ),
  width = 15,
  height = 5,
  dpi = 300
)





all_movements <-
  lapply(
    seq(
      min(zero_df$month_num)+8,
      max(zero_df$month_num),
      4
    ),
    get_zero_mean_movement,
    zero_df,
    4
  )


plot_movements <- as.data.frame(do.call(rbind, all_movements))
vector_data <- plot_movements[complete.cases(plot_movements),]

point_data_1 <- vector_data[1, c(1, 2)]
colnames(point_data_1) <- c("xend", "yend")
point_data_1$type = "start"
point_data_2 <-
  vector_data[c(2:nrow(vector_data) - 1), c(3, 4)]
point_data_2$type = "path"
point_data_3 <-
  vector_data[nrow(vector_data), c("xend", "yend")]
point_data_3$type = "end"

point_data <- rbind(point_data_1, point_data_2, point_data_3)
ggplot(data = vector_data,
       aes(x = xstart , y = ystart)) +
  geom_segment(aes(xend = xend   , yend = yend),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               size = 0.7) +
  geom_point(data = point_data,
             aes(
               x = xend,
               y = yend,
               col = type,
               size=type
             )) +
  scale_size_manual (values = c("start"=2, "path"=0.7, "end"=2)) +
  scale_color_manual(
    values = c(
      "start" = "#00BFC4",
      "path" = "black",
      "end" = "#F8766D"
    ),
    labels = c("end", "path", "start")
  ) +
  theme(
    plot.title = element_text(size = 12,
                              face = "bold",
                              hjust = 0.5),
    legend.position = "bottom",
    axis.text = element_text(size = 11),
    axis.title =
      element_text(size = 13),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
  ) +
  xlab("Group Average in Feminism") +
  ylab("Group Average in Mensrights")
ggsave(filename = "mean_movement.png",width = 5, height = 5)