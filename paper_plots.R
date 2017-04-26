library(data.table)
library(ggplot2)
zero_df <-
    read.csv(file = "./timeSeries_zeroFiltered_majoritySR_withQuantiles.csv", stringsAsFactors = FALSE)

zero_table <- data.table(zero_df)


user_level_groups <- as.data.frame(zero_table[,list(
    total_fem_score = sum(feminism_total_score),total_mr_score = sum(mensrights_total_score)
), by = list(author)])


plot <-
    ggplot(user_level_groups, aes(x = total_fem_score, y = total_mr_score)) +
    stat_density2d(aes(alpha = ..level.., fill = ..level..), geom = "polygon") +
    scale_fill_gradient(low = "#3ea87a", high = "#181f1c") +
    scale_alpha(range = c(0.00, 0.75), guide = FALSE) +
    ggtitle("") +      theme(plot.title = element_text(size = 10,
                                                       face = "bold",
                                                       hjust = 0.5)) +
    geom_vline(xintercept = 24) +
    geom_hline(yintercept = 7) +
    scale_colour_discrete(guide = "none") +
    scale_linetype_manual(name = 'Legend',
                          values = 1,
                          guide = "legend") +
    
    xlab("Feminism Total Score") +
             ylab("MensRights Total Score")


user_level_groups_median_groups<-as.data.frame(zero_table[,list(
    feminism_median = median(feminism_median),mensrights_median = mean(mensrights_median)
), by = list(author)])

plot_2 <-
    ggplot(user_level_groups_median_groups, aes(x = feminism_median, y = mensrights_median)) +
    stat_density2d(aes(alpha = ..level.., fill = ..level..), geom = "polygon") +
    scale_fill_gradient(low = "#3ea87a", high = "#181f1c") +
    scale_alpha(range = c(0.00, 0.75), guide = FALSE) +
    ggtitle("") +      theme(plot.title = element_text(size = 10,
                                                       face = "bold",
                                                       hjust = 0.5)) +
    geom_vline(xintercept = 2) +
    geom_hline(yintercept = 2) +
    scale_colour_discrete(guide = "none") +
    scale_linetype_manual(name = 'Legend',
                          values = 1,
                          guide = "legend") +
    
    xlab("Feminism Total Score") +
    ylab("MensRights Total Score")


user_level_majority_sr_groups<-as.data.frame(zero_table[,list(
    feminism_median = median(feminism_median),mensrights_median = mean(mensrights_median)
), by = list(author)])