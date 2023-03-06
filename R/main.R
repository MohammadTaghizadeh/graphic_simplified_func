

################################################################################

mtbarplota <- function(d, bar_lab, y_lab = NULL, x_lab = NULL,
                      plot_title = NULL, plwd = 2){
  df <- data.frame(col_data = d, barlab = bar_lab)
  {
  par(mar = c(6,6,6,6), lwd = plwd)
  ylim_min <- (min(df$col_data)-(sd(df$col_data)/sqrt(length(df$col_data))))
  ylim_max <- (max(df$col_data)+(sd(df$col_data)/sqrt(length(df$col_data))))
  barplot(col_data ~ bar_lab, data = df, ylim = c(ylim_min, ylim_max),
          space = 2, border = c(2,2,2,2), col = 'skyblue',
          ylab = y_lab, xlab = x_lab, main = plot_title, font.main = 4
  )
  }
  box()
  abline(h=0, col = 'gray')
}

mtbarplota(-9:9, letters[1:19], plwd = 2)
mtbarplota(sample(-10:10, 15, replace = T), letters[1:15])
################################################################################

mtscatterplot <- function(x_data, y_data,
                          plot_name = NULL, x_lab = NULL, y_lab = NULL){
  library(ggplot2)
  df <- data.frame(x = x_data, y = y_data)
  ggplot(df, aes(x, y)) +
    geom_point(alpha = 0.4, size = 3) + theme_bw() +
    theme(panel.background = element_rect(fill = 'white', color = 'black',
                                          size = 1.4),
          plot.margin = margin(2,2,2,2,'cm'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 12, face = 'bold'),
          axis.text.y = element_text(size = 12, face = 'bold'),
          axis.title = element_text(size = 14, face = 'bold'),
          plot.title = element_text(size = 14, face = 'bold', hjust = 0.5)) +


    labs(title = plot_name, x = x_lab, y = y_lab)

}

################################################################################
remove_gsf <- function(){
  remove.packages('mtgraphics')
}

################################################################################
install_gsf <- function(){
  devtools::install_github('MohammadTaghizadeh/graphic_simplified_func@master')
}

################################################################################
############################ ggplot barplot ####################################

mtbarplotb <- function(hight_data, lab_data,
                       plot_name = 'test mtbarplotb',
                       x_lab = 'x_lab', y_lab = 'y_lab'){
library(ggplot2)
df <- data.frame(d = hight_data, d_lab = lab_data)

ggplot(data = df, aes(x = d_lab, y = d)) +
  geom_bar(stat = 'identity', width = 0.5,
           fill = 'skyblue', color = 'black') + theme_bw() +
  theme(panel.background = element_rect(fill = 'white', color = 'black',
                                        size = 1.4),
        plot.margin = margin(1,1,1,1,'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.text.y = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 14, face = 'bold', hjust = 0.5)) +
        geom_hline(yintercept = 0) +

  labs(title = plot_name, x = x_lab, y = y_lab)
}

################################################################################
############################## mtbarplotc ######################################


mtbarplotc <- function(hight_data_a, hight_data_b,
                       da_sd = NULL, db_sd = NULL,
                       d_names, group_names = c('A', 'B'),
                       plot_name = 'test mtbarplotb',
                       x_lab = 'x_lab', y_lab = 'y_lab'){
  library(ggplot2)
  if (length(d_names) == length(hight_data_a)) {
      dn <- rep(d_names, 2)
  }else{dn <- d_names}
  df <- data.frame(d = c(hight_data_a, hight_data_b),
                   gnames = c(rep(group_names[1], length(hight_data_a)),
                              rep(group_names[2], length(hight_data_b))),
                   dnames = dn,
                   sd = c(da_sd, db_sd))

  ggplot(data = df, aes(x = dnames, y = d, fill = gnames)) +
    geom_bar(stat = 'identity', width = 0.5,
             position=position_dodge()) + theme_bw() +
    theme(panel.background = element_rect(fill = 'white', color = 'black',
                                          size = 1.4),
          plot.margin = margin(1,1,1,1,'cm'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 12, face = 'bold'),
          axis.text.y = element_text(size = 12, face = 'bold'),
          axis.title = element_text(size = 14, face = 'bold'),
          plot.title = element_text(size = 14, face = 'bold', hjust = 0.5)) +
    geom_hline(yintercept = 0) +

    labs(title = plot_name, x = x_lab, y = y_lab) +
    geom_errorbar(aes(ymin=d-sd, ymax=d+sd), width=.2,
                  position=position_dodge(0.5))
}

################################################################################



