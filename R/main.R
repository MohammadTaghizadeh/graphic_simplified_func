

################################################################################


mtbarplot <- function(data, bar_lab, y_lab = NULL, x_lab = NULL,
                      plot_title = NULL){
  df <- data.frame(data = data, bar_lab = bar_lab)

  par(mar = c(6,6,6,6), lwd = 2)
  barplot(data ~ bar_lab, data = df, ylim = c(max(min(data)-1, 0),
          max(data)+(max(data)*0.1)),
          space = 2, border = c(2,2,2,2), col = 'skyblue',
          ylab = y_lab, xlab = x_lab, main = plot_title, font.main = 4
  )

  box()

}


################################################################################

mtscatterplot1 <- function(data_1, data_2,
                          plot_name = NULL, x_lab = NULL, y_lab = NULL){
  library(ggplot2)
  df <- data.frame(data1 = data_1, data2 = data_2)
  ggplot(df, aes(data1, data2)) +
    geom_point(alpha = 0.4, size = 3) + theme_bw() +
    theme(panel.background = element_rect(fill = 'white', color = 'black',
                                          size = 1.4),
          plot.margin = margin(1,1,1,1,'cm'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 12, face = 'bold'),
          axis.text.y = element_text(size = 12, face = 'bold'),
          axis.title = element_text(size = 14, face = 'bold'),
          plot.title = element_text(size = 14, face = 'bold', hjust = 0.5)) +
    scale_x_continuous(breaks = seq(min(data_1), max(data_1)+1, 4)) +
    scale_y_continuous(breaks = seq(min(data_2), max(data_2)+1, 5)) +

    labs(title = plot_name, x = x_lab, y = y_lab)

}

################################################################################
remove_gsf <- function(){
  remove.packages('graphicsimpfuncs')
}

################################################################################
install_gsf <- function(){
  devtools::install_github('MohammadTaghizadeh/graphic_simplified_func@master')
}


library(idpr)
hsa <- unlist(protr::getUniProt('P02768'))
hsa

scaledHydropathyLocal(hsa,
                      plotResults = TRUE)

ex_globin <- unlist(protr::getUniProt('P28316'))

scaledHydropathyLocal(ex_globin,
                      plotResults = TRUE)

foldIndexR(sequence = hsa,
           plotResults = TRUE)


foldIndexR(sequence = ex_globin,
           plotResults = TRUE)


mtscatterplot1(cars$speed, cars$dist)
