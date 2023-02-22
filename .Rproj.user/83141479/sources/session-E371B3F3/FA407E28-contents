

################################################################################


mtbarplot <- function(data, bar_lab, y_lab = NULL, x_lab = NULL,
                      plot_title = NULL){
  df <- data.frame(data = data, bar_lab = bar_lab)

  par(mar = c(6,6,6,6), lwd = 2)
  barplot(data ~ bar_lab, data = df, ylim = c(max(min(data)-1, 0), max(data)+(max(data)*0.1)),
          space = 2, border = c(2,2,2,2), col = 'skyblue',
          ylab = y_lab, xlab = x_lab, main = plot_title, font.main = 4
  )

  box()

}


