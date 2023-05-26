#' Density plot for one variable with mean line and text
#'
#' @param data a data.frame or a tibble or anything that fits into ggplot
#' @param var is a continues variable to plot its density
#' @param fill_color  fill color from values 1 to 8 taken from the IA palette
#' @param mean_line add a mean line True or False
#' @param line_color line color from values 1 to 8 taken from the IA palette
#' @param text add text labels with the mean and standard deviation
#'
#' @return
#' @export
#'
#' @examples IA_density1(mtcars, qsec, fill_color = 3, mean_line = T, text = T)
#'

#' @importFrom magrittr %>%
IA_density1 <- function(data, var, fill_color = 3,mean_line = TRUE, line_color = 2, text = FALSE){

  m <-  data %>% dplyr::select({{var}}) %>% dplyr::summarize(m = mean({{var}}, na.rm = T),
                                               sd = sd({{var}}, na.rm = T))

  p1 <- ggplot(data, aes({{var}})) +
    list(
      geom_density(fill = IA_cols()[fill_color]),
      if(mean_line)
        geom_vline( xintercept = m[1,1], color = IA_cols()[line_color], size = 1.2)
    ) + IA_theme() +
    IA_remove_y()

  if(!text){
    p1
  } else{
    p2 <- ggplot_build(p1)
    p3 <-  p2[["data"]][[1]]

    x_pos <- ((range(p3$x)[2] + range(p3$x)[1]))/2*1.1
    y_pos <- max(p3$density)*0.9
    y_pos2 <- max(p3$density)*0.8

    p1 +
      annotate("text", x = x_pos, y = y_pos, label = paste ("Mean ==", m[1,1] ), parse = T, family = 'Alef') +
      annotate("text", x = x_pos, y = y_pos2, label = paste ("STD ==", m[1,2]), parse = T, family = 'Alef')


  }
}

