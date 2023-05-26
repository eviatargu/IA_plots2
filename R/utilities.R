#' Rotate the x axis text
#'
#' @param angle numeric value specifying the rotation angle. Default is 90 for vertical x-axis text.
#' @param hjust horizontal adjustments
#' @param vjust vertical adjustments
#' @param ... other arguments to pass to the function element_text().
#'
#' @return
#' @export
#' @examples p <- ggplot(mpg, aes(manufacturer , displ)) + geom_point()
#' p
#' p + IA_rotate_x_text(30)
#'
#' # you could use `coord_filp()` instead:
#' p + coord_flip()

IA_rotate_x_text <- function (angle = 90, hjust = NULL, vjust = NULL, ...){

  if (missing(hjust) & angle > 5)
    hjust <- 1
  if (missing(vjust) & angle == 90)
    vjust <- 0.5

  theme(axis.text.x = element_text(angle = angle, hjust = hjust,
                                   vjust = vjust, ...))
  }



#' remove the y axis.
#'
#' Remove line, ticks, text and title of the Y axis.
#' very convenient for kernel densities.
#' Add it to a ggplot function as the last theme adjustment.
#'
#' @return
#' @export
#'
#' @examples p <- ggplot(mpg, aes(x = hwy)) + geom_density()
#' p
#' p + IA_remove_y()
IA_remove_y <- function(){
  theme(axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y  = element_blank())
}

#' scale y continues versions
#'
#' add scale_y_continues for thousands, millions of percentage.
#'
#' @return
#' @examples # Add the name of the environment at the end of a ggplot call
#' # preceded by a plus "+" sign.
#'
#' p <- ggplot(txhousing,aes(year, volume, fill = city)) +
#'  geom_bar(stat = "summary", fun = "mean", show.legend = FALSE)
#'
#' p
#' p + IA_gg_k
#' p + IA_gg_m
#'
#'
#' p <- ggplot(mtcars, aes(gear)) + geom_bar(aes( y = ..prop..))
#'
#' p
#' p + IA_gg_p
#'
#'
#' @name Scale_y_continues_number_versions
NULL

#' @rdname Scale_y_continues_number_versions
#' @export
IA_gg_k <- scale_y_continuous(labels = scales::label_comma(scale = 1e-3, suffix =  " k"))

#' @rdname Scale_y_continues_number_versions
#' @export
IA_gg_m <- scale_y_continuous(labels = scales::label_comma(scale = 1e-6, suffix =  " M"))

#' @rdname Scale_y_continues_number_versions
#' @export
IA_gg_p <- scale_y_continuous(labels = scales::label_percent(accuracy = 1))






#' Print nice tables into the html document
#'
#' Add it with the pipe "%>%" at the end of a data frame
#'
#' @param df is a data frame
#' @param wi is the width of the table in pixels.
#' @param hi is the height of the table in pixels
#' @param group_var in the IA_t3 this is an option to group rows together
#'  by variable category.
#' @param  digits the number of rounded digits in numbers
#' @return
#' @examples
#' IA_t1(head(mtcars))
#' IA_t2(head(mtcars))
#' IA_t3(mpg)
#' IA_t3(mpg, group_var = 'manufacturer', digits = 2, hi = 800)
#' IA_t3(mpg, group_var = c('manufacturer', 'model'))
#' @name nice_html_table
NULL


#' @rdname nice_html_table
#' @export
#' @importFrom magrittr %>%
IA_t1 <- function(df, digits = 3){
  kableExtra::kbl(df, format.args = list(big.mark = ","), digits = digits) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = F)
  }


#'
#' @rdname nice_html_table
#' @export
#' @importFrom magrittr %>%
IA_t2 <- function(df, wi = 800, hi = 400 , digits = 3){
  wi2 <- paste0(wi, "px")
  hi2 <- paste0(hi, "px")

  IA_t1(df, digits = digits) %>%
    kableExtra::scroll_box(width = wi2, height = hi2 )
  }


#'
#' @rdname nice_html_table
#' @export
#' @importFrom magrittr %>%
IA_t3 <-  function(df, group_var = NULL, digits = 3, wi = NULL, hi = 500, ...) {

  dplyr::mutate(df,
         dplyr::across(where(is.numeric),
                ~round(.x, digits = digits))) %>%

    reactable::reactable(
      groupBy = group_var,
      fullWidth = FALSE,
      bordered =  TRUE,
      highlight = TRUE,
      compact = TRUE,
      striped = TRUE,
      resizable = TRUE,
      sortable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      wrap = TRUE,
      height = hi,
      paginateSubRows = TRUE,
      showPageSizeOptions = TRUE,
      selection = "multiple",
      defaultPageSize = 10,
      width = wi,
      theme = reactable::reactableTheme(style = list(fontFamily = "Alef")),
      defaultColDef = reactable::colDef(minWidth = 100,
                                        format = reactable::colFormat(separators = TRUE))
    )
  }




#' ggave with png device and white background
#'
#' Yotvat found bugs in the ggsave function. 1st, the saving does not specity
#' a background color. 2nd, in the new version one needs to specify the device.
#' this function is a wrapper around the ggsave with the correct defults to save time.
#'
#' @return
#' @export
#'
#' @examples # enter a file_name_with_path.png to save
IA_save <- function(file , plot = last_plot() , device = png, bg = "white"){
  ggsave(filename = file,plot = plot, device = device, bg  = bg)
}


#' wrap a long string into several lines
#'
#' create a line break in long string before printing into a plot.
#'
#' @return
#' @export
#' @examples # Normally str_wrap() is for 80 characters. This is to much for plots.
#' # So, this function is a wrapper with default of 20 characters to use easly in
#' # ggplot.
#'
#'
#' p <-
#' dplyr::filter(mpg,str_length(model) > 10) %>%
#' ggplot(aes(as.factor(year), displ)) +
#' geom_point()
#'
#' p + facet_wrap(~ model)
#' p + facet_wrap(~ IA_s_wrap(model, 10))
IA_s_wrap <- function(x, w = 20) stringr::str_wrap(x, width = w)

