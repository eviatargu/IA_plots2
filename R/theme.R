#'theme for use in Israeli Antitrust Authority.
#'
#'can be added in a ggplot command of set as default theme with "theme_set(theme_IA())".
#'Before use it for the first time make sure the font "Alef" exist in the systme and that R
#' can see it. use extrafont::font_import to import system fonts into R.
#' This should take a few minutes.
#'
#' @param base_size  specify your font size
#' @param base_family  specify font name
#'
#' @return
#' @export
#'
#' @examples
#' ggplot(mpg, aes(x = displ, y = cty, color = drv)) +
#'  geom_point(size = 4) + IA_theme() +
#'  labs(title = "כותרת",
#'  subtitle = "כותרת משנה",
#'  x = "ציר אופקי",
#'  y = NULL,
#'  caption = "כיתוב תחתון",
#'  tag = "טיוג",
#'  color = "צבע"
#'  )
#'
#'
#'
IA_theme <- function(base_size = 12, base_family = 'Alef'){
  extrafont::loadfonts(quiet = TRUE)

  if(!"Alef" %in% fonts()){
    warning("it seems the needed font Alef do not exist. you shuld use
    extrafont::font_import() to import all computer fonts. Importing fonts
    shoud take about 10 minuts. make yourself a coffee and then run the function again :-)")
    }

  theme_minimal(base_size = base_size, base_family = extrafont::choose_font(base_family)) +
    theme(panel.border = element_rect(fill = NA, colour = "grey90"),

          panel.grid.major = element_line(colour = "grey93", size = 0.2),
          panel.grid.minor = element_line(colour = "grey97", size = 0.7),

          panel.background = element_rect(fill = "grey97", colour = NA),
          panel.spacing = unit(1, "lines"),

          strip.background = element_rect(color = "gray30", fill = "gray30", size = 1),
          strip.text = element_text(size = 12, face = "bold", colour = "white"),

          axis.line = element_line(color="black"),
          axis.ticks = element_line(),
          axis.text=element_text(size=12, family = "Alef"),
          axis.title=element_text(size=12,face="bold", family= "Alef"),

          plot.title = element_text(hjust = 1),
          plot.subtitle = element_text(hjust = 1),
          plot.caption = element_text(hjust = 0),
          plot.background = element_rect(fill = "white")
    )

}
