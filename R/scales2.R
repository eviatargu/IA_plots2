# basic colors ----
IA_colors <- c(
  "black" = "#323231",
  "gray1" = "#666364",
  "gray2" = "#7F7C7D",
  "gray3" = "#AFAFAF",
  "blue1" = "#0055D1",
  "blue2" = "#3591ED",
  "blue3" = "#5279E8",
  "blue4" = "#90C0FF",
  "pink1" = "#F0D2C4",
  "pink2" = "#E0AcA0",
  "pink3" = "#FBD3C5",
  "pink4" = "#FDE2CF"
)

#' Function to extract IA_colors as hex codes
#'
#'
#' returns the vector colors defined in the palette.
#'
#' currently there are 8 colors.
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples IA_cols()
#' IA_cols()[1]
#' IA_cols()[1:3]
#'
IA_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (IA_colors)
  IA_colors[cols]
}


# color pallets ----
IA_palettes <- list(
  "gray" = IA_cols("black", "gray1", "gray2", "gray3"),
  "blue" = IA_cols("blue1", "blue2", "blue3", "blue4"),
  "pink" = IA_cols("pink1", "pink2", "pink3", "pink4"),
  "DPB" = IA_cols("black", "pink1", "blue1"),
  "c6" = IA_cols("black", "gray1", "pink1", "pink2", "blue1", "blue3"),
  "c7" = IA_cols("black", "gray1", "blue1", "blue3", "blue4", "pink1", "pink2"),

  "DP" = IA_cols("black", "pink1"),
  "DB" = IA_cols("black","blue1" ),
  "DG" = IA_cols("black","gray1" ),
  "GB" = IA_cols("gray1","blue1" ),
  "GP" = IA_cols("gray1","pink1" ),
  "BP" = IA_cols("blue1","pink1" ),

  "DBP"= IA_cols("black", "blue1", "pink1"),
  "DGP" = IA_cols("black", "gray1", "pink1"),
  "DGB"= IA_cols("black", "gray1", "blue1"),
  "GBP"= IA_cols("gray1", "blue1", "pink1"),
  "c4"= IA_cols("black", "gray1", "blue1", "pink1"),
  "c5"= IA_cols("black", "gray1", "blue1", "pink1", "pink3"),
  "c12" = IA_cols() )


# function to interpolate a IA_color palette ----
IA_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- IA_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}


#' Scale color and fill
#'
#'
#' Add scale_color or scale_fill to a ggplot object with IA color palettes of choice
#'
#'
#'
#' @param palette   Choose palette from a list of options.
#'  currently there are 19 variants.
#'  The colors are marked with capital letters:
#' "D" - Dark (black), "B" Blue, "G" gray and P pink.
#'
#' Options with two colors are: DP, DB, DG, GB, GP, BP.
#'
#' Options with three colors are: DBP, DPB, DGP, DGB, GBP.
#'
#' Options with more colors are called "c" (for colors) followed by the # of colors:
#' c4, c5,c6, c7, and c12.
#'
#' There are also pallets with one color with 4 color tones:
#' a gray pallet, blue pallet and pink pallet.
#' @param discrete  specify if the color/fill variable is discrete or continues
#' @param reverse  reverse colors of the palette.
#' @param ... pass other arguments to the  scale parameters such as "guide = "none""
#' or "position = "left",see ggplot discrete_scale for more options.
#' @return
#' @examples # Add scale_color_IA at any position inside a ggplot command:
#'# discrete color scales
#'  p <- ggplot(mpg, aes(x = displ, y = cty, color = drv)) +
#'   geom_point(size = 4)
#'
#'  p + IA_scale_color()
#'  p + IA_scale_color(palette = "DBP", reverse = T)
#'
#'  # palettes of one color:
#'  p <- ggplot(data.frame(x = 1:20), aes(x, x, fill = x)) + geom_col()
#'  p
#'  p + IA_scale_fill(palette =  "pink", discrete = FALSE)
#'  p + IA_scale_fill(palette =  "gray", discrete = FALSE)
#'  p + IA_scale_fill(palette =  "blue", discrete = FALSE)
#'
#'  # Intepolate between colors
#'  p + IA_scale_fill(palette = "DP", discrete = FALSE)
#'  p + IA_scale_fill(palette = "DBP", discrete = FALSE)
#'


#' @name Scale_Color_and_Fill
NULL

#'
#' @rdname Scale_Color_and_Fill
#' @export
IA_scale_color <- function(palette = "DPB", discrete = TRUE, reverse = FALSE, ...) {
  pal <- IA_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("IA_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


#'
#' @rdname Scale_Color_and_Fill
#' @export
IA_scale_fill <- function(palette = "DPB", discrete = TRUE, reverse = FALSE, ...) {
  pal <- IA_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("IA_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


