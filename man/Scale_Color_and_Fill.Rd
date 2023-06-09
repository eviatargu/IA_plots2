% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scales2.R
\name{Scale_Color_and_Fill}
\alias{Scale_Color_and_Fill}
\alias{IA_scale_color}
\alias{IA_scale_fill}
\title{Scale color and fill}
\usage{
IA_scale_color(palette = "DPB", discrete = TRUE, reverse = FALSE, ...)

IA_scale_fill(palette = "DPB", discrete = TRUE, reverse = FALSE, ...)
}
\arguments{
\item{palette}{Choose palette from a list of options.
currently there are 19 variants.
The colors are marked with capital letters:
"D" - Dark (black), "B" Blue, "G" gray and P pink.

Options with two colors are: DP, DB, DG, GB, GP, BP.

Options with three colors are: DBP, DPB, DGP, DGB, GBP.

Options with more colors are called "c" (for colors) followed by the # of colors:
c4, c5,c6, c7, and c12.

There are also pallets with one color with 4 color tones:
a gray pallet, blue pallet and pink pallet.}

\item{discrete}{specify if the color/fill variable is discrete or continues}

\item{reverse}{reverse colors of the palette.}

\item{...}{pass other arguments to the  scale parameters such as "guide = "none""
or "position = "left",see ggplot discrete_scale for more options.}
}
\description{
Add scale_color or scale_fill to a ggplot object with IA color palettes of choice
}
\examples{
# Add scale_color_IA at any position inside a ggplot command:
# discrete color scales
 p <- ggplot(mpg, aes(x = displ, y = cty, color = drv)) +
  geom_point(size = 4)

 p + IA_scale_color()
 p + IA_scale_color(palette = "DBP", reverse = T)

 # palettes of one color:
 p <- ggplot(data.frame(x = 1:20), aes(x, x, fill = x)) + geom_col()
 p
 p + IA_scale_fill(palette =  "pink", discrete = FALSE)
 p + IA_scale_fill(palette =  "gray", discrete = FALSE)
 p + IA_scale_fill(palette =  "blue", discrete = FALSE)

 # Intepolate between colors
 p + IA_scale_fill(palette = "DP", discrete = FALSE)
 p + IA_scale_fill(palette = "DBP", discrete = FALSE)

}
