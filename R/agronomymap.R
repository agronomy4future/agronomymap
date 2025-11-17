#' Field Heatmap Visualization for Agronomic Trials
#'
#' Generates an interpolated spatial heatmap of agronomic field trial data
#' using row/column coordinates and a chosen measurement variable (e.g., SOC,
#' yield, biomass). Points are plotted on top of a raster surface, and axes may
#' optionally be labeled or hidden to produce clean field maps for publication.
#'
#' @param data A data frame containing at least two coordinate columns
#'   (e.g., "Column", "Row") and one numeric response variable.
#' @param map A character vector of length 2 giving the column names that
#'   represent the X and Y grid positions (e.g., `c("Column","Row")`).
#' @param variable Name of the numeric variable to map as a heat surface.
#' @param spacing Numeric distance between grid cells (default: `5`). Used
#'   to scale plotting coordinates.
#' @param grid_res Resolution of the interpolation grid passed to `akima::interp`.
#' @param point_size Size of the field points drawn over the raster.
#' @param point_color Outline color of field points.
#' @param point_fill Fill color of field points.
#' @param point_shape Point shape (default `21`, filled circle).
#' @param palette Color palette name from `RColorBrewer`.
#' @param fill_limits Numeric vector giving min and max color scale limits.
#' @param fill_breaks Tick mark values for the color legend.
#' @param label_title Title for the legend (e.g., `"Yield (kg/plant)"`).
#' @param label_size Font size for legend title.
#' @param label_key Font size for legend tick labels.
#' @param label_position Legend position (`"right"`, `"bottom"`, `"none"`, etc.).
#' @param label_key_height Height of legend keys in centimeters.
#' @param label_key_width Width of legend keys in centimeters.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param add_border Logical. If `TRUE`, adds a black border around the plot panel.
#' @param axis_units Logical. If `FALSE`, hides axis tick labels and tick marks
#'   while retaining axis titles.
#'
#' @return A `ggplot2` object containing a raster heatmap with field points,
#'   suitable for further modification or direct printing.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Example field dataset
#'if(!require(remotes)) install.packages("readr")
#'library (readr)
#'github="https://raw.githubusercontent.com/agronomy4future/raw_data_practice/refs/heads/main/SOC.csv"
#'df=data.frame(read_csv(url(github),show_col_types= FALSE))
#'
#'print(head(df,3))
#'Column  Row  SOC
#'    1   1    1.45
#'    2   1    1.60
#'    3   1    2.34
#'    .
#'    .
#'
#' # Install the agronomymap() package
#' if(!require(remotes)) install.packages("remotes")
#' if (!requireNamespace("agronomymap", quietly = TRUE)) {
#'  remotes::install_github("agronomy4future/agronomymap", force= TRUE)
#'}
#'library(remotes)
#'library(agronomymap)
#'
#' # Basic field heatmap
#' agronomymap(df,
#'             map= c("Column","Row"),
#'             variable= "SOC")
#'
#' # With styled format
#' agronomymap(
#'  data= df,
#'  map= c("Column","Row"),
#'  variable= "SOC",
#'  # fill scale
#'  grid_res = 100,
#'  palette= "Blues",
#'  fill_limits= c(0, 4),
#'  fill_breaks= seq(0, 4, 1),
#'  # label
#'  label_title= "Yield (kg/plant)",
#'  label_size= 12,
#'  label_key= 12,
#'  label_position= "right",
#'  label_key_height= 1.2,
#'  label_key_width= 0.7,
#'  xlab= "Row",
#'  ylab= "Column",
#'  # point aesthetics
#'  point_size= 1,
#'  point_shape= 21,
#'  point_color= "black",
#'  point_fill= "grey25",
#'  #border and axis unit
#'  add_border= TRUE,
#'  axis_units= TRUE
#')
#'*Github: https://github.com/agronomy4future/agronomymap
#' }
agronomymap = function(data,
                       map = c("Column", "Row"),
                       variable = "SOC",
                       spacing = 5,
                       grid_res = 70,
                       point_size = 1,
                       point_color = "black",
                       point_fill = "black",
                       point_shape = 21,
                       palette = "Blues",
                       fill_limits = NULL,
                       fill_breaks = NULL,
                       label_title = "VALUE",
                       label_size = 15,
                       label_key = 15,
                       label_position = "right",
                       label_key_height = 0.6,
                       label_key_width = 0.4,
                       xlab = "",
                       ylab = "",
                       add_border = TRUE,
                       axis_units = TRUE
) {

  suppressPackageStartupMessages({
    if (!requireNamespace("akima", quietly = TRUE))    install.packages("akima")
    if (!requireNamespace("imputeTS", quietly = TRUE)) install.packages("imputeTS")
    if (!requireNamespace("ggplot2", quietly = TRUE))  install.packages("ggplot2")
    if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")
    if (!requireNamespace("grid", quietly = TRUE))     install.packages("grid")
    if (!requireNamespace("scales", quietly = TRUE))   install.packages("scales")

    library(akima)
    library(imputeTS)
    library(ggplot2)
    library(RColorBrewer)
    library(grid)
    library(scales)
  })

  # ---- extract columns ----
  # IMPORTANT: we intentionally flip the order here so that
  # map = c("Column","Row") gives a tall rectangle:
  # - Row (10) on x-axis
  # - Column (20) on y-axis
  x_name = map[2]  # "Row"
  y_name = map[1]  # "Column"
  v_name = variable

  x_raw = as.numeric(data[[x_name]])
  y_raw = as.numeric(data[[y_name]])
  val   = as.numeric(data[[v_name]])

  ok_coord = is.finite(x_raw) & is.finite(y_raw)
  x_raw = x_raw[ok_coord]
  y_raw = y_raw[ok_coord]
  val   = val[ok_coord]
  val[!is.finite(val)] = 0

  # ---- numeric XY with spacing ----
  X = (x_raw - 1) * spacing   # Row → 10 positions
  Y = (y_raw - 1) * spacing   # Column → 20 positions

  df_points = data.frame(
    X = X,
    Y = Y,
    VALUE = val
  )

  # ---- interpolation ----
  interp_points = df_points[is.finite(df_points$VALUE), ]

  # grid size based on desired resolution but respecting aspect
  nx_real = length(unique(X))  # should be 10 for your df
  ny_real = length(unique(Y))  # should be 20 for your df

  # scale resolution in each direction
  if (is.null(grid_res) || grid_res <= 1) {
    nx = nx_real
    ny = ny_real
  } else {
    nx = grid_res
    # keep aspect: more cells in the longer dimension
    ny = round(grid_res * (ny_real / nx_real))
  }

  if (nrow(interp_points) < 4) {
    gx = seq(min(X), max(X), length.out = nx)
    gy = seq(min(Y), max(Y), length.out = ny)
    grid_df = expand.grid(X = gx, Y = gy)
    grid_df$VALUE = 0
  } else {
    grid_data = suppressWarnings(
      akima::interp(
        x = interp_points$X,
        y = interp_points$Y,
        z = interp_points$VALUE,
        nx = nx,
        ny = ny,
        extrap = TRUE
      )
    )

    grid_df = expand.grid(X = grid_data$x, Y = grid_data$y)
    grid_df$VALUE = imputeTS::na_interpolation(
      as.vector(grid_data$z),
      option = "spline"
    )
  }

  # ---- color limits ----
  all_vals = c(df_points$VALUE, grid_df$VALUE)
  if (is.null(fill_limits))
    fill_limits = range(all_vals, na.rm = TRUE)
  if (is.null(fill_breaks))
    fill_breaks = pretty(fill_limits)

  # ---- base plot ----
  p = ggplot() +
    geom_raster(data = grid_df,
                aes(x = X, y = Y, fill = VALUE)) +
    geom_point(data = df_points,
               aes(x = X, y = Y),
               shape = point_shape,
               size = point_size,
               color = point_color,
               fill = point_fill) +
    scale_fill_gradientn(
      colours = colorRampPalette(brewer.pal(9, palette))(100),
      limits  = fill_limits,
      breaks  = fill_breaks,
      name    = label_title,
      oob     = squish
    ) +
    labs(x = xlab, y = ylab) +
    coord_fixed() +  # preserve true aspect
    theme_classic(base_size = 12) +
    theme(
      legend.title      = element_text(size = label_size,
                                       margin = margin(b = 15)),
      legend.text       = element_text(size = label_key),
      legend.position   = label_position,
      legend.key.height = unit(label_key_height, "cm"),
      legend.key.width  = unit(label_key_width,  "cm")
    )

  # ---- axis relabeling ----
  x_unique = sort(unique(X))
  y_unique = sort(unique(Y))

  p = p +
    scale_x_continuous(
      breaks = x_unique,
      labels = sort(unique(data[[x_name]]))  # Row labels: 1..10
    ) +
    scale_y_continuous(
      breaks = y_unique,
      labels = sort(unique(data[[y_name]]))  # Column labels: 1..20
    )

  # ---- optional border ----
  if (add_border) {
    p = p + theme(panel.border = element_rect(color = "black",
                                              fill = NA,
                                              linewidth = 0.5))
  }

  # ---- OPTIONAL remove axis units ----
  if (!axis_units) {
    p = p + theme(
      axis.text.x  = element_blank(),
      axis.text.y  = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank()
    )
  }

  return(p)
}
