`%||%` <- function (a, b)
{
  if (!is.null(a))
    a
  else b
}

ext_trip <- function (x, y)
{
  c(range(x), range(y))
}
## from vaster
.check_args <- function (dimension, extent = NULL)
{
  if (is.null(dimension) || !length(dimension) == 2L)
    stop("dimension must be length = 2")
  if (!is.numeric(dimension))
    stop("dimension must be integer/numeric")
  if (anyNA(dimension) || any(dimension < 1))
    stop("dimension must be length 2, valid values for ncol,nrow >=1")
  if (!is.null(extent)) {
    if (!length(extent) == 4L)
      stop("extent must be length = 4 xmin,xmax,ymin,ymax")
    if (!is.numeric(extent))
      stop("extent must be numeric")
    if (anyNA(extent))
      stop("extent must be length 4, valid values for xmin, xmax, ymin, ymax")
    if (diff(extent[1:2]) <= 0)
      stop("invalid xmin,xmax - must be xmax > xmin")
    if (diff(extent[3:4]) <= 0)
      stop("invalid ymin,ymax - must be ymax > ymin")
  }
  invisible(NULL)
}




#' Track grid
#'
#' Computes the cell a track location point falls in on a grid.
#'
#' A grid is defined by a 'dimension' ('ncol', 'nrow') and 'extent' ('xmin', 'xmax',
#' 'ymin', 'ymax'). The cell index returned is in 'raster order', this is by top
#' row, left to right and down as per 'rasterImage'. This is aligned with usage
#' in the Github organization 'hypertidy' packages 'vaster' and 'ximage', and is
#' how other raster packages work.
#'
#' This function doesn't care if the x,y input values are longitude latitude or
#' x, y and it makes no difference at all. No account of movement between points
#' is made.
#'
#' @param x longitude or x
#' @param y latitude or y
#' @param dimension grid size 'nx', 'ny' 2 element vector (ncol, nrow)
#' @param extent grid extent, if not supplied we use the range of the data input
#'
#' @return cell index of each input point in the grid specification
#' @export
#'
#' @examples
#' dimension <- c(50, 35)
#' extent <- c(range(trips0$x), range(trips0$y))
#' cells <- track_grid(trips0$x, trips0$y, dimension = dimension, extent = extent)
#' plot(extent[1:2], extent[3:4], asp = 1, type = "n")
#' tab <- tabulate(cells, nbin = prod(dimension))
#' rasterImage(matrix(1 - (tab/max(tab)), dimension[2L], byrow = TRUE),
#' extent[1L], extent[3L], extent[2L], extent[4L], interpolate = FALSE)
#' points(trips0$x, trips0$y, pch = ".", col = "firebrick")
track_grid <- function(x, y, dimension, extent = NULL) {
  extent <- extent %||% ext_trip(x, y)
  .check_args(dimension, extent)
  xx <- x
  yy <- y
  len <- length(xx)
  ncols <- dimension[1L]
  nrows <- dimension[2L]
  xmin <- extent[1L]
  xmax <- extent[2L]
  ymin <- extent[3L]
  ymax <- extent[4L]
  yres_inv = nrows/(ymax - ymin)
  xres_inv = ncols/(xmax - xmin)
  row = floor((ymax - yy) * yres_inv)
  row <- ifelse(yy == ymin, nrows - 1, row)
  col = floor((xx - xmin) * xres_inv)
  col <- ifelse(xx == xmax, ncols - 1, col)
  ifelse(row < 0 | row >= nrows | col < 0 | col >= ncols, NA_real_,
         row * ncols + col + 1)
}

