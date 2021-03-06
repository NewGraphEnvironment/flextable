chunk_dataframe <- function(...){
  x <- list(...)

  img_data <- x$img_data
  x$img_data <- NULL
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  def_chr <- rep(NA_character_, nrow(x))
  def_dbl <- rep(NA_real_, nrow(x))
  def_lgl <- as.logical(rep(NA_integer_, nrow(x)))
  def_lst <- rep(list(NULL), nrow(x))
  data0 <- data.frame(
    txt = def_chr,
    font.size = def_dbl,
    italic = def_lgl,
    bold = def_lgl,
    underlined = def_lgl,
    color = def_chr,
    shading.color = def_chr,
    font.family = def_chr,
    vertical.align = def_chr,
    width = def_dbl,
    height = def_dbl,
    url = def_chr,
    stringsAsFactors = FALSE )
  data0$img_data <- def_lst

  data0[names(x)] <- x
  if( !is.null(img_data))
    data0$img_data <- img_data
  data0
}

default_fptext_prop <- structure(list(
  font.size = NA_real_,
  bold = as.logical(NA_integer_),
  italic = as.logical(NA_integer_),
  underlined = as.logical(NA_integer_),
  color = NA_character_,
  shading.color = NA_character_,
  font.family = NA_character_,
  vertical.align = NA_character_),
  class = "fp_text")


#' @export
#' @title chunk of text wrapper
#' @description The function lets add text within flextable
#' objects with function \code{\link{compose}}.
#' It should be used inside a call to \code{\link{as_paragraph}}.
#' @param x text or any element that can be formatted as text
#' with function provided in argument \code{formatter}.
#' @param props an \code{\link[officer]{fp_text}} object to be used to format the text.
#' If not specified, it will be the default value corresponding to the cell.
#' @param formatter a function that will format x as a character vector.
#' @param ... additional arguments for \code{formatter} function.
#' @family chunk elements for paragraph
#' @examples
#' library(officer)
#'
#' ft <- flextable( head(iris))
#'
#' ft <- compose( ft, j = "Sepal.Length",
#'  value = as_paragraph(
#'    "Sepal.Length value is ",
#'    as_chunk(Sepal.Length, props = fp_text(color = "red"))
#'  ),
#'  part = "body")
#' ft <- color(ft, color = "gray40", part = "all")
#' ft <- autofit(ft)
#' ft
#' @section Illustrations:
#'
#' \if{html}{\figure{fig_as_chunk_1.png}{options: width=60\%}}
as_chunk <- function(x, props = NULL, formatter = format_fun, ...) {

  if(is.function(x)){
    stop("argument `x` in function `as_chunk` cannot be a function", call. = FALSE)
  }

  text <- formatter(x, ...)

  if( is.null(props) ){
    props <- default_fptext_prop
  }

  if( inherits(props, "fp_text") ){
    props <- rep(list(props), length(text))
  }

  if( length(props) > 0 && is.list(props) ){
    if( !all(sapply(props, inherits, "fp_text")) ){
      stop("props should be a list of fp_text object")
    }
    if( length(props) != length(text) ){
      stop("props should be a list of length ", length(text) )
    }
  }

  data <- chunk_dataframe(txt = text,
                  font.size = sapply(props, function(x) x$font.size),
                  italic = sapply(props, function(x) x$italic),
                  bold = sapply(props, function(x) x$bold),
                  underlined = sapply(props, function(x) x$underlined),
                  color = sapply(props, function(x) x$color),
                  shading.color = sapply(props, function(x) x$shading.color),
                  font.family = sapply(props, function(x) x$font.family),
                  vertical.align = sapply(props, function(x) x$vertical.align) )
  class(data) <- c("chunk", "data.frame")
  data
}


#' @export
#' @title subscript chunk
#' @description The function is producing a chunk with
#' subscript vertical alignment.
#' @note
#' This is a sugar function that ease the composition of complex
#' labels made of different formattings. It should be used inside a
#' call to \code{\link{as_paragraph}}.
#' @param x value, if a chunk, the chunk will be updated
#' @family chunk elements for paragraph
#' @examples
#' ft <- flextable( head(iris), col_keys = c("dummy") )
#'
#' ft <- compose(ft, i = 1, j = "dummy", part = "header",
#'     value = as_paragraph(
#'       as_sub("Sepal.Length"),
#'       " anything "
#'     ) )
#'
#' ft <- autofit(ft)
#' ft
#' @section Illustrations:
#'
#' \if{html}{\figure{fig_as_sub_1.png}{options: width=30\%}}
as_sub <- function(x){
  if( !inherits(x, "chunk") ){
    x <- as_chunk(x, formatter = format_fun)
  }
  x$vertical.align = "subscript"
  x
}
#' @export
#' @title superscript chunk
#' @description The function is producing a chunk with
#' superscript vertical alignment.
#' @inheritParams as_sub
#' @note
#' This is a sugar function that ease the composition of complex
#' labels made of different formattings. It should be used inside a
#' call to \code{\link{as_paragraph}}.
#' @family chunk elements for paragraph
#' @examples
#' ft <- flextable( head(iris), col_keys = c("dummy") )
#'
#' ft <- compose(ft, i = 1, j = "dummy", part = "header",
#'     value = as_paragraph(
#'       " anything ",
#'       as_sup("Sepal.Width")
#'     ) )
#'
#' ft <- autofit(ft)
#' ft
#' @section Illustrations:
#'
#' \if{html}{\figure{fig_as_sup_1.png}{options: width=30\%}}
as_sup <- function(x){
  if( !inherits(x, "chunk") ){
    x <- as_chunk(x, formatter = format_fun)
  }
  x$vertical.align = "superscript"
  x
}


#' @export
#' @title bold chunk
#' @description The function is producing a chunk with
#' bold font.
#' @note
#' This is a sugar function that ease the composition of complex
#' labels made of different formattings. It should be used inside a
#' call to \code{\link{as_paragraph}}.
#' @inheritParams as_sub
#' @family chunk elements for paragraph
#' @examples
#' ft <- flextable( head(iris),
#'   col_keys = c("Sepal.Length", "dummy") )
#'
#' ft <- compose(ft, j = "dummy",
#'     value = as_paragraph(
#'       as_b(Sepal.Length)
#'     ) )
#'
#' ft
#' @section Illustrations:
#'
#' \if{html}{\figure{fig_as_b_1.png}{options: width=30\%}}
as_b <- function(x){
  if( !inherits(x, "chunk") ){
    x <- as_chunk(x, formatter = format_fun)
  }
  x$bold = TRUE
  x
}

#' @export
#' @title italic chunk
#' @description The function is producing a chunk with
#' italic font.
#' @note
#' This is a sugar function that ease the composition of complex
#' labels made of different formattings. It should be used inside a
#' call to \code{\link{as_paragraph}}.
#' @inheritParams as_sub
#' @family chunk elements for paragraph
#' @examples
#' ft <- flextable( head(iris),
#'   col_keys = c("Sepal.Length", "dummy") )
#'
#' ft <- compose(ft, j = "dummy",
#'   value = as_paragraph(as_i(Sepal.Length)) )
#'
#' ft
#' @section Illustrations:
#'
#' \if{html}{\figure{fig_as_i_1.png}{options: width=30\%}}
as_i <- function(x){
  if( !inherits(x, "chunk") ){
    x <- as_chunk(x, formatter = format_fun)
  }
  x$italic = TRUE
  x
}

#' @export
#' @title chunk with values in brackets
#' @description The function is producing a chunk by
#' pasting values and add the result in brackets.
#' It should be used inside a call to \code{\link{as_paragraph}}.
#' @param ... text and column names
#' @param sep separator
#' @param p prefix, default to '('
#' @param s suffix, default to ')'
#' @family chunk elements for paragraph
#' @examples
#' ft <- flextable( head(iris),
#'   col_keys = c("Species", "Sepal", "Petal") )
#' ft <- set_header_labels(ft, Sepal="Sepal", Petal="Petal")
#' ft <- compose(ft, j = "Sepal",
#'   value = as_paragraph( as_bracket(Sepal.Length, Sepal.Width) ) )
#' ft <- compose(ft, j = "Petal",
#'   value = as_paragraph( as_bracket(Petal.Length, Petal.Width) ) )
#' ft
#' @section Illustrations:
#'
#' \if{html}{\figure{fig_as_bracket_1.png}{options: width=35\%}}
as_bracket <- function(..., sep = ", ", p = "(", s = ")"){
  x <- list(...)
  x <- lapply(x, formatC)
  x$sep <- sep
  x <- do.call(paste, x)
  x <- paste0(p, x, s)
  as_chunk(x)
}

#' @importFrom grDevices as.raster
#' @export
#' @title image chunk wrapper
#' @description The function lets add images within flextable
#' objects with function \code{\link{compose}}.
#' It should be used inside a call to \code{\link{as_paragraph}}.
#' @param src image filename
#' @param width,height size of the png file in inches
#' @param ... unused argument
#' @family chunk elements for paragraph
#' @note
#' This chunk option requires package officedown in a R Markdown
#' context with Word output format.
#'
#' PowerPoint cannot mix images and text in a paragraph, images
#' are removed when outputing to PowerPoint format.
#' @seealso \code{\link{compose}}, \code{\link{as_paragraph}}
#' @examples
#' img.file <- file.path( R.home("doc"), "html", "logo.jpg" )
#' library(officer)
#'
#' myft <- flextable( head(iris))
#'
#' myft <- compose( myft, i = 1:3, j = 1,
#'  value = as_paragraph(
#'    as_image(src = img.file, width = .20, height = .15),
#'    " blah blah ",
#'    as_chunk(Sepal.Length, props = fp_text(color = "red"))
#'  ),
#'  part = "body")
#'
#' ft <- autofit(myft)
#' ft
#' @section Illustrations:
#'
#' \if{html}{\figure{fig_as_image_1.png}{options: width=60\%}}
as_image <- function(src, width = .5, height = .2, ...) {

  if( length(src) > 1 ){
    if( length(width) == 1 ) width <- rep(width, length(src))
    if( length(height) == 1 ) height <- rep(height, length(src))
  }

  data <- chunk_dataframe(width = as.double(width),
                  height = as.double(height),
                  img_data = src
                  )
  class(data) <- c("img_src", "chunk", "data.frame")
  data
}


#' @export
#' @title chunk of text with hyperlink wrapper
#' @description The function lets add hyperlinks within flextable
#' objects with function \code{\link{compose}}.
#' It should be used inside a call to \code{\link{as_paragraph}}.
#' @note
#' This chunk option requires package officedown in a R Markdown
#' context with Word output format.
#' @inheritParams as_chunk
#' @param url url to be used
#' @seealso \code{\link{display}}
#' @examples
#' dat <- data.frame(
#'   col = "Google it",
#'   href = "https://www.google.fr/search?source=hp&q=flextable+R+package",
#'   stringsAsFactors = FALSE)
#'
#' ftab <- flextable(dat)
#' ftab <- compose( x = ftab, j = "col",
#'   value = as_paragraph(
#'     "This is a link: ",
#'     hyperlink_text(x = col, url = href ) ) )
#' ftab
#' @family chunk elements for paragraph
hyperlink_text <- function(x, props = NULL, formatter = format_fun, url, ...){
  zz <- data.frame(x = x, url = url, stringsAsFactors = FALSE)
  x <- as_chunk( x = zz$x, props = props, formatter = formatter, ...)
  x$url <- zz$url
  x
}


#' @export
#' @title mini barplots chunk wrapper
#' @description This function is used to insert bars into
#' flextable with function \code{\link{compose}}.
#' It should be used inside a call to \code{\link{as_paragraph}}
#' @param value values containing the bar size
#' @param max max bar size
#' @param barcol bar color
#' @param bg background color
#' @param width,height size of the resulting png file in inches
#' @note
#' This chunk option requires package officedown in a R Markdown
#' context with Word output format.
#'
#' PowerPoint cannot mix images and text in a paragraph, images
#' are removed when outputing to PowerPoint format.
#' @family chunk elements for paragraph
#' @examples
#' ft <- flextable( head(iris, n = 10 ))
#'
#' ft <- compose(ft, j = 1,
#'   value = as_paragraph(
#'     minibar(value = Sepal.Length, max = max(Sepal.Length))
#'   ),
#'   part = "body")
#'
#' ft <- autofit(ft)
#' ft
#' @importFrom grDevices as.raster col2rgb rgb
#' @seealso \code{\link{compose}}, \code{\link{as_paragraph}}
#' @section Illustrations:
#'
#' \if{html}{\figure{fig_minibar_1.png}{options: width=60\%}}
minibar <- function(value, max = NULL, barcol = "#CCCCCC", bg = "transparent", width = 1, height = .2) {


  if( all( is.na(value) ) ){
    max <- 1
  }
  value[is.na(value)] <- 0

  if( is.null(max))
    max <- max(value, na.rm = TRUE)

  stopifnot(value >= 0, max >= 0)
  barcol <- rgb(t(col2rgb(barcol))/255)
  bg <- ifelse( bg == "transparent", bg, rgb(t(col2rgb(bg))/255) )
  if( any(value > max) ){
    warning("value > max, truncate to max")
    value[value > max] <- max
  }
  width_ <- as.integer(width * 72)
  value <- as.integer( (value / max) * width_ )
  n_empty <- width_ - value

  rasters <- mapply(function(count_on, count_off, bg_on, bg_off ){
    as.raster( matrix(c(rep(bg_on, count_on), rep(bg_off, count_off)), nrow = 1) )
  }, value, n_empty, bg_on = barcol, bg_off = bg,
  SIMPLIFY = FALSE )
  z <- chunk_dataframe(width = as.double(rep(width, length(value)) ),
                  height = as.double(rep(height, length(value))),
                  img_data = rasters )

  class(z) <- c("img_chunk", "chunk", "data.frame")
  z
}

#' @export
#' @title mini linerange chunk wrapper
#' @description This function is used to insert lineranges into
#' flextable with function \code{\link{compose}}.
#' It should be used inside a call to \code{\link{as_paragraph}}
#' @param value values containing the bar size
#' @param min min bar size. Default min of value
#' @param max max bar size. Default max of value
#' @param rangecol bar color
#' @param stickcol jauge color
#' @param bg background color
#' @param width,height size of the resulting png file in inches
#' @param raster_width number of pixels used as width
#' when interpolating value.
#' @note
#' This chunk option requires package officedown in a R Markdown
#' context with Word output format.
#'
#' PowerPoint cannot mix images and text in a paragraph, images
#' are removed when outputing to PowerPoint format.
#' @family chunk elements for paragraph
#' @examples
#' myft <- flextable( head(iris, n = 10 ))
#'
#' myft <- compose( myft, j = 1,
#'   value = as_paragraph(
#'     linerange(value = Sepal.Length)
#'   ),
#'   part = "body")
#'
#' autofit(myft)
#' @importFrom grDevices as.raster col2rgb rgb
#' @importFrom stats approx
#' @seealso \code{\link{compose}}, \code{\link{as_paragraph}}
linerange <- function(value, min = NULL, max = NULL, rangecol = "#CCCCCC",
                      stickcol = "#FF0000", bg = "transparent", width = 1,
                      height = .2, raster_width = 30) {
  if( all( is.na(value) ) ){
    min <- 0
    max <- 1
  }

  if( raster_width < 2)
    stop("raster_width must be greater than 1")

  raster_nrow <- 9
  raster_center <- 5

  if( is.null(max))
    max <- max(value, na.rm = TRUE)
  if ( is.null(min))
    min <- min(value, na.rm = TRUE)

  value[!is.finite(value)] <- max + 1 # to be sure not displayed

  stopifnot(!is.null(value), !is.null(min), !is.null(min))

  # transform color code
  stickcol  <- rgb(t(col2rgb(stickcol))/255)
  rangecol  <- rgb(t(col2rgb(rangecol))/255)
  bg <- ifelse( bg == "transparent", bg, rgb(t(col2rgb(bg))/255) )


  # get value approx on range 1,raster_width
  stick_pos <- as.integer(approx(x = c(min,max), y = c(1, raster_width), xout = value)$y)
  base <- matrix(bg, nrow = raster_nrow, ncol = raster_width)
  base[, 1]   <- rangecol
  base[, raster_width] <- rangecol
  base[raster_center,] <- rangecol

  rasters <- lapply(stick_pos, function(val, def_mat, col) {
    newmat <- def_mat
    newmat[, val] <- col
    as.raster(newmat)
  }, base, stickcol)

  z <- chunk_dataframe(width = as.double(rep(width, length(value)) ),
                  height = as.double(rep(height, length(value))),
                  img_data = rasters )

  class(z) <- c("img_chunk", "chunk", "data.frame")
  z

}

#' @export
#' @title mini lollipop chart chunk wrapper
#' @description This function is used to insert lollipop charts into
#' flextable with function \code{\link{compose}}.
#' It should be used inside a call to \code{\link{as_paragraph}}
#' @param value values containing the bar size
#' @param min min bar size. Default min of value
#' @param max max bar size. Default max of value
#' @param rangecol bar color
#' @param bg background color
#' @param width,height size of the resulting png file in inches
#' @param raster_width number of pixels used as width
#' @param positivecol box color of positive values
#' @param negativecol box color of negative values
#' @param neutralcol box color of neutral values
#' @param neutralrange minimal and maximal range of neutral values (default: 0)
#' @param rectanglesize size of the rectangle (default: 2, max: 5)
#' when interpolating value.
#' @note
#' This chunk option requires package officedown in a R Markdown
#' context with Word output format.
#'
#' PowerPoint cannot mix images and text in a paragraph, images
#' are removed when outputing to PowerPoint format.
#' @family chunk elements for paragraph
#' @examples
#' iris$Sepal.Ratio <- (iris$Sepal.Length - mean(iris$Sepal.Length))/mean(iris$Sepal.Length)
#' ft <- flextable( tail(iris, n = 10 ))
#'
#' ft <- compose( ft, j = "Sepal.Ratio",   value = as_paragraph(
#'   lollipop(value = Sepal.Ratio, min=-.25, max=.25)
#' ),
#' part = "body")
#'
#' ft <- autofit(ft)
#' ft
#' @importFrom grDevices as.raster col2rgb rgb
#' @importFrom stats approx
#' @seealso \code{\link{compose}}, \code{\link{as_paragraph}}
#' @section Illustrations:
#'
#' \if{html}{\figure{fig_lollipop_1.png}{options: width=60\%}}
lollipop <- function(value, min = NULL, max = NULL, rangecol = "#CCCCCC",
                     bg = "transparent", width = 1,
                     height = .2, raster_width = 30, positivecol = "#00CC00",
                     negativecol = "#CC0000", neutralcol = "#CCCCCC", neutralrange = c(0,0),
                     rectanglesize = 2) {
  if( all( is.na(value) ) ){
    min <- 0
    max <- 1
  }

  if( raster_width < 2)
    stop("raster_width must be greater than 1")

  raster_nrow <- 9
  raster_center <- 5

  if(rectanglesize>5)
    rectanglesize <- 5

  if( is.null(max))
    max <- max(c(0,value), na.rm = TRUE)
  if ( is.null(min))
    min <- min(c(0,value), na.rm = TRUE)

  value[!is.finite(value)] <- max + 1 # to be sure not displayed

  stopifnot(!is.null(value), !is.null(min), !is.null(min))

  # transform color code
  rangecol  <- rgb(t(col2rgb(rangecol))/255)
  positivecol  <- rgb(t(col2rgb(positivecol))/255)
  negativecol  <- rgb(t(col2rgb(negativecol))/255)
  neutralcol <- rgb(t(col2rgb(neutralcol))/255)
  bg <- ifelse( bg == "transparent", bg, rgb(t(col2rgb(bg))/255) )


  # get value approx on range 1,raster_width
  zero_pos <- as.integer(approx(x = c(min,max), y = c(1, raster_width), xout = 0)$y)

  base <- matrix(bg, nrow = raster_nrow, ncol = raster_width)
  base[, 1]   <- rangecol
  base[, raster_width] <- rangecol
  base[, zero_pos] <- rangecol

  rasters <- lapply(value, function(val, def_mat) {
    stick_pos <- as.integer(approx(x = c(min,max), y = c(1, raster_width), xout = val)$y)

    newmat <- def_mat

    # fix switch statement and neutral color range
    value_position <- sign(val)
    value_position <- ifelse(val <= neutralrange[1] & val >= neutralrange[2], 0, value_position)

    rectangle_color <- switch(value_position + 2, negativecol, neutralcol, positivecol)

    # create middle stick
    if(value_position == -1)
      newmat[row(newmat) == raster_center & col(newmat) < zero_pos & col(newmat) > stick_pos] <- rangecol
    else
      newmat[row(newmat) == raster_center & col(newmat) > zero_pos & col(newmat) < stick_pos] <- rangecol

    # create rectangle
    newmat[col(newmat)>=stick_pos - rectanglesize/2 & col(newmat)<=stick_pos + rectanglesize/2 &
             row(newmat)>=raster_center - rectanglesize & row(newmat)<=raster_center + rectanglesize] <- rectangle_color

    as.raster(newmat)
  }, base)

  z <- chunk_dataframe(width = as.double(rep(width, length(value)) ),
                       height = as.double(rep(height, length(value))),
                       img_data = rasters )

  class(z) <- c("img_chunk", "chunk", "data.frame")
  z

}

#' @export
#' @title concatenate chunks in a flextable
#' @description The function is concatenating text and images within paragraphs of
#' a flextable object, this function is to be used with function \code{\link{compose}}.
#' @param ... chunk elements that are defining paragraph
#' @param list_values a list of chunk elements that are defining paragraph. If
#' specified argument \code{...} is unused.
#' @seealso \code{\link{as_chunk}}, \code{\link{minibar}},
#' \code{\link{as_image}}, \code{\link{hyperlink_text}}
#' @examples
#' library(officer)
#' ft <- flextable( head(iris, n = 10 ))
#'
#' ft <- compose(ft, j = 1,
#'   value = as_paragraph(
#'     minibar(value = Sepal.Length, max = max(Sepal.Length)),
#'     " ",
#'     as_chunk( Sepal.Length, formatter = formatC,
#'              props = fp_text(color = "orange") ),
#'     " blah blah"
#'   ),
#'   part = "body")
#'
#' ft <- autofit(ft)
#' ft
#' @section Illustrations:
#'
#' \if{html}{\figure{fig_as_paragraph_1.png}{options: width=60\%}}
as_paragraph <- function( ..., list_values = NULL ){
  if( is.null(list_values)) {
    list_values <- list(...)
  }

  if( any( is_atomic <- sapply(list_values, is.atomic) ) ){
    list_values[is_atomic] <- lapply(list_values[is_atomic], function(x){
      as_chunk(x, formatter = format_fun)
    })
  }

  if( !all( sapply(list_values, inherits, "chunk") ) ){
    stop("argument ... should only contain objects of class 'chunk'.")
  }

  nrows <- sapply(list_values, nrow)
  exp_nrow <- max(nrows)

  if( length(nrows) != 1 && length(id_recycl <- which( nrows == 1 )) > 0){

    list_values[id_recycl] <- lapply(list_values[id_recycl], function(x, n){
      z <- rbind.match.columns( rep(list(x), n) )
      z
    }, exp_nrow)

  }

  nrows <- sapply(list_values, nrow)

  if( length( unique(nrows) ) != 1 && 1 %in% nrows ){
    which_ <- which(nrows %in% 1)
    list_values[which_] <- lapply(list_values[which_], function(x, n){
      rbind.match.columns(rep(list(x), n))
    }, n = max(nrows, na.rm = TRUE))
  }
  nrows <- sapply(list_values, nrow)

  if( length( unique(nrows) ) != 1 ){
    stop('paragraph elements should all have the same length')
  }

  data <- mapply(function(x, index){
    x$seq_index <- rep(index, nrow(x) )
    x
  }, list_values, seq_along(list_values), SIMPLIFY = FALSE, USE.NAMES = FALSE)
  data <- rbind.match.columns(data )

  data <- split( data, rep( seq_len(nrow(list_values[[1]])), length((list_values)) ) )
  names(data) <- NULL
  class(data) <- c("paragraph")
  data
}



