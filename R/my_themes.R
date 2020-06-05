#' @export my_theme_booktabs
#' @title Apply my modified booktabs theme
#' @description Apply theme booktabs to a flextable
#' @param x a flextable object
#' @param fontsize size of font
#' @param fontsize font size in pixel
#' @param left_just_cols column index where left justify should stop (starts at 1)
#' @importFrom magrittr "%>%"
#' @examples
#' ftab <- flextable(iris)
#' ftab <- theme_booktabs(ftab)
#' @family flextable theme
my_theme_booktabs <- function(x, fontsize = 9, left_just_cols = 1){
  if( !inherits(x, "flextable") ) stop("theme_booktabs supports only flextable objects.")
  big_border <- fp_border(width = 2) ##changed this to one
  std_border <- fp_border(width = 1)
  h_nrow <- nrow_part(x, "header")
  f_nrow <- nrow_part(x, "footer")
  b_nrow <- nrow_part(x, "body")

  x <- border_remove(x)

  if(h_nrow > 0 ){
    x <- hline_top(x, border = big_border, part = "header")
    x <- hline(x, border = std_border, part = "header")
    x <- hline_bottom(x, border = big_border, part = "header")
  }
  if(b_nrow > 0 ){
    # x <- hline(x, border = std_border, part = "body")
    x <- hline_bottom(x, border = big_border, part = "body")
  }
  if(f_nrow > 0 ){
    # x <- hline(x, border = std_border, part = "footer")
    x <- hline_bottom(x, border = big_border, part = "footer")
  }
  x <- padding(x = x, padding.left = 5, padding.right = 5,
               padding.bottom = 2, padding.top = 2, part = "all")
  # x <- align_text_col(x, align = "left", header = TRUE)
  x <- align_nottext_col(x, align = "center", header = TRUE)
  x <- bg(x = x, bg = "transparent", part = "all")
  x <- fontsize(x, size = fontsize, part = "all") %>%
    align(j = 1:left_just_cols, align = 'left', part = "all") %>%
    bold(bold = TRUE, part = "header")
  x

}
