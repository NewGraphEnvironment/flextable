
rbind.match.columns <- function(list_df) {
  col <- unique(unlist(sapply(list_df, names)))

  list_df <- lapply(list_df, function(x, col) {
    x[, setdiff(col, names(x))] <- NA
    x
  }, col = col)
  list_df <- do.call(rbind, list_df)
  row.names(list_df) <- NULL
  list_df
}



get_i_from_formula <- function( f, data ){
  if( length(f) > 2 )
    stop("formula selection is not as expected ( ~ condition )", call. = FALSE)
  i <- eval(as.call(f[[2]]), envir = data)
  if( !is.logical(i) )
    stop("formula selection should return a logical vector", call. = FALSE)
  i
}
get_j_from_formula <- function( f, data ){
  if( length(f) > 2 )
    stop("formula selection is not as expected ( ~ variables )", call. = FALSE)
  j <- attr(terms(f, data = data), "term.labels")
  names_ <- names(data)
  if( any( invalid_names <- (!j %in% names_) ) ){
    invalid_names <- paste0("[", j[invalid_names], "]", collapse = ", ")
    stop("unknown variables:", invalid_names, call. = FALSE)
  }
  j
}

check_formula_i_and_part <- function(i, part){
  if( inherits(i, "formula") && "header" %in% part ){
    stop("formula in argument i cannot adress part '", part, "'.", call. = FALSE)
  } else if( inherits(i, "formula") && "footer" %in% part ){
    stop("formula in argument i cannot adress part '", part, "'.", call. = FALSE)
  }
  TRUE
}

nrow_part <- function(x, part){
  if( is.null(x[[part]]) )
    0
  else if( is.null(x[[part]]$dataset) )
    0
  else nrow(x[[part]]$dataset)
}

#' @importFrom xml2 xml_attr<-
process_url <- function(rel, url, str, pattern, double_esc = TRUE){

  if(double_esc)
    escape <- function(x) htmlEscape(htmlEscape(x))
  else escape <- function(x) htmlEscape(x)# it seems that word does not behave as powerpoint


  doc <- as_xml_document(str)
  for(url_ in url){
    new_rid <- sprintf("rId%.0f", rel$get_next_id())
    rel$add(
      id = new_rid, type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink",
      target = escape(url_), target_mode = "External" )

    linknodes <- xml_find_all(doc, paste0("//", pattern, "[@r:id=", shQuote(url_), "]"))
    xml_attr(linknodes, "r:id") <- new_rid
  }

  as.character(doc)
}

absolute_path <- function(x){

  if (length(x) != 1L)
    stop("'x' must be a single character string")
  epath <- path.expand(x)

  if( file.exists(epath)){
    epath <- normalizePath(epath, "/", mustWork = TRUE)
  } else {
    if( !dir.exists(dirname(epath)) ){
      stop("directory of ", x, " does not exist.", call. = FALSE)
    }
    cat("", file = epath)
    epath <- normalizePath(epath, "/", mustWork = TRUE)
    unlink(epath)
  }
  epath
}

#' @importFrom knitr opts_current
ref_label <- function() {
  label <- opts_current$get('label')
  if (is.null(label)) return('')
  paste0("(\\#tab:", label, ")")
}

has_label <- function(x) {
  grepl("^\\(\\\\#tab:[-[:alnum:]]+\\)", x)
}


#' @importFrom stats median median sd mad
#' @importFrom stats quantile
Q1 <- function(z) as.double(quantile(z, probs = .25, na.rm = TRUE, names = FALSE))
Q3 <- function(z) as.double(quantile(z, probs = .75, na.rm = TRUE, names = FALSE))
MEDIAN <- function(z) as.double(median(z, na.rm = TRUE))
MEAN <- function(z) as.double(mean(z, na.rm = TRUE))
SD <- function(z) as.double(sd(z, na.rm = TRUE))
MAD <- function(z) as.double(mad(z, na.rm = TRUE))
MIN <- function(z) as.double(min(z, na.rm = TRUE))
MAX <- function(z) as.double(max(z, na.rm = TRUE))
N <- function(z) length(z)
NAS <- function(z) sum(is.na(z))


#' @importFrom uuid UUIDgenerate
as_bookmark <- function(id, str) {
  new_id <- UUIDgenerate()
  bm_start_str <- sprintf("<w:bookmarkStart w:id=\"%s\" w:name=\"%s\"/>", new_id, id)
  bm_start_end <- sprintf("<w:bookmarkEnd w:id=\"%s\"/>", new_id)
  paste0(bm_start_str, str, bm_start_end)
}

#' @importFrom officer to_wml
caption_chunks_str <- function(label, autonum = NULL){
  run_str <- sprintf("<w:r><w:t xml:space=\"preserve\">%s</w:t></w:r>",
                     label)
  if(!is.null(autonum)){
    autonum <- to_wml(autonum)
    run_str <- paste0(autonum, run_str)
  }
  run_str

}
pandoc_chunks_wml <- function(x, bookdown){
  if(!bookdown){
    run_str <- caption_chunks_str(x$caption$value, x$caption$autonum)
    if(!is.null(x$caption$ref)){
      run_str <- as_bookmark(x$caption$ref, run_str)
    }
    paste0("`", run_str, "`{=openxml}")
  } else {
    x$caption$value
  }

}
pandoc_chunks_html <- function(x, bookdown){
  if(!bookdown){
    run_str <- sprintf("<div class=\"%s\">%s</div>", x$caption$style, x$caption$value)
  } else {
    run_str <- x$caption$value
  }
  run_str
}


