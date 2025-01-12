
#' This is
#' @param x a flextable object
#' @export
#' @return return a flextable that meets the need of academic paper
#' @example demo/flextable-format-tidier_demo.R
#' @details
#' In academic paper, we use three line table. This is a simpilified version to export the table to save time for group meeting.



themeGm <- function(x){
  apa.border <- list(width = 0.5,
                     color = "black", style = "solid")
  x <- flextable::font(x, part = "all", fontname = "Times New Roman")
  x <- flextable::line_spacing(x, space = 1, part = "all")
  x <- flextable::padding(x = x, padding.bottom = 0, padding.top = 0)
  x <- flextable::hline_top(x, part = "head", border = apa.border)
  x <- flextable::hline_bottom(x, part = "head", border = apa.border)
  x <- flextable::hline_top(x, part = "body", border = apa.border)
  x <- flextable::hline_bottom(x, part = "body", border = apa.border)
  x <- flextable::align(x, align = "center", part = "all")
  x <- flextable::valign(x, valign = "center", part = "all")
  flextable::fix_border_issues(x)
}


#' Function To add table name
#' @param x a flextable object
#' @param align align of the table name. It includes left, right and center
#' @param colwidths colwidth. It saves time to use col_keys length
#' @export
#' @return return a flextable that meets the need of academic paper
#' @example demo/flextable-format-tidier_demo.R
#' @details
#' In academic paper, we use three line table. This is a simpilified version to export the table to save time for group meeting.

addTableName <- function(tableName, align = "left", colwidths){

  x <- flextable::add_header_row(x = x, top = T, values = c(tableName), colwidths = colwidths) %>%
    flextable::align(x = ., i = 1, part = "header", align = align) %>%
    flextable::hline_top(border = flextable::fp_border_default(width = 0), part = "header")
  return(x)
}



#' Function To set table style
#' @param x a flextable object
#' @export
#' @return return a flextable that meets the need of academic paper
#' @example demo/flextable-format-tidier_demo.R
#' @details
#' In academic paper, we use three line table. This is a simpilified version to export the table to save time for group meeting.

formatTable <- function(x){

  x <- flextable::add_header_row(x = x, top = T, values = c(tableName), colwidths = colwidths) %>%
    flextable::align(x = ., i = 1, part = "header", align = align) %>%
    ftExtra::colformat_md(x = ., part = "all")

  return(x)
}





