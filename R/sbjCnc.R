#' subject conceive
#'
#' @param mesh your mesh term
#' @param st subject terms
#' @param toClip whether to export your result to clipboard
#' @template paramUnusedDots
#' @export
#' @return your subjects to use in pubmed
#' @example demo/sbjCnc_demo.R

sbjCnc <- function(mesh, st, toClip = FALSE, ...){
  a <- mesh
# b后面黏贴自由词
  b <- st
  c <- str_replace_all(string = b, pattern = "\n", replacement = "[Title/Abstract] OR ") %>% paste0(., "[Title/Abstract]")
  d <- paste0("(", a, "[Mesh Terms] OR ", c, ")")
  if(toClip == T){
    if(.Platform$OS.type == "windows"){
      write.table(x = d, file = "clipboard", quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
    }
    if(.Platform$OS.type == "unix"){
      clip <- pipe("pbcopy", "w")
      write.table(d, file=clip, quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
      close(clip)
    }
  }
  return(c)
}

