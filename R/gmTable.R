#' Academic table with name, footnote and zoomable HTML output
#'
#' @description
#' A one-stop wrapper that turns a data frame (or an existing flextable) into a
#' three-line academic table with a table name (caption) and footnote(s).
#' When knitted to HTML, the table is wrapped in a zoomable container with a
#' toolbar (fit-to-width / slider / +- buttons / Ctrl + mouse wheel), so a wide
#' table can be viewed as a whole and zoomed in for detail instead of being
#' trapped behind a horizontal scrollbar. For non-HTML output (Word / PDF) the
#' zoom layer is dropped and a plain static three-line flextable is returned.
#'
#' @param x a data.frame / tibble, or an already built flextable object
#' @param tableName the table name (caption) shown on top of the table. NULL to omit.
#' @param footnote a character string or character vector. Each element becomes
#'   one footnote line below the table. NULL to omit.
#' @param zoom whether to wrap the table in a zoomable container in HTML output
#'   (default TRUE). Ignored for non-HTML output.
#' @param align alignment of the table name and footnote, one of "left",
#'   "center", "right" (default "left").
#' @param md whether to interpret cell content as markdown via
#'   \code{ftExtra::colformat_md} (default FALSE).
#' @param theme whether to apply the academic three-line style via
#'   \code{themeGm} (default TRUE). Set FALSE to keep a flextable you have
#'   already styled yourself untouched.
#' @param maxHeight CSS max-height of the zoom viewport, e.g. "80vh" or "600px"
#'   (default "80vh"). Very tall tables scroll vertically inside the viewport.
#' @return In HTML output (and \code{zoom = TRUE}) an \code{htmltools} tag list
#'   that renders a zoomable table; otherwise a flextable object.
#' @seealso [themeGm()] [addTableName()]
#' @example demo/gmTable_demo.R
#' @export
gmTable <- function(x, tableName = NULL, footnote = NULL, zoom = TRUE,
                    align = "left", md = FALSE, theme = TRUE, maxHeight = "80vh"){
  ## build flextable
  if(!inherits(x, "flextable")){
    x <- flextable::flextable(data.frame(x, check.names = FALSE))
  }
  if(md){
    if(!requireNamespace("ftExtra", quietly = TRUE)){
      stop("Package 'ftExtra' is required when md = TRUE. Please install it.")
    }
    x <- ftExtra::colformat_md(x, part = "all")
  }
  if(theme){
    x <- themeGm(x)
  }
  if(!is.null(tableName)){
    x <- addTableName(x, tableName = tableName, align = align)
  }
  if(!is.null(footnote)){
    for(ln in footnote){
      x <- flextable::add_footer_lines(x, values = ln)
    }
    x <- flextable::align(x, part = "footer", align = align)
    x <- flextable::font(x, part = "footer", fontname = "Times New Roman")
    x <- flextable::fontsize(x, part = "footer", size = 9)
  }

  ## decide output
  isHtml <- requireNamespace("knitr", quietly = TRUE) && isTRUE(knitr::is_html_output())
  if(zoom && isHtml){
    return(zoomWrap(x, maxHeight = maxHeight))
  }
  return(x)
}


#' Wrap a flextable (or any html) in a zoomable container for HTML output
#'
#' @description
#' Wrap a flextable into a self-contained HTML widget with a zoom toolbar
#' (fit-to-width / slider / +- / Ctrl + wheel). Uses the CSS \code{zoom}
#' property so the layout reflows correctly and the table never overlaps the
#' following content. Intended for HTML R Markdown output.
#'
#' @param x a flextable object
#' @param maxHeight CSS max-height of the viewport (default "80vh")
#' @return an \code{htmltools} tag list
#' @export
zoomWrap <- function(x, maxHeight = "80vh"){
  if(!requireNamespace("htmltools", quietly = TRUE)){
    stop("Package 'htmltools' is required for zoomWrap(). Please install it.")
  }
  id <- paste0("gmtbl_", as.integer(stats::runif(1, 1e6, 1e9)))
  ft <- flextable::htmltools_value(x)

  head <- paste0(
'<style>',
'.gm-zoom{border:1px solid #ddd;border-radius:6px;margin:1em 0;}',
'.gm-zoom-toolbar{display:flex;align-items:center;gap:6px;padding:6px 8px;background:#f7f7f7;border-bottom:1px solid #eee;font-family:sans-serif;}',
'.gm-zoom-toolbar button{cursor:pointer;border:1px solid #ccc;background:#fff;border-radius:4px;padding:2px 8px;font-size:13px;line-height:1.4;}',
'.gm-zoom-toolbar button:hover{background:#ececec;}',
'.gm-zoom-range{flex:0 0 140px;}',
'.gm-zoom-label{font-size:12px;color:#555;min-width:44px;text-align:right;font-family:monospace;}',
'.gm-zoom-spacer{flex:1;}',
'.gm-zoom-viewport{overflow:auto;max-height:', maxHeight, ';padding:8px;}',
'.gm-zoom-content{transform-origin:top left;display:inline-block;}',
'</style>',
'<div class="gm-zoom" id="', id, '">',
'<div class="gm-zoom-toolbar">',
'<button type="button" data-act="out" title="缩小">&#8722;</button>',
'<input class="gm-zoom-range" type="range" min="25" max="300" step="5" value="100">',
'<button type="button" data-act="in" title="放大">&#43;</button>',
'<span class="gm-zoom-label">100%</span>',
'<span class="gm-zoom-spacer"></span>',
'<button type="button" data-act="fit" title="适应宽度">适应宽度</button>',
'<button type="button" data-act="reset" title="100%">100%</button>',
'</div>',
'<div class="gm-zoom-viewport"><div class="gm-zoom-content">'
  )

  tail <- paste0(
'</div></div></div>',
'<script>(function(){',
'var root=document.getElementById("', id, '");if(!root)return;',
'var content=root.querySelector(".gm-zoom-content");',
'var viewport=root.querySelector(".gm-zoom-viewport");',
'var range=root.querySelector(".gm-zoom-range");',
'var label=root.querySelector(".gm-zoom-label");',
'var natW=0;',
'function measure(){content.style.zoom=1;natW=content.offsetWidth;}',
'function fitScale(){var avail=viewport.clientWidth-16;if(natW<=0||avail<=0)return 1;return Math.min(1,avail/natW);}',
'function apply(s){s=Math.max(0.1,Math.min(5,s));content.style.zoom=s;var pct=Math.round(s*100);label.textContent=pct+"%";range.value=Math.max(parseInt(range.min,10),Math.min(parseInt(range.max,10),pct));}',
'function init(){measure();apply(fitScale());}',
'range.addEventListener("input",function(){apply(parseInt(range.value,10)/100);});',
'root.querySelector(\'[data-act="in"]\').addEventListener("click",function(){apply((parseInt(range.value,10)+10)/100);});',
'root.querySelector(\'[data-act="out"]\').addEventListener("click",function(){apply((parseInt(range.value,10)-10)/100);});',
'root.querySelector(\'[data-act="fit"]\').addEventListener("click",function(){measure();apply(fitScale());});',
'root.querySelector(\'[data-act="reset"]\').addEventListener("click",function(){apply(1);});',
'viewport.addEventListener("wheel",function(e){if(e.ctrlKey){e.preventDefault();var cur=parseInt(range.value,10)/100;apply(cur*(e.deltaY<0?1.1:0.9));}},{passive:false});',
'window.addEventListener("resize",function(){measure();});',
'if(document.readyState==="complete"||document.readyState==="interactive"){setTimeout(init,60);}else{window.addEventListener("load",init);}',
'})();</script>'
  )

  htmltools::browsable(
    htmltools::tagList(
      htmltools::HTML(head),
      ft,
      htmltools::HTML(tail)
    )
  )
}
