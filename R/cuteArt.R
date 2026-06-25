#' Print a cute ASCII art
#'
#' @description
#' Print a cute ASCII art to the console. Just for fun / testing.
#'
#' @param animal which art to print, one of "cat", "bear", "rabbit", "dog",
#'   "panda". Default "cat".
#' @return invisibly returns the art as a character string.
#' @export
#' @examples
#' cuteArt()
#' cuteArt("bear")
#' cuteArt("panda")
cuteArt <- function(animal = c("cat", "bear", "rabbit", "dog", "panda")){
  animal <- match.arg(animal)
  art <- switch(
    animal,
    cat = "
   /\\_/\\
  ( o.o )
   > ^ <
  meow~
",
    bear = "
  ( ˘ ω ˘ )
   /つ つ\\
  hug~
",
    rabbit = "
  (\\(\\
  ( -.-)
  o_(\")(\")
  hop~
",
    dog = "
   / \\__
  (    @\\___
  /         O
 /   (_____/
/_____/   U
  woof~
",
    panda = "
  ( •ω• )
  /づ🎋
  panda~
"
  )
  cat(art)
  invisible(art)
}
