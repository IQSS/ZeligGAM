#' @include model-gam.R
#' @include model-normal-gam.R

library(jsonlite)

z5normalgam <- znormalgam$new()
z5normalgam$toJSON()

zeligmodels <- list(zelig5models = list("normalgam" = z5normalgam$ljson))

# cat(toJSON(zeligmodels, pretty = TRUE), file = file.path("tools", "zelig5models.json"))
# file.copy(from = file.path("tools", "zelig5models.json"), to = file.path("inst", "JSON", "zelig5models.json"))

cat(toJSON(zeligmodels, pretty = TRUE), "\n", file = file.path("zelig5models.json"))
file.rename(from = file.path("zelig5models.json"), to = file.path("inst", "JSON", "zelig5models.json"))
file.remove(file.path("zelig5models.json"))

# cat(toJSON(zeligmodels, pretty = TRUE))
# j <- jsonlite::fromJSON(txt = readLines(file.path("..", "/JSON", "/zelig5models.json")))
