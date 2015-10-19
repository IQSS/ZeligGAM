#' @include model-gam.R
#' @include model-normal-gam.R

library(jsonlite)

z5normalgam <- znormalgam$new()
z5normalgam$toJSON()

zeliggammodels <- list(zelig5gammodels = list("normal.gam" = z5normalgam$ljson))

cat(jsonlite::toJSON(zeliggammodels, pretty = TRUE),
    file = file.path("inst/JSON", "zelig5gammodels.json"))
