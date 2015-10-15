#' @include model-semAR.R

library(jsonlite)

z5gam <- zgam$new()
z5gam$toJSON()

zeliggammodels <- list(zelig5semmodels = list("gam" = z5gam$ljson))

cat(jsonlite::toJSON(zeliggammodels, pretty = TRUE),
    file = file.path("inst/JSON", "zelig5gammodels.json"))
