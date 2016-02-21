#' @include model-gam.R
#' @include model-normal-gam.R

# library(jsonlite)

createJSONzeliggam <- function(){
  
  z5normalgam <- znormalgam$new()
  z5normalgam$toJSON()
  
  z5logitgam <- zlogitgam$new()
  z5logitgam$toJSON()
  
  z5probitgam <- zprobitgam$new()
  z5probitgam$toJSON()
  
  z5poissongam <- zpoissongam$new()
  z5poissongam$toJSON()
  
  zeliggammodels <- list(zelig5gammodels = list("normalgam" = z5normalgam$ljson,
                                                "logitgam" = z5logitgam$ljson,
                                                "probitgam" = z5probitgam$ljson,
                                                "poissongam" = z5poissongam$ljson))
  
  # cat(jsonlite::toJSON(zeligchoicemodels, pretty = TRUE),
  #     file = file.path("inst/JSON", "zelig5choicemodels.json"))
  
  cat(toJSON(zeliggammodels, pretty = TRUE), file = file.path("zelig5gammodels.json"))
  file.rename(from = file.path("zelig5gammodels.json"),
              to = file.path("inst", "JSON", "zelig5gammodels.json"))
  file.remove(file.path("zelig5gammodels.json"))
  
  return(TRUE)
}

# createJSONzeliggam()
