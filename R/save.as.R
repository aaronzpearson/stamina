#' Save As
#'
#' Save the output `data.frame`s as a .csv or .rds file
#'
#' @param data.set Data frame containing stamina data
#' @param file.name Intended file name
#'
#' @export
save.as.csv <- function (data.set, file.name)
{
  save.as <- paste0(file.name, ".csv")
  write.csv(data.set, save.as)
  print(paste(save.as, "was saved to", getwd()))
}

#' @export
#' @describeIn save.as.csv Alternate call for save.as.csv
save.as.excel <- function (data.set, file.name)
{
  save.as <- paste0(file.name, ".csv")
  write.csv(data.set, save.as)
  print(paste(save.as, "was saved to", getwd()))
}

#' @export
#' @describeIn save.as.csv Alternate call for save.as.rds
save.as.rdata <- function (data.set, file.name)
{
  save.as <- paste0(file.name, ".rds")
  saveRDS(data.set, save.as)
  print(paste(save.as, "was saved to", getwd()))
}

#' @export
#' @describeIn save.as.csv Alternate call for save.as.rdata
save.as.rds <- function (data.set, file.name)
{
  save.as <- paste0(file.name, ".rds")
  saveRDS(data.set, save.as)
  print(paste(save.as, "was saved to", getwd()))
}


