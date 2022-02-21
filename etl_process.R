library(app)

main <- function() {

  data_path <- Sys.getenv("DATA_PATH")
  data_file <- Sys.getenv("DATA_FILE")

  complete_path <- paste(data_path, data_file, sep = "/")

  # get number of sheets in excel file
  n_sheets <- loadWorkbook(complete_path) %>% getSheets %>% length

  dt_list <- lapply(
    1:n_sheets,
    function(sheetIndex) process(fetch_data(sheetIndex))
  )

  dt <- rbindlist(dt_list, use.names = T)

  save_dt(dt, path = data_path, file = Sys.getenv("PROCESSED_FILE"))

  print("Data process finishied!")

}

readRenviron(".Renviron")
main()

