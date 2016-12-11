#' Read a file
#'
#' Read a csv file and convert it into a tibble
#'
#' @note if file did not exit, the prgram will exit with an error
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @param filename A character string giving the name of the file
#'
#' @return a tibble
#'
#' @examples
#'
#' fars_read("accident_2015.csv.bz2")
#'
#'
#' @export
fars_read <- function(filename) {
  filename <- system.file("extdata", filename, package="fars")
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Make a file name
#'
#' Convert the year into a the name of the file which which contain
#' the accident information in this year
#'
#' @param year numeric the name of the year
#'
#' @return a character which is the file name
#'
#' @examples
#' make_filename(2015)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read a list of files
#'
#' Read the data in multiple years and only select the MONTH and year column
#'
#' @note if year is invalid, the program will exit silently with a warning
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @param years a vector of the name of multiple years
#'
#' @return a list of tibble
#'
#' @examples
#' fars_read_years(c(2013,2014,2015))
#'
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize the data of each year
#'
#' Summarize the number of accident in each year
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @inheritParams fars_read_years
#'
#' @return a tibble of the summarized data
#'
#' @examples
#' fars_summarize_years(c(2013,2014,2015))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot the accidence location in a map
#'
#' Plot a the accident location in a specified state and year in the map.
#' if the LONGITUD > 900 or LATITUDE > 90, the location will be ignored.
#'
#' @note if state.num is invalid, the program will exit with an error
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @param state.num the number of the state
#' @param year the name of the year
#'
#'
#' @return a map graph with the accidence location
#'
#' @examples
#' \dontrun{
#' fars_map_state(1,2015)
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
