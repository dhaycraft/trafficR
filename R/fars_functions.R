#' Read FARS data set file from CSV
#'
#' Reads in a CSV from a file location as tbl_df data for FARS.
#' @param filename file path as character string.
#' @return a tbl_df of the selected file if is csv if it exists
#' @examples
#' fars_read("accident_2013.csv.bz2")
#' @note Warning: Function will not work if not given valid file name.
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Create File Name FARS Dataset
#'
#' Takes either a number or a string that is the year with no spaces to create a file name for a FARS CSV to read in the data
#' @param year string or numeric that indicates the year of the FARS data that the file name will be created to read.
#' @return a string that is the file name to be used in fars_read for the input year
#' @examples
#' make_filename(2013)
#' @note Warning: Will not work if year is not 2013, 2014, or 2015 as those are the only years with data files
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Reads in FARS Data for multiple years
#'
#' Reads in a list of years and extracts the year and month for each row from the files for each year associated
#' @param years string or numeric that indicates the year of the FARS data that the file name will be created to read.
#' @return list of tbl_dfs with month and year columns
#' @examples
#' make_filename(2013)
#' @note Warning: Will not work if years are not 2013, 2014, or 2015 as those are the only years with data files
#'  @importFrom dplyr select mutate
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


#' Summarises FARS data for multiple years
#'
#' Reads in data for each year and summarises the counts of events by month and year.
#' @param years string or numeric that indicates the years to be summarised.
#' @return tbl_df with columns being the years summarised and rows being the months and each entry being the number of events
#' @examples
#' fars_summarize_years(2013:2014)
#' @note Warning: Will not work if years are not in 2013, 2014, or 2015 as those are the only years with data files
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Map events FARS data
#'
#' Reads in data for a particular year and maps the events in a given state specified by state number
#' @param state.num a number indicating the state can be any number from 1 through 56.
#' @param year the year for the accident events
#' @return a plot of the state with points for each accident
#' @examples
#' #Plot California accident patterns
#' fars_map_state(state.num=6, year=2013)
#' @note Warning: This function appears to be slightly buggy not returning all events on first plotting
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
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
