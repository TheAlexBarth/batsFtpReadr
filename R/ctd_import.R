#' Read QC ctd files
#'
#' Reads a single ctd file off the bats ftp
#' check out the file information here.
#'
#' @param code the cruise code/number
#' @param proj the project acronym 'bats', 'bval', 'bloom', 'gf', 'hs'
#' @export
read_bats_ctd <- function(code, proj) {
  cur_files <- list_qc_ctd(proj)

  if(length(proj) > 1) {
    stop('Sorry, I made this to only work one project at a time')
  }

  if(length(code) > 1) {
    paths <- lapply(code, get_qc_path,
                    proj = proj, cur_files = cur_files)
    ctd_data_list <- lapply(paths, ftp_ctd_import)
    names(ctd_data_list) <- paste0(proj,code)
    return(ctd_data_list)
  } else {
    ctd_data <- get_qc_path(code, proj, cur_files) |>
      ftp_ctd_import()
    return(ctd_data)
  }

}


#' Get single file imported
#'
#' Works strictly for qc casts paths
#' @param path the path to the txt file (ASCII)
#' @importFrom lubridate date_decimal
#' @importFrom utils read.table
ftp_ctd_import <- function(path) {
  col_names <- c('ctd_id', 'Date', 'Lat', "Lon", 'dbar', 'Depth',
                 'Temperature', 'Conductivity', 'Sal', 'DO', 'BAC',
                 'RFU', 'PAR')

  temp_read <- read.table(path, sep = '', col.names = col_names)

  ####
  # formatting output
  ####

  #get all -999 to NA
  na999 = function(x) {
    x[which(x == -999)] <- NA
    return(x)
  }
  rdf <- as.data.frame(lapply(temp_read, na999))

  # format date
  rdf$Date <- date_decimal(rdf$Date, tz = 'GMT')

  # format cruise_id
  rdf$cruise_id <- strsplit(as.character(rdf$ctd_id), "") |>
    lapply(function(x) x[2:5]) |>
    lapply(paste0, collapse = "") |>
    as.integer()

  rdf$cast_id <- strsplit(as.character(rdf$ctd_id), "") |>
    lapply(function(x) x[6:8]) |>
    lapply(paste0, collapse = "") |>
    as.integer()

  proj_code <- strsplit(as.character(rdf$ctd_id), "") |>
    lapply(function(x) x[1]) |>
    as.integer()

  rdf$proj_id <- proj_code |>
    sapply(function(x) x |>
             as.character(x) |>
             switch('1' = 'gf',
                    '2' = 'bla',
                    '3' = 'blb',
                    '5' = 'bv',
                    '6' = 'hs',
                    stop('ctd_id does not match projects')))

  return(rdf)
}



#' Get path name to ctd file
#'
#' @param code the cruise number
#' @param proj the project name
#' @param cur_files the current file list
get_qc_path <- function(code, proj, cur_files = list_qc_ctd(proj)) {

  #safety check
  if(!(proj %in% c('bats','bv','bval','gf','hs','bloom'))) {
    stop("Must provide one of the following:
         'bats','bv','bval','gf','hs','bloom'")
  }

  # find the file name
  if(proj != 'hs') {
    if(proj %in% c('bats','gf')) {
      prefix_num <- 1
    } else if (proj %in% c('bv','bval')) {
      prefix_num <- 5
    } else {
      prefix_num <- 2
    }
    proj_files <- cur_files[grep(paste0('^b',prefix_num), cur_files)]
    proj_name <- proj_files[grep(paste0(code,"_ctd"), proj_files)]

    if(length(proj_name) == 0) {
      stop(paste0('There was no cast found for project: ',proj,
                  " code: ", code))
    }

    path <- paste0('http://batsftp.bios.edu/BATS/ctd/ASCII/',proj_name)
  } else {
    cur_files <- list_qc_ctd(proj)
    prefix_num <- 6

    proj_files <- cur_files[grep(paste0('^b',prefix_num), cur_files)]
    proj_name <- proj_files[grep(paste0(code,"_ctd"), proj_files)]

    if(length(proj_name) == 0) {
      stop(paste0('There was no cast found for project: ',proj,
                  " code: ", code))
    }

    path <- paste0('http://batsftp.bios.edu/Hydrostation_S/ctd/ASCII/',
                   proj_name)
  }
  return(path)
}

#' Read list of available CTD files
#'
#' Gets the names of the QC ctd files for either bats or hydrostation
#'
#' @param proj either 'bats', 'gf', 'bv', 'bloom' or 'hs'
#' @import rvest
list_qc_ctd <- function(proj) {

  if (proj != 'hs') {
    table_link <- 'http://batsftp.bios.edu/BATS/ctd/ASCII/'

  } else {
    table_link <- 'http://batsftp.bios.edu/Hydrostation_S/ctd/ASCII/'
  }

  t_table <- read_html(table_link) |>
    html_nodes('table') |>
    html_table()

  available_casts <- t_table[[1]]$Name[-c(1:3)]
  return(available_casts)
}
