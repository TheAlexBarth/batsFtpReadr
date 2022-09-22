#' Read QC ctd files
#'
#' Reads a single ctd file off the bats ftp
#' check out the file information here
#'
#' @param code the cruise code/number
#' @param proj the project acronym 'bats', 'bval', 'bloom', 'gf', 'hs'
read_bats_ctd <- function(code, proj) {
  require(lubridate)

  #safety check
  if(!(proj %in% c('bats','bv','bval','gf','hs','bloom'))) {
    stop("Must provide one of the following:
         'bats','bv','bval','gf','hs','bloom'")
  }

  # find the file name
  if(proj != 'hs') {
    cur_files <- list_qc_ctd(proj)
    if(proj %in% c('bats','gf')) {
      prefix_num <- 1
    } else if (proj %in% c('bv','bval')) {
      prefix_num <- 5
    } else {
      prefix_num <- 2
    }
    proj_files <- cur_files[grep(paste0('^b',prefix_num), cur_files)]
    proj_name <- proj_files[grep(paste0(code,"_ctd"), proj_files)]

    if(length(cast_name) == 0) {
      stop(paste0('There was no cast found for project: ',proj,
                  " code: ", code))
    }

    path <- paste0('http://batsftp.bios.edu/BATS/ctd/ASCII/',proj_name)
  } else {
    cur_files <- list_qc_ctd(proj)
    prefix_num <- 6

    proj_files <- cur_files[grep(paste0('^b',prefix_num), cur_files)]
    proj_name <- proj_files[grep(paste0(code,"_ctd"), proj_files)]

    if(length(cast_name) == 0) {
      stop(paste0('There was no cast found for project: ',proj,
                  " code: ", code))
    }

    path <- paste0('http://batsftp.bios.edu/Hydrostation_S/ctd/ASCII/',
                   proj_name)
  }

  # Read in the dataframe
  col_names <- c("cast_id",'Date', )

  temp_read <- read.table(path, sep = '\t')
}


#' Read QC meta files
#'
#'


#' Read list of available CTD files
#'
#' Gets the names of the QC ctd files for either bats or hydrostation
#'
#' @param proj either 'bats', 'gf', 'bv', 'bloom' or 'hs'
list_qc_ctd <- function(proj) {
  require(rvest)

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
