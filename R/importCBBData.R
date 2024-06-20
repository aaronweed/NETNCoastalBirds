#' @title importCBBData Import NETN Coastal Bird data that are formatted as .xlxs files.
#'
#' @importFrom  readxl read_excel
#'
#' @description This function imports all CSVs exported from NETN's Coastal Bird Database. 
#'
#' @param path Quoted path of folder containing tables.
#'
#' @param new_env Logical. Specifies which environment to store views in. If \code{TRUE}(Default), stores
#' views in CBB_TABLES environment. If \code{FALSE}, stores views in global environment
#' 
#' @param zip_name Quoted string ending in .zip. If specified, function looks for the specified file name and
#' imports .csvs from the zip file. If not specified, function looks for and imports individual csvs. Note that
#' this takes slightly longer than loading individual .csvs, due to the unzipping process.
#'
#' @return NETN database views in specified environment
#'
#' @examples
#' \dontrun{
#' # Import individual xlsxs into global environment
#' importCBBData(path = "C:/Coastal_Birds/exports/NETN")
#'
#' # Import zipped csvs into global environment
#' path <- "C:/Coastal_Birds/exports/NETN"
#' importCBBData(path = path, zip_name = "CBB_Dataset_Export_20230119.zip")
#' }
#' 
#' @export

importCBBData<- function(path = NA, new_env = TRUE, zip_name = NA){
  
  # Error handling for path
  
  if(is.na(path)){stop("Must specify a path to import files.")
  } else if(!dir.exists(path)){stop("Specified path does not exist.")}
  
  # Add / to end of path if it wasn't specified.
  path <- if(substr(path, nchar(path), nchar(path)) != "/"){paste0(path, "/")} else {(paste0(path))}
  
  
  view_list <- c("qry_Dataset_1_Events_All","qry_Dataset_2_Survey_Incubation", "qry_Dataset_3_Survey_Nest",
                 "qry_Dataset_4_Survey_Creche", "qry_Dataset_5_Survey_AMOY", "qry_Dataset_6_Summary_AMOY_PIPL",
                 "qry_Dataset_7_Summary_Terns", "qry_Dataset_8_Survey_Surveillance","qry_Dataset_9_Incidental_Observations")
  
  # Make sure zip file exists and all the views are included
  if(!is.na(zip_name)){
    if(!file.exists(paste0(path, zip_name))){stop("Specified zip file doesn't exist in path.")}}
  
  
  # Make sure all the views are in the path or zip file. If anything is missing, function stops.
  files <-
    if(!is.na(zip_name)){
      zfiles <- unzip(paste0(path, zip_name), list = TRUE)$Name
      files <- substr(zfiles, 1, nchar(zfiles) - 5)
    } else if(is.na(zip_name)) {
      files <- substr(list.files(path), 1, nchar(list.files(path)) - 5)}
  
  missing <- setdiff(view_list, files)
  
  if(length(missing) > 0 & length(missing) < length(view_list)){
    stop(paste0("Missing the following views: ", paste0(missing, collapse = ", ")))
  } else if (length(missing) == length(view_list)){
    stop(paste0("Views were not detected in specified ", ifelse(is.na(zip_name), "path.", "zip file.")))}
  
  # Since the missing test passed, clean up files so only includes names in view_list, but
  # maintain order in files
  files <- intersect(files, view_list)
  
  # Import views now that all tests passed
  pb <- txtProgressBar(min = 0, max = length(view_list), style = 3)
  
  view_import <-
    if(!is.na(zip_name)){
      views <- unzip(paste0(path, zip_name), junkpaths = TRUE, exdir = tempdir())
      lapply(seq_along(view_list), function(x){
        setTxtProgressBar(pb,x)
        read_excel(views[x])})
    } else if(is.na(zip_name)){
      lapply(seq_along(view_list), function(x){
        setTxtProgressBar(pb, x)
        read_excel(paste0(path, view_list[x], ".csv"))
      })
    }
  
  view_import <- setNames(view_import, files)
  
  if(new_env == TRUE){
    CBB_TABLES <<- new.env()
    list2env(view_import, envir = CBB_TABLES)
  } else {
    list2env(view_import, envir = .GlobalEnv)}
  
    close(pb)
  
  print(ifelse(new_env == TRUE, paste0("Import complete. Views are located in CBB_TABLES environment."),
               paste0("Import complete. Views are located in global environment.")), quote = FALSE)
  
}

  
  
  
  