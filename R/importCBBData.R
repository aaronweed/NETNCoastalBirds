#' Import NETN Coastal Bird data from a database export ZIP
#'
#' Imports a NETN Coastal Bird database export and returns analysis-ready
#' data frames corresponding to the objects returned by
#' [GetIncubationData()], [GetCrecheData()], [GetNestData()], and
#' [GetSurveyData()].
#'
#' @param zip_file Character. Path to the NETN Coastal Bird database
#'   export ZIP file.
#'
#' @return A named list with four data frames:
#' \describe{
#'   \item{incubation_raw}{Boat-based incubation survey data.}
#'   \item{creche_raw}{Common Eider creche survey data.}
#'   \item{nest_surveys_raw}{Ground-based nest survey data.}
#'   \item{seg_event}{Survey effort by species, island, segment, and
#'   survey type.}
#' }
#'
#' @details
#' This function provides an alternative to direct ODBC access through
#' the `Get*Data()` functions. It is intended for users who have a current
#' database export ZIP but do not have direct access to the NETN ODBC
#' connection.
#'
#' The export must contain `Events`, `Segments`, `Survey_Incubation`,
#' `Survey_Creche`, and `Survey_Nest` CSV tables. Date-stamped filenames
#' such as `Survey_Nest_20260828.csv` are supported.
#'
#' The `Survey_*` files are already joined database views. Filtering,
#' date handling, column selection, and ordering reproduce the
#' corresponding `Get*Data()` functions.
#'
#' @examples
#' \dontrun{
#' cbb <- importCBBData(
#'   "NETN_CoastalBirds_Dataset_20260828.zip"
#' )
#'
#' incubation_raw <- cbb$incubation_raw
#' creche_raw <- cbb$creche_raw
#' nest_surveys_raw <- cbb$nest_surveys_raw
#' seg_event <- cbb$seg_event
#' }
#'
#' @export
importCBBData <- function(zip_file) {
  
  if (missing(zip_file) || length(zip_file) != 1L ||
      !is.character(zip_file) || !file.exists(zip_file)) {
    stop(
      "Please supply the path to an existing NETN Coastal Bird ZIP export.",
      call. = FALSE
    )
  }
  
  required_tables <- c(
    "Events",
    "Segments",
    "Survey_Incubation",
    "Survey_Creche",
    "Survey_Nest"
  )
  
  zip_list <- utils::unzip(zip_file, list = TRUE)$Name
  
  csv_files <- basename(
    zip_list[grepl("\\.csv$", zip_list, ignore.case = TRUE)]
  )
  
  find_table <- function(table_name) {
    
    hits <- csv_files[
      grepl(
        paste0("^", table_name, "_[0-9]{8}\\.csv$"),
        csv_files,
        ignore.case = TRUE
      )
    ]
    
    if (length(hits) == 0L) {
      stop(
        "Required table not found in ZIP export: ",
        table_name,
        call. = FALSE
      )
    }
    
    if (length(hits) > 1L) {
      stop(
        "More than one dated version of ",
        table_name,
        " was found in the ZIP export.",
        call. = FALSE
      )
    }
    
    hits
  }
  
  table_files <- stats::setNames(
    lapply(required_tables, find_table),
    required_tables
  )
  
  tmp <- tempfile("NETNCoastalBirds_")
  dir.create(tmp)
  
  on.exit(
    unlink(tmp, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  
  utils::unzip(
    zip_file,
    files = unlist(table_files),
    exdir = tmp,
    junkpaths = TRUE
  )
  
  # Read CSV while handling both UTF-8 and legacy Windows-1252
  # characters encountered in the database export.
  read_cbb_csv <- function(file) {
    
    raw <- readBin(
      file,
      what = "raw",
      n = file.info(file)$size
    )
    
    txt <- rawToChar(raw)
    
    txt_utf8 <- iconv(
      txt,
      from = "UTF-8",
      to = "UTF-8",
      sub = NA_character_
    )
    
    if (is.na(txt_utf8)) {
      txt <- iconv(
        txt,
        from = "Windows-1252",
        to = "UTF-8",
        sub = "byte"
      )
    } else {
      txt <- txt_utf8
    }
    
    utils::read.csv(
      text = txt,
      stringsAsFactors = FALSE,
      na.strings = c("", "NA"),
      check.names = FALSE,
      strip.white = TRUE
    )
  }
  
  Events <- read_cbb_csv(
    file.path(tmp, table_files$Events)
  )
  
  Segments <- read_cbb_csv(
    file.path(tmp, table_files$Segments)
  )
  
  Survey_Incubation <- read_cbb_csv(
    file.path(tmp, table_files$Survey_Incubation)
  )
  
  Survey_Creche <- read_cbb_csv(
    file.path(tmp, table_files$Survey_Creche)
  )
  
  Survey_Nest <- read_cbb_csv(
    file.path(tmp, table_files$Survey_Nest)
  )
  
  # ================================================================
  # Incubation
  # Reproduce GetIncubationData()
  # ================================================================
  
  temp.incub <- Survey_Incubation[
    Survey_Incubation$Obs_Type %in% "Target",
    ,
    drop = FALSE
  ]
  
  temp.incub$Date <- lubridate::ymd(temp.incub$Date)
  
  temp.incub$year <- lubridate::year(
    temp.incub$Date
  )
  
  temp.incub$month <- lubridate::month(
    temp.incub$Date
  )
  
  # The export already contains HH:MM:SS.
  temp.incub$Start_Time <- substr(
    as.character(temp.incub$Start_Time),
    1L,
    8L
  )
  
  incubation_raw <- temp.incub |>
    dplyr::select(
      Park,
      Island,
      Segment,
      Survey_Class,
      Survey_Type,
      Survey_MultiPart,
      Survey_Duplicate,
      Survey_Primary,
      Survey_Complete,
      Date,
      year,
      month,
      Start_Time,
      Observer,
      Species_Code,
      Species_Unit,
      Unit_Count,
      Obs_Notes,
      Recorder,
      Data_Source,
      Wind_Direction,
      Wind_Speed,
      Air_Temp_F,
      Cloud_Perc,
      Tide_Stage
    ) |>
    dplyr::arrange(
      Island,
      Segment,
      Date,
      Species_Code,
      Observer
    )
  
  # ================================================================
  # Creche
  # Reproduce GetCrecheData()
  # ================================================================
  
  temp.crec2 <- Survey_Creche[
    Survey_Creche$Species_Code %in% "COEI" &
      Survey_Creche$Obs_Type %in% "Group",
    ,
    drop = FALSE
  ]
  
  temp.crec2$Date <- lubridate::ymd(
    temp.crec2$Date
  )
  
  temp.crec2$year <- lubridate::year(
    temp.crec2$Date
  )
  
  temp.crec2$month <- lubridate::month(
    temp.crec2$Date
  )
  
  temp.crec2$Start_Time <- substr(
    as.character(temp.crec2$Start_Time),
    1L,
    8L
  )
  
  temp.crec2$Group_Time <- substr(
    as.character(temp.crec2$Group_Time),
    1L,
    8L
  )
  
  # Group_NewTerritory is not included in the current export.
  # Add it so the output matches GetCrecheData().
  if (!"Group_NewTerritory" %in% names(temp.crec2)) {
    
    temp.crec2$Group_NewTerritory <- NA_character_
    
  }
  
  creche_raw <- temp.crec2 |>
    dplyr::select(
      Park,
      Island,
      Segment,
      Survey_Class,
      Survey_Type,
      Date,
      Start_Time,
      year,
      month,
      Survey_MultiPart,
      Survey_Duplicate,
      Survey_Primary,
      Survey_Complete,
      Recorder,
      Observer,
      Species_Code,
      Group_Count,
      Group_Time,
      Group_NewTerritory,
      Group_Notes,
      Latitude,
      Longitude,
      Species_Unit,
      Unit_Count,
      Survey_Notes,
      Wind_Direction,
      Wind_Speed,
      Air_Temp_F,
      Cloud_Perc,
      Tide_Stage
    ) |>
    dplyr::filter(
      !is.na(Unit_Count)
    )
  
  # ================================================================
  # Nest surveys
  # Reproduce GetNestData()
  # ================================================================
  
  temp.nest <- Survey_Nest[
    Survey_Nest$Obs_Type %in% "Target" &
      Survey_Nest$c_TargetSpp_Group != "AMOY",
    ,
    drop = FALSE
  ]
  
  temp.nest$Date <- lubridate::ymd(
    temp.nest$Date
  )
  
  temp.nest$year <- lubridate::year(
    temp.nest$Date
  )
  
  temp.nest$month <- lubridate::month(
    temp.nest$Date
  )
  
  temp.nest$Start_Time <- substr(
    as.character(temp.nest$Start_Time),
    1L,
    8L
  )
  
  temp.nest$Unit_Count[
    temp.nest$Unit_Count == 999
  ] <- NA
  
  temp.nest$Egg_Count[
    temp.nest$Egg_Count == 999
  ] <- NA
  
  temp.nest$Chick_Count[
    temp.nest$Chick_Count == 999
  ] <- NA
  
  # Match the current ODBC branch of GetNestData().
  nest_surveys_raw <- temp.nest |>
    dplyr::select(
      Park,
      Island,
      Segment,
      Survey_Class,
      Survey_Type,
      Survey_MultiPart,
      Survey_Duplicate,
      Survey_Primary,
      Survey_Complete,
      Date,
      year,
      month,
      Start_Time,
      Species_Code,
      Species_Unit,
      Nest_Status,
      Unit_Count,
      Egg_Count,
      Chick_Count,
      Latitude,
      Longitude,
      Obs_Notes,
      Survey_Notes,
      Wind_Direction,
      Wind_Speed,
      Air_Temp_F,
      Cloud_Perc,
      Tide_Stage
    )
  
  # ================================================================
  # Survey effort
  # Reproduce GetSurveyData() output structure
  # ================================================================
  
  # The new export does not include the historical
  # tbl_Survey_Segments_Size or tlu_Species tables.
  #
  # SurveyEffortBySpecies is therefore used as the package-level
  # source for segment effort and species mappings.
  
  segment_keys <- Segments |>
    dplyr::transmute(
      Island = Site,
      Segment,
      Survey_Class,
      Survey_Type
    ) |>
    dplyr::distinct()
  
  Spec_Isl <- SurveyEffortBySpecies |>
    dplyr::inner_join(
      segment_keys,
      by = c(
        "Island",
        "Segment",
        "Survey_Class",
        "Survey_Type"
      )
    ) |>
    dplyr::distinct()
  
  All_Isl <- Spec_Isl |>
    dplyr::group_by(
      Species_Code,
      Survey_Class,
      Survey_Type
    ) |>
    dplyr::summarise(
      Survey_Size = sum(Survey_Size),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Size_Units = ifelse(
        Survey_Type %in% "Creche",
        "m",
        "m2"
      ),
      Island = "All Islands",
      Segment = "All"
    )
  
  seg_event <- dplyr::bind_rows(
    Spec_Isl,
    All_Isl
  ) |>
    dplyr::select(
      Species_Code,
      Island,
      Segment,
      Survey_Class,
      Survey_Type,
      Survey_Size,
      Size_Units
    ) |>
    dplyr::distinct()
  
  # ================================================================
  # Return analysis-ready objects
  # ================================================================
  
  list(
    incubation_raw = incubation_raw,
    creche_raw = creche_raw,
    nest_surveys_raw = nest_surveys_raw,
    seg_event = seg_event
  )
}