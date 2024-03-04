# renv::install(paste0("agdamsbo/",c("REDCapCAST")))

################################################################################
########
########      TALOS
########
################################################################################

#' Imports specified TALOS data
#'
#' @param token api token
#' @param vars variable names to retrieve (fields in REDCap lingo)
#'
#' @return tibble of REDCap exported data
#' @examples
#' data <- import_talos()
#' skimr::skim(data)
import_talos <- function(key = "TALOS_REDCAP_API",
                         vars = c("record_id", "inkl_rnumb", "cpr", "talos_inkl03x", "talos_basis02a", "talos_basis02b", "basis_sys_site")) {
  REDCapR::redcap_read(
    redcap_uri = "https://redcap.au.dk/api/", token = keyring::key_get(key),
    fields = vars
  )$data
}

#' Filter TALOS by site
#'
#' @param data
#' @param site default is "10015", which is Aarhus
#'
#' @return
#' @export
#'
#' @examples
#' aarhus <- import_talos() |> filter_talos_site(site = "10015")
filter_talos_site <- function(data, site = "10015") {
  if (is.null(site)) {
    data
  } else {
    data |> dplyr::filter(basis_sys_site %in% site)
  }
}

#' Quick number of subjects in project
#'
#' @param key API key name for ekyring key retrieval
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' redcap_get_n()
redcap_get_n <- function(key = "SVD_REDCAP_API") {
  REDCapR::redcap_read_oneshot(
    redcap_uri = "https://redcap.au.dk/api/",
    token = keyring::key_get(key),
    fields = "record_id"
  ) |>
    purrr::pluck("data") |>
    nrow()
}

#' Modifies exported talos data to new database format
#'
#' @param data tibbled REDCap data
#' @param trial source trial name. TALOS or RESIST
#' @param key destination project API key name
#'
#' @return tibble with modified data
#' @examples
#' ds |> modify_data_talos(index = 0, trial = "TALOS")
modify_data_talos <- function(data,
                              index = redcap_get_n(),
                              trial,
                              id.var = inkl_rnumb,
                              cpr.var = cpr,
                              name.var = talos_inkl03x,
                              date.var = talos_basis02a,
                              time.var = talos_basis02b) {
  ids <- seq_len(nrow(data)) + (index)

  data |>
    dplyr::mutate(ids = ids) |>
    dplyr::transmute(
      record_id = glue::glue("svd_{ids}"),
      trial_id = {{ id.var }},
      cpr = {{ cpr.var }},
      name = {{ name.var }},
      index_date = format(as.POSIXct({{ date.var }}), "%Y-%m-%d"),
      index_time = format(as.POSIXct({{ time.var }}), "%H:%M"),
      trial_name = trial,
      basis_complete = 2
    )
}

################################################################################
########
########      RESIST
########
################################################################################

#' Imports specified data
#'
#' @param token api token
#' @param vars variable names to retrieve (fields in REDCap lingo)
#'
#' @return tibble of REDCap exported data
#' @examples
#' data <- import_resist()
#' skimr::skim(data)
import_resist <- function(key = "RESIST-MIGRATION", keyring = "REDCAP_APIs", service = "redcapAPI",
                          vars = c("resistid", "cpr", "navn", "scan_dato_tid", "afdelingid", "target_population")) {
  REDCapR::redcap_read(
    redcap_uri = "https://redcap.au.dk/api/", token = keyring::key_get(service = service, username = key, keyring = keyring),
    fields = vars
  )$data
}

#' Filter RESIST by site and target and scan
#'
#' @param data
#' @param site default is "10015", which is Aarhus
#'
#' @return
#' @export
#'
#' @examples
#' resist_aarhus <- import_redcap(
#'   key = "RESIST-MIGRATION", keyring = "REDCAP_APIs", service = "redcapAPI",
#'   vars = c("resistid", "cpr", "navn", "scan_dato_tid", "afdelingid", "target_population")
#' ) |>
#'   filter_resist(site = 1, target = 1, datetime.var = "scan_dato_tid")
filter_resist <- function(data, site, target, datetime.var) {
  data |>
    dplyr::filter(afdelingid %in% site) |>
    dplyr::filter(target_population %in% target) |>
    dplyr::filter(!is.na(datetime.var))
}

#' Modifies exported RESIST data to new database format
#'
#' @param data tibbled REDCap data
#' @param trial source trial name. TALOS or RESIST
#' @param key destination project API key name
#'
#' @return tibble with modified data
#' @examples
#' resist_aarhus |> modify_data_resist(trial = "RESIST", id.var = resistid, datetime.var = scan_dato_tid, name.var = navn)
modify_data_resist <- function(data,
                               index = redcap_get_n(),
                               trial,
                               id.var = inkl_rnumb,
                               cpr.var = cpr,
                               datetime.var,
                               name.var = talos_inkl03x) {
  ids <- seq_len(nrow(data)) + (index)

  data |>
    dplyr::mutate(ids = ids) |>
    dplyr::transmute(
      record_id = glue::glue("svd_{ids}"),
      trial_id = {{ id.var }},
      cpr = {{ cpr.var }},
      name = {{ name.var }},
      index_date = strftime({{ datetime.var }}, "%Y-%m-%d"),
      index_time = strftime({{ datetime.var }}, "%H:%M"),
      trial_name = trial,
      basis_complete = 2
    )
}

################################################################################
########
########      UPLOAD
########
################################################################################

#' Write data to REDCap db
#'
#' @param key project key set in `keyring`
#' @param data data to write
#'
#' @return
#' @examples
#' ds |>
#'   filter_talos_site() |>
#'   modify_data() |>
#'   write2db()
#' resist_ds <- import_resist() |>
#'   filter_resist() |>
#'   modify_data_resist()
#' resist_ds |> write2db()
write2db <- function(data, key = "SVD_REDCAP_API") {
  REDCapR::redcap_write(
    redcap_uri = "https://redcap.au.dk/api/",
    token = keyring::key_get(key),
    ds_to_write = data
  )
}

################################################################################
########
########      SVD data management and analyses
########
################################################################################


## Inter-rater-reliability

# https://stackoverflow.com/questions/71587719/inter-rater-reliability-by-groups
# http://www.cookbook-r.com/Statistical_analysis/Inter-rater_reliability/
#
#   https://joon-e.github.io/tidycomm/reference/test_icr.html


#' Sample data for inter-rater-reliability testing
#'
#' @return data.frame
#' @export
#' @examples
#' irr_sample()
irr_sample <- function() {
  set.seed(8)
  paste0("svd_", sample(1:5, 5, replace = FALSE)) |>
    lapply(\(x){
      rbind(
        c(x, "A", sample(1:0, 20, replace = TRUE)),
        c(x, "B", sample(1:0, 20, replace = TRUE))
      ) |>
        as.data.frame() |>
        setNames(c("record_id", "svd_user", paste0("item", 1:20)))
    }) |>
    purrr::list_rbind()
}

#' Cleans ID numbers and arranges by increasing value
#'
#' @param data data set
#' @param remove string to remove
#'
#' @return
#' @export
#'
#' @examples
#' irr_sample() |> clean_record_id()
arrange_record_id <- function(data, remove = "svd_") {
  data |>
    dplyr::arrange(as.numeric(stringr::str_remove(record_id, remove)))
  ## Improve by removing everything not being a number?
}

#' Read single REDCap instrument
#'
#' @param key API key
#' @param instrument instrument name
#'
#' @return data.frame
#' @export
#'
#' @examples
#' data <- read_instrument(key = "SVD_REDCAP_API", instrument = "svd_score")
#' basis_ds <- read_instrument(key = "SVD_REDCAP_API", instrument = "basis")
#' basis_ds |>
#'   arrange_record_id() |>
#'   head(100)
read_instrument <- function(key = "SVD_REDCAP_API", instrument = "svd_score", raw_label = "raw") {
  REDCapCAST::read_redcap_tables(
    uri = "https://redcap.au.dk/api/", token = keyring::key_get(key),
    fields = "record_id",
    forms = instrument, raw_or_label = raw_label
  )[[instrument]]
}


#' Clean SVD data by filtering on repeated and on any marking as data not present
#'
#' @param data data set
#'
#' @return tibble
#' @export
#'
svd_score_clean <- function(data) {
  # Filtering non-performed annotations out
  filtered <- split(data, data$record_id)[!split(data, data$record_id) |>
    purrr::map(\(x){
      any(x$svd_perf == 2)
    }) |>
    purrr::list_c()] |> dplyr::bind_rows()

  # Keeps the first instance in case of the same user having filled out more than one
  split(filtered, filtered$record_id) |>
    purrr::map(\(x){
      x[!duplicated(x$svd_user), ]
    }) |>
    dplyr::bind_rows() |>
    arrange_record_id()
}


#' Filtering out users, that did not complete all annotations
#'
#' @param data data
#'
#' @return data.frame or tibble
#' @export

filter_incomplete_users <- function(data) {
  data |> (\(x){
    nall <- x |>
      dplyr::group_by(svd_user) |>
      dplyr::count() |>
      dplyr::ungroup() |>
      dplyr::filter(n == max(n)) |>
      dplyr::select(svd_user)

    x |> dplyr::filter(svd_user %in% nall[[1]])
  })()
}

#' Cleaning data for IRR calculation
#'
#' @param key
#' @param instrument
#'
#' @return
#' @export
#'
#' @examples
#' read_instrument() |> inter_rater_data()
inter_rater_data <- function(data) {
  data |>
    arrange_record_id() |>
    dplyr::select(tidyselect::all_of(
      c("record_id",
      "svd_user",
      "svd_quality",
      "svd_microbleed",
      "svd_microbleed_location___1",
      "svd_microbleed_location___2",
      "svd_microbleed_location___3",
      "svd_siderose",
      "svd_lacunes",
      "svd_wmh",
      "svd_atrophy"
    )))
}


#' Simplified SVD score 0-4
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
#' data <- read_instrument() |> inter_rater_data()
#' data |> simple_score()
simple_score <- function(data) {
  data |>
    arrange_record_id() |>
    dplyr::transmute(record_id, svd_user,
      microbleed = dplyr::if_else(svd_microbleed < 1, 0, 1),
      lacunes = dplyr::if_else(svd_lacunes < 1, 0, 1),
      wmh = dplyr::if_else(svd_wmh < 2, 0, 1),
      atrophy = dplyr::if_else(svd_atrophy < 2, 0, 1),
      score = microbleed + lacunes + wmh + atrophy
    )
}


#' Inter rater reliability calculations
#'
#' @param data data
#'
#' @return
#' @export tibble
#'
#' @examples
#' irr_sample |> inter_rater_calc()
#' data <- read_instrument() |>
#'   inter_rater_data()
#' data |> inter_rater_calc()
inter_rater_calc <- function(data) {
  data |> tidycomm::test_icr(
    unit_var = record_id,
    coder_var = svd_user,
    holsti = FALSE,
    fleiss_kappa = TRUE,
    brennan_prediger = TRUE,
    na.omit = TRUE
  )
}

#' ICC calculations
#'
#' @param data minimal dataset with only relevant variables
#' @param unit_var subject var
#' @param coder_var rater var
#'
#' @return
#' @export
#'
#' @examples
#' # ds_simple |> icc_multi(unit_var=record_id, coder_var=svd_user)
icc_multi <- function(data, unit_var = record_id, coder_var = svd_user) {
  # The function to calculate ICC
  icc_calc <- function(data) {
    irr::icc(data, model = "twoway", type = "agreement", unit = "single") |>
      purrr::pluck("value")
  }

  # Names of provided variables
  suppressWarnings(nms <- data |>
    dplyr::select(-{{ unit_var }}, -{{ coder_var }}) |>
    names())

  # ICC calculation for each variable
  nms |>
    lapply(function(.x) {
      tidycomm:::unit_coder_matrix(data,
        unit_var = {{ unit_var }},
        coder_var = {{ coder_var }},
        test_var = .x
      )
    }) |>
    purrr::map(icc_calc) |>
    purrr::list_c() |>
    (\(.y){
      tibble::tibble(
        Variable = nms,
        IntraclCorrCoef = .y
      )
    })()
}


#' Join IRR and ICC calculations
#'
#' @param data data
#'
#' @return tibble
#' @export
irr_icc_calc <- function(data) {
  dplyr::left_join(inter_rater_calc(data), icc_multi(data)) |>
    dplyr::select(-tidyselect::all_of(c("n_Coders", "n_Categories", "Level", "n_Units")))
}


#' Upload assessor allocation
#'
#' @param path allocation table path
#'
#' @return
#' @export
#'
#' @examples
#' allocate_assessors() |> write2db()
allocate_assessors <- function(path = "data/allocation.csv") {
  ds <- read.csv(here::here(path)) |> na.omit()

  seq_len(nrow(ds)) |>
    lapply(function(x) {
      ds[x, ] |>
        dplyr::tibble(
          record_id = paste0("svd_", seq(start, stop)),
          allocated_assessor = assessor
        ) |>
        dplyr::select(tidyselect::all_of(c(
          "record_id",
          "allocated_assessor"
        )))
    }) |>
    dplyr::bind_rows()
}

#' Cuts hms data into intervals
#'
#' @param data data
#' @param breaks specified breaks. Character vector
#' @param labels desired labels. Breaks are used if NULL.
#'
#' @return
#' @export
#'
time_cutter <- function(data,
                        breaks = c("00:00:00", "12:00:00", "23:59:00"),
                        labels = c("AM", "PM")) {
  if (!"hms" %in% class(data)) stop("Data has to be of class 'hms'")
  if (length(breaks) - 1 != length(labels) | is.null(labels)) {
    message("Generic labels are used")
    labels <- seq_len(length(breaks) - 1) |>
      lapply(\(x){
        glue::glue("[{substr(breaks,1,5)[x]}-{substr(breaks,1,5)[x+1]}]")
      }) |>
      purrr::list_c()
  }

  cut(lubridate::ymd_hms(paste(Sys.Date(), data)),
    breaks = lubridate::ymd_hms(paste(Sys.Date(), breaks)),
    labels = labels
  )
}

#' Identify subjects with missing assesment
#'
#' @param data data
#'
#' @return
#' @export
#'
who_is_missing <- function(data) {
  data |>
    dplyr::group_by(record_id) |>
    dplyr::group_split() |>
    (\(x){
      unique(dplyr::bind_rows(x)[["record_id"]])[!purrr::list_c(lapply(x, nrow)) == length(unique(dplyr::bind_rows(x)[["svd_user"]]))]
    })()
}


#' Replace multiple values in a vector with a named key
#'
#' @param data vector to replace
#' @param key named character vector with old values named with new values
#' @param keep.non.keyed keep non-keyed values or give NA
#'
#' @return character vector
#' @export
#'
#' @examples
#' ds <- sample(1:6, 20, replace = TRUE)
#' multi_replace(ds, key = c("3" = 1, "4" = 2, "8" = 3, "HEY" = 4, "0" = 5))
#' multi_replace(ds, key = c("3" = 1, "4" = 2, "8" = 3, "HEY" = 4, "0" = 5), keep.non.keyed = FALSE)
multi_replace <- function(data, key = assessor_key, keep.non.keyed = TRUE) {
  trans <- names(key)[match(data, key, nomatch = NA)]
  if (any(is.na(trans))) {
    message("Mind that the key is incomplete")
  }
  if (keep.non.keyed) {
    data[data %in% key] <- trans[!is.na(trans)]
    data
  } else {
    trans
  }
}

#' Collapse vector to readable sentence
#'
#' @param data vector
#' @param sep.last last sep word. Default is "and"
#'
#' @return character vector length 1
#' @export
#'
#' @examples
#' rownames(mtcars)[1:4] |> chr_collapse()
chr_collapse <- function(data, sep.last = "and") {
  if (is.numeric(data)) {
    data <- round(data, 2)
  }
  paste(paste(data[-length(data)], collapse = ", "), sep.last, data[length(data)])
}
