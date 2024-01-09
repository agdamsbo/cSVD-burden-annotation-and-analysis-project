#' Imports specified TALOS data
#'
#' @param token api token
#' @param vars variable names to retrieve (fields in REDCap lingo)
#'
#' @return tibble of REDCap exported data
#' @examples
#' data <- import_talos()
#' skimr::skim(data)
#' ds <- import_talos()
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
#' ds |> modify_data(index=0)
modify_data <- function(data,
                        index = redcap_get_n(),
                        trial = "TALOS",
                        key = "SVD_REDCAP_API",
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
      index_date = lubridate::ymd({{ date.var }}),
      index_time = format(as.POSIXct({{ time.var }}), "%H:%M"),
      trial_name = dplyr::case_match(
        trial,
        "TALOS" ~ 1,
        "RESIST" ~ 2
      ),
      basis_complete = 2
    )
}


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
#'   write2new_db()
write2new_db <- function(data, key = "SVD_REDCAP_API") {
  REDCapR::redcap_write(
    redcap_uri = "https://redcap.au.dk/api/",
    token = keyring::key_get(key),
    ds_to_write = data
  )
}


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
clean_record_id <- function(data, remove = "svd_") {
  data |>
    dplyr::mutate(record_id = stringr::str_remove(record_id, remove)) |>
    dplyr::arrange(record_id)
}

remotes::install_github("agdamsbo/REDCapCAST")
library(REDCapCAST)

#' Read single REDCap instrument
#'
#' @param key API key
#' @param instrument instrument name
#'
#' @return data.frame
#' @export
#'
#' @examples
#' read_instrument(key = "SVD_REDCAP_API", instrument = "svd_score")
read_instrument <- function(key = "SVD_REDCAP_API", instrument = "svd_score", raw_label = "raw") {
  REDCapCAST::read_redcap_tables(
    uri = "https://redcap.au.dk/api/", token = keyring::key_get(key),
    fields = "record_id",
    forms = instrument, raw_or_label = raw_label
  )[[instrument]]
}


#' Title
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
    clean_record_id() |>
    dplyr::filter(svd_perf == 1) |>
    dplyr::select(
      record_id,
      svd_user,
      svd_quality,
      svd_microbleed,
      svd_microbleed_location___1,
      svd_microbleed_location___2,
      svd_microbleed_location___3,
      svd_siderose,
      svd_lacunes,
      svd_wmh,
      svd_atrophy
    )
}


#' Inter rater reliability calculations
#'
#' @param data
#'
#' @return
#' @export tibble
#'
#' @examples
#' irr_sample |> inter_rater()
#' read_instrument() |>
#'   inter_rater_data() |>
#'   inter_rater()
inter_rater <- function(data) {
  data |> tidycomm::test_icr(unit_var = record_id, coder_var = svd_user, fleiss_kappa = TRUE)
}
