library(rlang)
library(purrr)
library(stringr)
library(glue)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(magrittr)

## read_xls_all_sheets
# Reads every sheet from an xls file into a single data frame.
# Assumes the headers are the first row of the first sheet, and that
# they aren't repeated on subsequent sheets.
read_excel_all_sheets <- function(path) {
  sheets <- excel_sheets(path)
  if (length(sheets) == 1) {
    ## Read only one sheet
    read_excel(path,
               sheet = 1,
               col_names = TRUE,
               col_types = "text")
  } else{
    # Read more than one sheet
    headers <- suppressMessages(read_excel(path,
                                           sheet = sheets[1],
                                           n_max = 1,
                                           col_names = FALSE,
                                           col_types = "text")) %>%
      extract(1, ) %>%
      as.character()
    # Skip the headers on the first sheet
    first_sheet <- read_excel(path,
                              sheet = sheets[1],
                              skip = 1,
                              col_names = headers,
                              col_types = "text")
    # Assume there are no headers on subsequent sheets
    other_sheets <- map_dfr(sheets[2:length(sheets)],
                            ~ read_excel(path,
                                         sheet = .x,
                                         skip = 0,
                                         col_names = headers,
                                         col_types = "text"))
    bind_rows(first_sheet, other_sheets)
  }
}

read_excel_or_csv <- function(datapath) {
  extension <- str_extract(datapath, "\\.[^.]+")
  if (extension %in% c(".txt", ".csv")) {
    read_csv(datapath,
             na = "",
             col_types = cols(.default = col_character()))
  } else if (extension  %in% c(".xls", ".xlsx")) {
    read_excel_all_sheets(datapath)
  } else {
    abort(glue("Extension '{extension}' not recognised."))
  }
}

## read_data
# input - (data frame) a data frame with the columns "name", "size"
#         "type" and "datapath".
read_data <- function(file_df) {
  file_df %>%
    rowwise() %>%
    mutate(
      data = list(read_excel_or_csv(datapath))
    ) %>%
    select(data) %>%
    unnest(data)
}

## percentage_hightlight
percentage_highlight <- function(x) {
  case_when(
    is.na(x) ~ NA_character_,
    x <= 3.2 & x >= 1.8 ~ "#B4E3BB", # green
    x <= 0.9 | x >= 4.1 ~ "#F6B3BD", # red
    TRUE ~ "#F3DE84") # yellow
}
