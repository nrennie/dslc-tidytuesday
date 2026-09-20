library(tidyverse)
library(rvest)

main_url <- "https://gender-pay-gap.service.gov.uk/viewing/download-data/"
all_data <- list()
all_years <- 2017:2025

for (i in 1:length(all_years)) {
  year_url <- paste0(main_url, all_years[i])

  raw_data <- readr::read_csv(year_url)

  all_data[[i]] <- raw_data |>
    filter(str_starts(CompanyNumber, "SC")) |>
    mutate(Year = paste0(all_years[i], "-", all_years[i] + 1)) |>
    select(
      Year,
      EmployerId,
      Address,
      EmployerSize,
      SicCodes,
      starts_with("Diff")
    )
}

pay_gaps <- all_data |>
  bind_rows() |>
  mutate(SicCodes = str_replace_all(SicCodes, "\n", "")) |>
  mutate(SIC2 = str_replace_all(SicCodes, "(^|,)\\s*(\\d{2})\\d*", "\\1\\2")) |>
  select(-SicCodes)


# SIC Codes ---------------------------------------------------------------

raw_url <- "https://siccode.com/sic-code-lookup-directory"
raw_html <- read_html(raw_url)
sic_html <- raw_html |>
  html_element(".sic-code")
cats <- sic_html |>
  html_elements(".bg-sic") |>
  html_attr("title")
ranges <- sic_html |>
  html_elements("dl") |>
  html_elements("dt") |>
  html_text()
sic_codes <- tibble(
  range = ranges,
  industry = cats
) |>
  separate_wider_delim(range, delim = "-", names = c("start", "end"))


