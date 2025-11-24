# ------------------------------------------
# 990 Men's Basketball Coach Scraper - LOCAL FILES VERSION
# ------------------------------------------

library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(xml2)
library(tidyr)

# ---- 1. Load peer EIN list ----
peers <- read_csv(
  "/Users/jakecox/rstudio/990-scraper/peers_eins.csv",
  show_col_types = FALSE
) %>%
  rename(
    school = SCHOOL,
    ein = EIN
  ) %>%
  mutate(ein = str_remove_all(as.character(ein), "-"))

print(paste("Loaded", nrow(peers), "schools"))

# ---- 2. Find XML file for an EIN ----
find_xml_file <- function(ein, xml_dir = "/Users/jakecox/rstudio/990-scraper/990_XML") {
  xml_files <- list.files(xml_dir, pattern = "\\.xml$", full.names = TRUE, ignore.case = TRUE)
  matching <- xml_files[grepl(ein, xml_files, ignore.case = TRUE)]

  if (length(matching) > 0) {
    return(matching[1])
  }

  return(NA_character_)
}

# ---- 3. Extract Basketball Coach from XML ----
extract_basketball_coach <- function(xml_path) {
  if (is.na(xml_path) || !file.exists(xml_path)) {
    warning("  XML file not found")
    return(NULL)
  }

  doc <- tryCatch({
    read_xml(xml_path)
  }, error = function(e) {
    warning("  Could not read XML: ", e$message)
    return(NULL)
  })

  if (is.null(doc)) return(NULL)

  # Try different XPath patterns
  nodes <- xml_find_all(doc, "//*[local-name()='Form990PartVIISectionAGrp']")

  if (length(nodes) == 0) {
    message("  No officer nodes found")
    return(NULL)
  }

  officers <- map_df(nodes, function(n) {
    tibble(
      name = xml_text(xml_find_first(n, ".//*[local-name()='PersonNm']")),
      title = xml_text(xml_find_first(n, ".//*[local-name()='TitleTxt']")),
      comp_org = suppressWarnings(as.numeric(xml_text(xml_find_first(n, ".//*[local-name()='ReportableCompFromOrgAmt']")))),
      comp_related = suppressWarnings(as.numeric(xml_text(xml_find_first(n, ".//*[local-name()='ReportableCompFromRltdOrgAmt']")))),
      comp_other = suppressWarnings(as.numeric(xml_text(xml_find_first(n, ".//*[local-name()='OtherCompensationAmt']"))))
    )
  }) %>%
    filter(!is.na(name), name != "")

  if (nrow(officers) == 0) {
    message("  No officers found")
    return(NULL)
  }

  # Find basketball coach by title
  bball_coach <- officers %>%
    filter(str_detect(tolower(title), "basketball|men's basketball|mens basketball|head coach.*basketball|coach.*men"))

  if (nrow(bball_coach) == 0) {
    message("  No basketball coach found")
    return(NULL)
  }

  # If multiple matches, take highest paid
  coach <- bball_coach %>%
    slice_max(comp_org, n = 1, with_ties = FALSE) %>%
    slice(1)

  message("  Found coach: ", coach$name, " - ", coach$title)

  return(coach)
}

# ---- 4. Process all EINs ----
message("\nStarting to process ", nrow(peers), " organizations...")

results <- peers %>%
  mutate(
    xml_file = map_chr(ein, function(e) {
      message("\nProcessing: ", school, " (EIN: ", e, ")")
      xml_path <- find_xml_file(e)
      if (is.na(xml_path)) {
        message("  XML file not found")
      } else {
        message("  Found: ", basename(xml_path))
      }
      return(xml_path)
    })
  ) %>%
  filter(!is.na(xml_file)) %>%
  mutate(
    coach_data = map(xml_file, extract_basketball_coach)
  ) %>%
  filter(!map_lgl(coach_data, is.null)) %>%
  unnest(coach_data, keep_empty = FALSE)

# ---- 5. Save results ----
if (nrow(results) > 0) {
  out_path <- "/Users/jakecox/rstudio/990-scraper/basketball_coach_compensation.csv"
  write_csv(results, out_path)
  message("\n✓ Success! Saved ", nrow(results), " basketball coaches of ", nrow(peers), " schools to:")
  message("  ", out_path)

  # Show preview
  print(results %>% select(school, ein, name, title, comp_org))
} else {
  warning("\n✗ No basketball coaches found in any 990 forms")
}

# Summary
message("\n=== SUMMARY ===")
message("Schools with basketball coaches found: ", nrow(results))
message("Schools without basketball coaches: ", nrow(peers) - nrow(results))
