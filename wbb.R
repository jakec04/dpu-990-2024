# ------------------------------------------
# 990 Women's Basketball Coach Scraper - IMPROVED VERSION
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

# ---- 3. Extract ALL officers and filter later ----
extract_all_officers <- function(xml_path) {
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

  return(officers)
}

# ---- 4. Process all EINs and get ALL officers ----
message("\nStarting to process ", nrow(peers), " organizations...")

all_officers <- peers %>%
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
    officers_data = map(xml_file, extract_all_officers)
  ) %>%
  filter(!map_lgl(officers_data, is.null)) %>%
  unnest(officers_data, keep_empty = FALSE)

# ---- 5. Look for basketball-related titles ----
message("\n\nSearching for basketball coaches in titles...")

# First, let's see ALL basketball-related titles
basketball_titles <- all_officers %>%
  filter(str_detect(tolower(title), "basketball|coach")) %>%
  select(school, ein, xml_file, name, title, comp_org, comp_related, comp_other) %>%
  arrange(school, title)

# Save all basketball-related for review
write_csv(basketball_titles, "/Users/jakecox/rstudio/990-scraper/all_basketball_titles.csv")
message("\nSaved all basketball-related titles to: all_basketball_titles.csv")
message("Review this file to see what titles exist")

# Now try to filter for women's basketball
womens_coaches <- basketball_titles %>%
  filter(str_detect(tolower(title), "women|w\\.b\\.|w/|wbb|female"))

# Also try finding by excluding men's
womens_coaches_alt <- basketball_titles %>%
  filter(
    str_detect(tolower(title), "basketball"),
    !str_detect(tolower(title), "men|m\\.b\\.|m/|mbb|male|assistant")
  )

# Combine both approaches
results <- bind_rows(womens_coaches, womens_coaches_alt) %>%
  distinct(school, name, .keep_all = TRUE) %>%
  group_by(school) %>%
  slice_max(comp_org, n = 1, with_ties = FALSE) %>%
  ungroup()

# ---- 6. Save results ----
if (nrow(results) > 0) {
  out_path <- "/Users/jakecox/rstudio/990-scraper/womens_basketball_coach_compensation.csv"
  write_csv(results, out_path)
  message("\n✓ Success! Saved ", nrow(results), " women's basketball coaches")
  message("  ", out_path)

  # Show preview with all comp columns
  print(results %>% select(school, name, title, comp_org, comp_related, comp_other))
} else {
  warning("\n✗ No women's basketball coaches found")
}

# Show summary
message("\n=== SUMMARY ===")
message("Total schools processed: ", length(unique(all_officers$school)))
message("Schools with ANY basketball titles: ", length(unique(basketball_titles$school)))
message("Schools with women's basketball coaches identified: ", nrow(results))
message("\nSchools missing women's basketball coaches:")
missing_schools <- peers %>%
  filter(!school %in% results$school) %>%
  pull(school)
print(missing_schools)
