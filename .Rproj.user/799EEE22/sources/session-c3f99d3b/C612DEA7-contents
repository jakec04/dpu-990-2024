library(dplyr)
library(readr)

# ---- 1. Load all three files ----
ceo <- read_csv("/Users/jakecox/rstudio/990-scraper/peer_exec_compensation.csv")
mens_bball <- read_csv("/Users/jakecox/rstudio/990-scraper/basketball_coach_compensation.csv")
womens_bball <- read_csv("/Users/jakecox/rstudio/990-scraper/womens_basketball_coach_compensation.csv")

# ---- 2. Prepare each dataset with role-specific column names ----
ceo_wide <- ceo %>%
  select(school, ein,
         ceo_name = name,
         ceo_title = title,
         ceo_comp_org = comp_org,
         ceo_comp_related = comp_related,
         ceo_comp_other = comp_other)

mens_wide <- mens_bball %>%
  select(school, ein,
         mens_coach_name = name,
         mens_coach_title = title,
         mens_coach_comp_org = comp_org,
         mens_coach_comp_related = comp_related,
         mens_coach_comp_other = comp_other)

womens_wide <- womens_bball %>%
  select(school, ein,
         womens_coach_name = name,
         womens_coach_title = title,
         womens_coach_comp_org = comp_org,
         womens_coach_comp_related = comp_related,
         womens_coach_comp_other = comp_other)

# ---- 3. Join all three by school ----
combined_wide <- ceo_wide %>%
  full_join(mens_wide, by = c("school", "ein")) %>%
  full_join(womens_wide, by = c("school", "ein"))

# ---- 4. Save wide format ----
out_path <- "/Users/jakecox/rstudio/990-scraper/all_compensation_wide.csv"
write_csv(combined_wide, out_path)
message("✓ Saved wide format to: ", out_path)

# ---- 5. Preview ----
print(combined_wide)
View(combined_wide)

# Show summary
message("\n=== SUMMARY ===")
message("Total schools: ", nrow(combined_wide))
message("Schools with CEO data: ", sum(!is.na(combined_wide$ceo_name)))
message("Schools with Men's Basketball Coach: ", sum(!is.na(combined_wide$mens_coach_name)))
message("Schools with Women's Basketball Coach: ", sum(!is.na(combined_wide$womens_coach_name)))
