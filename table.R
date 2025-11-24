library(dplyr)
library(readr)
library(stringr)
library(reactable)
library(htmltools)
library(crosstalk)

# Load your data
combined <- read_csv("/Users/jakecox/Downloads/990_combo.csv")

# Clean up the currency columns and convert to numeric
table_data <- combined %>%
  mutate(
    # Remove $ and , and convert to numeric, replace NA strings with actual NA
    CEO_TOTAL = ifelse(CEO_TOTAL == "NA" | is.na(CEO_TOTAL), NA_real_, as.numeric(str_remove_all(CEO_TOTAL, "[$,]"))),
    MBB_TOTAL = ifelse(MBB_TOTAL == "NA" | is.na(MBB_TOTAL), NA_real_, as.numeric(str_remove_all(MBB_TOTAL, "[$,]"))),
    WBB_TOTAL = ifelse(WBB_TOTAL == "NA" | is.na(WBB_TOTAL), NA_real_, as.numeric(str_remove_all(WBB_TOTAL, "[$,]")))
  ) %>%
  mutate(
    # Calculate rankings for each category
    CEO_RANK = rank(-CEO_TOTAL, na.last = "keep", ties.method = "min"),
    MBB_RANK = rank(-MBB_TOTAL, na.last = "keep", ties.method = "min"),
    WBB_RANK = rank(-WBB_TOTAL, na.last = "keep", ties.method = "min"),

    # Flag DePaul and Big East schools
    is_depaul = school == "DePaul University",
    Conference = ifelse(
      school %in% c(
        "Butler University",
        "University of Connecticut", "Connecticut",
        "Creighton University",
        "DePaul University",
        "Georgetown University",
        "Marquette University",
        "Providence College",
        "St. John's University-New York", "St. John's University",
        "Seton Hall University",
        "Villanova University",
        "Xavier University"
      ),
      "Big East",
      "Peer Institutions"
    ),
    sort_key = ifelse(is_depaul, 0, 1)
  ) %>%
  arrange(sort_key, school) %>%
  select(school, Conference, CEO_TOTAL, MBB_TOTAL, WBB_TOTAL, CEO_RANK, MBB_RANK, WBB_RANK, is_depaul, sort_key)

# Wrap data in SharedData for crosstalk
shared_data <- SharedData$new(table_data)

# Calculate total count for each category (non-NA values)
ceo_total_count <- sum(!is.na(table_data$CEO_TOTAL))
mbb_total_count <- sum(!is.na(table_data$MBB_TOTAL))
wbb_total_count <- sum(!is.na(table_data$WBB_TOTAL))

# Custom currency formatter
format_currency <- function(value) {
  if (is.na(value)) return("—")
  paste0("$", format(round(value), big.mark = ",", scientific = FALSE))
}

# Create professional interactive table with expandable details
tbl <- reactable(
  shared_data,
  defaultSorted = "sort_key",
  defaultSortOrder = "asc",
  searchable = TRUE,
  highlight = TRUE,
  bordered = FALSE,
  striped = FALSE,
  compact = FALSE,
  defaultPageSize = 25,
  pageSizeOptions = c(10, 25, 50, 100),
  showPageSizeOptions = TRUE,
  showSortIcon = TRUE,

  # Highlight DePaul row
  rowStyle = function(index) {
    if (table_data$is_depaul[index]) {
      list(
        background = "#f0f4f8",
        borderLeft = "5px solid #0b305a",
        fontWeight = "600"
      )
    }
  },

  # Make rows expandable
  details = function(index) {
    school_data <- table_data[index, ]
    is_depaul <- school_data$is_depaul

    htmltools::div(
      style = "padding: 20px; background: #f8f9fa; border-top: 2px solid #e0e0e0;",

      htmltools::h4(
        style = "margin: 0 0 16px 0; font-size: 18px; font-weight: 600; color: #0b305a;",
        paste0(school_data$school, " — Rankings")
      ),

      htmltools::div(
        style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 16px;",

        # CEO Ranking Card
        htmltools::div(
          style = "background: white; padding: 16px; border-radius: 8px; border-left: 4px solid #0b305a;",
          htmltools::div(
            style = "font-size: 12px; color: #666; text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 8px;",
            "CEO/President"
          ),
          htmltools::div(
            style = "font-size: 28px; font-weight: 700; color: #0b305a; margin-bottom: 4px;",
            if (is.na(school_data$CEO_RANK)) "—" else paste0("#", school_data$CEO_RANK, " / ", ceo_total_count)
          ),
          htmltools::div(
            style = "font-size: 14px; color: #666;",
            format_currency(school_data$CEO_TOTAL)
          )
        ),

        # Men's BB Ranking Card
        htmltools::div(
          style = "background: white; padding: 16px; border-radius: 8px; border-left: 4px solid #0b305a;",
          htmltools::div(
            style = "font-size: 12px; color: #666; text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 8px;",
            "Men's Basketball Coach"
          ),
          htmltools::div(
            style = "font-size: 28px; font-weight: 700; color: #0b305a; margin-bottom: 4px;",
            if (is.na(school_data$MBB_RANK)) "—" else paste0("#", school_data$MBB_RANK, " / ", mbb_total_count)
          ),
          htmltools::div(
            style = "font-size: 14px; color: #666;",
            format_currency(school_data$MBB_TOTAL)
          )
        ),

        # Women's BB Ranking Card
        htmltools::div(
          style = "background: white; padding: 16px; border-radius: 8px; border-left: 4px solid #0b305a;",
          htmltools::div(
            style = "font-size: 12px; color: #666; text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 8px;",
            "Women's Basketball Coach"
          ),
          htmltools::div(
            style = "font-size: 28px; font-weight: 700; color: #0b305a; margin-bottom: 4px;",
            if (is.na(school_data$WBB_RANK)) "—" else paste0("#", school_data$WBB_RANK, " / ", wbb_total_count)
          ),
          htmltools::div(
            style = "font-size: 14px; color: #666;",
            format_currency(school_data$WBB_TOTAL)
          )
        )
      )
    )
  },

  # Column definitions
  columns = list(
    school = colDef(
      name = "School",
      minWidth = 220,
      style = function(value, index) {
        if (table_data$is_depaul[index]) {
          list(fontWeight = 600, fontSize = "15px", cursor = "pointer", color = "#0b305a")
        } else {
          list(fontWeight = 500, fontSize = "15px", cursor = "pointer")
        }
      }
    ),
    Conference = colDef(
      name = "Group",
      width = 140,
      align = "center",
      style = function(value) {
        if (value == "Big East") {
          list(
            fontWeight = 600,
            color = "#0b305a",
            fontSize = "13px"
          )
        } else {
          list(
            color = "#666",
            fontSize = "13px"
          )
        }
      }
    ),
    CEO_TOTAL = colDef(
      name = "CEO/President",
      align = "right",
      minWidth = 160,
      defaultSortOrder = "desc",
      format = colFormat(currency = "USD", separators = TRUE, digits = 0),
      na = "—",
      style = function(value) {
        if (is.na(value)) return(list(color = "#ccc", fontWeight = 500))
        list(fontWeight = 500)
      }
    ),
    MBB_TOTAL = colDef(
      name = "Men's BB Coach",
      align = "right",
      minWidth = 160,
      defaultSortOrder = "desc",
      format = colFormat(currency = "USD", separators = TRUE, digits = 0),
      na = "—",
      style = function(value) {
        if (is.na(value)) return(list(color = "#ccc", fontWeight = 500))
        list(fontWeight = 500)
      }
    ),
    WBB_TOTAL = colDef(
      name = "Women's BB Coach",
      align = "right",
      minWidth = 160,
      defaultSortOrder = "desc",
      format = colFormat(currency = "USD", separators = TRUE, digits = 0),
      na = "—",
      style = function(value) {
        if (is.na(value)) return(list(color = "#ccc", fontWeight = 500))
        list(fontWeight = 500)
      }
    ),
    # Hide the rank and flag columns
    CEO_RANK = colDef(show = FALSE),
    MBB_RANK = colDef(show = FALSE),
    WBB_RANK = colDef(show = FALSE),
    is_depaul = colDef(show = FALSE),
    sort_key = colDef(show = FALSE)
  ),

  # Default column styling
  defaultColDef = colDef(
    headerStyle = list(
      backgroundColor = "#0b305a",
      color = "white",
      borderBottom = "2px solid #0b305a",
      fontWeight = 600,
      fontSize = "13px",
      textTransform = "uppercase",
      letterSpacing = "0.5px"
    )
  ),

  # Overall theme
  theme = reactableTheme(
    borderColor = "#e0e0e0",
    highlightColor = "#f0f5ff",
    cellPadding = "14px 16px",
    style = list(
      fontFamily = "'Hanken Grotesk', -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif",
      fontSize = "14px"
    ),
    searchInputStyle = list(
      width = "100%",
      maxWidth = "400px",
      padding = "8px 12px",
      border = "2px solid #0b305a",
      borderRadius = "4px",
      fontSize = "14px"
    )
  )
)

# Wrap in a nice container with title and filter toggle
output <- div(
  style = "max-width: 1300px; margin: 40px auto; padding: 20px; font-family: 'Hanken Grotesk', -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif;",

  # Add Google Font
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Hanken+Grotesk:wght@400;500;600;700&display=swap",
      rel = "stylesheet"
    )
  ),

  h1(
    style = "font-size: 32px; font-weight: 700; margin-bottom: 8px; color: #0b305a;",
    "DePaul Leadership Compensation Comparison"
  ),

  p(
    style = "font-size: 17px; color: #666; margin-bottom: 12px; line-height: 1.5;",
    "How DePaul's executive and coaching compensation compares to its peer institutions and Big East competitors, based on IRS Form 990 filings."
  ),

  p(
    style = "font-size: 14px; color: #999; margin-bottom: 24px; font-style: italic;",
    "Peer group selected by DePaul through IPEDS data. Click any school to see detailed rankings."
  ),

  # Filter Toggle using crosstalk
  div(
    style = "margin-bottom: 24px;",

    tags$style(HTML("
      .crosstalk-input-checkboxgroup label {
        display: inline-block;
        margin-right: 24px;
        font-family: 'Hanken Grotesk', sans-serif;
        font-size: 15px;
        font-weight: 500;
        color: #333;
        cursor: pointer;
      }
      .crosstalk-input-checkboxgroup input[type='checkbox'] {
        margin-right: 8px;
        width: 18px;
        height: 18px;
        accent-color: #0b305a;
        cursor: pointer;
      }
      .crosstalk-input-checkboxgroup {
        padding: 0;
        background: transparent;
        border: none;
      }
    ")),

    filter_checkbox("conference", "Show:", shared_data, ~Conference, inline = TRUE)
  ),

  # The table
  tbl,

  div(
    style = "margin-top: 24px; padding: 16px; background: #f8f9fa; border-left: 4px solid #0b305a; border-radius: 4px;",
    p(
      style = "margin: 0 0 8px 0; font-size: 14px; color: #333; font-weight: 600;",
      "About this data:"
    ),
    p(
      style = "margin: 0; font-size: 13px; color: #666; line-height: 1.6;",
      "Compensation data comes from IRS Form 990 filings. Total compensation includes base salary, compensation from related organizations, and other compensation. Dashes (—) indicate positions not found in 990 filings. DePaul University is highlighted in blue throughout the table."
    )
  )
)

# Display in viewer
output

# Save as HTML
save_html(
  output,
  "/Users/jakecox/Downloads/compensation_interactive.html",
  libdir = "lib"
)

message("✓ Saved DePaul-branded interactive table to: /Users/jakecox/Downloads/compensation_interactive.html")
