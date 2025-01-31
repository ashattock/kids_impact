##############################################################################
#
# Pulls citation counts for all papers published by Kids researchers in a
# given year. Pulls from either the Scopus database API or the CrossRef
# equivalent. Takes a PDF file with a complete list of papers. Note that
# this is by no means an exhuastive calculation, so manual checking may be
# needed if this is for anything other than pure curiosity. 
#
# Written (very quickly) by A.J.Shattock, any questions feel free to reach out:
# andrew.shattock@thekids.org.au
#
##############################################################################

# Set working directory to sourced file
if (interactive()) setwd(getSrcDirectory(function() {}))

# ---- Set options ----

# File name of list papers to examine (must be in same directory as this script)
pdf_file = "2024 The Kids Publications complete"

# Which database to pull impact info from
pull_from = "scopus"  # OPTIONS: "crossref" or "scopus"

# Display top n results (by number of citations)
top_n_results = 10

# Define API keys
api_key = list(
  scopus = "6266bdec9dc0002d63a127ec7487b751")

# Regular expression for The Kids affiliation
kids_ref = "Telethon Kids Institute|Kids Research Institute"

# ---- Load packages ----

# Packages used
packages = c(
  "tidyverse", 
  "data.table", 
  "magrittr", 
  "purrr", 
  "pdftools", 
  "httr", 
  "jsonlite", 
  "stringdist", 
  "progress", 
  "ggpubr", 
  "ggrepel")

# We'll use pacman - check whether it's been installed itself
if (!"pacman" %in% rownames(installed.packages())) 
  install.packages("pacman")

# Load pacman
library(pacman) 

# Load all required packages, installing them if required
pacman::p_load(char = packages)

# Clear console
cat("\014")

# ---- Extract all paper details ----

# Load PDF and seperate out
papers = pdf_text(paste0(pdf_file, ".pdf")) %>%
  paste(collapse = "\n") %>%
  str_split("\n\n[0-9]+\\.") %>%
  unlist() %>%
  str_replace_all("\\n", " ") %>%
  str_remove("^\\s+")

# Remove leading text from first line
papers[1] %<>% str_remove("^[^.]+\\.\\s*") 

# Extract only paper titles
titles = papers %>%
  str_remove("^[^.]+\\.\\s*") %>%
  str_remove("[.?].*")

# ---- Crossref ----

# Function to pull Scopus info
pull_crossref = function(idx, pb) {
  
  # Preallocate trivial result
  result = data.table(idx = idx)
  
  # Extract paper title
  title = titles[idx]
  
  # API URL and title-based query
  api   = "https://api.crossref.org/works?query="
  query = URLencode(title, reserved = TRUE)
  
  # Pull from the API
  info = GET(paste0(api, query))
  
  # Update progress bar
  pb$tick()
  
  # Return out if no success
  if (status_code(info) != 200)
    return(result)
  
  # Unpack data of interest
  data = fromJSON(
    txt = content(
      x  = info, 
      as = "text", 
      encoding = "UTF-8")) %>%
    pluck("message") %>%
    pluck("items")
  
  # Return out if no info
  if (is.null(data) || length(data) == 0)
    return(result)
  
  # Multiple hits possible, find the most likely true hit
  hits = unlist(data$title)
  this = which.min(stringdist(title, hits, method = "lv"))
  
  # Extract details about citations and affiliation of first author
  cites = data$"is-referenced-by-count"[this]
  affil = grepl(kids_ref, data$author[[this]]$affiliation[[1]])
  
  # Append outcomes to result datatable
  result %<>% mutate(citations = cites, affil = isTRUE(affil))
  
  # Update progress bar
  pb$tick()
  
  return(result)
}

# ---- Scopus ----

# Function to pull Scopus info
pull_scopus = function(idx, pb) {
  
  # Preallocate trivial result
  result = data.table(idx = idx)
  
  # Extract paper title
  title = titles[idx]
  
  # API URL and title-based query
  api   = "https://api.elsevier.com/content/search/scopus?query="
  query = URLencode(paste0("TITLE(", title, ")"))
  
  # Query with API key
  headers = add_headers(
    "X-ELS-APIKey" = api_key$scopus, 
    Accept = "application/json")
  
  # Pull from the API
  info = GET(paste0(api, query), headers)
  
  # Update progress bar
  pb$tick()
  
  # Return out if no success
  if (status_code(info) != 200)
    return(result)
  
  # Unpack data of interest
  data = fromJSON(
    txt = content(
      x  = info, 
      as = "text", 
      encoding = "UTF-8")) %>%
    pluck("search-results") %>%
    pluck("entry")
  
  # Return out if no info
  if (is.null(data) || length(data) == 0)
    return(result)
  
  # Multiple hits possible, find the most likely true hit
  hits = data$"dc:title"
  this = which.min(stringdist(title, hits, method = "lv"))
  
  # Extract details about citations and affiliation of first author
  cites = as.numeric(data$"citedby-count"[this])
  affil = any(grepl(kids_ref, data$affiliation[[this]]))
  
  # Return out if no info
  if (length(cites) == 0)
    return(result)
  
  # Append outcomes to result datatable
  result %<>% mutate(
    citations = cites, 
    kids_lead = affil)
  
  return(result)
}

# ---- Set up for heavy lifting ----

message("* Running impact estimates of The Kids Research papers")

# Number of papers identified
n_papers = length(papers)

# Report basic properties and settings to user
message(" - PDF input file: '", pdf_file, "'")
message(" - Papers identified: ", n_papers)
message(" - Impact method: ", pull_from)

# Set of papers to examine
idx = seq_along(titles)

# Reference function to do the heavy lifting
pull_fn = get(paste0("pull_", pull_from))

# Initiate progress bar from progress package
pb = progress_bar$new(
  format     = " [:bar] :percent (remaining: :eta)",
  total      = length(idx), # Number of tasks to complete
  complete   = "-",   # Completion bar character
  incomplete = " ",   # Incomplete bar character
  current    = ">",   # Current bar character
  clear      = TRUE,  # If TRUE, clears the bar when finished
  width      = 125)   # Width of the progress bar

# ---- Pull data for each paper ----

# Apply function, and rank by number of citations
results_dt = lapply(idx, pull_fn, pb = pb) %>%
  rbindlist(fill = TRUE) %>%
  mutate(paper = papers[idx]) %>%
  arrange(-citations) %>%
  mutate(rank = seq_along(idx), 
         .before = 1)

# Save all results to file
save_name = paste(pdf_file, "results", pull_from, sep = " - ")
fwrite(results_dt, paste0(save_name, ".csv"))

# ---- Plot diagnostic figure ----

# Construct plotting datatable
plot_dt = results_dt %>%
  mutate(title = str_wrap(titles[idx], 60)) %>%
  select(citations, kids_lead, title) %>%
  filter(!is.na(citations))

# Histogram of citation numbers
g1 = ggplot(plot_dt[citations > 0]) +
  aes(x = citations, fill = kids_lead) + 
  geom_histogram(
    binwidth = 1) + 
  scale_y_continuous(
    name = "Count (one or more citations)") + 
  scale_fill_manual(
    name = "The Kids lead", 
    values = c("grey30", "dodgerblue"))

# Dotplot of top_n_results
g2 = ggplot(plot_dt[1 : top_n_results, ]) +
  aes(x = citations) + 
  geom_label_repel(
    mapping = aes(y = 0.2, label = title), 
    size  = 2,
    hjust = 0,              
    box.padding   = 2,      
    point.padding = 0,    
    segment.size  = 0.1,     
    segment.color = "grey50",
    label.padding = 0.1,    
    label.size    = 0.1,      
    max.overlaps  = 100, 
    force      = 1, 
    force_pull = 0.5) + 
  geom_dotplot(
    mapping  = aes(fill = kids_lead),
    binwidth = 1,
    dotsize  = 1) + 
  scale_y_continuous(
    name = paste("Top", top_n_results, "papers"),
    limits = c(0, 0.4)) + 
  scale_fill_manual(
    name = "The Kids lead", 
    values = c("grey30", "dodgerblue"))

# Combine plots
g = ggarrange(
  plotlist = list(g1, g2), 
  ncol   = 1,
  legend = "right", 
  common.legend = TRUE)

# Save diagnostic figure to file
save_name = paste(pdf_file, "diagnostics", pull_from, sep = " - ")
ggsave(filename = paste0(save_name, ".png"), 
       plot   = g, 
       device = "png", 
       width  = 9, 
       height = 6)

# ---- Other diagnostics ----

message("* Basic diagnostics: ")

# Number and percentage of papers identified
n_hits = sum(!is.na(results_dt$citations))
p_hits = round(100 * n_hits / n_papers)

# Number and percentage of papers identified to be lead by The Kids (first author)
n_kids = sum(na.omit(results_dt$kids_lead))
p_kids = round(100 * n_kids / n_papers)

# Total number of citations, and mean of identified papers
n_cite = sum(na.omit(results_dt$citations))
n_mean = round(n_cite / n_hits, 2)

# Display the basic diagnostics
message(" - Papers evaluated: ", n_hits, " (", p_hits, "%)")
message(" - Identified as The Kids lead: ", n_kids, " (", p_kids, "%)")
message(" - Total citations: ", n_cite, " (mean = ", n_mean, ")")

# ---- Display top results ----

message("* Top ", top_n_results, " results:")

# Display top n results
results_dt %>%
  mutate(title = titles[idx]) %>%
  slice_head(n = top_n_results) %>%
  mutate(citations = paste0("[", citations, " citations]"), 
         kids_lead = ifelse(kids_lead, "*The Kids lead*", "")) %>%
  mutate(str = paste(" #", rank, citations, kids_lead, title)) %>%
  pull(str) %>%
  paste(collapse = "\n") %>%
  message()

