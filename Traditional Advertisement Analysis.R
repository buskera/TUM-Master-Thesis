# Required Libraries
library(imager)
library(dplyr)
library(tibble)
library(gt)

# Function to calculate visual complexity
check_visual_complexity <- function(image_path) {
  tryCatch({
    img <- load.image(image_path)
    file_size_kb <- as.numeric(file.info(image_path)$size) / 1024 # File size in kB
    resolution <- as.numeric(dim(img)[1]) * as.numeric(dim(img)[2])
    visual_complexity <- file_size_kb / resolution
    if (length(visual_complexity) != 1) stop("Invalid output size")
    return(visual_complexity)
  }, error = function(e) {
    cat("Error in check_visual_complexity:", conditionMessage(e), "\n")
    return(NA)
  })
}

# Function to calculate color harmony
check_color_harmony <- function(image_path) {
  tryCatch({
    img <- load.image(image_path)
    img_array <- as.array(img)
    hsv_img <- RGBtoHSV(img_array)
    color_harmony <- 1 / (1 + sd(as.numeric(hsv_img[,,2]), na.rm = TRUE))
    if (length(color_harmony) != 1) stop("Invalid output size")
    return(color_harmony)
  }, error = function(e) {
    cat("Error in check_color_harmony:", conditionMessage(e), "\n")
    return(NA)
  })
}

# Function to calculate edge density
check_edge_density <- function(image_path) {
  tryCatch({
    img <- load.image(image_path)
    edges <- imgradient(img, "xy") %>% enorm() # Edge detection using gradient
    edge_density <- mean(edges > 0) # Proportion of edge pixels
    if (length(edge_density) != 1) stop("Invalid output size")
    return(edge_density)
  }, error = function(e) {
    cat("Error in check_edge_density:", conditionMessage(e), "\n")
    return(NA)
  })
}

# Function to calculate saturation
check_saturation <- function(image_path) {
  tryCatch({
    # Load the image
    img <- load.image(image_path)
    
    # Convert the image from RGB to HSV
    hsv_img <- RGBtoHSV(as.array(img))
    
    # Extract the saturation channel (second channel in HSV)
    saturation_values <- as.numeric(hsv_img[,,2])
    
    # Calculate the average saturation value
    avg_saturation <- mean(saturation_values, na.rm = TRUE)
    
    if (length(avg_saturation) != 1) stop("Invalid output size")
    return(avg_saturation)
    
  }, error = function(e) {
    cat("Error in check_color_saturation:", conditionMessage(e), "\n")
    return(NA)
  })
}
# Function to calculate image contrast
check_contrast <- function(image_path) {
  tryCatch({
    img <- load.image(image_path)
    img_gray <- grayscale(img) # Convert to grayscale
    contrast <- sd(as.numeric(img_gray)) # Standard deviation of pixel intensities
    if (length(contrast) != 1) stop("Invalid output size")
    return(contrast)
  }, error = function(e) {
    cat("Error in check_contrast:", conditionMessage(e), "\n")
    return(NA)
  })
}

# Function to create HTML image tags for the table
create_image_html <- function(image_path) {
  if (file.exists(image_path)) {
    paste0('<img src="', normalizePath(image_path, winslash = "/"), '" style="max-width: 70px; height: auto;">')
  } else {
    "Image not found"
  }
}

# Modify the analyze_image function to include optimized image HTML
analyze_image <- function(image_path, nickname, industry) {
  message("Analyzing image: ", nickname)
  
  visual_complexity <- check_visual_complexity(image_path)
  color_harmony <- check_color_harmony(image_path)
  edge_density <- check_edge_density(image_path)
  saturation <- check_saturation(image_path)
  contrast <- check_contrast(image_path)
  
  # Create optimized HTML for displaying the image in the table
  image_html <- create_image_html(image_path)
  
  data.frame(
    Image = image_html,
    Industry = as.character(industry),
    Visual_Complexity = visual_complexity,
    Color_Harmony = color_harmony,
    Edge_Density = edge_density,
    Saturation = saturation,
    Contrast = contrast,
    stringsAsFactors = FALSE
  )
}

# Image paths and their nicknames
image_paths <- list(
  Beneficial_Bank = "C:/Users/allyr/OneDrive/Documents/Masters Thesis/Images/Edited AI Ads/finance/finance ad with human.jpg",
  Carrefour = "C:/Users/allyr/OneDrive/Documents/Masters Thesis/Images/Edited AI Ads/finance/finance ad.jpg",
  NetFlix = "C:/Users/allyr/OneDrive/Documents/Masters Thesis/Images/Edited AI Ads/media/netflix ad with human.jpeg",
  Audible = "C:/Users/allyr/OneDrive/Documents/Masters Thesis/Images/Edited AI Ads/media/audible ad.jpeg",
  Dior = "C:/Users/allyr/OneDrive/Documents/Masters Thesis/Images/Edited AI Ads/retail/Dior mascara ad.jpg",
  Sun_Bum = "C:/Users/allyr/OneDrive/Documents/Masters Thesis/Images/Edited AI Ads/retail/retail ad.jpeg"
)

# Industry labels for each image
industry_labels <- list(
  Beneficial_Bank = "Financial Services",
  Carrefour = "Financial Services",
  NetFlix = "Entertainment & Media",
  Audible = "Entertainment & Media",
  Dior = "Retail",
  Sun_Bum = "Retail"
)

# Analyze all images and bind results
image_analysis_results <- lapply(names(image_paths), function(nickname) {
  path <- image_paths[[nickname]]  # Ensure path is retrieved correctly
  industry <- industry_labels[[nickname]]  # Retrieve industry label
  analyze_image(path, nickname, industry)  # Analyze image
}) %>% bind_rows()

# Display the results in a polished table with optimized images
image_analysis_results %>%
  gt() %>%
  tab_header(
    title = "Visual Metrics Analysis for Traditional Advertisements",
    subtitle = "Comparison of Visual Complexity, Color Harmony, Edge Density, Luminance Entropy, and Contrast across Industries"
  ) %>%
  fmt_number(columns = c(Visual_Complexity, Color_Harmony, Edge_Density, Saturation, Contrast), decimals = 4) %>%
  cols_label(
    Image = "Advertisement Image",
    Industry = "Industry",
    Visual_Complexity = "Visual Complexity",
    Color_Harmony = "Color Harmony",
    Edge_Density = "Edge Density",
    Saturation = "Saturation",
    Contrast = "Contrast"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()  # Centers all columns in the table
  ) %>%
  tab_options(
    table.font.size = "small",
    heading.align = "center",
    column_labels.font.size = "medium",
    column_labels.font.weight = "bold"
  ) %>%
  # Enable HTML rendering to show optimized images
  fmt_markdown(columns = vars(Image))  # Render images as HTML
