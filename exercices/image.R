
# Function to convert a color image to black and white
convert_to_bw <- function(image) {
  bw_image <- channel(image, "gray")
  return(bw_image)
}

# Function to convert a color image to high contrast black and white
high_contrast_bw <- function(image, threshold = 0.5) {
  bw_image <- channel(image, "gray")
  high_contrast_image <- bw_image > threshold
  return(high_contrast_image)
}

# Function to emulate creative filters
creative_filter <- function(image, filter_matrix) {
  filtered_image <- filter2(image, filter_matrix)
  return(filtered_image)
}

# Function to change the balance of colors (keeping the luminosity)
change_color_balance <- function(image, red_factor = 1, green_factor = 1, blue_factor = 1) {
  balanced_image <- image
  balanced_image[,,1] <- balanced_image[,,1] * red_factor
  balanced_image[,,2] <- balanced_image[,,2] * green_factor
  balanced_image[,,3] <- balanced_image[,,3] * blue_factor
  return(balanced_image)
}

# Function to apply a convolution filter for lowering detail (blurring)
apply_blur_filter <- function(image, blur_matrix) {
  blurred_image <- filter2(image, blur_matrix)
  return(blurred_image)
}

# Function to apply a convolution filter for enhancing detail
apply_sharpen_filter <- function(image, sharpen_matrix) {
  sharpened_image <- filter2(image, sharpen_matrix)
  return(sharpened_image)
}

# Function to apply a convolution filter for detecting contour lines
detect_contours <- function(image, contour_matrix) {
  contour_image <- filter2(image, contour_matrix)
  return(contour_image)
}

# Function to resize an image with interpolation
resize_image <- function(image, new_width, new_height) {
  resized_image <- resize(image, w = new_width, h = new_height, interpolation = "bilinear")
  return(resized_image)
}

# Example Usage:
# img <- readImage("path/to/your/image.jpg")
# img_bw <- convert_to_bw(img)
# img_high_contrast_bw <- high_contrast_bw(img)
# img_filtered <- creative_filter(img, matrix(c(-1, -1, -1, -1, 8, -1, -1, -1, -1), nrow = 3))
# img_balanced <- change_color_balance(img, red_factor = 1.2, green_factor = 0.8, blue_factor = 0.9)
# img_blurred <- apply_blur_filter(img, matrix(1/9, nrow = 3, ncol = 3))
# img_sharpened <- apply_sharpen_filter(img, matrix(c(-1, -1, -1, -1, 9, -1, -1, -1, -1), nrow = 3))
# img_contours <- detect_contours(img, matrix(c(-1, -1, -1, -1, 8, -1, -1, -1, -1), nrow = 3))
# img_resized <- resize_image(img, new_width = 200, new_height = 150)
