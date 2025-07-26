# Debug: Check current working directory and library paths
cat("Working directory:", getwd(), "\n")
cat("Library paths:", .libPaths(), "\n")

# Change to project root first
original_wd <- getwd()
setwd("../..")
cat("Changed to project root:", getwd(), "\n")

# Now source .Rprofile from project root
if (file.exists(".Rprofile")) {
  source(".Rprofile")
}

# Change back to dashboard directory
setwd(original_wd)

# Check library paths again
cat("Library paths after renv:", .libPaths(), "\n")

# Load and call the function
library(abmdash)
encrypt_dashboard()
