# ZZ_commits.R

# Method for committing lots of changes while staying under limits
library(gert)
library(dplyr)

s = gert::git_status() %>%
  # calculate file size in megabytes
  mutate(size = file.size(file) / 1e6) %>%
  # Exclude files over 100 MB
  filter(size < 100) %>%
  with(gert::git_add(file = file))

# Commit the changes with message
gert::git_commit_all("added /get/data/data.rds")

# Pull the changes
gert::git_pull()

# Push the changes
gert::git_push()


# Check for files over 100 MB
s = gert::git_status() %>%
  # calculate file size in megabytes
  mutate(size = file.size(file) / 1e6) %>%
  # find files over 100 MB
  filter(size > 100) 

