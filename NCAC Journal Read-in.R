
library(tidyverse)
library(pdftools)

### Read in conference pdf###
txt01 <- pdf_text("NC Conference Journal - 2018 (Alph records).pdf") %>%
  readr::read_lines()

### Remove unnecessary opener and column titles###
# Find where data actually begins and 
#create new vector with only this data
first_index <- min(which(grepl("^[[:space:]]+Appt", txt01)))
dat <- txt01[first_index:length(txt01)]

columns <- "^[[:space:]]+Appt|^Name[[:space:]]+Years|^[[:space:]]+Lp Appt"
header <- "Section VI: Pastoral and Diaconal Records|The North Carolina Annual Conference"
empty <- ""

# Remove columns, headers, empty rows
dat<- dat[!grepl(paste(columns, header, sep = "|"), dat)]
dat <- dat[!dat==empty]
# Dat is vector where each element is a row of the pdf #

### Function to build dataframe from each row in dat ###
getDat <- function(ele) {
  if (!grepl("^\\s",  ele)) {
    # Get the name
    name <- str_extract(ele,"^\\w+,\\s\\w+\\s\\w+|^\\w+,\\s\\w+")
    tdat <- data.frame(Name = substr(ele, 1, 45), Lp.Appt.Years = substr(ele, 46, 52), 
                       Pres.Relation = substr(ele,53,65), First.Admit = substr(ele, 65, 81),
                       When.Admit = substr(ele, 82, 94), Ord.Decon = substr(ele, 95, 105),
                       Ord.Elder.AND.Appt.NotInc.LP = substr(ele, 106, nchar(ele)))
    return(tdat)
  }
}

# Get temp dataset with only these (non-history) rows
temp <- dat
temp <- temp[!(grepl("^\\s", temp))]

# Apply getDat over each row in the pdf, store in frame
lst <- lapply(temp, getDat)
frame <- do.call("rbind", lst)

# Turn all factors into characters
frame <- data.frame(lapply(frame, as.character), stringsAsFactors=FALSE)

### Function to split the final two variables (Ord.elder and Appt.NotInc.LP)
# into two separate columns ###
splits <- function(x) {
  new_dat <- data.frame(Ord.Elder = NA, Appt.NotInc.LP = NA)
  if (x=="") {
    return(new_dat)
  }
  v <- unlist(str_split(x, "\\s+"))
  for (val in v) {
    if (nchar(val)==4) {
      new_dat[1,1]=val
    }
    else {
      new_dat[1,2]=val
    }
  }
  return(new_dat)
}

# Call splits over the correpsonding column in frame, combine them into a single dataframe, 
# then combine that with our earlier dataset, frame
frame2 <- cbind(frame, do.call("rbind", lapply(frame$Ord.Elder.AND.Appt.NotInc.LP, splits)))

# Convert numeric variables to type numeric (previously characters)
frame2$Lp.Appt.Years <- as.numeric(frame2$Lp.Appt.Years)
frame2$Appt.NotInc.LP <- as.numeric(frame2$Appt.NotInc.LP)

### Function to isolate the words from a character value with 
# a bunch of whitespace(s) ###
isolateStrings <- function(x) {
  if (grepl("^\\s+$|^\\s$",x)) {
    return(NA)
  }
  val <- NA
  wrds <- unlist(str_split(x, "\\s+"))
  for (el in wrds) {
    if (nchar(el)!=0) {
      val = el
    }
  }
  return(val)
}

# Isolate words in the remaining character variables
frame2$Pres.Relation <- sapply(frame2$Pres.Relation, isolateStrings)
frame2$First.Admit <- sapply(frame2$First.Admit, isolateStrings)
frame2$When.Admit <- sapply(frame2$When.Admit, isolateStrings)
frame2$Ord.Decon <- sapply(frame2$Ord.Decon, isolateStrings)

# Remove preceding/trailing whitespace from Name
remove_white <- function(x) gsub("^\\s+|\\s+$", "", x)
frame2$Name <- sapply(frame2$Name, remove_white)

# Get all indices for history rows
indices <- which(grepl("^\\s", dat))
### Recursive function to get connecting indices ###
find_near <- function(ind) {
  if ((ind == length(indices)) | (indices[ind+1]-indices[ind]> 1)) {
    return(c(indices[ind]))
  }
  return(c(indices[ind], find_near(ind+1)))
}
# Initialise list and loop through all indices
n <- 1
i <- 1
l <- list()
while (i <= length(indices)) {
  l[[n]] <- find_near(i)
  i <- length(find_near(i))+i
  n <- n+1
}

# Apply the paste function to each set of connected
# indices to form a vector of characters (strings, here)
l.vec <- sapply(l, function(ele) {
  return(paste(dat[ele], collapse = ""))
})

# Add as a column to the dataset
frame2$History <- l.vec
# Finalize data and write the csv
ar2018 <- frame2[!(names(frame2)%in% c("Ord.Elder.AND.Appt.NotInc.LP"))]
write.csv(ar2018, file = "Alphabetical Rolls (2018).csv")

