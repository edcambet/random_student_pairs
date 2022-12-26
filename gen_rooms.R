# Author: Eduardo Campillo Betancourt

# Generate unique random pairs for class assignments:

library(readxl)
library(glue)
library(readr)
library(tidyverse)

## -------------------------------------------------------------------------- ##
## Notes: This file creates a new set of student pairs for the week making    ##
##        sure that no student repeats a past partner. To run this you must   ##
##        have in the directory a file called "roster.xls" with a column      ##
##        named "Name" which has the names of all students. If students add   ##
##        or drop the class, update the "roster.xls" file and this code will  ##
##        add/delete the relevant names from the old adjacency matrices.      ##
##        The file also saves all past adjacency matrices and group assign-   ##
##        ments out of caution but these can be deleted (with the exception   ##
##        of last week's).                                                    ##
##                                                                            ##
##        Every time you run this file, make sure to update the date that a-  ##
##        pplies to the new week to create a new set of groups.               ##
## -------------------------------------------------------------------------- ##

# Set directory and ensure necessary folders exist:
setwd("/Users/edcambet/Downloads/gen_rooms")
if (!file.exists("rooms")){
  dir.create(file.path("rooms"))
}
if (!file.exists("stored_mats")){
  dir.create(file.path("stored_mats"))
}

# Set basic parameters
start_date <- "2023-01-09" # To calculate the number of the current week
date <- 20230109 # seed is the date of the Monday of class week, i.e., the first
                 # seed is "20230109" (update each week)
date_str <- as.character(date)
set.seed(date)
week <- as.integer(ceiling((as.Date(paste(substr(date_str, 1, 4), substr(date_str, 5, 6), substr(date_str, 7, 8), sep = "-")) - as.Date(start_date)) / 7 + 1))

# Check that you won't overwrite existing group assingments by accident:
stopifnot(!file.exists(glue("stored_mats/adj_mat_week{week}.Rdata"))) # Adj. matrix already exists
stopifnot(!file.exists(glue("rooms/week{week}_groups.csv"))) # Groups for this week already exist

# Load current class roster:
roster <- read_excel("roster.xls") %>%
    arrange(Name)
N <- nrow(roster) # Number of students currently in the class

# Load adjacency matrix up to the previous week or create empty matrix:
if (file.exists(glue("stored_mats/adj_mat_week{week - 1}.Rdata"))) {
  load(glue("stored_mats/adj_mat_week{week - 1}.Rdata"))
} else {
  print("Generating first adjacency matrix.")
  adj_mat <- matrix(0, nrow = N, ncol = N)
  colnames(adj_mat) <- roster$Name
  row.names(adj_mat) <- roster$Name
}

# Update last adjacency matrix in case students added and dropped:
oldnames <- colnames(adj_mat)
newnames <- roster$Name
dropnames <- setdiff(oldnames, newnames)
if (length(dropnames)>0) {
  print(glue("The following people dropped the class:\n"), dropnames)
}
addnames  <- setdiff(newnames, oldnames)
if (length(addnames)>0) {
  print(glue("The following people added the class:\n"), addnames)
}

adj_mat_dropped <- adj_mat[setdiff(colnames(adj_mat),dropnames), setdiff(colnames(adj_mat),dropnames)]
adj_mat_added_rows <- matrix(0, nrow = length(addnames), ncol = ncol(adj_mat_dropped))
adj_mat_added_cols <- matrix(0, ncol = length(addnames), nrow = nrow(adj_mat_dropped) + length(addnames))
adj_mat_new <- cbind(rbind(adj_mat_dropped, adj_mat_added_rows),adj_mat_added_cols)
colnames(adj_mat_new) <- c(colnames(adj_mat_dropped), addnames)
rownames(adj_mat_new) <- colnames(adj_mat_new)
adj_mat <- adj_mat_new[sort(rownames(adj_mat_new)), sort(colnames(adj_mat_new))]
rm(adj_mat_dropped, adj_mat_added_rows, adj_mat_added_cols, adj_mat_new, dropnames, addnames, newnames, oldnames)

# Check that new matrix matches new roster:
stopifnot(colnames(adj_mat)==roster$Name)

# Run loop until everyone has a new partner:
coinc <- 100
while (coinc != 0) {
    names <- roster$Name[sample(N, N)]
    names_mod <- names
    odd <- (N %% 2) != 0 # check if N is odd
    if (odd) names_mod <- c(names, "-") # add "dummy" student if N is odd
    rooms <- data.frame(room = paste("Group", rep(1:ceiling(N/2), each = 2)),
                        name = names_mod)
    if (odd) {
        rooms <- rooms[-nrow(rooms),] # drop dummy student
        rooms$room[nrow(rooms)] <- rooms$room[(nrow(rooms) - 1)] # assign lone student to last pair number
    }
    colnames(rooms) <- c("Group #", "Name")
    
    # Create adjacency matrix with new group draw:
    adj_mat_aux <- matrix(0, nrow = N, ncol = N)
    colnames(adj_mat_aux) <- roster$Name
    row.names(adj_mat_aux) <- roster$Name
    
    # entry_ij == 1 if same group and == 0 otherwise:
    for (i in 1:nrow(roster)) {
        group_i <- rooms$`Group #`[which(rooms$Name == colnames(adj_mat_aux)[i])]
        for (j in i:nrow(roster)) {
            group_j <- rooms$`Group #`[which(rooms$Name == colnames(adj_mat_aux)[j])]
            adj_mat_aux[i,j] <- ifelse(group_i == group_j, 1, 0)
            adj_mat_aux[j,i] <- adj_mat_aux[i,j]
            if (i == j) adj_mat_aux[i,j] <- 0
        }
    }
    
    # Check that there the new and old adjacency matrices are different in all 
    # entries, i.e., there are no repeated pairs:
    coinc <- sum(adj_mat == adj_mat_aux & adj_mat == 1)
}
print("DONE!")

# Save combined adjacency matrix to be uploaded next week as the old one:
adj_mat <- adj_mat + adj_mat_aux
rm(coinc, group_i, group_j, i, j, names, names_mod, odd, adj_mat_aux)
if (sum(adj_mat > 1) != 0) week <- "ERROR"
save(adj_mat, file = glue("stored_mats/adj_mat_week{week}.Rdata"))
print(glue("Saved new adjacency matrix in: \n"), glue("stored_mats/adj_mat_week{week}.Rdata"))
write.csv(rooms, glue("rooms/week{week}_groups.csv"), row.names = FALSE)
print(glue("Saved new room assignment in: \n"), glue("rooms/week{week}_groups.csv"))
