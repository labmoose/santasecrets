# Create a list of all participants in the gift exchange; These will be the "Santas"
p <- c("Chris", "Carla", "Jim", "Pam", 
        "Leslie", "Ben", "Mitch", "Cameron", 
        "Louis", "Jessica", "Dwight", "Jeremy", 
        "Ray", "Andy")

# Create a vector "d" (draw) for the names of all partipants that aren't recipients in pre-fixed set: 
# "Chris" has a gift for "Carla", so "Carla" will be removed from the random sampling.
d <- c("Chris", "Jim", "Pam", "Leslie", "Ben", "Cameron", "Mitch", 
       "Louis", "Jessica", "Dwight", "Jeremy", "Ray", "Andy")
# Create a vector "so" (significant others) for the ID of which participants a Santa cannot buy for
so <- c(NA, "Chris", "Pam", "Jim", "Ben", "Leslie", "Cameron", "MItch", "Jessica", "Louis", NA, NA, NA, NA)
# Create a vector "f" (frenemies) for the IDs of which participants a Santa will not buy for
f <- c(NA, NA, "Dwight", NA, "Jeremy", NA, NA, NA, "Ray", NA, "Jim", "Leslie", "Louis", NA)
# Create a dataframe "dfx" (exchange) to line up Santas, SOs, and frenemies
dfx <- data_frame(p, so, f)
dfx
# Create a vector "buyfor" for randomly names from d to be placed in; Set the 1st result = "Carla", 
# as Chris is fixed to buy for Carla
buyfor <- c("Carla")
# Randomly sample the participant names without replacement from the "d" (draw) vector
sampled <- sample(d, 13, replace = FALSE)
# Add the sampled participants into the buyfor vector
buyfor <- c(buyfor, sampled)
# Add the buyfor vector to the dfx dataframe
dfx <- add_column(dfx, buyfor)
# Print dataframe dfx to see all categories are present: Participants (p), SOs (so), Frenemies (f), and Buy For (buyfor)
dfx
# Use logical checks to look for disallowed matches (Buyfor != p, so, or f)
# Logical T/F test for buyfor ID & ID numbers, return a FALSE result for disallowed matches
testID <- c(buyfor != p)
# Check the vector testID for FALSE (disallowed matches); Save result as new variable to use later (TID)
TID <- FALSE %in% testID
# Print the result of variable TID to check that this code is working
TID
# Logical T/F test for buyfor ID & SO ID numbers, return a FALSE result for disallowed matches
testso <- c(buyfor != so)
# Check the vector testso for FALSE (dissallowed matches); Save result as a new variable to use later (TSO)
TSO <- FALSE %in% testso
# Print the result of variable TSO to check that this code if working
TSO
# Logical T/F test for buyfor ID & frenemy ID numbers, return a FALSE result for disallowed matches
testf <- c(buyfor != f)
# Check the vector testf for FALSE (disallowed matches); Save result as new variable to use later (TF)
TF <- FALSE %in% testf
# Print the result of variable TSO to check that this code if working
TF
# Create a loop to check for disallowed matches
while((TID == TRUE) | (TSO == TRUE) | (TF == TRUE)) {
# If disallowed matches = TRUE, then reset to sample until there are no disallowed matches
  buyfor <- c("Carla")
  # Randomly sample the participant ID numbers without replacement from the "d" (draw) vector
  sampled <- sample(d, 13, replace = FALSE)
  # Add the sampled participants into the buyfor vector
  buyfor <- c(buyfor, sampled)
  # Add the buyfor vector to the dfx dataframe
  dfx <- data_frame(dfx, buyfor)
  # Use logical checks to look for disallowed matches (Buyfor != p, so, or f)
  # Logical T/F test for buyfor ID & ID numbers, return a FALSE result for disallowed matches
  testID <- c(buyfor != p)
  # Check the vector testID for FALSE (disallowed matches); Save result as new variable to use later (TID)
  TID <- FALSE %in% testID
  # Print the result of variable TID to check that this code is working
  TID
  # Logical T/F test for buyfor ID & SO ID numbers, return a FALSE result for disallowed matches
  testso <- c(buyfor != so)
  # Check the vector testso for FALSE (dissallowed matches); Save result as a new variable to use later (TSO)
  TSO <- FALSE %in% testso
  # Print the result of variable TSO to check that this code if working
  TSO
  # Logical T/F test for buyfor ID & frenemy ID numbers, return a FALSE result for disallowed matches
  testf <- c(buyfor != f)
  # Check the vector testf for FALSE (disallowed matches); Save result as new variable to use later (TF)
  TF <- FALSE %in% testf
  # Print the result of variable TSO to check that this code if working
  TF
}
# If there are disallowed matches in the testing above, print message to signal where adjustments are required
if(TID == TRUE) {
  print("DISALLOWED MATCH TID- LIST REQUIRES ADJUSTMENT")
  dftest <- data_frame(p, buyfor, buyfor != p, buyfor !=so, buyfor !=f)
  dftest
} else if (TSO == TRUE){
  print("DISALLOWED MATCH TSO- LIST REQUIRES ADJUSTMENT")
  dftest <- data_frame(p, buyfor, buyfor != p, buyfor !=so, buyfor !=f)
  dftest
} else if (TF == TRUE){
  print("DISALLOWED MATCH TF- LIST REQUIRES ADJUSTMENT")
  dftest <- data_frame(p, buyfor, buyfor != p, buyfor !=so, buyfor !=f)
  dftest
} else {
  # If all results = TRUE and all matches are allowed, then create a result dataframe:
  dfmatch <- data_frame(p, buyfor)
}
# Add a vector for a range of gift prices- let's assume $25 to $100
vprice <- c(25:100)
# Randomly select a price for each Santa to have paid for the gift purchased
paid <- sample(vprice, 14, replace = TRUE)
# Add the vector paid as an add'l column in dfmatch
dfmatch <- add_column(dfmatch, paid)
# Add a vector for a range of dates of purchase- let's assume from Dec. 1st- Dec. 5th
vdate <- c("12/01", "12/02", "12/03", "12/04", "12/05")
# Randomly select a date of purchase for each Santa's gift
ndate <- sample(vdate, 14, replace = TRUE)
# Make the character date into dates
date <- as.Date(ndate, format = "%m/%d")
# Add the vector date as an add'l column in dfmatch
dfmatch <- add_column(dfmatch, date)
# Add nice descriptive names for the columns so the data makes sense at a glance
colnames(dfmatch) <- c("Santa", "Recipient", "Spent", "Purchased")
# Print out the finalized dataframe
dfmatch

# Create a nice little bar graph how much each santa spent on their gift
ggplot(dfmatch, aes(x=Santa, y=Spent)) + geom_bar(stat = "identity") +
  geom_text(aes(label=Spent), vjust=-0.25) + ggtitle("Who's the Cheapskate?")

# Plot out how much was spent based on the date it was spent
ggplot(dfmatch, aes(x=Purchased, y=Spent)) + geom_point(color = "blue", size = 4) +
  geom_text(aes(label=Spent), hjust=-0.75) + ggtitle("Do Big Spenders Really Spend Last?")

# Create a graph of the amount spent on the gift each recipient received
ggplot(dfmatch, aes(x=Santa, y=Purchased)) + geom_point(color = "green4", size = 3) + theme_bw() +
  ggtitle("Now We Know Who's Been Too Busy Binging Netflix")