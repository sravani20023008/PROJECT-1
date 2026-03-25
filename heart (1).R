# ============================================================================
# Project 2 Milestone 1: Association Rule Mining
# ============================================================================

# Install and load the necessary package for association rule mining
install.packages("arules")
library(arules)

# ============================================================================
# Task 1(b): Full Set of Pre-processing Operations
# ============================================================================

# --- Step 1: Load the Dataset from a Local CSV File ---
# The 'header = TRUE' argument tells R that the first row contains column names.
file_path <- "heart.csv"
heart_data <- read.csv(file_path, header = TRUE, na.strings = "?")

# --- Step 2: Select the Required Features ---
# The project specifies using only 8 features for mining.
features_to_keep <- c("sex", "cp", "fbs", "age", "trestbps",
                      "chol", "thal", "oldpeak")
heart_data_selected <- heart_data[, features_to_keep]


# --- Step 3: Handle Missing Values ---
# Inspect for missing values (which were loaded as NA) and remove any rows containing them.
cat("Number of rows before removing missing values:", nrow(heart_data_selected), "\n")
heart_data_clean <- na.omit(heart_data_selected)
cat("Number of rows after removing missing values:", nrow(heart_data_clean), "\n")


# --- Step 4: Pre-process Categorical Features ---
# Convert numeric codes into meaningful categorical factors

# Sex: 1 = male; 0 = female
heart_data_clean$sex <- factor(heart_data_clean$sex,
                               levels = c(0, 1),
                               labels = c("Sex=Female", "Sex=Male"))

# Chest Pain Type (cp): 1 = typical angina; 2 = atypical angina; 3 = non-anginal pain; 4 = asymptomatic
heart_data_clean$cp <- factor(heart_data_clean$cp,
                              levels = c(1, 2, 3, 4),
                              labels = c("CP=TypicalAngina", "CP=AtypicalAngina",
                                         "CP=NonAnginal", "CP=Asymptomatic"))

# Fasting Blood Sugar (fbs): 1 = > 120 mg/dl (True); 0 = <= 120 mg/dl (False)
heart_data_clean$fbs <- factor(heart_data_clean$fbs,
                               levels = c(0, 1),
                               labels = c("FBS<=120", "FBS>120"))

# Thalassemia (thal): 3 = normal; 6 = fixed defect; 7 = reversable defect
# Note: The original dataset might contain values other than 3, 6, 7. We only factor these.
heart_data_clean$thal <- factor(heart_data_clean$thal,
                                levels = c(3.0, 6.0, 7.0),
                                labels = c("Thal=Normal", "Thal=FixedDefect", "Thal=ReversableDefect"))


# --- Step 5: Discretize (Bin) Numeric Features ---
# Convert continuous numeric features into categorical bins.

# Age: Binning into 3 groups
heart_data_clean$age <- cut(heart_data_clean$age,
                            breaks = c(0, 45, 60, Inf),
                            labels = c("Age=Young", "Age=Middle-Aged", "Age=Senior"),
                            right = FALSE)

# Resting Blood Pressure (trestbps): Binning into 3 groups
heart_data_clean$trestbps <- cut(heart_data_clean$trestbps,
                                 breaks = c(0, 120, 140, Inf),
                                 labels = c("RestingBPS=Normal", "RestingBPS=Elevated", "RestingBPS=High"),
                                 right = FALSE)

# Cholesterol (chol): Binning into 3 groups
heart_data_clean$chol <- cut(heart_data_clean$chol,
                             breaks = c(0, 200, 240, Inf),
                             labels = c("Chol=Normal", "Chol=Borderline", "Chol=High"),
                             right = FALSE)

# Oldpeak (ST depression): Binning into 3 groups
heart_data_clean$oldpeak <- cut(heart_data_clean$oldpeak,
                                breaks = c(-Inf, 1, 2.5, Inf),
                                labels = c("Oldpeak=Low", "Oldpeak=Medium", "Oldpeak=High"),
                                right = FALSE)


# --- Step 6: Create Transactions Object ---
# The data is now fully pre-processed. Convert it into a 'transactions' object.
heart_transactions <- as(heart_data_clean, "transactions")

# Display a summary and inspect the first few transactions.
cat("\n--- Summary of Transactions ---\n")
summary(heart_transactions)
cat("\n--- First 5 Transactions ---\n")
inspect(heart_transactions[1:5])


# ============================================================================
# Task 2(a): R Code for Rule Generation
# ============================================================================

# Generate association rules using the Apriori algorithm.
# Confidence is set to 0.9.
# A support of 0.1 is chosen to balance rule quantity and quality.
# Minlen=2 avoids trivial rules (e.g., {} => {item}).
rules <- apriori(heart_transactions,
                 parameter = list(supp = 0.1, conf = 0.9, minlen = 2))

# Sort the generated rules by 'lift' in descending order to find the most interesting ones.
rules_sorted_by_lift <- sort(rules, by = "lift", decreasing = TRUE)

# --- Display the Top 20 Rules ---
# This output is for Task 2(b) and should be presented in the PDF document.
cat("\n--- Top 20 Association Rules (sorted by Lift) ---\n")
inspect(rules_sorted_by_lift[1:20])


# ============================================================================
# Task 3(a): Code to Identify and Remove Redundant Rules
# ============================================================================

# --- Step 1: Identify Redundant Rules ---
# The is.redundant() function finds rules that are more specific versions
# of other rules without offering a gain in confidence.
redundant_rules_indices <- is.redundant(rules_sorted_by_lift)

# Display a summary of how many rules were found to be redundant.
cat("\n--- Checking for Redundant Rules ---\n")
print(summary(redundant_rules_indices))

# --- Step 2: Extract and Display the Redundant Rules ---
# These are the rules to be presented and explained in the PDF for Task 3(b).
redundant_rules <- rules_sorted_by_lift[redundant_rules_indices]

cat("\n--- Redundant Rules Identified ---\n")
if (length(redundant_rules) > 0) {
  inspect(redundant_rules)
} else {
  cat("No redundant rules were found with the current parameters.\n")
}

# --- Step 3: Remove Redundant Rules ---
# Create the final, concise set of non-redundant rules by filtering out the redundant ones.
non_redundant_rules <- rules_sorted_by_lift[!redundant_rules_indices]

# --- Display the Final Set of Non-Redundant Rules ---
cat("\n--- Final Set of Non-Redundant Rules ---\n")
inspect(sort(non_redundant_rules, by = "lift", decreasing = TRUE))