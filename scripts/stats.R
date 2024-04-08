# chi-square test of independence -----------------------------------------

# testbunrsf

# Create a contingency table
contingency_table_test <- table(testbunrsf$midden, testbunrsf$status)

# Perform chi-squared test
chi_squared_test <- chisq.test(contingency_table_test)

# Print the results
print(chi_squared_test)
