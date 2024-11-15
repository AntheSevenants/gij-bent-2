# That's so random!
set.seed(2204355)

# ElasticTools imports
# Make sure the git submodules have been pulled!
source("ElasticToolsR/Dataset.R")
source("ElasticToolsR/ElasticNet.R")
source("ElasticToolsR/MetaFile.R")

# Load the dataset
source("0-common.R")


# Create an ElasticTools dataset
ds <- dataset(df=df %>% no_unknown_gender(),
              response_variable_column="construction_type",
              to_binary_columns=c("user_id", "dialect", "gender"),
              other_columns=c("distance_from_antwerp", "is_reply"))

# Get the list of features
# In our case, this is the list of users
feature_list <- ds$as_feature_list()

# Elastic Net regression itself
net <- elastic_net(ds=ds)
gc()
output <- net$do_elastic_net_regression_auto_alpha(k=5)

output$results

# Get the lowest loss from the results
lowest_loss_row <- output$results[which.min(output$results$loss),]
lowest_loss_row

# Extract the coefficients from the model with the lowest loss
# Attach features to the coefficients
coefficients_with_labels <- net$attach_coefficients(
  output$fits[[lowest_loss_row[["X_id"]]]])

# Export
write.csv(coefficients_with_labels, "../output/gij_bent_coefficients.csv",
          row.names=FALSE)

#
# Export model information
#

model_meta <- meta.file()

for (attribute in colnames(lowest_loss_row)) {
  if (attribute == "X_id") {
    next
  }
  
  model_meta$add_model_information(attribute, lowest_loss_row[[attribute]])
}

write.csv(model_meta$as.data.frame(), "../output/model_meta.csv", row.names=FALSE)
