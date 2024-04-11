#load packages
source("scripts/00-packages.R")

#read in data
dat <- readRDS("output/ready_for_rsf.rds")

#figure used vs avail
dat$status <- factor(dat$status)
dat$midden <- factor(dat$midden)

ggplot(dat, aes(x = id, fill = midden)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ status) +
  labs(x = "ID", y = "Count", fill = "Midden") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(dat, aes(x = midden, y = , fill = midden)) +
  geom_bar(position = "dodge", color = "black") +
  facet_grid(status ~ .) +
  labs(title = "Box Plot of Midden Status by Point Status",
       x = "Midden", y = "Count") +
  theme_minimal()


# RSF ---------------------------------------------------------------------

rsf <- glm(status ~ midden, data = dat, family = "binomial")
summary(rsf)

RSF <- function(status, midden) {
  # make the model
  model <- glm(status ~ midden, family = "binomial")
  # transpose the coef of the model and cast as data.table
  coefOut <- data.table(t(coef(model)))
  # transpose the standard error coef of the model and cast as data.table
  secoefOut <- data.table(t(se.coef(model)))
  # extract r-squared from model
  rsqOut <- data.table(rsq(model))
  # label the column name for the rsqOut
  names(rsqOut)<-c("rsq")
  # add 'se-' prefix to standard error coef table
  setnames(secoefOut, paste0('se-', colnames(secoefOut)))
  # return combined columns
  return(data.table(coefOut, secoefOut, rsqOut))
}

output_id <- dat[, RSF(status, midden), by = id]
output_all <- dat[, RSF(status, midden)]

# plot --------------------------------------------------------------------

ggplot(output_id, aes(x = id, y = middenyes, color = id)) +
  geom_point() +
  geom_errorbar(aes(ymin = middenyes - `se-middenyes`, ymax = middenyes + `se-middenyes`), width = 0.2) +
  labs(x = "Midden", y = "Selection Coefficient") +
  geom_hline(yintercept = 0, linetype = "dotted") +  # Add a dotted line at y = 0
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#remove bun 22109
dat2 <- dat[id !=22109]

rsf2 <- glm(status ~ midden, data = dat2, family = "binomial")
summary(rsf2)

output_id2 <- dat2[, RSF(status, midden), by = id]
output_all2 <- dat2[, RSF(status, midden)]

ggplot(output_id2, aes(x = id, y = middenyes, color = id)) +
  geom_point() +
  geom_errorbar(aes(ymin = middenyes - `se-middenyes`, ymax = middenyes + `se-middenyes`), width = 0.2) +
  labs(x = "Midden", y = "Selection Coefficient") +
  geom_hline(yintercept = 0, linetype = "dotted") +  # Add a dotted line at y = 0
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave("output/rsf_results.jpeg", rsf_results, width = 6, height = 4, units = "in")