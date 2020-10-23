
#############################################################################################

# Quantitative Methods: Session 1
# Elias Nosrati
# October 2020

#############################################################################################

# Clear environment, set working directory
rm(list = ls())
setwd("/Users/Elias/Documents/.../Problem sheets/PS1")

# Load tidyverse packages
library(tidyverse)

########################################
## A1: Construct die and sample space ##
########################################

die <- 1:6
S <- expand.grid(die, die, die) 

#############################
## A2: Create new variable ##
#############################

S <- S %>%
		mutate(Value = Var1 + Var2 + Var3)

#################################################
# A3: P(A) = # elements in A / # elements in S ##
#################################################

sum(S$Value == 12) / nrow(S)

##################################
## A4: What if dice are biased? ##
##################################

Prob <- c("1" = 1/8, "2" = 1/8, "3" = 1/8, 
          "4" = 1/8, "5" = 1/8, "6" = 3/8)

# Assign individual and joint probabilities
S$Prob1 <- Prob[S$Var1]
S$Prob2 <- Prob[S$Var2]
S$Prob3 <- Prob[S$Var3]
S$Prob_joint <- S$Prob1 * S$Prob2 * S$Prob3

###################################################
## A5: Extract event A and calculate probability ##
###################################################

A <- subset(S, Value == 12)
sum(A$Prob_joint)

########################
## B1: Load data sets ##
########################

kenya <- read_csv("kenya.csv") # See Imai (2017), Table 1.4, page 29.
sweden <- read_csv("sweden.csv")
world <- read_csv("world.csv")

#################
## B2: Inspect ##
#################

summary(kenya)
summary(sweden)
summary(world)

head(kenya)
head(sweden)
head(world)

tail(kenya)
tail(sweden)
tail(world)

print(kenya, n = 30)
print(sweden, n = 30)
print(world, n = 30)

###############################################
## B3: Calculate age-specific fertility rate ##
###############################################

asfr <- function(data) {
	data %>%
		mutate(
			asfr = births / py.women) %>%
		select(period, age, asfr) %>%
		data.frame() # Convert tibble to data frame
}

asfr(kenya)
asfr(sweden)
asfr(world)

########################################
## B4: Calculate total fertility rate ##
########################################

tfr <- function(data) {
	out <- asfr(data) 
	out %>%
		group_by(period) %>%
		summarise(
			tfr = 5 * sum(asfr))
}

tfr(kenya)
tfr(sweden)
tfr(world)

###########################################
## B5: Calculate age-specific death rate ##
###########################################

asdr <- function(data) {
	data %>%
		mutate(
			asdr = 1000 * deaths / (py.men + py.women)) %>%
		select(period, age, asdr) %>%
		data.frame() # Convert tibble to data frame
}

asdr(kenya)
asdr(sweden)
asdr(world)

###########################
## B6: Visualise results ##
###########################

# Collect ASFR and ASDR for each unit
ken <- left_join(asfr(kenya), asdr(kenya))
swe <- left_join(asfr(sweden), asdr(sweden))
wor <- left_join(asfr(world), asdr(world))

# Create one data frame with all results
df <- rbind(ken, swe, wor)
df$country <- c(rep("Kenya", 30), rep("Sweden", 30), rep("World", 30))

# Transform age groups to ordered factor
df$age <- factor(df$age, 
	levels = c("0-4", "5-9", "10-14", 
				"15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
				"50-54", "55-59", "60-69", "70-79", "80+"))

# Age groups for reproductive age range
age_groups <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")

# Visualise ASFR
g1 <- ggplot(subset(df, age %in% age_groups), # Limit age range
				aes(age, 1000 * asfr, fill = country)) + # Modify rate and colour code by country
			geom_col() + # Show as columns
			labs(x = "Age", y = "Age-specific fertility rate per 1000 women") + # Axis labels
			scale_fill_viridis_d() + # Choose a nice colour palette
			facet_grid(country ~ period) + # Stratify plot by country and period
			theme_bw() + # Remove redundant lines
			theme(
				legend.position = "none",
				axis.text.x = element_text(angle = 90)) # Remove legend and turn axis text to avoid cluttering

# Visualise ASDR
g2 <- ggplot(df, aes(age, asdr, fill = country)) + # Colour code by country
			geom_col() + # Show as columns
			scale_y_continuous(breaks = seq(0, 200, 50)) + # Y-axis scale
			labs(x = "Age", y = "Age-specific death rate per 1000 population") + # Axis labels
			scale_fill_viridis_d(option = "plasma") + # Choose a nice colour palette
			facet_grid(country ~ period) + # Stratify plot by country and period
			theme_bw() + # Remove redundant lines
			theme(
				legend.position = "none",
				axis.text.x = element_text(angle = 90)) # Remove legend and turn axis text to avoid cluttering
							
# Save images
ggsave("asfr.jpg", g1)
ggsave("asdr.jpg", g2)

#############################################################################################
