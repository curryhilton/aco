##################################################################
#
# TITLE: ACO Analytics
#
# DESCRIPTION: An R script to develop appointment demand models
#              for UNC Family Medicine and UNC Specialty Medicine
#              practices.
#
# AUTHOR: Curry W. Hilton
#         Riaz Hedayati
#
# LAST UPDATED: 1/16/2018 
#
##################################################################

##################################################################
#           Package requirements and custom functions            # 
##################################################################



##################################################################
#                     Data import procedure                      # 
##################################################################

aco_claims_2016 <- read.delim("~/Projects/ACO_Analytics/aco/2016_aco_claims.txt")
aco_claims_2017 <- read.delim("~/Projects/ACO_Analytics/aco/2017_aco_claims.txt")

