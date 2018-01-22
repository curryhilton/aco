##################################################################
#
# TITLE: ACO Analytics
#
# DESCRIPTION: A R script to manage and clean data
#
# AUTHOR: Curry W. Hilton
#         Riaz Hedayati
#
# LAST UPDATED: 1/22/2018 
#
##################################################################

##################################################################
#           Package requirements and custom functions            # 
##################################################################

require(plyr)

library(plyr)

##################################################################
#            Data import procedure and Cleansing                 # 
##################################################################

esrd <- read.delim("~/Projects/ACO_Analytics/aco/Input_Data/esrd.txt")
esrd$DV <- "1"
colnames(esrd) <- c("HIC", "DV")

part_a_16 <- read.delim("~/Projects/ACO_Analytics/aco/Input_Data/part_a_16.txt")
part_a_17 <- read.delim("~/Projects/ACO_Analytics/aco/Input_Data/part_a_17.txt")
part_b_16 <- read.delim("~/Projects/ACO_Analytics/aco/Input_Data/part_b_16.txt")
part_b_17 <- read.delim("~/Projects/ACO_Analytics/aco/Input_Data/part_b_17.txt")
part_d_16 <- read.delim("~/Projects/ACO_Analytics/aco/Input_Data/part_d_16.txt")
part_d_17 <- read.delim("~/Projects/ACO_Analytics/aco/Input_Data/part_d_17.txt")

part_a_16 <- part_a_16[, c(1:5, 7:9)]
part_a_17 <- part_a_17[, c(1:5, 7:9)]

part_b_16$Claim.Proc.Code <- "NA" 
part_b_16 <- part_b_16[, c(1, 2, 3, 4, 5, 6, 8, 7)]
part_b_17$Claim.Proc.Code <- "NA" 
part_b_17 <- part_b_17[, c(1, 2, 3, 4, 5, 6, 8, 7)]

part_d_16$Claim.Diag.Code <- "NA"
part_d_16$Claim.Proc.Code <- "NA"
part_d_16$Facility.Code <- "NA"
part_d_17$Claim.Diag.Code <- "NA"
part_d_17$Claim.Proc.Code <- "NA"
part_d_17$Facility.Code <- "NA"


part_a_16$Claim.From.Date <- as.Date(part_a_16$Claim.From.Date, format = "%m/%d/%y")
part_a_16$Claim.Through.Date <- as.Date(part_a_16$Claim.Through.Date, format = "%m/%d/%y")
part_a_17$Claim.From.Date <- as.Date(part_a_17$Claim.From.Date, format = "%m/%d/%y")
part_a_17$Claim.Through.Date <- as.Date(part_a_17$Claim.Through.Date, format = "%m/%d/%y")

part_b_16$Claim.From.Date <- as.Date(part_b_16$Claim.From.Date, format = "%m/%d/%y")
part_b_16$Claim.Thru.Date <- as.Date(part_b_16$Claim.Thru.Date, format = "%m/%d/%y")
part_b_17$Claim.From.Date <- as.Date(part_b_17$Claim.From.Date, format = "%m/%d/%y")
part_b_17$Claim.Thru.Date <- as.Date(part_b_17$Claim.Thru.Date, format = "%m/%d/%y")

part_d_16$Claim.From.Date <- as.Date(part_d_16$Claim.From.Date, format = "%m/%d/%y")
part_d_16$Claim.Thru.Date <- as.Date(part_d_16$Claim.Thru.Date, format = "%m/%d/%y")
part_d_17$Claim.From.Date <- as.Date(part_d_17$Claim.From.Date, format = "%m/%d/%y")
part_d_17$Claim.Thru.Date <- as.Date(part_d_17$Claim.Thru.Date, format = "%m/%d/%y")


colnames(part_a_16) <- c("HIC", "Claim.From.Date", "Claim.Through.Date", "Claim.Type.Code",
                      "Claim.Payment.Amount", "Claim.Diag.Code", "Claim.Proc.Code",
                      "Facility.Code")
colnames(part_a_17) <- c("HIC", "Claim.From.Date", "Claim.Through.Date", "Claim.Type.Code",
                         "Claim.Payment.Amount", "Claim.Diag.Code", "Claim.Proc.Code",
                         "Facility.Code")

colnames(part_b_16) <- c("HIC", "Claim.From.Date", "Claim.Through.Date", "Claim.Type.Code",
                         "Claim.Payment.Amount", "Claim.Diag.Code", "Claim.Proc.Code",
                         "Facility.Code")
colnames(part_b_17) <- c("HIC", "Claim.From.Date", "Claim.Through.Date", "Claim.Type.Code",
                         "Claim.Payment.Amount", "Claim.Diag.Code", "Claim.Proc.Code",
                         "Facility.Code")

colnames(part_d_16) <- c("HIC", "Claim.From.Date", "Claim.Through.Date", "Claim.Type.Code",
                         "Claim.Payment.Amount", "Claim.Diag.Code", "Claim.Proc.Code",
                         "Facility.Code")
colnames(part_d_17) <- c("HIC", "Claim.From.Date", "Claim.Through.Date", "Claim.Type.Code",
                         "Claim.Payment.Amount", "Claim.Diag.Code", "Claim.Proc.Code",
                         "Facility.Code")

part_a_16_agg <- aggregate(part_a_16$Claim.Payment.Amount, by = list(HIC = part_a_16$HIC),
                             FUN = sum)
part_a_16_agg$Part <- "A"
colnames(part_a_16_agg) <- c("HIC", "Yearly.Patient.Spend", "Part")

part_a_17_agg <- aggregate(part_a_17$Claim.Payment.Amount, by = list(HIC = part_a_17$HIC),
                           FUN = sum)
part_a_17_agg$Part <- "A"
colnames(part_a_17_agg) <- c("HIC", "Yearly.Patient.Spend", "Part")


part_b_16_agg <- aggregate(part_b_16$Claim.Payment.Amount, by = list(HIC = part_b_16$HIC),
                           FUN = sum)
part_b_16_agg$Part <- "B - P"
colnames(part_b_16_agg) <- c("HIC", "Yearly.Patient.Spend", "Part")

part_b_17_agg <- aggregate(part_b_17$Claim.Payment.Amount, by = list(HIC = part_b_17$HIC),
                           FUN = sum)
part_b_17_agg$Part <- "B - P"
colnames(part_b_17_agg) <- c("HIC", "Yearly.Patient.Spend", "Part")


part_d_16_agg <- aggregate(part_d_16$Claim.Payment.Amount, by = list(HIC = part_d_16$HIC),
                           FUN = sum)
part_d_16_agg$Part <- "B - DME"
colnames(part_d_16_agg) <- c("HIC", "Yearly.Patient.Spend", "Part")

part_d_17_agg <- aggregate(part_d_17$Claim.Payment.Amount, by = list(HIC = part_d_17$HIC),
                           FUN = sum)
part_d_17_agg$Part <- "B - DME"
colnames(part_d_17_agg) <- c("HIC", "Yearly.Patient.Spend", "Part")

agg_16_part <- rbind(part_a_16_agg, part_b_16_agg, part_d_16_agg)
agg_16_part_join <- join(agg_16_part, esrd, by = "HIC")
agg_16_part_sort <- agg_16_part_join[order(agg_16_part_join$HIC), ]

agg_17_part <- rbind(part_a_17_agg, part_b_17_agg, part_d_17_agg)
agg_17_part_join <- join(agg_17_part, esrd, by = "HIC")
agg_17_part_sort <- agg_17_part_join[order(agg_17_part_join$HIC), ]

write.csv(agg_16_part_sort, "~/Projects/ACO_Analytics/aco/Output_Data/agg_16.csv", row.names = F)
write.csv(agg_17_part_sort, "~/Projects/ACO_Analytics/aco/Output_Data/agg_17.csv", row.names = F)


