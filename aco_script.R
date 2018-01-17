##################################################################
#
# TITLE: ACO Analytics
#
# DESCRIPTION: A R script to evaluate the YOY performance and overages
#              based on procedural variation
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

require(plyr)

library(plyr)

##################################################################
#            Data import procedure and Cleansing                 # 
##################################################################

part_a <- read.delim("~/Projects/ACO_Analytics/aco/part_a.txt")
part_b <- read.delim("~/Projects/ACO_Analytics/aco/part_b.txt")

part_a <- part_a[, 1:7]
part_b <- part_b[, 1:7]

part_a$type <- "A"
part_b$type <- "B"

part_a$Claim.Bill.Facility.Type.Code <- as.character(part_a$Claim.Bill.Facility.Type.Code)
part_b$Rendering.Provider.Type.Code <- as.character(part_b$Rendering.Provider.Type.Code)

provider_type_a <- data.frame(Claim.Bill.Facility.Type.Code = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), 
                              prov = c("Hospital", "SNF", "HHA", "Religious Non-Medical (Hospital)",
                                       "Religious Non-Medical (Extended)", "Intermediate Care", "Clinic
                                       or Hospital based renal dialysis facility", "Ambulatory Surgical
                                       Center or Specialty Facility", "Reserved"))

provider_type_b <- data.frame(Rendering.Provider.Type.Code = c("0", "1", "2", "3", "4", "5", "6", "7", "UI", "N2", "D", "BP",
                                       "BG", "A"), prov = c("Clinics, groups, associations", "Physicians
                                                            or suppliers reporting as solo", "Suppliers(other
                                                            than sole prop.)", "Institutional
                                                            provider", "Independent lab", "Clinics (Mult.Spec)",
                                                            "Groups (Single Spec)", "Other Ent.", "UPIN Id.",
                                                            "National Council for Prescription Drug Programs",
                                                            "National Supplier Clearinghouse", "PIN Individual",
                                                            "PIN Group", "Online Survey, Certification, and 
                                                            Reporting"))

part_a <- join(part_a, provider_type_a, by = "Claim.Bill.Facility.Type.Code")
part_b <- join(part_b, provider_type_b, by = "Rendering.Provider.Type.Code")

colnames(part_a) <- c("HIC", "Claim.From.Date", "Claim.Through.Date", "Claim.Type.Code",
                      "Claim.Payment.Amount","Provider.Code", "Diagnosis.Code", "Type", "Provider.Type")
colnames(part_b) <- c("HIC", "Claim.From.Date", "Claim.Through.Date", "Claim.Type.Code",
                      "Claim.Payment.Amount","Provider.Code", "Diagnosis.Code", "Type", "Provider.Type")

part_a$Claim.From.Date <- as.Date(part_a$Claim.From.Date, format = "%m/%d/%y")
part_a$Claim.Through.Date <- as.Date(part_a$Claim.Through.Date, format = "%m/%d/%y")

part_b$Claim.From.Date <- as.Date(part_b$Claim.From.Date, format = "%m/%d/%y")
part_b$Claim.Through.Date <- as.Date(part_b$Claim.Through.Date, format = "%m/%d/%y")

start_2016 <- as.Date("01/01/2016", "%m/%d/%Y")
end_2016 <- as.Date("12/31/2016", "%m/%d/%Y")

start_2017 <- as.Date("01/01/2017", "%m/%d/%Y")
end_2017 <- as.Date("12/31/2017", "%m/%d/%Y")

part_a_2016 <- subset(part_a, part_a$Claim.Through.Date >= start_2016 & 
                        part_a$Claim.Through.Date <= end_2016)
part_a_2017 <- subset(part_a, part_a$Claim.Through.Date >= start_2017 & 
                        part_a$Claim.Through.Date <= end_2017)

part_b_2016 <- subset(part_b, part_b$Claim.Through.Date >= start_2016 & 
                        part_b$Claim.Through.Date <= end_2016)
part_b_2017 <- subset(part_b, part_b$Claim.Through.Date >= start_2017 & 
                        part_b$Claim.Through.Date <= end_2017)

part_a_2016_agg <- aggregate(part_a_2016$Claim.Payment.Amount, by = list(HIC = part_a_2016$HIC),
                                FUN = sum)
part_a_2016_agg$Part <- "A"
colnames(part_a_2016_agg) <- c("HIC", "Yearly.Patient.Spend", "Part")

part_b_2016_agg <- aggregate(part_b_2016$Claim.Payment.Amount, by = list(HIC = part_b_2016$HIC),
                             FUN = sum)
part_b_2016_agg$Part <- "B"
colnames(part_a_2016_agg) <- c("HIC", "Yearly.Patient.Spend", "Part")

part_a_2017_agg <- aggregate(part_a_2017$Claim.Payment.Amount, by = list(HIC = part_a_2017$HIC),
                             FUN = sum)
part_a_2017_agg$Part <- "A"
colnames(part_a_2017_agg) <- c("HIC", "Yearly.Patient.Spend", "Part")

part_b_2017_agg <- aggregate(part_b_2017$Claim.Payment.Amount, by = list(HIC = part_b_2017$HIC),
                             FUN = sum)
part_b_2017_agg$Part <- "B"
colnames(part_a_2017_agg) <- c("HIC", "Yearly.Patient.Spend", "Part")

dat.2016 <- rbind(part_a_2016, part_b_2016)
dat.2017 <- rbind(part_a_2017, part_b_2017)

agg_2016 <- aggregate(dat.2016$Claim.Payment.Amount, by = list(HIC = dat.2016$HIC),
                      FUN = sum)
colnames(agg_2016) <- c("HIC", "Yearly.Patient.Spend")

agg_2017 <- aggregate(dat.2017$Claim.Payment.Amount, by = list(HIC = dat.2017$HIC),
                      FUN = sum)
colnames(agg_2017) <- c("HIC", "Yearly.Patient.Spend")