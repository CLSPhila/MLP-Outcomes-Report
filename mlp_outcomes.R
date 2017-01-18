# CLS MLP Master Spreadsheet 
# This was originally taken from a complicated spreadsheet created by Michael Bowen, CLS's then director of IT.  
# This has been translated into R for readability, updatability, etc...
# by Michael Hollander and Nate Vogel.


library(ggplot2)

# First, a long section trying to figure out what the hell the excel
# spreadsheet does
# The spreadsheet basically pulls from legal server case and outreach reports, the following information
# * Number of cases opened in a time period and hours spent on those cases
# * Number of cases involved in securing health insurance or health services
# * Number of consultations in a time period and hours spent on those consultations
# * Number of Consultation about medical assistance/health insurance
# 
# *** Trainings
# * Number of trainings provided to PHMC staff
# * Total Attendance at those trainings
# 
# *** Language Access
# * Language breakdown for the cases handled (english vs non, total languages served, and then a pie chart with all languages accounted for)
# * Geography (how many zip codes served)
# * Pie chart with number of cases from each zip
# 
# *** Demographics
# * Gender breakdown
# * Age breakdown (>/< 18)
# ** Cases where client was < 18; 
# ** number of total children served in household of client (regardless of client's age')
# 
# *** Financial benefits
# * this seems to be the SROI calculations; 
# * notably, it excludes value for things that are hard to count, like preventing water shutoff.  Consults also have no outcomes recorded.  Some cases have multiple outcomes and others have none.
# 
# *** Case breakdowns
# * Cases are broken down as: health insurance, health services, other public benefits, utilities
# * Outcomes are broekn down as: medical issues, other public benefits, utilities, other financial benefits
# * consultations broken down as: medical assistance/health insurance, housing, social security/SSI, utilities

# start by reading in the two CSV reports from LS that we need to complete this task.  
# from the LS report MLP Cases Report
# export as CSV, rather than excel format


readLSReport <- function (lsFile ) {
  lsReport <- read.csv(lsFile, na.strings=c(""," ", "NA"))
  return(lsReport)
}

# from the LS report MLP Cases Report
# export as CSV, rather than excel format
readCasesFile <- function (lsFile = "C:/Users/Michael/Dropbox/work/state_of_unit/MLP_Cases_Report.csv") {
  mlpCases <- readLSReport(lsFile)
  return (mlpCases)
}

# from the LS report MLP Outreaches Report
# export as CSV, rather than excel format
readOutreachesFile <- function (lsFile = "C:/Users/Michael/Dropbox/work/state_of_unit/MLP_Outreach_Report__includes_trainings_.csv") {
  mlpOutreaches <- readLSReport(lsFile)
  return (mlpOutreaches)
}

getCaseStats <- function(mlpCases) {
  caseStats <- list()
  
  # MGH NOTE: The number below seems low - Bowen found 119 cases in Sept? 2016, so why only 7 more 4 months later?
  caseStats$totalCases <- length(unique(mlpCases$Matter.Case.ID.))

  # MGH Note: I get one less 511 case than Bowen did in his count.  Why?
  mlpCasesByLPC <- aggregate(Matter.Case.ID. ~ Legal.Problem.Code, mlpCases, FUN= function(x) length(unique(x)))
  mlpCasesByLPC <- setNames(mlpCasesByLPC, c("LPC", "Count"))
  medicalBenefitLPC = c("511 Medicaid-Seeking to Obtain or Retain Health Insurance",
                        "512 Medicaid-Seeking to Obtain or Retain Health Services")
  
  #  MLP report cases with 511 or 512 LPC
  caseStats$HealthInsServCases <- sum(mlpCasesByLPC$Count[mlpCasesByLPC$LPC %in% medicalBenefitLPC])

  #  the total time spent on the cases.  Note: unique is needed b/c cases are repeated if they have multiple outcomes
  mlpCasesTime <- aggregate(Total.Time.For.Case ~ Matter.Case.ID., mlpCases, FUN=function(x) sum(unique(x)))
  caseStats$TotalTime <- sum(mlpCasesTime$Total.Time.For.Case)

  
  ########## Language
  mlpCasesLanguage <- aggregate(Matter.Case.ID. ~ Language, mlpCases, FUN=function(x) length(unique(x)))
  mlpCasesLanguage <- setNames(mlpCasesLanguage, c("Language", "Count"))
  caseStats$languageEnglish <-  mlpCasesLanguage$Count[mlpCasesLanguage$Language=="English"]
  caseStats$languageNonEnglish <-  sum(mlpCasesLanguage$Count) - caseStats$languageEnglish
  caseStats$languageTotal <- NROW(mlpCasesLanguage)

  
  #### NOTE PIE CHART NOT WORKING WELL - Improve in the future!
  y.breaks <- cumsum(mlpCasesLanguage$Count) - .5 * mlpCasesLanguage$Count
  
  caseStats$languageGraph <- ggplot(mlpCasesLanguage, aes(x=1, y=Count, fill=Language)) + 
      geom_bar(stat = "identity", color="black") +
      guides(fill=guide_legend(override.aes=list(colour=NA))) +
      coord_polar(theta="y") +
      theme_minimal() +
      theme(axis.ticks=element_blank(),  # the axis ticks
            axis.title=element_blank(),  # the axis labels
            axis.text.y=element_blank(), 
            axis.text.x=element_blank(),
            plot.title = element_text(face="bold", size=20, hjust=.5)) +
      ggtitle("Languages Served")
  
  
  
  #######  Geography
  mlpCasesZip <- aggregate(Matter.Case.ID. ~ Zip.Code, mlpCases[mlpCases$Zip.Code > 19099 & mlpCases$Zip.Code < 19154,], FUN=function(x) length(unique(x)))
  mlpCasesZip <- setNames(mlpCasesZip, c("Zip", "Count"))
  caseStats$zipTotal <-  NROW(mlpCasesZip)
  
  caseStats$zipGraph <- ggplot(mlpCasesZip, aes(x=Zip, y=Count)) + 
      geom_bar(stat = "identity", color="black") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
            plot.title = element_text(face="bold", size=20, hjust=.5)) +
      scale_x_continuous(breaks=c(19100:19154)) +
      ggtitle("Zip COdes Served")
  
  
  ## Gender
  mlpCasesGender <- aggregate(Matter.Case.ID. ~ Gender, mlpCases, FUN=function(x) length(unique(x)))
  mlpCasesGender <- setNames(mlpCasesGender, c("Gender", "Count"))
  caseStats$GenderFemale <- mlpCasesGender$Count[mlpCasesGender$Gender=="F"]
  caseStats$GenderMale <-  mlpCasesGender$Count[mlpCasesGender$Gender=="M"]
  
  
  ## Children
  mlpCasesChildren <- aggregate(Matter.Case.ID. ~ Number.of.People.under.18, mlpCases, FUN=function(x) length(unique(x)))
  mlpCasesChildren <- setNames(mlpCasesChildren, c("NumberOfChildren", "Count"))
  caseStats$ChildrenCaseTotal <- sum(mlpCasesChildren$Count[mlpCasesChildren$NumberOfChildren != "0"])
  
  # sum the chidren in each unique case ID
  caseStats$ChildrenTotalChildrenServed <- sum(mlpCases$Number.of.People.under.18[unique(mlpCases$Matter.Case.ID.)])
  
  
  
  ############## Case Type and Outcome TYpes
  lpcHealthInsurance = c("511 Medicaid-Seeking to Obtain or Retain Health Insurance",
                         "521 Medicare-Seeking to Obtain or Retain Health Insurance",
                         "591 Other Health Insurance-Seeking to Obtain or Retain Health Insurance",
                         "599 Health, Other")
  
  lpcHealthServices = c("512 Medicaid-Seeking to Obtain or Retain Health Services")
  
  lpcOtherPublicBenefits = c("713 Welfare, LIHEAP",
                             "719 Welfare, Other",
                             "731 Food Stamps",
                             "760 Unemployment Compensation")
  
  lpcUtilities = c("74 Public Utilities, Low-income Program (e.g.CAP, LIURP)",
                   "75 Public Utilities, Metering/wiring",
                   "78 Public Utilities, Shut-Off/Disconnects",
                   "79 Public Utilities, Amount of Charge")
  
  lpcMisc = c("818 Immigration or Naturalization")
  
  
  caseStats$typeHealthInsurance <- sum(mlpCasesByLPC$Count[mlpCasesByLPC$LPC %in% lpcHealthInsurance])
  caseStats$typeHealthServices <- sum(mlpCasesByLPC$Count[mlpCasesByLPC$LPC %in% lpcHealthServices])
  caseStats$typeOtherPB <- sum(mlpCasesByLPC$Count[mlpCasesByLPC$LPC %in% lpcOtherPublicBenefits])
  caseStats$typeUtilities <- sum(mlpCasesByLPC$Count[mlpCasesByLPC$LPC %in% lpcUtilities])
  caseStats$typeImmigration <- sum(mlpCasesByLPC$Count[mlpCasesByLPC$LPC %in% lpcMisc])
  
  
  ############### Types of OUtcomes
  outcomesMedicalIssues = c("Marketplace coverage obtained",
                            "Medical bills avoided",
                            "Welfare: Medicaid obtained, Adult",
                            "Welfare: Medicaid obtained, Child",
                            "Welfare: Medicaid obtained, Disabled",
                            "Welfare: Medicaid obtained, Elderly")
  
  outcomesOtherPublicBenefits = c("Welfare: Benefits, lump sum payment",
                                  "Welfare: Monthly payment, FS")
  
  outcomesUtilities = c("Energy: Negotiated affordable payment",
                        "Energy: Prevented utility shutoff",
                        "Energy: Restored utility service")
  
  outcomesOtherFinancialBenefits = c("Lump Sum Avoidance",
                                     "Lump Sum Recovery",
                                     "Monthly Avoidance",
                                     "Monthly Recovery")
  
  caseStats$outcomeMedical <- NROW(mlpCases[mlpCases$Outcome %in% outcomesMedicalIssues,])
  caseStats$outcomeOtherPB <- NROW(mlpCases[mlpCases$Outcome %in% outcomesOtherPublicBenefits,])
  caseStats$outcomeUtilities <- NROW(mlpCases[mlpCases$Outcome %in% outcomesUtilities,])
  caseStats$outcomeOtherFinancial <- NROW(mlpCases[mlpCases$Outcome %in% outcomesOtherFinancialBenefits,])
  
  
    
  return (caseStats)
}

getOutreachStats <- function(mlpOutreaches) {
  outreachStats <- list()
  
  #Consults
  outreachStats$totalOutreaches <- length(mlpOutreaches$ID[mlpOutreaches$Presentation.Types == "Consulting"])
  outreachStats$totalOutreaches <- outreachStats$totalOutreaches + 353 # 353 is static number from MBOWEN spreadsheet based on historic data

  outreachStats$mlpOutreachesTime <- sum(mlpOutreaches$Time.Spent[mlpOutreaches$Presentation.Types=="Consulting"])
  outreachStats$mlpOutreachesTime <- outreachStats$mlpOutreachesTime + 113 # 113 is a static number from MBOWEN spreadsheet based on historic data

  outreachStats$totalMedicalHealthOutreaches <- length(mlpOutreaches$ID[mlpOutreaches$Subject=="Medical Assistance/Health Insurance"])
  outreachStats$totalMedicalHealthOutreaches <- outreachStats$totalMedicalHealthOutreaches + 251 # 251 is a static number from MBOWEN based on historic data
  
  outreachStats$totalSSIOutreaches <- length(mlpOutreaches$ID[mlpOutreaches$Subject=="SSI"])
  outreachStats$totalSSIOutreaches  <- outreachStats$totalSSIOutreaches  + 22 # 22 is a static number from MBOWEN based on historic data
  
  outreachStats$totalHousingOutreaches <- length(mlpOutreaches$ID[mlpOutreaches$Subject=="Landlord Tenant"])
  outreachStats$totalHousingOutreaches  <- outreachStats$totalHousingOutreaches  + 18 # 18 is a static number from MBOWEN based on historic data
  
  outreachStats$totalUtilitiesOutreaches <- length(mlpOutreaches$ID[mlpOutreaches$Subject=="Utilities"])
  outreachStats$totalUtilitiesOutreaches  <- outreachStats$totalUtilitiesOutreaches  + 14 # 14 is a static number from MBOWEN based on historic data
  
  
  ##################  Trainings
  outreachStats$totalTrainings <- length(mlpOutreaches$ID[mlpOutreaches$Presentation.Types == "Training - Non-Legal Professionals"])
  outreachStats$totalTrainingTime <- sum(mlpOutreaches$Time.Spent[mlpOutreaches$Presentation.Types == "Training - Non-Legal Professionals"])
  outreachStats$totalTrainingAttendees <- sum(mlpOutreaches$Number.of.Attendees..Actual.[mlpOutreaches$Presentation.Types == "Training - Non-Legal Professionals"])

  outreachStats$totalTrainings <- length(mlpOutreaches$ID[mlpOutreaches$Presentation.Types == "Training - Non-Legal Professionals"])
  outreachStats$totalTrainingTime <- sum(mlpOutreaches$Time.Spent[mlpOutreaches$Presentation.Types == "Training - Non-Legal Professionals"])
  outreachStats$totalTrainingAttendees <- sum(mlpOutreaches$Number.of.Attendees..Actual.[mlpOutreaches$Presentation.Types == "Training - Non-Legal Professionals"])
    
  return(outreachStats)
}

createReport <- function(caseStats, outreachStats)
{
  mlpReport <- data.frame(Field = "Total Cases", Value = caseStats$totalCases, stringsAsFactors=FALSE)
  
  #  MLP report cases with 511 or 512 LPC
  mlpReport <- rbind(mlpReport, c("Cases involving securing health insurance or health services", caseStats$HealthInsServCases))
  
  
  #  the total time spent on the cases.  
  mlpReport <- rbind(mlpReport, c(paste("Hours spent on these", caseStats$totalCases, "cases"), caseStats$totalTime))
  
  
  ################ Consultations
  
  mlpReport <- rbind(mlpReport, c("Total Outreaches", outreachStats$totalOutreaches))
  mlpReport <- rbind(mlpReport, c(paste("Hours spent on these", outreachStats$totalOutreaches, "Outreaches"), outreachStats$mlpOutreachesTime))
  mlpReport <- rbind(mlpReport, c("Consultations about Medical Assistance/Health Insurance", outreachStats$totalMedicalHealthOutreaches))
  
  ##################  Trainings
  
  mlpReport <- rbind(mlpReport, c("Total Trainings to PHMC Staff", outreachStats$totalTrainings))
  mlpReport <- rbind(mlpReport, c("Total Attendees", outreachStats$totalTrainingAttendees))
  
  ################## Demographics
  ## Language
  mlpReport <- rbind(mlpReport, c("English speakers", caseStats$languageEnglish))
  mlpReport <- rbind(mlpReport, c("Non-English speakers", caseStats$languageNonEnglish))
  mlpReport <- rbind(mlpReport, c("Total Languages Spoken", caseStats$languageTotal))
  
  
  ## Geography
  mlpReport <- rbind(mlpReport, c("Total Zipcodes Served", caseStats$zipTotal))
  
  ## Gender
  mlpReport <- rbind(mlpReport, c("Females", caseStats$GenderFemale))
  mlpReport <- rbind(mlpReport, c("Males", caseStats$GenderFemale))
  
  
  ## Children
  
  # sum the chidren in each unique case ID
  mlpReport <- rbind(mlpReport, c("Number of cases with children under 18", caseStats$ChildrenCaseTotal))
  # sum the chidren in each unique case ID
  mlpReport <- rbind(mlpReport, c("Total number of children under 18 in our clients' households", caseStats$ChildrenTotalChildrenServed ))
  
  
  ################# Types of cases
  mlpReport <- rbind(mlpReport, c("Health Insurance Cases", caseStats$typeHealthInsurance))
  mlpReport <- rbind(mlpReport, c("Health Services", caseStats$typeHealthServices))
  mlpReport <- rbind(mlpReport, c("Other Public Benefits", caseStats$typeOtherPB))
  mlpReport <- rbind(mlpReport, c("Utilities", caseStats$typeUtilities))
  mlpReport <- rbind(mlpReport, c("Immigration/Naturalization", caseStats$typeImmigration))
  
  
  ############### Types of OUtcomes
  mlpReport <- rbind(mlpReport, c("Outcomes: Medical Issues", caseStats$outcomesMedical))
  mlpReport <- rbind(mlpReport, c("Outcomes: Other Public Benefits", caseStats$outcomesOtherPB))
  mlpReport <- rbind(mlpReport, c("Outcomes: Utilities", caseStats$outcomesUtilities))
  mlpReport <- rbind(mlpReport, c("Outcomes: Other Financial Benefits", caseStats$outcomesOtherFinancial))
  
  
  
  ################ Report on Outreaches
  mlpReport <- rbind(mlpReport, c("Consults: Medical Assistance/Health Insurance", outreachStats$totalMedicalHealthOutreaches))
  mlpReport <- rbind(mlpReport, c("Consults: Housing", outreachStats$totalHousingOutreaches))
  mlpReport <- rbind(mlpReport, c("Consults: Social Security/SSI", outreachStats$totalSSIOutreaches))
  mlpReport <- rbind(mlpReport, c("Consults: Utilities", outreachStats$totalUtilitiesOutreaches))
  
  return(mlpReport)
}

# mlpCases <- readCasesFile()
# mlpOutreaches <- readOutreachesFile()
# caseStats <- getCaseStats(mlpCases)
# outreachStats <- getOutreachStats(mlpOutreaches)
# mlpReport <- createReport(caseStats, outreachStats)
