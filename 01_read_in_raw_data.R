# set the referrence 
options(digits = 4)
options(java.parameters='-Xmx1000m') #ensure that we can read in raw data#
require(xlsx)
require(reshape2)
require(plyr)
require(ggplot2)

# read in the raw data
raw_data <- read.xlsx2(file = "放大数据.xlsx", sheetName = "蒂清市场",
                       startRow = 2)
prod_info <- read.xlsx2(file = "产品信息.xlsx", sheetName = "cpa02",
                        startRow = 1)

num_var <- c("Est_DrugIncome_RMB", grep("^X", colnames(raw_data), perl = TRUE, value = TRUE))
raw_data[, num_var] <- lapply(raw_data[, num_var], function(x) as.numeric(levels(x)[x]))
str(raw_data)

# get the amplyfying factors
mkt_amplify_var <- c("seg", "Est_DrugIncome_RMB", "X2012Q1", "X2012Q2", "X2012Q3", 
                     "X2012Q4", "X2013Q1", "X2013Q2", "X2013Q3", "X2013Q4",
                     "X2014Q1", "X2014Q2")
mkt_amplify_core_var <- c("X2012Q1", "X2012Q2", "X2012Q3", 
                          "X2012Q4", "X2013Q1", "X2013Q2", "X2013Q3", "X2013Q4",
                          "X2014Q1", "X2014Q2")
for_mkt_amplify_factor_1 <- subset(raw_data, If.Panel.All == "Yes", select = mkt_amplify_var)
for_mkt_amplify_factor_1[,-1] <- lapply(for_amplify_factor_1[,-1], function(x) as.numeric(levels(x)[x]))
str(for_mkt_amplify_factor_1)
for_mkt_amplify_factor_2 <- aggregate(. ~ seg, for_amplify_factor_1, sum)
str(for_mkt_amplify_factor_2)
mkt_amplify_factor <- cbind(for_mkt_amplify_factor_2[, 1], 
                            for_mkt_amplify_factor_2[, c(-1, -2)]  / for_mkt_amplify_factor_2[, 2])
colnames(mkt_amplify_factor) <- c("seg", paste(mkt_amplify_core_var, "_f", sep = ""))

# amplifying the observations with mkt factor
subset_need_amplify <- subset(raw_data, If.Panel.All != "Yes")
str(subset_need_amplify)
subset_need_amplify_2 <- merge(subset_need_amplify, mkt_amplify_factor, "seg", all.x = TRUE)
subset_need_amplify_2[, mkt_amplify_core_var] <- 
  subset_need_amplify_2[, "Est_DrugIncome_RMB"] * subset_need_amplify_2[, paste(mkt_amplify_core_var, "_f", sep = "")]

# amplifying the observations with product factor
data_for_prod_amp <- rbind(subset(raw_data, If.Panel.All == "Yes"), subset_need_amplify_2[, 1:75])
any(is.na(data_for_prod_amp[, "X2012Q1"]))

str(data_for_prod_amp)
dq_all_strength_select = c("seg", "Region", paste0(c("X2012Q1", "X2012Q2", "X2012Q3",
                                                     "X2012Q4", "X2013Q1", "X2013Q2",
                                                     "X2013Q3", "X2013Q4", "X2014Q1",
                                                     "X2014Q2"), ".1", sep = ""),
                           "X2012Q1", "X2012Q2", "X2012Q3",
                           "X2012Q4", "X2013Q1", "X2013Q2",
                           "X2013Q3", "X2013Q4", "X2014Q1",
                           "X2014Q2")

dq_50mg_select = c("seg", "Region", paste0(c("X2012Q1", "X2012Q2", "X2012Q3",
                                             "X2012Q4", "X2013Q1", "X2013Q2",
                                             "X2013Q3", "X2013Q4", "X2014Q1",
                                             "X2014Q2"), ".2", sep = ""),
                   "X2012Q1", "X2012Q2", "X2012Q3",
                   "X2012Q4", "X2013Q1", "X2013Q2",
                   "X2013Q3", "X2013Q4", "X2014Q1",
                   "X2014Q2")

td_all_strength_select = c("seg", "Region", paste0(c("X2012Q1", "X2012Q2", "X2012Q3",
                                                     "X2012Q4", "X2013Q1", "X2013Q2",
                                                     "X2013Q3", "X2013Q4", "X2014Q1",
                                                     "X2014Q2"), ".3", sep = ""),
                           "X2012Q1", "X2012Q2", "X2012Q3",
                           "X2012Q4", "X2013Q1", "X2013Q2",
                           "X2013Q3", "X2013Q4", "X2014Q1",
                           "X2014Q2")

td_50mg_select = c("seg", "Region", paste0(c("X2012Q1", "X2012Q2", "X2012Q3",
                                             "X2012Q4", "X2013Q1", "X2013Q2",
                                             "X2013Q3", "X2013Q4", "X2014Q1",
                                             "X2014Q2"), ".4", sep = ""),
                   "X2012Q1", "X2012Q2", "X2012Q3",
                   "X2012Q4", "X2013Q1", "X2013Q2",
                   "X2013Q3", "X2013Q4", "X2014Q1",
                   "X2014Q2")

dq_sale_all_strength <- data_for_prod_amp[, dq_all_strength_select]
dq_sale_all_strength[, 3:ncol(dq_sale_all_strength)] <- lapply(dq_sale_all_strength[, 3:ncol(dq_sale_all_strength)],
                                                               function(x) {
                                                                 x[is.na(x)] <- 0
                                                                 x})
dq_sale_50mg <- data_for_prod_amp[, dq_50mg_select]
dq_sale_50mg[, 3:ncol(dq_sale_50mg)] <- lapply(dq_sale_50mg[, 3:ncol(dq_sale_50mg)],
                                               function(x) {
                                                 x[is.na(x)] <- 0
                                                 x})
td_sale_all_strength <- data_for_prod_amp[, td_all_strength_select]
td_sale_all_strength[, 3:ncol(td_sale_all_strength)] <- lapply(td_sale_all_strength[, 3:ncol(td_sale_all_strength)],
                                                               function(x) {
                                                                 x[is.na(x)] <- 0
                                                                 x})
td_sale_50mg <- data_for_prod_amp[, td_50mg_select]
td_sale_50mg[, 3:ncol(td_sale_50mg)] <- lapply(td_sale_50mg[, 3:ncol(td_sale_50mg)],
                                               function(x) {
                                                 x[is.na(x)] <- 0
                                                 x})

# calculate the product factor
dq_sale_all_strength_smmry <- aggregate(. ~ Region + seg, dq_sale_all_strength, sum, na.action = na.pass)
dq_sale_all_strength_smmry[, 2] <- as.numeric(levels(dq_sale_all_strength_smmry[, 2])[dq_sale_all_strength_smmry[, 2]])

dq_sale_50mg_smmry <- aggregate(. ~ Region + seg, dq_sale_50mg, sum)
dq_sale_50mg_smmry[, 2] <- as.numeric(levels(dq_sale_50mg_smmry[, 2])[dq_sale_50mg_smmry[, 2]])

td_sale_all_strength_smmry <- aggregate(. ~ Region + seg, td_sale_all_strength, sum)
td_sale_all_strength_smmry[, 2] <- as.numeric(levels(td_sale_all_strength_smmry[, 2])[td_sale_all_strength_smmry[, 2]])

td_sale_50mg_smmry <- aggregate(. ~ Region + seg, td_sale_50mg, sum)
td_sale_50mg_smmry[, 2] <- as.numeric(levels(td_sale_50mg_smmry[, 2])[td_sale_50mg_smmry[, 2]])


dq_sample_count <- as.data.frame(table(dq_sale_all_strength[, "Region"], dq_sale_all_strength[, "seg"]))
colnames(dq_sample_count) <- c("Region", "seg", "Freq")
dq_sample_count[, 2] <- as.numeric(levels(dq_sample_count[, 2])[dq_sample_count[, 2]])

dq_sample_count <- dq_sample_count[order(dq_sample_count[,1], dq_sample_count[, 2]), ]
dq_sample_count <- dq_sample_count[dq_sample_count[, "Freq"] > 0, ]


# get the sample info
dq_sale_all_strength_smmry <- merge(dq_sale_all_strength_smmry, dq_sample_count, 
                                    c("Region", "seg"), all.x = TRUE )
dq_sale_all_strength_smmry <- dq_sale_all_strength_smmry[order(dq_sale_all_strength_smmry[,1], 
                                                               dq_sale_all_strength_smmry[, 2]), ]

dq_sale_50mg_smmry <- merge(dq_sale_50mg_smmry, dq_sample_count,
                                    c("Region", "seg"), all.x = TRUE )
dq_sale_50mg_smmry <- dq_sale_50mg_smmry[order(dq_sale_50mg_smmry[,1], 
                                               dq_sale_50mg_smmry[, 2]), ]


td_sale_all_strength_smmry <- merge(td_sale_all_strength_smmry, dq_sample_count,
                                    c("Region", "seg"), all.x = TRUE )
td_sale_all_strength_smmry <- td_sale_all_strength_smmry[order(td_sale_all_strength_smmry[,1], 
                                                               td_sale_all_strength_smmry[, 2]), ]
td_sale_50mg_smmry <- merge(td_sale_50mg_smmry, dq_sample_count,
                            c("Region", "seg"), all.x = TRUE )
td_sale_50mg_smmry <- td_sale_50mg_smmry[order(td_sale_50mg_smmry[,1], 
                                               td_sale_50mg_smmry[, 2]), ]
dim(td_sale_50mg_smmry)

# update the factor
for (i in 2:nrow(dq_sale_all_strength_smmry)){
  if (dq_sale_all_strength_smmry[i, "Region"] == dq_sale_all_strength_smmry[i - 1, "Region"] &&
        dq_sale_all_strength_smmry[i, "Freq"] < 3){
    dq_sale_all_strength_smmry[i, 3:22] <- dq_sale_all_strength_smmry[i - 1, 3:22]
  }
}

for (i in 2:nrow(dq_sale_50mg_smmry)){
  if (dq_sale_50mg_smmry[i, "Region"] == dq_sale_50mg_smmry[i - 1, "Region"] &&
        dq_sale_50mg_smmry[i, "Freq"] < 3){
    dq_sale_50mg_smmry[i, 3:22] <- dq_sale_50mg_smmry[i - 1, 3:22]
  }
}

for (i in 2:nrow(td_sale_all_strength_smmry)){
  if (td_sale_all_strength_smmry[i, "Region"] == td_sale_all_strength_smmry[i - 1, "Region"] &&
        td_sale_all_strength_smmry[i, "Freq"] < 3){
    td_sale_all_strength_smmry[i, 3:22] <- td_sale_all_strength_smmry[i - 1, 3:22]
  }
}

for (i in 2:nrow(td_sale_50mg_smmry)){
  if (td_sale_50mg_smmry[i, "Region"] == td_sale_50mg_smmry[i - 1, "Region"] &&
        td_sale_50mg_smmry[i, "Freq"] < 3){
    td_sale_50mg_smmry[i, 3:22] <- td_sale_50mg_smmry[i - 1, 3:22]
  }
}

dq_sale_all_strength_factor <- cbind(dq_sale_all_strength_smmry[, c((1:2), 23)],
                                     dq_sale_all_strength_smmry[, 3:12] / dq_sale_all_strength_smmry[, 13:22])
colnames(dq_sale_all_strength_factor)[4:13] <- paste(colnames(dq_sale_all_strength_factor)[4:13], "_f", sep = "")
dq_sale_50mg_factor <- cbind(dq_sale_50mg_smmry[, c((1:2), 23)],
                             dq_sale_50mg_smmry[, 3:12] / dq_sale_50mg_smmry[, 13:22])
colnames(dq_sale_50mg_factor)[4:13] <- paste(colnames(dq_sale_50mg_factor)[4:13], "_f", sep = "")
td_sale_all_strength_factor <- cbind(td_sale_all_strength_smmry[, c((1:2), 23)],
                                     td_sale_all_strength_smmry[, 3:12] / td_sale_all_strength_smmry[, 13:22])
colnames(td_sale_all_strength_factor)[4:13] <- paste(colnames(td_sale_all_strength_factor)[4:13], "_f", sep = "")
td_sale_50mg_factor <- cbind(td_sale_50mg_smmry[, c((1:2), 23)],
                             td_sale_50mg_smmry[, 3:12] / dq_sale_50mg_smmry[, 13:22])
colnames(td_sale_50mg_factor)[4:13] <- paste(colnames(td_sale_50mg_factor)[4:13], "_f", sep = "")


head(subset_need_amplify_2)

subset_need_amplify_3 <- merge(subset_need_amplify_2, dq_sale_all_strength_factor[, -3],
                               by = c("Region", "seg"))
subset_need_amplify_3 <- merge(subset_need_amplify_3, dq_sale_50mg_factor[, -3],
                               by = c("Region", "seg"))
subset_need_amplify_3 <- merge(subset_need_amplify_3, td_sale_all_strength_factor[, -3],
                               by = c("Region", "seg"))
subset_need_amplify_3 <- merge(subset_need_amplify_3, td_sale_50mg_factor[, -3],
                               by = c("Region", "seg"))

subset_need_amplify_3[, paste(c("X2012Q1", "X2012Q2", "X2012Q3",
                                "X2012Q4", "X2013Q1", "X2013Q2",
                                "X2013Q3", "X2013Q4", "X2014Q1",
                                "X2014Q2"), ".1", sep = "")] <-
  subset_need_amplify_3[, c("X2012Q1", "X2012Q2", "X2012Q3",
                            "X2012Q4", "X2013Q1", "X2013Q2",
                            "X2013Q3", "X2013Q4", "X2014Q1",
                            "X2014Q2")] * 
  subset_need_amplify_3[, paste(c("X2012Q1", "X2012Q2", "X2012Q3",
                                  "X2012Q4", "X2013Q1", "X2013Q2",
                                  "X2013Q3", "X2013Q4", "X2014Q1",
                                  "X2014Q2"), ".1_f", sep = "")]

subset_need_amplify_3[, paste(c("X2012Q1", "X2012Q2", "X2012Q3",
                                "X2012Q4", "X2013Q1", "X2013Q2",
                                "X2013Q3", "X2013Q4", "X2014Q1",
                                "X2014Q2"), ".2", sep = "")] <-
  subset_need_amplify_3[, c("X2012Q1", "X2012Q2", "X2012Q3",
                            "X2012Q4", "X2013Q1", "X2013Q2",
                            "X2013Q3", "X2013Q4", "X2014Q1",
                            "X2014Q2")] * 
  subset_need_amplify_3[, paste(c("X2012Q1", "X2012Q2", "X2012Q3",
                                  "X2012Q4", "X2013Q1", "X2013Q2",
                                  "X2013Q3", "X2013Q4", "X2014Q1",
                                  "X2014Q2"), ".2_f", sep = "")]

subset_need_amplify_3[, paste(c("X2012Q1", "X2012Q2", "X2012Q3",
                                "X2012Q4", "X2013Q1", "X2013Q2",
                                "X2013Q3", "X2013Q4", "X2014Q1",
                                "X2014Q2"), ".3", sep = "")] <-
  subset_need_amplify_3[, c("X2012Q1", "X2012Q2", "X2012Q3",
                            "X2012Q4", "X2013Q1", "X2013Q2",
                            "X2013Q3", "X2013Q4", "X2014Q1",
                            "X2014Q2")] * 
  subset_need_amplify_3[, paste(c("X2012Q1", "X2012Q2", "X2012Q3",
                                  "X2012Q4", "X2013Q1", "X2013Q2",
                                  "X2013Q3", "X2013Q4", "X2014Q1",
                                  "X2014Q2"), ".3_f", sep = "")]

subset_need_amplify_3[, paste(c("X2012Q1", "X2012Q2", "X2012Q3",
                                "X2012Q4", "X2013Q1", "X2013Q2",
                                "X2013Q3", "X2013Q4", "X2014Q1",
                                "X2014Q2"), ".4", sep = "")] <-
  subset_need_amplify_3[, c("X2012Q1", "X2012Q2", "X2012Q3",
                            "X2012Q4", "X2013Q1", "X2013Q2",
                            "X2013Q3", "X2013Q4", "X2014Q1",
                            "X2014Q2")] * 
  subset_need_amplify_3[, paste(c("X2012Q1", "X2012Q2", "X2012Q3",
                                  "X2012Q4", "X2013Q1", "X2013Q2",
                                  "X2013Q3", "X2013Q4", "X2014Q1",
                                  "X2014Q2"), ".4_f", sep = "")]





