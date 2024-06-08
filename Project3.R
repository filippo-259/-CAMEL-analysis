library(readxl)
library(RColorBrewer)
library(ggplot2)
library(reshape2)
rm(list=ls())
setwd('C:/Users/frami/Desktop/Università/Magistrale/1° anno/2° semestre/FINANCE AND BANKING/Projects/3')

banks=read_excel('banks.xlsx', sheet=2)

banks[,12:95]=lapply(banks[,12:95], as.numeric)
#banks=na.omit(banks)
banks$`Consolidation code`=as.factor(banks$`Consolidation code`)
banks$`Country ISO code`=as.factor(banks$`Country ISO code`)
banks$Specialisation=as.factor(banks$Specialisation)
banks$`Company name Latin alphabet`=as.factor(banks$`Company name Latin alphabet`)
summary(banks)

#some banks are present more than once, we remove according to the consolidation code
banks=banks[!(banks$`Consolidation code`%in%c('U1', 'U2', 'U*', 'C*')),]
summary(banks)

#pie chart for specialisation
pie(table(factor(banks$Specialisation,levels=c('Bank holding company','Commercial bank','Cooperative bank','Finance company','Investment bank','Non-Bank holding company','Real estate & mortgage finance institution','Savings bank','Specialized governmental credit institution'))), col=brewer.pal(10, "Set3"),cex = 0.8,main = "Specialisation Distribution in EU Banks")
legend('topleft', legend = paste(names(table(factor(banks$Specialisation,levels=c('Bank holding company','Commercial bank','Cooperative bank','Finance company','Investment bank','Non-Bank holding company','Real estate & mortgage finance institution','Savings bank','Specialized governmental credit institution')))), ": ", table(factor(banks$Specialisation,levels=c('Bank holding company','Commercial bank','Cooperative bank','Finance company','Investment bank','Non-Bank holding company','Real estate & mortgage finance institution','Savings bank','Specialized governmental credit institution'))), sep = ""), fill = brewer.pal(10, "Set3"), cex = 0.8, bty = "n")

#pie charts for main country (specialisation)
it_banks=subset(banks, `Country ISO code`=='IT')
pie(table(factor(it_banks$Specialisation, levels=c('Commercial bank','Cooperative bank','Bank holding company'))), main = "Specialisation Distribution in 'IT' Banks", col=brewer.pal(3, "Set1"))
legend('topleft', legend = paste(names(table(factor(it_banks$Specialisation, levels=c('Commercial bank','Cooperative bank','Bank holding company')))), ": ", table(factor(it_banks$Specialisation, levels=c('Commercial bank','Cooperative bank','Bank holding company'))), sep = ""), fill = brewer.pal(3, "Set1"), cex = 0.8, bty = "n")

fr_banks=subset(banks, `Country ISO code`=='FR')
pie(table(factor(fr_banks$Specialisation, levels=c('Commercial bank','Cooperative bank','Specialized governmental credit institution'))), main = "Specialisation Distribution in 'FR' Banks", col=brewer.pal(3, "Set1"))
legend('topright', legend = paste(names(table(factor(fr_banks$Specialisation, levels=c('Commercial bank','Cooperative bank','Specialized governmental credit institution')))), ": ", table(factor(fr_banks$Specialisation, levels=c('Commercial bank','Cooperative bank','Specialized governmental credit institution'))), sep = ""), fill = brewer.pal(3, "Set1"), cex = 0.8, bty = "n")

de_banks=subset(banks, `Country ISO code`=='DE')
pie(table(factor(de_banks$Specialisation, levels=c('Commercial bank','Bank holding company','Cooperative bank','Finance company','Real estate & mortgage finance institution','Investment bank'))), main = "Specialisation Distribution in 'DE' Banks", col=brewer.pal(6, "Set1"))
legend('topleft', legend = paste(names(table(factor(de_banks$Specialisation, levels=c('Commercial bank','Bank holding company','Cooperative bank','Finance company','Real estate & mortgage finance institution','Investment bank')))), ": ", table(factor(de_banks$Specialisation, levels=c('Commercial bank','Bank holding company','Cooperative bank','Finance company','Real estate & mortgage finance institution','Investment bank'))), sep = ""), fill = brewer.pal(6, "Set1"), cex = 0.8, bty = "n")

es_banks=subset(banks, `Country ISO code`=='ES')
pie(table(factor(es_banks$Specialisation, levels=c('Commercial bank','Savings bank','Cooperative bank'))), main = "Specialisation Distribution in 'ES' Banks", col=brewer.pal(3, "Set1"))
legend('topright', legend = paste(names(table(factor(es_banks$Specialisation, levels=c('Commercial bank','Savings bank','Cooperative bank')))), ": ", table(factor(es_banks$Specialisation, levels=c('Commercial bank','Savings bank','Cooperative bank'))), sep = ""), fill = brewer.pal(3, "Set1"), cex = 0.8, bty = "n")

#barplot for country/specialisation
filtered_banks_comm = subset(banks, `Specialisation` == 'Commercial bank')
filtered_banks_hold = subset(banks, `Specialisation` == 'Bank holding company')

filtered_banks_spec = subset(banks, `Specialisation` == 'Specialized governmental credit institution')
filtered_banks_coop = subset(banks, `Specialisation` == 'Cooperative bank')

# 2022 - comm bank - france
filtered_banks_comm_fr = subset(filtered_banks_comm, `Country ISO code` == 'FR')


filtered_banks_comm_melted_fr <-
  melt(
    filtered_banks_comm_fr,
    id.vars = "Company name Latin alphabet",
    measure.vars = c(
      "Liquidity 2022",
      "Asset quality 2022",
      "Capital adequacy 2022",
      "Earnings quality 2022...73"
    )
  )

ggplot(
  filtered_banks_comm_melted_fr,
  aes(fill = variable, y = value, x = `Company name Latin alphabet`)
) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Sum of ratings for 2022 - France commercial banks", y = "Rank", fill = "Variable") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 2022 - spec bank - france
filtered_banks_spec_fr = subset(filtered_banks_spec, `Country ISO code` == 'FR')


filtered_banks_spec_melted_fr <-
  melt(
    filtered_banks_spec_fr,
    id.vars = "Company name Latin alphabet",
    measure.vars = c(
      "Liquidity 2022",
      "Asset quality 2022",
      "Capital adequacy 2022",
      "Earnings quality 2022...73"
    )
  )

ggplot(
  filtered_banks_spec_melted_fr,
  aes(fill = variable, y = value, x = `Company name Latin alphabet`)
) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Sum of ratings for 2022 - France SGCI banks", y = "Rank", fill = "Variable") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 2022 - cooperative bank - france
filtered_banks_coop_fr = subset(filtered_banks_coop, `Country ISO code` == 'FR')


filtered_banks_coop_melted_fr <-
  melt(
    filtered_banks_coop_fr,
    id.vars = "Company name Latin alphabet",
    measure.vars = c(
      "Liquidity 2022",
      "Asset quality 2022",
      "Capital adequacy 2022",
      "Earnings quality 2022...73"
    )
  )

ggplot(
  filtered_banks_coop_melted_fr,
  aes(fill = variable, y = value, x = `Company name Latin alphabet`)
) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Sum of ratings for 2022 - France cooperative banks", y = "Rank", fill = "Variable") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 2022 - commercial banks - italy
filtered_banks_comm_it = subset(filtered_banks_comm, `Country ISO code` == 'IT')


filtered_banks_comm_melted_it <-
  melt(
    filtered_banks_comm_it,
    id.vars = "Company name Latin alphabet",
    measure.vars = c(
      "Liquidity 2022",
      "Asset quality 2022",
      "Capital adequacy 2022",
      "Earnings quality 2022...73"
    )
  )

ggplot(
  filtered_banks_comm_melted_it,
  aes(fill = variable, y = value, x = `Company name Latin alphabet`)
) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Sum of ratings for 2022 - Italy commercial banks", y = "Rank", fill = "Variable") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 2022 - cooperative banks - italy
filtered_banks_coop_it = subset(filtered_banks_coop, `Country ISO code` == 'IT')


filtered_banks_coop_melted_it <-
  melt(
    filtered_banks_coop_it,
    id.vars = "Company name Latin alphabet",
    measure.vars = c(
      "Liquidity 2022",
      "Asset quality 2022",
      "Capital adequacy 2022",
      "Earnings quality 2022...73"
    )
  )

ggplot(
  filtered_banks_coop_melted_it,
  aes(fill = variable, y = value, x = `Company name Latin alphabet`)
) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Sum of ratings for 2022 - Italy cooperative bank", y = "Rank", fill = "Variable") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# 2022 - holding bank - italy
filtered_banks_hold_it = subset(filtered_banks_hold, `Country ISO code` == 'IT')


filtered_banks_hold_melted_it <-
  melt(
    filtered_banks_hold_it,
    id.vars = "Company name Latin alphabet",
    measure.vars = c(
      "Liquidity 2022",
      "Asset quality 2022",
      "Capital adequacy 2022",
      "Earnings quality 2022...73"
    )
  )

ggplot(
  filtered_banks_hold_melted_it,
  aes(fill = variable, y = value, x = `Company name Latin alphabet`)
) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Sum of ratings for 2022 - Italy holding bank", y = "Rank", fill = "Variable") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# 2022 - commercial bank - germany
filtered_banks_comm_de = subset(filtered_banks_comm, `Country ISO code` == 'DE')


filtered_banks_comm_melted_de <-
  melt(
    filtered_banks_comm_de,
    id.vars = "Company name Latin alphabet",
    measure.vars = c(
      "Liquidity 2022",
      "Asset quality 2022",
      "Capital adequacy 2022",
      "Earnings quality 2022...73"
    )
  )

ggplot(
  filtered_banks_comm_melted_de,
  aes(fill = variable, y = value, x = `Company name Latin alphabet`)
) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Sum of ratings for 2022 - Germany commercial banks", y = "Rank", fill = "Variable") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 2022 - holding bank - germany
filtered_banks_hold_de = subset(filtered_banks_hold, `Country ISO code` == 'DE')


filtered_banks_hold_melted_de <-
  melt(
    filtered_banks_hold_de,
    id.vars = "Company name Latin alphabet",
    measure.vars = c(
      "Liquidity 2022",
      "Asset quality 2022",
      "Capital adequacy 2022",
      "Earnings quality 2022...73"
    )
  )

ggplot(
  filtered_banks_hold_melted_de,
  aes(fill = variable, y = value, x = `Company name Latin alphabet`)
) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Sum of ratings for 2022 - Germany holding banks", y = "Rank", fill = "Variable") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 2022 - comm bank - East Europe
filtered_banks_comm_easteu <- subset(filtered_banks_comm, `Country ISO code` %in% c("EE", "LV", "LT", "FI"))


filtered_banks_comm_melted_eastue <-
  melt(
    filtered_banks_comm_easteu,
    id.vars = "Company name Latin alphabet",
    measure.vars = c(
      "Liquidity 2022",
      "Asset quality 2022",
      "Capital adequacy 2022",
      "Earnings quality 2022...73"
    )
  )

ggplot(
  filtered_banks_comm_melted_eastue,
  aes(fill = variable, y = value, x = `Company name Latin alphabet`)
) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Sum of ratings for 2022 - East Europe commercial banks", y = "Rank", fill = "Variable") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 2022 - Bank holding company - East Europe

filtered_banks_hold_easteu <- subset(filtered_banks_hold, `Country ISO code` %in% c("EE"))


filtered_banks_hold_melted_eastue <-
  melt(
    filtered_banks_hold_easteu,
    id.vars = "Company name Latin alphabet",
    measure.vars = c(
      "Liquidity 2022",
      "Asset quality 2022",
      "Capital adequacy 2022",
      "Earnings quality 2022...73"
    )
  )

ggplot(
  filtered_banks_hold_melted_eastue,
  aes(fill = variable, y = value, x = `Company name Latin alphabet`)
) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Sum of ratings for 2022 - East Europe Bank holding company", y = "Rank", fill = "Variable") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 2022 - comm bank - North Europe

filtered_banks_comm_northeu <- subset(filtered_banks_comm, `Country ISO code` %in% c("NL", "BE", "IE"))


filtered_banks_comm_melted_northue <-
  melt(
    filtered_banks_comm_northeu,
    id.vars = "Company name Latin alphabet",
    measure.vars = c(
      "Liquidity 2022",
      "Asset quality 2022",
      "Capital adequacy 2022",
      "Earnings quality 2022...73"
    )
  )

ggplot(
  filtered_banks_comm_melted_northue,
  aes(fill = variable, y = value, x = `Company name Latin alphabet`)
) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Sum of ratings for 2022 - North Europe commercial banks", y = "Rank", fill = "Variable") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 2022 - Bank holding company - North Europe

filtered_banks_hold_northeu <- subset(filtered_banks_hold, `Country ISO code` %in% c("NL", "BE", "IE"))


filtered_banks_hold_melted_northue <-
  melt(
    filtered_banks_hold_northeu,
    id.vars = "Company name Latin alphabet",
    measure.vars = c(
      "Liquidity 2022",
      "Asset quality 2022",
      "Capital adequacy 2022",
      "Earnings quality 2022...73"
    )
  )

ggplot(
  filtered_banks_hold_melted_northue,
  aes(fill = variable, y = value, x = `Company name Latin alphabet`)
) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Sum of ratings for 2022 - North Europe Bank holding company", y = "Rank", fill = "Variable") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 2022 - comm bank - South Europe

filtered_banks_comm_southheu <- subset(filtered_banks_comm, `Country ISO code` %in% c("ES", "PT", "MT", "CY"))


filtered_banks_comm_melted_southue <-
  melt(
    filtered_banks_comm_southheu,
    id.vars = "Company name Latin alphabet",
    measure.vars = c(
      "Liquidity 2022",
      "Asset quality 2022",
      "Capital adequacy 2022",
      "Earnings quality 2022...73"
    )
  )

ggplot(
  filtered_banks_comm_melted_southue,
  aes(fill = variable, y = value, x = `Company name Latin alphabet`)
) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Sum of ratings for 2022 - South Europe commercial banks", y = "Rank", fill = "Variable") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 2022 - Bank holding company - South Europe

filtered_banks_hold_southheu <- subset(filtered_banks_hold, `Country ISO code` %in% c("ES", "PT", "MT", "CY"))


filtered_banks_hold_melted_southue <-
  melt(
    filtered_banks_hold_southheu,
    id.vars = "Company name Latin alphabet",
    measure.vars = c(
      "Liquidity 2022",
      "Asset quality 2022",
      "Capital adequacy 2022",
      "Earnings quality 2022...73"
    )
  )

ggplot(
  filtered_banks_hold_melted_southue,
  aes(fill = variable, y = value, x = `Company name Latin alphabet`)
) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Sum of ratings for 2022 - South Europe Bank holding company", y = "Rank", fill = "Variable") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 2022 - Investment banks

filtered_banks_inv = subset(banks, `Specialisation` == 'Investment bank')

filtered_banks_inv_melted <-
  melt(
    filtered_banks_inv,
    id.vars = "Company name Latin alphabet",
    measure.vars = c(
      "Liquidity 2022",
      "Asset quality 2022",
      "Capital adequacy 2022",
      "Earnings quality 2022...73"
    )
  )

ggplot(
  filtered_banks_inv_melted,
  aes(fill = variable, y = value, x = `Company name Latin alphabet`)
) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Sum of ratings for 2022 - Investment Banks", y = "Rank", fill = "Variable") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#score for each bank
df=data.frame(it_banks$`Company name Latin alphabet`, it_banks$`Complessive rank 2022`)
names(df)=c('Bank', 'Rank')
df
df=data.frame(fr_banks$`Company name Latin alphabet`, fr_banks$`Complessive rank 2022`)
names(df)=c('Bank', 'Rank')
df
df=data.frame(de_banks$`Company name Latin alphabet`, de_banks$`Complessive rank 2022`)
names(df)=c('Bank', 'Rank')
df
#east
d = subset(banks, `Country ISO code` %in% c('EE', 'LV', 'LT', 'FI') &`Specialisation` %in% c('Commercial bank','Bank holding company') )
df=data.frame(d$`Company name Latin alphabet`,d$`Country ISO code`, d$`Complessive rank 2022`)
names(df)=c('Bank', 'Country', 'Rank')
df
#north
d = subset(banks, `Country ISO code` %in% c('ES', 'PT', 'MT','CY') &`Specialisation` %in% c('Commercial bank','Bank holding company') )
df=data.frame(d$`Company name Latin alphabet`,d$`Country ISO code`, d$`Complessive rank 2022`)
names(df)=c('Bank', 'Country', 'Rank')
df
#south
d = subset(banks, `Country ISO code` %in% c('ES', 'PT', 'MT','CY') &`Specialisation` %in% c('Commercial bank','Bank holding company') )
df=data.frame(d$`Company name Latin alphabet`,d$`Country ISO code`, d$`Complessive rank 2022`)
names(df)=c('Bank', 'Country', 'Rank')
df
#investing
d = subset(banks,`Specialisation` == 'Investment bank')
df=data.frame(d$`Company name Latin alphabet`,d$`Country ISO code`, d$`Complessive rank 2022`)
names(df)=c('Bank', 'Country', 'Rank')
df
#Annual comparison
banks$change18_19=banks$`Complessive rank 2019`/banks$`Complessive rank 2018`-1
banks$change19_20=banks$`Complessive rank 2020`/banks$`Complessive rank 2019`-1
banks$change20_21=banks$`Complessive rank 2021`/banks$`Complessive rank 2020`-1
banks$change21_22=banks$`Complessive rank 2022`/banks$`Complessive rank 2021`-1
banks$change22_23=banks$`Complessive rank 2023`/banks$`Complessive rank 2022`-1
banks$change18_23=banks$`Complessive rank 2023`/banks$`Complessive rank 2018`-1

#so we can see the best/worst changes
df=data.frame(banks$`Company name Latin alphabet`,banks$`Country ISO code`,banks$Specialisation,banks[,96:101])
names(df)=c('Bank','Country','Specialisation','Change18/19','Change19/20','Change20/21','Change21/22','Change22/23','Change18/23')
View(df)

#annual changes BEST and WORST in the entire period
year=c(2018,2019,2020,2021,2022,2023) 
plot(year, rev(as.numeric(banks[which.min(banks$change18_23), 90:95])), type = 'l', main = 'Best and worst', ylab='Rank', ylim=c(1.3,4), lwd=2)
lines(year,rev(as.numeric(banks[which.max(banks$change18_23), 90:95])), col='red', lwd=2)
legend("topright", legend = c('BANCA MONTE DEI PASCHI DI SIENA SPA (-58%)', 'ADDIKO BANK AG (+93%)'), fill = c('black', 'red'))

#annual changes Italy
plot(year, rev(as.numeric(it_banks[1, 90:95])), type = 'l', main = 'Italy', ylab='Rank', lwd=2)
lines(year, rev(as.numeric(it_banks[2, 90:95])), col='red', lwd=2)
lines(year, rev(as.numeric(it_banks[3, 90:95])), col='blue', lwd=2)
legend("topright", legend = c('INTESA SANPAOLO S.P.A.', 'UNICREDIT SPA', 'BANCO BPM SPA'), fill = c('black', 'red', 'blue'))

#annual changes France
plot(year, rev(as.numeric(fr_banks[1, 90:95])), type = 'l', main = 'France', ylab='Rank', ylim=c(3,4.5), lwd=2)
lines(year, rev(as.numeric(fr_banks[2, 90:95])), col='red', lwd=2)
lines(year, rev(as.numeric(fr_banks[3, 90:95])), col='blue', lwd=2)
legend("topright", legend = c('BNP PARIBAS', 'CREDIT AGRICOLE SA', 'SOCIETE GENERALE'), fill = c('black', 'red', 'blue'))

#annual changes Germany
plot(year, rev(as.numeric(de_banks[1, 90:95])), type = 'l', main = 'Germany', ylab='Rank', ylim=c(3.3,3.9), lwd=2)
lines(year, rev(as.numeric(de_banks[2, 90:95])), col='red', lwd=2)
lines(year, rev(as.numeric(de_banks[3, 90:95])), col='blue', lwd=2)
legend("topright", legend = c('DEUTSCHE BANK AG', 'DZ BANK AG DEUTSCHE ZENTRAL', 'COMMERZBANK AG'), fill = c('black', 'red', 'blue'))
