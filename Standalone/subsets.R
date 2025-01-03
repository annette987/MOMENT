#Create subsets of data for each modality

print(Sys.info()['nodename'])
if (Sys.info()['nodename'] == 'nemesis.cse.unsw.edu.au')
{
	code_dir = "./Code/"
	code_dir = "./"
} else if (substring(Sys.info()['nodename'], 1, 1) == 'k') {
	code_dir = "./"
	data_dir = "/srv/scratch/z8039617/"

} else {
	code_dir = "C:/Users/Annette/Documents/Liver Cancer Project/Experiments/R Code/"
	data_dir = "C:/Users/Annette/Documents/Liver Cancer Project/Experiments/Data/"
}
print(code_dir)
print(data_dir)

source("const.R")
source("config.R")

combns = list(c('CIR', 'LN'), c('CIR', 'LX'), c('LN', 'LX'), c('CIR', 'CON'), c('LN', 'CON'), c('LX', 'CON'))
datasets = list('Clinical_cmn.csv', 'Cytokine_cmn.csv', 'Pathologic_cmn.csv', 'Metabolomic_cmn.csv', 
								'OralGenus_num_cmn.csv', 'OralSpecies_num_cmn.csv', 'StoolGenus_num_cmn.csv', 'StoolSpecies_num_cmn.csv',
								'concat_all.csv')
setwd(data_dir)

dat = list()
for (i in 1:length(datasets)) {
		raw = read.csv(datasets[[i]], row.names = NULL, sep = ",", dec = '.', header = TRUE, stringsAsFactors=FALSE)
		dat[[i]] = raw
}

for (j in 1:length(combns)) {
	for (i in 1:length(datasets)) {
		subdat = dat[[i]][dat[[i]]$Label %in% combns[[j]], ]
		names = strsplit(datasets[[i]], ".", fixed = TRUE)
		subdat_name = paste0(names[[1]][[1]], "_", paste(combns[[j]], collapse = ""), ".csv")
		write.csv(subdat, subdat_name, row.names=FALSE, na="")
	}
}

