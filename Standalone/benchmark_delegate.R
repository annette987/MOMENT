print(Sys.info()['nodename'])
if (substring(Sys.info()['nodename'], 1, 1) == 'k') { # Katana
	data_dir = "/srv/scratch/z8039617"
	code_dir = "./"
} else if (substring(Sys.info()['nodename'], 1, 1) == 'g')  { #Gadi
	data_dir = "/scratch/zq94/as0786"
	code_dir = "./"
} else {
	data_dir = "C:/Users/Annette/Documents/Liver Cancer Project/Experiments/R Code/"
	code_dir = data_dir
}
cache_dir = paste0(data_dir, "/mlr/.cache")

source(paste(code_dir, "delegation_tests.R", sep=""))

#-------------------------------------------------------------------
# Get Configuration File
#-------------------------------------------------------------------
args = commandArgs(TRUE)
config_file <- args[1]
#config_file = "config_delegate2.xlsx"
cf = config(config_file)
cat("Config file: ", config_file, "\n")
#print(cf)

#-------------------------------------------------------------------
# Get Environment Variables
#-------------------------------------------------------------------
res_index = 0
excl = TRUE
imp = TRUE

env_vars = Sys.getenv()
for (name in names(env_vars)) {
	switch(name, 
				 res_index = {res_index = as.numeric(env_vars[[name]])}, 
				 excl = {excl = as.logical(env_vars[[name]])},
				 imp = {imp = as.logical(env_vars[[name]])}
				)
}

print(paste0("res_index = ", res_index))
print(paste0("excl = ", excl))
print(paste0("imp = ", imp))

#-------------------------------------------------------------------
# Run Delegating Classifiers
#-------------------------------------------------------------------
delegation_tests(cf, res_index, cache_dir)