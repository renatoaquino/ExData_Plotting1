source("util.R")
housepower <- if(exists("housepower")) housepower else read_housepower()
png("plot1.png",width=480,height=480,units="px",bg="transparent")
gen_plot_1(housepower)
dev.off()
