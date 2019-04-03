#' A simple_plot Function
#'
#' @return a plot from the queries.


# TO DOWNLOAD PLS install_github("oaa97181/simpleplotpkg", force=TRUE)

simple_plot <- function(){

mydb = dbConnect(MySQL(), 
                 user="guest", 
                 password="", 
                 dbname="expdata_hsapi_ipf", 
                 host="10.200.0.42")


#dbListTables(mydb)

sql = 'SELECT \
sc.contrast_name,e.experiment_access_id,p.platform_name,p.platform_type \
FROM sample_contrast as sc \
INNER JOIN sample as s ON sc.test_sample_fk = s.sample_id \
INNER JOIN experiment as e ON s.experiment_fk= e.experiment_id \
INNER JOIN sample_hybrid_mapping as shm ON s.sample_id=shm.sample_fk \
INNER JOIN hybridization as h ON shm.hybridization_fk = h.hybridization_id \
INNER JOIN array as a ON h.array_fk = a.array_id \
INNER JOIN platform as p ON a.platform_fk = p.platform_id
WHERE e.status_fk = 3
' 

rs = dbSendQuery(mydb,sql)
data = fetch(rs, n=-1)
contrasts_gse = data

platform <- table(contrasts_gse$platform_name)
platform <- as.data.frame(platform)
platform$Var1 <- factor(platform$Var1, levels = platform[order(platform$Freq), "Var1"])
platform <- platform[-grep("Duke",platform$Var1),]
sum(platform$Freq)

platform[grep("Affy",platform$Var1),"Plat"] <- "Affymetrix"
platform[grep("Agilent",platform$Var1),"Plat"] <- "Agilent"
platform[grep("Illumina",platform$Var1),"Plat"] <- "Illumina"
#platform$Plat <- factor(platform$Plat, levels = platform[order(platform$Freq), "Plat"])

plat_top <- platform[platform$Freq>50,]
plat_top <- rbind(plat_top, 
                  data.frame(
                    Var1="Others",
                    Freq=sum(platform[platform$Freq<50,"Freq"]),
                    Plat="Others")
                  )

plat_top$Var1 <- factor(plat_top$Var1, levels = rev(plat_top$Var1))

colourCount = length(unique(plat_top$Var1))
getPalette = colorRampPalette(brewer.pal(9, "YlGnBu"))

g <- ggplot(plat_top, aes(x = Plat, y = Freq, fill= Var1),position = "dodge")
gg <- g + geom_bar(stat="identity") +
  geom_text(aes(label= Freq),position = position_stack(vjust = 0.5), color = "white", size= 6.5) +
  scale_fill_manual(values= getPalette(colourCount+5)[-c(1:5)]) +
  labs(y = "Number of contrast samples", x= "Platform", fill = "Platform types") +
  coord_flip()  

gg + theme(axis.title.x=element_text(vjust=0, size=25),  # X axis title
      axis.title.y=element_text(size=25),  # Y axis title
      axis.text.x=element_text(size=20,color = "#5b5b59"),  # X axis text
      axis.text.y=element_text(size=20,color = "#5b5b59"),  # Y axis text 
      legend.position = c(.75,.72),
      legend.title = element_text(size=20), 
      legend.text = element_text(size=15),
      legend.key.size = unit(1, "cm")
      ) 

}