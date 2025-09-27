# Loop through Institutional reports
library(rmarkdown)

# Liste der Institutionen
# [1] Université de Lausanne, UNIL                            Universität Basel, UNIBAS                              
# [3] Berner Fachhochschule, BFH                              Universität Luzern, UNILU                              
# [5] Universität Bern, UNIBE                                 Université de Fribourg / Universität Fribourg, UNIFR   
# [7] Eidgenössische Technische Hochschule Zürich, ETH Zürich Universität Zürich, UZH                                
# [9] Université de Genève, UNIGE                             Ecole Polytechnique Fédérale de Lausanne, EPFL         
# [11] Haute Ecole Spécialisée de Suisse occidentale, HES-SO   Université de Neuchâtel, UniNE                         
# [13] Pädagogische Hochschule Bern, PHBern    
# Aus Timons national dokument

# Load data
load("../data/SWiMS2024_Data_2025-09-08.RData")
unique(dat$institution) # sind die 13 

object.inst <- levels(dat$institution)
# g <- 7

# Loop über jede Gruppe
for (g in 5) {
  
  if(object.inst[g] == "Eidgenössische Technische Hochschule Zürich, ETH Zürich"){
    obj <- "ETHZ"
  } else {
    obj <- object.inst[g]
  }
  
  render(
    input = "00_Institutional_Report.Rmd",        # dein Rmd-Dateipfad
    output_file = paste0("./instit_reports/report_", sub(".*,\\s*", "", obj), ".pdf"),  # individuelle Ausgabe
    params = list(object.inst = g),   # Übergabe des Parameters
    envir = new.env()            # wichtig, um Nebeneffekte zu vermeiden
  )
}


