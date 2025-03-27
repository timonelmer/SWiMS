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
load("../data/SWiMS2024_Data_2025-03-25.RData")
unique(dat$institution) # sind die 13 

object.inst <- levels(dat$institution)

# Loop über jede Gruppe
for (g in 1:length(object.inst)) {
  
  if(g == 3){
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


# Feedback von Neele & Patricia
# - Positioning: Phd / PostDoc / Other Researcher with a PhD / Other
# - Cronbach alpha für Depression
# - Depression binär machen nach Timon
# - Schlimm rot besser grün
# - multibar na exclude pro variable nicht allen in plot_data
# - overview wie viele diskriminierung erlebt wurden (anzahl an leuten)
# - Funding includieren im Instituion
# - multibar plot zeit manchmal alpha 
# - timons idee zum cutten items umbenenen nach seiten 
# - instituionell alpha höher (solved)
# - demografische gruppe unter 10 muss weg
