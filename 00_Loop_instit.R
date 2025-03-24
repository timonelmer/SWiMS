# Loop through Institutional reports
library(rmarkdown)

# Liste der Gruppen (oder anderer Input-Werte)
object.inst <- institution_counts$inst

# Loop über jede Gruppe
for (g in object.inst) {
  render(
    input = "00_Institutional_Report.Rmd",        # dein Rmd-Dateipfad
    output_file = paste0("report_", sub(".*,\\s*", "", g), ".html"),  # individuelle Ausgabe
    params = list(gruppe = g),   # Übergabe des Parameters
    envir = new.env()            # wichtig, um Nebeneffekte zu vermeiden
  )
}