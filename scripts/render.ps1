$error.clear()
cd .\relatorio\
R.exe -e 'knitr::knit("projeto_modelos.Rnw", encoding="UTF-8", output="projeto_modelos.tex")'
if (!$error) {tectonic .\projeto_modelos.tex}
cd ..