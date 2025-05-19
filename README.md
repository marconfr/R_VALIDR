     .-.    __
    |   |  /\ \
    |   |  \_\/      __        .-.
    |___|        __ /\ \      /:::\
    |==:|       / /\\_\/     /==::/
    |:::|       \/_/        / `-:/
    ':::'__   _____ _____  /    /
        / /\ /     |==:==\ \   /
        \/_/ \     |==:==/  `"`
              `"""""""""`
    __     __    _ _     _ ____
    \ \   / /_ _| (_) __| |  _ \
     \ \ / / _` | | |/ _` | |_) |
      \ V / (_| | | | (_| |  _ <
       \_/ \__,_|_|_|\__,_|_| \_\

To cite ValidR :

> Frédéric Marçon. (2023). marconfr/R_VALIDR: v1.0 (v1.0). Zenodo. <https://doi.org/10.5281/zenodo.7706081>

Analytical chemistry app written in shiny / R to ease the validation of method of dosing

-\> **ValidR Matrix** : Detection of matrix effects

Based on regression analysis to detect of significant variations in intercepts or slopes

-\> **ValidR Assay** : Method validation with accuracy profiles

Based on the tolerance intervals and accuracy profiles, which guarantee that a pre-defined proportion of future measurements obtained with the method will be included within the acceptance limits.

     ┌─────────────────────┐     ╔════════════════╗       ┌──────────────────────┐
     │MATRIX_TEMPLATE.XLSX │────▶║  ValidR Matrix ║──────▶│ REPORT_MATRIX.html   │
     └─────────────────────┘     ╚════════════════╝       └──────────────────────┘
                                                                                
                                                          ┌──────────────────────┐
                                                      ┌──▶│ REPORT_ASSAY.html    │
     ┌─────────────────────┐     ╔════════════════╗   │   └──────────────────────┘
     │ ASSAY_TEMPLATE.XLSX │────▶║  ValidR Assay  ║───┤                         
     └─────────────────────┘     ╚════════════════╝   │   ┌──────────────────────┐
                                                      └──▶│REPORT_ASSAY_1CAL.html│
                                                          └──────────────────────┘

Hosting at :

-   ValidR Assay : <https://fred-marcon.shinyapps.io/R_PROJET_VALIDR_ASSAY/>

-   ValidR Matrix : <https://fred-marcon.shinyapps.io/R_PROJET_VALIDR_MATRIX/>
