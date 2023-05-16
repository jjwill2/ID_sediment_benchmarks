# ID_sediment_benchmarks

#####################################################################
Guide to data and R code files associated with draft document
'Development and Evluation of a Framework for Assessing Substrate
Fine Sediment Impacts on Macroinvertebrate Community Composition
in Idaho Streams'

https://github.com/jjwill2/ID_sediment_benchmarks

Jason Williams, Idaho DEQ Lewiston Regional Office
jason.williams@deq.idaho.gov

last updated: May 2023
####################################################################


---------------------------------------------------------------------
File Organization Overview
---------------------------------------------------------------------

The 'raw_data' folder contains data files provided by USFS and BLM
or exported from a DEQ database prior to any processing completed
as part of this project.

The 'formatted_data' folder contains formatted data files created
from raw data either using R scripts or through manual edits

The 'figures' folder contains plot figures generated using
R scrits


---------------------------------------------------------------------
R scripts that format data for FSBI calculations with the FSBI
calculator tool
---------------------------------------------------------------------

formats_sample_macro_data_for_FSBI.R 
creates data required for the 'sample_taxa' tab in the FSBI calculator
input file

formats_unique_taxa_list_forFSBI.R
creates data required for the 'taxa_fsbi' tab in the FSBI calculator
input file

----------------------------------------------------------------------
R scripts that format data to create project_data.csv
----------------------------------------------------------------------

formats_BURP_habitat_type.R
formats raw BURP habitat type data to create burp_habitat_type_formatted.csv,
which is then used by 'formats_BURP_data.R'

formats_BURP_data.R
formats raw DEQ BURP data files to create formatted file 'BURP_data.csv'
that is used for BURP exploratory analysis and to create 
project_data.csv

formats_pibo_siteid_rchid_crosswalk.R
creates crosswalk between PIBO siteid and reachid for use in formatting

formats_tbl_sites.R
formats 'tbl_sites.csv' containing unique sites and relevant attributes
for DEQ-BURP, BLM-AIM, and USFS-PIBO data

formats_project_data.R
formats 'project_data.csv' containing formatted DEQ-BURP, BLM-AIM, and 
USFS-PIBO data used for data analysis

 
----------------------------------------------------------------------
R scripts for data analysis & plotting
----------------------------------------------------------------------

BURP_exploratory_plots.R
exploratory analysis of DEQ BURP data used to select approach to
calculate reference and stressor response benchmarks

summarizes_BURP_reference.R
calculates BURP reference benchmarks

FSBI_logistic_regressions.R
executes logistic regression analysis to develop stressor-response
benchmarks

calculates_performance_statistics.R
applies framework to BURP, BLM-AIM, and USFS-PIBO data to calculate 
framework performance statistics for reference and sediment tmdl reaches

plots_project_data.R
creates various plots from project_data.csv

----------------------------------------------------------------------
# Disclaimer
----------------------------------------------------------------------
Data and R code are DRAFT and provided as-is, and no warranty 
expressed or implied is made regarding the accuracy and functioning
of code and data. Nor shall the fact of distribution constitute any 
such warranty, and no responsibility is assumed by DEQ in connection 
therewith. The data and code were published for the purpose of 
facilitating discussion and review among project participants.
