This Readme file is available in Word format in the repository as Readme.docx. The formatting will be better if you use that version.

This Readme provides guidance for navigating the code base for the “Management alternatives for climate-smart agriculture in the U.S.: A model ensemble case study” paper by Ellen Maas and Debjani Sihi. It is divided into the following sections:
* Overview of the study and code
* Computer and software setup
* Model acquisition
* Code roadmap

Overview of the study and code
-----------------------------------------------------------
This study utilized three biogeochemical models to investigate the future global warming potential of several alternative agricultural management practices at two sites (Michigan and Texas) with contrasting climates, soils, and cropping systems. Global warming potential in this study is limited to soil sequestration and emissions of CO2, N2O, and CH4 greenhouse gases. Each site provided field measurements of some soil variables, crop yields, and climate conditions. Missing variables were either estimated or pulled from an alternative source. 
This Readme file does not duplicate the methods information provided in the paper, and it is recommended that the paper is read first before these files are explored.

Computer and software setup
-----------------------------------------------------------
The code for this study was run on a Windows 10 computer with 16 GB RAM and an Intel Core i5 processor. The following versions of software were used. All links and contact details were current as of Oct. 5, 2023.
* R – 4.2.1. As of Oct. 5, 2023, available at https://cran.utstat.utoronto.ca/bin/windows/base/rpatched.html
* APSIM Classic – 7.10 r4220 (this is the last version available for APSIM Classic; subsequent versions are under the new APSIM NextGen architecture). To download, start at https://www.apsim.info/download-apsim/ 
* Daycent – EVI version, release 279. Contact SCSC_Consortium@colostate.edu to request.
* RothC – 26.3. Request at https://www.rothamsted.ac.uk/rothamsted-carbon-model-rothc . When installing, use a separate folder from this code initially. This is because the model also creates a number of folders it uses to find input and output files which are included in the code and could be overwritten if RothC is installed in the /RothC/<site> folders. You’ll have to copy the model executable and other supporting program files in each site’s folder, both KBS and LRF. The following files should be copied to each site’s folder:
A_263_1.dll
A_263_2.dll
C14bomb.dat
C14NOBOM.DAT
DFORMD.DLL
DFORRT.DLL
fort.13
fort.99
M_263_32.DLL
RothC_Interface.CAB
RothC_Interface.exe
SETUP.LST
ST6UNST.LOG
The following R packages will need to installed:
apsimx
berryFunctions
broom
data.table
datetime
dplyr
FME
ggplot2
ggpubr
graphics
lubridate
magrittr
nasapower
pracma
readr
readxl
soilDB
soiltexture
sqldf
stringi
stringr
tidyverse
XML
xml2


Code roadmap
-----------------------------------------------------------
The study itself is run entirely by R scripts (*.R). There are a few auxiliary scripts for analysis which are coded to be run as R Markdown (*.Rmd). RStudio was used throughout the development of this project.
To run the full study, it is recommended to run the KBS site first, then LRF.
To run each site in its entirety, run “00_Main_KBS.R” and let it complete. This could take an estimated 24-48 hours. Then run “00_Main_LRF.R” and let it complete. It will take a similar length of time. 
The 00_Main_<site> scripts set up some global variables for the site and controls which climate and management scenarios to run, based on numeric codes for each. These numeric codes are described the “scenario_df” data frame created by the 0_Observations_and_constants_<site> script. It is also written to the “Scenario_table.csv” file in the corresponding site’s results folder (<site>_results_2050). Note that at KBS, the management scenario group numbers run from 3-6, while at LRF, they run from 3-8. The management scenario group numbers indicate the scenario “family” where there are 0-5 different variations on the same treatment. The following are group numbers with multiple variations:
•	Group 4: Reduced fertilizer (4 treatment levels)
•	Group 5: Residue removal (3 and 6 treatment levels at KBS and LRF, respectively, due to no-till versions at LRF but not KBS)
•	Group 6: Biochar addition (5 treatment levels)
These group numbers are followed by the treatment level, numbered 1-# number of levels. For example:
•	41 = reduced fertilizer, 5%
•	42 = reduced fertilizer, 15%
•	51 = remove residue, 50%
•	52 = remove residue, 25%
•	61 = biochar addition, 19 Mg ha-1
•	Etc.
The majority of the steps are executed from the 0_Controller2.R file. The Controller script executes each script in the process. There is a section at the beginning which duplicates the global variable declarations from the 00_Main_<site> scripts, for easier testing of one scenario at a time. It should remain commented out when running the full study. The remaining code is run in the following order:
•	Create soil and other site data files
•	Create management input files for APSIM and Daycent
o	APSIM management is written to a text file which must be manually copied and pasted into its corresponding scenario file. 
	The file is written to /APSIM/<site> named "mgmt_#_#.txt", where the first # is the 1-digit climate scenario number and the second # is the 1- or 2-digit management scenario number. The text in each management file must be copied and pasted into the Operations Schedule model window for that scenario in the corresponding APSIM scenario file, saved, and then run. The APSIM scenario files follow the same naming convention as "scen_#_#.apsim". Note, however, that some scenarios are bundled together to take advantage of APSIM’s “linking” capabilities. In APSIM, scenarios can be created that “point” to a common scenario, then small changes are made. At KBS, the “scen_#_1.apsim” files include groups 4 and 5 scenarios too, because they are all the same as group 1 (the baseline scenario) during the experimental period, then the future management is changed, which is the only difference between them. At LRF, “scen_#_5.apsim” includes all the group 4 and 5 scenarios, because they are extensions of scenario 53, which is the baseline scenario at LRF.
	To copy in the text, follow these steps exactly: 
•	Copy the “mgmt_#_#.txt” text (ctrl-a, copy).
•	Open the applicable scenario file.
•	Expand the + at the “KBS” or “LRF” top level.
•	Expand the + at the “scen_#_#” level that corresponds to the “mgmt_#_#” file.
•	Expand the + at “paddock”.
•	Right-click on “Operations Schedule” and select Delete (this actually works much cleaner than clearing the existing model text and pasting into the old one, which can result in errors when run).
•	At the bottom of the APSIM window, click on “Management”.
•	In the Toolbox window that opens, on the left side, scroll down to “Operations Schedule”.
•	Click on “Operations Schedule”, drag it to the window above and drop it on the word “paddock” which you just expanded. This will create the model within the “paddock” group, which is the required location.
•	Click on the new “Operations Schedule” you just created and a blank window to the right will have two empty columns labeled “Date” and “Action”.
•	Click in the empty cell under “Date” to highlight it, then paste in the text you copied in the first step. All the dates and actions should be in the proper columns, one day per row. If not, delete the Operations Schedule model and start over.
•	Run APSIM and Daycent
o	Every APSIM scenario file must be run manually and separately. To do so:
	Open the first scenario file for the site you are running.
	Click “Run” from the menu bar on the right. Most scenarios will take several minutes to run. If there are errors, there will be a red “X” you can click on to see what the error messages are.
	For .apsim files which contain multiple scenarios, as long as you don’t highlight any individual scenario (or have clicked on the <site> label at the top level), then all the scenarios within the .apsim file will run. Check for the “scen_#_#.out” files, which is the model output.
•	Graph and analyze APSIM and Daycent output
•	Create management input files for RothC (using Daycent output) and run 
o	No extra setup is needed for RothC, however each file must be run individually
	Run RothC from the /RothC/<site> folder. Double click “RothC_Interface.exe”
	Click on Run Models, then the first option “Carbon Model (RothC_26.3)”
	In the dialog window that pops up, navigate to /RothC/<site>/scenario, click on the scenario, and click Open. It will be named “#_#.SET” using the same naming convention as the APSIM files.
	Upon opening the scenario file, the model will start running. There are no indicators that it is doing so. When the model is finished, if it has run 
	successfully, a dialog box will appear saying “Model has finished”. Click “OK” and you can open the next scenario to run.
	Note that the model should only take a few seconds to run per scenario, unlike all the other models which take several minutes.
•	Graph and analyze RothC output
•	Run model ensemble compilations for the climate/management scenario
o	All four models must have been run in the same session, specifically the “9_Results..._setup” scripts for each model. If the output from each model does not have its variables available in the R Environment, there will be errors.
The final steps, running scenario and site compilations, can be run at the end of either 00_Main_KBS or 00_Main_LRF, but both sites have to have been run up to this point before the scripts will run, otherwise there will be errors. Currently, the files are commented out at the end of 00_Main_KBS and active at the end of 00_Main_LRF.
Most scripts, in order to be run independently (such as for testing purposes), must at least have the site's relevant “0_Observations_and_constants_<site_name>.R” script run first as a prerequisite. See the comments in the first code block of “0_Controller.R” for more information.




