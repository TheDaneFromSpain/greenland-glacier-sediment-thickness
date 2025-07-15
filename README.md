# Greenland glacier sediment thickness

Project started under internship at the department of Geoscience, Aarhus University, with collaboration from the Geological Survey of Denmark and Greenland research institute.

This app pulls pre-processed (by me) geospatial data from a local database and visualizes it using ShinyApp integration to build an interactive HTML webpage, allowing you to see, modify and export the data in real time.

![Shiny app preview](shiny-app.jpg)

<span style="color:red">Removed all datasets except app-crucial GEBCO depth and GRADP thickness because of data confidentiality and excessive size.</span>

Restrictions caused:

	1. Can only select "depth" feature.

	2. Can only select "GRADP, no sd" in training data dropdown.


## Instructions to run

Windows: 

	Either execute "run.bat", which opens the app automatically, or follow the Linux steps.

Linux:

	1. Install R or use the portable version of R already installed as "Application/R/bin/R.exe".
	2. Open the environment "Application/app/Environment.Rproj" and then the app "Application/app/app.R". (or just directly the .R file, but you will have no guarantee of a functioning working directory and will have to follow step 3 of MAC OS fully)
	3. Step 4 of MAC OS. (command <-> ctrl)

MAC OS:

	1. Install R.
	2. Open the environment "Application/app/Environment.Rproj" with R and then the app "Application/app/app.R" if necessary. (or just directly the .R file, but you will have no guarantee of a functioning working directory and will have to follow step 3 fully)
	3. Make sure the working directory is correct by executing getwd() (using command+Enter) and executing setwd(" your-working-directory/Application/app/ ") if necessary.
	4. Uncomment the line that installs the packages (2nd line) if using for the first time and run by executing the last line of code ´´shinyApp(ui = ui, server = server)´´ with command+Enter. (or the button at the top of Rstudio that also runs the app)
