# trajclass

Classification of Longitudinal Trajectories / Time Series

## Installation

Go to [releases](https://github.com/agalecki-work/trajclass/releases) page 
and download selected release (in zip format)

## Scripts distributed with the package

* Locate scripts and list them. They are named `step0.R`, `step1.R` and so on. We will refer to them as "step" scripts

```
  (scriptsDir <- system.file("scripts", package = "trajclass"))
  list.files(scriptsDir, pattern = "[[:alnum:]][.][R]$")

```

* Set your working directory

```
 setwd("~") # You are welcome to change working directory to a different folder
 getwd()    # Make sure that working directory is properly set 

```

* Execute "step" scripts

Execute scripts one by one in a sequence starting with `step0.R`. Execute each script in a fresh R session.
For every script log file will be generated in working directory


```
trajclass::runScript_with_log("step0.R") # step0.log file is created in working directory

```   

Important: Locate these scripts and *copy* them to and execute in your working folder.



