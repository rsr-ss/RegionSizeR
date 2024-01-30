RegionSizeR is a Shiny application designed for planning the sample size for regions in a multi-regional clinical trial(MRCT), utilizing the preservation of treatment effect method outlined in ICH E17. This application supports various types of endpoints, including continuous, binary, and time-to-event, for superiority, non-inferiority, and MCP-Mod designs(Quan et al.(2010)<doi:10.1002/pst.380> Li et al.(2022) <DOI:10.1201/9781003109785> Yamaguchi et al.(2021) <DOI:10.1080/19466315.2020.1752298>). It also considers specific enrollment characteristics in the region of interest during the planning process.
Note: Despite our validation efforts, it is not guaranteed that all scenarios have been tested. Therefore, we recommend that users perform their own validation based on specific circumstances before using it for any official purposes.

# Install and launch the app

```R
devtools::install_github("rsr-ss/RegionSizeR")

RegionSizeR::launch_RegionSizeR()

```


