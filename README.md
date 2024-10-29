RegionSizeR is a Shiny application designed for planning the sample size for regions in a multi-regional clinical trial(MRCT), utilizing the preservation of treatment effect method outlined in ICH E17. This application supports various types of endpoints, including continuous, binary, and time-to-event, for superiority, non-inferiority, and MCP-Mod designs(Quan et al.(2010)<doi:10.1002/pst.380> Li et al.(2022) <DOI:10.1201/9781003109785> Yamaguchi et al.(2021) <DOI:10.1080/19466315.2020.1752298>). It also considers specific enrollment characteristics in the region of interest during the planning process.
Note: Despite our validation efforts, it is not guaranteed that all scenarios have been tested. Therefore, we recommend that users perform their own validation based on specific circumstances before using it for any official purposes.

article [DOI: 10.1007/s43441-024-00679-6](https://eur03.safelinks.protection.outlook.com/?url=https%3A%2F%2Flink.springer.com%2Farticle%2F10.1007%2Fs43441-024-00679-6&data=05%7C02%7C%7Cb3e34b689c1543f41f1908dccbfc5c06%7Cfcb2b37b5da0466b9b830014b67a7c78%7C0%7C0%7C638609532034451121%7CUnknown%7CTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7C0%7C%7C%7C&sdata=0f0K2UraivnBB2M59CItdhGKlz3StxZxfQWtRx4ZrAU%3D&reserved=0)

# Install and launch the app

```R
devtools::install_github("rsr-ss/RegionSizeR")

RegionSizeR::launch_RegionSizeR()

```


