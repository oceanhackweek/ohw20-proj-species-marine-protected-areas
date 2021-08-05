# MPA Vizualizer

Goal of this shiny app is to have an interactive UI to visualize the Marine Protected Areas and associate the protection with biodiversity data drawn from OBIS or from user input. Users should be able to:

- Load the marine protected areas (MPAs) as polygons in a global map
- Select MPAs by type of protection or by clicking
- Load OBIS data for a given species or group of species for the selected MPAs
- Upload species abundance table with GPS coordinates and automatically select MPAs that fall within those coordinates
- Generate a table of biodiversity statistics (such as ES50, Alpha diversity) for each MPA based on the loaded data


## Build stages

This shiny app will be built in stages as described below. Header arrange the tasks to sections that may be able to be worked on in parallel.

### Interactive MPAs
- [ ] Outline of UI with dummy inputs and outputs
- [ ] Static image of MPAs
- [ ] Map of MPAs able to be clicked to pull up details about protections
- [ ] Ability to select or filter MPAs interactively

### Loading OBIS records
- [ ] Download OBIS records for a specified polygon
- [ ] Download OBIS records for MPA polygons
- [ ] Dropdown menu for selective download of OBIS records
- [ ] View OBIS records in tabular format

### Loading other public records (physical/chemical oceanography)
- [ ] Like ERDAPP

### User upload of data
- [ ] User is able to upload species abundance data and view locations of samples on map
- [ ] User can see summary statistics for each location
- [ ] MPAs that overlap with user data are highlighted and added to summary statistics

### Combining OBIS and MPA
- [ ] Generate summary statistics and graphs for the biodiversity data in OBIS
- [ ] Visualize other OBIS data (like SST,  etc)