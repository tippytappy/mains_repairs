##Mains Repairs Work

This repo is for my work related to mains repairs:

- preparing the mains repair to pipe (mr2p) script
- validating or improving the mains repairs data


###get_pipe_sizes
Collates alternative pipe sizes to make a set of data for the validation script 


###validate_mr_size
Attempts to validate the pipe sizes entered in the SAP job using other sources of job information:
- sap job
- supply interruptions
- permits to work

The script test various models on the data already gathered to find the approach which best confirms or disputes the size entered on the mains repair.


###prepare_mr2p_table
The MR2P script matches mains repairs to GIS mains using a decision-based approach taking into account job date, job type, mjob code and other factors. It uses a modified version of the SAP mains repairs data; excuding unnecessary columns and adding columns to hold data created by the script.

The script in this repo. performs all of these tasks. It also tests the mains repair coordinates and if they are outside the TW operating area the script tries to find the address by geocoding the XY coordinates (using Bing).

Final output is 
- the MR2P input table
- an exported list of grocoded records
- an exported list of failed geocoding attempts
