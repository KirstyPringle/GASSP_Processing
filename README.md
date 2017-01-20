# GASSP_Processing
Files used to process data collected during the Global Aerosol Science and Synthesis Project (GASSP) from mixed data formats to netCDF format with standard attribute naming.

The GASSP database identifies three levels of data processing:

Level 0 : The raw data from data providers <br>
Level 1 : The same data saved in netCDF format <br>
Level 2 : The data saved in a netCDF format, plus names changed to ensure standard naming and additional attributes added. <br>

The code held within this repository converts Level 1 GASSP data to Level 2, this includes re-naming to GASSP standard names, and adding attribute information.  It is written in Interactive Data Language (IDL).

The IDL proceedure relies on a file called Processed_file_list.txt which contains a list of the files that have been converted to Level 1 format. 


Readme file for 

Script to convert all Level 1 data to Level 2 format (other scripts in this directory convert Level 0 to Level 1).<br>

Reads files from Processed_file_list.txt, specifying which projects to process using “projarr” array.<br>

Uses “read_netCDF” and “write_netCDF” procedures modified from original code (downloaded from the web) to deal with GASSP data files.<br>

“update_posvar_names_cfcompliant” procedure attempts to convert position variable names to CF-compliant names automatically.<br>

“match_varnames_standardnames” procedure attempts to match non-standard aerosol variable names and convert these to the ‘standardised’ names (need to keep adding to this list manually).<br>

“standardise_timestamp” procedure converts primary time variable in file to a standardised time stamp.<br>

“standardise_unit_strings” procedure attempts to match non-standard unit strings and convert these to standardised (not all CF-compliant) unit strings (need to keep adding to this list manually).<br>

Code then creates a new data structure including standardised time variable and all other variables in file (replacing symbols in variable names that are not accepted by IDL data structures).<br>

Additional variable and global attributes are added then the new data structure is written to netCDF.<br>


List of GASSP Attributes

