;------------------------------------------------------------------------------------------

function STR2NUM, svalue, TYPE = type
;
; NAME:
;	STR2NUM
; PURPOSE:
;	Return the numeric value of string, if possible; other wise
;	return the input string or number.
; CALLING SEQUENCE:
;	result = STR2NUM(svalue)
; INPUT:
;	svalue = a scalar string to be converted to its numeric value or 
;		a numeric value to be converted to its 'smallest' form.
; OPTIONAL KEYWORD INPUT:
;	TYPE - Optional keyword to return the integer scalar that
;		corresponds to the IDL type of the result.
; OUTPUT:
;	Function value = numeric value of input string, or unaltered string
;		if numeric conversion is not possible.
; EXAMPLE:
;	Given a scalar string, svalue = '123', return the numeric value 
;	of svalue or svalue, itself, if numeric conversion not possible.
;	IDL> x = strnum('123')		;convert '123' to its numeric value
;
; PROCEDURE:
;	The input string, svalue, is first tested to see if it is a pds
;	time value by searching for ':' or 'T' in the string; if so it
;	is returned unchanged as a string. The string is then tested to 
;	see if it could be made numeric, by attempting to convert it to 
;	a double precision number. If that succeeds, then further tests 
;	are done to determine what type of numeric value is best used, 
;	and svalue is converted to numeric value. If it fails then svalue 
;	is returned as entered. 
;
;	If a numeric value has a ',' then it is a complex number and
;	is returned as such.
;
;	If a numeric value has a decimal point it is returned as type
;	DOUBLE or FLOAT. If it contains more than 8 numerals, it is
;	returned as type DOUBLE, other wise it is returned as type FLOAT.
;
;	If it contains no decimal point then it could be any numeric type.
;	If the string contains the character 'D', or an absolute numeric 
;	value > 2Billion then it is returned as type DOUBLE. If it contains 
;	the character 'E' it is returned as type FLOAT. If the absolute 
;	value is less than 32767 it is type FLOAT, unless it is between 
;	0 and 255, then it is type BYTE. Otherwise it is type LONG.
;
; HISTORY:
;	Written by John D. Koch, July, 1994
;
;	27 July 1999, M. Barker: fixed bug that converted a date in format of
;				 1991-05-12 to 1991, so that if a '-' is 
;				 detected and neither 'e' or 'E' are detected,
;				 the value is left as a string.
;----------------------------------------------------------------------

 if ( N_PARAMS() NE 1)then begin
     print,'Syntax - result =str2num(svalue,[ TYPE = type])
     return, -1
 endif 

 value = 0

;
;	Check that svalue is a scalar
;
 s = size(svalue)			
 if ( s(0) NE 0 ) then message,'svalue must be a scalar '
 type = 7
;
;	trap value as a string if it is a time expression
;
 if (strpos(svalue,':')GE 0) or (strpos(svalue,'T')GE 0) then goto,THE_CASE
;
;	27 July 1999, M. Barker:
;	trap value as a string if it may be a date expression
;
 if strpos((no_white=strcompress(svalue,/remove_all)),'-') GT 1 then begin
   if strpos(no_white,'e') EQ -1 and strpos(no_white,'E') EQ -1 $
   then goto,THE_CASE
 endif
;
;	
;

 l = strlen(svalue)
 temp = svalue
 ON_IOERROR,THE_CASE		; goto case if conversion on next line fails
 temp = double(temp)
 c=strpos(svalue,',')
 if(c GT -1) and (s(1) EQ 7)then begin
   temp = complex(temp,float(strmid(svalue,c+1,l-c)))
   type=6
 endif
 atemp = abs(temp)
 if type NE 6 then if(strpos(svalue,'.') GT 0) then begin   
   type = 4   
   if(strlen(svalue) GE 8) then type = 5
 endif else begin
   if(atemp GT 32767)then type = 3  else type = 2
   if(temp GT -1) and (temp LT 256)then type = 1
   if(strpos(svalue,'E') GT 0) then type = 4
   if(atemp GT 2000000000) then type = 5 
 endelse
 if(strpos(svalue,'D') GT 0) then type = 5
 ON_IOERROR,NULL
 THE_CASE:
	CASE type OF
	    7 : value=svalue
            6 : value=temp
            5 : value=double(temp);temp
       	    4 : value=float(temp)
       	    3 : value=long(temp)
	    2 : value=fix(temp)
       	    1 : value=byte(temp)
	  else: message,'Flag error in STR2NUM,no corresponding type'
	ENDCASE
 return,value
end
;------------------------------------------------------------------------------------------
;+
; NAME:
;	write_netCDF.pro
;
; PURPOSE:
;	Write netCDF file given a structure variable
;
; CATEGORY:
;	All levels of processing
;
; CALLING SEQUENCE:  
;	write_netCDF, data, filename, status, path=dir_path, att_file=att_filename, /clobber
;
; INPUTS:
;	data = structure variable of input data
;	filename = filename for new netCDF file
;	path = optional directory path for the attributes definition file
;	att_file = optional filename for the attributes definition file
;	clobber = optional option for creating netCDF file
;			clobber means any old file will be destroyed
;
;	An external *.att file is used to define attributes (where * = "data" structure name)
;
; OUTPUTS:  
;	status = result status: 0 = OK_STATUS, -1 = BAD_PARAMS, -2 = BAD_FILE,
;			-3 = BAD_FILE_DATA, -4 = FILE_ALREADY_OPENED
;
;	A netCDF file is created and written.
;
; COMMON BLOCKS:
;	None
;
; PROCEDURE:
;	Check for valid input parameters
;	Open the netCDF file
;	Use the structure's tag names for defining the variable names in the netCDF.
;	Use the structure name and optional 'path' variable for the Attributes filename
;		OR use the optional 'att_file' parameter for this filename
;	If this Attributes definition file exists, then transfer those attributes into the netCDF file
;		OR else don't write any attributes to the netCDF file.
;	Once netCDF variables and attributes are defined, then write the structure's data to netCDF file
;	Close the netCDF file
;
;	NetCDF IDL Procedures / Process:
;	1. NCDF_CREATE: Call this procedure to begin creating a new file. The new file is put into define mode.
;	2. NCDF_DIMDEF: Create dimensions for the file.
; 	3. NCDF_VARDEF: Define the variables to be used in the file.
;	4. NCDF_ATTPUT: Optionally, use attributes to describe the data.  Global attributes also allowed.
;	4. NCDF_CONTROL, /ENDEF: Leave define mode and enter data mode.
;	5. NCDF_VARPUT: Write the appropriate data to the netCDF file.
;	6. NCDF_CLOSE: Close the file.
;
; MODIFICATION HISTORY:
;	9/20/99		Tom Woods		Original release code, Version 1.00
;
;+

pro write_netCDF, data, filename, tagname_arr, tagrspn_arr, num_var2, $
                  var_natts, varatt, varatt_val, status, dim_name, dim_size, clobber=clobber

print, 'Writing data to GASSP Level 2 netCDF file'
;print, tag_names(data)
;
;	Generic "status" values
;
OK_STATUS = 0
BAD_PARAMS = -1
BAD_FILE = -2
BAD_FILE_DATA = -3
FILE_ALREADY_OPENED = -4

debug_mode = 0		; set to >= 1 if want to debug this procedure
					; set to 2 if want to debug and force directory to special Woods Mac directory

;
;	check for valid parameters
;
status = BAD_PARAMS
if (n_params(0) lt 1) then begin
	print, 'USAGE: write_netCDF, data, filename, status, path=dir_path, att_file=att_filename, /clobber'
	return
endif
dsize = size(data)
if (dsize[0] ne 1) or (dsize[2] ne 8) then begin
	print, 'ERROR: write_netCDF requires the data to be a structure array'
	return
endif
if (n_params(0) lt 2) then begin
	filename = ''
	read, 'Enter filename for the new netCDF file : ', filename
	if (strlen(filename) lt 1) then return
endif
dir_path = ''
att_filename = tag_names( data, /structure_name ) + '.att'
if keyword_set(path) then dir_path = path
if keyword_set(att_file) then att_filename = att_file
att_filename = dir_path + att_filename

;
;	Do initial survery of variables and nested structures
;	to verify limitation on dimensions of arrays and nested structures
;	
;	LIMITATIONS:  4 dimensions on arrays and 4 nested structures
;
;	Use internal name structure for tracking any nested structures
;
temp_def = { name : ' ', isVar : 0B, tag_index : 0L, var_size : lonarr(10), nest_level : 0, $
	struct_index : lonarr(4), dim_index : lonarr(16), var_ptr : ptr_new() }
var_def = temp_def

;
;	define first structure entry into "var_def" for the "data" structure
;
var_def[0].name = tag_names( data, /structure_name )
var_def[0].isVar = 0
var_def[0].tag_index = 0
var_def[0].var_size = size( data )
var_def[0].nest_level = 0
temp_index = lonarr(4)
var_def[0].struct_index = temp_index
temp_dim = lonarr(16) - 1
var_def[0].dim_index = temp_dim
var_def[0].var_ptr = ptr_new(data[0])

next_var = 1
level_index = lonarr(5)
level_index[0] = 1
extra_var = n_tags( data )
nest_level = 0

;help,data,/structure


while (extra_var gt 0) and (nest_level le 4) do begin
	;
	; each level of nested structures are appended to var_def
	;
	var_def = [ var_def, replicate( temp_def, extra_var ) ]
	if (nest_level gt 0) then j_start = level_index[nest_level-1] else j_start = 0
	j_end = level_index[nest_level] - 1
	extra_var = 0	
	for j=j_start, j_end do begin
		;
		; only process structure definitions
		;
		if ( var_def[j].isVar eq 0 ) then begin
			theData = *(var_def[j].var_ptr)
			tnames = tag_names( theData )
                        ;print, tnames
			temp_index = var_def[j].struct_index
			k_total = n_tags( theData ) - 1
			for k= 0, k_total do begin
				theVar = theData[0].(k)
				theName = ''
				nn = var_def[j].nest_level
				if ( nn gt 0 ) then begin
					theName = var_def[ var_def[j].struct_index[nn-1] ].name + '.'
				endif
				theName = theName + tnames[k]
				var_def[next_var].name = theName
				var_def[next_var].isVar = 1
				var_def[next_var].tag_index = k
				var_def[next_var].nest_level = nest_level
				var_def[next_var].struct_index = temp_index
				var_def[next_var].dim_index = temp_dim
				tempsize = size( theVar )
				if (tempsize[0] gt 4) then begin
					print, 'ERROR:  write_netCDF  has a limitation of 4 dimensions for its variables'
					print, 'ABORTING....'
					; NCDF_CONTROL, fid, /ABORT
					return
				endif
				var_def[next_var].var_size = tempsize
				var_def[next_var].var_ptr = ptr_new( theVar )
				;
				;	if structure, then need to set it up special
				;
				if (tempsize[tempsize[0]+1] eq 8) then begin
					var_def[next_var].isVar = 0
					var_def[next_var].nest_level = nest_level + 1
					var_def[next_var].struct_index[nest_level] = next_var
					extra_var = extra_var + n_tags( theVar[0] )
				endif
				next_var = next_var + 1
			endfor
		endif
	endfor
	;
	;	get ready for next level of nested structures
	;
	nest_level = nest_level + 1
	level_index[nest_level] = next_var
endwhile

num_var = next_var		; the maximum number of variables for netCDF file (size of var_def)
if (num_var ne n_elements(var_def)) then begin
	print, 'WARNING: write_netCDF has error in pre-parsing for variable definitions'
endif

if (extra_var gt 0) then begin
	print, 'ERROR:  write_netCDF  has a limitation of 4 nested structures for its variables'
	print, 'ABORTING....'
	; NCDF_CONTROL, fid, /ABORT
	return
endif

;if (debug_mode gt 0) then stop, 'Check out "var_def" structure results...'

;
;	Open the netCDF file - option to CLOBBER any existing file
;
status = BAD_FILE
if keyword_set(clobber) then fid = NCDF_CREATE( filename, /CLOBBER ) $
else fid = NCDF_CREATE( filename, /NOCLOBBER )
status = OK_STATUS

;
;	Define the netCDF dimensions
;	Use the size() function to make dimensions
;	Define the dimension of the structure itself as UNLIMITED (in case want to append to this file)
;
ndimmax=n_elements(dim_size)
for idim=0,ndimmax-1 do begin
   var_dim = NCDF_DIMDEF( fid, STRUPCASE(dim_name[idim]), dim_size[idim] );;dim_name[idim]
   if strmatch(dim_name[idim],'flag_length',/fold_case) eq 1 then $
   str_did=var_dim ;need string length for AMF station files
   if idim eq 0 then dim_id=var_dim $
   else dim_id=[dim_id,var_dim]
endfor

; if (debug_mode gt 0) then stop, 'Check out the var_def.dim_index[]...'

if (debug_mode gt 0) then begin
   print, ' '
   print, 'Number of structures / variables = ', num_var
   print, ' '
   print, 'Defining dimensions and variables...'
   print, '    Index   Dimensions   Data-Type   Name'
   print, '    -----   ----------   ---------   ----'
endif
;
;	Now define the netCDF variables
;	Use the structure's tag names for defining the variable names in the netCDF
;
first_var=0
for k=0,num_var-1 do begin
	;
	;  only process real variables (not structure definitions)
	;
   if (var_def[k].isVar ne 0) then begin
      var_size = var_def[k].var_size
      data_type = var_size[ var_size[0] + 1 ]
      var_ndim = var_def[k].var_size[0]

      if var_ndim eq 0 then var_ndim=1 ;***FUDGE***
      
      if (debug_mode gt 0) then print, k, var_ndim, data_type, '   ', var_def[k].name
      ;
      ;-Get size of dimensions in variable
      for idim=0,var_ndim-1 do begin
         if idim eq 0 then var_dims=var_def[k].var_size[idim+1] $
         else var_dims=[var_dims,var_def[k].var_size[idim+1]]
      endfor
      index=0
      for idim=0,var_ndim-1 do begin
         ;-Find which dim_size the var_dims correspond to (store index)
         var_dim_id=where(dim_size eq var_dims[idim],nvals)

         ;-Check for multiple matches of dimensions
         if nvals gt 1 then begin
            vardim_match=where(strmatch(dim_name[var_dim_id],$
                                        var_def[k].name,/fold_case) eq 1,nmatchvals)
            ;;If dimension and variable name match then set that dimension
            if nmatchvals eq 1 then var_dim_id=var_dim_id[vardim_match] $
            else begin
               ;;-For AOE1996 data file: DMPS_INT_AOE1996_960723.dat.nc
               if strmatch(var_def[k].name,'LATITUDE') eq 1 or $
                  strmatch(var_def[k].name,'LONGITUDE') eq 1 or $
                  strmatch(var_def[k].name,'N5') eq 1 then $
                     var_dim_id=var_dim_id[where(strmatch(dim_name[var_dim_id],$
                                                          'time',/fold_case) eq 1,nmatchvals)] $
               else var_dim_id=var_dim_id[index];For ARM AMF station data
               index++
            endelse
            ;print,dim_name[var_dim_id],var_def[k].name,var_dim_id,nmatchvals
         endif
         
         ;-Define the dimensions of the variable in terms 
         ;-of NCDF_DIMDEF (using dim_id array) and make dimnsions array
         if idim eq 0 then the_dim=dim_id[var_dim_id] $
         else the_dim=[the_dim,dim_id[var_dim_id]]
         ;print, var_def[k].name, idim, var_dims[idim], dim_size[var_dim_id], data_type
      endfor
      ;
      ;	now make variable in a big case statement now for different data type
      ;
      case data_type of 
         1:	var_defid = NCDF_VARDEF( fid, var_def[k].name, the_dim, /BYTE )
         2:	var_defid = NCDF_VARDEF( fid, var_def[k].name, the_dim, /SHORT )
         3:	var_defid = NCDF_VARDEF( fid, var_def[k].name, the_dim, /LONG )
         4:	var_defid = NCDF_VARDEF( fid, var_def[k].name, the_dim, /FLOAT )
         5:	var_defid = NCDF_VARDEF( fid, var_def[k].name, the_dim, /DOUBLE )
         7:	var_defid = NCDF_VARDEF( fid, var_def[k].name, [str_did, the_dim], /CHAR )
         12:	var_defid = NCDF_VARDEF( fid, var_def[k].name, the_dim, /SHORT )
         13:	var_defid = NCDF_VARDEF( fid, var_def[k].name, the_dim, /LONG )
         14:	var_defid = NCDF_VARDEF( fid, var_def[k].name, the_dim, /LONG) ;DOUBLE);UINT64 )       
         else: begin
            print, 'WARNING: write_netCDF error in variable type, assuming float'
            var_defid = NCDF_VARDEF( fid, var_def[k].name, the_dim ) ; assume it is /FLOAT ???
         end
      endcase
      if (first_var eq 0) then var_id = replicate( var_defid, num_var )
      first_var = 1
      var_id[k] = var_defid
    endif 
endfor

;if (debug_mode gt 0) then stop, 'Check out the "var_id"...'

;
;	Use the structure name and optional 'path' variable for the Attributes filename
;		OR use the optional 'att_file' parameter for this filename
;	If this Attributes definition file exists, then transfer those attributes into the netCDF file
;		OR else don't write any attributes to the netCDF file.
;

ntags=n_elements(tagname_arr)
for itag=0,ntags-1 do begin
   if tagname_arr[itag] eq 'File_N_Var' then goto,skiptag
   if tagname_arr[itag] eq 'File_Var_Name' then goto,skiptag
   if tagname_arr[itag] eq 'Output_Variable' then goto,skiptag
   if tagname_arr[itag] eq 'Output_Variable_2D' then goto,skiptag
   if tagname_arr[itag] eq 'Output_Variable_2D_Units' then goto,skiptag
   if tagname_arr[itag] eq 'Output_Variable_2D_Missing' then goto,skiptag
   if tagname_arr[itag] eq 'Time_Stamp_Info' then goto,skiptag
   ncdf_attput,fid,/global,/char,tagname_arr[itag],tagrspn_arr[itag]
   ;print,'Inserting tag: ',tagname_arr[itag]+' = '+tagrspn_arr[itag]
   skiptag:
endfor
for k=0,num_var2-1 do begin
    for jj=0,var_natts[k]-1 do begin
       ;;if (varatt[k,jj] eq 'units') or (varatt[k,jj] eq 'missing_value') or $
       ;; ;;;****NOT ONLY THESE VARIABLE ATTRIBUTES***
       ;;   (varatt[k,jj] eq 'long_name') then begin
          if (varatt[k,jj] eq 'missing_value') or $
             (varatt[k,jj] eq '_FillValue') or $
             (varatt[k,jj] eq 'scale_factor') or $
             (varatt[k,jj] eq 'add_offset') or $
             (varatt[k,jj] eq 'ASTG') or $
             (varatt[k,jj] eq 'SFCT') or $
             (varatt[k,jj] eq 'SampledRate') or $
             (varatt[k,jj] eq 'OutputRate') or $
             (varatt[k,jj] eq 'VectorLength')  or $
             (varatt[k,jj] eq 'valid_min')  or $
             (varatt[k,jj] eq 'valid_max')  or $
             (varatt[k,jj] eq 'DespikeSlope')  or $
             (varatt[k,jj] eq 'scale') then begin
             if FINITE(varatt_val[k,jj]) eq 1 then begin
                varatt_val_temp=STR2NUM((varatt_val[k,jj]),TYPE=attype)
                THE_CASE:
                CASE attype OF
                   7 : NCDF_ATTPUT, fid, k, varatt[k,jj], varatt_val[k,jj]
                   5 : NCDF_ATTPUT, fid, k, varatt[k,jj], varatt_val_temp, /FLOAT;/DOUBLE;
                   4 : NCDF_ATTPUT, fid, k, varatt[k,jj], varatt_val_temp, /FLOAT ;value=float(temp)
                   3 : NCDF_ATTPUT, fid, k, varatt[k,jj], varatt_val_temp, /LONG  ;value=long(temp)
                   2 : NCDF_ATTPUT, fid, k, varatt[k,jj], varatt_val_temp, /LONG  ;value=fix(temp)
                   1 : NCDF_ATTPUT, fid, k, varatt[k,jj], varatt_val_temp, /LONG  ;value=byte(temp)
                   else: NCDF_ATTPUT, fid, k, varatt[k,jj], varatt_val_temp
                ENDCASE
             endif else begin
                NCDF_ATTPUT, fid, k, varatt[k,jj], !Values.F_NAN
             endelse 
          endif else begin
             varatt_val_temp=varatt_val[k,jj]
             if varatt_val_temp ne '' then NCDF_ATTPUT, fid, k, varatt[k,jj], varatt_val_temp
          endelse
          ;print, 'Inserting variable attribute: ',k,'  '+varatt[k,jj]+' = ', varatt_val[k,jj]
       ;;endif
    endfor
 endfor

;stop

on_ioerror, NULL

;
;	Once netCDF variables and attributes are defined, then write the structure's data to netCDF file
;
NCDF_CONTROL, fid, /ENDEF;exit define mode

for k=0,num_var-1 do begin
	;
	;  only process real variables (not structure definitions)
	;
	if (var_def[k].isVar ne 0) then begin
		ti = var_def[k].struct_index
		k_ti = var_def[k].tag_index
		ti_0 = var_def[ti[0]].tag_index
		ti_1 = var_def[ti[1]].tag_index
		ti_2 = var_def[ti[2]].tag_index
		ti_3 = var_def[ti[3]].tag_index

                case  var_def[k].nest_level of
			0 :        theData = data.(k_ti)
			1 : theData = data.(ti_0).(k_ti)
			2 : theData = data.(ti_0).(ti_1).(k_ti)
			3 : theData = data.(ti_0).(ti_1).(ti_2).(k_ti)
			4 : theData = data.(ti_0).(ti_1).(ti_2).(ti_3).(k_ti)
		else : begin
			print, 'WARNING: write_netCDF has error in parsing data for writing'
			theData = 0.0
			end
		endcase
                NCDF_VARPUT, fid, var_id[k], theData
                ;help, thedata
                ;print, max(thedata)
	endif
endfor
;
;	Close the netCDF file
;
NCDF_CLOSE, fid

;
;	clean up pointer heap before leaving
;
num_var_def = n_elements( var_def )
for k=0,num_var_def-1 do begin
	if ( ptr_valid( var_def[k].var_ptr ) ) then ptr_free, var_def[k].var_ptr
endfor

return
end
;-----------------------------------------------------------------------------------

;+
; NAME:
;	read_netCDF.pro
;
; PURPOSE:
;	Read netCDF file into structure variable
;
; CATEGORY:
;	All levels of processing
;
; CALLING SEQUENCE:  
;	read_netCDF, filename, data, attributes, status
;
; INPUTS:
;	filename = filename for existing netCDF file
;
; OUTPUTS:  
;	data = structure variable for data read from netCDF file
;	attributes = array of strings of the attributes from the netCDF file
;	status = result status: 0 = OK_STATUS, -1 = BAD_PARAMS, -2 = BAD_FILE,
;			-3 = BAD_FILE_DATA, -4 = FILE_ALREADY_OPENED
;
; COMMON BLOCKS:
;	None
;
; PROCEDURE:
;	Check for valid input parameters
;	Open the netCDF file
;	Create structures based on the netCDF definitions
;	Once structures are defined, then read the netCDF variables into the structure's data
;	Read the attributes into a string array
;	Close the netCDF file
;
;	NetCDF IDL Procedures / Process:
;	1.	NCDF_OPEN: Open an existing netCDF file.
;	2.	NCDF_INQUIRE: Call this function to find the format of the netCDF file.
;	3.	NCDF_DIMINQ: Retrieve the names and sizes of dimensions in the file.
;	4.	NCDF_VARINQ: Retrieve the names, types, and sizes of variables in the file.
;	5.	NCDF_ATTINQ: Optionally, retrieve the types and lengths of attributes.
;	6.	NCDF_ATTNAME: Optionally, retrieve attribute names.
;	7.	NCDF_ATTGET: Optionally, retrieve the attributes.
;	8.	NCDF_VARGET: Read the data from the variables.
;	9.	NCDF_CLOSE: Close the file.
;
; MODIFICATION HISTORY:
;	9/20/1999		Tom Woods		Original release of code, Version 1.00
;	12/3/1999		Tom Woods		Removed BYTE array conversion to STRING
;
; $Log: read_netcdf.pro,v $
; Revision 1.1.1.1  2000/11/21 21:49:17  dlwoodra
; SEE Code Library Import
;
;
;idver='$Id: read_netcdf.pro,v 1.1.1.1 2000/11/21 21:49:17 dlwoodra Exp $'
;
;+

pro read_netCDF, filename, data, attributes, status, dim_name, dim_size, $
                 globatts, globatts_val, varatts, varatts_val, nvaratt

;
;	Generic "status" values
;
OK_STATUS = 0
BAD_PARAMS = -1
BAD_FILE = -2
BAD_FILE_DATA = -3
FILE_ALREADY_OPENED = -4

debug_mode = 0			; set to 1 if want to debug this procedure

;
;	check for valid parameters
;
status = BAD_PARAMS
if (n_params(0) lt 1) then begin
	print, 'USAGE: read_netCDF, filename, data, attributes, status'
	return
endif
if (n_params(0) lt 2) then begin
	filename = ''
	read, 'Enter filename for the existing netCDF file : ', filename
	if (strlen(filename) lt 1) then return
endif

status = OK_STATUS

if (debug_mode gt 2) and ( !d.name eq 'MAC' ) then begin
	SEE_MAC_CODE = !dir + ':SEE DPS ï¿½:'
	full_file = SEE_MAC_CODE + 'see_data:' + filename
endif else begin
	full_file = filename
endelse

;
;	Open the netCDF file
;	1.	NCDF_OPEN: Open an existing netCDF file.
;
if (debug_mode gt 0) then print, 'Opening ', filename, ' ...'
fid = NCDF_OPEN( full_file, /NOWRITE )

;
;	Create structures based on the netCDF definitions
;	2.	NCDF_INQUIRE: Call this function to find the format of the netCDF file.
;	3.	NCDF_DIMINQ: Retrieve the names and sizes of dimensions in the file.
;	4.	NCDF_VARINQ: Retrieve the names, types, and sizes of variables in the file.
;
finq = NCDF_INQUIRE( fid )		; finq /str = ndims, nvars, ngatts, recdim

;
;	get dimension definitions first
;	get unlimited dimension (finq.recdim)
;	
dim_unlimited = finq.recdim		; = -1 if undefined, otherwise index into dim array
if ( finq.ndims gt 0 ) then begin
	dimstr = ' '
	dimsize = 0L
	dim_name = strarr( finq.ndims )
	dim_size = lonarr( finq.ndims )
	for k=0,finq.ndims-1 do begin
		NCDF_DIMINQ, fid, k, dimstr, dimsize
		dim_name[k] = dimstr
		dim_size[k] = dimsize
	endfor
endif

;
;	get variable definitions next
;	also determine nested structure levels, max. dimension, and command dimension value
;
;	LIMITATION: 6 dimensions allowed per variable
;	netCDF does not really define unsigned variable types
;
;	Have internal structure definition for tracking variables / structures
;		name = name from netCDF file
;		var_name = name from structure definition (last word after last '.')
;		type = data type value (same values as used by size())
;		natts = number of attributes for this variable
;		ndims = number of dimensions in "dim"
;		dim = dimension index into dim_size[]
;		nest_level = nest level of structures (number of '.' in name)
;		nest_name = structure name (nested)
;		nest_id = index to first case of structure name (nested)
;		nest_cnt = index of variable within a single structure (nested)
;		ptr = data variable pointer
;		str_ptr = structure pointer (if first case of new structure)
;		
var_inq1 = { name : " ", var_name : " ", type : 0, natts : 0L, ndims : 0L, dim: lonarr(8), nest_level : 0, $
	nest_name: strarr(6), nest_id : lonarr(6), nest_cnt : lonarr(6), ptr : PTR_NEW(), str_ptr : PTRARR(6) }

var_inq = replicate( var_inq1, finq.nvars )
max_level = 0			; track max structure nest level while getting variable definitions
max_dim = 1			; track max base structure dimension required
has_common_dim = 1		; assume TRUE to start out, any conflict makes it FALSE

;
;	sort out first the dimensions and attribute numbers
;	check for max. dim needed for base structure
;	and if should have base structure array (if all the same last dim)
;
for k=0, finq.nvars-1 do begin
   var_def = NCDF_VARINQ( fid, k )
   var_inq[k].ndims = var_def.ndims
   var_inq[k].natts = var_def.natts

   if (var_def.ndims gt 0) then begin
      for j=0, var_def.ndims-1 do var_inq[k].dim[j] = var_def.dim[j]
   endif
   if (var_def.ndims gt 0) then begin
      lastdim = dim_size[ var_def.dim[var_def.ndims-1] ]
      if (lastdim gt max_dim) then max_dim = lastdim
      if (var_inq[k].dim[var_inq[k].ndims-1] ne var_inq[0].dim[var_inq[0].ndims-1]) then has_common_dim = 0
   endif else has_common_dim = 0
endfor

if (debug_mode gt 0) then begin
	print, ' '
	if (has_common_dim) then print, 'Array dimension for base structure = ', strtrim(max_dim, 2) $
	else print, 'Single structure element will be defined - max dim. seen though is ', strtrim(max_dim, 2)
endif

if (has_common_dim eq 0) then max_dim = 1		; make single-element structure only

str_dim_limit = 1					; define limit for converting BYTE array into STRING
if (has_common_dim) then str_dim_limit = 2

;
;	now define variables
;
for k=0, finq.nvars-1 do begin
	var_def = NCDF_VARINQ( fid, k )
        if strmatch(var_def.name,'*-*') ge 1 then begin
           temp=strjoin(strsplit( var_def.name,'-',/extract),'_')
           var_def.name=temp
           print, var_def.name
        endif
        if strmatch(var_def.name,'*.*') ge 1 then begin
           temp=strjoin(strsplit( var_def.name,'.',/extract),'_')
           var_def.name=temp
           print, var_def.name
        endif
        var_inq[k].name = var_def.name
        ;print, var_inq[k].name

        ;;-Time variables stored as doubles but should be LONG64
        ;;if STRMATCH(var_def.name,'*time*',/FOLD_CASE) $
        ;;   EQ 1 then var_def.datatype='UINT64'

        case strupcase(var_def.datatype) of 
           'BYTE': begin
              theType = 1       ; use size() definitions for data type numbers
	      ; if (var_def.ndims ge str_dim_limit) then begin
	       ; if (debug_mode gt 0) then print, 'Forcing STRING type for ', var_def.name
	       ; theType = 7
	      ; endif
           end
           'CHAR': begin
              theType = 7       ; expect STRING type
              if (debug_mode gt 0) then print, 'STRING type for ', var_def.name
           end
           'SHORT': theType = 2
           'LONG': theType = 3
           'DOUBLE': theType = 5
           'UINT64': theType = 8
           else: theType = 4    ; default is FLOAT
        endcase
        ;print, var_inq[k].name,thetype
	;
	;	set up structure variable definitions, assume nest level 0 before looking for '.'
	;  increase nest_level for each '.' found and fill in nest_name, nest_id[], nest_cnt[]
	;
	var_inq[k].type = theType
	var_inq[k].nest_level = 0
	for ii=0,5 do begin
		var_inq[k].nest_name[ii] = ''
		var_inq[k].nest_id[ii] = 0
		var_inq[k].nest_cnt[ii] = 0
	endfor
	var_inq[k].nest_id[0] = 0
	if (k eq 0) then var_inq[k].nest_cnt[0] = 0 $
	else var_inq[k].nest_cnt[0] =  var_inq[k-1].nest_cnt[0] + 1
	dotpos = 0
	while (dotpos ge 0) do begin
		lastpos = dotpos
		dotpos = strpos( var_def.name, '.', lastpos )
		if (dotpos ge 0) then begin
			var_inq[k].nest_level = var_inq[k].nest_level + 1
			nn = var_inq[k].nest_level
			if (nn gt max_level) then max_level = nn
			if (nn gt 5) then begin
				print, 'ERROR: write_netCDF can not handle more than 4 nested structures !'
				print, 'Aborting...'
				NCDF_CONTROL, fid, /ABORT
				status = BAD_FILE_DATA
				return
			endif
			newname = strmid(var_def.name, lastpos, dotpos-lastpos)
			var_inq[k].nest_name[nn] = newname
			if (k eq 0) then k1=0 else k1 = k - 1
			if (k ne 0) and ( var_inq[k1].nest_level ge nn ) and (var_inq[k1].nest_name[nn] eq newname) then begin
				var_inq[k].nest_cnt[nn-1] = var_inq[k].nest_cnt[nn-1] - 1
				var_inq[k].nest_id[nn] = var_inq[k1].nest_id[nn]
				var_inq[k].nest_cnt[nn] = var_inq[k1].nest_cnt[nn] + 1				
			endif else begin
				var_inq[k].nest_id[nn] = k
				var_inq[k].nest_cnt[nn] = 0
			endelse
			dotpos = dotpos + 1
		endif
	endwhile
	var_inq[k].var_name = strmid( var_def.name, lastpos, strlen(var_def.name) - lastpos )
	;
	;	now define variable and save as PTR
	;	uses dumb dimension rules : 
	;		ndim_var = ndim_total - 1			for base structure being an array
	;		if (CHAR) then ndim_var = ndim_var - 1		for string definitions
	;
	ndim_array = var_inq[k].ndims
        
	if (has_common_dim) then ndim_array = ndim_array - 1
	if (var_inq[k].type eq 7) then ndim_array = ndim_array - 1
	if (ndim_array lt 0) then ndim_array = 0
	case ndim_array of
		0:	begin
			case var_inq[k].type of 
				1: theData = 0B
				2: theData = 0
				3: theData = 0L
				5: theData = 0.0D0
				7: theData = ''
                                8: theData = 0LL
				else: theData = 0.0
			endcase
			end
		1:  begin
			case var_inq[k].type of 
				1: theData = bytarr( dim_size[ var_inq[k].dim[0] ] )
				2: theData = intarr( dim_size[ var_inq[k].dim[0] ] )
				3: theData = lonarr( dim_size[ var_inq[k].dim[0] ] )
				5: theData = dblarr( dim_size[ var_inq[k].dim[0] ] )
				7: theData = strarr( dim_size[ var_inq[k].dim[1] ] )	; offset 1 Dim for char array
                                8: theData = lon64arr( dim_size[ var_inq[k].dim[0] ] )
				else: theData = fltarr( dim_size[ var_inq[k].dim[0] ] )
			endcase
			end
		2:  begin
			case var_inq[k].type of 
				1: theData = bytarr( dim_size[ var_inq[k].dim[0] ], dim_size[ var_inq[k].dim[1] ] )
				2: theData = intarr( dim_size[ var_inq[k].dim[0] ], dim_size[ var_inq[k].dim[1] ] )
				3: theData = lonarr( dim_size[ var_inq[k].dim[0] ], dim_size[ var_inq[k].dim[1] ] )
				5: theData = dblarr( dim_size[ var_inq[k].dim[0] ], dim_size[ var_inq[k].dim[1] ] )
				;;7: theData = strarr( dim_size[ var_inq[k].dim[1] ], dim_size[ var_inq[k].dim[2] ] );orig_code
                                7: theData = strarr( dim_size[ var_inq[k].dim[1] ] ) ; offset 1 Dim for char array 
                                8: theData = lon64arr( dim_size[ var_inq[k].dim[0] ], dim_size[ var_inq[k].dim[1] ] )
				else: theData = fltarr( dim_size[ var_inq[k].dim[0] ], dim_size[ var_inq[k].dim[1] ] )
			endcase
			end
		3: 	begin
			case var_inq[k].type of 
				1: theData = bytarr( dim_size[ var_inq[k].dim[0] ], dim_size[ var_inq[k].dim[1] ], $
								dim_size[ var_inq[k].dim[2] ] )
				2: theData = intarr( dim_size[ var_inq[k].dim[0] ], dim_size[ var_inq[k].dim[1] ], $
								dim_size[ var_inq[k].dim[2] ]  )
				3: theData = lonarr( dim_size[ var_inq[k].dim[0] ], dim_size[ var_inq[k].dim[1] ], $
								dim_size[ var_inq[k].dim[2] ]  )
				5: theData = dblarr( dim_size[ var_inq[k].dim[0] ], dim_size[ var_inq[k].dim[1] ], $
								dim_size[ var_inq[k].dim[2] ]  )
				7: theData = strarr( dim_size[ var_inq[k].dim[1] ], dim_size[ var_inq[k].dim[2] ], $
								dim_size[ var_inq[k].dim[3] ]  )		; offset 1 Dim for char array
                                8: theData = lon64arr( dim_size[ var_inq[k].dim[0] ], dim_size[ var_inq[k].dim[1] ], $
                                                                dim_size[ var_inq[k].dim[2] ]  )
				else: theData = fltarr( dim_size[ var_inq[k].dim[0] ], dim_size[ var_inq[k].dim[1] ], $
								dim_size[ var_inq[k].dim[2] ]  )
			endcase
			end
		4:	begin
			case var_inq[k].type of 
				1: theData = bytarr( dim_size[ var_inq[k].dim[0] ], dim_size[ var_inq[k].dim[1] ], $
								dim_size[ var_inq[k].dim[2] ], dim_size[ var_inq[k].dim[3] ] )
				2: theData = intarr( dim_size[ var_inq[k].dim[0] ], dim_size[ var_inq[k].dim[1] ], $
								dim_size[ var_inq[k].dim[2] ], dim_size[ var_inq[k].dim[3] ]  )
				3: theData = lonarr( dim_size[ var_inq[k].dim[0] ], dim_size[ var_inq[k].dim[1] ], $
								dim_size[ var_inq[k].dim[2] ], dim_size[ var_inq[k].dim[3] ]  )
				5: theData = dblarr( dim_size[ var_inq[k].dim[0] ], dim_size[ var_inq[k].dim[1] ], $
								dim_size[ var_inq[k].dim[2] ], dim_size[ var_inq[k].dim[3] ]  )
				7: theData = strarr( dim_size[ var_inq[k].dim[1] ], dim_size[ var_inq[k].dim[2] ], $
								dim_size[ var_inq[k].dim[3] ], dim_size[ var_inq[k].dim[4] ]  )	
                                8: theData = lon64arr( dim_size[ var_inq[k].dim[0] ], dim_size[ var_inq[k].dim[1] ], $
								dim_size[ var_inq[k].dim[2] ], dim_size[ var_inq[k].dim[3] ]  )
				else: theData = fltarr( dim_size[ var_inq[k].dim[0] ], dim_size[ var_inq[k].dim[1] ], $
								dim_size[ var_inq[k].dim[2] ], dim_size[ var_inq[k].dim[3] ]  )
			endcase
			end
		else: begin
			print, 'ERROR: read_netCDF can only handle 4 dimensions for arrays'
			print, 'Aborting...'
			NCDF_CONTROL, fid, /ABORT
			status = BAD_FILE_DATA
			return
			end
	endcase
	var_inq[k].ptr = PTR_NEW( theData )
endfor

if (debug_mode gt 0) then begin
	print, ' '
	nvar = n_elements( var_inq )
	print, 'Indx Lvl -- 0  1 ID 2  3--< 0  1 CT 2  3 >  NAME'
	for jj=0,nvar-1 do print, jj, var_inq[jj].nest_level, var_inq[jj].nest_id[0:3], var_inq[jj].nest_cnt[0:3], $
		var_inq[jj].name, form="(10I4,'   ',A)"
	;stop, 'Check out var_inq and dim_name, dim_size...'
endif

;
;	define structures based on var and dim definitions from netCDF file
;	using anonymous structure name with CREATE_STRUCT()
;
;	start with largest nest level and work down to zero level
;	store higher level structures as PTR (in var_inq[XX].str_ptr)
;
;	search backwards in variables for structure definitions
;       assume structure variables are grouped together
;

for nn=max_level,0,-1 do begin
	for k=0, finq.nvars-1 do begin
		;
		;	check if new structure found (same nest level as "nn" and cnt = 0)
		;	if new, then ss = CREATE_STRUCT( tag, value ) for first parameter and
		;	then ss = CREATE_STRUCT( ss, tag, value ) for other parameters
		;
		if (k eq 0) then firstzero = var_inq[k].nest_cnt[nn] eq 0 $
		else firstzero = (var_inq[k].nest_cnt[nn] eq 0) and $
				( (var_inq[k-1].nest_cnt[nn] ne 0) or (var_inq[k-1].nest_id[nn] ne var_inq[k].nest_id[nn]) )
		if (var_inq[k].nest_level ge nn) and (firstzero) then begin
 			if (nn lt var_inq[k].nest_level) then begin
				ss = CREATE_STRUCT( var_inq[k].nest_name[nn+1], *(var_inq[k].str_ptr[nn+1]) )
			endif else begin
				ss = CREATE_STRUCT( var_inq[k].var_name, *(var_inq[k].ptr) )
			endelse
			k1 = k
			for kk=k+1, finq.nvars-1 do begin
				k2 = kk
				if ( var_inq[k2].nest_level ge nn ) and ( var_inq[k2].nest_id[nn] eq var_inq[k].nest_id[nn] ) and $
						( var_inq[k2].nest_cnt[nn] eq (var_inq[k1].nest_cnt[nn] + 1) ) then begin
					if (nn lt var_inq[kk].nest_level) then begin
						ss = CREATE_STRUCT( ss, var_inq[kk].nest_name[nn+1], *(var_inq[kk].str_ptr[nn+1]) )
					endif else begin
						ss = CREATE_STRUCT( ss, var_inq[kk].var_name, *(var_inq[kk].ptr) )
					endelse
					k1 = k2
				endif
			endfor
			;
			;	store new structure as PTR
			;	if BASE structure, then replicate for all data reading later
			var_inq[k].str_ptr[nn] = PTR_NEW( ss )
			if (nn eq 0) then begin
				data = replicate( ss, max_dim )
			endif
			if (debug_mode gt 0) then begin
				if (nn gt 0) then print, k, nn, '  Structure defined for ', var_inq[k].nest_name[nn] $
				else print, k, nn, '  Base Structure defined as '
				help, ss, /struct
			endif
		endif
	endfor
     endfor

if (debug_mode gt 0) then begin
	print, ' '
	print, '"data" array size is ', strtrim(max_dim,2)
	;stop, 'Check out structure definitions in data...'
endif

;
;	Once structures are defined, then read the netCDF variables into "data"
;	8.	NCDF_VARGET: Read the data from the variables.
;
for k=0, finq.nvars-1 do begin
	case var_inq[k].nest_level of
		0:  begin
			NCDF_VARGET, fid, k, value
			if ( var_inq[k].type eq 7 ) then $
			data.(var_inq[k].nest_cnt[0]) = string( value ) $ ;Convert from BYTE to STRING
			else data.(var_inq[k].nest_cnt[0]) = value
			end
		1:  begin
			NCDF_VARGET, fid, k, value
			if ( var_inq[k].type eq 7 ) then $
			data.(var_inq[k].nest_cnt[0]).(var_inq[k].nest_cnt[1]) = string( value ) $
			else data.(var_inq[k].nest_cnt[0]).(var_inq[k].nest_cnt[1]) = value
			end
		2:  begin
			NCDF_VARGET, fid, k, value
			if ( var_inq[k].type eq 7 ) then $
			data.(var_inq[k].nest_cnt[0]).(var_inq[k].nest_cnt[1]).(var_inq[k].nest_cnt[2]) = string( value ) $
			else data.(var_inq[k].nest_cnt[0]).(var_inq[k].nest_cnt[1]).(var_inq[k].nest_cnt[2]) = value
			end
		3:  begin
			NCDF_VARGET, fid, k, value
			if ( var_inq[k].type eq 7 ) then $
			data.(var_inq[k].nest_cnt[0]).(var_inq[k].nest_cnt[1]).(var_inq[k].nest_cnt[2]).(var_inq[k].nest_cnt[3]) = string( value ) $
			else data.(var_inq[k].nest_cnt[0]).(var_inq[k].nest_cnt[1]).(var_inq[k].nest_cnt[2]).(var_inq[k].nest_cnt[3]) = value
			end
		else: begin
			print, 'ERROR: read_netCDF can only process 4 nested structures'
			print, '       data is lost for ', var_inq[k].name
			end
	endcase
endfor

;
;	now define "attributes" as string array and read attributes from the netCDF file
;	5.	NCDF_ATTINQ: Optionally, retrieve the types and lengths of attributes.
;	6.	NCDF_ATTNAME: Optionally, retrieve attribute names.
;	7.	NCDF_ATTGET: Optionally, retrieve the attributes.
;
;	LIMITATION: limit attributes with more than 1 parameter are compressed into single string
;
CR = string( [ 13B ] )
num_att = 0L
;	finq.ngatts	= number of GLOBAL attributes from NCDF_INQUIRE earlier
if (finq.ngatts gt 0) then num_att = finq.ngatts + 1
nvaratt = intarr(finq.nvars)
for k=0, finq.nvars-1 do begin
    if (var_inq[k].natts gt 0) then num_att = num_att + var_inq[k].natts + 1
    nvaratt[k] = var_inq[k].natts
endfor
nvaratt_max=max(nvaratt)

if ( num_att gt 0 ) then begin
    attributes = strarr( num_att )
    globatts = strarr( finq.ngatts ) & globatts_val=globatts
    varatts = strarr( finq.nvars, nvaratt_max) & varatts_val =varatts
    acnt = 0L
	;
	;	do global variables first
	;
    if ( finq.ngatts gt 0) then begin
        attributes[acnt] = 'GLOBAL:' ;	+ CR
        acnt = acnt + 1
        for jj=0,finq.ngatts-1 do begin
            att_name = NCDF_ATTNAME( fid, /GLOBAL, jj )
            NCDF_ATTGET, fid, /GLOBAL, att_name, att_value
            att_str = string( att_value )
            n_str = n_elements(att_str)
            if (n_str gt 1) then begin
                new_str = ''
                for ii=0,n_str-1 do new_str = new_str + ' ' + strtrim(att_str[ii],2)
                att_str = new_str
            endif
            attributes[acnt] = '    ' + att_name + ' = ' + att_str ; + CR
            globatts[jj] =att_name
            globatts_val[jj] =att_str
            acnt = acnt + 1
        endfor
    endif
    for k=0, finq.nvars-1 do begin
        if (var_inq[k].natts gt 0) then begin
            attributes[acnt] = var_inq[k].name + ':' ;  + CR
            acnt = acnt + 1
            for jj=0,var_inq[k].natts-1 do begin
                att_name = NCDF_ATTNAME( fid, k, jj )
                NCDF_ATTGET, fid, k, att_name, att_value

                if (att_name eq 'missing_value') or $ ;Added for CARIBIC
                   (att_name eq  '_FillValue') then begin
                   if (ISA(att_value, /SCALAR) eq 1) then begin
                      att_value2=str2num(att_value, TYPE = type)
                      ;print,'type',type
                      if type ne 1 then att_value=att_value2 ;For EBAS data missing value = 9
                   endif
                endif

                att_str = string( att_value )
                n_str = n_elements(att_str)
                if (n_str gt 1) then begin
                   new_str = ''
                   for ii=0,n_str-1 do new_str = new_str + ' ' + strtrim(att_str[ii],2)
                   att_str = new_str
                endif
                attributes[acnt] = '    ' + att_name + ' = ' + att_str ; + CR
                varatts[k,jj] =att_name
                varatts_val[k,jj] =att_str
                ;print, k, jj, '  '+var_inq[k].name+': '+att_name+', '+att_str
                acnt = acnt + 1
             endfor
        endif
    endfor
endif else begin
   attributes = "NONE"
endelse

;
;	Close the netCDF file
;	9.	NCDF_CLOSE: Close the file.
;
NCDF_CLOSE, fid

;
;	Free up Pointers before exiting
;
for k=0, finq.nvars-1 do begin
	if PTR_VALID( var_inq[k].ptr ) then PTR_FREE, var_inq[k].ptr
	for jj=0,5 do if PTR_VALID( var_inq[k].str_ptr[jj] ) then PTR_FREE, var_inq[k].str_ptr[jj]
endfor

return

end

;--------------------------------------------------------------------------------------------

PRO update_posvar_names_cfcompliant,gloatt,gloatt_val,platform,var_names,$
                                    time_cf,lat_cf,lon_cf,alt_cf,rh_cf,$
                                    palt_cf,temp_cf,pres_cf,dpres_cf,$
                                    time_varname,var_names_cf

timevar_str=['*JDAY*','*UTC*','*tim*']
latvar_str=['*lat*']            ;,'*latitude*']
lonvar_str=['*lon*']            ;,'longitude*']
altvar_str=['*alt*','GGALT_NTL']
tempvar_str=['ATX','*temp*','AT_3051','TAT_DI_R','T_STAT','St_Air_Tm*','TS']
rhvar_str=['*rh*','RH','*HUMIDITY*','RH_DLH_WATER','RELHUM','Rel_Hum*']
altpvar_str=['*alt*p*','p*alt*','FMS_ALT_PRES_'] ;,'ALTITUDE_PRESSURE']
presvar_str=['PSXC','*pres*','pre','*stat*pres*','BP_915',$
             'STATICPRS','PS_RVSM','P_STAT','Stat_Pr','PSTATIC','PSMB']
dpresvar_str=['*QCXC*']

timematch=where(strupcase(var_names[*]) eq strupcase(time_varname[0]),tval)
var_names_cf[timematch]=time_cf

if platform eq 'Aircraft' then begin
   alt_varname=gloatt_val[where(gloatt[*] eq 'Vertical_Coordinate')]
   altmatch=where(strupcase(var_names[*]) eq strupcase(alt_varname[0]),alval)
   ;-Test if alt is pressure alt or not
   test_palt=0
   for i=0,n_elements(altpvar_str)-1 do begin
      temp=strmatch(alt_varname,altpvar_str[i],/fold_case)
      if temp gt 0 then test_palt=temp
   endfor
   if strupcase(alt_varname[0]) eq 'ALTITUDE_GPS' then test_palt=0
   if test_palt eq 1 then var_names_cf[altmatch]=palt_cf $
   else var_names_cf[altmatch]=alt_cf

   ;-Reset alt name in attributes to CF name
   gloatt_val[where(gloatt[*] eq 'Vertical_Coordinate')]=STRUPCASE(var_names_cf[altmatch]) 

   if var_names_cf[altmatch] ne palt_cf then begin
      for i=0,n_elements(altpvar_str)-1 do begin
         apmatch=where(strmatch(var_names[*],altpvar_str[i],/fold_case) eq 1,apval)
         if apval eq 2 then begin
            var_names_cf[apmatch[1]]=palt_cf
            goto,skipap
         endif
         if apval eq 1 then begin
            var_names_cf[apmatch]=palt_cf
            goto,skipap
         endif
      endfor
      print, 'Error: No pressure altitude variable'
skipap:
   endif
endif

if (platform eq 'Aircraft') or (platform eq 'Ship') then begin
   lat_varname=gloatt_val[where(gloatt[*] eq 'Latitude_Coordinate')]
   latmatch=where(strupcase(var_names[*]) eq strupcase(lat_varname[0]),ltval)
   var_names_cf[latmatch]=lat_cf
   lon_varname=gloatt_val[where(gloatt[*] eq 'Longitude_Coordinate')]
   lonmatch=where(strupcase(var_names[*]) eq strupcase(lon_varname[0]),lnval)
   var_names_cf[lonmatch]=lon_cf
   ;-Reset Lon / Lat names in attributes to CF names
   gloatt_val[where(gloatt[*] eq 'Latitude_Coordinate')] =STRUPCASE(lat_cf)
   gloatt_val[where(gloatt[*] eq 'Longitude_Coordinate')]=STRUPCASE(lon_cf)
endif

for i=0,n_elements(rhvar_str)-1 do begin
   rhmatch=where(strmatch(var_names[*],rhvar_str[i],/fold_case) eq 1,rhval)
   if rhval eq 1 then begin
      var_names_cf[rhmatch]=rh_cf
      goto,skiprh
   endif
endfor
print, 'Error: No RH variable'
skiprh:
for i=0,n_elements(tempvar_str)-1 do begin
   tpmatch=where(strmatch(var_names[*],tempvar_str[i],/fold_case) eq 1 and $
                 ;;stop CCN_temp_unstable_flag begin converted to "air_temperature":
                 strmatch(var_names[*],'*CCN_temp*',/fold_case) eq 0,tpval);flag  
   if tpval eq 1 then begin
      var_names_cf[tpmatch]=temp_cf
      goto,skiptp
   endif
   if tpval eq 2 then begin
      tpmatch2=where(strmatch(var_names[tpmatch],$
                              '*numflag*'+tempvar_str[i],/fold_case) eq 1,tpval2)
      if tpval2 eq 1 then var_names_cf[tpmatch[tpmatch2]]='numflag_'+temp_cf
      tpmatch3=where(strmatch(var_names[tpmatch],$
                              '*numflag*'+tempvar_str[i],/fold_case) eq 0,tpval3)
      if tpval3 eq 1 then var_names_cf[tpmatch[tpmatch3]]=temp_cf
      goto,skiptp
   endif
endfor
print, 'Error: No temperature variable'
skiptp:
for i=0,n_elements(presvar_str)-1 do begin
   prmatch=where((strmatch(var_names[*],presvar_str[i],/fold_case) eq 1) and $
                 (strmatch(var_names[*],altpvar_str[0],/fold_case) eq 0) and $
                 (strmatch(var_names[*],altpvar_str[1],/fold_case) eq 0),prval)
   if prval eq 2 then begin
      ;empty=where(var_names_cf[prmatch] eq '')
      ;var_names_cf[prmatch[empty]]=pres_cf

      prmatch2=where(strmatch(var_names[prmatch],$
                              'numflag'+presvar_str[i],/fold_case) eq 1,prval2)
      if prval2 eq 1 then var_names_cf[prmatch[prmatch2]]='numflag_'+pres_cf
      prmatch3=where(strmatch(var_names[prmatch],$
                              'numflag'+presvar_str[i],/fold_case) eq 0,prval3)
      if prval3 eq 1 then var_names_cf[prmatch[prmatch3]]=pres_cf
      goto,skippr
   endif
   if prval eq 1 then begin
      var_names_cf[prmatch]=pres_cf
      goto,skippr
   endif
endfor
print, 'Error: No pressure variable'
skippr:
for i=0,n_elements(dpresvar_str)-1 do begin
   dprmatch=where(strmatch(var_names[*],dpresvar_str[i],/fold_case) eq 1,dprval)
   if dprval eq 2 then begin
      var_names_cf[dprmatch[0]]=dpres_cf
      goto,skipdpr
   endif
   if dprval eq 1 then begin
      var_names_cf[dprmatch]=dpres_cf
      goto,skipdpr
   endif
endfor
;print, 'Error: No dynamic pressure variable'
skipdpr:
;;bpmatch=where(strmatch(var_names[*],'BP_915',/fold_case) eq 1,bpval)
;;if bpval eq 1 then var_names_cf[bpmatch]='BAROMETRIC PRESSURE


end

;----------------------------------------------------------------------------------------------
PRO match_varnames_standardnames,file_vars,spec_arr,var_names_cf

;;I'm sure there is a more efficient way of doing this!

;***Time***
vn=where(STRMATCH(file_vars,'*TIME*',/FOLD_CASE) EQ 1,vnval)
if vnval gt 1 then begin
   vn=where(STRMATCH(file_vars,'TIMEEND',/FOLD_CASE) EQ 1,vnval)
   if vnval eq 1 then var_names_cf[vn]='TIME_END'
   vn=where(STRMATCH(file_vars,'END_TIME',/FOLD_CASE) EQ 1,vnval)
   if vnval eq 1 then var_names_cf[vn]='TIME_END'
   vn=where(STRMATCH(file_vars,'Time_E',/FOLD_CASE) EQ 1,vnval)
   if vnval eq 1 then var_names_cf[vn]='TIME_END'
   vn=where(STRMATCH(file_vars,'Time_S',/FOLD_CASE) EQ 1,vnval)
   if vnval eq 1 then var_names_cf[vn]='TIME_START'
   vn=where(STRMATCH(file_vars,'Time_M',/FOLD_CASE) EQ 1,vnval)
   if vnval eq 1 then var_names_cf[vn]='TIME'  
endif
vn=where(STRMATCH(file_vars,'STOP_UTC',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='TIME_END'
vn=where(STRMATCH(file_vars,'MID_UTC',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='TIME_MID'

;;**Number conc**
vn=where(STRMATCH(file_vars,'NONVOLN10',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N10_NONVOL'
vn=where(STRMATCH(file_vars,'totalCNgt5nm_CPC1',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N5'
vn=where(STRMATCH(file_vars,'totalCNgt14nm_CPC2',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N14'
vn=where(STRMATCH(file_vars,'nonvolCNgt10nm_CPCA3',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N10_NONVOL'
vn=where(STRMATCH(file_vars,'CN_5_NUMBER',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N5'
vn=where(STRMATCH(file_vars,'CN>4_NM',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N4'
vn=where(STRMATCH(file_vars,'UNHEATED_CN>14_NM',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N14'
vn=where(STRMATCH(file_vars,'HEATED_CN>14_NM',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N14_NONVOL'
vn=where(STRMATCH(file_vars,'CN>3NM',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N3'
vn=where(STRMATCH(file_vars,'CNGT3NM',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N3'
vn=where(STRMATCH(file_vars,'CN>10NM',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N10'
vn=where(STRMATCH(file_vars,'NGT50NM',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N50'
vn=where(STRMATCH(file_vars,'NGT70NM',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N70'

;;also need to convert Ntot**;;could be N8,N10...
;vn=where(STRMATCH(file_vars,'NSUB',/FOLD_CASE) EQ 1,vnval) ;;Clarke data TOO RISKY: could be N8TO750, N10TO750, N150TO750
;if vnval eq 1 then var_names_cf[vn]='N10TO750'

vn=where(STRMATCH(file_vars,'NCOA',/FOLD_CASE) EQ 1,vnval) ;;Clarke data ***risky***
if vnval eq 1 then var_names_cf[vn]='N750'
vn=where(STRMATCH(file_vars,'CNGT10NM',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N10'
vn=where(STRMATCH(file_vars,'CN>10NM_NONVOL',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N10_NONVOL'
vn=where(STRMATCH(file_vars,'CNGT10NM_NONVOL',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N10_NONVOL'
vn=where(STRMATCH(file_vars,'NONVOLN10',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N10_NONVOL'
vn=where(STRMATCH(file_vars,'CPC3025',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N3' 
vn=where(STRMATCH(file_vars,'CPC3010',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N10'
vn=where(STRMATCH(file_vars,'CPC3010_DIL_FLAG',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N10_DIL_FLAG'
vn=where(STRMATCH(file_vars,'UCN_3025',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N3'
vn=where(STRMATCH(file_vars,'CNC_CONC',/FOLD_CASE) EQ 1,vnval);;CPC, FAAM
if vnval eq 1 then var_names_cf[vn]='N3'
vn=where(STRMATCH(file_vars,'CNC_FLAG',/FOLD_CASE) EQ 1,vnval);;CPC FLAG, FAAM
if vnval eq 1 then var_names_cf[vn]='N3_FLAG'
vn=where(STRMATCH(file_vars,'N_CPC_1',/FOLD_CASE) EQ 1,vnval);;CPC, AMF stations ***RISKY***
if vnval eq 1 then var_names_cf[vn]='N10'
vn=where(STRMATCH(file_vars,'total_CN',/FOLD_CASE) EQ 1,vnval);;CPC, AMAZE ***risky***
if vnval eq 1 then var_names_cf[vn]='N20'
vn=where(STRMATCH(file_vars,'UF_AEROSOL',/FOLD_CASE) EQ 1,vnval);;CPC, PEMTropicsA ***risky***
if vnval eq 1 then var_names_cf[vn]='N4'
vn=where(STRMATCH(file_vars,'F_AER_UNH',/FOLD_CASE) EQ 1,vnval);;CPC, PEMTropicsA
if vnval eq 1 then var_names_cf[vn]='N15'
vn=where(STRMATCH(file_vars,'F_AER_H',/FOLD_CASE) EQ 1,vnval);;CPC, PEMTropicsA
if vnval eq 1 then var_names_cf[vn]='N15_NONVOL'
vn=where(STRMATCH(file_vars,'PN_TOTAL_PARTICLES_STP',/FOLD_CASE) EQ 1,vnval);;PCASP, PEMTropicsB
if vnval eq 1 then var_names_cf[vn]='N100_STP'
vn=where(STRMATCH(file_vars,'PN_TOTAL_PARTICLES_AMBIENT',/FOLD_CASE) EQ 1,vnval);;PCASP, PEMTropicsB
if vnval eq 1 then var_names_cf[vn]='N100_AMB'
vn=where(STRMATCH(file_vars,'INTEGN_DMOB_PSL_SMPS_LARGE',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N10TO340'
vn=where(STRMATCH(file_vars,'DMA_50C_number_0.007<_Dp<_0.1um',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N7TO100'
vn=where(STRMATCH(file_vars,'OPC_50C_number_0.1<_Dp<_20.0um',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N100'
vn=where(STRMATCH(file_vars,'Number_dry_.1-.75',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N100TO750'
vn=where(STRMATCH(file_vars,'Number_.75-2',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N750TO2000'
vn=where(STRMATCH(file_vars,'Number_2-5',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N2000TO5000'
vn=where(STRMATCH(file_vars,'Number_5-20',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='N5000'


;;**Size distribution**
vn=where(STRMATCH(file_vars,'CN_size_distribution',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NSD'
vn=where(STRMATCH(file_vars,'CCN_SIZE_DISTRIBUTION',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='CCN_NSD'
vn=where(STRMATCH(file_vars,'SIZE_DISTRIBUTION',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NSD'
vn=where(STRMATCH(file_vars,'Bin_Diam',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='DP_MID'
vn=where(STRMATCH(file_vars,'MIDPT_DIAM',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='DP_MID'
vn=where(STRMATCH(file_vars,'MID_DIAM',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='DP_MID'
vn=where(STRMATCH(file_vars,'CON_M',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NSD'
vn=where(STRMATCH(file_vars,'CON',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]=spec_arr[0]
vn=where(STRMATCH(file_vars,'DP',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='DP_MID'
vn=where(STRMATCH(file_vars,'DP_M',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='DP_MID_RT'
vn=where(STRMATCH(file_vars,'DP_REF',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='DP_EDGE'
vn=where(STRMATCH(file_vars,'DP_B',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='DP_EDGE'
vn=where(STRMATCH(file_vars,'NUMDIST',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NSD'
vn=where(STRMATCH(file_vars,'NUMDIST_DMA_300C',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NSD_DMA_NONVOL'
vn=where(STRMATCH(file_vars,'NUMDIST_DMA_OPC',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NSD_DMA_OPC'

;;**SP2**
vn=where(STRMATCH(file_vars,'INCAND_N',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_NUM'
vn=where(STRMATCH(file_vars,'INCAND_S',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_NUM_SC'
vn=where(STRMATCH(file_vars,'INCAND_MASS',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_MASS'
vn=where(STRMATCH(file_vars,'INCAND_MASS_S',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_MASS_SC'
vn=where(STRMATCH(file_vars,'INCAND_N_M',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_NSD'
vn=where(STRMATCH(file_vars,'INCAND_MASS_M',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_MSD'
vn=where(STRMATCH(file_vars,'CNCIND',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_NUM'
vn=where(STRMATCH(file_vars,'INCANDNUM',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_NUM'
vn=where(STRMATCH(file_vars,'MASSIND',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_MASS'
vn=where(STRMATCH(file_vars,'INCANDMASS',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_MASS'
vn=where(STRMATCH(file_vars,'cncNoInd',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='SCAT_NUM'
vn=where(STRMATCH(file_vars,'ScatNum',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='SCAT_NUM'
vn=where(STRMATCH(file_vars,'massNoInd',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='SCAT_MASS'
vn=where(STRMATCH(file_vars,'ScatMass',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='SCAT_MASS'
vn=where(STRMATCH(file_vars,'BC_ng_m3',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_MASS'
vn=where(STRMATCH(file_vars,'BC_ng_kg',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_MMR'
vn=where(STRMATCH(file_vars,'BC',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_MASS'
vn=where(STRMATCH(file_vars,'MASSCONC_RBC',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_MASS'
vn=where(STRMATCH(file_vars,'MASSCONC_RBC_STP',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_MASS_STP'
vn=where(STRMATCH(file_vars,'BlackCarbonMassConcentration',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_MASS'
vn=where(STRMATCH(file_vars,'BC_M',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_MASS'
vn=where(STRMATCH(file_vars,'INCMASS',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_MASS'
vn=where(STRMATCH(file_vars,'INCNUM',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_NUM'
vn=where(STRMATCH(file_vars,'INCMASS5',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_MASS_5'
vn=where(STRMATCH(file_vars,'INCMASS15',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_MASS_15'
vn=where(STRMATCH(file_vars,'INCNUM5',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_NUM_5'
vn=where(STRMATCH(file_vars,'INCNUM15',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='BC_NUM_15'

;;**CATIONS/ANIONS**
vn=where(STRMATCH(file_vars,'SO4_1950',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='SO4_PM1'
vn=where(STRMATCH(file_vars,'NO3_1947',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NO3_PM1'
vn=where(STRMATCH(file_vars,'CL_1944',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='CHL_PM1'
vn=where(STRMATCH(file_vars,'NSSSO4_1953',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NSSSO4_PM1'
vn=where(STRMATCH(file_vars,'NH4_1923',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NH4_PM1'
vn=where(STRMATCH(file_vars,'NA_1920',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NA_PM1'
vn=where(STRMATCH(file_vars,'SO4_1952',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='SO4_PM10'
vn=where(STRMATCH(file_vars,'NO3_1949',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NO3_PM10'
vn=where(STRMATCH(file_vars,'CL_1946',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='CHL_PM10'
vn=where(STRMATCH(file_vars,'NSSSO4_1955',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NSSSO4_PM10'
vn=where(STRMATCH(file_vars,'NH4_1925',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NH4_PM10'
vn=where(STRMATCH(file_vars,'NA_1922',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NA_PM10'

vn=where(STRMATCH(file_vars,'SULFATE',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='SO4'
vn=where(STRMATCH(file_vars,'NITRATE',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NO3'
vn=where(STRMATCH(file_vars,'CHLORIDE',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='CHL'
vn=where(STRMATCH(file_vars,'AMMONIUM',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NH4'
vn=where(STRMATCH(file_vars,'SODIUM',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NA'


;;**Mass concentration**
vn=where(STRMATCH(file_vars,'SUBEC',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='EC_PM1'
vn=where(STRMATCH(file_vars,'SUBOC',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='OC_PM1'
vn=where(STRMATCH(file_vars,'SUB_2_5_EC',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='EC_PM2P5'
vn=where(STRMATCH(file_vars,'SUB_2_5_OC',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='OC_PM2P5'
vn=where(STRMATCH(file_vars,'PM_2_5',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='PM2P5'
vn=where(STRMATCH(file_vars,'PM_12_5',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='PM12P5'
vn=where(STRMATCH(file_vars,'SUBMASS',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='PM1'
vn=where(STRMATCH(file_vars,'SUPMASS',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='PM1TO10'

;;**AMS**
vn=where(STRMATCH(file_vars,'SULFATE_LT_1UM_AMS',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='SO4'
vn=where(STRMATCH(file_vars,'ORG_LT_1UM_AMS',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='ORG'
vn=where(STRMATCH(file_vars,'NITRATE_LT_1UM_AMS',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NO3'
vn=where(STRMATCH(file_vars,'AMMONIUM_LT_1UM_AMS',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NH4'
vn=where(STRMATCH(file_vars,'CHLORIDE_LT_1UM_AMS',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='CHL'
vn=where(STRMATCH(file_vars,'AMS_SO4',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='SO4'
vn=where(STRMATCH(file_vars,'AMS_ORG',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='ORG'
vn=where(STRMATCH(file_vars,'AMS_NO3',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NO3'
vn=where(STRMATCH(file_vars,'AMS_NH4',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NH4'
vn=where(STRMATCH(file_vars,'AMS_CHL',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='CHL'
vn=where(STRMATCH(file_vars,'AMS_SO4_all',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='SO4'
vn=where(STRMATCH(file_vars,'AMS_Org_all',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='ORG'
vn=where(STRMATCH(file_vars,'AMS_NO3_all',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NO3'
vn=where(STRMATCH(file_vars,'AMS_NH4_all',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NH4'
vn=where(STRMATCH(file_vars,'AMS_Chl_all',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='CHL'
vn=where(STRMATCH(file_vars,'Sulfate-lt-1um_AMS-60s',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='SO4'
vn=where(STRMATCH(file_vars,'Org-lt-1um_AMS-60s',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='ORG'
vn=where(STRMATCH(file_vars,'Nitrate-lt-1um_AMS-60s',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NO3'
vn=where(STRMATCH(file_vars,'Ammonium-lt-1um_AMS-60s',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NH4'
vn=where(STRMATCH(file_vars,'Chloride-lt-1um_AMS-60s',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='CHL'
vn=where(STRMATCH(file_vars,'AMSSO4',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='SO4'
vn=where(STRMATCH(file_vars,'AMSORG',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='ORG'
vn=where(STRMATCH(file_vars,'AMSNO3',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NO3'
vn=where(STRMATCH(file_vars,'AMSNH4',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NH4'
vn=where(STRMATCH(file_vars,'AMSCHL',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='CHL'
vn=where(STRMATCH(file_vars,'ORGANIC_MASS',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='ORG'
vn=where(STRMATCH(file_vars,'SULPHATE_TOTAL',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='SO4'
vn=where(STRMATCH(file_vars,'SUB25SO4',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='SO4'
vn=where(STRMATCH(file_vars,'SUB25NO3',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NO3'
vn=where(STRMATCH(file_vars,'SUB25NH4',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='NH4'

;;**CCN**
vn=where(STRMATCH(file_vars,'CCN_below0_08',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='CCN_LT0P08'
vn=where(STRMATCH(file_vars,'CCN_over0_65',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='CCN_GT0P65'
vn=where(STRMATCH(file_vars,'CCNlt0_3',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='CCN_LT0P3'
vn=where(STRMATCH(file_vars,'CCNgt0_8',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='CCN_GT0P8'
vn=where(STRMATCH(file_vars,'CCN0_02*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_02', '_0P02')
vn=where(STRMATCH(file_vars,'CCN0_08*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_08', '_0P08')
vn=where(STRMATCH(file_vars,'CCN0_2*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_2', '_0P2')
vn=where(STRMATCH(file_vars,'CCN0_6*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_6', '_0P6')
vn=where(STRMATCH(file_vars,'CCN0_04',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='CCN_0P04'
vn=where(STRMATCH(file_vars,'CCNtot*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('tot', '')
vn=where(STRMATCH(file_vars,'CCN0_08to0_23',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='CCN_0P08to0P23'
vn=where(STRMATCH(file_vars,'CCN0_23to0_43',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='CCN_0P23to0P43'
vn=where(STRMATCH(file_vars,'CCN0_43to0_65',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='CCN_0P43to0P65'
vn=where(STRMATCH(file_vars,'CCN0_08to0_13',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='CCN_0P08to0P13'
vn=where(STRMATCH(file_vars,'CCN0_13to0_18',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='CCN_0P13to0P18'
vn=where(STRMATCH(file_vars,'CCN0_18to0_23',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='CCN_0P18to0P23'
vn=where(STRMATCH(file_vars,'SSc_percent',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='SS'

;; vn=where(STRMATCH(file_vars,'',/FOLD_CASE) EQ 1,vnval)
;; if vnval eq 1 then var_names_cf[vn]=''
;vn=where(STRMATCH(file_vars,'CCN0_2',/FOLD_CASE) EQ 1,vnval)
;if vnval eq 1 then var_names_cf[vn]='CCN_0P2' ;CCN0_2_inverted ;CCN0_2_measured
;vn=where(STRMATCH(file_vars,'CCN0_08',/FOLD_CASE) EQ 1,vnval)
;if vnval eq 1 then var_names_cf[vn]='CCN_0P08' ;CCN0_08_measured ;CCN0_08_inverted
;vn=where(STRMATCH(file_vars,'CCN0_02',/FOLD_CASE) EQ 1,vnval)
;if vnval eq 1 then var_names_cf[vn]='CCN_0P02'
;vn=where(STRMATCH(file_vars,'CCN0_02_measured',/FOLD_CASE) EQ 1,vnval)
;if vnval eq 1 then var_names_cf[vn]='CCN_0P02_measured';CCN0_02_inverted
;vn=where(STRMATCH(file_vars,'CCN0_6',/FOLD_CASE) EQ 1,vnval)
;if vnval eq 1 then var_names_cf[vn]='CCN_0P6' ;CCN0_6_measured ;CCN0_6_inverted
;vn=where(STRMATCH(file_vars,'CCNtot',/FOLD_CASE) EQ 1,vnval)
;if vnval eq 1 then var_names_cf[vn]='CCN' ;CCNtot_measured ;CCNtot_inverted

;;Below conditions could be simplified to reduce number of lines of code
vn=where(STRMATCH(file_vars,'*0_06*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_06', '0P06')
vn=where(STRMATCH(file_vars,'*0_09*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_09', '0P09')
vn=where(STRMATCH(file_vars,'*0_11*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_11', '0P11')
vn=where(STRMATCH(file_vars,'*0_16*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_16', '0P16')
vn=where(STRMATCH(file_vars,'*0_17*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_17', '0P17')
vn=where(STRMATCH(file_vars,'*0_18*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_18', '0P18')
vn=where(STRMATCH(file_vars,'*0_28*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_28', '0P28')
vn=where(STRMATCH(file_vars,'*0_29*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_29', '0P29')
vn=where(STRMATCH(file_vars,'*0_30*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_30', '0P30')
vn=where(STRMATCH(file_vars,'*0_32*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_32', '0P32')
vn=where(STRMATCH(file_vars,'*0_35*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_35', '0P35')
vn=where(STRMATCH(file_vars,'*0_37*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_37', '0P37')
vn=where(STRMATCH(file_vars,'*0_47*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_47', '0P47')
vn=where(STRMATCH(file_vars,'*0_48*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_48', '0P48')
vn=where(STRMATCH(file_vars,'*0_50*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_50', '0P50')
vn=where(STRMATCH(file_vars,'*0_56*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_56', '0P56')
vn=where(STRMATCH(file_vars,'*0_63*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_63', '0P63')
vn=where(STRMATCH(file_vars,'*0_65*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_65', '0P65')
vn=where(STRMATCH(file_vars,'*0_73*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_73', '0P73')
vn=where(STRMATCH(file_vars,'*0_74*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_74', '0P74')
vn=where(STRMATCH(file_vars,'*0_80*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_80', '0P80')
vn=where(STRMATCH(file_vars,'*0_91*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_91', '0P91')
vn=where(STRMATCH(file_vars,'*0_94*',/FOLD_CASE) EQ 1,vnval)
if vnval ge 1 then var_names_cf[vn] = var_names_cf[vn].Replace('0_94', '0P94')


;**Position variables (not identified previously**
vn=where(STRMATCH(file_vars,'LAT*',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='LATITUDE'
vn=where(STRMATCH(file_vars,'LONGT*',/FOLD_CASE) EQ 1,vnval)
if vnval eq 1 then var_names_cf[vn]='LONGITUDE'

end
;----------------------------------------------------------------------------------------------

PRO standardise_timestamp,gloatt,gloatt_val,var_names_cf,time_cf,$
                          unit_arr,miss_arr,data,time_new,timeend,timestart

tind=where(strmatch(var_names_cf,time_cf,/fold_case) eq 1)
time=data.(tind)
timeend=time
timestart=time
tinfo=strsplit(unit_arr[tind],',- :',/extract)
print, 'Time stamp info: ',tinfo[0]+' '+tinfo[1]+' '+tinfo[2]+'-'+tinfo[3]+'-'+tinfo[4]+','+tinfo[5]+':',tinfo[6]+':'+tinfo[7]
if tinfo[2] ne '1970' then begin
   if tinfo[4] eq '00' then begin
      startdate=JULDAY(tinfo[3],'01',tinfo[2],tinfo[5],tinfo[6],tinfo[7])-1
      caldat,startdate,mntmp,dytmp,yrtmp
      print, startdate,dytmp,mntmp,yrtmp
   endif else startdate=JULDAY(tinfo[3],tinfo[4],tinfo[2],tinfo[5],tinfo[6],tinfo[7])
   ;;Month , Day    , Year   , Hour   , Minute , Second
   jultime = startdate - JULDAY(1,1,1970,0,0,0)
   
   dy_test=strmatch(tinfo[0],'*days*',/fold_case)
   if dy_test eq 1 then begin
      diff=time[1]-time[0]
      if ((size(time,/type) eq 4) or (size(time,/type) eq 5))$ ;FLOAT or DOUBLE
         and (diff lt 1.0) then begin                                      
         time_temp=long64(time*24*60*60) ;-Calculate time in seconds
         ;-Calculate # seconds since 1970-01-01
         if time[0] lt 1.0 then var_time=long64(jultime)* 24LL * 60 * 60 $ ;-Assumes day 1 of year is zero
         else var_time=long64(jultime-1)* 24LL * 60 * 60                   ;-Assumes day 1 of year is one
         ;;**Need to check above when day 1 is
         ;;zero but arrays starts on any other
         ;;day than 1st Jan
         time_new=time_temp+var_time

         ;-Test for end time stamps
         te=where(strmatch(var_names_cf,'TIME_END',/fold_case) eq 1,te_vals)
         if te_vals eq 1 then begin
            timeend_temp=long64(data.(te)*24*60*60) ;-Calculate time in seconds
            timeend=timeend_temp+var_time             
            unit_arr[te]='Seconds since 1970-01-01, 00:00:00 UTC'
         endif
      endif else begin
         var_time= long64(jultime) ;-Calculate time in days since 1970-01-01
         time_new=(long64(time)+var_time)* 24LL * 60 * 60
      endelse
   endif
   hr_test=strmatch(tinfo[0],'*hours*',/fold_case)
   if hr_test eq 1 then begin
      ;var_time= long64(jultime * 24LL) ;-Calculate time in hours since 1970-01-01
      ;time_new=(long64(time)+var_time)* 60 * 60
      var_time=long64(jultime * 24LL * 60 * 60) ;-Calculate time in seconds since 1970-01-01
      time_sec=time*60.*60.;-Calculate time in seconds
      time_new=long64(time_sec+var_time)
   endif
   min_test=strmatch(tinfo[0],'*minutes*',/fold_case)
   if min_test eq 1 then begin
      var_time= long64(jultime * 24LL * 60) ;-Calculate time in hours since 1970-01-01
      time_new=(long64(time)+var_time)* 60
   endif
   sc_test=strmatch(tinfo[0],'*seconds*',/fold_case)
   if sc_test eq 1 then begin
      var_time= long64(jultime * 24LL * 60 * 60) ;-Calculate time in seconds since 1970-01-01
      time_new= long64(time)+var_time
      
      ;-Test for start/end time stamps
      te=where(strmatch(var_names_cf,'Time_E*',/fold_case) eq 1,te_vals)
      if te_vals eq 1 then begin
         timeend=long64(data.(te))+var_time             
         ;-Check time-stamp for negative values & set to missing value
         neg_te=where(timeend lt (-1.0),neg_vals)
         if neg_vals gt 0 then timeend[neg_te]=str2num((miss_arr[te])[0])
         ;data.(te)=timeend
         unit_arr[te]='Seconds since 1970-01-01, 00:00:00 UTC'
      endif
      ts=where(strmatch(var_names_cf,'Time_S*',/fold_case) eq 1,ts_vals)
      if ts_vals eq 1 then begin
         timestart=long64(data.(ts))+var_time
         ;-Check time-stamp for negative values & set to missing value
         neg_ts=where(timestart lt (-1.0),neg_vals)
         if neg_vals gt 0 then timestart[neg_ts]=str2num((miss_arr[ts])[0])
         ;data.(ts)=timestart
         unit_arr[ts]='Seconds since 1970-01-01, 00:00:00 UTC'
      endif
   endif
   if sc_test eq 0 and hr_test  eq 0 and $
      dy_test eq 0 and min_test eq 0 then begin
      print, 'Error: time stamp is not recognised (not seconds, minutes, hours or days)'
      stop                      ;******** 
   endif
   t_unit='Seconds since 1970-01-01, 00:00:00 UTC'
   unit_arr[tind]=t_unit
       
   ;-Check time-stamp for negative values & set to missing value
   neg_check=where(time lt (-1.0),neg_vals);old time stamp
   if neg_vals gt 0 then begin
      print,'****Time array contains negative values!!****'
      time_new[neg_check]=str2num((miss_arr[tind])[0])
      valid_time=where(time_new ne str2num((miss_arr[tind])[0]))
      ;-Calculate new start/end values of time coverage
      mintime=min(time_new[valid_time]) & maxtime=max(time_new[valid_time])
   endif else begin
      ;-Put missing values into new time array
      miss_check=where(time eq miss_arr[tind],miss_vals)
      if miss_vals gt 0 then begin
         time_new[miss_check]=miss_arr[tind]
         mintime=min(time_new[where(time ne miss_arr[tind])])
         maxtime=max(time_new[where(time ne miss_arr[tind])])
      endif else begin
         ;-Calculate new start/end values of time coverage
         mintime=min(time_new) & maxtime=max(time_new)
      endelse
   endelse

   ;-Reset time-stamp in data structure
   data.(tind)=time_new         ;& help, time_new, var_time
endif else begin
   ;-For time stamps already converted to secs since 1970
   time_new=time
      
   ;-Check if characters are present in time coverages attributes
   att=(gloatt_val[where(gloatt[*] eq 'Time_Coverage_Start')])[0]
   if (strmatch(att,'*-*') eq 1) or (strmatch(att,'*:*') eq 1) $
      or (strmatch(att,'* *') eq 1) then begin
      mintime=min(time)
      maxtime=max(time)
   endif else begin
      ;-Get start/end values of time coverage from attributes
      mintime=gloatt_val[where(gloatt[*] eq 'Time_Coverage_Start')]
      maxtime=gloatt_val[where(gloatt[*] eq 'Time_Coverage_End'  )]
   endelse
endelse

;-Convert seconds since 1970 to meaningful time stamp for attributes 
mon_arr=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
mintimestr = strsplit(SYSTIME(0,mintime,/UTC),' ',/extract) ;;DOW MON DD HH:MM:SS YEAR
mon=string(where(mon_arr eq mintimestr[1])+1,format='(1i2.2)')
mindate=mintimestr[4]+'-'+mon+'-'+string(mintimestr[2],format='(1i2.2)')$
        +' '+mintimestr[3]+' UTC'
maxtimestr = strsplit(SYSTIME(0,maxtime,/UTC),' ',/extract) ;;DOW MON DD HH:MM:SS YEAR
mon=string(where(mon_arr eq maxtimestr[1])+1,format='(1i2.2)')
maxdate=maxtimestr[4]+'-'+mon+'-'+string(maxtimestr[2],format='(1i2.2)')$
        +' '+maxtimestr[3]+' UTC'
print,mindate;,mintimestr
print,maxdate;,maxtimestr

;-Replace time coordinate attributes with new time stamp name and values
gloatt_val[where(gloatt[*] eq 'Time_Coverage_Start')]=strtrim(mindate,2)  
gloatt_val[where(gloatt[*] eq 'Time_Coverage_End'  )]=strtrim(maxdate,2)
gloatt_val[where(gloatt[*] eq 'Time_Coordinate'    )]=STRUPCASE(time_cf)

;ntime=n_elements(time)
;stop   
end
;----------------------------------------------------------------------------------------------

PRO convert_altinfeet_to_altinmetres,gloatt,gloatt_val,var_names_cf,$
                                     alt_cf,palt_cf,unit_arr,data

altmin=str2num((gloatt_val[where(gloatt[*] eq 'Vertical_Min')])[0])
altmax=str2num((gloatt_val[where(gloatt[*] eq 'Vertical_Max')])[0])
aind=where(var_names_cf eq alt_cf,valalt)
pind=where(var_names_cf eq palt_cf,valpalt)
if (valalt eq 1) and (valpalt eq 1) then begin
   altind=[aind,pind]
   for a=0,1 do begin
      test_ft=strmatch(unit_arr[altind[a]],'*f*t*',/fold_case)
      if test_ft eq 1 then begin
         alt=data.(altind[a])*0.3048 ;;1 foot = 0.3048 meters
         data.(altind[a])=alt
         unit_arr[altind[a]]='m'
         if a eq 0 then begin
            gloatt_val[where(gloatt[*] eq 'Vertical_Min')]=strtrim(altmin*0.3048,2)
            gloatt_val[where(gloatt[*] eq 'Vertical_Max')]=strtrim(altmax*0.3048,2)
         endif
      endif
   endfor
endif else begin
   if (valalt eq 1) then altind=aind
   if (valalt eq 0) and (valpalt eq 1) then altind=pind
   test_ft=strmatch(unit_arr[altind],'*f*t*',/fold_case)
   if test_ft eq 1 then begin
      alt=data.(altind)*0.3048 ;;1 foot = 0.3048 meters
      data.(altind)=alt
      unit_arr[altind]='m'
      gloatt_val[where(gloatt[*] eq 'Vertical_Min')]=strtrim(altmin*0.3048,2)
      gloatt_val[where(gloatt[*] eq 'Vertical_Max')]=strtrim(altmax*0.3048,2)
   endif
endelse

end

;----------------------------------------------------------------------------------------------

PRO convert_longitude,gloatt,gloatt_val,var_names_cf,$
                      lon_cf,miss_arr,data

;-Get longitude array
lon_ind=where(strmatch(var_names_cf,lon_cf,/fold_case) eq 1,val)
if val eq 0 then stop
lon=data.(lon_ind)

   ;-Get type of variable and missing value
dbl_test=ISA(lon[0], 'Double')
flt_test=ISA(lon[0], 'Float')
if dbl_test then lon_missval=double((miss_arr[lon_ind])[0])
if flt_test then lon_missval=float((miss_arr[lon_ind])[0])
if (dbl_test eq 0) and (flt_test eq 0) then $ 
   lon_missval=str2num((miss_arr[lon_ind])[0])

bad_lon=where(lon eq lon_missval or $
              lon ge 9.0e30 or $
              lon le -777.0, badvals)
lon_tmp=lon
if badvals ge 1 then lon_tmp[bad_lon]=!VALUES.F_NAN

print,'Min,max longitude =    ',min(lon_tmp,/nan),max(lon_tmp,/nan)

;-Check longitude array is -180 to 180 degrees
if (max(lon_tmp,/nan) gt 180.) and (max(lon_tmp,/nan) le 360.) then begin

   ;-Convert longitude array to -180 to 180 if it's 0 to 360
   lon_new = ((lon_tmp + 180) MOD 360) - 180

   print,'Min,max longitude new =',min(lon_new,/nan),max(lon_new,/nan)

   ;-Calculate new min and max longitude and insert into attributes
   gloatt_val[where(gloatt[*] eq 'Lon_Min')]=min(lon_new,/nan)
   gloatt_val[where(gloatt[*] eq 'Lon_Max')]=max(lon_new,/nan)

   ;-Insert missing values back into new longitude array
   if badvals ge 1 then lon_new[bad_lon]=lon_missval

   ;-Insert new longitude back into data structure
   data.(lon_ind)=lon_new
;stop
endif

;-HIPPO data
if (min(lon_tmp,/nan) lt -180.) and (min(lon_tmp,/nan) ge -360.) then begin

   ;-Convert longitude array to -180 to 180 if it's -360 to 0
   lon_new = ((lon_tmp - 180) MOD (-360)) + 180 ;HIPPO data

   print,'Min,max longitude new =',min(lon_new,/nan),max(lon_new,/nan)

   ;-Calculate new min and max longitude and insert into attributes
   gloatt_val[where(gloatt[*] eq 'Lon_Min')]=min(lon_new,/nan)
   gloatt_val[where(gloatt[*] eq 'Lon_Max')]=max(lon_new,/nan)

   ;-Insert missing values back into new longitude array
   if badvals ge 1 then lon_new[bad_lon]=lon_missval

   ;-Insert new longitude back into data structure
   data.(lon_ind)=lon_new
;stop
endif

end
;----------------------------------------------------------------------------------------------

PRO standardise_unit_strings,unit_arr,var_names_cf

units2change=$
   ['/cm3','/cm^3','1/cm^3','1/cm3','cm^-3','#/cm3','#cm-3','particles/cm^3','parts/cc','#/cm^3','1/cm3@stp','1/cm^3_STP',$
    'per std cc','particle cm-3 at 1atm, 0C','parts/cc','count/cm^3','cm3',$
    'um3/cm3','um^3/cm^3','um3cm-3','micrometers^3/cm^3','um3/cm3@ambient','um^3/scc','um3 cm-3 at 1atm, 0C','um3cm-3',$
    'um2/cm3','um^2/cm^3','um2cm-3','micrometers^2/cm^3','um2/cm3@ambient','um^2/scc','um2cm-3',$
    'ug/m^3','ug/m3','mg/m3','ugC/m3','ugstdm-3','µg / m3','ug/sm^3',$
    'ng/m^3','ng/m3','ngstdm-3','ng m-3 at 1atm, 0C',$
    'ng/kg',$
    'N degree','degree N','degree_N','Deg_N','degN',$
    'E degree','degree E','degree_E','Deg_E','degE',$
    'degK','degree_K','degree_K, CAPS',$
    'degC','degree_C','degrees C','degree C','Celcius','deg_C','Celcius',$
    'pct','percent','%, CAPS',$
    'mb','mbar','hPa, CAPS',$
    'meters','micrometers',$
    'km, POS',$
    'unitless','no units',' ','#',$;check this last one!
    'm/s']                      


stdunits=$
   ['cm-3','cm-3','cm-3','cm-3','cm-3','cm-3','cm-3','cm-3','cm-3','cm-3','cm-3 stp','cm-3 stp',$
    'cm-3 stp','cm-3 stp','cm-3','cm-3','cm-3',$
    'um3 cm-3','um3 cm-3','um3 cm-3','um3 cm-3','um3 cm-3 ambient','um3 cm-3 stp','um3 cm-3 stp','um3 cm-3',$
    'um2 cm-3','um2 cm-3','um2 cm-3','um2 cm-3','um2 cm-3 ambient','um2 cm-3 stp','um2 cm-3',$
    'ug m-3','ug m-3','ug m-3','ugC m-3','ug m-3 stp','ug m-3','ug m-3 stp',$  ;;ugs m-3??
    'ng m-3','ng m-3','ng m-3 stp','ng m-3 stp',$
    'ng kg-1',$
    'degree_north','degree_north','degree_north','degree_north','degree_north',$
    'degree_east','degree_east','degree_east','degree_east','degree_east',$
    'K','K','K',$
    'C','C','C','C','C','C','C',$
    '%','%','%',$
    'hPa','hPa','hPa',$
    'm','um',$
    'km',$
    'none','none','none','none',$
    'm s-1']

;help,units2change,stdunits
if n_elements(units2change) ne n_elements(stdunits) then stop,'****Unit array dimensions do not match****'

unspecunit=['deg','degs','degrees','degree, POS']

for u=0,n_elements(unit_arr)-1 do begin

   unitmatch=where(strmatch(units2change,unit_arr[u],/fold_case) eq 1,val)
   if val eq 1 then begin
      print,'Old unit:',unit_arr[u]
      unit_arr[u]=stdunits[unitmatch]
      print,'New unit:',unit_arr[u]
   endif
   if val eq 0 then begin

      ;-Replace general lat/lon units (deg, degs, etc.)
      unitmatch2=where(strmatch(unspecunit,unit_arr[u],/fold_case) eq 1,val2)
      if val2 eq 1 then begin
         print,'Old unit:',unit_arr[u]
         ;;how do you replace: 'deg', 'degs', 'degrees'
         ;;if it's used for both lon and lat --> need to put a check in
         if strmatch(var_names_cf[u],'longitude',/fold_case) eq 1 then $
            unit_arr[u]='degree_east'
         if strmatch(var_names_cf[u],'latitude',/fold_case) eq 1 then $
            unit_arr[u]='degree_north'
         print,'New unit:',unit_arr[u]
      endif else print,'Retaining original unit:',unit_arr[u]
   endif

   ;-Check for encoding errors (assume corresponds to 'mu' symbol)
   if ((byte(unit_arr[u]))[0] eq 181) and $
      (STRMID(unit_arr[u], 1, 1) eq 'm') then begin
      print,'Encoding error, changing to "um"'
      ;byte(STRMID(unit_arr[u], 0, 1))
      unit_arr[u]='um'
   endif

   ;-Check for upper case metres
   if strmatch(unit_arr[u],'M') eq 1 then unit_arr[u]='m'

endfor

;stop

;Need to add in unit conversions: km, degC ;*************
;Celcius,deg_C,C

end

;----------------------------------------------------------------------------------------------

PRO set_GASSP_Level2_filename,project,gloatt,gloatt_val,filename

;;-Standardised filename for Level 2:
;;Variable_Instrument_Project[Database]_PlatformType_PlatformName[StationName]_Startdate_Enddate.nc,
;;dates in format yyyymmdd_hhmmss to be human readable

vartmp=strtrim(gloatt_val[where(gloatt eq 'Species_Short_Name')],2)
variable=strjoin(strsplit(vartmp,'|',/extract),'_')

insttmp=strtrim(gloatt_val[where(gloatt eq 'Instrument')],2)
instrument=strjoin(strsplit(insttmp,'|',/extract),'_')

platform=strtrim(gloatt_val[where(gloatt eq 'Platform')],2)
plattmp=strsplit(strtrim(gloatt_val[where(gloatt eq 'Platform_Name')],2),'|',/extract)
platfmname=strjoin(strsplit(plattmp[0],' ',/extract),'_')

starttime=strtrim(gloatt_val[where(gloatt[*] eq 'Time_Coverage_Start')],2)
endtime  =strtrim(gloatt_val[where(gloatt[*] eq 'Time_Coverage_End'  )],2)
starttmp=strsplit(starttime,' ',/extract,COUNT=nstr)
endtmp=strsplit(endtime,' ',/extract,COUNT=nstr2)
if nstr ge 2 then startdate=strjoin(strsplit(starttmp[0],':',/extract),'')+'_'+$
                            strjoin(strsplit(starttmp[1],':',/extract),'')
if nstr2 ge 2 then enddate=strjoin(strsplit(endtmp[0],':',/extract),'')+'_'+$
                           strjoin(strsplit(endtmp[1],':',/extract),'')
if nstr  eq 1 then startdate=strjoin(strsplit(starttmp[0],':',/extract),'')
if nstr2 ge 1 then enddate=strjoin(strsplit(endtmp[0],':',/extract),'')


filename=variable+'_'+instrument+'_'+project+'_'+platform+$
         '_'+platfmname+'_'+startdate+'_'+enddate+'.nc'

print,filename
stop

end
;----------------------------------------------------------------------------------------------
;-----MAIN PROCEDURE---------------------------------------------------------------------------

;;IDL code written by Carly Reddington to convert GASSP Level 1 data
;;files to GASSP Level 2 file format. 
;;***Make sure that the "Processed_file_list.txt" file is up-to-date
;;(to produce this file, run the IDL code:
;;Processing_code/count_number_processed_GASSP_files.pro)*****


;projarr=['CARIBIC','TRACEP','TRACEA','IMPROVE','A-PAD','SOS','ITCT2004','ITCT2002','TEXAQS2006','TROMPEX','GoAmazon',$
;         'SEAC4RS','MAMM','HolmeMoss','HIPPO','RONOCO','BORTAS','AMMA','COPS','Chilbolton','RHaMBLe',$
;         'MIRAGE','PEMTropicsB','PEMTropicsA','PASE','ACE1','ACE2','ACEASIA','VOCALS','INTEX-A','INDOEX','ARCTAS',$
;         'CLACE6','OP3','EUCAARI','Weybourne','AEGEAN-GAME','ACCACIA','COPE','CAST','CARRIBA',$
;         'Melpitz','A-FORCE','CALNEX','NACHTT','Polarstern','EM25','APPRAISE','Bird_Island',$
;         'CAREBeijing','PRIDE_PRD','AMAZE-08','WACS2014','WACS2012','UBWOS2012','UBWOS2013',$
;         'TEXAQS2000','RITS94','RITS93','NEAQS2004','NEAQS2002','AEROINDO99',$
;         'NAURU99','MAGE92','INTEX-B','ICEALOT','EUSAAR','Environment_Canada',$
;         'DYNAMO','DC3','ARCPAC2008','AMS_GlobalDatabase','DISCOVERAQ','AOE2001','AOE1996',$
;         'PEMWestB','PEMWestA','AMF_stations'] 


projarr=['EBAS_ACTRIS']



;;'EBAS_ACTRIS'

;;;seg fault on 17196, /nfs/a107/ear3clsr/GASSP/Processed_data/AMF_stations/EasternNorthAtlantic/CCN.enaaosccn100C1.a1.20140112.000000.nc

;;****'EBAS_ACTRIS',,****STILL TO PROCESS "NSD" DATA TO LEVEL 1!!!!!!****
;;EBAS_ACTRIS/pm2_5 files are: 826,1323

path='/nfs/see-fs-02_users/libclsr/GASSP/source_code/working_code/'
file=path+'Processed_file_list.txt'
openr,lun,file,/get_lun
header=''
readf,lun,header
filearr=strarr(file_lines(file)-1)
readf,lun,filearr
close,lun & free_lun,lun
nfiles=file_lines(file)-1

time_cf='Time'
lat_cf='Latitude'
lon_cf='Longitude'
alt_cf='Altitude'
rh_cf ='Relative_humidity'
palt_cf='Pressure_altitude';?? barometric_altitude
temp_cf='Air_temperature'
pres_cf='Air_pressure'
dpres_cf='Dynamic_pressure'

outdir='/nfs/a107/earkpr/GASSP/Level_2/'


for i=0L,nfiles-1 do begin
;for i=49910,49910 do begin

   if i eq 17196 then goto,skip_file

   strarr=strsplit(filearr[i],'/',/extract)
   proj=strarr[5]
   str=WHERE(STRMATCH(projarr,proj,/FOLD_CASE) EQ 1,matchvals)
   project=projarr[str]
   if proj eq 'EBAS_ACTRIS'  then proj='EBAS_ACTRIS/'+strarr[6]
   if proj eq 'AMF_stations' then proj='AMF_stations/'+strarr[6]
  
   if matchvals ge 1 then begin ;goto, skip_file;

    filename=filearr[i]
    print,''
    print, filename+',  '+strtrim(i,2)
    fileout=strarr[6]
    if proj eq 'EBAS_ACTRIS/'+strarr[6] then fileout=strarr[7]
    if proj eq 'AMF_stations/'+strarr[6] then fileout=strarr[7]
    ;print, fileout

     read_netCDF, filename, data, attributes, status, dim_name, dim_size, $
       gloatt, gloatt_val, varatts, varatts_val, nvaratts
    print, tag_names(data)
    ;help,data,/structure
    ;help, data.(0)
    ;help, data.time

    ntime=n_elements(data.(0))
    num_att=n_elements(globatts)
    num_var=n_elements(varatts[*,0])
    num_varatts=max(nvaratts) ;max nvaratts
    var_names=tag_names(data)
    var_names_cf=strarr(num_var)

    ;-Use global and variable attributes to get file info
    time_varname=gloatt_val[where(gloatt[*] eq 'Time_Coordinate')]
    platform    =strtrim (gloatt_val[where(gloatt[*] eq 'Platform')],2)
    spec_arr    =strsplit(gloatt_val[where(gloatt[*] eq 'Species_Short_Name')],'| ',/extract)
    vartag=where(gloatt[*] eq 'File_Var_Name',nvartag)
    if nvartag eq 1 then file_vars=strsplit(gloatt_val[vartag],'| ',/extract)$
    else file_vars=strsplit(gloatt_val[where(gloatt[*] eq 'Output_Variable')],'| ',/extract)

    ;Test for 2D variable names:
    var2D=where(gloatt[*] eq 'Output_Variable_2D',nvar2D) 
    if nvar2D ge 1 then begin
       var2Ds=strsplit(gloatt_val[var2D],'| ',/extract)
       file_vars=[file_vars,var2Ds[0]]
    endif

    nspec=n_elements(spec_arr)
    unit_arr=strarr(num_var)
    for ivar=0,num_var-1 do unit_arr[ivar]=reform(varatts_val[ivar,where(varatts[ivar,*] eq 'units')])
    miss_arr=reform(varatts_val[where(varatts eq 'missing_value')])

    if (var_names[0] eq 'JDAY') or (var_names[1] eq 'JDAY') then stop;;goto,skip_file ;************************************

    print,'Variable names specified:',file_vars
    print,'Variable names NetCDF:   ',var_names
    print,'Variable units:',unit_arr
    print,'Species short names:', spec_arr
      
    update_posvar_names_cfcompliant,gloatt,gloatt_val,platform,var_names,$
                                    time_cf,lat_cf,lon_cf,alt_cf,rh_cf,$
                                    palt_cf,temp_cf,pres_cf,dpres_cf,$
                                    time_varname,var_names_cf
 
    ;-Replace variable names with species short names
    variables=where(var_names_cf eq '',varvals)
    if varvals eq nspec then begin
       var_names_cf[variables]=spec_arr
       match_varnames_standardnames,file_vars,spec_arr,var_names_cf
    endif else begin
       print,''
       print, 'Error: Number of species not equal to nvariables'
       if n_elements(file_vars) eq num_var then begin
          print, 'Using variable names: ',file_vars[variables]
          var_names_cf[variables]=file_vars[variables]
          match_varnames_standardnames,file_vars,spec_arr,var_names_cf
       endif
       if (n_elements(file_vars) ne num_var) $
          or (proj eq 'TRACEA') then begin
          print, 'Using NetCDF names: ',var_names[variables]
          var_names_cf[variables]=var_names[variables]
          match_varnames_standardnames,var_names,spec_arr,var_names_cf
       endif
    endelse
    print, 'Final variable names: ',var_names_cf
    print, ''
    print,'CF-compliant variable names: ',var_names_cf
    
    ;-Rename dimensions to CF names
    for idim=0,n_elements(dim_name)-1 do begin
       dim=where(strmatch(var_names,dim_name[idim],/fold_case) eq 1,dimval)
       if dimval eq 1 then dim_name[idim]=var_names_cf[dim]
    endfor
    print,'CF-compliant dimension names: ',dim_name

    ;-Standardise time stamp
    standardise_timestamp,gloatt,gloatt_val,var_names_cf,time_cf,$
                          unit_arr,miss_arr,data,time_new,timeend,timestart

    
    ;-Convert altitude variable to metres
    if Platform eq 'Aircraft' then $
       convert_altinfeet_to_altinmetres,gloatt,gloatt_val,var_names_cf,$
                                        alt_cf,palt_cf,unit_arr,data

    ;-Convert longitude variable to -180 to 180 degrees
    if Platform eq 'Aircraft' or Platform eq 'Ship' then $
       convert_longitude,gloatt,gloatt_val,var_names_cf,$
                         lon_cf,miss_arr,data

    ;-Standardised unit strings
    standardise_unit_strings,unit_arr,var_names_cf

    ;-Replace units with new units
    for ivar=0,num_var-1 do varatts_val[ivar,where(varatts[ivar,*] eq 'units')]=unit_arr[ivar]

    ;-Get info to create data structure        
    var_ptr=PTRARR(num_var)
    for ivar=0,num_var-1 do begin

       ;Insert time variable separately to ensure it is 'long' data type
       if strmatch(var_names_cf[ivar],'Time*',/fold_case) eq 1 then begin
          if strmatch(var_names_cf[ivar],'Time*',/fold_case) eq 1 then $
             var_ptr[ivar] = PTR_NEW( reform(time_new) )
          if strmatch(var_names_cf[ivar],'Time_End',/fold_case) eq 1 then $ 
             var_ptr[ivar] = PTR_NEW( reform(timeend) )
          if strmatch(var_names_cf[ivar],'Time_Start',/fold_case) eq 1 then $ 
             var_ptr[ivar] = PTR_NEW( reform(timestart) )
       endif else $
          var_ptr[ivar] = PTR_NEW( reform(data.(ivar)) )
    endfor

    ;-Create structure to contain selected variables for netCDF file 
    for ivar=0,num_var-1 do begin
       temp=strjoin(strsplit(var_names_cf[ivar],'-',/extract),'_')
       ;print,temp
       if strmatch(temp,'*<*') eq 1 then temp=STRJOIN(STRSPLIT(temp,'<', /EXTRACT), 'lt')
       if strmatch(temp,'*>*') eq 1 then temp=STRJOIN(STRSPLIT(temp,'>', /EXTRACT), 'gt')
       if strmatch(temp,'*.*') eq 1 then temp=STRJOIN(STRSPLIT(temp,'.', /EXTRACT), 'p')
       var_names_cf[ivar]=temp
       ;print,var_names_cf[ivar]
       ;print, var_names_cf[ivar]
       if ivar eq 0 then ss = CREATE_STRUCT( var_names_cf[ivar], *(var_ptr[ivar]) )
       if ivar ge 1 then ss = CREATE_STRUCT( ss, var_names_cf[ivar], *(var_ptr[ivar]) )
       ;print,tag_names(ss)
    endfor
    
    ;-Add variable attribute to store original variable names
    varatts_new=strarr(num_var,num_varatts+1)
    varatts_val_new=varatts_new
    if n_elements(file_vars) eq num_var then varname_orig=file_vars $
    else varname_orig=var_names
    for ivar=0,num_var-1 do begin
       if nvaratts[ivar] gt 0 then begin
          varatts_new[ivar,0:nvaratts[ivar]-1]=varatts[ivar,0:nvaratts[ivar]-1]
          varatts_new[ivar,nvaratts[ivar]]='original_name'
          varatts_val_new[ivar,0:nvaratts[ivar]-1]=varatts_val[ivar,0:nvaratts[ivar]-1]
          varatts_val_new[ivar,nvaratts[ivar]]=varname_orig[ivar] ;var_names[ivar]
       endif else begin
          varatts_new[ivar,0]='original_name'
          varatts_val_new[ivar,0]=varname_orig[ivar]
       endelse 
    endfor
    nvaratts_new=nvaratts+1

    ;-Standardised GASSP & Software version tags
    gasspv=where(strmatch(gloatt[*],'Gassp_version',/fold_case) eq 1) ;'GASSP_Version'
    gloatt[gasspv]='GASSP_Version'
    gloatt_val[gasspv]='2.0'
    gloatt_val[where(gloatt[*] eq 'Software_Version')]='Level1_to_Level2_IDL'

    ;-Replace "Unknown" data tag fields with "NULL"
    field2replace=where(strmatch(gloatt_val,'unknown',/fold_case) eq 1,nfields)
    if nfields ge 1 then gloatt_val[field2replace]='NULL'

    ;-Standardise GASSP Level 2 filename
    set_GASSP_Level2_filename,project,gloatt,gloatt_val,filename_new
    fileproc=outdir+proj+'/'+filename_new

    fileproc=outdir+proj+'/'+fileout

    write_netCDF, ss, fileproc, gloatt, gloatt_val, $
                  num_var, nvaratts_new, varatts_new, varatts_val_new, $
                  status, dim_name, dim_size, /clobber
    print, tag_names(ss)
  
    print,'***********************************************************************************************************'
    print,'Created netCDF file: ',fileproc
    print,'***********************************************************************************************************'
    print, ''
;stop
skip_file:
 endif

;stop
endfor ;nfiles

;;*******NetCDF Error Codes*******
;;% NCDF_CONTROL: Attempt to take the file out of define mode (ENDEF) failed. (NC_ERROR=-45)
;;It's likely that the type of missing value does not match the
;;variable type (i.e. float, double, integer etc.) -> need to go to
;;line 452 and edit cases (note case of double -> default is float).
;;% NCDF_CONTROL: Attempt to take the file out of define mode (ENDEF) failed. (NC_ERROR=-62)
;;File being created is too big (>16G), needs splitting up into
;;smaller chunks.
;;http://www.nco.ncep.noaa.gov/pmb/codes/nwprod/sorc/rtofs_archv2netCDF.fd/netcdf.inc


;;http://www.stsci.edu/~valenti/idl/struct_replace_field.pro

END


    ;-Get missing values & replace with nans
    ;for ivar=1,num_var do begin
    ;   missing_val=float(varatts_val[ivar,where(varatts[ivar,*] eq 'missing_value')])
    ;   temp=data.(ivar)
    ;   nans=where(temp eq missing_val[0],nanval)
    ;   if nanval ge 1 then temp[nans]=-
    ;   data.(ivar)=temp
    ;endfor
    ;varatts[where(varatts eq 'missing_value')]='_FillValue'
;stop
