# rtop_tryout_2021nov  
Use the R package rtop to interpolate along rivers in Norway  

The aim is to get the 'rtop' package for R to interpolate along rivers in Norway. Input data should be x,y, z data. The idea is to plot  the concentrations measured per sample for lat and long positions, while also plotting predicted concentrations for predicted sample sites in the vicinity of the sampled (i.e. observed) sites.  
The shape file with rivers for Norway stems from this website: https://kartkatalog.geonorge.no/metadata/norges-vassdrags-og-energidirektorat/vannforekomster/b203e422-5270-4efc-93a5-2073725c43ef  

and this website:  
https://nedlasting.nve.no/gis/  
I have placed a copy of the shape file here:  
https://www.dropbox.com/scl/fi/7xafevro61agrehhlrdaw/NVE_60751B14_1689328325787_768.zip?rlkey=ho28jcm7ij5r2mujjmhdljpc8&dl=0"  
The R code should fetch this shape file. If not try downloading it inside the git cloned directory, and make sure the path to this unzipped file is correct.  
