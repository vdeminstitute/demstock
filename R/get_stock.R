#' Calculate stock measures from V-Dem data. 
#' 
#' This package allows you to generate a cumulative weighted sum of past values 
#' for any V-Dem variable with user-defined weights. The resulting measure is 
#' re-scaled to represent the share of the total possible stock the country 
#' could have accumulated up until that year. 
#' The package uses the latest version of the data loaded by the 
#' vdemdata R package.
#' The function generates a data frame containing a number of stock variables 
#' equal to length(var)*length(val). Specifying multiple elements in var and val 
#' will result in higher computational time. If your machine is slower or you 
#' specify many elements in var and val, be prepared to wait for the data to process.
#' 
#' @param var The V-Dem variables you wish to calculate a stock measure from. 
#' The default is the Electoral Democracy Index (v2x_polyarchy). 
#' A stock variable is calculated for each element of var.
#' (Note: each variable in var is first normalized to be between 0 and 1.)\cr
#' @param val The weights to apply to the cumulative sum of past values. The 
#' default weight is 0.99. A stock variable is calculated for each element of 
#' var and val. \cr
#' (Note: the weight must be between 0 and 1 or will receive an error message.) 
#' @param fill The number of years to fill forward missing values. 
#' The default is five years. To prevent values being filled forward, use 'fill=0'\cr 
#' (Note: the fill value must be a single whole number greater than or equal to 
#' zero or will return an error message.)\cr 
#' @param add Any additional V-Dem variables you wish added to the output dataset.\cr
#' Names must match variables names in the V-Dem dataset.
#' @param name An optional name for the output. The default name is 'v.dem.out'.\cr
#' (Note: re-running the command without specifying a name for the new output 
#' will result in previous output being rewritten.) 
#' @return A dataframe containing the original V-Dem variable(s), their stock 
#' measure(s) at each specified weight, along with country-year identifiers and 
#' any user-specified additional variables from the V-Dem dataset.
#' @examples
#' get_stock()
#' get_stock(var="v2x_libdem")
#' get_stock(var="v2x_libdem", val=.975)
#' get_stock(var="v2x_libdem", fill=10)
#' get_stock(var="v2x_libdem", name="newdata")
#' @export

get_stock <- 
  function(var = "v2x_polyarchy", val = 0.99, fill=5, add=NULL, name = "v.dem.out") {

#0. Load packages
load.packages<-function(){
	library(vdemdata)
	library(dplyr)
  library(tidyr)
	print("Necessary libraries loaded (vdemdata, dplyr, and tidyr).")
	}

#1. Get V-dem data
get.data<-function(){v.dem<<-vdem; cat("V-Dem dataset loaded.\n")}

#2. Subset variables
sub.data<-function(x,a=NULL){
	if(exists("v.dem")==F){print("Error: must get data before subsetting.")}
	`%!in%` <- Negate(`%in%`)
	v.dem.sub<<-as.data.frame(subset(v.dem, country_id %!in% c(349,354,355,356,357,358,359,360,361,362,363,366), select=c("country_name", "country_id", "year", x, a)))
	cat(paste0("Removed the following states from subset data: ", paste(names(table(v.dem[which(v.dem$country_id %in% c(349,354,355,356,357,358,359,360,361,362,363,366)),]$country_name)), collapse=", "),".\n"))
}

#3. Expand data
expand.data<-function(){
	if(exists("v.dem.sub")==F){print("Error: must subset data before expanding.")}
	orig.dim<-dim(v.dem.sub)
	v.dem.sub$original=1
  fullset<-matrix(NA, ncol=3, nrow=length(unique(v.dem.sub$country_id))*length(unique(v.dem.sub$year)))
      for(c in 1:length(unique(v.dem.sub$country_id))){
        fullset[(1:length(unique(v.dem.sub$year)))+(length(unique(v.dem.sub$year))*(c-1)),1]<-unique(v.dem.sub$country_id)[c]
        fullset[(1:length(unique(v.dem.sub$year)))+(length(unique(v.dem.sub$year))*(c-1)),2]<-unique(v.dem.sub[which(v.dem.sub$country_id==unique(v.dem.sub$country_id)[c]),]$country_name)
        fullset[(1:length(unique(v.dem.sub$year)))+(length(unique(v.dem.sub$year))*(c-1)),3]<-seq(range(unique(v.dem.sub$year))[1],range(unique(v.dem.sub$year))[2],by=1)
      }; colnames(fullset)<-c("country_id", "country_name", "year")
      v.dem.sub<-merge(v.dem.sub,fullset,all=T)
      v.dem.sub<-v.dem.sub[order(as.numeric(v.dem.sub$country_id),v.dem.sub$year),]
      v.dem.sub<-v.dem.sub[,c(ncol(v.dem.sub),1:3, c(1:ncol(v.dem.sub))[-c(ncol(v.dem.sub), 1:3)])]
      rownames(v.dem.sub)<-NULL
      v.dem.sub[which(is.na(v.dem.sub$original)==T),]$original<-0
      v.dem.sub<<-v.dem.sub
  cat(paste("Subset data expanded.\nOld dimensions were ", orig.dim[1], " by ", orig.dim[2], "; new dimensions are ", dim(v.dem.sub)[1], " by ", dim(v.dem.sub)[2], '.\n', sep=""))
	}

#4. Create country id for "historical" states
hist.states<-function(){
		if(exists("v.dem.sub")==F){print("Error: v.dem.sub does not exist.")}
      v.dem.sub$country_id.hist<-v.dem.sub$country_id
            #Albania (12), 1789-1911
            v.dem.sub[which(v.dem.sub$year<=1911 & v.dem.sub$country_id==12),]$country_id.hist<-99 
            #Yemen (14), 1851-1917
            v.dem.sub[which(v.dem.sub$year>=1851 & v.dem.sub$year<=1917 & v.dem.sub$country_id==14),]$country_id.hist<-99 
            #Poland (17), 1796-1808
            v.dem.sub[which(v.dem.sub$year>=1796 & v.dem.sub$year<=1808 & v.dem.sub$country_id==17),]$country_id.hist<-11 
            #Poland (17), 1868-1917
            v.dem.sub[which(v.dem.sub$year>=1868 & v.dem.sub$year<=1917 & v.dem.sub$country_id==17),]$country_id.hist<-11 
            #Poland (17), 1939-1943
            v.dem.sub[which(v.dem.sub$year>=1939 & v.dem.sub$year<=1943 & v.dem.sub$country_id==17 ),]$country_id.hist<-77 
            #South Yemen (23), 1838-1899
            v.dem.sub[which(v.dem.sub$year>=1838 & v.dem.sub$year<=1899 & v.dem.sub$country_id==23),]$country_id.hist<-39 
            #Bangladesh (24) 1858-1946
            v.dem.sub[which(v.dem.sub$year>=1858 & v.dem.sub$year<=1946 & v.dem.sub$country_id==24),]$country_id.hist<-39 
            #Bangladesh (24) 1947-1970
            v.dem.sub[which(v.dem.sub$year>=1947 & v.dem.sub$year<=1970 & v.dem.sub$country_id==24),]$country_id.hist<-29 
            #Bolivia (25), 1810-1824
            v.dem.sub[which(v.dem.sub$year<=1824 & v.dem.sub$year>=1810 & v.dem.sub$country_id==25),]$country_id.hist<-37 
            #Pakistan (29) 1858-1946
            v.dem.sub[which(v.dem.sub$year>=1858 & v.dem.sub$year<=1946 & v.dem.sub$country_id==29),]$country_id.hist<-39 
            #South Sudan (32) 1900-2010
            v.dem.sub[which(v.dem.sub$year>=1900 & v.dem.sub$year<=2010 & v.dem.sub$country_id==32),]$country_id.hist<-33 
            #Vietnam (34) 1802-1944
            v.dem.sub[which(v.dem.sub$year>=1802 & v.dem.sub$year<=1944 & v.dem.sub$country_id==34),]$country_id.hist<-35 
            #Vietnam (35) 1976-2018
            v.dem.sub[which(v.dem.sub$year>=1976 & v.dem.sub$country_id==35),]$country_id.hist<-34 
            #North Korea (41) 1789-1944
            v.dem.sub[which(v.dem.sub$year>=1789 & v.dem.sub$year<=1944 & v.dem.sub$country_id==41),]$country_id.hist<-42 
            #Kosovo (43) 1789-1912
            v.dem.sub[which(v.dem.sub$year>=1789 & v.dem.sub$year<=1912 & v.dem.sub$country_id==43),]$country_id.hist<-99 
            #Kosovo (43) 1913-1998
            v.dem.sub[which(v.dem.sub$year>=1913 & v.dem.sub$year<=1998 & v.dem.sub$country_id==43),]$country_id.hist<-198 
            #Lebanon (44) 1789-1830
            v.dem.sub[which(v.dem.sub$year<=1830 & v.dem.sub$country_id==44),]$country_id.hist<-99 
            #Lebanon (44) 1831-1841
            v.dem.sub[which(v.dem.sub$year>=1831 & v.dem.sub$year<=1841 & v.dem.sub$country_id==44),]$country_id.hist<-13 
            #Lebanon (44) 1842-1918
            v.dem.sub[which(v.dem.sub$year>=1842 & v.dem.sub$year<=1918 & v.dem.sub$country_id==44),]$country_id.hist<-99 
            #Venezuela (51) 1820-1829
            v.dem.sub[which(v.dem.sub$year>=1820 & v.dem.sub$year<=1829 & v.dem.sub$country_id==51),]$country_id.hist<-15 
            #Central African Republic (71) 1903-1919
            v.dem.sub[which(v.dem.sub$year>=1903 & v.dem.sub$year<=1919 & v.dem.sub$country_id==71),]$country_id.hist<-112 
            #Ecuador (75) 1810-1829
            v.dem.sub[which(v.dem.sub$year>=1810 & v.dem.sub$year<=1829 & v.dem.sub$country_id==75),]$country_id.hist<-15 
            #Germany (77) 1945-1948
            v.dem.sub[which(v.dem.sub$year>=1945 & v.dem.sub$year<=1948 & v.dem.sub$country_id==77),]$country_id.hist<-144 
            #Iraq (80) 1831-1919
            v.dem.sub[which(v.dem.sub$year>=1831 & v.dem.sub$year<=1919 & v.dem.sub$country_id==80),]$country_id.hist<-99 
            #Ireland (81) 1789-1918
            v.dem.sub[which(v.dem.sub$year<=1918 & v.dem.sub$country_id==81),]$country_id.hist<-101 
            #Italy (82) 1789-1860
            v.dem.sub[which(v.dem.sub$year<=1861 & v.dem.sub$country_id==82),]$country_id.hist<-373 
            #Jordan (83) 1789-1802
            v.dem.sub[which(v.dem.sub$year<=1802 & v.dem.sub$country_id==83),]$country_id.hist<-99 
            #Jordan (83) 1803-1818 
            v.dem.sub[which(v.dem.sub$year>=1803 & v.dem.sub$year<=1818 & v.dem.sub$country_id==83),]$country_id.hist<-197 
            #Jordan (83) 1819-1832
            v.dem.sub[which(v.dem.sub$year>=1819 & v.dem.sub$year<=1832 & v.dem.sub$country_id==83),]$country_id.hist<-99 
            #Jordan (83) 1833-1841
            v.dem.sub[which(v.dem.sub$year>=1833 & v.dem.sub$year<=1841 & v.dem.sub$country_id==83),]$country_id.hist<-13 
            #Jordan (83) 1842-1918 
            v.dem.sub[which(v.dem.sub$year>=1842 & v.dem.sub$year<=1918 & v.dem.sub$country_id==83),]$country_id.hist<-99 
            #Jordan (83) 1919-1921
            v.dem.sub[which(v.dem.sub$year>=1919 & v.dem.sub$year<=1921 & v.dem.sub$country_id==83),]$country_id.hist<-97 
            #Latvia (84) 1789-1919
            v.dem.sub[which(v.dem.sub$year<=1919 & v.dem.sub$country_id==84),]$country_id.hist<-11 
            #Latvia (84) 1940-1989
            v.dem.sub[which(v.dem.sub$year>=1940 & v.dem.sub$year<=1989 & v.dem.sub$country_id==84),]$country_id.hist<-11 
            #Mongolia (89) 1789-1910
            v.dem.sub[which(v.dem.sub$year<=1910 & v.dem.sub$country_id==89),]$country_id.hist<-110 
            #Netherlands (91) 1811-1812
            v.dem.sub[which((v.dem.sub$year==1811 | v.dem.sub$year==1812) & v.dem.sub$country_id==91),]$country_id.hist<-76 
            #Panama (92) 1810-1902
            v.dem.sub[which(v.dem.sub$year>=1810 & v.dem.sub$year<=1902 & v.dem.sub$country_id==92),]$country_id.hist<-15 
            #Qatar (94) 1871-1899
            v.dem.sub[which(v.dem.sub$year>=1871 & v.dem.sub$year<=1899 & v.dem.sub$country_id==94),]$country_id.hist<-99 
            #Syria (97) 1789-1830
            v.dem.sub[which(v.dem.sub$year<=1830 & v.dem.sub$country_id==97),]$country_id.hist<-99 
            #Syria (97) 1831-1841
            v.dem.sub[which(v.dem.sub$year>=1831 & v.dem.sub$year<=1841 & v.dem.sub$country_id==97),]$country_id.hist<-13 
            #Syria (97) 1842-1917
            v.dem.sub[which(v.dem.sub$year>=1842 & v.dem.sub$year<=1917 & v.dem.sub$country_id==97),]$country_id.hist<-99 
            #Ukraine (100) 1789-1989
            v.dem.sub[which(v.dem.sub$year<=1989 & v.dem.sub$country_id==100),]$country_id.hist<-11 
            #Uruguay (102) 1810-1820
            v.dem.sub[which(v.dem.sub$year>=1810 & v.dem.sub$year<=1820 & v.dem.sub$country_id==102),]$country_id.hist<-37 
            #Uruguay (102) 1821-1824 
            v.dem.sub[which(v.dem.sub$year>=1821 & v.dem.sub$year<=1824 & v.dem.sub$country_id==102),]$country_id.hist<-19 
            #Armenia (105) 1826-1989
            v.dem.sub[which(v.dem.sub$year>=1826 & v.dem.sub$year<=1989 & v.dem.sub$country_id==105),]$country_id.hist<-11 
            #Azerbaijan (106) 1814-1989
            v.dem.sub[which(v.dem.sub$year>=1814 & v.dem.sub$year<=1989 & v.dem.sub$country_id==106),]$country_id.hist<-11 
            #Belarus (107) 1789-1989
            v.dem.sub[which(v.dem.sub$year<=1989 & v.dem.sub$country_id==107),]$country_id.hist<-11 
            #Chad (109) 1903-1919
            v.dem.sub[which(v.dem.sub$year>=1903 & v.dem.sub$year<=1919 & v.dem.sub$country_id==109),]$country_id.hist<-112 
            #Dominican Republic (114) 1823-1843
            v.dem.sub[which(v.dem.sub$year>=1823 & v.dem.sub$year<=1843 & v.dem.sub$country_id==114),]$country_id.hist<-26 
            #Georgia (118) 1800-1989
            v.dem.sub[which(v.dem.sub$year>=1800 & v.dem.sub$year<=1989 & v.dem.sub$country_id==118),]$country_id.hist<-11 
            #Kazakhstan (121) 1789-1990
            v.dem.sub[which(v.dem.sub$year<=1990 & v.dem.sub$country_id==121),]$country_id.hist<-11 
            #Kyrgyzstan (122) 1876-1989
            v.dem.sub[which(v.dem.sub$year>=1876 & v.dem.sub$year<=1989 & v.dem.sub$country_id==122),]$country_id.hist<-11 
            #Moldova (126) 1789-1811
            v.dem.sub[which(v.dem.sub$year<=1811 & v.dem.sub$country_id==126),]$country_id.hist<-99 
            #Moldova (126) 1812-1917
            v.dem.sub[which(v.dem.sub$year>=1812 & v.dem.sub$year<=1917 & v.dem.sub$country_id==126),]$country_id.hist<-11 
            #Moldova (126) 1918-1939
            v.dem.sub[which(v.dem.sub$year>=1918 & v.dem.sub$year<=1939 & v.dem.sub$country_id==126),]$country_id.hist<-190 
            #Moldova (126) 1940-1941
            v.dem.sub[which(v.dem.sub$year>=1940 & v.dem.sub$year<=1941 & v.dem.sub$country_id==126),]$country_id.hist<-11 
            #Moldova (126) 1942-1944 
            v.dem.sub[which(v.dem.sub$year>=1942 & v.dem.sub$year<=1944 & v.dem.sub$country_id==126),]$country_id.hist<-190 
            #Moldova (126) 1945-1989
            v.dem.sub[which(v.dem.sub$year>=1945 & v.dem.sub$year<=1989 & v.dem.sub$country_id==126),]$country_id.hist<-11 
            #Tajikistan (133) 1864-1989
            v.dem.sub[which(v.dem.sub$year>=1864 & v.dem.sub$year<=1989 & v.dem.sub$country_id==133),]$country_id.hist<-11 
            #Turkmenistan (136) 1864-1989
            v.dem.sub[which(v.dem.sub$year>=1864 & v.dem.sub$year<=1990 & v.dem.sub$country_id==136),]$country_id.hist<-11 
            #German Democratic Republic (137) 1789-1944
            v.dem.sub[which(v.dem.sub$year<=1944 & v.dem.sub$country_id==137),]$country_id.hist<-77 
            #German Democratic Republic (137) 1990-2018
            v.dem.sub[which(v.dem.sub$year>1990 & v.dem.sub$country_id==137),]$country_id.hist<-77 
            #Somaliland (139) 1960-1990
            #v.dem.sub[which(v.dem.sub$year>=1960 & v.dem.sub$year<=1990 & v.dem.sub$country_id==139),]$country_id.hist<-130 
            #Uzbekistan (140) 1921-1989
            v.dem.sub[which(v.dem.sub$year>=1921 & v.dem.sub$year<=1989 & v.dem.sub$country_id==140),]$country_id.hist<-11 
            #Austria (144) 1939-1944
            v.dem.sub[which(v.dem.sub$year>=1939 & v.dem.sub$year<=1944 & v.dem.sub$country_id==144),]$country_id.hist<-77 
            #Belgium (148) 1796-1814
            v.dem.sub[which(v.dem.sub$year>=1796 & v.dem.sub$year<=1814 & v.dem.sub$country_id==148),]$country_id.hist<-76 
            #Belgium (148) 1815-1829
            v.dem.sub[which(v.dem.sub$year>=1815 & v.dem.sub$year<=1829 & v.dem.sub$country_id==148),]$country_id.hist<-91 
            #Bosnia and Herzegovina (150) 1918-1991
            v.dem.sub[which(v.dem.sub$year>=1918 & v.dem.sub$year<=1991 & v.dem.sub$country_id==150),]$country_id.hist<-198 
            #Bulgaria (152) 1789-1877
            v.dem.sub[which(v.dem.sub$year<=1877 & v.dem.sub$country_id==152),]$country_id.hist<-99 
            #Comoros (153) 1914-1945
            v.dem.sub[which(v.dem.sub$year>=1914 & v.dem.sub$year<=1945 & v.dem.sub$country_id==153),]$country_id.hist<-125 
            #Croatia (154) 1867-1918
            v.dem.sub[which(v.dem.sub$year>=1867 & v.dem.sub$year<=1918 & v.dem.sub$country_id==154),]$country_id.hist<-210 
            #Croatia (154) 1919-1940
            v.dem.sub[which(v.dem.sub$year>=1919 & v.dem.sub$year<=1940 & v.dem.sub$country_id==154),]$country_id.hist<-198 
            #Croatia (154) 1945-1990
            v.dem.sub[which(v.dem.sub$year>=1945 & v.dem.sub$year<=1990 & v.dem.sub$country_id==154),]$country_id.hist<-198 
            #Czech Republic (157) 1789-1917
            v.dem.sub[which(v.dem.sub$year<=1917 & v.dem.sub$country_id==157),]$country_id.hist<-144 
            #Estonia (161) 1789-1917
            v.dem.sub[which(v.dem.sub$year<=1917 & v.dem.sub$country_id==161),]$country_id.hist<-11 
            #Estonia (161) 1940-1989
            v.dem.sub[which(v.dem.sub$year>=1940 & v.dem.sub$year<=1989 & v.dem.sub$country_id==161),]$country_id.hist<-11 
            #Greece (164) 1789-1821
            v.dem.sub[which(v.dem.sub$year<=1821 & v.dem.sub$country_id==164),]$country_id.hist<-99 
            #Iceland (168) 1843-1899
            v.dem.sub[which(v.dem.sub$year>=1843 & v.dem.sub$year<=1899 & v.dem.sub$country_id==168),]$country_id.hist<-158 
            #Lithuania (173) 1789-1794
            v.dem.sub[which(v.dem.sub$year<=1794 & v.dem.sub$country_id==173),]$country_id.hist<-17 
            #Lithuania (173) 1795-1917
            v.dem.sub[which(v.dem.sub$year>=1795 & v.dem.sub$year<=1917 & v.dem.sub$country_id==173),]$country_id.hist<-11 
            #Lithuania (173) 1940-1989
            v.dem.sub[which(v.dem.sub$year>=1940 & v.dem.sub$year<=1989 & v.dem.sub$country_id==173),]$country_id.hist<-11 
            #Macedonia (176) 1789-1911
            v.dem.sub[which(v.dem.sub$year<=1911 & v.dem.sub$country_id==176),]$country_id.hist<-99 
            #Macedonia (176) 1912-1990
            v.dem.sub[which(v.dem.sub$year>=1912 & v.dem.sub$year<=1990 & v.dem.sub$country_id==176),]$country_id.hist<-198 
            #Montenegro (183) 1918-1997
            v.dem.sub[which(v.dem.sub$year>=1919 & v.dem.sub$year<=1997 & v.dem.sub$country_id==183),]$country_id.hist<-198 
            #Saudi Arabia (197) 1819-1821
            v.dem.sub[which(v.dem.sub$year>=1819 & v.dem.sub$year<=1821 & v.dem.sub$country_id==197),]$country_id.hist<-13 
            #Slovakia (201) 1789-1918
            v.dem.sub[which(v.dem.sub$year<=1918 & v.dem.sub$country_id==201),]$country_id.hist<-210 
            #Slovakia (201) 1919-1938
            v.dem.sub[which(v.dem.sub$year>=1919 & v.dem.sub$year<=1938 & v.dem.sub$country_id==201),]$country_id.hist<-157 
            #Slovakia (201) 1945-1992
            v.dem.sub[which(v.dem.sub$year>=1945 & v.dem.sub$year<=1992 & v.dem.sub$country_id==201),]$country_id.hist<-157 
            #Slovenia (202) 1789-1918
            v.dem.sub[which(v.dem.sub$year>=1789 & v.dem.sub$year<=1918 & v.dem.sub$country_id==202),]$country_id.hist<-144 
            #Slovenia (202) 1919-1988
            v.dem.sub[which(v.dem.sub$year>=1919 & v.dem.sub$year<=1988 & v.dem.sub$country_id==202),]$country_id.hist<-198 
            #Parma (352) 1803-1814
            v.dem.sub[which(v.dem.sub$year>=1803 & v.dem.sub$year<=1814 & v.dem.sub$country_id==352),]$country_id.hist<-76 
            #Papal States (361) 1810-1813
            #v.dem.sub[which(v.dem.sub$year>=1810 & v.dem.sub$year<=1813 & v.dem.sub$country_id==361),]$country_id.hist<-76 
            v.dem.sub<-v.dem.sub[,c(1,ncol(v.dem.sub),2:(ncol(v.dem.sub)-1))]
      v.dem.sub<<-v.dem.sub
      print("Historical country ID created.")
			}

#5. Normalize and fill forward years
fill.data<-function(x,f=5){
			if(exists("v.dem.sub")==F){print("Error: v.dem.sub does not exist.")}
			if(f<0){print("Error: 'fill' must be a single positive integer.")}
      for(e in x){
  			if(length(which(colnames(v.dem.sub)==e))==0){print("Error: necessary variable does not exist.")}
  			if(typeof(v.dem.sub[,which(colnames(v.dem.sub)==e)])=="character"){print("Error: variable is not numeric.")}
            	# Normalize variable
              maxval<<-max(v.dem.sub[,which(colnames(v.dem.sub)==e)], na.rm=T)
              minval<<-min(v.dem.sub[,which(colnames(v.dem.sub)==e)], na.rm=T)
        	    v.dem.sub[,ncol(v.dem.sub)+1]<-(v.dem.sub[,which(colnames(v.dem.sub)==e)]-minval)/(maxval-minval)
        	    colnames(v.dem.sub)[ncol(v.dem.sub)]<-paste0(e,"n")
        	v.dem.sub<- v.dem.sub %>% group_by(country_id) %>% mutate(!!as.name(paste0(e,"_filled")):=!!as.name(paste0(e,"n")))}
      if(f>0){
        for(e in x){
        	for(lagval in c(1:f)){
          	v.dem.sub<- v.dem.sub %>% group_by(country_id) %>% mutate(!!as.name(paste0("L",lagval,e)):=lag(!!as.name(paste0(e,"n")), n=lagval))
            v.dem.sub[which(is.na(v.dem.sub[,which(colnames(v.dem.sub)==paste0(e,"_filled"))])==T),which(colnames(v.dem.sub)==paste0(e,"_filled"))]<-v.dem.sub[which(is.na(v.dem.sub[,which(colnames(v.dem.sub)==paste0(e,"_filled"))])==T),which(colnames(v.dem.sub)==paste0("L",lagval,e))]
            v.dem.sub<-v.dem.sub[,-which(colnames(v.dem.sub)==paste0("L",lagval,e))]
        	}}}
			v.dem.sub<<-v.dem.sub
			print(paste0("Variables filled forward ", f, " years."))
      }
	
#6. Create measure based on historical antecedence
ante.data<-function(x){
			if(exists("v.dem.sub")==F){print("Error: v.dem.sub does not exist.")}
			for(e in x){
			if(length(which(colnames(v.dem.sub)==paste0(e,"_filled")))==0){print("Error: necessary variable does not exist.")}
	    v.dem.sub[,ncol(v.dem.sub)+1]<-v.dem.sub[, which(colnames(v.dem.sub)==paste0(e,"_filled"))]; colnames(v.dem.sub)[ncol(v.dem.sub)]<-paste0(e,".hist")
    
  	  changed<-c(11, 13, 15, 17, 19, 26, 29, 33, 34, 35, 37, 39, 42, 76, 77, 91, 97, 99, 101, 110, 112, 125, 144, 157, 158, 190, 197, 198, 210, 373)
	    temp<-subset(v.dem.sub, country_id.hist %in% changed)
  	  temp<-temp %>% group_by(country_id.hist, year) %>% 
  	    mutate("tempmean" := mean(!!as.name(paste0(e,".hist")), na.rm=T))
			temp<-subset(temp, select=c("country_id.hist", "country_name", "year", "tempmean"))
  	  v.dem.sub <- v.dem.sub %>% left_join(temp, by = c("country_id.hist", "country_name", "year"))
  	  v.dem.sub[which(v.dem.sub$country_id.hist!=v.dem.sub$country_id),which(colnames(v.dem.sub) %in% paste0(e,".hist"))]<-v.dem.sub[which(v.dem.sub$country_id.hist!=v.dem.sub$country_id),]$tempmean
			v.dem.sub<-v.dem.sub[,-which(colnames(v.dem.sub)%in%"tempmean")]
  		
      #Carrying forward Vietnam, which shows similar values before and after a gap from 1887-1902. This was all French colonial rule period
      for(y in c(1891:1901)){v.dem.sub[which(v.dem.sub$country_id==34 & v.dem.sub$year==y),which(colnames(v.dem.sub)==paste0(e,".hist"))]<-v.dem.sub[which(v.dem.sub$country_id==34 & v.dem.sub$year==(y-1)),which(colnames(v.dem.sub)==paste0(e,".hist"))]}
      #Carrying forward Egypt, which shows similar values before and after a gap from 1883-1912. This was all British colonial rule period
      for(y in c(1883:1912)){v.dem.sub[which(v.dem.sub$country_id==13 & v.dem.sub$year==y),which(colnames(v.dem.sub)==paste0(e,".hist"))]<-v.dem.sub[which(v.dem.sub$country_id==13 & v.dem.sub$year==(y-1)),which(colnames(v.dem.sub)==paste0(e,".hist"))]}
      #Averaged values for Burkina Faso under French West Africa
      FWA<-subset(v.dem.sub, year>=1932 & year<=1946 & (country_id %in% c(64,28,60)), select=c(country_name, country_id, year, eval(as.name(paste0(e,"_filled")))))
      v.dem.sub[which(v.dem.sub$country_id==54 & v.dem.sub$year>=1932 & v.dem.sub$year<=1946),which(colnames(v.dem.sub)==paste0(e,".hist"))]<-aggregate(FWA[,which(colnames(FWA)==paste0(e,"_filled"))],by=list(FWA$year), FUN="mean", na.rm=T)[,2]
      rm(FWA)
			}
	
			v.dem.sub<<-v.dem.sub
			print("Antecedent values imputed.")
			}

#7. Calculate stock
calc.stock<-function(x,v=.99){
			if(exists("v.dem.sub")==F){print("Error: necessary data do not exist or have not been prepared for calculating stock.")}
	v.dem.out<-v.dem.sub
          if(length(x)==1 & length(v)==1){print(paste0("Working on calculating stock for ", length(x), " variable and ",  length(v), " depreciation rate. "));cat("\n")}
          if(length(x)>1  & length(v)==1){print(paste0("Working on calculating stock for ", length(x), " variables and ", length(v), " depreciation rate. "));cat("\n")}
          if(length(x)==1 & length(v)>1 ){print(paste0("Working on calculating stock for ", length(x), " variable and ",  length(v), " depreciation rates."));cat("\n")}
          if(length(x)>1  & length(v)>1 ){print(paste0("Working on calculating stock for ", length(x), " variables and ", length(v), " depreciation rates."));cat("\n")}
      for(e in x){
			if(length(which(colnames(v.dem.out)==e))==0){print("Error: necessary variable does not exist.")}
      if(length(table(v.dem.out[,which(colnames(v.dem.out)==e)]))<10){print("Small number of possible values: stock may not be appropriate for this measure.")}
			if(is.numeric(v)==F){print("Error: weight is not numeric.")}
        v.dem.out$startyear<-NA
      	v.dem.out[which(is.na(v.dem.out[,which(colnames(v.dem.out)==paste0(e,".hist"))])==F),]$startyear<-v.dem.out[which(is.na(v.dem.out[,which(colnames(v.dem.out)==paste0(e,".hist"))])==F),]$year
          v.dem.out <- v.dem.out %>%
            group_by(country_id) %>%
            mutate(!!as.name(paste0(e,".start")) := min(startyear, na.rm=T)) %>%
            dplyr::select(-startyear)
      	for(w in v){
      	       print(paste0("Variable: ", e, "; Depreciation rate: ", 100-(w*100), "%"))
        #formatting weight label
        places<-nchar(strsplit(sub('0+$', '', as.character(w)), ".", fixed=TRUE)[[1]][[2]])
        vlab<<-sub("^(-?)0.", "\\1.", sprintf(paste("%.",places,"f",sep=""), w))
  
        if(length(grep("Lval",  names(v.dem.out)))>0){v.dem.out<-v.dem.out[,-grep("Lval",  names(v.dem.out))]}
        if(length(grep("Lhist", names(v.dem.out)))>0){v.dem.out<-v.dem.out[,-grep("Lhist", names(v.dem.out))]}
        
      	    #sets values to minimum value for first observed year
            v.dem.out[,ncol(v.dem.out)+1]<-NA; colnames(v.dem.out)[ncol(v.dem.out)]<-paste0(e,vlab)
            v.dem.out[which(v.dem.out$year==v.dem.out[,which(colnames(v.dem.out)==paste0(e,".start"))]),which(colnames(v.dem.out)==paste0(e,vlab))]<-0 #minval
            v.dem.out<-as.data.frame(v.dem.out)

      #calculate stock
pb<-txtProgressBar(min=0, max=max(as.numeric(v.dem.out$year)-as.numeric(unlist(v.dem.out[,which(colnames(v.dem.out)==paste0(e,".start"))])), na.rm=T), style=3)

      for(y in 1:max(as.numeric(v.dem.out$year)-as.numeric(unlist(v.dem.out[,which(colnames(v.dem.out)==paste0(e,".start"))])), na.rm=T)){
          setTxtProgressBar(pb, value=y)

            v.dem.out<-v.dem.out %>% group_by(country_id) %>% dplyr::mutate(Lval=dplyr::lag(!!as.name(paste0(e,vlab))))
            v.dem.out<-v.dem.out %>% group_by(country_id) %>% dplyr::mutate(Lhist=dplyr::lag(!!as.name(paste0(e,".hist"))))
            #sets all other years to previous year's stock times depreciation, plus previous year's democracy level
            v.dem.out[which(is.na(v.dem.out[,which(colnames(v.dem.out)==paste0(e,vlab))])==T & 
                              (as.numeric(v.dem.out$year)-as.numeric(unlist(v.dem.out[,which(colnames(v.dem.out)==paste0(e,".start"))])))==y),
                      which(colnames(v.dem.out)==paste0(e,vlab))]<-(w*v.dem.out[which(is.na(v.dem.out[,which(colnames(v.dem.out)==paste0(e,vlab))])==T & 
                                                                                        (as.numeric(v.dem.out$year)-as.numeric(unlist(v.dem.out[,which(colnames(v.dem.out)==paste0(e,".start"))])))==y),]$Lval)+
                                                                  v.dem.out[which(is.na(v.dem.out[,which(colnames(v.dem.out)==paste0(e,vlab))])==T & 
                                                                                        (as.numeric(v.dem.out$year)-as.numeric(unlist(v.dem.out[,which(colnames(v.dem.out)==paste0(e,".start"))])))==y),]$Lhist
                  }
            close(pb)

      #replace one for first year
      v.dem.out[which(v.dem.out$year==v.dem.out[,which(colnames(v.dem.out)==paste0(e,".start"))]),which(colnames(v.dem.out)==paste0(e,vlab))]<-NA
      
      #drop stock values for antecedent years
      v.dem.out$orig.year<-NA; v.dem.out[which(is.na(v.dem.out[,which(colnames(v.dem.out)==e)])==F),]$orig.year<-v.dem.out[which(is.na(v.dem.out[,which(colnames(v.dem.out)==e)])==F),]$year
      v.dem.out<-v.dem.out%>%group_by(country_id)%>%mutate("keepyear"=min(orig.year, na.rm=T))%>%dplyr::select(-orig.year)
			v.dem.out[which(v.dem.out$year<v.dem.out$keepyear),which(colnames(v.dem.out)==paste0(e,vlab))]<-NA
  print(paste("Stock of ", e, " calculated based on weight ", w, " (", 100-(w*100), "% depreciation rate).", sep=""))
  print("Note: Stock will be missing after breaks between values.");cat("\n")
			v.dem.out[which(v.dem.out$year<v.dem.out$keepyear),which(colnames(v.dem.out)==paste0(e,vlab))]<-NA
			v.dem.out<-v.dem.out%>%dplyr::select(-keepyear)

			colnames(v.dem.out)[which(colnames(v.dem.out)==paste0(e,vlab))]<-paste0(e,vlab,"_raw")
						
      #apply PT transformation to the variable
      v.dem.out[,ncol(v.dem.out)+1]<- as.numeric(unlist(v.dem.out[,which(colnames(v.dem.out)==paste0(e,vlab,"_raw"))]))*(1-w); colnames(v.dem.out)[ncol(v.dem.out)]<-paste0(e,vlab)

      }}
	v.dem.out<<-v.dem.out
      }
  
#8. Remove unnecessary variables (and V-dem dataset); make year and country ID numeric
drop.vars<-function(x){
	for(e in x){v.dem.out<-v.dem.out[,-c(which(colnames(v.dem.out) %in% c(paste0(e,"_filled"), paste0(e,".hist"), paste0(e,"n"), paste0(e,".start"), "Lval", "Lhist", "startnew")))]}
  rm(v.dem, envir = .GlobalEnv)
  cat("V-Dem dataset removed.\n")
  if(length(grep("original", names(v.dem.out))>0)){v.dem.out<-v.dem.out[,-grep("original", names(v.dem.out))]}
      v.dem.out$country_id.hist<-as.numeric(v.dem.out$country_id.hist)
      v.dem.out$country_id<-as.numeric(v.dem.out$country_id)
      v.dem.out$year<-as.numeric(v.dem.out$year)
  v.dem.out<-v.dem.out[,c(
    which(names(v.dem.out)=="country_name"), 
    which(names(v.dem.out)=="country_id.hist"),
    which(names(v.dem.out)=="country_id"),
    which(names(v.dem.out)=="year"):ncol(v.dem.out))]
	v.dem.out<<-as.data.frame(v.dem.out)
}

#9. Assign optional name in global environment
assign.names<-function(n){
  if(is.character(n)==F){print("Error: output name must be a string.")}
  assign(n,v.dem.out, envir = .GlobalEnv)
	cat(paste0("Subset data stored as object '", n, "'.","\n"))
	cat(paste("New variables include measure with depreciation rate and measure rescaled using min/max method."))
	if(n!="v.dem.out" & exists("v.dem.out")==T & is.character(n)==T){rm(v.dem.out, envir = .GlobalEnv)}
}

##################################################
##################################################

x<-var
a<-add
v<-val
n<-name
f<-fill

### Prep it
load.packages()
get.data()

sub.data(x,a)
expand.data()
hist.states()
fill.data(x,f)
ante.data(x)

### Calculate stock
calc.stock(x,v)
drop.vars(x)
assign.names(n)
}

##################################################
##################################################