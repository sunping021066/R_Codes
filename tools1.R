
library(haven)
# use sql ;
# uses SQLite, refer to https://www.sqlite.org/lang_corefunc.html for sql functions avaiable to sqldf ;
library(sqldf)

# library (psych)

library (lattice)
library (ggplot2)

library (dplyr)

#  install.packages ("RNetLogo", dependencies = TRUE )
# library (RNetLogo)

# -------------------------------------------------------------------------------------------------------------------------------------
#
# -------------------------------------------------------------------------------------------------------------------------------------

vectorEmbeddedString <- function (String_Vector
                                  , delimiter = " , "
                                  , formula = "this String__ is a test for String__" 
                                  , replace = "String__"       ) {
  
  # String_Vector <- c("S1", "S2", "S3")
  # delimiter     <- " UNION "
  # formula       <- "this String__ is a test for String__"
  # replace       <- "String__" 
  
  b <- String_Vector
  
  for (i in 1:length(String_Vector)) {
    b[i] <- gsub (replace, String_Vector[i], formula)
  }
  
  # combine the elements with the specified delimiter
  b <- paste ( b , collapse = delimiter )
  
  return (b) 
  
}

# vectorEmbeddedString (String_Vector = c("S1", "S2", "S3")
#                       , delimiter = " union "
#                       , formula = "Select String__ from dataset group by String__"
#                       , replace = "String__"       )


# -------------------------------------------------------------------------------------------------------------------------------------
#
# -------------------------------------------------------------------------------------------------------------------------------------



chkdata <- function (df, n=10, show_original_rownum=FALSE ) {
  if ( all(as.numeric(rownames(df)) == 1:nrow(df)) && show_original_rownum ) {
    df$row_num_123_ <- paste(rownames(df),sep='_')
    df <- df[,c(ncol(df),1:ncol(df)-1)]
  }
  df.sample <- df[sort(sample(1:nrow(df),min(n,nrow(df)),replace = FALSE)), ]
  str(df)
  View(df.sample)
  print(paste('Showing', n,'rows out of the total of',nrow(df),'rows', sep=' '))
  df.sample
}

# -------------------------------------------------------------------------------------------------------------------------------------
#
# -------------------------------------------------------------------------------------------------------------------------------------

addcomma <- function ( String, current_delimiter=" |\n",  new_delimiter=",", unique=FALSE, quote=FALSE) {
  
  # current_delimiter = " |\n" # space or line feed
  # unique            = FALSE  # when TRUE, the duplicate elements in original string are kept only once
  # quote             = FALSE  # when TRUE, each elements are quoted with single quote
  
  # a <-'JOB_CODE_DESC PROVIDER_SPECIALTY RAD PATH ANES CO CITY ZIP LOC_NUM
  # LOC_CITY LOC_ZIP LOC_COUNTY ATTEST_OTHER_YN GRAD_YN ATTEST_NEEDED_YN NPI_ELIG VST_ELIG
  # MEDICARE_VST_ELIG DIR_HRS_CHK DIR_HRS_ELIG OP_DIR_PCT_ELIG ATTEST_ELIG
  # ALL_CHK ALL_ELIG LABOR_QUALIFY
  # ep_flag_stage2 prov_name prov_area prov_dept
  # GL_DEPT_CD  GL_DEPT_NM CC_CD CPM_RESOURCE_ID  EMPL_ID   JOB_FAMILY_DESC'
  
  # get all the elements seperated by space or new line 
  b <- strsplit ( String , current_delimiter )[[1]]
  
  # get unique elements 
  if ( unique == TRUE ) {
    b <- unique(b)      
  }
  
  # drop the missing elements resulted from having multiple delimiters between two elements
  b <- b[b != ""]
  
  if ( quote == TRUE ) {
    b <- mapply(paste0, "'",b,"'")      
  }
  
  # combine the elements with the specified delimiter
  b <- paste ( b , collapse = new_delimiter )

  return(b)
  
}


# -------------------------------------------------------------------------------------------------------------------------------------
#
# -------------------------------------------------------------------------------------------------------------------------------------
to_vartype <- function (df, cols_string, to_var_type = 'factor', stringtype='Vector', print=FALSE) {
  
    # to_var_type   :   factor, character, integer, numeric, logical  
  
    # df          <- data.frame( var1="", var2="", var3="", var4=FALSE)
    # cols_string <- 'var1 var2'
    # to_var_type <- 'character'
    # 
    # str(df)
    # cols_string
    # to_var_type
    
    # stringtype='Vector' vs 'String':  if not Vector, need to convert and store the col names in a vector
  
    if ( stringtype == 'Vector') {
        d <- cols_string
    } else {
        d <- strsplit ( addcomma (cols_string, unique=TRUE),"," ) [[1]]      
    }

    df[, d]  <- lapply ( df[, d], as.symbol(paste('as.',to_var_type, sep='')) )
    
    if ( print == TRUE ) {str(df[,d]) }
    
    return (df)
    
}


# -------------------------------------------------------------------------------------------------------------------------------------
#
# -------------------------------------------------------------------------------------------------------------------------------------

freq_all_factor <- function ( df )  {

  # all factor variables  
  factorVariables <- colnames ( df ) [ sapply ( df, is.factor )]

  # bar graph for all factor variables
  
  for ( i in 1:length ( factorVariables ) ) {
    
    a <- factorVariables[i]
    
    b <- paste(deparse(substitute(df)),'$',a,sep='')
    
    c <- eval(parse(text=b))
    
    barplot( table( c ), col ="wheat", main=b )    

    rm ( list = c('a','b','c'))
  }
  
  # -------------  to generate the freq tables for alol factor variables ------------------
  
  df_name <- deparse(substitute(df)) 

  # # vector of strings translates to vector of sql statements for each of the variables ;
  factorVariables_SQL <- paste ( 'Select ', "'",factorVariables,"' as factor_name,"," ", factorVariables, ' as factor_value, count(', factorVariables, ') as N_Freq from ', df_name, ' group by ' ,factorVariables ,sep='' )

  # the overall sql statement for all variables
  f <- paste(factorVariables_SQL, collapse = ' UNION ')

  f_count <- sqldf( f )
  
  f_count <- sqldf (" 
              Select A.factor_name, A.factor_value, A.N_Freq, 1.0*A.N_Freq / B.N_Total as Percent 
              From f_count A
              left join ( Select factor_name, sum(N_Freq) as N_Total
              From f_count
              group by factor_name
              ) B on A.factor_name = B.factor_name 
              ")

  return ( f_count )
  
}

# a <- freq_all_factor ( prov ) 
# freq_all_factor ( prov[,c('LABOR_QUALIFY')])



# -------------------------------------------------------------------------------------------------------------------------------------
#
# -------------------------------------------------------------------------------------------------------------------------------------

hist_all_num <- function ( df )  {
  
  # all numeric or integer variables 
  df.num <- df [, c( colnames ( df ) [ sapply ( df, is.numeric )], colnames ( df ) [ sapply ( df, is.integer )] ) ]
  
  # str(df.num)
  
  summary ( df.num )
  
  for ( i in 1:length(df.num)) {
    
    a <- colnames(df.num)[i]
    
    b <- paste(deparse(substitute(df)),'$',a,sep='')
    
    c <- eval(parse(text=b))
  
#   hist( c, col = "blue", main=b, breaks = 100 )      # number of bars 
#   hist( c, col = "blue", main=b )
    
    hist( c, main=b )    
    rug( c )  
    abline ( v = median (c, na.rm=TRUE), col = "magenta", lwd=2)
    abline ( v = mean(c, na.rm=TRUE)  , col = "black"  , lwd=2)
    
#   boxplot ( C, col = "blue" )                        # boxplot

    rm ( list = c('a','b','c'))
  }
  
}

# hist_all_num ( prov ) 


# -------------------------------------------------------------------------------------------------------------------------------------
#
# -------------------------------------------------------------------------------------------------------------------------------------

drop_elements <- function (whole, toBeDropped=c(NULL), toBeRetained=c(NULL)) {

    # whole <- c("1","2","3")
    # toBeDropped <- c("1","2")
    # toBeRetained <- c("2")
    
    a <- whole %in% toBeDropped
    b <- whole %in% toBeRetained
    
    return ( whole [ !a | b ] )

}

# drop_elements ( c("1","2","3") , toBeDropped = c("1","2") ,toBeRetained = c("2") )


# -------------------------------------------------------------------------------------------------------------------------------------
#
# -------------------------------------------------------------------------------------------------------------------------------------


categorizeCol <- function ( df ,in_var=NULL ,out_var="CAT_" ,Group_E=NULL 
                           ,bygroup=NULL        # not be able to do within by group assignment for now
                           ,debug=0 ) {
  
  # bygroup is not currently implemented ;
  # 
  # df <- prov
  # in_var <- 'OP_DIR_HRS_YTD'
  # bygroup <- 'LABOR_QUALIFY'
  # Group_E <- "(OP_DIR_HRS_YTD < 1000) @2 , ((1000 <= OP_DIR_HRS_YTD) and (OP_DIR_HRS_YTD < 2000)) @2 , @3"
  # out_var <- 'CAT_'
  # out_varm <- 'Cat_M'
  # out_var_range <- 'Cat_R'
  
  Group_E_ <- strsplit ( Group_E,"," ) [[1]]
  length(Group_E_)

  a <- list(1:3)
  df_sub <- list(1:3)
  out <- data.frame()
  
  for (i in 1:length(Group_E_)) {
 
    b <- strsplit ( Group_E_[i],"@" ) [[1]] 
    # print (paste(i,' 1: ',b[1],' 2: ',b[2], sep=' '))
    a[i] <- list(b)
    
    #  if specified, group subdata 
    if ( nchar(trimws(a[[i]][1])) > 0) {
      t <- paste ( "select * from df where ", a[[i]][1], sep='' )
      not_t <- paste ( "select * from df where not (", a[[i]][1],")", sep='' )      
      df_sub[i] <- list( sqldf (t)  )
      df <- sqldf (not_t)  
    } else {
      df_sub[i] <- list(df)
    }
    
    # if specified, get sub-group variable within subdata
    if ( nchar(trimws(a[[i]][2])) > 0 ) {
      # need a better way to subgroup data: ntile may categorize obs with the same value into different group, to make N_sub comparable
      t <- paste("ntile( df_sub[[i]]$",in_var,",", a[[i]][2],")",sep='')
      df_sub[[i]][c(out_var)] <- paste("Sub_",i,"_",eval(parse(text=t)),sep='') 
    }  else {
      df_sub[[i]][c(out_var)] <- paste("Sub_",i,sep='')
    }
    
    out <- rbind ( out, df_sub[[i]]  )
  }

  sqlCode <- paste ( "select ", out_var
               , "       , count(*) as N_total "
               , "       , count(case when ", in_var, " is not null then ", in_var, " end) as N_not_missing"
               , "       , avg(", in_var, ") as ", out_var, "_mean"
               , "       , median(", in_var, ") as ", out_var, "_median"
               , "       , \"[\"||cast(min(", in_var,") as text)||\" - \"||cast(max(",in_var,") as text)||\"]\" as ",out_var,"_range"
               , "  from out "
               , "  group by ", out_var 
               , sep= "" )
    
    
  #  writeLines(sqlCode)
  
    df_stats <- sqldf ( sqlCode )

    df_stats_var <- c(out_var, paste0(out_var,"_","median"), paste0(out_var,"_","range"));

    out <- merge ( x = out, y=df_states[ ,df_stats_var], by = out_var )
    
    out <- to_vartype (out[, c(colnames(df),df_stats_var)], df_stats_var, to_var_type = 'factor') 

    df_stats      
    
    par(mar=c(1,1,1,1))
    boxplot ( eval(parse(text=in_var)) ~ eval(parse(text=out_var)), data = out, col = "green" )
    
    return ( out )
    
}

 Prov <- categorizeCol ( prov
               ,in_var = 'OP_DIR_HRS_YTD'
               ,out_var="CAT_"
               ,Group_E = "(OP_DIR_HRS_YTD < 1000) @2 , ((1000 <= OP_DIR_HRS_YTD) and (OP_DIR_HRS_YTD < 2000)) @2 , @3" )



