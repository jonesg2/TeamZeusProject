## <----- Description of data source: Table of cancer survival rates of BOTH genders in 2014 ------>

## Create data frame ## 
    Stages_Overall <- read.csv("Cancer_Stages.csv")
    df_Overall <- data.frame(Stages_Overall)

## Change column names ## 
    names(df_Overall)[1]<-paste("Cancer.Type") 
    names(df_Overall)[2]<-paste("Gender")
    
    
## <---- Plot 1 - Men Survial Rates 2014 ---->

    ## Subset all cancers related to men 
        df_Male <- df_Overall[df_Overall$Gender == "Male", ]
        df_Male
    
    ## Change data frame formot from wide to long
        head(df_Male)
        melt_df_Male <- melt(df_Male)
        melt_df_Male # View long fromat
        
    ## Plot multiple line graph of survival rates of each stage in men
        ggplot(data = melt_df_Male, aes(x=variable, y=value, group = Cancer.Type, colour = Cancer.Type)) +
              geom_line() +
              geom_point( size=4, shape=21, fill="white") +
              ylim(0,105) +
              labs(x="Stage of Cancer", y="Survival Rate %") +
              ggtitle("Cancer Survival Rates of Men (2014)")
    

## <---- Plot 2 - Women Survial Rates 2014 ---->  
        
    ## Subset all cancers related to women 
        df_Female <- df_Overall[df_Overall$Gender == "Female", ]
        df_Female
        
    ## Change data frame formot from wide to long
        head(df_Female)
        melt_df_Female <- melt(df_Female)
        melt_df_Female # View long fromat
        
    ## Plot multiple line graph of survival rates of each stage in men
        ggplot(data = melt_df_Female, aes(x=variable, y=value, group = Cancer.Type, colour = Cancer.Type)) +
              geom_line() +
              geom_point( size=4, shape=21, fill="white") +
              ylim(0,105) +
              labs(x="Stage of Cancer", y="Survival Rate %") +
              ggtitle("Cancer Survival Rates of Women (2014)")  
