#our custom zooniverse functions, in alphabetical order.


##choose_my_workflow: R function to prompt user to select proper workflow and workflow version. Requires that the user know which workflow and workflow version they desire.
choose_my_workflow<-function(df){
  #turn on dplyr
  require(dplyr)

  select_workflow<- function(df) {
    #identify workflows by making workflow_id into a factor
    df$workflow_id<-as.factor(df$workflow_id)
    workflows<-levels(df$workflow_id)
    return(workflows)
    #works
  }

  workflows<- select_workflow(df)

  #now ask user which workflow to use

  my_selection<- menu(workflows,
                      title= "Which workflow do you want?")

  my_selection_filtered<-workflows[my_selection]
  #now find the desired version of the workflow
  DF2<-filter(df, workflow_id == my_selection_filtered)
  DF2$workflow_version<-as.factor(
    DF2$workflow_version)

  versions<-levels(DF2$workflow_version)

  my_wkflow_version<- menu(
    versions,
    title = "Which workflow version do you want?")
  #now filter df to that workflow version

  DF3<-filter(
    DF2,
    workflow_version == versions[my_wkflow_version])
}

#flatten_json function
#now flatten data to relevant tasks within a classification
flatten_json<-function(json_data){
  flat_to_task<-json_data %>%
    select(., subject_ids, user_name, classification_id, workflow_version, annotations) %>%
    as.tbl_json(json.column = "annotations") %>%
    gather_array(column.name = "task_index") %>%  #really important for joining later
    spread_values(
      task = jstring("task"),
      task_label = jstring("task_label")) %>%
    enter_object("value") %>%
    gather_array %>%
    spread_all
  return(flat_to_task)
}

#random_subset_dataframe:function to subset a large dataset to get a smaller one for testing code
random_subset_dataframe <- function(df) {
  #turn on dplyr
  require(dplyr)
  #establish how many samples the user would like
  sample_size<-as.integer(readline(prompt = "How many rows of data would you like to sample? "))
  #select sample_size rows from DF using sample_n from dplyr
  NewDF<-sample_n(df, sample_size)
  return(NewDF)
}

#View_annotations function to view json data in annotations column
#need to provide n, number of records to view
View_annotations<-function(jdata,n ){
  for (i in 1:n){
    jdata$annotations[i] %>% prettify %>% print
  }
}

#View_subject_data function to view json data in subject data column
#need to provide n, number of records to view
View_subject_data<-function(jdata, n){
  for(i in 1:n){
    jdata$subject_data[i] %>% prettify %>% print
  }
}

