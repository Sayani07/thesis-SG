# contains the script to preprocess the eight households and bring them in one file

preprocess <- function() {
  
id4 =  read_csv(here::here("paper/data-raw", "meter04.csv"),
                  skip = 1,
                  col_names = c("dtype","rc","rate_desc","start_date","end_date","kwh","register_val", "qual_flag")) %>% 
    separate(start_date, sep = " ", into = c("date", "stamp")) %>% 
    select(date, stamp, kwh)
  
  id_lookup = id4 %>%
    distinct(stamp) %>% 
    bind_cols(time = as.character(1:48))
  
  meter_id1 = read_csv(here::here("paper/data-raw", "meter01.csv"),
                       skip=2,
                       col_names = c("id", "date",  1:48),
                       cols(`2` = col_double())) %>% 
    filter(id == "300") %>% 
    mutate(date = ymd(date)) %>% 
    gather(time, kwh, -id, -date) %>% 
    left_join(id_lookup) %>% 
    mutate(reading_time = paste(date, stamp, ""), 
           reading_time = ymd_hm(reading_time)) %>% 
    select(reading_time, kwh)
  
  
  meter_id2 = read_csv(here::here("paper/data-raw", "meter02.csv"), 
                       skip=2,
                       col_names = c("id", "date",  1:48),
                       cols(`2` = col_double())) %>% 
    filter(id == "300")%>% 
    mutate(date = ymd(date)) %>% 
    gather(time, kwh, -id, -date) %>% 
    select(-id) %>% 
    left_join(id_lookup) %>% 
    mutate(reading_time = paste(date, stamp, ""), 
           reading_time = ymd_hm(reading_time)) %>% 
    select(reading_time, kwh)
  
  
  meter_id3 = read_csv(here::here("paper/data-raw", "meter03.csv"),
                       skip = 1,
                       col_names = c("nmi","msn","gen","date","est",1:48)) %>% 
    select(-c(nmi, msn, est, gen)) %>% 
    gather(time, kwh, -date) %>% 
    mutate(date = dmy(date))%>% 
    left_join(id_lookup) %>% 
    mutate(reading_time = paste(date, stamp, ""), 
           reading_time = ymd_hm(reading_time)) %>% 
    select(reading_time, kwh)
  
  
  meter_id4 =  id4 %>% 
    mutate(date = dmy(date),
           reading_time = paste(date, stamp, ""), 
           reading_time = ymd_hm(reading_time)) %>% 
    select(reading_time, kwh)
  
  meter_id5 = read_csv(here::here("paper/data-raw", "meter05.csv"), 
                       skip=1,
                       col_names = c("id", "date",  1:48),
                       cols(`2` = col_double())) %>% 
    filter(id == "300")%>% 
    mutate(date = ymd(date)) %>% 
    gather(time, kwh, -id, -date) %>% 
    select(-id) %>% 
    left_join(id_lookup) %>% 
    mutate(reading_time = paste(date, stamp, ""), 
           reading_time = ymd_hm(reading_time)) %>% 
    select(reading_time, kwh)
  
  meter_id6 = read_csv(here::here("paper/data-raw", "meter06.csv"),
                       skip = 1,
                       col_names = c("gen","date","est",1:48)) %>% 
    select(-c(est, gen)) %>% 
    gather(time, kwh, -date) %>% 
    mutate(date = dmy(date))%>% 
    left_join(id_lookup) %>% 
    mutate(reading_time = paste(date, stamp, ""), 
           reading_time = ymd_hm(reading_time)) %>% 
    select(reading_time, kwh)
  
  
  meter_id7 = read_csv(here::here("paper/data-raw", "meter07.csv"), 
                       skip=1,
                       col_names = c("id", "date",  1:48),
                       cols(`2` = col_double())) %>% 
    filter(id == 300)%>% 
    mutate(date = ymd(date)) %>% 
    gather(time, kwh, -id, -date) %>% 
    select(-id) %>% 
    left_join(id_lookup) %>% 
    mutate(reading_time = paste(date, stamp, ""), 
           reading_time = ymd_hm(reading_time)) %>% 
    select(reading_time, kwh)
  
  
  meter_id8 = read_csv(here::here("paper/data-raw", "meter08.csv"),
                       skip = 1,
                       col_names = c("id", "date",  1:48),
                       cols(`1` = col_double(),
                            `2` = col_double(),
                            `3` = col_double(),
                            `6` = col_double())) %>% 
    filter(id == 300)%>% 
    mutate(date = ymd(date)) %>% 
    gather(time, kwh, -id, -date) %>% 
    select(-id) %>% 
    left_join(id_lookup) %>% 
    mutate(reading_time = paste(date, stamp, ""), 
           reading_time = ymd_hm(reading_time)) %>% 
    select(reading_time, kwh) 
  
  
  
  
  # %>% 
  #   mutate(reading_time = case_when(
  #     duplicated(reading_time) ~ reading_time + hours(1),
  #     
  #     TRUE ~ reading_time
  #   ))
  # 
  
  
  
  
  # bind all data together
  
  elec_all = bind_rows(meter_id1,
                       meter_id2,
                       meter_id3,
                       meter_id4, 
                       meter_id5,
                       meter_id6,
                       meter_id7,
                       meter_id8,
                       .id = "household_id") %>% 
    group_by(household_id, reading_time) %>% 
    mutate(meter_id = row_number()) %>% 
    ungroup() %>% 
    as_tsibble(index = reading_time,
               key = c(household_id, meter_id)) %>% 
    rename("reading_datetime" = "reading_time") %>% 
    select(household_id, meter_id, reading_datetime, kwh)
  
  #write_rds(elec_all, "data/elec_all.rds")
  return(elec_all)
}


elec_all = preprocess()

write_rds(elec_all, "data/elec_all-8.rds")