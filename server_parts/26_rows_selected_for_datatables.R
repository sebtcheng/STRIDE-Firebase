# Rows Selected for Tables
# 
# This file handles the "Row Click" events for DETAILED tables.
# It now safely accesses the filtered data from '25_mapping_run.R'
# to ensure the row you click is exactly the row you get.
#
# UPDATED: Added safety checks for missing columns to prevent "pivot_longer" crashes.

# =========================================================
# 1. TEXT TABLE (Quick School Search)
# =========================================================
observeEvent(input$TextTable_rows_selected, {
  
  # This uses data_snapshot from '22_quick_school_search.R'
  req(exists("data_snapshot")) 
  mainreact1 <- data_snapshot()
  req(nrow(mainreact1) > 0)
  
  # Apply filters
  df1 <- reactive({
    if (is.null(input$TextMapping_bounds)) {
      mainreact1
    } else {
      bounds <- input$TextMapping_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      subset(mainreact1, Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
    }
  })
  
  # Safe Row Selection
  req(input$TextTable_rows_selected)
  req(nrow(df1()) >= input$TextTable_rows_selected)
  row_selected = df1()[input$TextTable_rows_selected,]
  
  # Zoom Map
  leafletProxy("TextMapping") %>%
    flyTo(lng = row_selected$Longitude, lat = row_selected$Latitude, zoom = 15)
  
  # --- GENERATE TABLES ---
  
  # Helper to pivot safely
  safe_pivot <- function(data, cols_to_select, table_name) {
    # Select only columns that actually exist in the data
    temp <- data %>% select(any_of(cols_to_select))
    
    # Check if we successfully selected any columns
    if (ncol(temp) > 0) {
      temp %>% 
        mutate(dplyr::across(everything(), as.character)) %>% 
        pivot_longer(cols = everything(), names_to = table_name, values_to = "Data")
    } else {
      # Return a placeholder if no columns match
      data.frame(Message = paste("No", table_name, "available"))
    }
  }

  output$schooldetails <- renderTable({ 
    safe_pivot(row_selected, c("Region","Province","Municipality","Division","District","Barangay","Street.Address","SchoolID","School.Name","School.Head.Name","SH.Position","Implementing.Unit","Modified.COC","Latitude","Longitude"), "Basic Info")
  }, rownames = FALSE, colnames = TRUE, hover = TRUE, bordered = TRUE)

  output$schooldetails2 <- renderTable({ 
    safe_pivot(row_selected, c("ES.Excess","ES.Shortage","JHS.Excess","JHS.Shortage","SHS.Excess","SHS.Shortage","ES.Teachers","JHS.Teachers","SHS.Teachers","ES.Enrolment","JHS.Enrolment","SHS.Enrolment","School.Size.Typology","Clustering.Status","Outlier.Status"), "HR Data")
  }, rownames = FALSE, colnames = TRUE, hover = TRUE, bordered = TRUE)

  output$schooldetails3 <- renderTable({ 
    safe_pivot(row_selected, c("Buildings","Instructional.Rooms.2023.2024","Classroom.Requirement","Est.CS","Buidable_space","Major.Repair.2023.2024","SBPI","Shifting","OwnershipType","ElectricitySource","WaterSource","Total.Seats.2023.2024","Total.Seats.Shortage.2023.2024"), "Infra Data")
  }, rownames = FALSE, colnames = TRUE, hover = TRUE, bordered = TRUE)

  output$schooldetails4 <- renderTable({ 
    safe_pivot(row_selected, c("SchoolID","Division","Modified.COC","School.Name","SHS.Packages"), "Learning Resources")
  }, rownames = FALSE, colnames = TRUE, hover = TRUE, bordered = TRUE)
})


# =========================================================
# 2. SHS TABLE - DETAILED VIEW (Tracks & Strands)
# =========================================================
observeEvent(input$SHSListTable_rows_selected, {
  
  req(input$SHSListTable_rows_selected)
  
  # LINK TO FILE 25: Use the same reactive data displayed in the table
  req(exists("data_SHS_filtered")) 
  current_data <- data_SHS_filtered() 
  req(nrow(current_data) >= input$SHSListTable_rows_selected)
  
  row_selected <- current_data[input$SHSListTable_rows_selected, ]
  
  # --- 1. Basic Info ---
  output$SHSTablex <- renderTable({
    # Use any_of to ignore missing columns like School.Head.Name if they don't exist
    cols_check <- c("SchoolID", "School.Name", "School.Head.Name", "SH.Position", "Implementing.Unit", "Modified.COC", "Offering")
    temp <- row_selected %>% select(any_of(cols_check))
    
    if (ncol(temp) > 0) {
      # Optional: Rename columns if they exist
      if("Modified.COC" %in% names(temp)) temp <- rename(temp, "Modified Curricular Offering" = "Modified.COC")
      if("School.Head.Name" %in% names(temp)) temp <- rename(temp, "School Head" = "School.Head.Name")
      
      temp %>% 
        mutate(dplyr::across(everything(), as.character)) %>% 
        pivot_longer(cols = everything(), names_to = "Basic Info", values_to = "Data")
    } else {
      data.frame(Info = "Basic info not available")
    }
  }, rownames = FALSE, colnames = TRUE, hover = TRUE, bordered = TRUE)
  
  # --- 2. Track Offerings ---
  # CRASH FIX: The logs show 'Academic', 'TVL', etc. do NOT exist in data_SHS.
  # We check for them first. If missing, we show the G11/G12 specific breakdown if available, or just a message.
  output$PilotSpec <- renderTable({
    # Check for summary columns OR specific grade level columns
    cols_check <- c("Academic", "TVL", "Arts.Design", "Sports", 
                    "G11.ACAD", "G11.TVL", "G12.ABM", "G12.HUMMS", "G12.STEM", "G12.TVL") 
    
    temp <- row_selected %>% select(any_of(cols_check))
    
    if (ncol(temp) > 0) {
      temp %>% 
        mutate(dplyr::across(everything(), as.character)) %>% 
        pivot_longer(cols = everything(), names_to = "Track/Strand", values_to = "Enrolment")
    } else {
      data.frame(Track = "Track data not found in dataset")
    }
  }, rownames = FALSE, colnames = TRUE, hover = TRUE, bordered = TRUE)
  
  # --- 3. Subject Specialization (Summed) ---
  # The logs confirm columns like "English", "Mathematics" DO exist, so this should work.
  output$PilotSpec2 <- renderTable({
    cols_check <- c("English","Mathematics","Science","Biological.Sciences","Physical.Sciences","General.Ed","Araling.Panlipunan","TLE","MAPEH","Filipino","ESP","Agriculture","ECE","SPED")
    
    temp <- row_selected %>% select(any_of(cols_check))
    
    if (ncol(temp) > 0) {
      temp %>% 
        mutate(dplyr::across(everything(), as.character)) %>% 
        pivot_longer(cols = everything(), names_to = "Profile", values_to = "Data") %>% 
        mutate(Data = as.numeric(Data)) %>% 
        group_by(Profile) %>% 
        summarise(Data = sum(Data, na.rm = TRUE))
    } else {
      data.frame(Profile = "Subject data not found in dataset", Data = "")
    }
  }, rownames = FALSE, colnames = TRUE, hover = TRUE, bordered = TRUE)
})


# =========================================================
# 3. FACILITIES TABLE (EFD) - DETAILED VIEW
# =========================================================
observeEvent(input$FacListTable_rows_selected, {
  
  req(input$FacListTable_rows_selected)
  
  # LINK TO FILE 25: Use the same reactive data
  req(exists("data_Fac_filtered"))
  current_data <- data_Fac_filtered()
  req(nrow(current_data) >= input$FacListTable_rows_selected)
  
  row_selected <- current_data[input$FacListTable_rows_selected, ]
  
  # Generate Detail Table
  fac_table_data <- reactive({
    # Use any_of because Project.Description and Mode.of.Implementation are missing in your logs
    cols_check <- c("School.Name", "SchoolID", "FundingYear", "Category", "Allocation", "Project.Description", "Mode.of.Implementation", "Status", "Completion")
    
    temp <- row_selected %>% select(any_of(cols_check))
    
    if (ncol(temp) > 0) {
      temp %>% 
        mutate(dplyr::across(everything(), as.character)) %>% 
        pivot_longer(cols = everything(), names_to = "Facility Info", values_to = "Data")
    } else {
      data.frame(Info = "No facility details available")
    }
  })
  
  # Output to specific Facilities ID
  output$FacTableDetail <- renderTable({ fac_table_data() }, rownames = FALSE, colnames = TRUE, hover = TRUE, bordered = TRUE)
  
  # Output to Generic ID (Fallback)
  output$schooldetails <- renderTable({ fac_table_data() }, rownames = FALSE, colnames = TRUE, hover = TRUE, bordered = TRUE)
})