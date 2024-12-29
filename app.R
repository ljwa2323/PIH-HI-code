library(shiny)
library(data.table)
library(lcmm)
library(xgboost)
library(magrittr)

# 加载模型
m7 <- readRDS("./www/m7_model.rds")
xgboost_model <- readRDS("./www/xgb_model_huaxi.rds")

column_names <- list(
  age = "Age",
  sex = "Sex",
  bmi = "BMI",
  asa = "ASA Physical Status Classification",
  emop = "Emergency Operation",
  surgery_siteAbdomen = "Surgery Site: Abdomen",
  surgery_siteBones_and_Joints = "Surgery Site: Bones and Joints",
  surgery_siteCentral_Nervous_System_and_Cranial_Nerves = "Surgery Site: Central Nervous System and Cranial Nerves",
  surgery_siteHead_and_Neck = "Surgery Site: Head and Neck",
  surgery_siteHeart_and_Great_Vessels = "Surgery Site: Heart and Great Vessels",
  surgery_siteOther = "Surgery Site: Other",
  surgery_siteRespiratory_System = "Surgery Site: Respiratory System",
  surgery_siteSkin_and_Soft_Tissue = "Surgery Site: Skin and Soft Tissue",
  surgery_siteUrinary_and_Reproductive_Systems = "Surgery Site: Urinary and Reproductive Systems",
  have_vaso = "Vasopressor Usage",
  mdz = "Midazolam Usage",
  ppf = "Propofol Usage",
  ppfi = "Propofol Infusion",
  rfti = "Remifentanil Infusion",
  sft = "Sufentanil Total",
  etdes = "End-tidal Desflurane",
  etsevo = "End-tidal Sevoflurane",
  bp_control = "Blood Pressure Control",
  ACEI = "ACE Inhibitors",
  ARB = "Angiotensin II Receptor Blockers",
  LXP = "Lipid Lowering Medication",
  hypo_med_other = "Other Hypotensive Medications",
  Essential_primary_hypertension = "Essential Primary Hypertension",
  Coronary_heart_disease = "Coronary Heart Disease",
  Congestive_heart_failure = "Congestive Heart Failure",
  Atrial_fibrillation_and_flutter = "Atrial Fibrillation and Flutter",
  Abnormalities_of_heart_beat = "Heart Rhythm Abnormalities",
  Diabetes_mellitus = "Diabetes Mellitus",
  Cerebral_infarction = "Cerebral Infarction",
  Transient_cerebral_ischemic_attacks_and_related_syndromes = "Transient Cerebral Ischemic Attacks",
  Emphysema_or_chronic_obstructive_pulmonary_disease = "Chronic Obstructive Pulmonary Disease",
  Asthma = "Asthma",
  Abnormalities_of_breathing = "Breathing Abnormalities",
  Chronic_kidney_disease_CKD = "Chronic Kidney Disease",
  Chronic_viral_hepatitis = "Chronic Viral Hepatitis",
  Liver_disease = "Liver Disease",
  Gastroesophageal_reflux_disease = "Gastroesophageal Reflux Disease",
  Anemia = "Anemia",
  Disorder_of_thyroid = "Thyroid Disorder",
  Pre_CRRT = "Preoperative Continuous Renal Replacement Therapy"
)


recode_map <- c('1' = 2, '2' = 2, '3' = 1, '4' = 3, '5' = 4, '6' = 3, '7' = 4)

ds_ref <- read.csv("./www/huaxi_traj_7_to_4_all.csv")
ds_ref$mean[ds_ref$class == 1 & ds_ref$time <= 60]

ds_OR  <- read.csv("./www/hx_4_from_7.csv")
ds_OR[[3]] <- rep(paste0(rep("   ",3),collapse = ""), nrow(ds_OR))
names(ds_OR)[3] <- "   "
names(ds_OR)[4] <- "OR (95% CI)"

aggregate_data <- read.csv("./www/huaxi_traj_7_to_4_all.csv")

default_mbp <- c(80, 70, 60, 65, 67,66, 64,66,66,67,66,66,66)
default_mbp <- c(default_mbp, rep(NA, 13 - length(default_mbp)))  # 尾部填充NA

# View(ds_OR)
# Define UI
ui <- fluidPage(

  shinyjs::useShinyjs(),  # 初始化 shinyjs

  div(style = "text-align: left; width: 100%;", img(src = "huaxi_logo.png", style="width:100%; height:auto;")),
  mainPanel(style = "width: 100%; margin: auto;",
    tabsetPanel(id = "mainTabset",  # 给 tabsetPanel 添加一个 ID
      tabPanel(title = HTML("<span style='font-size: 18px; color: #4472C4; font-weight: bold;'>Home</span>"), class = "first-tab",
        # 使用图片替换原有的欢迎信息和描述
        tags$head(
            tags$style(HTML("
                .first-tab .container {
                    display: flex;
                    align-items: center;
                }
                .first-tab .text-content {
                    flex: 1;
                }
                .first-tab .image-content {
                    flex: 1;
                }
                .first-tab img {
                    width: 100%;
                    height: auto;
                }
                .sidebar, .mainPanel {
                  background-color: #333;
                  color: #fff;
                }
                /* 添加这个新的 CSS 规则 */
                .shiny-table td:first-child {
                    text-align: left !important;
                }
                .shiny-table {
                    margin: auto !important;
                    width: 850px !important;
                }
            "))
        ),
        tags$h1("Welcome to PIH-HI Classification - A Trajectory Classification System for Post-induction Hypotension!"),
        div(class = "container", style="text-align: left; width: 100%;",
            div(class = "text-content",
                tags$p("This system is a machine learning-based prediction tool designed to help clinicians classify and predict blood pressure trajectories after anesthesia induction. By inputting patient data including basic information, surgery-related information, medication usage, and comorbidities, the system can predict the pattern of blood pressure changes following anesthesia induction.",
                      style = "font-size: 18px;"),  # 增大字体大小
                      
                tags$p("Through the navigation bar, you can easily switch between different functional pages. To promote transparency and facilitate local adaptation, all source code for this system is publicly available at ", 
                      tags$a(href="https://github.com/ljwa2323/PIH-HI-code", "https://github.com/ljwa2323/PIH-HI-code"), 
                      ". We hope this tool will provide valuable reference for your clinical decision-making.",
                      style = "font-size: 18px;"),  # 增大字体大小

                tags$p("You can utilize this system through the following modules:", style="font-size: 18px; color: black; text-decoration: underline;")
            )
        ),
        
        tags$div(style = "background-color: #f0f0f0; border-radius: 8px; padding: 10px; margin-bottom: 20px;",
            tags$ul(
                tags$li(tags$b("Prediction:"), " Input detailed patient information on this page, including basic information (age, gender, etc.), surgical information, use of vasoactive drugs, anesthetic induction medications, hypertension control status, and preoperative comorbidities.", style="font-size: 18px;"),
                tags$li(tags$b("Classification:"), " View blood pressure trajectory classification results, including trajectory plots, event odds ratios, and feature importance analysis.", style="font-size: 18px;"),
                tags$li(tags$b("Trajectory References:"), " Access reference information about blood pressure trajectory classification.", style="font-size: 18px;")
            )
        ),
        HTML("<br><br><br><br><br><br><br><br><br><br><br><br>")
      ),
      tabPanel(title = HTML("<span style='font-size: 18px; color: #4472C4; font-weight: bold;'>Prediction</span>"),  # 放大 Prediction tab 的标题
        sidebarLayout(
          sidebarPanel(
            HTML("<p style='color: #4472C4; font-weight: bold; font-size: 14px;'>Base Information</p>"),
            numericInput("age", "Age, years", value = 23, min = 0, max = 150, step = 1),
            selectInput("sex", "Sex", choices = c("Male", "Female"), selected = "Male"),
            numericInput("bmi", "Body mass index(BMI), kg/m^2", value = 19.6, min = 0, max = 50),
            numericInput("asa", "American society of anesthesiologists physical status(ASA)", value = 2, min = 1, max = 5, step = 1),
            HTML("<br>"),
            HTML("<p style='color: #4472C4; font-weight: bold; font-size: 14px;'>Hypertension Control and Medication</p>"),
            selectInput("bp_control", "Hypertension control status", choices = c("Not Needed", "Well Controlled", "Poorly Controlled"), selected = "Not Needed"),
            checkboxGroupInput("medication", "Hypertensive drugs:", choices = list(
              "Angiotensin-Converting Enzyme Inhibitor" = "ACEI",
              "Angiotensin II Receptor Blocker" = "ARB",
              "Reserpine" = "LXP",
              "Other Antihypertensive Medications" = "hypo_med_other"
            ), selected = c()),
            HTML("<br>"),
            checkboxGroupInput("medical_history", HTML("<span style='color: #4472C4; font-weight: bold; font-size: 14px;'>Preoperative Comorbidities:</span>"), choices = list(
              "Essential(primary) hypertension" = "Essential_primary_hypertension",
              "Coronary heart disease" = "Coronary_heart_disease",
              "Congestive heart failure" = "Congestive_heart_failure",
              "Atrial fibrillation and flutter" = "Atrial_fibrillation_and_flutter",
              "Abnormalities of heart beat" = "Abnormalities_of_heart_beat",
              "Diabetes mellitus" = "Diabetes_mellitus",
              "Cerebral infarction" = "Cerebral_infarction",
              "Transient cerebral ischemic attacks and related syndromes" = "Transient_cerebral_ischemic_attacks_and_related_syndromes",
              "Emphysema or chronic obstructive pulmonary disease" = "Emphysema_or_chronic_obstructive_pulmonary_disease",
              "Asthma" = "Asthma",
              "Abnormalities of breathing" = "Abnormalities_of_breathing",
              "Chronic kidney disease" = "Chronic_kidney_disease_CKD",
              "Chronic viral hepatitis" = "Chronic_viral_hepatitis",
              "Liver disease" = "Liver_disease",
              "Gastro-esophageal reflux disease" = "Gastroesophageal_reflux_disease",
              "Anemia" = "Anemia",
              "Thyroid disorder" = "Disorder_of_thyroid",
              "Dialysis" = "Pre_CRRT"
            ), selected = c()),
            HTML("<br>"),
            HTML("<p style='color: #4472C4; font-weight: bold; font-size: 14px;'>Surgery Information</p>"),
            selectInput("surgery_site", "Surgery Site:", choices = c(
              "Abdomen" = "surgery_siteAbdomen",
              "Bones and joints" = "surgery_siteBones_and_Joints",
              "Central nervous system and cranial nerves" = "surgery_siteCentral_Nervous_System_and_Cranial_Nerves",
              "Head and neck" = "surgery_siteHead_and_Neck",
              "Heart and great vessels" = "surgery_siteHeart_and_Great_Vessels",
              "Other" = "surgery_siteOther",
              "Respiratory system" = "surgery_siteRespiratory_System",
              "Skin and soft tissue" = "surgery_siteSkin_and_Soft_Tissue",
              "Urinary and reproductive systems" = "surgery_siteUrinary_and_Reproductive_Systems"
            ), selected = "surgery_siteSkin_and_Soft_Tissue"),
            selectInput("emop", "Emergent Operation:", choices = c("YES", "NO"), selected = "NO"),
            HTML("<br>"),
            HTML("<p style='color: #4472C4; font-weight: bold; font-size: 14px;' title='Guidelines for anesthesia induction medications: The first three items are for conventional intravenous induction, while the fourth can be used for TCI (Target Controlled Infusion) induction. The medications from remifentanil to sevoflurane represent the maintenance regimen from post-induction to the start of surgery.'>Anesthesia Induction Medication</p>"),
            numericInput("ppf", "Propofol dosage, mg:", value = 130, min = 0, max = 500),
            numericInput("sft", "Sufentanil level, ug:", value = 20, min = 0, max = 500),
            numericInput("mdz", "Midazolam dosage, mg:", value = 2, min = 0, max = 20),
            numericInput("ppfi", "Propofol infusion level, ug/ml:", value = 0, min = 0, max = 20),
            numericInput("rfti", "Remifentanil infusion rate, ng/ml:", value = 2.5, min = 0, max = 10),
            numericInput("etdes", "Desflurane concentration, %:", value = 0, min = 0, max = 10),
            numericInput("etsevo", "Sevoflurane concentration, %:", value = 0, min = 0, max = 10),
            HTML("<br>"),
            HTML("<p style='color: #4472C4; font-weight: bold; font-size: 14px;'>Vasoactive Drug</p>"),
            selectInput("have_vaso", "Planned Prophylactic or Therapeutic Use of Vsasopressors", choices = c("YES", "NO"), selected = "YES"),
            HTML("<br>"),
            actionButton("submit_new", "Submit"),
            actionButton("clear_new", "Clear")
          ),
          mainPanel(
            HTML("<p style='font-weight: bold;'>Prediction Result</p>"),
            plotOutput("pred_plot", width = "850px", height = "500px"),
            HTML("<br>"),
            HTML("<p style='font-weight: bold;'>Top 10 Feature Importance</p>"),
            plotOutput("fea_imp_plot", width = "850px", height = "500px")
          )
        )
      ),
      tabPanel(title = HTML("<span style='font-size: 18px; color: #4472C4; font-weight: bold;'>Classification</span>"),  # 放大 Classification tab 的标题
        sidebarLayout(
          sidebarPanel(
            HTML("<p style='color: #4472C4; font-weight: bold; font-size: 14px;'>Since Anesthesia Induction</p>"),
            lapply(0:12, function(i) {
              numericInput(paste0("mbp_input_", i*5), 
                          label = paste0("Enter MBP at ", i*5, " min:"), 
                          value = default_mbp[i + 1])
            }),
            HTML("<p style='color: #4472C4; font-weight: bold; font-size: 14px;'>Until Operation Begins</p>"),
            actionButton("submit", "Submit"),
            actionButton("clear", "Clear")
          ),
          mainPanel(
            HTML("<p style='font-weight: bold;'>Trajectory Plot</p>"),
            textOutput("classification"),
            plotOutput("plot_mbp", width = "850px"),
            HTML("<br><br>"),
            HTML("<p style='font-weight: bold;'>Event Odds Ratio</p>"),
            # 添加居中样式的 div 容器
            div(style = "display: flex; justify-content: center;",
              tableOutput("ds_OR_table") %>% 
                tagAppendAttributes(style = "margin: auto; width: 65%;")  # 或者使用 max-width: 600px
            ),
            HTML("<br><br>"),
            HTML("<p style='font-weight: bold;'>Feature Importance</p>"),            
            div(style = "text-align: center;",
              imageOutput("ref_class_image")
            )
          )
        )
      ),

      tabPanel(title = HTML("<span style='font-size: 18px; color: #4472C4; font-weight: bold;'>Trajectory References</span>"),  # 放大 Trajectory References tab 的标题
        sidebarPanel(
          div(style = "display: flex; flex-direction: column; gap: 10px; margin: 10px;",
            HTML("<p style='color: #4472C4; font-weight: bold; font-size: 14px;'>Select Phenotype:</p>"),
            actionButton("phenotype0", "Phenotype 0", 
              style = "width: 100%; margin: 0; background-color: #B0B0B0; color: white;"),
            actionButton("phenotype1", "Phenotype 1", 
              style = "width: 100%; margin: 0; background-color: #92A8D1; color: white;"),
            actionButton("phenotype2", "Phenotype 2", 
              style = "width: 100%; margin: 0; background-color: #74777C; color: white;"),
            actionButton("phenotype3", "Phenotype 3", 
              style = "width: 100%; margin: 0; background-color: #D2C19E; color: white;"),
            actionButton("phenotype4", "Phenotype 4", 
              style = "width: 100%; margin: 0; background-color: #FF6F61; color: white;")
          )
        ),
        mainPanel(
          HTML("<p style='font-weight: bold;'>Trajectory Plot</p>"),
          textOutput("The following is a reference trajectory diagram for each category"),
          div(style = "text-align: center;",
            plotOutput("dynamic_plot", width = "600px", height = "400px") %>%
              tagAppendAttributes(style = "margin: 0 auto;")
          ),
          HTML("<br><br>"),
          HTML("<p style='font-weight: bold;'>Forest Plot</p>"),
          div(style = "text-align: center;",
            img(src = "hx_4_from_7_app.png", width = "600px", alt = "Image cannot be loaded")
          ),
          HTML("<br><br>"),
          HTML("<p style='font-weight: bold;'>Feature Importance for All Phenotypes</p>"),
          div(style = "text-align: center;",
            img(src = "fea_imp_all.png", width = "800px", alt = "Image cannot be loaded")
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

    # 在server函数中添加观察者来监控输入值并调整
    observe({
      # 对年龄输入进行调整
      if (!is.null(input$age) && !is.na(input$age)) {
        if (input$age < 0) {
          updateNumericInput(session, "age", value = 0)
        } else if (input$age > 150) {
          updateNumericInput(session, "age", value = 150)
        }
      }
      
      # 对BMI输入进行调整
      if (!is.null(input$bmi) && !is.na(input$bmi)) {
        if (input$bmi < 10) {
          updateNumericInput(session, "bmi", value = 10)
        } else if (input$bmi > 32) {
          updateNumericInput(session, "bmi", value = 32)
        }
      }
      
      # 对ASA分类输入进行调整
      if (!is.null(input$asa) && !is.na(input$asa)) {
        if (input$asa < 1) {
          updateNumericInput(session, "asa", value = 1)
        } else if (input$asa > 6) {
          updateNumericInput(session, "asa", value = 6)
        }
      }

      # 对Propofol剂量进行调整
      if (!is.null(input$ppf) && !is.na(input$ppf)) {
        if (input$ppf < 0) {
          updateNumericInput(session, "ppf", value = 0)
        } else if (input$ppf > 200) {
          updateNumericInput(session, "ppf", value = 200)
        }
      }
      
      # 对Sufentanil剂量进行调整
      if (!is.null(input$sft) && !is.na(input$sft)) {
        if (input$sft < 0) {
          updateNumericInput(session, "sft", value = 0)
        } else if (input$sft > 35) {
          updateNumericInput(session, "sft", value = 35)
        }
      }
      
      # 对Midazolam剂量进行调整
      if (!is.null(input$mdz) && !is.na(input$mdz)) {
        if (input$mdz < 0) {
          updateNumericInput(session, "mdz", value = 0)
        } else if (input$mdz > 3) {
          updateNumericInput(session, "mdz", value = 3)
        }
      }
      
      # 对Propofol输注浓度进行调整
      if (!is.null(input$ppfi) && !is.na(input$ppfi)) {
        if (input$ppfi < 0) {
          updateNumericInput(session, "ppfi", value = 0)
        } else if (input$ppfi > 3) {
          updateNumericInput(session, "ppfi", value = 3)
        }
      }
      
      # 对Remifentanil输注速率进行调整
      if (!is.null(input$rfti) && !is.na(input$rfti)) {
        if (input$rfti < 0) {
          updateNumericInput(session, "rfti", value = 0)
        } else if (input$rfti > 5) {
          updateNumericInput(session, "rfti", value = 5)
        }
      }
      
      # 对Desflurane浓度进行调整
      if (!is.null(input$etdes) && !is.na(input$etdes)) {
        if (input$etdes < 0) {
          updateNumericInput(session, "etdes", value = 0)
        } else if (input$etdes > 3.5) {
          updateNumericInput(session, "etdes", value = 3.5)
        }
      }
      
      # 对Sevoflurane浓度进行调整
      if (!is.null(input$etsevo) && !is.na(input$etsevo)) {
        if (input$etsevo < 0) {
          updateNumericInput(session, "etsevo", value = 0)
        } else if (input$etsevo > 1.5) {
          updateNumericInput(session, "etsevo", value = 1.5)
        }
    }
  })

  observeEvent(input$submit_new, {
    
    # print(input$medical_history)
    # 收集输入数据
    new_data <- data.frame(
      age = input$age,
      sex = ifelse(is.na(input$sex), 0, ifelse(input$sex == "Male", 1, 0)),
      bmi = input$bmi,
      asa = input$asa,
      emop = ifelse(input$emop == "YES", 1, 0),  # 修改 Emergent Operation 的处理逻辑，以匹配下拉选择框
      surgery_siteAbdomen = ifelse(is.na(input$surgery_site), 0, ifelse(input$surgery_site == "surgery_siteAbdomen", 1, 0)),
      surgery_siteBones_and_Joints = ifelse(is.na(input$surgery_site), 0, ifelse(input$surgery_site == "surgery_siteBones_and_Joints", 1, 0)),
      surgery_siteCentral_Nervous_System_and_Cranial_Nerves = ifelse(is.na(input$surgery_site), 0, ifelse(input$surgery_site == "surgery_siteCentral_Nervous_System_and_Cranial_Nerves", 1, 0)),
      surgery_siteHead_and_Neck = ifelse(is.na(input$surgery_site), 0, ifelse(input$surgery_site == "surgery_siteHead_and_Neck", 1, 0)),
      surgery_siteHeart_and_Great_Vessels = ifelse(is.na(input$surgery_site), 0, ifelse(input$surgery_site == "surgery_siteHeart_and_Great_Vessels", 1, 0)),
      surgery_siteOther = ifelse(is.na(input$surgery_site), 0, ifelse(input$surgery_site == "surgery_siteOther", 1, 0)),
      surgery_siteRespiratory_System = ifelse(is.na(input$surgery_site), 0, ifelse(input$surgery_site == "surgery_siteRespiratory_System", 1, 0)),
      surgery_siteSkin_and_Soft_Tissue = ifelse(is.na(input$surgery_site), 0, ifelse(input$surgery_site == "surgery_siteSkin_and_Soft_Tissue", 1, 0)),
      surgery_siteUrinary_and_Reproductive_Systems = ifelse(is.na(input$surgery_site), 0, ifelse(input$surgery_site == "surgery_siteUrinary_and_Reproductive_Systems", 1, 0)),
      have_vaso = ifelse(input$have_vaso == "YES", 1, 0),  # 修改 Has Vasoactive Drug 的处理逻辑，以匹配下拉选择框
      mdz = input$mdz,
      ppf = input$ppf,
      ppfi = input$ppfi,
      rfti = input$rfti,
      sft = input$sft,
      etdes = input$etdes,
      etsevo = input$etsevo,
      bp_control = ifelse(input$bp_control %in% c("Not Needed", "Well Controlled"), 1, 0),
      ACEI = ifelse("ACEI" %in% input$medication, 1, 0),
      ARB = ifelse("ARB" %in% input$medication, 1, 0),
      LXP = ifelse("LXP" %in% input$medication, 1, 0),
      hypo_med_other = ifelse("hypo_med_other" %in% input$medication, 1, 0),
      Essential_primary_hypertension = ifelse("Essential_primary_hypertension" %in% input$medical_history, 1, 0),
      Coronary_heart_disease = ifelse("Coronary_heart_disease" %in% input$medical_history, 1, 0),
      Congestive_heart_failure = ifelse("Congestive_heart_failure" %in% input$medical_history, 1, 0),
      Atrial_fibrillation_and_flutter = ifelse("Atrial_fibrillation_and_flutter" %in% input$medical_history, 1, 0),
      Abnormalities_of_heart_beat = ifelse("Abnormalities_of_heart_beat" %in% input$medical_history, 1, 0),
      Diabetes_mellitus = ifelse("Diabetes_mellitus" %in% input$medical_history, 1, 0),
      Cerebral_infarction = ifelse("Cerebral_infarction" %in% input$medical_history, 1, 0),
      Transient_cerebral_ischemic_attacks_and_related_syndromes = ifelse("Transient_cerebral_ischemic_attacks_and_related_syndromes" %in% input$medical_history, 1, 0),
      Emphysema_or_chronic_obstructive_pulmonary_disease = ifelse("Emphysema_or_chronic_obstructive_pulmonary_disease" %in% input$medical_history, 1, 0),
      Asthma = ifelse("Asthma" %in% input$medical_history, 1, 0),
      Abnormalities_of_breathing = ifelse("Abnormalities_of_breathing" %in% input$medical_history, 1, 0),
      Chronic_kidney_disease_CKD = ifelse("Chronic_kidney_disease_CKD" %in% input$medical_history, 1, 0),
      Chronic_viral_hepatitis = ifelse("Chronic_viral_hepatitis" %in% input$medical_history, 1, 0),
      Liver_disease = ifelse("Liver_disease" %in% input$medical_history, 1, 0),
      Gastroesophageal_reflux_disease = ifelse("Gastroesophageal_reflux_disease" %in% input$medical_history, 1, 0),
      Anemia = ifelse("Anemia" %in% input$medical_history, 1, 0),
      Disorder_of_thyroid = ifelse("Disorder_of_thyroid" %in% input$medical_history, 1, 0),
      Pre_CRRT = ifelse("Pre_CRRT" %in% input$medical_history, 1, 0)
    )
    
    for(i in 1:ncol(new_data)){
      new_data[[i]] <- as.numeric(new_data[[i]])
      new_data[[i]][is.na(new_data[[i]])] <- 0
    }
    
    # 将数据框转换为矩阵
    new_sample <- as.matrix(new_data)
    pred <- predict(xgboost_model, new_sample)
    fea_imp <- predict(xgboost_model, new_sample, predcontrib = TRUE)
    k <- which.max(pred)
    fea_imp_k <- fea_imp[[k]]
    fea_imp_k <- fea_imp_k[-length(fea_imp_k)]
    # print(new_sample[1,,drop=T])
    fea_imp_k[which(new_sample[1,,drop=T] == 0)] <- 0
    
    fea_imp_k <- abs(fea_imp_k)
    fea_imp_k <- fea_imp_k[order(fea_imp_k, decreasing = T)]
    names(fea_imp_k) <- column_names[names(fea_imp_k)]  # 使用 column_names 替换 fea_imp_k 的索引名称

    output$fea_imp_plot <- renderPlot({
      # 获取前10个最重要的特征
      top_features <- names(fea_imp_k[1:10])
      top_importances <- fea_imp_k[1:10]

      # 如果特征名称中有空格位置大于15，则在第一个这样的空格处添加换行符
      top_features <- sapply(top_features, function(x) {
        # 找到所有空格的位置
        space_positions <- which(strsplit(x, "")[[1]] == " ")
        # 找到第一个大于15的空格位置
        first_space_after_15 <- space_positions[which(space_positions > 15)[1]]
        
        if (!is.na(first_space_after_15)) {
          # 在找到的空格位置插入换行符
          paste0(
            substr(x, 1, first_space_after_15 - 1),
            "\n",
            substr(x, first_space_after_15 + 1, nchar(x))
          )
        } else {
          x
        }
      })

      cols <- c("#B0B0B0", "#92A8D1", "#74777C", "#D2C19E", "#FF6F61")
      col <- cols[k]

      # 设置图形的边界参数
      par(mar=c(5, 15, 4, 2) + 0.1)
      # 使最重要的特征显示在最上方
      barplot(rev(top_importances), names.arg = rev(top_features), col = col, las = 1,
              main = NULL,
              xlab = "Importance (Absolute value)", ylab = "", horiz = TRUE, cex.names = 1.2,
              cex.axis = 1.2,  # 放大 xtick 和 ytick 的字体大小
              cex.lab = 1.2)
      grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
      box()
    })

    output$pred_plot <- renderPlot({
      par(mar=c(5, 15, 4, 2) + 0.1, mgp=c(4, 1, 0))
      # pred 是一个长度为5的向量，包含各类的预测概率
      cols <- c("#B0B0B0", "#92A8D1", "#74777C", "#D2C19E", "#FF6F61")  # 颜色向量
      barplot(pred, names.arg = 0:4, col = cols, las = 1,
              main = NULL,
              xlab = "Probability", ylab = "Class", horiz = TRUE, 
              cex.names = 1.2, xlim=c(0, 1),
              cex.axis = 1.2,  # 放大 xtick 和 ytick 的字体大小
              cex.lab = 1.2)   # 放大 xlab 和 ylab 的字体大小
      grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
      box()
    })

  }, ignoreInit = FALSE)

  observeEvent(input$submit, {
    mbp_values <- sapply(0:12, function(i) input[[paste0("mbp_input_", i*5)]])

    # 检查是否所有输入都是 NA
    if (all(is.na(mbp_values))) {
      output$classification <- renderText("All inputs are missing. Please provide at least one measurement.")
      output$plot_mbp <- renderPlot({})  # 清除图像输出
      return()
    }

    # 找到第一个NA的位置
    first_na <- which(is.na(mbp_values))[1]
    # 检查第一个NA之后是否还有非NA值

    if (!is.na(first_na) && any(!is.na(mbp_values[first_na:length(mbp_values)]))) {
      output$classification <- renderText("Missing inputs are only allowed at the end. Please check your input.")
      output$plot_mbp <- renderPlot({})  # 清除图像输出
      return()
    }

    # 移除末尾的NA值
    mbp_values <- mbp_values[!is.na(mbp_values)]

    data_input <- data.table(time = seq(0, length(mbp_values) - 1) * 5 / 60, mbp = mbp_values, op_id = rep(1, length(mbp_values)))
    
    classification_result <- predictClass(m7, data_input[!is.na(mbp_values), ])
    
    output$plot_mbp <- renderPlot({
        par(mar=c(4,15,1,1) + 0.1)
        # 将所有曲线颜色设置为黑色
        plot(data_input$time * 60, data_input$mbp, type = "b", pch = 19, lty = 1, col = "black", cex = 1,
            xlim = c(0, 60), ylim = c(50, 110), 
            xlab = "Time after Anesthesia Induction (minute)", ylab = "Mean Blood Pressure (mmHg)", 
            xaxt = "n", yaxt = "n", cex.lab = 1.2)
        ref_class <- ifelse(all(mbp_values >= 65), 0, recode_map[as.character(classification_result$class[1])])
        # 在绘图代码块中，设置颜色数组
        cols <- c("#B0B0B0", "#92A8D1", "#74777C", "#D2C19E", "#FF6F61")

        # 根据 ref_class 选择颜色
        selected_col <- cols[ref_class + 1]  # 加1因为R的索引从1开始
        # 绘制置信区间多边形，使用选择的颜色并添加透明度
        with(ds_ref[ds_ref$class == ref_class & ds_ref$time <= 60, ], {
            polygon(c(time, rev(time)), c(lower, rev(upper)), col = adjustcolor(selected_col, alpha.f = 0.3), border = NA)
        })

        # 绘制参考线，使用选择的颜色
        lines(ds_ref$time[ds_ref$class == ref_class & ds_ref$time <= 60], ds_ref$mean[ds_ref$class == ref_class & ds_ref$time <= 60], type = "b", lty = 2, col = selected_col, pch=19, cex=1)
        axis(1, at = seq(0, 60, 5), labels = seq(0, 60, 5), cex.axis = 1.2)
        axis(2, at = seq(50, 110, 10), labels = seq(50, 110, 10), cex.axis = 1.2)
        abline(h=65, lty=2, col="red")
        grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
        # 更新图例，使用选择的颜色
        legend("topright", legend=c("Patient MBP Trajectory", paste0("PIH-HI classification (Phenotype = ", as.character(ref_class), ")")), 
              col=c("black", selected_col), lty=c(1, 2), pch=c(19, 19), cex=1.2, bty="n")
        # legend("topright", legend=c("Patient MBP Trajectory"), col=c("black"), lty=c(1), pch=c(19), cex=1, bty="n")
        # legend("bottomright", legend=c(paste0("PIH-HI classification (Phenotype = ", as.character(ref_class), ")")), 
        #       fill=adjustcolor(selected_col, alpha.f = 0.3), bty="n", border=NA)
    })

    output$ds_OR_table <- renderTable({
      ref_class <- ifelse(all(mbp_values >= 65), 0, recode_map[as.character(classification_result$class[1])])
      names(ds_OR)[1] <- paste0("Event (Phenotype = ", as.character(ref_class), ")")
      ds_OR[ds_OR$Phenotype %in% c(as.character(ref_class)), c(1,3,4), drop = FALSE]
    })

    output$ref_class_image <- renderImage({
        ref_class <- ifelse(all(mbp_values >= 65), 0, recode_map[as.character(classification_result$class[1])])
        if (ref_class %in% c(1, 2, 3, 4)) {
            list(src = paste0("./www/fea_imp_", ref_class, ".png"),
                contentType = 'image/png',
                width = '600px',
                alt = "Image cannot be loaded",
                deleteFile = FALSE)  # 明确指定 deleteFile 参数
        } else {
            list(src = "", alt = "", deleteFile = FALSE)  # 明确指定 deleteFile 参数
        }
    }, deleteFile = FALSE)  # 明确指定 deleteFile 参数
  }, ignoreInit = FALSE)

  observeEvent(input$clear, {
    # 重置所有输入
    lapply(0:12, function(i) {
      updateNumericInput(session, paste0("mbp_input_", i*5), value = NA)
    })
    # 清除输出
    output$classification <- renderText({""})
    output$plot_mbp <- renderPlot({})  # 清除图像输出
    output$ds_OR_table <- renderTable({data.frame()})  # 清除表格输出
    output$dynamic_plot <- renderPlot({})  # 清除动态图像输出
    output$ref_class_image <- renderImage({list(src = "", alt = "",deleteFile = FALSE)}, deleteFile = FALSE)  # 清除图像输出
  })

  observeEvent(input$clear_new, {
    # 重置所有输入
    updateNumericInput(session, "age", value = 50)
    updateSelectInput(session, "sex", selected = "Male")
    updateNumericInput(session, "bmi", value = 23)
    updateNumericInput(session, "asa", value = 3)
    updateSelectInput(session, "emop", selected = "NO")
    updateSelectInput(session, "surgery_site", selected = "surgery_siteAbdomen")
    updateSelectInput(session, "have_vaso", selected = "NO")  
    updateNumericInput(session, "mdz", value = 0)
    updateNumericInput(session, "ppf", value = 0)
    updateNumericInput(session, "ppfi", value = 0)
    updateNumericInput(session, "rfti", value = 0)
    updateNumericInput(session, "sft", value = 0)
    updateNumericInput(session, "etdes", value = 0)
    updateNumericInput(session, "etsevo", value = 0)
    updateSelectInput(session, "bp_control", selected = "Not Needed")
    updateCheckboxInput(session, "ACEI", value = FALSE)
    updateCheckboxInput(session, "ARB", value = FALSE)
    updateCheckboxInput(session, "LXP", value = FALSE)
    updateCheckboxInput(session, "hypo_med_other", value = FALSE)
    updateCheckboxGroupInput(session, "medical_history", selected = NULL)
    output$fea_imp_plot <- renderPlot({})
    output$pred_plot <- renderPlot({})
  })

    lapply(0:4, function(i) {
      observeEvent(input[[paste0("phenotype", i)]], {
        output$dynamic_plot <- renderPlot({
          ref_class <- i
          par(mar=c(4,5,1,1) + 0.1)
          plot(NULL, xlim = c(0, 60), ylim = c(50, 110),
              xlab = "Time after Anesthesia Induction (minute)", 
              ylab = "Mean Blood Pressure (mmHg)", 
              xaxt = "n", yaxt = "n", cex.lab = 1.2)
          
          colors <- rep("gray", length(unique(aggregate_data$class)))
          uniq_class <- sort(unique(aggregate_data$class))
          colors[uniq_class == ref_class] <- "red"
          legend_text <- paste("Phenotype ", uniq_class, sep="")
          point_shapes <- seq(1, length(unique(aggregate_data$class))) + 11

          for (i in 1:length(uniq_class)) {
            subset_data <- aggregate_data[aggregate_data$class == uniq_class[i], ]
            lines(subset_data$time, subset_data$mean, 
                  col = colors[i], type = "b", 
                  pch = point_shapes[i], lty = 1, cex = 1.2)
            segments(subset_data$time, subset_data$lower, 
                    subset_data$time, subset_data$upper, 
                    col = colors[i])
            segments(subset_data$time - 0.2, subset_data$lower, 
                    subset_data$time + 0.2, subset_data$lower, 
                    col = colors[i])
            segments(subset_data$time - 0.2, subset_data$upper, 
                    subset_data$time + 0.2, subset_data$upper, 
                    col = colors[i])
          }
          
          abline(h=65, lty=2, col="red")
          grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
          axis(1, at = seq(0, 60, 5), labels = seq(0, 60, 5), cex.axis = 1.2)
          axis(2, at = seq(50, 110, 10), labels = seq(50, 110, 10), cex.axis = 1.2)
          legend("topright", legend = legend_text, 
                col = colors, lty = 1, pch = point_shapes, 
                cex = 1.2, bty='n', ncol = 1)
        })
      })
    })

  observeEvent(input$clear_ref, {
    # 重置选择的类别到默认值
    updateSelectInput(session, "class_select", selected = 0)
    # 可以选择清除图表或其他输出
    output$dynamic_plot <- renderPlot({})
  })

  # 在应用初始化后模拟点击这三个按钮
  observe({
    shinyjs::click("submit")
    shinyjs::click("submit_new")
    shinyjs::click("phenotype0")
  })

}

shinyApp(ui = ui, server = server)