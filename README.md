# Lipid Profile Changes in Chronic Hepatitis B Patients: Comparing Tenofovir Alafenamide and Tenofovir Disoproxil Fumarate in a Real-World Setting

This repository contains the code used for the study titled **"Lipid Profile Changes in Chronic Hepatitis B Patients: Comparing Tenofovir Alafenamide and Tenofovir Disoproxil Fumarate in a Real-World Setting"**. The study investigates the lipid profile changes in chronic hepatitis B (CHB) patients treated with either Tenofovir Alafenamide (TAF) or Tenofovir Disoproxil Fumarate (TDF) in a real-world clinical setting. The analysis involves organizing and processing raw data, extracting patient information, and performing statistical analyses to compare the effects of TAF and TDF on serum lipid levels.

## Code Files

- **0_Lab.R**  
  This script organizes the laboratory test data of patients, focusing on lipid profiles and other relevant lab results.

- **1_TDF_TAF.R**  
  This script analyzes the raw data from the electronic medical records (EMR) of patients treated with either TAF or TDF.

- **2_Combination_data_analysis.R**  
  This script combines the organized lab data and the EMR data from previous steps to create a unified dataset for further analysis.

- **3_Duration.R**  
  This script extracts and organizes the time-series data of patient treatments, tracking the duration of TAF and TDF usage.

- **4_Extraction_update_231202.R**  
  This script identifies patients with sufficient data for analysis, ensuring that all included patients have the necessary lipid profile information.

- **Analysis_240115.R**  
  This is the main analysis script, which performs statistical comparisons of lipid profile changes between TAF and TDF groups.

- **flowchart.R**  
  This script generates a flowchart to visually represent the study design and patient selection process.

- **twoCom.R**  
  This script contains the function for performing paired T-tests, used in comparing lipid profile changes within and between treatment groups.

## Usage

Each script is designed to be run sequentially, starting with data organization and ending with the main analysis. Detailed instructions on how to use the scripts and their dependencies can be found in the comments within each file.

## Citation

If you use any part of this code or data for your own research, please cite the study as follows:  

