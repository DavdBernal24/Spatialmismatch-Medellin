
**Better or Worse Job Accessibility Understanding Changes in Spatial Mismatch – Evidence from Medellín, Colombia**

**Authors:** David Bernal; Jorge Perez Perez; Gustavo Garcia

---

## 📄 Overview

This repository contains all the code used to replicate the results of the paper:  
**“Better or Worse Job Accessibility: Understanding Changes in Spatial Mismatch – Evidence from Medellín, Colombia.”**

All code (except for the final two scripts) is written in **R**.

---

## 📁 Project Structure


- Scripts in `Base/` and `Output/` are **fully replicable** using the code.
- R scripts are stored outside these folders to enable consistent working directory management.

---

## ▶️ How to Run the Code

1. **Start with `Master.R`**
   - This is the main script that runs all other scripts in sequence (**52 total**).
   - Scripts **must** be run in order.

2. **Function Scripts**
   - Some scripts are utility functions (e.g., `Maps_Function.R`).
   - Ensure they are in the same folder as `Master.R`—they are called from other scripts.

3. **Working Directory Setup**
   - Scripts begin with:
     ```r
     setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
     ```
   - This ensures the script uses its own path as the working directory—no need to manually set it.

---

## ⚠️ Important Notes

- ❌ **Do not run Scripts 2 and 3**  
  These download live data from Google and Bing APIs. You need API keys, and results will differ from the paper due to time sensitivity.

- 📦 **Deprecated R packages**
  - We use `rgdal`, which has been removed from CRAN.
  - Download it from the archive:  
    https://cran.r-project.org/src/contrib/Archive/rgdal/
  - You must have **Rtools** installed to build it.

- 🧰 **R Version**
  - All scripts were tested in **R 4.0.3 (64-bit)**
  - Ensure package compatibility with this version.

- 📊 **Stata Scripts**
  - The final two scripts are written in **Stata** (version 16+ required).
  - Follow the instructions in `Master.R` to run them correctly.

---

## 📬 Contact

If you have questions or issues replicating the code, feel free to reach out:

- vdbernal@cougarnet.uh.edu  
- davidbernal224@gmail.com


