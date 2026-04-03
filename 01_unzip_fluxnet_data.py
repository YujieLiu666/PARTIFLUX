# -*- coding: utf-8 -*-
"""
Created on Thu Apr  2 17:20:15 2026

@author: yl763
"""

import pandas as pd
from pathlib import Path
from zipfile import ZipFile
import shutil

# -----------------------------
# Purpose:
# This script extracts high-frequency (HH or HR) CSV files from FLUXNET ZIP archives.
# Only files for a selected list of site IDs (subset_list.csv) are processed.
# Files containing "ERA5" or "BIFVARINFO" inside the ZIP are skipped.
# Extracted CSVs are copied to a flat output directory for analysis.
# -----------------------------

# Paths
site_file = Path(r"F:\23 PARTIFLUX\data_FLUXNET2025\subset_list.csv")  # CSV with desired site IDs
zip_dir = Path(r"F:\23 PARTIFLUX\fluxnet_bulk\fluxnet_downloads")       # Folder with FLUXNET ZIP files
output_dir = Path(r"F:\23 PARTIFLUX\HH_FLUXNET2025")                   # Destination for extracted CSVs
output_dir.mkdir(parents=True, exist_ok=True)                           # Create output folder if it doesn't exist

# Read site IDs from CSV
# Assumes one column with no header
site_ids = pd.read_csv(site_file, header=None)[0].tolist()
print(f"Loaded {len(site_ids)} site IDs")

# Loop over all ZIP files in the downloads directory
for zip_path in zip_dir.glob("*.zip"):
    fname = zip_path.name
    
    # Check if any of the site IDs match part of the ZIP filename
    if any(site_id in fname for site_id in site_ids):
        print(f"Processing {fname} ...")
        
        # Open the ZIP file
        with ZipFile(zip_path, 'r') as zip_ref:
            
            # Loop over each file inside the ZIP
            for file in zip_ref.namelist():
                
                # Skip files containing unwanted metadata
                if "ERA5" in file or "BIFVARINFO" in file:
                    continue
                
                # Only extract CSV files with HH (Half-Hourly) or HR (Hourly) data
                if file.endswith(".csv") and ("HH" in file or "HR" in file):
                    
                    # Extract the file to the output directory (keeps original folder structure temporarily)
                    zip_ref.extract(file, output_dir)
                    
                    # Flatten the folder structure: move file to root of output_dir
                    src = output_dir / file
                    dst = output_dir / Path(file).name
                    if src != dst:
                        shutil.move(src, dst)
        
        print(f"Finished {fname}")