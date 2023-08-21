import os
import json
import pandas as pd
from tqdm import tqdm
import random

destination_combined_path = "data/output/combined/"
target_size = 1.76e6  # total size of of the smallest corpus (Tunisia)
titles = []  # list of excluded newspapers, update as needed
random.seed(42)  # Set the seed value for reproducibility

# Loop through countries
# Filter to only include directories, ignoring files like .DS_Store
countries = [os.path.join("data/raw", d, "ar") for d in os.listdir("data/raw") if os.path.isdir(os.path.join("data/raw", d))]
for country_path in tqdm(countries, desc="Processing countries"):
    filepaths = [os.path.join(country_path, f) for f in os.listdir(country_path) if f.endswith('.json')]
    country = os.path.basename(os.path.dirname(country_path))
    print(f"Processing country: {country}")

    total_articles = 0
    # Get the total number of articles
    for filepath in filepaths:
        with open(filepath, 'r') as file:
            total_articles += len(file.readlines())

    # Compute the sampling fraction for this country
    sampling_fraction = min(target_size / total_articles, 1)
    print(f"Sampling fraction for {country} is {sampling_fraction}")

    sampled_data = []  # to hold the final sample
    for filepath in tqdm(filepaths, desc=f"Processing JSON files for {country}"):
        source = os.path.basename(filepath).replace(".json", "")

        # Skip if the source is in the excluded_newspapers list
        if source in titles:
            continue

        with open(filepath, 'r') as file:
            lines = file.readlines()
            sampled_lines = random.sample(lines, int(len(lines) * sampling_fraction))
            for line in sampled_lines:
                record = json.loads(line)
                sampled_data.append({
                    "content": record.get("content", ""),  # Default to an empty string if key is missing
                    "author": record.get("author", ""),
                    "date": record.get("date", ""),
                    "new_category": record.get("new_category", ""),
                    "newspaper": record.get("newspaper", "")
                })

    # Create the combined directory for this country if it doesn't exist
    combined_path = os.path.join(destination_combined_path, country)
    os.makedirs(combined_path, exist_ok=True)

    # Write the combined file for this country
    sampled_df = pd.DataFrame(sampled_data)
    sampled_df.to_parquet(os.path.join(combined_path, f"{country}_combined.parquet"))
