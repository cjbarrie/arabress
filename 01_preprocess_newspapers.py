# SCRIPT TO PREPROCESS NEWSPAPER SAMPLES
import pandas as pd
import re
import pyreadr
import os

def clean_text(text):
    ## Remove punctuation
    text = re.sub('[%s]' % re.escape("""!"#$%&'()*+,،-./:;<=>؟?@[\]^_`{|}~"""), ' ', str(text))  # remove punctuation
    ## Remove numbers
    text = re.sub("\d+", " ", str(text))
    ## Remove Latin
    text = re.sub("[a-zA-Z]", " ", str(text))
    ## remove extra whitespace
    text = re.sub('\s+', ' ', str(text))
    return text

# Define the path where the files are stored
destination_combined_path = "data/output/combined/"

# Define the list of countries
countries = ["djazairess", "maghress", "masress", "sauress", "turess"]

# Loop through all countries
for country in countries:
    print(f"Processing {country} ...")
    
    # Define the input path for the current country
    input_path = os.path.join(destination_combined_path, country, f"{country}_combined.parquet")
    
    # Read the .rds file for the current country
    df = pd.read_parquet(input_path)
    
    # Preprocess the content
    df = df.dropna(subset=['content', 'date'])
    df = df.reset_index(drop=True)
    df['content'] = df['content'].apply(clean_text)
    
    # Define the output path for the current country
    output_path = os.path.join(destination_combined_path, country, f"{country}_combined_prepro.rds")
    
    # Save the preprocessed data as an .rds file
    pyreadr.write_rds(output_path, df, compress="gzip")
    
    print(f"Saved {output_path}")

print("All countries processed.")
