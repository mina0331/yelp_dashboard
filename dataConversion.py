from pathlib import Path
import pandas as pd

input_folder = Path("./yelp_dataset")
output_folder = Path("./yelp_dataset_csv")

output_folder.mkdir(exist_ok=True)

for json_file in input_folder.glob("*.json"):
    #finding all files ending in .json
    # Load JSON
    df = pd.read_json(json_file, lines=True)

    # Create output file path
    csv_file = output_folder / (json_file.stem + ".csv")
    #stem basically gets the file name without the extension

    # Save as CSV
    df.to_csv(csv_file, index=False)

    print(f"Converted {json_file} -> {csv_file}")