# full_sync 
# Compiled by MH on 08/23/25
# for long, stationary recordings and/or when annotations must be made in raw format (i.e., looming stims & bites)
# to synchronize left/right first-video pairs and extract the sam2_sf for all sequential videos
# NOTE: ensure that you have already moved the calibration video to a different folder, ideally called raw_first

import csv
from pathlib import Path
from tqdm import tqdm  # Progress bar
import os
import shutil
from sync_stat_functions import *






####### USER EDITS START HERE ########
# Base_bath: This should be to the date-folder where the folders of videos are stored
base_path = "/Volumes/eel_2/garden_eel_diel/29_05_2025" 

# Adding site_id, which should be the Left and right video folder names. 
siteID_L= "garden_eel_diel_29_05_25_F2-cam1-B9"
siteID_R= "garden_eel_diel_29_05_25_F2-cam2-B9"

# First video names, after the calibration videos have been removed 
first_left_vid = "GH079670.MP4"
first_right_vid = "GH079788.MP4"


# The fps of extracted frames that will be run through SAM2, usually 3
out_fps = 1 

####### USER EDITS END HERE #########


# Construct paths for left/right videos
left_vids_folder = os.path.join(base_path, siteID_L) 
right_vids_folder = os.path.join(base_path, siteID_R)

# Construct paths for first videos
first_left_path = os.path.join(left_vids_folder, first_left_vid)
first_right_path = os.path.join(right_vids_folder, first_right_vid)

# Get the frame rate of the videos
fps = get_frame_rate(first_right_path)
print("FPS of right camera video:", fps)

# Extract audio features and calculate frame delay
frame_delay = extract_audio_features(first_left_path, first_right_path, fps)

# Construct output paths
output_left_path = os.path.join(left_vids_folder, first_left_vid.replace(".MP4", "_synced.MP4"))
output_right_path = os.path.join(right_vids_folder, first_right_vid.replace(".MP4", "_synced.MP4"))

# Sync videos
trim_videos1(first_left_path, first_right_path, output_left_path, output_right_path, frame_delay, fps)

print("Videos synchronized")


# Move raw first videos to separate directory called raw_first
raw_dir_L = os.path.join(left_vids_folder, "raw_first")
raw_dir_R = os.path.join(right_vids_folder, "raw_first")

try:
    # Create the directory and any necessary parent directories
    # exist_ok=True prevents an error if the directory already exists
    os.makedirs(raw_dir_L, exist_ok=True)
    os.makedirs(raw_dir_R, exist_ok=True)
except OSError as e:
    # Handle potential errors during directory creation (e.g., permissions)
    print(f"Error creating raw directories: {e}")
    
shutil.move(first_right_path, raw_dir_R)
print(f"Raw first right video moved to '{raw_dir_R}'.")
shutil.move(first_left_path, raw_dir_L)
print(f"Raw first left video moved to '{raw_dir_L}'.")



#### Run Left video info extraction 

# Specify file paths
icl = siteID_L + "_video_info.csv"
icr = siteID_R + "_video_info.csv"
info_csv_L = os.path.join(left_vids_folder, icl)
info_csv_R = os.path.join(right_vids_folder, icr)
out_csv_L = siteID_L + "_SAM2Frames.csv"
out_csv_R = siteID_R + "_SAM2Frames.csv"
output_path_L = os.path.join(left_vids_folder, out_csv_L)
output_path_R = os.path.join(right_vids_folder, out_csv_R)

# Remove old info file if it exists
if os.path.exists(info_csv_L):
    os.remove(info_csv_L)

 # Remove old info file if it exists
if os.path.exists(info_csv_R):
    os.remove(info_csv_R)

# Find .mp4 files
mp4_files_L = [f for f in Path(left_vids_folder).iterdir() if f.is_file() and f.suffix.lower() == ".mp4"]
if not mp4_files_L:
    print(f"No .mp4 files found in {left_vids_folder}.")
    exit(1)

mp4_files_R = [f for f in Path(right_vids_folder).iterdir() if f.is_file() and f.suffix.lower() == ".mp4"]
if not mp4_files_R:
    print(f"No .mp4 files found in {right_vids_folder}.")
    exit(1)

# Write header to the CSV file
header = ["File Name", "File Path", "Frame Rate", "Total Frames", "Start Time", "Duration", "End Time"]


# Process left files
with open(info_csv_L, mode="w", newline="") as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(header)
    for video_file in tqdm(mp4_files_L, desc="Processing videos", unit="file"):
        file_name = video_file.name
        file_path = str(video_file.resolve())
        frame_rate_fraction = get_ffprobe_data(video_file, "stream=r_frame_rate")
        total_frames = get_ffprobe_data(video_file, "stream=nb_frames")
        start_time = get_ffprobe_data(video_file, "format=start_time")
        duration = get_ffprobe_data(video_file, "format=duration")
        frame_rate = "N/A"
        end_time = "N/A"

        invalid_vals = ("N/A", "", None, "nan", "NaN")


        if start_time in invalid_vals or duration in invalid_vals:
           print(f"Skipping file (missing metadata): {file_name}")
           writer.writerow([file_name, file_path, frame_rate, total_frames, start_time, duration, "N/A"])
           continue

        if frame_rate_fraction and "/" in frame_rate_fraction:
            numerator, denominator = map(int, frame_rate_fraction.split("/"))
            frame_rate = round(numerator / denominator, 3)
        else:
            frame_rate = frame_rate_fraction
        if start_time and duration:
            end_time = float(start_time) + float(duration)
        else:
            end_time = duration
        writer.writerow([file_name, file_path, frame_rate, total_frames, start_time, duration, end_time])

# print(f"Left camera midpoint information extraction complete. Data saved to {info_csv_L}.")


# Process right files
with open(info_csv_R, mode="w", newline="") as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(header)
    for video_file in tqdm(mp4_files_R, desc="Processing videos", unit="file"):
        file_name = video_file.name
        file_path = str(video_file.resolve())
        frame_rate_fraction = get_ffprobe_data(video_file, "stream=r_frame_rate")
        total_frames = get_ffprobe_data(video_file, "stream=nb_frames")
        start_time = get_ffprobe_data(video_file, "format=start_time")
        duration = get_ffprobe_data(video_file, "format=duration")
        frame_rate = "N/A"
        end_time = "N/A"

        invalid_vals = ("N/A", "", None, "nan", "NaN")
        if start_time in invalid_vals or duration in invalid_vals:
           print(f"Skipping file (missing metadata): {file_name}")
           writer.writerow([file_name, file_path, frame_rate, total_frames, start_time, duration, "N/A"])
           continue

        if frame_rate_fraction and "/" in frame_rate_fraction:
            numerator, denominator = map(int, frame_rate_fraction.split("/"))
            frame_rate = round(numerator / denominator, 3)
        else:
            frame_rate = frame_rate_fraction
        if start_time and duration:
            end_time = float(start_time) + float(duration)
        else:
            end_time = duration
        writer.writerow([file_name, file_path, frame_rate, total_frames, start_time, duration, end_time])

print(f"Right camera midpoint information extraction complete. Data saved to {info_csv_R}.")

# Extract SAM2 start frames for all videos within left/right folders based on the out_fps specified by user

try: 
    extract_sf(info_csv_L, output_path_L, out_fps)
    print(f"SAM2 start frames saved to {output_path_L} assuming an extracted fps of {out_fps}.")
except OSError as e:
    # Handle potential errors during directory creation (e.g., permissions)
    print(f"Error creating left SAM2 start_frames: {e}")

try: 
    extract_sf(info_csv_R, output_path_R, out_fps)
    print(f"SAM2 start frames saved to {output_path_R} assuming an extracted fps of {out_fps}.")
except OSError as e:
    # Handle potential errors during directory creation (e.g., permissions)
    print(f"Error creating right SAM2 start_frames: {e}")



print(f"The frame delay is {frame_delay}")
print("Check frame delay is reasonable, and that videos actually look synced")


