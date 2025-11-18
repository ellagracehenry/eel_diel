#SR 03-21-24 3D reconstruction script
import numpy as np
import os
import subprocess
from scipy.signal import correlate
import librosa
import shutil
import pandas as pd





######### Frame Syncing and Cutting Functions

#Get Frame Rate
def get_frame_rate(video_path):
    # Run ffprobe to get the frame rate in the format "numerator/denominator"
    result = subprocess.run(
        ['ffprobe', '-v', 'error', '-select_streams', 'v:0', '-show_entries', 'stream=r_frame_rate', '-of', 'default=nk=1:nw=1', video_path],
        capture_output=True,
        text=True
    )
    
    # Extract the frame rate string
    frame_rate_str = result.stdout.strip()
    
    # Split the frame rate string by '/' and calculate the rate
    numerator, denominator = map(int, frame_rate_str.split('/'))
    frame_rate = numerator / denominator
    
    return frame_rate


#function to extract audio features and find the delay between cameras. Frame rate must be the same for
#the cameras in the stereo pair
def extract_audio_features(filename1, filename2,fps):
    video1=str(filename1)
    video2=str(filename2)
    # Load audio using librosa
    y1, sr1 = librosa.load(video1, sr=None)
    y2, sr2 = librosa.load(video2, sr=None)
    corr = correlate(y1, y2, mode='full')
    norm_corr=corr / (np.linalg.norm(corr) * np.linalg.norm(corr))
    offset = np.argmax(norm_corr) - (len(y2) - 1)
    # ***Change this if not 24fps:
    offset_frames= round((offset/sr1)*fps)
    #frame offset: if positive, camera 1 started first. If negative, camera 2 started first
    if offset_frames >0:
        frame_delay=[offset_frames,0]
    else:
        frame_delay=[0,-offset_frames]
    return frame_delay

def trim_videos(input_video_1, input_video_2, output_video_1, output_video_2, delay, fps):
    """
    Trims the first few frames from two videos and saves them as new files.

    :param input_video_1: Path to the first input video
    :param input_video_2: Path to the second input video
    :param output_video_1: Path to save the trimmed first video
    :param output_video_2: Path to save the trimmed second video
    :param delay: List of two integers [frames_to_cut_from_video_1, frames_to_cut_from_video_2]
    """
    frames_to_cut_video_1 = int(delay[0])
    frames_to_cut_video_2 = int(delay[1])
    
    # Convert frame delays to seconds
    # Assuming frame rate is 30 fps, you can adjust this if the fps is different.
    seconds_to_cut_video_1 = frames_to_cut_video_1 / fps
    seconds_to_cut_video_2 = frames_to_cut_video_2 / fps

    # Build ffmpeg command to trim the videos
    cmd_1 = [
        'ffmpeg', '-i', input_video_1, '-ss', str(seconds_to_cut_video_1),
        '-c:v', 'libx264', '-c:a', 'aac', '-strict', 'experimental', '-preset', 'fast',
        '-vsync', 'vfr', output_video_1
    ]

    cmd_2 = [
        'ffmpeg', '-i', input_video_2, '-ss', str(seconds_to_cut_video_2),
        '-c:v', 'libx264', '-c:a', 'aac', '-strict', 'experimental', '-preset', 'fast',
        '-vsync', 'vfr', output_video_2
    ]

    try:
        subprocess.run(cmd_1, check=True)
        subprocess.run(cmd_2, check=True)
        print(f"Videos trimmed and saved as {output_video_1} and {output_video_2}")
    except subprocess.CalledProcessError as e:
        print(f"Error during ffmpeg processing: {e}")

def trim_videos1(input_video_1, input_video_2, output_video_1, output_video_2, delay, fps):
    """
    Trims the first few frames from the video that needs trimming and saves it as a new file.

    :param input_video_1: Path to the first input video
    :param input_video_2: Path to the second input video
    :param output_video_1: Path to save the trimmed first video
    :param output_video_2: Path to save the trimmed second video
    :param delay: List of two integers [frames_to_cut_from_video_1, frames_to_cut_from_video_2]
    """
    frames_to_cut_video_1 = int(delay[0])
    frames_to_cut_video_2 = int(delay[1])

    # Convert frame delays to seconds
    seconds_to_cut_video_1 = frames_to_cut_video_1 / fps
    seconds_to_cut_video_2 = frames_to_cut_video_2 / fps

    try:
        # Trim the first video if needed
        if seconds_to_cut_video_1 > 0:
            cmd_1 = [
                'ffmpeg', '-i', input_video_1, '-ss', str(seconds_to_cut_video_1),
                '-c:v', 'libx264', '-c:a', 'aac', '-strict', 'experimental', '-preset', 'fast',
                '-vsync', 'vfr', output_video_1
            ]
            subprocess.run(cmd_1, check=True)
            print(f"First video trimmed and saved as {output_video_1}")
        else:
            # Copy the first video without reprocessing
            shutil.copy(input_video_1, output_video_1)
            print(f"First video unchanged. Copied to {output_video_1}")

        # Trim the second video if needed
        if seconds_to_cut_video_2 > 0:
            cmd_2 = [
                'ffmpeg', '-i', input_video_2, '-ss', str(seconds_to_cut_video_2),
                '-c:v', 'libx264', '-c:a', 'aac', '-strict', 'experimental', '-preset', 'fast',
                '-vsync', 'vfr', output_video_2
            ]
            subprocess.run(cmd_2, check=True)
            print(f"Second video trimmed and saved as {output_video_2}")
        else:
            # Copy the second video without reprocessing
            shutil.copy(input_video_2, output_video_2)
            print(f"Second video unchanged. Copied to {output_video_2}")

    except subprocess.CalledProcessError as e:
        print(f"Error during ffmpeg processing: {e}")

def synchronize_videos(video1_path, video2_path, delay, output_video1, output_video2, fps):
    """
    Synchronizes two videos by trimming the delay from the one that started later.
    
    Args:
        video1_path (str): Path to the first video.
        video2_path (str): Path to the second video.
        video1_delay (float): Delay in seconds for video 1 (e.g., 0.67).
        video2_delay (float): Delay in seconds for video 2 (e.g., 0.45).
        output_video1 (str): Output path for the trimmed video 1.
        output_video2 (str): Output path for the trimmed video 2.
    """
    video1_delay, video2_delay = delay
    # Determine which video has the larger delay
    if video1_delay < video2_delay:
        # video1 started first, remove delay from video2
        delay_to_cut = video2_delay
        video_to_trim = video2_path
        output_video = output_video2
    else:
        # video2 started first, remove delay from video1
        delay_to_cut = video1_delay
        video_to_trim = video1_path
        output_video = output_video1

    # Build the FFmpeg command to trim the delay (using -ss to cut from the start)
    ffmpeg_command = [
        'ffmpeg',
        '-i', video_to_trim,  # Input video
        '-ss', str(delay_to_cut),  # Start video from this delay (in seconds)
        '-c', 'copy',  # Copy the codec (to avoid re-encoding)
        output_video  # Output path
    ]

    # Run the FFmpeg command to trim the delay
    subprocess.run(ffmpeg_command)

    print(f"Video synchronized. Output saved to {output_video}")


def sync_videos(video1_path, video2_path, frame_delay, output1_path, output2_path, fps=24):
    """
    Syncs two videos by trimming the starting frames of the video that starts first.

    Parameters:
    - video1_path: str, path to the first video file.
    - video2_path: str, path to the second video file.
    - frame_delay: list, delay in frames [delay_cam1, delay_cam2].
    - output1_path: str, path to save the synced first video.
    - output2_path: str, path to save the synced second video.
    - fps: int, frame rate of the videos (default: 24).
    """
    delay_cam1, delay_cam2 = frame_delay

    # Calculate the start time in seconds based on the frame delay
    start_time1 = delay_cam1 / fps if delay_cam1 > 0 else 0
    start_time2 = delay_cam2 / fps if delay_cam2 > 0 else 0

    # Trim the videos using ffmpeg_extract_subclip
    if start_time1 > 0:
        ffmpeg_extract_subclip(video1_path, start_time1, None, targetname=output1_path)
    else:
        ffmpeg_extract_subclip(video1_path, 0, None, targetname=output1_path)

    if start_time2 > 0:
        ffmpeg_extract_subclip(video2_path, start_time2, None, targetname=output2_path)
    else:
        ffmpeg_extract_subclip(video2_path, 0, None, targetname=output2_path)

    print("Videos synced and saved:")
    print(f"- {output1_path}")
    print(f"- {output2_path}")

def delete_unmatched_files(folder1, folder2):
    # Get the set of filenames from each folder
    files1 = set(os.listdir(folder1))
    files2 = set(os.listdir(folder2))
    
    # Files that are in folder1 but not in folder2
    unmatched_files1 = files1 - files2
    
    # Files that are in folder2 but not in folder1
    unmatched_files2 = files2 - files1
    
    # Full paths for the files to be deleted
    unmatched_files1_paths = [os.path.join(folder1, f) for f in unmatched_files1]
    unmatched_files2_paths = [os.path.join(folder2, f) for f in unmatched_files2]

    # Delete files that are in folder1 but not in folder2
    for file_path in unmatched_files1_paths:
        try:
            os.remove(file_path)
            print(f"Deleted {file_path} from {folder1}")
        except Exception as e:
            print(f"Error deleting {file_path}: {e}")
    
    # Delete files that are in folder2 but not in folder1
    for file_path in unmatched_files2_paths:
        try:
            os.remove(file_path)
            print(f"Deleted {file_path} from {folder2}")
        except Exception as e:
            print(f"Error deleting {file_path}: {e}")



def remove_first_n_files(folder_path, n):
    try:
        # Get a list of files in the folder
        files = sorted([f for f in os.listdir(folder_path) if os.path.isfile(os.path.join(folder_path, f))])
        
        # Ensure there are files to remove
        if not files:
            print(f"No files found in {folder_path}.")
            return
        
        # Determine the number of files to remove
        files_to_remove = files[:n]
        
        # Remove the files
        for file in files_to_remove:
            file_path = os.path.join(folder_path, file)
            os.remove(file_path)
            print(f"Removed: {file_path}")
            
        print(f"Removed {len(files_to_remove)} files from {folder_path}.")
        
    except Exception as e:
        print(f"An error occurred: {e}")


# Function to extract video metadata using ffprobe
def get_ffprobe_data(video_file, entry):
    try:
        result = subprocess.run(
            ["ffprobe", "-v", "error", "-select_streams", "v:0", "-show_entries", entry, "-of", "csv=p=0", str(video_file)],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            check=True
        )
        return result.stdout.strip()
    except subprocess.CalledProcessError:
        return None
    

# Extract SAM2_sf frames
def extract_sf(input_path, output_path, out_fps):
# Load the CSV (EDIT PATH AND NAME AS NEEDED)

    # Read CSV
    df = pd.read_csv(input_path)

    # Sort by 'File Name'
    df = df.sort_values(by="File Name")

    # Drop rows where 'Total Frames' is missing
    df = df.dropna(subset=["Total Frames"])

    # Expand rows according to 'Total Frames'
    df_expanded = df.loc[df.index.repeat(df["Total Frames"].astype(int))].copy()

    # Add a sequential RowIndex (starting from 0)
    df_expanded["RowIndex"] = range(len(df_expanded))

    # Add RowIndex_File (reset for each 'File Name')
    df_expanded["RowIndex_File"] = df_expanded.groupby("File Name").cumcount()

    # Filter rows based on every (Frame Rate / out_fps)
    df_expanded = df_expanded[
        df_expanded["RowIndex"] % (df_expanded["Frame Rate"].round().astype(int) // out_fps) == 0
    ]

    # Keep only the first row per file
    df_final = df_expanded.groupby("File Name").head(1)

    # Select and rename columns
    df_final = df_final[["File Name", "Frame Rate", "Total Frames", "RowIndex_File"]].rename(
        columns={"RowIndex_File": "SAM2_sf"}
    )

    # Save CSV
    df_final.to_csv(output_path, index=False)