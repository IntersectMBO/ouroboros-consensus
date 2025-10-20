import os
import sys
import json
import pandas as pd
import numpy as np

# --- Configuration ---
# Filter for the event containing the timestamp we want to measure at node 0 and node 1
BLOCK_EVENT_FILTER = 'BlockFetch.Client.CompletedBlockFetch'
# Filter for the event containing the slot and hash. We need to do this because the 'CompletedBlockFetch' event does not contain the slot number.
HEADER_EVENT_FILTER = 'ChainSync.Client.DownloadedHeader'

def filter_log_events(log_path: str, filter_text: str):
    """
    Reads a log file, parses JSON lines, and extracts relevant fields
    based on the filter type.
    """
    log_filename = os.path.basename(log_path)
    print(f"\n--- Analyzing Log: {log_filename} for event: '{filter_text}' ---")

    parsed_data = []

    try:
        with open(log_path, 'r') as f:
            for line in f:
                try:
                    log_entry = json.loads(line)

                    # Check if the namespace matches the filter
                    if log_entry.get('ns') == filter_text:

                        event_data = log_entry.get('data', {})
                        block_hash = None
                        block_slot = None

                        # Determine extraction logic based on the event type
                        if filter_text == HEADER_EVENT_FILTER:
                            # Structure: "data":{"block": "HASH", ..., "slot": SLOT}
                            block_hash = event_data.get('block')
                            block_slot = event_data.get('slot')
                        elif filter_text == BLOCK_EVENT_FILTER:
                            # Structure: "data":{"block": "HASH", ...}
                            block_hash = event_data.get('block')
                            block_slot = None

                        # Base record structure
                        record = {
                            'node': log_filename.split('-')[-1].split('.')[0],
                            'at': log_entry.get('at'),
                            'hash': block_hash,
                            'slot': block_slot,
                        }

                        # Only add if the core fields were successfully extracted
                        if record['at'] and record['hash']:
                            parsed_data.append(record)

                except json.JSONDecodeError:
                    continue
                except Exception as e:
                    # This catch remains for general unexpected issues.
                    print(f"Warning: Failed to parse or extract fields from a line in {log_filename}. Error: {e}", file=sys.stderr)
                    continue

            print(f"Successfully extracted {len(parsed_data)} records matching '{filter_text}'.")
            return parsed_data

    except FileNotFoundError:
        print(f"Error: Log file not found at {log_path}.", file=sys.stderr)
        return []
    except Exception as e:
        print(f"An unexpected error occurred while processing {log_path}: {e}", file=sys.stderr)
        return []

def create_and_clean_df(records: list, node_id: str, timestamp_column: str, unique_subset: list) -> pd.DataFrame:
    """
    Converts records to a DataFrame, converts types, removes duplicates,
    and renames the 'at' column.
    """
    if not records:
        # Return an empty DataFrame with the expected columns if no records were found.
        # This prevents KeyError later during column selection.
        return pd.DataFrame(columns=['hash', 'slot', 'at', 'node']).rename(columns={'at': timestamp_column})


    df = pd.DataFrame(records)

    # Convert columns to appropriate data types
    try:
        if 'at' in df.columns:
            df['at'] = pd.to_datetime(df['at'])
        if 'slot' in df.columns:
            df['slot'] = pd.to_numeric(df['slot'], errors='coerce').astype('Int64')
    except Exception as e:
        print(f"Warning: Failed to convert data types in DataFrame for node {node_id}: {e}", file=sys.stderr)
        return pd.DataFrame(columns=['hash', 'slot', 'at', 'node']).rename(columns={'at': timestamp_column})


    # Deduplication: Keep only the first (earliest) occurrence
    initial_rows = len(df)
    df = df.sort_values(by='at' if 'at' in df.columns else df.columns[0]).drop_duplicates(subset=unique_subset, keep='first')

    if len(df) < initial_rows:
        duplicates_removed = initial_rows - len(df)
        print(f"Warning: Removed {duplicates_removed} duplicate log entries from node {node_id}.")

    # Rename the timestamp column for merging later
    if 'at' in df.columns:
        df = df.rename(columns={'at': timestamp_column})

    return df


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Configuration Error: Please provide the full path to exactly TWO log files.", file=sys.stderr)
        print("Example Usage: python log_parser.py /path/to/node-0.log /path/to/node-1.log", file=sys.stderr)
        sys.exit(1)

    log_path_0 = sys.argv[1]
    log_path_1 = sys.argv[2]

    # --- STEP 1: Create Hash-to-Slot Lookup Table (Headers) ---

    # Collect header data from node 0 only (the primary source for slot mapping)
    header_data = filter_log_events(log_path_0, HEADER_EVENT_FILTER)

    if not header_data:
        print("\nNo header events found for slot/hash lookup. Exiting.")
        sys.exit(0)

    # Create the header lookup DataFrame
    df_headers_full = create_and_clean_df(header_data, '0', 'at_header_lookup', ['slot', 'hash', 'node'])

    # Select only the necessary lookup columns and drop any entries where slot is still None
    df_headers = df_headers_full[['hash', 'slot']].dropna(subset=['slot']).drop_duplicates(subset=['hash'], keep='first')
    print(f"Created Hash-to-Slot lookup table with {len(df_headers)} unique entries.")


    # --- STEP 2: Collect and Process Block Fetch Timestamps ---

    # Node 0 Block Fetch Data
    raw_data_0 = filter_log_events(log_path_0, BLOCK_EVENT_FILTER)
    df_node_0_block = create_and_clean_df(raw_data_0, '0', 'at_node_0', ['hash', 'node'])

    # Node 1 Block Fetch Data
    raw_data_1 = filter_log_events(log_path_1, BLOCK_EVENT_FILTER)
    df_node_1_block = create_and_clean_df(raw_data_1, '1', 'at_node_1', ['hash', 'node'])

    # --- STEP 3: Inject Slot Number into Block Fetch Data ---

    # Inject 'slot' into Node 0 data using 'hash'
    df_node_0_final = pd.merge(
        df_node_0_block[['hash', 'at_node_0']],
        df_headers,
        on='hash',
        how='inner'
    )

    # Inject 'slot' into Node 1 data using 'hash'
    df_node_1_final = pd.merge(
        df_node_1_block[['hash', 'at_node_1']],
        df_headers,
        on='hash',
        how='inner'
    )


    # --- STEP 4: Final Merge on Hash AND Slot ---

    if df_node_0_final.empty or df_node_1_final.empty:
        print("\nCould not match block fetch times to slot numbers for one or both nodes. Exiting.")
        sys.exit(0)

    # Final merge to compare the two nodes for the same block
    df_merged = pd.merge(
        df_node_0_final,
        df_node_1_final,
        on=['hash', 'slot'],
        how='inner',
    )


    # --- STEP 5: Calculate Latency (Time Difference) ---
    df_merged['latency_ms'] = (df_merged['at_node_1'] - df_merged['at_node_0']).dt.total_seconds() * 1000


    print("\n--- Extracted and Merged Data Summary (First 5 Rows) ---")
    print("Each row represents a unique block seen by both nodes, joined by hash and slot.")
    print(df_merged.head())
    print(f"\nTotal unique block events matched: {len(df_merged)}")
