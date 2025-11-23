import os
import sys
import json
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# --- Configuration ---
# Filter for the event containing the timestamp we want to measure at node0 and downstream
BLOCK_EVENT_FILTER = "BlockFetch.Client.CompletedBlockFetch"
# Filter for the event containing the slot and hash. We need to do this because the 'CompletedBlockFetch' event does not contain the slot number.
HEADER_EVENT_FILTER = "ChainSync.Client.DownloadedHeader"

# Filter for the event containing the timestamp we want to measure at node0 and downstream
LEIOS_EVENT_FILTER = "Consensus.LeiosKernel"

LEIOS_BLOCK_EVENT_SUBFILTER = "LeiosBlockAcquired"
LEIOS_BLOCKTXS_EVENT_SUBFILTER = "LeiosBlockTxsAcquired"

def filter_log_events(log_path: str, filter_text: str, subfilter_text : str = ""):
    """
    Reads a log file, parses JSON lines, and extracts relevant fields
    based on the filter type.
    """
    log_filename = os.path.basename(log_path)
    print(f"\n--- Analyzing Log: {log_filename} for event: '{filter_text}' ---")

    parsed_data = []

    try:
        with open(log_path, "r") as f:
            for line in f:
                try:
                    log_entry = json.loads(line)

                    # Check if the namespace matches the filter
                    if log_entry.get("ns") == filter_text:

                        event_data = log_entry.get("data", {})
                        block_hash = None
                        block_slot = None

                        # Determine extraction logic based on the event type
                        if filter_text == HEADER_EVENT_FILTER:
                            # Structure: "data":{"block": "HASH", ..., "slot": SLOT}
                            block_hash = event_data.get("block")
                            block_slot = event_data.get("slot")
                        elif filter_text == BLOCK_EVENT_FILTER:
                            # Structure: "data":{"block": "HASH", ...}
                            block_hash = event_data.get("block")
                            block_slot = None
                        elif filter_text == LEIOS_EVENT_FILTER:
                            # Structure: "data":{"kind": "LeiosBlockTxsAcquired", "ebHash": "HASH", "ebSlot": "SLOT"}
                            if subfilter_text != event_data.get("kind", "XXXXXXXX"):
                                continue
                            block_hash = event_data.get("ebHash")
                            block_slot = None   # event_data.get("ebSlot")   # slot comes from schedule.json

                        # Base record structure
                        record = {
                            "at": log_entry.get("at"),
                            "hash": block_hash,
                            "slot": block_slot,
                        }

                        # Only add if the core fields were successfully extracted
                        if record["at"] and record["hash"]:
                            parsed_data.append(record)

                except json.JSONDecodeError:
                    # Some log lines are not JSON; we simply skip those
                    continue
                except Exception as e:
                    print(f"An unexpected error occurred while processing a line in {log_path}", file=sys.stderr)
                    raise

            return parsed_data

    except Exception as e:
        print(f"An unexpected error occurred while processing {log_path}", file=sys.stderr)
        raise

def create_and_clean_df(
    records: list, node_id: str
) -> pd.DataFrame:
    """
    Converts records to a DataFrame, converts types and removes duplicates.
    """
    # Return an empty DataFrame with the expected columns if no records were found.
    if not records:
        return pd.DataFrame(columns=["hash", "slot", "at"])

    df = pd.DataFrame(records)

    # Convert columns to appropriate data types
    try:
        df["at"] = pd.to_datetime(df["at"])
        if "slot" in df.columns:
            df["slot"] = pd.to_numeric(df["slot"], errors="coerce").astype("Int64")
    except Exception as e:
        print(
            f"Warning: Failed to convert data types in DataFrame for node {node_id}: {e}",
            file=sys.stderr,
        )
        raise

    # Deduplication: Keep only the first (earliest) occurrence
    initial_rows = len(df)
    df = df.sort_values(
        by="at"
    ).drop_duplicates(subset=["hash"], keep="first")

    if len(df) < initial_rows:
        duplicates_removed = initial_rows - len(df)
        print(
            f"Warning: Removed {duplicates_removed} duplicate log entries from node {node_id}.",
            file=sys.stderr,
        )

    return df

def load_block_arrivals(df_headers: pd.DataFrame, log_path: str, node_id: str):
    """
    Loads the hash arrival times at the node of the given log file.
    """
    raw_data = filter_log_events(log_path, BLOCK_EVENT_FILTER)
    df = create_and_clean_df(
        raw_data, node_id
    )[["hash", "at"]]

    # add slot column
    df = pd.merge(df, df_headers, on="hash", how="left")

    if df["slot"].isna().any():
        print(
            f"Error: {node_id} downloaded somes blocks with unknown slot.",
            file=sys.stderr,
        )
        print(
            df[df["slot"].isnull()],
            file=sys.stderr,
        )
        raise

    # Use 'Int64' to allow NaNs; see https://stackoverflow.com/a/54194908
    df["latency_ms"] = ((
        df["at"] - df["slot_onset"]
    ).dt.total_seconds() * 1000).round(0).astype('Int64')

    return df

def load_leios_block_arrivals(df_schedule: pd.DataFrame, log_path: str, node_id: str, subfilter: str):
    """
    Loads the hash arrival times for Leios blocks at the node of the given log file.
    """
    raw_data = filter_log_events(log_path, LEIOS_EVENT_FILTER, subfilter)
    df = create_and_clean_df(
        raw_data, node_id
    )[["hash", "at"]]

    # add slot column
    df = pd.merge(df, df_schedule, on="hash", how="left")

    if df["offer_slot"].isna().any():
        print(
            f"Error: {node_id} downloaded somes Leios blocks with unknown offer slot.",
            file=sys.stderr,
        )
        print(
            df[df["offer_slot"].isnull()],
            file=sys.stderr,
        )
        raise

    # Use 'Int64' to allow NaNs; see https://stackoverflow.com/a/54194908
    df["latency_ms"] = ((
        df["at"] - df["offer"]
    ).dt.total_seconds() * 1000).round(0).astype('Int64')

    return df

def plot_onset_vs_arrival(df: pd.DataFrame, output_file: str = None):
    """
    Generates and displays a scatter plot of slot_onset vs. at_downstream.
    If output_file is provided, saves the plot to that file.
    """
    print("\n--- Generating Scatter Plot ---")
    try:
        if "slot_onset" in df.columns and "at_downstream" in df.columns:
            # Ensure both columns are datetime objects for plotting
            df["slot_onset"] = pd.to_datetime(df["slot_onset"])
            df["at_downstream"] = pd.to_datetime(df["at_downstream"])

            plt.figure(figsize=(10, 6))
            plt.scatter(df["slot_onset"], df["at_downstream"], alpha=0.5, s=10)

            # Add a y=x reference line
            # Find common min/max for a good 1:1 line
            all_times = pd.concat([df["slot_onset"], df["at_downstream"]])
            min_time = all_times.min()
            max_time = all_times.max()
            plt.plot(
                [min_time, max_time],
                [min_time, max_time],
                "r--",
                label="1:1 Line (Onset = Arrival)",
            )

            plt.title("Block Arrival Time (downstream) vs. Slot Onset Time")
            plt.xlabel("Slot Onset Time (Calculated)")
            plt.ylabel("Block Arrival Time (at_downstream)")
            plt.grid(True, linestyle="--", alpha=0.6)
            plt.legend()
            plt.tight_layout()

            # Rotate x-axis labels for better readability
            plt.xticks(rotation=45)

            if output_file:
                plt.savefig(output_file, bbox_inches="tight")
                print(f"Plot saved to {output_file}")
            else:
                print("Displaying plot...")
                plt.show()

            plt.close(plt.gcf())  # Close the figure to free memory

        else:
            print(
                "Warning: 'slot_onset' or 'at_downstream' column not found. Skipping plot generation."
            )

    except ImportError:
        print(
            "\n--- Plotting Skipped ---",
            file=sys.stderr,
        )
        print(
            "To generate the plot, please install matplotlib: pip install matplotlib",
            file=sys.stderr,
        )
    except Exception as e:
        print(
            f"Error: Failed to generate plot. Error: {e}",
            file=sys.stderr,
        )


if __name__ == "__main__":
    # --- Argument Parsing ---
    try:
        initial_slot_str = sys.argv[1]
        initial_time_str = sys.argv[2]
        eb_schedule_path = sys.argv[3]
        log_path_node0 = sys.argv[4]
        log_path_downstream = sys.argv[5]
    except Exception:
        print(
            "Configuration Error: Please provide initial-slot, initial-time, EB sending schedule, two log files, and optionally an output plot file.",
            file=sys.stderr,
        )
        print(
            "Example Usage: python log_parser.py <initial-slot> <initial-time> demoSchedule.json /path/to/node0.log /path/to/downstream.log [output_plot.png]",
            file=sys.stderr,
        )
        sys.exit(1)

    try:
        plot_output_file = sys.argv[6]
    except Exception:
        plot_output_file = None

    try:
        initial_slot = int(initial_slot_str)
    except ValueError:
        print(
            f"Configuration Error: Could not parse initial-slot '{initial_slot_str}' as an integer.",
            file=sys.stderr,
        )
        sys.exit(1)

    # Try to parse as a POSIX timestamp (integer string) or else as a datetime string
    try:
        try:
            posix_time = int(initial_time_str)
            # Convert from POSIX seconds to a UTC datetime object
            initial_time = pd.to_datetime(posix_time, unit="s", utc=True)
            print(
                f"Note: Interpreted initial-time '{initial_time_str}' as POSIX timestamp (UTC)."
            )
        except ValueError:
            # If not an integer, try to parse as a standard datetime string
            initial_time = pd.to_datetime(initial_time_str)
            # If the provided string has no timezone, assume UTC for consistency
            if initial_time.tzinfo is None:
                initial_time = initial_time.tz_localize("UTC")
                print(
                    f"Note: Interpreted initial-time '{initial_time_str}' as datetime string (assuming UTC)."
                )
            else:
                # If it has a timezone, convert it to UTC for consistency
                initial_time = initial_time.tz_convert("UTC")
    except Exception as e:
        print(
            f"Configuration Error: Could not parse initial-time '{initial_time_str}' as either a POSIX timestamp or a datetime string. Error: {e}",
            file=sys.stderr,
        )
        sys.exit(1)

    print(f"\n--- Initial Configuration ---")
    print(f"Initial Slot: {initial_slot}")
    print(f"Initial Time: {initial_time}")
    print(f"node0 Log File: {log_path_node0}")
    print(f"downstream Log File: {log_path_downstream}")
    if plot_output_file:
        print(f"Plot Output File: {plot_output_file}")

    # Collect header data
    #
    # node0 log suffices, since downstream only has a header if node0 does.
    header_data = filter_log_events(log_path_node0, HEADER_EVENT_FILTER)

    if not header_data:
        print("\nNo header events found for slot/hash lookup. Exiting.")
        sys.exit(0)

    # Create the header lookup DataFrame
    #
    # Retain only the hash and slot columns and only if slot isn't None
    df_headers = (
        create_and_clean_df(
            header_data, "node0"
        )[["hash", "slot"]]
        .dropna(subset=["slot"])
        .drop_duplicates(subset=["hash"], keep="first")
    )

    print(f"Created Hash-to-Slot lookup table with {len(df_headers)} unique entries.")

    try:
        # Calculate the difference in slots (where 1 slot happens to be 1 second)
        slot_diff_seconds = df_headers["slot"] - initial_slot

        # Convert the second difference into a timedelta and add to the initial time
        df_headers["slot_onset"] = initial_time + pd.to_timedelta(
            slot_diff_seconds, unit="s"
        )
    except Exception:
        print("Error: Failed to calculate slot onset times.", file=sys.stderr)
        raise

    df_node0 = load_block_arrivals(df_headers, log_path_node0, "node0")
    df_downstream = load_block_arrivals(df_headers, log_path_downstream, "downstream")

    if df_node0.empty or df_downstream.empty:
        print(
            "\nCould not match block fetch times to slot numbers for one or both nodes. Exiting."
        )
        sys.exit(0)

    # We also merge on "slot_onset" to retain it. It's a function of "slot", so
    # this is harmless.
    df_merged = pd.merge(
        df_node0, df_downstream,
        suffixes=["_node0", "_downstream"],
        on=["slot", "slot_onset", "hash"],   # note that this determines order of resulting rows
        how="outer",
    )

#    plot_onset_vs_arrival(df_merged, plot_output_file)

    print("\n--- Extracted and Merged Data Summary ---")
    print(
        "Each row represents a unique Praos block seen by both nodes, joined by hash and slot."
    )
    # Which columns to display
    display_columns = [
        "slot",
        "hash",
        "slot_onset",
        "latency_ms_node0",
        "latency_ms_downstream",
    ]

    pd.set_option('display.max_columns', None)
    pd.set_option('display.expand_frame_repr', False)
    print(df_merged[display_columns])

    # ---------------------------------------- again, for Leios messages

    # Create the schedule lookup DataFrame
    with open(eb_schedule_path) as f:
        schedule_json = json.load(f)

    # only the MsgLeiosBlockTxsOffer messages
    df_schedule = pd.DataFrame(
        [ (x[0], x[1][1]) for x in schedule_json if None == x[1][2] ],
        columns=["offer_slot", "hash"],
    )

    print(f"Created Hash-to-Offer lookup table with {len(df_schedule)} unique entries.")

    try:
        # Calculate the difference in slots (where 1 slot happens to be 1 second)
        slot_diff_seconds = df_schedule["offer_slot"] - initial_slot

        # Convert the second difference into a timedelta and add to the initial time
        df_schedule["offer"] = initial_time + pd.to_timedelta(
            slot_diff_seconds, unit="s"
        )
    except Exception:
        print("Error: Failed to calculate offer times.", file=sys.stderr)
        raise

    df_leios_block_node0 = load_leios_block_arrivals(df_schedule, log_path_node0, "node0", LEIOS_BLOCK_EVENT_SUBFILTER)
    df_leios_block_downstream = load_leios_block_arrivals(df_schedule, log_path_downstream, "downstream", LEIOS_BLOCK_EVENT_SUBFILTER)

    # We also merge on "slot_onset" to retain it. It's a function of "slot", so
    # this is harmless.
    df_leios_block_merged = pd.merge(
        df_leios_block_node0, df_leios_block_downstream,
        suffixes=["_node0", "_downstream"],
        on=["offer_slot", "offer", "hash"],   # note that this determines order of resulting rows
        how="outer",
    )

    print("\n--- Extracted and Merged Data Summary for Leios blocks ---")
    print(
        "Each row represents a unique Leios block seen by both nodes, joined by hash and offer slot."
    )
    # Which columns to display
    leios_display_columns = [
        "offer_slot",
        "hash",
        "offer",
        "latency_ms_node0",
        "latency_ms_downstream",
    ]

    print(df_leios_block_merged[leios_display_columns])

    df_leios_blocktxs_node0 = load_leios_block_arrivals(df_schedule, log_path_node0, "node0", LEIOS_BLOCKTXS_EVENT_SUBFILTER)
    df_leios_blocktxs_downstream = load_leios_block_arrivals(df_schedule, log_path_downstream, "downstream", LEIOS_BLOCKTXS_EVENT_SUBFILTER)

    # We also merge on "slot_onset" to retain it. It's a function of "slot", so
    # this is harmless.
    df_leios_blocktxs_merged = pd.merge(
        df_leios_blocktxs_node0, df_leios_blocktxs_downstream,
        suffixes=["_node0", "_downstream"],
        on=["offer_slot", "offer", "hash"],   # note that this determines order of resulting rows
        how="outer",
    )

    print("\n--- Extracted and Merged Data Summary for Leios blocks ---")
    print(
        "Each row represents a unique Leios closure seen by both nodes, joined by hash and offer slot."
    )
    # Which columns to display
    leios_display_columns = [
        "offer_slot",
        "hash",
        "offer",
        "latency_ms_node0",
        "latency_ms_downstream",
    ]

    print(df_leios_blocktxs_merged[leios_display_columns])
