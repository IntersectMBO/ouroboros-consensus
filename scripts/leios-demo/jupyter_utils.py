# Utility module for analysis in Jupyter notebooks
import json
import pandas as pd
import altair as alt
import itables as itables
import ipywidgets as widgets
import plotly.offline as plotly
import plotly.express as px

plotly.init_notebook_mode(connected=True)
itables.init_notebook_mode(all_interactive=True)


def df_from_cardano_node_logs(fp):
    lines = open(fp, "r").readlines()
    at_lines = [json.loads(line) for line in lines if line.startswith('{"at')]
    return pd.DataFrame.from_records(at_lines)


def df_from_cardano_node_journal(fp):
    lines = open(fp, "r").readlines()
    records = []
    for line in lines:
        js = json.loads(line)
        if (
            js["_SYSTEMD_UNIT"] == "cardano-node.service"
            and js["MESSAGE"]
            and 'at":' in js["MESSAGE"]
        ):
            records.append(json.loads(js["MESSAGE"]))
    df = pd.DataFrame.from_records(records)
    return df.assign(source=lambda r: r.host)


def events_chart(df, y, color, symbol=None):
    fig = px.scatter(
        df,
        x="at",
        y=y,
        color=color,  # Optional: Group events by color
        symbol=color if not symbol else symbol,
        title="Event chart",
        hover_data=["ns"],  # Show event name on hover
    )

    # Add the Range Slider and Range Selector Buttons
    fig.update_xaxes(
        # Enable the Range Slider below the chart
        rangeslider_visible=True,
        # Add preset buttons for easy selection (e.g., 1 month, 6 months)
        rangeselector=dict(
            buttons=list(
                [
                    dict(count=1, label="1M", step="month", stepmode="backward"),
                    dict(count=6, label="6M", step="month", stepmode="backward"),
                    dict(step="all", label="ALL"),
                ]
            )
        ),
    )
    return fig


NS_WITH_BLOCK_HASH = [
    "BlockFetch.Client.CompletedBlockFetch",
    "BlockFetch.Client.SendFetchRequest",
    "ChainDB.AddBlockEvent.AddedBlockToQueue",
    "ChainDB.AddBlockEvent.AddedBlockToVolatileDB",
    "ChainDB.AddBlockEvent.AddedToCurrentChain",
    "ChainDB.AddBlockEvent.ChangingSelection",
    "ChainDB.AddBlockEvent.PipeliningEvent.OutdatedTentativeHeader",
    "ChainDB.AddBlockEvent.PipeliningEvent.SetTentativeHeader",
    "ChainDB.AddBlockEvent.PoppedBlockFromQueue",
    "ChainDB.AddBlockEvent.TryAddToCurrentChain",
    "ChainDB.ChainSelStarvationEvent",
    "ChainSync.Client.DownloadedHeader",
    "ChainSync.Client.GaveLoPToken",
    "ChainSync.Client.RolledBack",
    "ChainSync.Client.ValidatedHeader",
    "Consensus.GSM.EnterCaughtUp",
    "Consensus.GSM.LeaveCaughtUp",
]


def is_exception(record):
    return any(
        [
            record.ns == "ChainDB.AddBlockEvent.PoppedBlockFromQueue"
            and "risingEdge" in record.data,
            record.ns == "ChainDB.ChainSelStarvationEvent"
            and "risingEdge" in record.data,
            record.ns == "ChainSync.Client.RolledBack"
            and record.data["tip"]["kind"] == "GenesisPoint",
            record.ns == "Consensus.GSM.EnterCaughtUp"
            and record.data["currentSelection"]["kind"] == "TipGenesis",
        ]
    )


def extract_block_hash(record):
    if "block" in record.data:
        if isinstance(record.data["block"], str):
            return record.data["block"]
        if "hash" in record.data["block"]:
            return record.data["block"]["hash"]
        if "headerHash" in record.data["block"]:
            return record.data["block"]["headerHash"]
    if "headerHash" in record.data:
        return record.data["headerHash"]
    if "head" in record.data:
        return record.data["head"]
    if "fallingEdge" in record.data:
        return record.data["fallingEdge"]["hash"]
    if "newtip" in record.data:
        return record.data["newtip"]
    if "tip" in record.data:
        return record.data["tip"]["headerHash"]
    if "currentSelection" in record.data:
        return record.data["currentSelection"]["tipHash"]
