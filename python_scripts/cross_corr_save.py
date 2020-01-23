from ephys_queries import select_spike_times
from ephys_queries import db_setup_core
from spiketimes.df import cross_corr_df_test, cross_corr_between_groups_test
import dotenv
from pathlib import Path
import pandas as pd
import numpy as np
from pyarrow.feather import write_feather, read_feather

# load df labels
data_dir = Path(__file__).absolute().parent.parent / "data"
tmp_dir = data_dir / "tmp"
df_labels = pd.read_csv(data_dir / "chronic_baseline.csv")
df_labels = df_labels[["neuron_id", "type", "session_name"]]

# select baseline spiketimes
dotenv.load_dotenv()
engine, metadata = db_setup_core()
group_names = [
    "citalopram_continuation",
    "chronic_saline",
    "citalopram_discontinuation",
    "chronic_citalopram",
    "chronic_saline_",
]
block_name = "pre"
df_spikes = select_spike_times(engine, metadata, block_name=block_name)
df_spikes["spiketimes"] = df_spikes["spike_time_samples"].divide(30000)

# merge datasets
df = pd.merge(df_spikes, df_labels, on="neuron_id")

# apply exclusion criteria
df = df[df["type"] != "no_baseline"]

# calculate cross corr
recording_sessions = df_labels["session_name"].unique()
gw_frames = []
sr_frames = []

for session in recording_sessions:
    df_sub = df[df["session_name"] == session].copy()

    gw = cross_corr_between_groups_test(
        df_sub,
        group_col="type",
        spiketimes_col="spiketimes",
        neuron_col="neuron_id",
        n_cores=15,
        bin_window=0.01,
        num_lags=100,
    )
    gw["session_name"] = session
    gw_frames.append(gw)

    sr = cross_corr_df_test(
        df_sub[df_sub["type"] == "slow_regular"].copy(),
        spiketimes_col="spiketimes",
        neuron_col="neuron_id",
        n_cores=15,
        bin_window=0.01,
        num_lags=100,
    )
    sr["session_name"] = session
    sr_frames.append(sr)


gw = pd.concat(gw_frames, axis=0)
gw.to_csv(data_dir / "cross_corr_gw.csv", index=False)

sr = pd.concat(sr_frames, axis=0)
sr.to_csv(data_dir / "cross_corr_sr.csv", index=False)
