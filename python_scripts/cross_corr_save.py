from ephys_queries import select_spike_times
from ephys_queries import db_setup_core
from spiketimes.df import cross_corr_df_test
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
fnames = []

for session in recording_sessions:
    df_sub = df[df["session_name"] == session].copy()
    df_cc = cross_corr_df_test(
        df_sub,
        jitter_window_size=0.4,
        n_boot=1000,
        spiketimes_col="spiketimes",
        neuron_col="neuron_id",
        n_cores=15,
    )

    df_cc["session_name"] = session

    fname = str(tmp_dir / f"{session}_cc.feather")

    write_feather(df_cc, fname)

    fnames.append(fname)

cc_frames = [read_feather(fname) for fname in fnames]
df_cc = pd.concat(cc_frames, axis=0)
df_cc.to_csv(data_dir / "cross_corr_400ms_jitter.csv", index=False)
