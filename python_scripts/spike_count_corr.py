from ephys_queries import select_spike_times
from ephys_queries import db_setup_core
from spiketimes.df import (
    spike_count_correlation_df_test,
    spike_count_correlation_between_groups_test,
    spike_count_correlation_between_groups,
)
import dotenv
from pathlib import Path
import pandas as pd


# load neuron_type data
data_dir = Path(__file__).absolute().parent.parent / "data"
df_labels = pd.read_csv(data_dir / "chronic_baseline.csv")
df_labels = df_labels[["neuron_id", "type", "session_name"]]

# select spiketime data from the database
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

# iterate over recording sessions
recording_sessions = df_labels["session_name"].unique()
for session in recording_sessions:
    df_sub = df[df["session_name"] == session]
    spike_count_correlation_between_groups_test(
        df_sub,
        fs=1,
        n_boot=4000,
        spiketimes_col="spiketimes",
        neuron_col="neuron_id",
        group_col="type",
    )
    spike_count_correlation_between_groups(
        df_sub,
        fs=1,
        spiketimes_col="spiketimes",
        neuron_col="neuron_id",
        group_col="type",
    )

