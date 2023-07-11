from itertools import groupby
import pandas as pd
from sklearn.linear_model import LinearRegression
import numpy as np
import random
import matplotlib.pyplot as plt
from collections import Counter
import os


def positivity_calls(data_sort, Ig_name, Ig_sample):
    Dmin_dict = []
    Dmin_list = []
    T_dict = {}
    for q in range(len(Ig_name)):
        Dmin_list_temp = []
        temp_sample = Ig_sample[q]
        sample_dict = {}
        for j in range(len(temp_sample)):  # sampleList
            I = data_sort.loc[:, temp_sample[j]]
            # Generate Threshold
            I_arr = I.to_numpy()

            I_arr = I_arr[~np.isnan(I_arr)]
            min_num = np.min(np.abs(I_arr))
            max_num = np.max(np.abs(I_arr))

            seq_Threshold = list(np.arange(np.min(I_arr), np.max(I_arr), 0.01))

            seqY = np.arange(min_num, max_num, 0.01)
            seqY = list(seqY)
            seqY[0] = min_num
            #     seqY_list[temp_sample[j]]=seqY.reverse()
            # Generate FDR
            FDR = []
            # Finding F_s
            for i in seqY:
                temp = []
                rowSignal = I.to_numpy()
                temp.append(min([np.sum(rowSignal < -i) / np.sum(rowSignal > i), 1]))
                temp = [x for x in temp if str(x) != 'nan']
                FDR.append(np.median(temp))

            T_dict[Ig_name[q]] = seq_Threshold
            FDR = np.array(FDR)
            cutoff = 0.1

            Dmin = seqY[np.argmin(np.abs(FDR - cutoff))]
            Dmin_list_temp.append(Dmin)
            sample_dict[temp_sample[j]] = Dmin

        Dmin_dict.append(sample_dict)
        Dmin_list.append(Dmin_list_temp)
    return Dmin_dict


def build_directories(animal_ids):
    """
    Builds directories for storing tables & plots
    :param animal_ids: The ids of the animals
    :return: None
    """
    if not os.path.exists("plot"):
        os.makedirs("plot")
    if not os.path.exists("plot/boxplot"):
        os.makedirs("plot/boxplot")
    if not os.path.exists("plot/fluorescence"):
        os.makedirs("plot/fluorescence")
    if not os.path.exists("plot/positivity"):
        os.makedirs("plot/positivity")
    if not os.path.exists("tables"):
        os.makedirs("tables")
    for animal_id in animal_ids:
        animal_id = animal_id.split("_")[0]
        if not os.path.exists("tables/" + animal_id):
            os.makedirs("tables/" + animal_id)
        if not os.path.exists("tables/" + animal_id + "/IgG"):
            os.makedirs("tables/" + animal_id + "/IgG")
        if not os.path.exists("tables/" + animal_id + "/IgM"):
            os.makedirs("tables/" + animal_id + "/IgM")


def plot_positivity(data_summary_loc, labels, positives, blank_label):
    """
    Plots the fluorescent signal at the 2nd and 3rd timepoint with the signal from the 0 timepoint
    It also highlights regions of positivity
    :param data_summary_loc: The full dataframe
    :param labels: The labels for the 2nd and 3rd timepoint
    :param positives: The positive positions in the sequence
    :param blank_label: The 0 DPI signal
    :return: None
    """
    plt.close("all")
    fig, ax = plt.subplots(2)
    fig.set_size_inches(20, 15)
    for i, label in enumerate(labels):
        fluorescence = data_summary_loc[label]
        fluorescence_blank = data_summary_loc[blank_label[i]]
        position = ([tup[1] for tup in data_summary_loc.index])
        zipped_lists = zip(position, fluorescence)
        sorted_pairs = sorted(zipped_lists)
        tuples = zip(*sorted_pairs)
        position, fluorescence = [list(tup) for tup in tuples]
        ax[i].plot(position[290:794], fluorescence[290:794], label=label, color='blue')
        ax[i].plot(position[290:794], fluorescence_blank[290:794], label=blank_label[i], color='red')
        for j, pos in enumerate(positives[i]):  # For each group now
            last_index = len(pos) - 1
            if not ((pos[0] <= 290 and pos[last_index] <= 290)
                    or (pos[0] >= 794 and pos[last_index] >= 794)):
                ax[i].axvspan(max(pos[0], 290), min(pos[last_index], 794), color='blue', alpha=0.3)
        ax[i].legend()
        ax[i].set_xlabel("position")
        ax[i].set_ylabel("fluorescence")
        ax[i].set_title("Envelope region for " + label)

    name = labels[0].split("_")
    name = name[0] + "_" + name[3]
    plt.savefig("plot/fluorescence/peaks_plot_" + name + ".png")


def makeZpepMatrix(sequence):
    """
    Constructs Zpep matrix for sequence to be used in z-scale normalization

    :param sequence: The amino acid sequence to construct for
    :return: The Zpep matrix for the sequence
    """
    seqList = list(sequence)
    AA = ["A", "C", "D", "E", "F", "G", "H", "I", "K", "L",
          "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y"]
    zScore = np.array([0.24, 0.84, 3.98, 3.11, -4.22,
                       2.05, 2.47, -3.89, 2.29, -4.28,
                       -2.85, 3.05, -1.66, 1.75, 3.52,
                       2.39, 0.75, -2.59, -4.36, -2.54,
                       -2.32, -1.67, 0.93, 0.26, 1.94,
                       -4.06, 1.95, -1.73, 0.89, -1.3,
                       -0.22, 1.62, 0.27, 0.5, 2.5,
                       -1.07, -2.18, -2.64, 3.94, 2.44,
                       0.6, 3.71, 1.93, -0.11, 1.06,
                       0.36, 0.26, -1.71, -2.49, -1.49,
                       0.47, 1.04, 1.84, -1.44, -3.5,
                       1.15, -1.12, -1.54, 0.59, 0.43,
                       -0.14, 0.18, -2.46, -3.04, 0.54,
                       -0.82, 3.9, -0.84, 1.49, -0.72,
                       1.94, -1.15, 0.7, -1.34, 1.99,
                       -1.39, -1.46, -0.85, 3.44, 0.04,
                       1.3, -2.65, 0.75, -0.25, -0.62,
                       -0.38, 0.09, 0.26, 0.31, 0.84,
                       -0.98, 1.61, 2, 0.66, -.17,
                       0.67, -0.4, -0.02, -1.59, -1.47])
    zScore = zScore.reshape(5, 20)
    zMap = {}
    for i in range(len(AA)):
        zMap[AA[i]] = zScore[:, i]

    result = zMap[seqList[0]]
    for i in range(len(seqList)):
        if i != 0:
            result = np.vstack((result, zMap[seqList[i]]))

    zscore = np.sum(result, axis=0)
    ztable = pd.DataFrame(zscore.reshape((1, 5)), index=[sequence], columns=["z1", "z2", "z3", "z4", "z5"])

    return ztable


def quantileNormalize(df_input):
    """
    Quantile normalize each sample

    :param df_input: the input dataframe
    :return: the quantile normalized dataframe
    """
    df = df_input.copy()
    # compute rank
    dic = {}
    for col in df:
        dic.update({col: sorted(df[col])})
    sorted_df = pd.DataFrame(dic)
    rank = sorted_df.mean(axis=1).tolist()
    # sort
    for col in df:
        t = np.searchsorted(np.sort(df[col]), df[col])
        df[col] = [rank[i] for i in t]
    return df


def generate_boxplots(meta_loc, data_nonNorm_loc, data_loc):
    """
    Generates boxplots of the normalized and non normalized data to show the effects of z-scale normalization

    :param meta_loc: The local copy of the metadata dataframe. Used to gather all sample ids
    :param data_nonNorm_loc: The local copy of the non normalized data to be boxplotted
    :param data_loc: The local copy of the normalized data to be boxplotted
    :return: None
    """
    # Boxplot
    # Samples List for IgG IgM
    sample_id = list(meta_loc.loc[:, 'SAMPLE_ID'])
    IgM_sample = []
    IgG_sample = []
    Blank_sample = []
    for i in sample_id:
        if 'IgG' in i:
            IgG_sample.append(i)
        if 'IgM' in i:
            IgM_sample.append(i)
        if 'Blank' in i:
            Blank_sample.append(i)
    Ig_sample = [IgM_sample, IgG_sample]

    Ig_name = ['IgM', 'IgG']
    # Plot the bar plot for selected sample to explain how data normalization perform.
    # Select sample randomly.
    # It make sense that plot here is quite different from that in previous week. Since we didn't make summary here.
    IgG_Blank = []
    IgM_Blank = []
    IgM = []
    IgG = []
    for j in range(len(Ig_name)):
        for i in Ig_sample[j]:
            if "IgG" in i and "Blank" not in i:
                IgG.append(i)
        for i in Ig_sample[j]:
            if "IgM" in i and "Blank" not in i:
                IgM.append(i)
        for i in Blank_sample:
            if "IgG_Blank_" in i:
                IgG_Blank.append(i)
        for i in Blank_sample:
            if "IgM_Blank_" in i:
                IgM_Blank.append(i)

    select_samples = random.sample(IgG, k=15) + random.sample(IgM, k=5) + random.sample(
        IgG_Blank, k=2) + random.sample(IgM_Blank, k=2)
    fig2, ax2 = plt.subplots(1, 2, figsize=(40, 20), constrained_layout=True)
    title_name = 'Signal_Boxplot'
    fig2.suptitle(title_name, fontsize=30)
    medianprops = dict(linestyle='-', linewidth=3, color='firebrick')  # Set bold median
    boxprops = dict(linestyle='-', linewidth=2.5, color='black')  # Set bold boxplot
    capprops = dict(linestyle='-', linewidth=2.5, color='black')  # Set bold cap
    whiskerprops = dict(linestyle='-', linewidth=2.5, color='black')

    ax2[0].boxplot(data_nonNorm_loc.loc[:, select_samples].to_numpy(), medianprops=medianprops
                   , boxprops=boxprops, capprops=capprops, whiskerprops=whiskerprops)
    ax2[0].set_xticklabels(select_samples, rotation=60)
    ax2[0].xaxis.set_tick_params(labelsize=30)
    ax2[0].yaxis.set_tick_params(labelsize=30)
    ax2[0].set_title("Boxplot of Non-Normalization", size=30)
    ax2[0].set_ylabel("log2(fluorescence intensity)", size=30)
    ax2[0].set_xlabel("Sample", size=30)
    ax2[1].boxplot(data_loc.loc[:, select_samples].to_numpy(), medianprops=medianprops
                   , boxprops=boxprops, capprops=capprops, whiskerprops=whiskerprops)
    ax2[1].set_xticklabels(select_samples, rotation=60)
    ax2[1].set_title("Boxplot of Z-scale-Normalization", size=30)
    ax2[1].set_ylabel("log2(fluorescence intensity)", size=30)
    ax2[1].set_xlabel("Sample", size=30)
    ax2[1].xaxis.set_tick_params(labelsize=30)
    ax2[1].yaxis.set_tick_params(labelsize=30)
    save_file_path = 'plot/boxplot/' + title_name + '_nonSummary.png'
    plt.savefig(save_file_path)
    return Ig_sample


def normalize_no_median(data_loc):
    """
    log transforms the data, then does z-score normalization.
    :param data_loc: A local copy of the input data to be normalized
    :return data: the normalized data
    :return data_nonNorm: the nonormalized data
    """
    # log transformation
    data_loc.loc[:, sample_id] = data_loc.loc[:, sample_id].apply(np.log2, axis=0)
    # Store non-normalization data
    data_nonNorm = data_loc.copy().replace([np.inf, -np.inf, np.nan], 0)
    # data_nonNorm = data_nonNorm.dropna(axis=0, subset=subset_array).reset_index()
    Probe_List = data_nonNorm.loc[:, "PROBE_SEQUENCE"]
    print("[INFO]\tNormalizing...")

    # Z score normalization implementation
    zlist = list(np.array(makeZpepMatrix(Probe_List[0]))[0])
    zlist.insert(0, 1)
    zlist = np.array(zlist)
    for i in range(len(Probe_List)):
        if i > 0:
            temp = list(np.array(makeZpepMatrix(Probe_List[i]))[0])
            temp.insert(0, 1)
            zlist = np.vstack((zlist, temp))

    print("[INFO]\tLinear regression...")
    # for k in sample_all:
    for i, j in enumerate(sample_id):
        y = np.array(data_nonNorm.loc[:, j])
        x = zlist
        reg = LinearRegression().fit(x, y)  # Find regression line
        residuals = np.array(data_nonNorm.loc[:, j] - (np.dot(zlist, reg.coef_) + reg.intercept_))  # Get residuals

        n = len(Probe_List)
        for i in range(10):
            RSS = np.sum(np.square(residuals))
            w = (4 + 1) / (4 + (n / RSS) * np.square(residuals))  # Scale t-distribution w/ 4 degrees of freedom
            reg = LinearRegression().fit(x, y,
                                         w)  # Fit linear regression model to relationship between the z-scores, data,
            # and t-distribution
            residuals = np.array(data_nonNorm.loc[:, j] - (np.dot(zlist, reg.coef_) + reg.intercept_))  # Get residuals
        data_nonNorm.loc[:, j] = np.array(residuals)  # Get array of residuals

    data_full = data_nonNorm.copy()
    return data_full, data_nonNorm


def normalize(data_loc):
    """
    log transforms the data, then does z-score normalization.
    :param data_loc: A local copy of the input data to be normalized
    :return data: the normalized data
    :return data_nonNorm: the nonormalized data
    """
    # log transformation
    data_loc.loc[:, sample_id] = data_loc.loc[:, sample_id].apply(np.log2, axis=0)
    # Store non-normalization data
    data_nonNorm = data_loc.copy()
    data_nonNorm_summary = data_nonNorm.groupby(['PROBE_SEQUENCE', 'POSITION'], as_index=False)[sample_id].median()
    Probe_List = data_nonNorm_summary.loc[:, "PROBE_SEQUENCE"]
    data = data_loc.reset_index()
    data = data.groupby(['PROBE_SEQUENCE', 'POSITION'])[sample_id].median()
    print(data)
    print("[INFO]\tNormalizing...")

    # Z score normalization implementation
    zlist = list(np.array(makeZpepMatrix(Probe_List[0]))[0])
    zlist.insert(0, 1)
    zlist = np.array(zlist)
    for i in range(len(Probe_List)):
        if i > 0:
            temp = list(np.array(makeZpepMatrix(Probe_List[i]))[0])
            temp.insert(0, 1)
            zlist = np.vstack((zlist, temp))

    print("[INFO]\tLinear regression...")
    # for k in sample_all:
    for i, j in enumerate(sample_id):
        y = np.array(data.loc[:, j])
        x = zlist
        reg = LinearRegression().fit(x, y)  # Find regression line
        residuals = np.array(data.loc[:, j] - (np.dot(zlist, reg.coef_) + reg.intercept_))  # Get residuals

        n = len(Probe_List)
        for i in range(10):
            RSS = np.sum(np.square(residuals))
            w = (4 + 1) / (4 + (n / RSS) * np.square(residuals))  # Scale t-distribution w/ 4 degrees of freedom
            reg = LinearRegression().fit(x, y,
                                         w)  # Fit linear regression model to relationship between the z-scores, data,
            # and t-distribution
            residuals = np.array(data.loc[:, j] - (np.dot(zlist, reg.coef_) + reg.intercept_))  # Get residuals
        data.loc[:, j] = np.array(residuals)  # Get array of residuals

    data_full = data.copy()
    data_full.to_csv("data/dataframe.csv")
    print("Saved.")
    return data_full, data_nonNorm


if __name__ == "__main__":
    # Load Data
    print("[INFO]\tLoading data...")
    IgG = pd.read_csv('data/data/IgG/Processed_aggregate_data.txt', sep='\t')
    IgM = pd.read_csv('data/data/IgM/Processed_aggregate_data.txt', sep='\t')

    # Load Config
    meta = pd.read_csv('metadata/Metadata.csv')
    protein_lookup = pd.read_csv('data/data/200603_UW_Mohr_ZIKV_DENV_PEP_QX12_correspondence_key.txt', header=1)
    protein_lookup = protein_lookup.iloc[1:, :]

    # ZiKa virus AAV34151
    IgG_ZiKa = IgG[IgG['SEQ_ID'] == 'AAV34151']
    IgM_ZiKa = IgM[IgM['SEQ_ID'] == 'AAV34151']

    # Join the two datasets
    Ig_ZiKa = pd.merge(IgG_ZiKa, IgM_ZiKa, how='inner')
    # Get all sample IDS
    sample_id = list(meta.loc[:, 'SAMPLE_ID'])
    IgG_sample = []
    IgM_sample = []
    Blank_sample = []
    # Split them into day 1 and day 2
    for i in sample_id:
        if 'IgG' in i:
            IgG_sample.append(i)
        if 'IgM' in i:
            IgM_sample.append(i)

    # Change the name of part of the sample
    Data_Sample_dict = {}
    data_id = meta.loc[:, 'DATA_ID'].tolist()
    sample_id = meta.loc[:, 'SAMPLE_ID'].tolist()
    for i in range(len(data_id)):  # Change names of vars
        Data_Sample_dict[data_id[i]] = sample_id[i]
    Ig_ZiKa = Ig_ZiKa.rename(columns=Data_Sample_dict)  # rename the cols
    for col in Ig_ZiKa.columns:  # drop unnecessary columns
        if ".dat" in col:
            Ig_ZiKa = Ig_ZiKa.drop(col, axis=1)
    # Analysis for Corr Data
    # Read Data
    data = Ig_ZiKa.copy()
    print(data)
    meta = pd.read_csv('metadata/Metadata.csv', dtype='object')
    # Build subtraction data set
    sample_id = list(meta.loc[:, 'SAMPLE_ID'])

    # Probe Sequence
    Probe_List = data.loc[:, "PROBE_SEQUENCE"]
    # Normalize grouped and non grouped data
    data_summary, data_nonNorm = normalize(data)
    data_norm, data_non_norm = normalize_no_median(data)
    Ig_sample = generate_boxplots(meta, data_nonNorm, data_summary)
    labels = data.columns[17:]
    data_summary = data_summary.groupby(['POSITION', 'PROBE_SEQUENCE']).apply(pd.DataFrame)
    data_summary = data_summary.sort_index(level=1)
    # Remove background signal
    for label in labels:
        blank_sample = meta.where(meta["SAMPLE_ID"] == label)
        blank_sample = blank_sample.dropna(axis=0)
        blank_sample = blank_sample.reset_index()["BG_SUBTRACT"]
        if "BLANK" in blank_sample[0]:
            continue
        data_temp = data_summary.loc[:, label]
        blank_sample_data = data_summary.loc[:, blank_sample[0]]
        # Baseline Correction
        temp = data_temp - blank_sample_data
        data_summary.loc[:, label] = temp

    Ig_name = ['IgG', 'IgM']
    Dmin_dict = positivity_calls(data_summary, Ig_name, Ig_sample)
    dict_of_pos = {}
    for Ig in Dmin_dict:
        for label in Ig:
            data_temp = data_summary.groupby(['POSITION', 'PROBE_SEQUENCE'])[label].apply(list)
            positives = []
            for i, row in enumerate(data_temp):
                if row[0] > Ig[label]:
                    positives.append(int(data_temp.index[i][0]))
            dict_of_pos[label] = positives

    # Remove overlapping positives from 0 DPI sample
    for pos in dict_of_pos:
        blank_sample = meta.where(meta["SAMPLE_ID"] == pos)
        blank_sample = blank_sample.dropna(axis=0)
        blank_sample = blank_sample.reset_index()["COMPARE_TO"]
        blank_key = blank_sample[0]
        if blank_key == "BLANK":
            continue
        blank_pos = dict_of_pos[blank_key]
        dpi_pos = dict_of_pos[pos]
        new_pos = list((Counter(dpi_pos) - Counter(blank_pos)).elements())
        dict_of_pos[pos] = new_pos
        sep_by_animal = {}

    for key in dict_of_pos:
        if "Blank" in key:
            continue
        animal = key.split("_")[3] + "_" + key.split("_")[0]
        dpi = key.split("_")[1]
        if animal in sep_by_animal:
            sep_by_animal[animal][dpi] = dict_of_pos[key]
        else:
            sep_by_animal[animal] = {dpi: dict_of_pos[key]}

    #  Plot positivity
    fig, axs = plt.subplots(2, len(sep_by_animal) // 2)
    axs = axs.ravel()
    for i, animal in enumerate(sep_by_animal):
        dpis = []
        num_pos = []
        for dpi in sep_by_animal[animal]:
            dpis.append(int(dpi))
            num_pos.append(len(sep_by_animal[animal][dpi]))

        zipped_lists = zip(dpis, num_pos)
        sorted_pairs = sorted(zipped_lists)

        tuples = zip(*sorted_pairs)
        dpis, num_pos = [list(tup) for tup in tuples]
        axs[i].bar(dpis, num_pos)
        axs[i].set_xlabel("DPI")
        axs[i].set_ylabel("Number of positive positions")
        axs[i].set_title(animal)

    fig.set_size_inches(40, 10.5)
    fig.tight_layout(pad=3.0)
    plt.savefig("plot/positivity/positive_positions.png")

    fig, axs = plt.subplots(2, len(sep_by_animal) // 2)
    axs = axs.ravel()
    breadth_by_animal = {}
    IgG_epitopes_dpi_1 = []
    IgG_epitopes_dpi_2 = []
    IgM_epitopes_dpi_1 = []
    IgM_epitopes_dpi_2 = []
    epitope_counts = {}
    for i, animal in enumerate(sep_by_animal):
        dpis = []
        epitope_count = []
        ranges = []
        breadth_by_animal[animal] = {}
        for dpi in sep_by_animal[animal]:
            epitopes = []
            if int(dpi) < 10:
                continue
            positives = sep_by_animal[animal][dpi]
            # Group by error from index (i.e. when x - i(x) changes, then we know our deviation is > 1)
            count = 0
            for k, g in groupby(enumerate(sep_by_animal[animal][dpi]), lambda x: x[0] - x[1]):
                group = list(g)
                epitopes.append(tuple(np.transpose(group)[1]))
            dpis.append(dpi)
            epitopes = list(dict.fromkeys(epitopes))
            if "IgG" in animal:
                if int(dpi) < 50:
                    IgG_epitopes_dpi_1.append(len(epitopes))
                else:
                    IgG_epitopes_dpi_2.append(len(epitopes))
            else:
                if int(dpi) < 20:
                    IgM_epitopes_dpi_1.append(len(epitopes))
                else:
                    IgM_epitopes_dpi_2.append(len(epitopes))

            epitope_count.append(len(epitopes))
            breadth_by_animal[animal][dpi] = epitopes
        epitope_counts[str(animal)] = (dpis, epitope_count)
        axs[i].bar(dpis, epitope_count)
        axs[i].set_xlabel("DPI")
        axs[i].set_ylabel("Breadth of response")
        axs[i].set_title(animal)



    fig.set_size_inches(40, 10.5)
    fig.tight_layout(pad=3.0)
    plt.savefig("plot/positivity/positive_groups.png")
    plt.close("all")
    fig, ax = plt.subplots(2)
    IgG_counts_dpi_1 = []
    IgG_counts_dpi_2 = []
    IgM_counts_dpi_1 = []
    IgM_counts_dpi_2 = []
    for count in epitope_counts:
        dpis = epitope_counts[count][0]
        counts = epitope_counts[count][1]
        if "IgG" in count:
            # ax[0].plot(["27-31 DPI", "108-135 DPI"], counts, label=count)
            IgG_counts_dpi_1.append(counts[0])
            IgG_counts_dpi_2.append(counts[1])
        else:
            # ax[1].plot(["13-17 DPI", "21-24 DPI"], counts, label=count)
            IgM_counts_dpi_1.append(counts[0])
            IgM_counts_dpi_2.append(counts[1])
    ax[0].set_ylabel("Number of Epitopes")
    ax[0].set_xlabel("DPI")
    ax[0].set_title("Breadth of IgG Response")
    # ax[0].legend(bbox_to_anchor=(1.05, 1))
    ax[1].set_ylabel("Number of Epitopes")
    ax[1].set_xlabel("DPI")
    ax[1].set_title("Breadth of IgM Response")
    # ax[1].legend(bbox_to_anchor=(1.05, 1))
    ax[0].plot(["27-31 DPI", "108-135 DPI"], [np.mean(IgG_counts_dpi_1), np.mean(IgG_counts_dpi_2)], color="red")
    ax[0].bar(["27-31 DPI", "108-135 DPI"], [np.mean(IgG_counts_dpi_1), np.mean(IgG_counts_dpi_2)], width=0.8)
    ax[1].plot(["13-17 DPI", "21-24 DPI"], [np.mean(IgM_counts_dpi_1), np.mean(IgM_counts_dpi_2)], color="red")
    ax[1].bar(["13-17 DPI", "21-24 DPI"], [np.mean(IgM_counts_dpi_1), np.mean(IgM_counts_dpi_2)], width=0.8)
    fig.set_size_inches(10, 15)
    plt.savefig("plot/positivity/positive_groups_single_line.png")
    plt.close("all")
    data_summary = data_summary.sort_index(level=1)
    plot_positivity(data_summary,
                    ["IgG_27_dpi_044-117", "IgG_126_dpi_044-117"],
                    [breadth_by_animal["044-117_IgG"]["27"],
                     breadth_by_animal["044-117_IgG"]["126"]],
                    ["IgG_0_dpi_044-117", "IgG_0_dpi_044-117"])
    plot_positivity(data_summary,
                    ["IgG_31_dpi_044-103", "IgG_114_dpi_044-103"],
                    [breadth_by_animal["044-103_IgG"]["31"],
                     breadth_by_animal["044-103_IgG"]["114"]],
                    ["IgG_-6_dpi_044-103", "IgG_-6_dpi_044-103"])

    proteins = {
        (0, 123): "Flavi_capsid",
        (124, 214): "Premembrane",
        (215, 289): "Membrane_protein",
        (290, 794): "Envelope",
        (795, 1156): "NS1",
        (1157, 1374): "NS2A",
        (1375, 1519): "NS2B",
        (1519, 2121): "NS3",
        (2122, 2268): "NS4A",
        (2269, 2770): "NS4B",
        (2771, 3423): "NS5"
    }
    pos_count_by_protein = {
        "IgG": {
            "Flavi_capsid": {1: [], 2: [], 3: []},
            "Premembrane": {1: [], 2: [], 3: []},
            "Membrane_protein": {1: [], 2: [], 3: []},
            "Envelope": {1: [], 2: [], 3: []},
            "NS1": {1: [], 2: [], 3: []},
            "NS2A": {1: [], 2: [], 3: []},
            "NS2B": {1: [], 2: [], 3: []},
            "NS3": {1: [], 2: [], 3: []},
            "NS4A": {1: [], 2: [], 3: []},
            "NS4B": {1: [], 2: [], 3: []},
            "NS5": {1: [], 2: [], 3: []}
        },
        "IgM": {
            "Flavi_capsid": {1: [], 2: [], 3: []},
            "Premembrane": {1: [], 2: [], 3: []},
            "Membrane_protein": {1: [], 2: [], 3: []},
            "Envelope": {1: [], 2: [], 3: []},
            "NS1": {1: [], 2: [], 3: []},
            "NS2A": {1: [], 2: [], 3: []},
            "NS2B": {1: [], 2: [], 3: []},
            "NS3": {1: [], 2: [], 3: []},
            "NS4A": {1: [], 2: [], 3: []},
            "NS4B": {1: [], 2: [], 3: []},
            "NS5": {1: [], 2: [], 3: []}
        }
    }

    build_directories(breadth_by_animal.keys())
    for animal in breadth_by_animal:
        for dpi in breadth_by_animal[animal]:
            animal_sep = animal.split("_")
            animal_id = animal_sep[0]
            antibody_id = animal_sep[1]
            fig, axs = plt.subplots()
            temp_count_lookup = {
                "Flavi_capsid": 0,
                "Premembrane": 0,
                "Membrane_protein": 0,
                "Envelope": 0,
                "NS1": 0,
                "NS2A": 0,
                "NS2B": 0,
                "NS3": 0,
                "NS4A": 0,
                "NS4B": 0,
                "NS5": 0
            }
            positives = breadth_by_animal[animal][dpi]  # List of lists of positions in groups
            for pos in positives:  # Pos is a subarray!
                last_index = len(pos) - 1

                # The if statements check if the group intersects with the region on the protein
                if not ((pos[0] < 1 and pos[last_index] < 1)
                        or (pos[0] > 124 and pos[last_index] > 124)):
                    temp_count_lookup['Flavi_capsid'] += 1
                if not ((pos[0] < 125 and pos[last_index] < 125)
                        or (pos[0] > 215 and pos[last_index] > 215)):  # 125, 215
                    temp_count_lookup['Premembrane'] += 1
                if not ((pos[0] < 216 and pos[last_index] < 216)
                        or (pos[0] > 290 and pos[last_index] > 290)):  # 216, 290
                    temp_count_lookup['Membrane_protein'] += 1
                if not ((pos[0] < 291 and pos[last_index] < 291)
                        or (pos[0] > 795 and pos[last_index] > 795)):  # 291, 795
                    temp_count_lookup['Envelope'] += 1
                if not ((pos[0] < 796 and pos[last_index] < 796)
                        or (pos[0] > 1157 and pos[last_index] > 1157)):  # 796, 1157
                    temp_count_lookup['NS1'] += 1
                if not ((pos[0] < 1158 and pos[last_index] < 1158)
                        or (pos[0] > 1375 and pos[last_index] > 1375)):  # 1158, 1375
                    temp_count_lookup['NS2A'] += 1
                if not ((pos[0] < 1376 and pos[last_index] < 1376)
                        or (pos[0] > 1519 and pos[last_index] > 1519)):  # 1376, 1519
                    temp_count_lookup['NS2B'] += 1
                if not ((pos[0] < 1520 and pos[last_index] < 1520)
                        or (pos[0] > 2122 and pos[last_index] > 2122)):  # 1520, 2122
                    temp_count_lookup['NS3'] += 1
                if not ((pos[0] < 2123 and pos[last_index] < 2123)
                        or (pos[0] > 2269 and pos[last_index] > 2269)):  # 2123, 2269
                    temp_count_lookup['NS4A'] += 1
                if not ((pos[0] < 2270 and pos[last_index] < 2270)
                        or (pos[0] > 2771 and pos[last_index] > 2771)):  # 2270, 2771
                    temp_count_lookup['NS4B'] += 1
                if not ((pos[0] < 2772 and pos[last_index] < 2772)
                        or (pos[0] > 3424 and pos[last_index] > 3424)):  # 2772, 3424
                    temp_count_lookup['NS5'] += 1
            dpi_idx = 0
            if int(dpi) < 10:
                dpi_idx = 1
            elif (int(dpi) < 20 and "IgM" in animal) or (int(dpi) < 50 and "IgG" in animal):
                dpi_idx = 2
            else:
                dpi_idx = 3
            if "IgG" in animal:
                pos_count_by_protein["IgG"]["Flavi_capsid"][dpi_idx].append(temp_count_lookup["Flavi_capsid"])
                pos_count_by_protein["IgG"]["Premembrane"][dpi_idx].append(temp_count_lookup["Premembrane"])
                pos_count_by_protein["IgG"]["Membrane_protein"][dpi_idx].append(temp_count_lookup["Membrane_protein"])
                pos_count_by_protein["IgG"]["Envelope"][dpi_idx].append(temp_count_lookup["Envelope"])
                pos_count_by_protein["IgG"]["NS1"][dpi_idx].append(temp_count_lookup["NS1"])
                pos_count_by_protein["IgG"]["NS2A"][dpi_idx].append(temp_count_lookup["NS2A"])
                pos_count_by_protein["IgG"]["NS2B"][dpi_idx].append(temp_count_lookup["NS2B"])
                pos_count_by_protein["IgG"]["NS3"][dpi_idx].append(temp_count_lookup["NS3"])
                pos_count_by_protein["IgG"]["NS4A"][dpi_idx].append(temp_count_lookup["NS4A"])
                pos_count_by_protein["IgG"]["NS4B"][dpi_idx].append(temp_count_lookup["NS4B"])
                pos_count_by_protein["IgG"]["NS5"][dpi_idx].append(temp_count_lookup["NS5"])
            else:
                pos_count_by_protein["IgM"]["Flavi_capsid"][dpi_idx].append(temp_count_lookup["Flavi_capsid"])
                pos_count_by_protein["IgM"]["Premembrane"][dpi_idx].append(temp_count_lookup["Premembrane"])
                pos_count_by_protein["IgM"]["Membrane_protein"][dpi_idx].append(temp_count_lookup["Membrane_protein"])
                pos_count_by_protein["IgM"]["Envelope"][dpi_idx].append(temp_count_lookup["Envelope"])
                pos_count_by_protein["IgM"]["NS1"][dpi_idx].append(temp_count_lookup["NS1"])
                pos_count_by_protein["IgM"]["NS2A"][dpi_idx].append(temp_count_lookup["NS2A"])
                pos_count_by_protein["IgM"]["NS2B"][dpi_idx].append(temp_count_lookup["NS2B"])
                pos_count_by_protein["IgM"]["NS3"][dpi_idx].append(temp_count_lookup["NS3"])
                pos_count_by_protein["IgM"]["NS4A"][dpi_idx].append(temp_count_lookup["NS4A"])
                pos_count_by_protein["IgM"]["NS4B"][dpi_idx].append(temp_count_lookup["NS4B"])
                pos_count_by_protein["IgM"]["NS5"][dpi_idx].append(temp_count_lookup["NS5"])
            table_data_temp = []
            for key in temp_count_lookup:
                table_data_temp.append([key, temp_count_lookup[key]])
            fig, ax = plt.subplots()
            ax.set_axis_off()
            ax.table(table_data_temp, loc='upper left')
            ax.set_title("Epitopes per Protein for " + animal_id + " - " + antibody_id + "_" + dpi)
            plt.savefig("tables/" + animal_id + "/" + antibody_id + "/epitope_by_protein_" + dpi + ".png")
            plt.close("all")

    table_data_dpi_1 = []
    table_data_dpi_2 = []
    table_data_dpi_3 = []

    for protein in pos_count_by_protein["IgG"]:
        count_dpi_1 = round(float(np.mean(pos_count_by_protein["IgG"][protein][1])), 3)
        table_data_dpi_1.append([protein, count_dpi_1])
        count_dpi_2 = round(float(np.mean(pos_count_by_protein["IgG"][protein][2])), 3)
        table_data_dpi_2.append([protein, count_dpi_2])
        count_dpi_3 = round(float(np.mean(pos_count_by_protein["IgG"][protein][3])), 3)
        table_data_dpi_3.append([protein, count_dpi_3])

    fig3, ax = plt.subplots()
    ax.set_axis_off()
    ax.table(table_data_dpi_2, loc='upper left')
    ax.set_title("Mean Value of Epitopes per Protein at 2nd DPI")
    plt.savefig("tables/median_epitope_by_protein_2nd_dpi_IgG.png")

    fig3, ax = plt.subplots()
    ax.set_axis_off()
    ax.table(table_data_dpi_3, loc='upper left')
    ax.set_title("Mean Value of Epitopes per Protein at 3rd DPI")
    plt.savefig("tables/median_epitope_by_protein_3rd_dpi_IgG.png")

    table_data_dpi_1 = []
    table_data_dpi_2 = []
    table_data_dpi_3 = []
    for protein in pos_count_by_protein["IgM"]:
        count_dpi_1 = round(float(np.mean(pos_count_by_protein["IgM"][protein][1])), 3)
        table_data_dpi_1.append([protein, count_dpi_1])
        count_dpi_2 = round(float(np.mean(pos_count_by_protein["IgM"][protein][2])), 3)
        table_data_dpi_2.append([protein, count_dpi_2])
        count_dpi_3 = round(float(np.mean(pos_count_by_protein["IgM"][protein][3])), 3)
        table_data_dpi_3.append([protein, count_dpi_3])

    fig3, ax = plt.subplots()
    ax.set_axis_off()
    ax.table(table_data_dpi_2, loc='upper left')
    ax.set_title("Mean Value of Epitopes per Protein at 2nd DPI")
    plt.savefig("tables/median_epitope_by_protein_2nd_dpi_IgM.png")

    fig3, ax = plt.subplots()
    ax.set_axis_off()
    ax.table(table_data_dpi_3, loc='upper left')
    ax.set_title("Mean Value of Epitopes per Protein at 3rd DPI")
    plt.savefig("tables/median_epitope_by_protein_3rd_dpi_IgM.png")
