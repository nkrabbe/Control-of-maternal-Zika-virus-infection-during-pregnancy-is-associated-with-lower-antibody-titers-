from itertools import groupby
from operator import itemgetter

import pandas as pd
from sklearn.linear_model import LinearRegression
import numpy as np
import random
import matplotlib.pyplot as plt
import scipy.stats as stats
import os
import statsmodels.stats.multitest as multitest

def positivity_calls(data_sort, Ig_name, Ig_sample):
    Dmin_dict = []
    Dmin_list = []
    T_dict = {}
    for q in range(len(Ig_name)):
        Dmin_list_temp = []
        temp_sample = Ig_sample[q]
        seqY_list = {}

        sample_dict = {}
        #     for j in range(len(temp_sample)): #sampleList

        #     I = data_sort.loc[:,temp_sample]
        I = data_sort.loc[:, temp_sample]

        # Generate Threshold
        min_num = np.min(np.abs(I.to_numpy()))
        max_num = np.max(np.abs(I.to_numpy()))

        seq_Threshold = list(np.arange(np.min(I.to_numpy()), np.max(I.to_numpy()), 0.01))

        seqY = np.arange(min_num, max_num, 0.01)
        seqY = list(seqY)
        seqY[0] = min_num
        #     seqY_list[temp_sample[j]]=seqY.reverse()

        # Generate FDR
        seq_list = [tup[0] for tup in data_sort.index]
        position = []
        for i in range(len(seq_list)):
            startPos = i
            endPos = i + len(seq_list[i])
            midpoint = round((startPos + endPos) / 2)
            position.append(midpoint)
            if midpoint > 3400:
                break
        c = position

        FDR = []
        n = 0

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
        for j in range(len(temp_sample)):
        #     print(temp_sample[j])
        #     print(Ig_name[q])
            sample_dict[temp_sample[j]] = Dmin
        #     print(Dmin)
        #     print(" ")
        Dmin_dict.append(sample_dict)
        Dmin_list.append(Dmin_list_temp)
    return Dmin_dict

def build_directories(animal_ids):
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
        if not os.path.exists("tables/"+animal_id):
            os.makedirs("tables/" + animal_id)
        if not os.path.exists("tables/"+animal_id + "/IgG"):
            os.makedirs("tables/" + animal_id + "/IgG")
        if not os.path.exists("tables/" + animal_id + "/IgM"):
            os.makedirs("tables/" + animal_id + "/IgM")

def plot_positivity(data_summary_loc, labels, positives, blank_label):
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
        ax[i].plot(position[1376:1519], fluorescence[1376:1519], label=label, color='blue')
        ax[i].plot(position[1376:1519], fluorescence_blank[1376:1519], label=blank_label[i], color='red')
        last_x = 1376
        for j, pos in enumerate(positives[i]):
            if 1376 <= pos <= 1519:
                if j == len(positives[i]) - 1:
                    break
                if positives[i][j+1] - positives[i][j] > 1:
                    ax[i].axvspan(last_x, positives[i][j], color='blue', alpha=0.3)
                    last_x = positives[i][j+1]

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
    subset_array = ['PROBE_SEQUENCE', 'POSITION',
                    'X', 'Y', 'IgG_Blank_703577', 'IgG_0_dpi_044-118',
                    'IgG_-6_dpi_044-103', 'IgG_0_dpi_044-114', 'IgG_-6_dpi_044-104',
                    'IgG_0_dpi_044-117', 'IgG_0_dpi_044-116', 'IgG_0_dpi_044-112',
                    'IgG_0_dpi_044-109', 'IgG_0_dpi_044-102', 'IgG_0_dpi_044-110',
                    'IgG_0_dpi_044-101', 'IgG_28_dpi_044-101', 'IgG_28_dpi_044-102',
                    'IgG_27_dpi_044-112', 'IgG_27_dpi_044-117', 'IgG_28_dpi_044-114',
                    'IgG_29_dpi_044-118', 'IgG_27_dpi_044-110', 'IgG_28_dpi_044-109',
                    'IgG_29_dpi_044-116', 'IgG_29_dpi_044-104', 'IgG_31_dpi_044-103',
                    'IgG_Blank_702923', 'IgG_11_dpi_044-101', 'IgG_11_dpi_044-102',
                    'IgG_12_dpi_044-112', 'IgG_12_dpi_044-117', 'IgG_13_dpi_044-114',
                    'IgG_13_dpi_044-118', 'IgG_13_dpi_044-110', 'IgG_10_dpi_044-109',
                    'IgG_13_dpi_044-116', 'IgG_11_dpi_044-104', 'IgG_11_dpi_044-103',
                    'IgG_Blank_702501', 'IgM_0_dpi_044-101', 'IgM_0_dpi_044-102',
                    'IgM_0_dpi_044-112', 'IgM_0_dpi_044-117', 'IgM_0_dpi_044-114',
                    'IgM_0_dpi_044-118', 'IgM_0_dpi_044-110', 'IgM_0_dpi_044-109',
                    'IgM_0_dpi_044-116', 'IgM_-6_dpi_044-104', 'IgM_-6_dpi_044-103',
                    'IgM_Blank_702594', 'IgM_17_dpi_044-101', 'IgM_15_dpi_044-102',
                    'IgM_16_dpi_044-112', 'IgM_16_dpi_044-117', 'IgM_14_dpi_044-114',
                    'IgM_15_dpi_044-118', 'IgM_13_dpi_044-110', 'IgM_14_dpi_044-109',
                    'IgM_13_dpi_044-116', 'IgM_15_dpi_044-104', 'IgM_15_dpi_044-103',
                    'IgM_Blank_702562', 'IgM_21_dpi_044-101', 'IgM_24_dpi_044-102',
                    'IgM_23_dpi_044-112', 'IgM_23_dpi_044-117', 'IgM_21_dpi_044-114',
                    'IgM_22_dpi_044-118', 'IgM_23_dpi_044-110', 'IgM_21_dpi_044-109',
                    'IgM_22_dpi_044-116', 'IgM_22_dpi_044-104', 'IgM_22_dpi_044-103',
                    'IgM_Blank_703409']
    data_nonNorm = data_loc.copy().replace([np.inf, -np.inf, np.nan], 0)
    # data_nonNorm = data_nonNorm.dropna(axis=0, subset=subset_array).reset_index()
    data_nonNorm_summary = data_nonNorm.groupby(['PROBE_SEQUENCE', 'POSITION'], as_index=False)[sample_id].median()
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
            reg = LinearRegression().fit(x, y, w)  # Fit linear regression model to relationship between the z-scores, data,
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
    data_nonNorm_summary = data_nonNorm.groupby(['PROBE_SEQUENCE', 'POSITION'], as_index=False)[sample_id].mean()
    Probe_List = data_nonNorm_summary.loc[:, "PROBE_SEQUENCE"]
    data = data_loc.reset_index()
    data = data.groupby(['PROBE_SEQUENCE', 'POSITION'])[sample_id].mean()
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
    # virus_grouping = pd.read_csv('metadata/21298_VIRUS_GROUPING_LOOKUP.CSV')

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
    meta = pd.read_csv('metadata/Metadata.csv', dtype='object')
    # Build subtraction data set
    sample_id = list(meta.loc[:, 'SAMPLE_ID'])

    # Probe Sequence
    Probe_List = data.loc[:, "PROBE_SEQUENCE"]
    # Normalize grouped and non grouped data
    data_summary, data_nonNorm = normalize(data)
    data_norm, data_non_norm = normalize_no_median(data)
    # Generate box plots
    Ig_sample = generate_boxplots(meta, data_nonNorm, data_summary)
    Ig_name = ['IgG', 'IgM']
    # Dmin_dict = positivity_calls(data_summary.sort_index(level=1), Ig_name, Ig_sample)

    dict_of_pos = {}
    # Eliminate columns that are metadata
    labels = data.columns[17:]
    # Use anova to determine positivity:
    blank_dict = {}
    print("[INFO]\tPositivity Calls...")
    for key in labels.tolist():
        blank_sample = meta.where(meta["SAMPLE_ID"] == key)
        blank_sample = blank_sample.dropna(axis=0)
        blank_sample = blank_sample.reset_index()["COMPARE_TO"]
        if blank_sample[0] == "BLANK":
            continue
        data_temp = data_norm.groupby(['POSITION', 'PROBE_SEQUENCE'])[key].apply(list)
        blank_sample_data = data_norm.groupby(['POSITION', 'PROBE_SEQUENCE'])[blank_sample[0]].apply(list)
        p_vals = []
        for i, row in enumerate(data_temp):
            # Use one way t-test with bonferroni post hoc correction
            f, p = stats.f_oneway(row, blank_sample_data.loc(axis=0)[data_temp.index[i]])
            p_vals.append(p)
        rejected, p_vals, alphaSidak, alphaBonf = multitest.multipletests(p_vals, alpha=0.05, method='bonferroni')
        for i, p in enumerate(p_vals):
            # if p < 0.05 and data_temp[i].mean() > blank_sample_data.loc(axis=0)[data_temp.index[i]].mean():
            if p < 0.05:
                if key in dict_of_pos:
                    dict_of_pos[key].append(int(data_temp.index[i][0]))
                else:
                    dict_of_pos[key] = [int(data_temp.index[i][0])]
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

    # Plot positivity
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
    for i, animal in enumerate(sep_by_animal):
        dpis = []
        epitope_count = []
        ranges = []
        for dpi in sep_by_animal[animal]:
            count = 0
            for k, g in groupby(enumerate(sep_by_animal[animal][dpi]), lambda x: x[0] - x[1]):
                count += 1
                group = (map(itemgetter(1), g))
            dpis.append(dpi)
            epitope_count.append(count)

        axs[i].bar(dpis, epitope_count)
        axs[i].set_xlabel("DPI")
        axs[i].set_ylabel("Breadth of response")
        axs[i].set_title(animal)

    fig.set_size_inches(40, 10.5)
    fig.tight_layout(pad=3.0)
    plt.savefig("plot/positivity/positive_groups.png")
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
            positives = breadth_by_animal[animal][dpi]
            for pos in positives:
                pos += 1
                if 1 <= pos <= 124:
                    temp_count_lookup['Flavi_capsid'] += 1
                elif 125 <= pos <= 215:
                    temp_count_lookup['Premembrane'] += 1
                elif 216 <= pos <= 290:
                    temp_count_lookup['Membrane_protein'] += 1
                elif 291 <= pos <= 795:
                    temp_count_lookup['Envelope'] += 1
                elif 796 <= pos <= 1157:
                    temp_count_lookup['NS1'] += 1
                elif 1158 <= pos <= 1375:
                    temp_count_lookup['NS2A'] += 1
                elif 1376 <= pos <= 1519:
                    temp_count_lookup['NS2B'] += 1
                elif 1520 <= pos <= 2122:
                    temp_count_lookup['NS3'] += 1
                elif 2123 <= pos <= 2269:
                    temp_count_lookup['NS4A'] += 1
                elif 2270 <= pos <= 2771:
                    temp_count_lookup['NS4B'] += 1
                elif 2772 <= pos <= 3424:
                    temp_count_lookup['NS5'] += 1
            dpi_idx = 0
            if int(dpi) < 10:
                dpi_idx = 1
            elif int(dpi) < 20:
                dpi_idx = 2
            else:
                dpi_idx = 3
            pos_count_by_protein["Flavi_capsid"][dpi_idx].append(temp_count_lookup["Flavi_capsid"])
            pos_count_by_protein["Premembrane"][dpi_idx].append(temp_count_lookup["Premembrane"])
            pos_count_by_protein["Membrane_protein"][dpi_idx].append(temp_count_lookup["Membrane_protein"])
            pos_count_by_protein["Envelope"][dpi_idx].append(temp_count_lookup["Envelope"])
            pos_count_by_protein["NS1"][dpi_idx].append(temp_count_lookup["NS1"])
            pos_count_by_protein["NS2A"][dpi_idx].append(temp_count_lookup["NS2A"])
            pos_count_by_protein["NS2B"][dpi_idx].append(temp_count_lookup["NS2B"])
            pos_count_by_protein["NS3"][dpi_idx].append(temp_count_lookup["NS3"])
            pos_count_by_protein["NS4A"][dpi_idx].append(temp_count_lookup["NS4A"])
            pos_count_by_protein["NS4B"][dpi_idx].append(temp_count_lookup["NS4B"])
            pos_count_by_protein["NS5"][dpi_idx].append(temp_count_lookup["NS5"])
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
    for protein in pos_count_by_protein:
        count_dpi_1 = np.median(pos_count_by_protein[protein][1])
        table_data_dpi_1.append([protein, count_dpi_1])
        count_dpi_2 = np.median(pos_count_by_protein[protein][2])
        table_data_dpi_2.append([protein, count_dpi_2])
        count_dpi_3 = np.median(pos_count_by_protein[protein][3])
        table_data_dpi_3.append([protein, count_dpi_3])

    fig3, ax = plt.subplots()
    ax.set_axis_off()
    ax.table(table_data_dpi_2, loc ='upper left')
    ax.set_title("Median Value of Epitopes per Protein")
    plt.savefig("tables/median_epitope_by_protein.png")
