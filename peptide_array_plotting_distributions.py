import pandas as pd
import matplotlib.pyplot as plt


def plot_distributions(dframe: pd.DataFrame, labels: list) -> None:
    fig, axs = plt.subplots(9, 8)
    fig.set_size_inches(50, 50)
    title_dict = {'fontsize': 10}
    for i, label in enumerate(labels):
        row = i // 8
        col = i % 8
        arr = dframe[label]
        label_arr = label.split("_")
        if len(label_arr) == 4:
            dpi = label_arr[1]
            animal = label_arr[3]
            antibody = label_arr[0]
            title = antibody + " " + animal + " " + dpi + " dpi"
        else:
            slide_no = label_arr[2]
            title = "Blank Sample for Slide " + slide_no
        axs[row][col].hist(arr, bins=50)
        axs[row][col].set_xlabel("fluorescence")
        axs[row][col].set_ylabel("density")
        axs[row][col].set_title(title, fontdict=title_dict)

    plt.savefig("plot/density/fluorescence_density.png")




if __name__ == "__main__":
    dframe = pd.read_csv("data/dataframe.csv")
    labels = ["IgG_Blank_703577", "IgG_0_dpi_044-118", "IgG_-6_dpi_044-103", "IgG_0_dpi_044-114", "IgG_-6_dpi_044-104",
              "IgG_0_dpi_044-117",	"IgG_0_dpi_044-116",	"IgG_0_dpi_044-112",	"IgG_0_dpi_044-109",	"IgG_0_dpi_044-102",
              "IgG_0_dpi_044-110",	"IgG_0_dpi_044-101",	"IgG_28_dpi_044-101", "IgG_28_dpi_044-102", "IgG_27_dpi_044-112",
              "IgG_27_dpi_044-117",	"IgG_28_dpi_044-114", "IgG_29_dpi_044-118", "IgG_27_dpi_044-110", "IgG_28_dpi_044-109",
              "IgG_29_dpi_044-116", "IgG_29_dpi_044-104", "IgG_31_dpi_044-103", "IgG_Blank_702923", "IgG_112_dpi_044-101",
              "IgG_115_dpi_044-102", "IgG_129_dpi_044-112", "IgG_126_dpi_044-117", "IgG_135_dpi_044-114",
              "IgG_130_dpi_044-118", "IgG_132_dpi_044-110", "IgG_108_dpi_044-109", "IgG_134_dpi_044-116",
              "IgG_114_dpi_044-104", "IgG_114_dpi_044-103", "IgG_Blank_702501", "IgM_0_dpi_044-101", "IgM_0_dpi_044-102",
              "IgM_0_dpi_044-112", "IgM_0_dpi_044-117", "IgM_0_dpi_044-114", "IgM_0_dpi_044-118", "IgM_0_dpi_044-110",
              "IgM_0_dpi_044-109", "IgM_0_dpi_044-116", "IgM_-6_dpi_044-104", "IgM_-6_dpi_044-103", "IgM_Blank_702594",
              "IgM_17_dpi_044-101", "IgM_15_dpi_044-102", "IgM_16_dpi_044-112", "IgM_16_dpi_044-117", "IgM_14_dpi_044-114",
              "IgM_15_dpi_044-118", "IgM_13_dpi_044-110", "IgM_14_dpi_044-109", "IgM_13_dpi_044-116", "IgM_15_dpi_044-104",
              "IgM_15_dpi_044-103", "IgM_Blank_702562", "IgM_21_dpi_044-101", "IgM_24_dpi_044-102", "IgM_23_dpi_044-112",
              "IgM_23_dpi_044-117", "IgM_21_dpi_044-114", "IgM_22_dpi_044-118", "IgM_23_dpi_044-110", "IgM_21_dpi_044-109",
              "IgM_22_dpi_044-116", "IgM_22_dpi_044-104", "IgM_22_dpi_044-103", "IgM_Blank_703409"]
    plot_distributions(dframe, labels)
