import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd

maps_path = 'E:/CS-GO-Analytics/Raw Data/'
processed_data_path = 'E:/CS-GO-Analytics/Processed Data/'
output_path = 'E:/CS-GO-Analytics/Output/'

def PlotKillDensity(df_, round_, map_):
    # identify map name
    map_name = df_.loc[df_['map'] == map_, 'map_name'].unique()
    # subset on a particular round
    df_ = df_.loc[(df_['round'] == round_) & (df_['isKill'] == True)]
    # generate axes of specified map
    bg = plt.imread(maps_path + map_ + '.png')
    fig, (ax1, ax2) = plt.subplots(1,2,figsize=(18,16))
    ax1.grid(b=True, which='major', color='w', linestyle='--', alpha=0.25)
    ax2.grid(b=True, which='major', color='w', linestyle='--', alpha=0.25)
    ax1.imshow(bg, zorder=0, extent=[0.0, 1024, 0., 1024])
    ax2.imshow(bg, zorder=0, extent=[0.0, 1024, 0., 1024])
    plt.xlim(0,1024)
    plt.ylim(0,1024)
    ax1.axes.get_xaxis().set_visible(False)
    ax1.axes.get_yaxis().set_visible(False)
    ax2.axes.get_xaxis().set_visible(False)
    ax2.axes.get_yaxis().set_visible(False)
    # plot density of where attacking players are standing when they kill an opponent
    plot_df = df_.loc[(df_['map'] == map_) & (df_['att_side'] == 'Terrorist')]
    sns.kdeplot(plot_df['att_pos_x'], plot_df['att_pos_y'], cmap = 'YlOrBr', bw = 15, ax = ax1)
    t_title = map_name + ': Terrorists Kills in Round ' + str(round_)
    ax1.set_title(t_title[0])
    plot_df = df_.loc[(df_['map'] == map_) & (df_['att_side'] == 'CounterTerrorist')]
    sns.kdeplot(plot_df['att_pos_x'], plot_df['att_pos_y'], cmap = 'Blues', bw = 15, ax = ax2)
    ct_title = map_name + ': Counter-Terrorists Kills in Round ' + str(round_)
    ax2.set_title(ct_title[0])
    # save image
    plt.savefig(output_path + 'Kill Density Plots/kill_density_round' + str(round_) + '_' + map_ + '.png')

# read in data
df = pd.read_csv(processed_data_path + 'processed_damage.csv')

# generate plots
PlotKillDensity(df_ = df, round_ = 1, map_ = 'de_dust2')
PlotKillDensity(df_ = df, round_ = 2, map_ = 'de_dust2')
PlotKillDensity(df_ = df, round_ = 3, map_ = 'de_dust2')
PlotKillDensity(df_ = df, round_ = 1, map_ = 'de_inferno')
PlotKillDensity(df_ = df, round_ = 2, map_ = 'de_inferno')
PlotKillDensity(df_ = df, round_ = 3, map_ = 'de_inferno')




