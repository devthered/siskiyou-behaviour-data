# masters-thesis

This project processes data and produces figures for the 2015-2020 Siskiyou Puma-Deer Project. It combines camera deployment metadata with behavioural observations from remote motion-trigger cameras deployed at Puma Kill sites, recorded in BORIS software. It also incorporates metadata from the video files themselves.

Entry point is make_figures.R. Other files referenced at the top and commented out are needed to reproduce results from raw inputs.

Final outputs are boris_data_pruned.csv (pruned and cleaned up behavioural observation data from the first 21 days of camera deployment at each site)
and deployments_all_stats.csv (behavioural and observational data aggregated for each camera deployment)