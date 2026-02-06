import json

import pandas as pd
import geopandas as gpd


def get_cluster_centroid(x) -> tuple[float, float]:
    c = x.geometry.union_all().centroid
    lon = c.x
    lat = c.y
    return lon, lat


def calculate_cluster_centroid(
    stops: gpd.GeoDataFrame,
) -> tuple[dict[str, float], dict[str, float]]:
    clust2latlon = (
        stops.groupby(["clust"])
        .apply(get_cluster_centroid, include_groups=False)
        .reset_index()
    )
    clust2latlon.columns = ["clust", "lonlat"]

    c2lat = {}
    c2lon = {}
    for row in clust2latlon.itertuples():
        c2lat[row.clust] = row.lonlat[1]
        c2lon[row.clust] = row.lonlat[0]
    return c2lon, c2lat


def recluster_accessible_stops(
    accessible_stops: dict[str, list[str]], mapper: dict
) -> dict[str, list[str]]:
    result = {}
    for k, v in accessible_stops.items():
        if mapper[k] not in result:
            result[mapper[k]] = []
        for vv in v:
            result[mapper[k]].append(mapper[vv])
    return result


if __name__ == "__main__":
    import argparse

    argparser = argparse.ArgumentParser()
    argparser.add_argument(
        "--city",
        type=str,
        required=True,
        help="city ID (lowercase name)",
    )
    argparser.add_argument(
        "--data-version",
        type=str,
        default="",
        required=False,
        help="data version (subfolder in city)",
    )
    opts = argparser.parse_args()

    stops = pd.read_csv(
        f"../data/stops/{opts.city}/{opts.data_version}/stops_with_centrality.csv"
    )

    stops = pd.read_csv(
        f"../data/stops/{opts.city}/{opts.data_version}/stops_with_centrality.csv",
        dtype={"stop_id": str},
    )
    stops = stops.drop(
        columns=[
            "Node",
            "Eigenvector Centrality",
            "Degree Centrality",
            "Closeness Centrality",
            "Betweenness Centrality",
        ]
    ).copy()
    stops = gpd.GeoDataFrame(
        stops,
        geometry=gpd.points_from_xy(stops["stop_lon"], stops["stop_lat"]),
        crs=4326,
    )

    with open(
        f"../data/stops/{opts.city}/{opts.data_version}/accessible_stops.json", "r"
    ) as fp:
        accessible_stops = json.load(fp)

    c2lon, c2lat = calculate_cluster_centroid(stops)

    clust2stop = (
        stops.sort_values(["clust", "stop_id"])
        .drop_duplicates(subset=["clust"], keep="first")[["clust", "stop_id"]]
        .copy()
    )
    c2s = {}
    for row in clust2stop.itertuples():
        c2s[row.clust] = row.stop_id

    clustered_stops = stops.copy()
    clustered_stops = clustered_stops.drop_duplicates(subset=["clust"]).copy()

    clustered_stops["stop_id"] = clustered_stops["clust"].map(c2s)
    clustered_stops["stop_lat"] = clustered_stops["clust"].map(c2lat)
    clustered_stops["stop_lon"] = clustered_stops["clust"].map(c2lon)

    clustered_stops.to_csv(
        f"../data/stops/{opts.city}/{opts.data_version}/clustered_stops.csv",
        index=False,
    )

    stop2stop = stops.sort_values(["clust", "stop_id"]).copy()
    stop2stop["clust"] = stop2stop["clust"].map(c2s)

    s2s = {}
    for row in stop2stop[["stop_id", "clust"]].itertuples():
        s2s[row.stop_id] = row.clust

    clusetered_accessible_stops = recluster_accessible_stops(accessible_stops, s2s)

    with open(
        f"../data/stops/{opts.city}/{opts.data_version}/clustered_accessible_stops.json",
        "w",
    ) as fp:
        json.dump(clusetered_accessible_stops, fp, indent=1)
