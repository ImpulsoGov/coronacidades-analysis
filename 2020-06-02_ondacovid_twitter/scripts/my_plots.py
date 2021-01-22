import plotly.graph_objs as go
import geobr
import geopandas as gpd
import json


def _generate_hovertext(df_to_plotly):

    hovertext = list()
    for yi, yy in enumerate(df_to_plotly["y"]):
        hovertext.append(list())
        for xi, xx in enumerate(df_to_plotly["x"]):
            hovertext[-1].append(
                "<b>{}</b><br>Data: {}<br>Percentual do máximo: {}".format(
                    yy, str(xx)[:10], round(df_to_plotly["z"][yi][xi], 2)
                )
            )

    return hovertext


def _df_to_plotly(df):
    return {"z": df.values.tolist(), "x": df.columns.tolist(), "y": df.index.tolist()}


def _generate_mvg_deaths(df, place_type, mavg_days):

    df = (
        df[~df["deaths"].isnull()][[place_type, "last_updated", "deaths", "new_deaths"]]
        .groupby([place_type, "last_updated"])["deaths", "new_deaths"]
        .sum()
        .reset_index()
    )

    df["rolling_deaths_new"] = df.groupby(
        place_type, as_index=False, group_keys=False
    ).apply(lambda x: _get_rolling_amount(x, mavg_days))

    return df


def _get_rolling_amount(grp, time, data_col="last_updated", col_to_roll="new_deaths"):
    return grp.rolling(time, min_periods=1, on=data_col)[col_to_roll].mean()


def prepare_heatmap(df, place_type, group=None, mavg_days=5, **kwargs):

    refresh = df["data_last_refreshed"][0]

    # if place_type == "city":
    #     df = df[df["state"] == group]

    if place_type == "city" or place_type == "state":
        df = _generate_mvg_deaths(df, place_type, mavg_days)
        col_date = "last_updated"
        col_deaths = "deaths"

    if place_type == "country_pt":
        col_date = "date"
        col_deaths = "total_deaths"

    place_max_deaths = (
        df.groupby(place_type)[[col_deaths]].max().reset_index().sort_values(col_deaths)
    )
    return plot_heatmap(
        df,
        place_type,
        **kwargs
        # min_deaths=min_deaths,
        # title ="Distribuição de novas mortes nas UFs (mavg = 5 days)",
    )


def plot_heatmap(df, place_type, title=None, group=None, datasource=""):

    if place_type == "state" or place_type == "city":
        col_date = "last_updated"
        col_deaths = "deaths"

    if place_type == "country_pt":
        col_date = "date"
        col_deaths = "total_deaths"

    pivot = (
        df.reset_index()
        .pivot(index=place_type, columns=col_date, values="rolling_deaths_new")
        .fillna(0)
        .apply(lambda x: x / x.max(), axis=1)
        .dropna(how="all")
    )

    # remove days with all states zero
    pivot = pivot.loc[:, (pivot != 0).any(axis=0)]

    pivot = pivot.applymap(lambda x: 0 if x < 0 else x)

    # entender o que acontece aqui
    states_total_deaths = (
        df.groupby(place_type)[col_deaths]
        .max()
        .loc[pivot.index]
        .sort_values(ascending=False)[:30]
        .sort_values()
    )

    # Ordena por: 1. Dia do máximo, 2. Quantidade de Mortes
    sorted_index = (
        pivot.loc[states_total_deaths.index]
        .idxmax(axis=1)
        .to_frame()
        .merge(states_total_deaths.to_frame(), left_index=True, right_index=True)
        .sort_values(by=[0, col_deaths])
        .index
    )

    states_total_deaths = states_total_deaths.reindex(sorted_index)

    data = _df_to_plotly(pivot.loc[states_total_deaths.index])
    traces = []
    traces.append(
        go.Heatmap(
            data,
            hoverinfo="text",
            hovertext=_generate_hovertext(data),
            colorscale="temps",
            showscale=False,
        )
    )

    legend = [("Mínimo", "#159392"), ("Médio", "#ddcc90"), ("Máximo", "#d1637d")]
    for name, color in legend:
        traces.append(
            go.Scatter(
                x=[None],
                y=[None],
                mode="markers",
                showlegend=True,
                name=name,
                marker=dict(size=12, symbol="square", color=color),
            )
        )

    layout = go.Layout(
        title=dict(text=title, y=0.9, x=0.5, xanchor="center", yanchor="auto"),
        plot_bgcolor="rgba(0,0,0,0)",
        # autosize=True,
        # width=1000,
        height=700,
        margin={"l": 100, "r": 100, "t": 100},
        xaxis=dict(domain=[0, 1]),
        xaxis_tickformat="%d/%m",
        yaxis=dict(tickmode="linear"),
        annotations=[
            go.layout.Annotation(
                showarrow=False,
                text=f"Siga @ImpulsoGov",
                xref="paper",
                yref="paper",
                x=1,
                y=-0.1,
            ),
            go.layout.Annotation(
                showarrow=False,
                text=f"Dados: {datasource}",
                xref="paper",
                yref="paper",
                x=0,
                y=-0.1,
            ),
        ],
    )

    fig = go.Figure(data=traces, layout=layout)

    return fig


def plot_map(df, data_type, title, cmap, cbar_title, UF=None):
    # df must be in format pd.DataFrame({'id': [...] ,'z':[...]})
    year = 2018
    var = df.columns[-1]
    if data_type == "state":
        df1 = geobr.read_state(code_state="all", year=year)
        df1 = df1.sort_values("abbrev_state").reset_index(drop=True)

        gdf = gpd.GeoDataFrame(df1[["abbrev_state", "geometry"]])
        jdf = json.loads(df1[["abbrev_state", "geometry"]].to_json())

        try:
            assert list(df1["abbrev_state"].values) == list(df["state"])
        except:
            print(
                "df is not in the same order of IBGE data, try to put it on state name alphabetical order "
            )

        plot_data = dict(
            type="choropleth",
            geojson=jdf,
            locations=gdf.index.astype(str),
            locationmode="geojson-id",
            colorscale=cmap,
            text=gdf["abbrev_state"].astype(str),
            z=df[var].astype(float),
            colorbar={"title": cbar_title},
        )
        layout = dict(title_text=title, geo={"scope": "south america"})
        fig = go.Figure(data=plot_data, layout=layout)
        fig.update_geos(fitbounds="locations", visible=False)

    return fig


if __name__ == "__main__":

    # This is executed you run via terminal

    print("I'm in terminal! - example.py")
