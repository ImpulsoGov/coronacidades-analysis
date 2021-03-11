import pandas as pd
import numpy as np
import geobr
import plotly
import plotly.graph_objs as go
import cufflinks as cf
plotly.offline.init_notebook_mode(connected=True)

def format_data(df,data_type,var, UF=None, date=None):
    
    if date == None:
        date = df['last_updated'].max()

    if data_type == 'state':
        
        d = df.query(f'last_updated=="{date}"')\
                    .groupby('state', 
                             as_index=False,
                             group_keys=False)[var]\
                    .sum()\
                    .sort_values('state')\
                    .reset_index(drop=True)
        d = d[['state',var]]
        return d
    
    elif data_type == 'city':
        d = df.query(f'last_updated=="{date}"')\
                    .query(f'state=="{UF}"')\
                    .sort_values('state')\
                    .reset_index(drop=True)
                
        d = d[['city_id',var]]
        return d
    
    
def plot_map(df, data_type, title, cmap, cbar_title, UF=None):
    #df must be in format pd.DataFrame({'id': [...] ,'z':[...]})
    year = 2018
    var = df.columns[-1]
    if data_type == 'state':
        df1 = geobr.read_state(code_state='all', year=year)
        df1 = df1.sort_values('abbrev_state').reset_index(drop=True)
        
        gdf = gpd.GeoDataFrame(df1[['abbrev_state','geometry']])
        jdf = json.loads(df1[['abbrev_state','geometry']].to_json())

        try:
            assert list(df1['abbrev_state'].values) == list(df['state'])
        except:
            print("df is not in the same order of IBGE data, try to put it on state name alphabetical order ")
    
        plot_data = dict(type = 'choropleth',
            geojson=jdf,
            locations =gdf.index.astype(str),
            locationmode = 'geojson-id',
            colorscale= cmap,
            text= gdf['abbrev_state'].astype(str),
            z=df[var].astype(float),
            colorbar = {'title':cbar_title})
        layout = dict(title_text = title,
                      geo = {'scope':'south america'})
        fig = go.Figure(data = plot_data,layout = layout)
        fig.update_geos(fitbounds="locations", visible=False)
    
    
    elif data_type == 'city':
        df1 = geobr.read_municipality(code_muni=UF, year=year)
        df1 = df1.sort_values('name_muni').reset_index(drop=True)
        df1['code_muni'] = df1['code_muni'].astype(int)
        
        idx = [pd.Index(df1['code_muni']).get_loc(i) for i in df['city_id'].values]
        idx = np.array(idx)
        idxx =list(set(range(len(df1))).difference(idx))

        
        z = np.ones(len(df1))
        z[idx] = df[var]
        z[idxx] = np.nan
        
        
        gdf = gpd.GeoDataFrame(df1[['name_muni','geometry']])
        jdf = json.loads(df1[['name_muni','geometry']].to_json())
    
        try:
            assert list(df1.loc[idx]['code_muni'].values) == list(df['city_id'])
        except:
            print("df isnt in the same order of IBGE data, try to put it on city name alphabetical order ")
    
    
        plot_data = dict(type = 'choropleth',
            geojson=jdf,
            locations =gdf.index.astype(str),
            locationmode = 'geojson-id',
            colorscale= cmap,
            text= gdf['name_muni'].astype(str),
            z=z.astype(float),
            colorbar = {'title':cbar_title})
        layout = dict(title_text = title,
                      geo = {'scope':'south america'})
        fig = go.Figure(data = plot_data,layout = layout)
        fig.update_geos(fitbounds="locations", visible=False)
    


    plotly.offline.iplot(fig)
    

    
    
def _df_to_plotly(df):
    return {'z': df.values.tolist(),
            'x': df.columns.tolist(),
            'y': df.index.tolist()}

def heatmap(df_heatmap, city_total_deaths, title, colors):
    trace1 = go.Heatmap(_df_to_plotly(df_heatmap), 
                                        colorscale=colors,showscale=False, zmin=0, zmax=1)

    trace2 = go.Bar(x=city_total_deaths,y=df_heatmap.index,
                    xaxis="x2",yaxis="y2",orientation='h')
    d = [trace1,trace2]
    layout = go.Layout(title=title,
        plot_bgcolor='rgba(0,0,0,0)',
        xaxis=dict(
            domain=[0, 0.7]
        ),
        xaxis2=dict(
            domain=[0.8, 1],
            showticklabels=True
        ),
        yaxis=dict(tickmode='linear'
        ),
        yaxis2=dict(tickmode='linear',anchor='x2'
        )

    )

    return go.Figure(data=d, layout=layout)

    
