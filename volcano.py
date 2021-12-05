import streamlit as st
import pandas as pd
import altair as alt
import plotly.express as px

volcanoRaw = pd.read_csv(r"dataset\volcanoes around the world in 2021.csv")

regionCount = volcanoRaw.groupby(['Region'], sort=True).size().reset_index(name='counts')

bars = alt.Chart(regionCount).mark_bar().encode(
    y = alt.Y("Region", sort=alt.EncodingSortField(field="counts", order='descending')),
    x = 'counts')
text = bars.mark_text(align = 'left', baseline = 'middle', dx=3).encode(text = 'counts')

plot = (bars + text).properties(height=900).configure_axis(grid = False).configure_view(strokeWidth = 0)

fig = px.scatter_geo(volcanoRaw,lat='Latitude',lon='Longitude', hover_name="Volcano Name")
fig.update_layout(title = 'Volcano of the World 2021', title_x=0.5)

st.title("Volcano Around the World in 2021")
st.dataframe(volcanoRaw)

st.subheader('Volcano Count arross Regions')
st.altair_chart(plot, use_container_width=True)
st.plotly_chart(fig, use_container_width=True)