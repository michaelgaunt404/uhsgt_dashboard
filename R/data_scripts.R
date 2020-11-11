#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Holds scripts
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: this holds all the long text strings that are used in the application
#-------- should be called by the app.R file
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#map_intro====
map_intro ='
<p style="text-align: left;">Base Map Page.</p>
<hr />
<p style="text-align: left;">This page displays the default Cascadia UHSGT corridor map from Eugene, OR to Vancouver, BC. Then main body of this page contains the map and the right sidebar contains map inputs.</p>

  <p style="text-align: left;">&nbsp;<strong>Base Map Information:</strong></p>
  <ul>
  <li style="text-align: left;">The default map displays a multitude of layers depicting jurisdictional boundaries and important transportation features located along corridor</li>
  <li style="text-align: left;">The map contains layers for British Columbia, Washington, and Oregon</li>
  <li style="text-align: left;">The default displayed layers are US MPO RTPOs and US Census</li>
  <li style="text-align: left;">Layers can be hidden or displayed - see layer menu in map upper left corner</li>
  <li style="text-align: left;">Measurements can be taken between map features - see ruler box in map lower left corner&nbsp;</li>
  <li style="text-align: left;">The metric used to color US Census tract polygons can be changed by interacting with the Census Filters sidebar dropdown</li>
  <ul>
  <li style="text-align: left;">Filter Census Data (by group) changes/limits the avaible Census Layer Color Metric options</li>
  <li style="text-align: left;">Census Layer Color Metric is the actual input to change the map coloring scheme</li>
  <li style="text-align: left;">The census layer\'s will reglect this change and indicate what the shown metic is</li>
  </ul>
  </ul>' %>% 
  HTML()

#mpo_overview====
mpo_overview = "Purposes of MPOs: \nTransportation investment allocates scarce federal and other transportation funding resources\nPlanning needs to reflect the region's shared vision for its future; 
                                          \nAdequate transportation planning requires a comprehensive examination of the region's future and investment alternatives; and
\nAn MPO acts as a Council of Governments; that is, it facilitates collaboration of governments, interested parties, and residents in the planning process."

#intro_modal_front====
intro_modal_front = '<!DOCTYPE html>
<html>
<head>
<style>
body {color: #484848;}
h1 {color: #484848;}
h4 {color: #484848;}
</style>
</head>

<body>
<h1 style="text-align: center;">Cascadia UHSGT Corridor Planning Dashboard</h1>
<h4 style="text-align: center;">An interactive, web-based tool to help explore the proposed UHSGT corridor.</h4>
<hr />
<p style="text-align: left;">The goal of this dashboard is to provide a high-level view of the entire corridor to aid and facilitate the planning of UHSGT in the Pacific Northwest.  
The platform accomplishes this by displaying current transportation assets, political jurisdictions boundaries, and census data in an interactive map. 
The map and dashboard were designed to be intuitive and user-friendly to </p>
<hr />
<p style="text-align: left;">&nbsp;<strong>What you can do with this dashboard:</strong></p>
<ul>
<li style="text-align: left;">Explore the Cascadia corridor given a variety of map layers relevant to UHSGT.&nbsp;</li>
<li style="text-align: left;">Inspect aggregated census metrics for the corridor and by user defined by spatial subsets.&nbsp;</li>
<li style="text-align: left;">Download data and metrics to share with colleagues offline.&nbsp;</li>
<li style="text-align: left;">Understand important regional planning deadlines and publications.&nbsp;</li>
</ul>
<p style="text-align: left;">&nbsp;<strong>How to navigate this dashboard:</strong></p>
<ul>
<li style="text-align: left;">This dashboard has three tabs which are navigable via the sidebar (left).&nbsp;</li>
<li style="text-align: left;">The <strong>Map Dashboard</strong> tab contains the corridor map and its own sidebar for map filtering controls.&nbsp;</li>
<li style="text-align: left;">The <strong>Census Metrics</strong> tab contains a filterable corridor mini-map which returns aggregated census statistics which you can compare to the corridor aggregates.&nbsp;</li>
<li style="text-align: left;">The <strong>Data Center</strong> tab contains all the data used to create the map and metrics tables.&nbsp;</li>
<ul>
      <li>The tables on this page contain direct links to the data sources so you may perform further investigation at you convenience.</li>
</ul>
</ul>
</body>
</html>'


intro_modal_back= '<!DOCTYPE html>
<html>
<head>
<style>
body {color: #484848;=}

</style>
</head>

<body>
<p style="text-align: center;"><span style="color: #000000;"> For support, issue reporting, or for features you\'d like to see - contact: michael.gaunt@wsp.com or vist the project <a href="https://github.com/michaelgaunt404/GEOCDR">GIT repo</a></span></p>
  <p style="text-align: center;"><strong>Need more help?</strong> <a href="https://wsponline-my.sharepoint.com/:w:/g/personal/nicholas_richter_wsp_com/EXFdg_4V-LRDqr-lNXurfwgBHXnMj9cbJyKWGTn0fHu-fw">Download our instruction manual here.</a></p>
  <p style="text-align: center;"><strong><span style="color: #ff0000;">IMPORTANT: This is for internal use only. </span></strong></p>
  </body>
  </html>'

map_edit_info ='<p style="text-align: left;">Lorem ipsum dolor sit amet, consectetur adipiscing elit.</p>
  <p style="text-align: left;">This tab contains all  
  It contains several layers which depict jurisdictional boundaries and important transportation features located in the corridor. /p>
  <hr />
  <p style="text-align: left;">Please use this map to expore the region.</p>
  <hr />
  <p style="text-align: left;">&nbsp;<strong>Spatially Filtered Maps Tab</strong></p>
  <ul>
  <li style="text-align: left;">This tab contains two maps, featuers can be               and the right map a filtered version of the same map given the user defined features.</li>
  <li style="text-align: left;">The left map can be edited to include user defined geometry.</li>
  <ul>
      <li>This can be performed using the toolbar (map left).</li>
      <li>User defined geometries can be used to subset the corridor map can produce aggregated census metrics for said subset.</li>
</ul>
  <li style="text-align: left;"> </li>
  <li style="text-align: left;"> </li>
  <li style="text-align: left;">The features can be added to the left map .&nbsp;</li>
  <li style="text-align: left;">The US census layer can be augmented in 2X ways - by reducing the the number of mapped census vars. or by changing the variable which is used to color the census polygons - see boxes below.</li>
  <li style="text-align: left;">The map can be spaitally filtered in two ways - by coordinates or geocoded addresses - see boxes below.</li>' %>%
  HTML()

map_edit_tab_info = 
'<p style="text-align: left;">Filtered Map Page.</p>
<hr />
<p style="text-align: left;">This page enables the user to define new geometries to use to spatially filter the default corridor map. The subset returns all layers and features that it overlaps with and removes all layers that it has no interaction with. Metrics for the remaining census tracts are aggregated and returned to the user both visually in a series of plots and in tabular form. The subset aggregates can be compared to the corridor level aggregates to investigate how user defined spatial subset deviates from the corridor as a whole.</p>
<p style="text-align: left;">Use Example:  A user is interested in equity and a specific demographic\'s representation for a potential Right-of-Way (ROW). The user draws a line on the map representing the proposed ROW, all relevent layer and map features are returned to the user once they intiate the subset process. They can then investigate their question regarding equity with respect to the proposed ROW.</p>
<hr />
<p style="text-align: left;">How to use this page:</strong></p>
  <ul>
  <li style="text-align: left;">The user defines a new geometry - polygons, lines, points, etc - using the left toolbar on the left hand map (see Spatially Filtered Maps tab)</li>
  <li style="text-align: left;">The user defines a buffer (default 1 mile) to be applied to their geometry (see top navbar) </li>
  <li style="text-align: left;">The user submits their selections and initiates the filtering and aggregation process by clicking the <strong>Filter Default Map</strong> button (see Navbar above)</li>
  <li style="text-align: left;">The user defined features and the resulting subset will be displayed on the right hand map (see) </li>
  <li style="text-align: left;">US Census metrics (corridor and subsets) are displayed on the Subset Plots and Subset Data tabs</li>
  <li style="text-align: left;">The user has the option to download their geometry by pressing the <strong>Download User Geometry</strong> button (see Navbar above)</li>
  </ul>
  <p style="text-align: left;">How to navigate this page:</strong></p>
  <ul>
  <li style="text-align: left;">Navbar</li>
  <ul>
  <li style="text-align: left;">Displays the buttons used to filter map and download shapefile</li>
  <li style="text-align: left;">Displays the inputs to change the buffer applied to the user\'s geometry and selector for variables to show the box plots</li>
</ul>
<li style="text-align: left;">Spatially Filtered Maps</li>
<ul>
<li style="text-align: left;">This is the default tab for the tab box</li>
<li style="text-align: left;">Displays two maps - the left is editable and the right displays those edits and returns buffered corridor map</li>
</ul>
<li style="text-align: left;">Metric Plots</li>
<ul>
<li style="text-align: left;">This tab displays plotted aggregated metrics given user geometries and corridor subset</li>
<li style="text-align: left;">Both of these plots are interactive - zoom capabilities and hover tooltips</li>
<li style="text-align: left;">The top plot displays the Percent Error/Difference between the corridor and corridor subset for all US Census variables</li>
<ul>
<li style="text-align: left;">E.g. 100*(Subset-Corridor)/Corridor</li>
</ul>
<li style="text-align: left;">The bottom plot displays the boxplots for the same metrics</li>
</ul>
<li style="text-align: left;">Metric Tabular Data</li>
<ul>
<li style="text-align: left;">This tab displays the same information  but in tabular form </li>
<li style="text-align: left;">This table can be downloaded using the buttons on the top of the table</li>
</ul>
<li style="text-align: left;"Census Tract Subset</li>
<ul>
<li style="text-align: left;">This tab contains a table detailing which census tracts were contained in the subset</li>
</ul>
</ul>' %>%  
  HTML()











