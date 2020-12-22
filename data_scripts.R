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
map_intro =
'<p style="text-align: left;">Base Map Page</p>
<hr />
<p style="text-align: left;">This page displays the base Cascadia UHSGT corridor map from Eugene, OR to Vancouver, BC. 
Then main body of this page contains the base map and the right sidebar contains US and Canadian census variable inputs.</p>
<p style="text-align: left;">&nbsp;<strong>Base Map Information:</strong></p>
<ul>
<li style="text-align: left;">The default map displays a multitude of layers depicting jurisdictional boundaries and important transportation features located along the corridor</li>
<li style="text-align: left;">The map contains layers for British Columbia, Washington, and Oregon</li>
<li style="text-align: left;">The default displayed layers are Regional Planning and US and Canadian Census</li>
<li style="text-align: left;">Layers can be hidden or displayed - see layer menu in map upper left corner</li>
<li style="text-align: left;">Measurements can be taken between map features - see ruler box in map lower right corner&nbsp;</li>
<li style="text-align: left;">The metric used to color US Census tract polygons can be changed by interacting with the Census Filters sidebar dropdown</li>
<ul>
<li style="text-align: left;">Filter Census Data (by group) changes/limits the available Census Layer Color Metric options and which metrics are displayed in the census layer pop-ups</li>
<li style="text-align: left;">Census Layer Color Metric is the actual input to change the map coloring scheme</li>
<li style="text-align: left;">The census layers will reflect this change and indicate what the shown metric is</li>
</ul>
</ul>
<p style="text-align: left;">&nbsp;<strong>Sidebar Information:</strong></p><ul>
<ul>
<li style="text-align: left;">As mentioned above, the sidebar contains census layer inputs</li>
<li style="text-align: left;">The sidebar also contains dynamic histograms which display the distribution for the selected census metric</li>
<li style="text-align: left;">The top plot per each dropdown window displays the variable\'s distribution using every census tract along the corridor</li>
<li style="text-align: left;">The bottom plot per each dropdown window displays the variable\'s distribution using a subset of census tract along the corridor</li>
<li style="text-align: left;">These plots are dynamic:</li>
<ul>
<li style="text-align: left;">Users can see where a specific census tract is located on this distribution by clicking on the map</li>
<li style="text-align: left;">Census tracts used by the subset plot are determined by the map window bounding box - users can zoom into different corridor regions to see how the selected variable distribution of said area varies from that of the corridor</li>
</ul>
</ul>
' %>% 
  HTML()

#mpo_overview====
mpo_overview = "Purposes of MPOs: \nTransportation investment allocates scarce federal and other transportation funding resources\nPlanning needs to reflect the region's shared vision for its future; 
                                          \nAdequate transportation planning requires a comprehensive examination of the region's future and investment alternatives; and
\nAn MPO acts as a Council of Governments; that is, it facilitates collaboration of governments, interested parties, and residents in the planning process."

intro_modal_front = '
<!DOCTYPE html>
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
<h4 style="text-align: center;">An interactive, web-based tool to help explore and plan for the proposed UHSGT corridor.</h4>
<hr />
<p style="text-align: left;">Welcome to the dashboard! The goal of this tool is to provide a high-level view of the entire corridor to aid and facilitate the planning of Ultra High-speed Ground Transporation (UHSGT) in the Pacific Northwest.  
The platform accomplishes this by displaying current transportation assets, governance boundaries, and census data in an interactive map. </p>
<hr />
<p style="text-align: left;">&nbsp;<strong>What you can do with this dashboard:</strong></p>
<ul>
<li style="text-align: left;">Explore the Cascadia corridor given a variety of base map layers relevant to UHSGT.&nbsp;</li>
<li style="text-align: left;">Inspect aggregated census metrics for the corridor and by user defined by spatial subsets.&nbsp;</li>
<li style="text-align: left;">Download data and metrics to share with colleagues offline.&nbsp;</li>
<li style="text-align: left;">Understand important regional planning deadlines and publications.&nbsp;</li>
</ul>
<p style="text-align: left;">&nbsp;<strong>How to navigate this dashboard:</strong></p>
<ul>
<li style="text-align: left;">This dashboard has four main pages which can be navigated to using the left sidebar</li>
<li style="text-align: left;">The <strong>Base Map</strong> page contains the corridor map and its own sidebar for map filtering controls</li>
<li style="text-align: left;">The <strong>Filtered Map</strong> page contains mapping controls which allow the user to create their own geometry and apply it to the base map</li>
<ul>
<li style="text-align: left;">The user-defined geometry can be used to spatially filter the corridor and return aggregated census statistics</li>
</ul>
<li style="text-align: left;">The <strong>Regional Planning</strong> page contains information regarding regional transportation planning organizations located in the US and Canada.</li>
<li style="text-align: left;">The <strong>Data Center</strong> page contains all the data used to create the map and metrics tables.</li>
<li style="text-align: left;">The <strong>Help Center</strong> tab contains help buttons the user can use if they are stuck or need more information</li>
<ul>
<li style="text-align: left;">The tables on this page contain direct links to the data sources so you may perform further investigation at your convenience.</li>
</ul>
</ul>
<p style="text-align: left;">&nbsp;<strong>Note:</strong> Clicking on html links in map pop-ups or tables will exit out of the dashboard</p><ul>
<ul>
<li style="text-align: left;">To open a new window for the html link, use <strong>right click -> "open new window"</strong></li>
<ul>
</body>
</html>' %>% 
  HTML()

intro_modal_back= '<!DOCTYPE html>
<html>
<head>
<style>
body {color: #484848;=}
</style>
</head>
<body>
<p style="text-align: left;">&nbsp;<strong>Additional Info</strong></p>
<p style="text-align: left;"> This dashboard\'s main intent is to enable UHSGT planners to identify corridor stakeholders early in the planning process. This dashboard was not meant to be a public facing product or made for the use by the general public. Users who have been granted access to this dashboard should ask for permission before sharing this with others. Please contact your WSDOT representative for permission to do so.</p>
 <hr />
<p style="text-align: center;"><span style="color: #000000;">For additional information, please see the operational manual located at this <a href="https://github.com/michaelgaunt404/uhsgt_dashboard/blob/main/Operation_Manual.pdf">GIT repo location.</a></span></p>
<p style="text-align: center;"><span style="color: #000000;">For support, issue reporting, or for features you\'d like to see included in the next release - contact: michael.gaunt@wsp.com, your UHSGT corridor planning contact or visit the project\'s <a href="https://github.com/michaelgaunt404/uhsgt_dashboard">GIT repo</a></span></p>
<p style="text-align: center;"><strong><span style="color: #ff0000;">IMPORTANT: This is for internal use only - please check with your UHSGT corridor planning contact before sharing. </span></strong></p>
</body>
</html>' %>% 
  HTML()

map_edit_tab_info = 
'<p style="text-align: left;">Filtered Map Page</p>
<hr />
<p style="text-align: left;">This page enables the user to define new geometries to spatially filter the base corridor map. The subset returns all layers and features that it overlaps with and removes all layers that it has no interaction with. Metrics for the remaining census tracts are aggregated and returned to the user both visually in a series of plots and in tabular form. The subset aggregates can be compared to the corridor level aggregates to investigate how user defined spatial subset deviates from the corridor as a whole.</p>
<p style="text-align: left;">Use Example: A user is interested in equity and a specific demographic\'s representation for a potential Right-of-Way (ROW). The user draws a line on the map representing the proposed ROW, all relevant layer and map features are returned to the user once they initiate the subset process. They can then investigate their question regarding equity with respect to the proposed ROW.</p>
<hr />
<p style="text-align: left;">How to use this page:</strong></p>

<ul>
<li style="text-align: left;">The user defines a new geometry - polygons, lines, points, etc. - using the left toolbar on the left-hand map (see Spatially Filtered Maps tab)</li>
<li style="text-align: left;">The user defines a buffer (default 1 mile) to be applied to their geometry (see top navbar) </li>
<li style="text-align: left;">The user submits their selections and initiates the filtering and aggregation process by clicking the <strong>Filter Default Map</strong> button (see top navbar)</li>
<li style="text-align: left;">The user will be notified by a pop-up window that their selection was received</li>
<li style="text-align: left;">US and Canadian Census metrics (corridor and subsets) are displayed on the metric plots, Metric Tabular Data, and Census tract Subset tabs</li>
<li style="text-align: left;">The user has the option to download their geometry by pressing the <strong>Download User Geometry</strong> button (see top navbar)</li>
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
<li style="text-align: left;">Displays two maps - the left is editable, and the right displays those edits and returns buffered corridor map</li>
</ul>
<li style="text-align: left;">Metric Plots</li>
<ul>
<li style="text-align: left;">This tab displays plotted aggregated metrics given user geometries and corridor subset</li>
<li style="text-align: left;">Both plots are interactive - zoom capabilities and hover tooltips are enabled</li>
<li style="text-align: left;">The top plot displays the Percent Error/Difference between the corridor and corridor subset for all US Census variables</li>
<ul>
<li style="text-align: left;">E.g. 100*(Subset-Corridor)/Corridor</li>
</ul>
<li style="text-align: left;">The bottom plot displays the boxplots for the same metrics</li>
</ul>
<li style="text-align: left;">Metric Tabular Data</li>
<ul>
<li style="text-align: left;">This tab displays the same information as the metric plots mentioned above but in tabular form </li>
<li style="text-align: left;">This table can be downloaded using the buttons on the top of the table</li>
</ul>
<li style="text-align: left;">Census Tract Subset</li>
<ul>
<li style="text-align: left;">This tab contains a table detailing which census tracts were contained in the subset</li>
</ul>
</ul>
' %>%  
  HTML()


mpo_drop_down = "This plot was created by merging the UC Census and the MPO spatial layers. 
The metrics shown in this table are mean aggregations of the US Census metrics for census tracts contained in each MPO jurisdiction. 
These metrics should be considered as estimates - the spatial boundaries for each layer do not align perfectly given the fidelity of US Census layer."


