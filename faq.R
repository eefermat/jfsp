bsCollapse(id="faq",
  bsCollapsePanel("What are Fire Mgmt Zones?",
    p("These regions are the current Fire Management Zones for Alaska. For more information, please see the following link.", style="text-align:justify"),
    a("Bureau of Land Management / Alaska Fire Service - Alaska zone coverage maps", 
      href="https://afs.ak.blm.gov/fire-management/zones-alaska-zone-coverage-maps.php", target="_blank"),
    style="info"),
  bsCollapsePanel("What is a GCM?",
    p("General Circulation Models (GMCs) are used to depict how climate processes respond to the composition of various gases in the atmosphere.
      Using future projections of the composition of gases in the atmosphere, projections of future climate can be made.
      For this work, the GCMs provide different projections of future climate that are used to inform projections of future fire activity.
      If you are interested in exploring the range of burned area with these GCMs,
      the GFDL-CM3 and NCAR-CCSM4 models tend to correspond to the projections with the most area burned
      and the MRI-CGCM3 tends to have the least area burned. For more information, please see the following link.", style="text-align:justify"),
    a("Intergovernmental Panel on Climate Change GCM guide",
      href="http://www.ipcc-data.org/guidelines/pages/gcm_guide.html", target="_blank"),
    style="info"),
  bsCollapsePanel("What is an RCP?",
    p("Representative Concentration Pathways (RCPs) are used to characterize the consequences of different assumptions 
      about human population growth and economic development. In particular, economic development associated with energy usage 
      (e.g. how much and from what sources) is an important driver of future climate.
      We consider 3 RCPs here (4.5, 6.0, and 8.5). RCP 4.5 represents an aggressive reduction 
      in the emission of greenhouse gases like CO2 and methane. 
      RCP 8.5 represents significant increases in the population and a continuation of the use of energy sources
      that emit large quantities of green house gases. 
      RCP 6.0 lies somewhere in between.", style="text-align:justify"), 
      style="info"),
  bsCollapsePanel("What do \"simulations\" refer to in the data selection area?", 
    p("In order to accommodate the uncertainty associated with ignitions in a given year, 
      ALFRESCO simulates several dozen different possible fire seasons for each year. 
      In this context, each year in the historical record is just one single possible outcome that is consistent with the weather for that summer. 
      Uncertainty in the out put form this tool comes from several different places. 
      The ability to summarize across simulations using the mean, minimum or maximum, 
      allows us to explore the uncertainty that is specifically associated with the simulation of fire activity in ALFRESCO.", style="text-align:justify"),
    style="info"),
  bsCollapsePanel("What are fact sheets and how do they work?", 
    p("
      Fact sheets are short summary reports pertaining to selected data.
      The fact sheet button in the sidebar offers dynamic reports for download.
      Dynamic report generation refers to the creation of reports that update automatically in response to changing input such as a new data set.
      Data sets are often updated over time leading to routine, time-consuming report revision,
      or a similar report might need to be written based on different data entirely.
      Dynamic reports are typically used in cases like these where data are expected to change, affecting plots, tables and text in a document,
      and it would be cumbersome to revise a report by hand.", style="text-align:justify"),

    p("In this application, fact sheets are generated uniquely depending on which variable is currently selected for investigation.
      Pressing the fact sheet button generates a report for the currently displayed variable.
      For example, if viewing burn area simulation outputs, the download button generates a burn area fact sheet.
      The button is only available when on an analysis tab pertaining to a specific ALFRESCO simulation output variable.
      It is not visible from the data selection tab because there is nothing to yet lay out in a report.", style="text-align:justify"),
    
    p("This is because the download button is not merely a dynamic link to five different static reports;
      it is used for accessing completely dynamic reports.
      Fact sheets pertaining to the five available variables are in turn each customized based on the user's data selections and other input settings.
      Changing the selected range of years, the subset of climate models or geographic regions, and so on,
      changes the content of the report. Plots are included as displayed in the app, using settings chosen by the user,
      and the text in a fact sheet mutates accordingly as well.", style="text-align:justify"),
      
    style="info"),
  bsCollapsePanel("Why are fact shees not always available?", 
    p("Fact sheets are available when sufficient data are selected.
      At least 30 years of observations are required.
      For example, this means that at least 30 unique years of data must be selected in the annual time series plot.
      This does not include any explicitly withheld observations (points toggled off by the user)
      or any data falling outside a selection box drawn by the user to focus on a subset of observations.
      Note that both of these actions serve to remove observations from the displayed regression model.
      Since the same is true when downloading a customized fact sheet, the sample size requirements are similarly determined.", 
      style="text-align:justify"),
      
    p("Dynamic reports are powerful, but imperfect.
      There may be extreme edge cases where a generated report includes text that does not make sense.
      For example, these fact sheets tend to focus on summarizing change through time.
      If a user selected only one year of data, while trivial plots would still be incorpated into a fact sheet,
      document text pertaining to such temporal changes would not make any sense and the resulting report would be clearly defective.
      This is separate from other more general considerations regarding ensuring that fact sheets
      contain summaries based on relatively robust data sets.", style="text-align:justify"),
      
    p("Such a problem can be avoided with sensible data selection by the user in conjunction with the existing broader data selection requirements.
      Dynamic report generation generally attempts to strikes a balance between flexibility and rigidity.
      Flexibility allows a report to adapt to changing inputs, but it is impossible to anticipate all potential idiosyncracies in the input data.
      Some document text may have to remain relatively fixed even if the user tries to incorporate more extreme inputs.
      Nevertheless, the available fact sheets represent a highly convenient, customizable resource when app content is properly prepared by the user.", 
      style="text-align:justify"),
    style="info")
)
