
library(shiny)
library(htmltools)
library(shinydashboard)
# library(tippy)
# library(jsonlite)
library(plotly)
library(purrr)
# library(stringr)
library(DT)
# library(caret)
#library(waiter)
#library(reactlog)

#reactlogReset()
#reactlog_enable()
#reactlogShow()

#renv::activate()
#renv::snapshot()
#renv::restore()

r4Deps <- function() {
    htmlDependency(
        name = "r4",
        version = "0.1",
        src = c(file = "www"),
        script = list(
            "r4.js",
            # support tinge
            "tingle.js",
            # support Handsontable
            "handsontable.full.min.js",
            # support sliderInput
            "rangeSlider.min.js",
            # support tippy
            "popper.min.js",
            "tippy-bundle.umd.min.js",
            # support PapaParse
            "papaparse.min.js",
            # support date.format
            "date.format.js",
            # support sample-data-table
            "simple-data-table.js",
            "simple-data-table-model.js"
        ),
        stylesheet = list(
            "r4.css",
            "tingle.css",
            "handsontable.full.min.css",
            "rangeSlider.min.css",
            "simple-data-table.css",
            "simple-data-table-model.css"
        )
    )
}

#waiting_screen <- tagList(
#    spin_fading_circles(),
#    h4("R server is running, please wait...")
#)

outUI <- function(id){
    ns <- NS(id)
    if (id != 'cont7' && id != 'cont8') {
        tabsetPanel(id=ns(id),
                    tabPanel("Plot",value=ns("plot"),plotlyOutput(ns("plot"))),
                    tabPanel("Data",value=ns("data2"),DT::dataTableOutput(ns("data2")),
                             downloadButton(ns("downloaddata"),"Download")),
                    selected = ns("plot")
        )
    } else {
        tabsetPanel(id=ns(id),
                    tabPanel("Data",value=ns("data2"),DT::dataTableOutput(ns("data2")),
                             downloadButton(ns("downloaddata"),"Download")),
                    selected = ns("data2")
        )
    }
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    r4Deps(),
    tags$head(HTML("<meta http-equiv='cache-control' content='no-cache'>")),
    tags$head(HTML("<meta http-equiv='pragma' content='no-cache'>")),
    #useWaiter(),

    tags$head(HTML("<title>RegionSizeR</title>")),
    tags$head(HTML("<link rel='icon' type='image/x-icon' href='logo3.ico'>")),
    tags$body(onload="main()"),

    dashboardPage(

        ############ DASHBOARD HEADER: START ####################################################################
        dashboardHeader(
            tags$li(class = "dropdown",
                    tags$style(".main-header {max-height: 80px}")
            ),
            title=span(img(src="logo_title_new2.gif", height = 55, style="cursor:pointer", onclick="$('a[href*=\"#shiny-tab-blank\"]').tab('show');")),
            titleWidth=380
        ),
        ############ DASHBOARD HEADER: END ######################################################################

        ############ DASHBOARD SIDEBAR: START ###################################################################
        dashboardSidebar(
            width=380,

            tags$p(),
            ######## SIDEBAR MENU: START ########################################################################
            tags$div(class="row",
                     HTML('<div class="col-sm-12">'),
                     HTML(    '<ul class="sidebar-menu">'),
                     ##### SIDEBAR MENU 0: Blank page ################################################################
                     HTML(        '<li id="treeview0" class="treeview">'),
                     HTML(            '<ul class="treeview-menu" style="display: none;">'),
                     HTML(                '<li class="active">'),
                     HTML(                    '<a href="#shiny-tab-blank" data-toggle="tab"></a>'),
                     HTML(                '</li>'),
                     HTML(            '</ul>'), #ul treeview-menu
                     HTML(        '</li>'), #li treeview
                     ##### SIDEBAR MENU 1: Superiority ###############################################################
                     HTML(        '<li id="treeview1" class="treeview">'),
                     HTML(            '<a href="#">'),
                     HTML(                '<i class="icon1"></i>'),
                     HTML(                '<span>Superiority</span>'),
                     HTML(                '<i class="fa fa-angle-left pull-right" ></i>'),
                     HTML(            '</a>'),
                     HTML(            '<ul class="treeview-menu" style="display: none;">'),
                     ##### SIDEBAR MENU1 SUBMENU 1: Continuous Endpoint ---------------------------------------------#
                     HTML(                '<li>'),
                     HTML(                    '<a href="#">'),
                     HTML(                        '<i class="fa fa-angle-right"></i>'),
                     #HTML(                        '<i class="icon2"></i>'),
                     HTML(                        'Continuous Endpoint'),
                     HTML(                    '</a>'),
                     HTML(                    '<ul class="treeview-menu" style="display: none;">'),
                     ##### SIDEBAR MENU1 SUBMENU 1.1: T Test --------------------------------------------------------#
                     HTML(                        '<li>'),
                     HTML(                            '<a id="tab_con_end_t_test" href="#shiny-tab-con_end_t_test" data-toggle="tab">'),
                     HTML(                                '<i class="fa fa-angle-double-right"></i>'),
                     HTML(                                'T Test'),
                     HTML(                            '</a>'),
                     HTML(                        '</li>'),
                     HTML(                    '</ul>'),
                     HTML(                '</li>'),
                     ##### SIDEBAR MENU1 SUBMENU 2: Binary Endpoint -------------------------------------------------#
                     HTML(                '<li>'),
                     HTML(                    '<a href="#">'),
                     HTML(                        '<i class="fa fa-angle-right"></i>'),
                     #HTML(                        '<i class="icon3"></i>'),
                     HTML(                        'Binary Endpoint'),
                     HTML(                    '</a>'),
                     HTML(                    '<ul class="treeview-menu" style="display: none;">'),
                     ##### SIDEBAR MENU1 SUBMENU 2.1: Chi-square ----------------------------------------------------#
                     HTML(                        '<li>'),
                     HTML(                            '<a id="tab_bin_end_chi_square" href="#shiny-tab-bin_end_chi_square" data-toggle="tab">'),
                     HTML(                                '<i class="fa fa-angle-double-right"></i>'),
                     HTML(                                'Chi-square'),
                     HTML(                            '</a>'),
                     HTML(                        '</li>'),
                     HTML(                    '</ul>'),
                     HTML(                '</li>'),
                     ##### SIDEBAR MENU1 SUBMENU 3: Time-to-Event Endpoint ------------------------------------------#
                     HTML(                '<li>'),
                     HTML(                    '<a href="#">'),
                     HTML(                        '<i class="fa fa-angle-right"></i>'),
                     #HTML(                        '<i class="icon4"></i>'),
                     HTML(                        'Time-to-event Endpoint'),
                     HTML(                    '</a>'),
                     HTML(                    '<ul class="treeview-menu" style="display: none;">'),
                     ##### SIDEBAR MENU1 SUBMENU 3.1: Life Test -----------------------------------------------------#
                     HTML(                        '<li>'),
                     HTML(                            '<a id="tab_t2e_end_life_test" href="#shiny-tab-t2e_end_life_test" data-toggle="tab">'),
                     HTML(                                '<i class="fa fa-angle-double-right"></i>'),
                     HTML(                                'Logrank Test'),
                     HTML(                            '</a>'),
                     HTML(                        '</li>'),
                     HTML(                    '</ul>'),
                     HTML(                '</li>'),
                     #####-------------------------------------------------------------------------------------------#
                     HTML(            '</ul>'), #ul treeview-menu
                     HTML(        '</li>'), #li treeview
                     ##### SIDEBAR MENU 2: Non-inferiority ###########################################################
                     HTML(        '<li id="treeview2" class="treeview">'),
                     HTML(            '<a href="#">'),
                     HTML(                '<i class="icon5"></i>'),
                     HTML(                '<span>Non-inferiority</span>'),
                     HTML(                '<i class="fa fa-angle-left pull-right"></i>'),
                     HTML(            '</a>'),
                     HTML(            '<ul class="treeview-menu" style="display: none;">'),
                     ##### SIDEBAR MENU 2 SUBMENU 1: Continuous Endpoint --------------------------------------------#
                     HTML(                '<li>'),
                     HTML(                    '<a id="tab_ni_con_end" href="#shiny-tab-ni_con_end" data-toggle="tab">'),
                     HTML(                        '<i class="fa fa-angle-right"></i>'),
                     HTML(                        'Continuous Endpoint'),
                     HTML(                    '</a>'),
                     HTML(                '</li>'),
                     #####-------------------------------------------------------------------------------------------#
                     ##### SIDEBAR MENU 2 SUBMENU 2: Binary Endpoint ------------------------------------------------#
                     HTML(                '<li>'),
                     HTML(                    '<a href="#shiny-tab-ni_bin_end" data-toggle="tab">'),
                     HTML(                        '<i class="fa fa-angle-right"></i>'),
                     HTML(                        'Binary Endpoint'),
                     HTML(                    '</a>'),
                     HTML(                '</li>'),
                     #####-------------------------------------------------------------------------------------------#
                     ##### SIDEBAR MENU 2 SUBMENU 3: Time-to-event Endpoint -----------------------------------------#
                     HTML(                '<li>'),
                     HTML(                    '<a id="tab_ni_t2e_end" href="#shiny-tab-ni_t2e_end" data-toggle="tab">'),
                     HTML(                        '<i class="fa fa-angle-right"></i>'),
                     HTML(                        'Time-to-event Endpoint'),
                     HTML(                    '</a>'),
                     HTML(                '</li>'),
                     #####-------------------------------------------------------------------------------------------#
                     HTML(            '</ul>'), #ul treeview-menu
                     HTML(        '</li>'), #li treeview
                     ##### SIDEBAR MENU 3: MCP-Mod ###################################################################
                     HTML(        '<li id="treeview3" class="treeview">'),
                     HTML(            '<a href="#">'),
                     HTML(                '<i class="icon6"></i>'),
                     HTML(                '<span>MCP-Mod</span>'),
                     HTML(                '<i class="fa fa-angle-left pull-right"></i>'),
                     HTML(            '</a>'),
                     HTML(            '<ul class="treeview-menu" style="display: none;">'),
                     ##### SIDEBAR MENU 3 SUBMENU 1: Continuous Endpoint --------------------------------------------#
                     HTML(                '<li>'),
                     HTML(                    '<a id="tab_mcpmod_con_end" href="#shiny-tab-mcpmod_con_end" data-toggle="tab">'),
                     HTML(                        '<i class="fa fa-angle-right"></i>'),
                     HTML(                        'Continuous Endpoint'),
                     HTML(                    '</a>'),
                     HTML(                '</li>'),
                     #####-------------------------------------------------------------------------------------------#
                     ##### SIDEBAR MENU 3 SUBMENU 2: Binary Endpoint ------------------------------------------------#
                     HTML(                '<li>'),
                     HTML(                    '<a id="tab_mcpmod_bin_end" href="#shiny-tab-mcpmod_bin_end" data-toggle="tab">'),
                     HTML(                        '<i class="fa fa-angle-right"></i>'),
                     HTML(                        'Binary Endpoint'),
                     HTML(                    '</a>'),
                     HTML(                '</li>'),
                     #####-------------------------------------------------------------------------------------------#
                     HTML(            '</ul>'), #ul treeview-menu
                     HTML(        '</li>'), #li treeview
                     #####-------------------------------------------------------------------------------------------#
                     HTML(    '</ul>'), #ul sidebar-nemu
                     HTML('</div>') #col-sm-12
            ),
            ######## SIDEBAR MENU: END ##########################################################################

            tags$hr(),

            ######### SIDEBAR BOX 1: START ######################################################################
            ######### SIDEBAR BOX 1a: Global study ##############################################################
            tags$div(class="row",
                     HTML('<div id="ui_siderbar_global" class="col-sm-12">'),
                     HTML(    '<div class="box box-solid bg-blue collapsed-box uiBox">'),
                     ##### SIDEBAR BOX 1a BOXHEADER: Global study ---------------------------------------------------#
                     HTML(        '<div class="box-header uiBoxHeaderBlue">'),
                     HTML(            '<h3 class="box-title">Global study</h3>'),
                     HTML(            '<div class="box-tools pull-right">'),
                     HTML(                '<button class="btn btn-box-tool" data-widget="collapse">'),
                     HTML(                    '<i class="fa fa-plus" role="presentation" aria-label="plus icon"></i>'),
                     HTML(                '</button>'),
                     HTML(            '</div>'), #box-tools
                     HTML(        '</div>'), #box-header
                     ##### SIDEBAR BOX 1a BOXBODY : Global study ----------------------------------------------------#
                     HTML(        '<div class="box-body uiBoxBodyBlue">'),
                     ##### SIDEBAR BOX 1a BOXBODY ROW 1 -------------------------------------------------------------#
                     HTML(            '<div class="row">'),
                     ##### SIDEBAR BOX 1a BOXBODY ROW 1 BOXITEM 1: totalN - Sample size -----------------------------#
                     HTML(                '<div class="col-sm-6">'),
                     HTML(                    '<b>Sample size</b>'),
                     HTML(                    '<div class="form-group">'),
                     HTML(                        '<input id="ui_number_global_totaln" type="number" class="form-control" min="1" step="1">'),
                     HTML(                    '</div>'), #form-group
                     HTML(                '</div>'), #col-sm-6
                     ##### SIDEBAR BOX 1 BOXBODY ROW 1 BOXITEM 2: ratio - Randomization ratio
                     HTML(                '<div class="col-sm-6">'),
                     HTML(                    '<b>Randomization ratio</b>'),
                     HTML(                    '<span id="info_1_1_2" tabindex="0">'),
                     HTML(                        '<i class="fa fa-info-circle" role="presentation"></i>'),
                     HTML(                    '</span>'),
                     HTML(                    '<script>tippy("#info_1_1_2", {content: "Randomization ratio between experimental and control arms in subpopulation. For example, for 1:1 randomization, randomization ratio = 1; for 1.5:1 randomization, randomization ratio = 1.5; for 0.5:1 randomization, randomization ratio = 0.5; etc.",});'),
                     HTML(                    '</script>'),
                     HTML(                    '<div class="form-group">'),
                     HTML(                        '<input id="ui_number_gloabl_ratio" type="number" class="form-control" min="0" step="any">'),
                     HTML(                    '</div>'), #form-group
                     HTML(                '</div>'), #col-sm-6
                     HTML(            '</div>'), #row
                     ##### SIDEBAR BOX 1a BOXBODY ROW 2 -------------------------------------------------------------#
                     HTML(            '<div class="row">'),
                     ##### SIDEBAR BOX 1a BOXBODY ROW 2 BOXITEM 1: altHypo - Alternative hypothesis -----------------#
                     HTML(                '<div class="col-sm-6">'),
                     HTML(                    '<b>Alternative hypothesis</b>'),
                     HTML(                    '<div class="form-group">'),
                     HTML(                        '<select id="ui_select_global_alt_hypo" class="selectized uiSelectized">'),
                     HTML(                            '<option value="t">two.sided</option>'),
                     HTML(                            '<option value="g">greater</option>'),
                     HTML(                            '<option value="l">less</option>'),
                     HTML(                        '</select>'),
                     HTML(                    '</div>'), #form-group
                     HTML(                '</div>'), #col-sm-6
                     ##### SIDEBAR BOX 1a BOXBODY ROW 2 BOXITEM 2: Significance level(sigL) -------------------------#
                     HTML(                '<div class="col-sm-6">'),
                     HTML(                    '<b>Significance level</b>'),
                     HTML(                    '<div class="form-group">'),
                     HTML(                        '<input id="ui_number_sig_level" type="number" class="form-control" min="0" max="1" step="0.01">'),
                     HTML(                    '</div>'), #form-group
                     HTML(                '</div>'), #col-sm-6
                     #####-------------------------------------------------------------------------------------------#
                     HTML(            '</div>'), #row
                     ##### SIDEBAR BOX 1a BOXBODY ROW 3 -------------------------------------------------------------#
                     HTML(            '<div class="row">'),
                     ##### SIDEBAR BOX 1a BOXBODY ROW 3 BOXITEM 1: nim - non-infereriority margin -------------------#
                     HTML(                '<div class="col-sm-12">'),
                     HTML(                    '<fieldset id="ui_fieldset_ni_nim_hma" class="uiFieldsetWhite" style="display: none;">'),
                     HTML(                        '<legend class="uiLegendWhite">Non-Inferiority Only:</legend>'),
                     HTML(                        '<div class="col-sm-6">'),
                     HTML(                            '<label>Non-inferiority margin'),
                     HTML(                            '<span id="info_1_3_2" tabindex="0">'),
                     HTML(                                '<i class="fa fa-info-circle"></i>'),
                     HTML(                            '</span>'),
                     HTML(                            '</label>'),
                     HTML(                            '<script>tippy("#info_1_3_2", {content: "Input the absolute of margin",});'),
                     HTML(                            '</script>'),
                     HTML(                            '<div class="form-group">'),
                     HTML(                                '<input id="ui_number_ni_nim" type="number" class="form-control" min="0" max="10" step="any">'),
                     HTML(                            '</div>'), #form-group
                     HTML(                        '</div>'), #col-sm-6
                     ##### SIDEBAR BOX 1a BOXBODY ROW 3 BOXITEM 2: hma - higher means are ---------------------------#
                     HTML(                        '<div class="col-sm-6">'),
                     HTML(                            '<label>Higher values are</label>'),
                     HTML(                            '<span id="info_1_3_3" tabindex="0">'),
                     HTML(                                '<i class="fa fa-info-circle"></i>'),
                     HTML(                            '</span>'),
                     HTML(                            '<script>tippy("#info_1_3_3", {content: "1=worse, 0=better",});'),
                     HTML(                            '</script>'),
                     HTML(                            '<div class="form-group">'),
                     #HTML(                                '<input id="ui_number_ni_hma" type="number" class="form-control" min="0" max="1" step="any">'),
                     HTML(                                '<select id="ui_select_ni_hma" class="selectized uiSelectized">'),
                     HTML(                                    '<option value="0">better</option>'),
                     HTML(                                    '<option value="1">worse</option>'),
                     HTML(                                '</select>'),
                     HTML(                            '</div>'), #form-group
                     HTML(                        '</div>'), #col-sm-6
                     HTML(                    '</fieldset>'),
                     HTML(                '</div>'), #col-sm-12
                     #####-------------------------------------------------------------------------------------------#
                     HTML(            '</div>'), #row
                     #####-------------------------------------------------------------------------------------------#
                     HTML(        '</div>'), #box-body
                     HTML(    '</div>'), #box
                     HTML('</div>') #col-sm-12
            ),
            ######### SIDEBAR BOX 1a: END #######################################################################

            ######### SIDEBAR BOX 1b: Global study - MCPMOD #####################################################
            tags$div(class="row",
                     HTML('<div class="col-sm-12" id="ui_siderbar_mcpmod_global" style="display: none;">'),
                     HTML(    '<div class="box box-solid bg-blue collapsed-box uiBox">'),
                     ##### SIDEBAR BOX 1b BOXHEADER: Global study ---------------------------------------------------#
                     HTML(        '<div class="box-header uiBoxHeaderBlue">'),
                     HTML(            '<h3 class="box-title">Global study</h3>'),
                     HTML(            '<div class="box-tools pull-right">'),
                     HTML(                '<button class="btn btn-box-tool" data-widget="collapse">'),
                     HTML(                    '<i class="fa fa-plus" role="presentation" aria-label="plus icon"></i>'),
                     HTML(                '</button>'),
                     HTML(            '</div>'), #box-tools
                     HTML(        '</div>'), #box-header
                     ##### SIDEBAR BOX 1b BOXBODY : Global study ----------------------------------------------------#
                     HTML(        '<div class="box-body uiBoxBodyBlue">'),
                     ##### SIDEBAR BOX 1b BOXBODY ROW 1 -------------------------------------------------------------#
                     HTML(            '<div class="row">'),
                     HTML(                '<div class="col-sm-6">'),
                     ##### SIDEBAR BOX 1b BOXBODY ROW 1 BOXITEM 1: mcpmod_alpha - Alpha -----------------------------#
                     HTML(                    '<label>Alpha (one-sided)</label>'),
                     HTML(                    '<div class="form-group">'),
                     HTML(                        '<input id="ui_number_mcpmod_global_alpha" type="number" class="form-control" min="0" max="1" step="0.01">'),
                     HTML(                    '</div>'), #form-group
                     HTML(                '</div>'), #col-sm-6
                     ##### SIDEBAR BOX 1b BOXBODY ROW 1 BOXITEM 2: mcpmod_direction - Direction ---------------------#
                     HTML(                '<div class="col-sm-6">'),
                     HTML(                    '<label>Direction</label>'),
                     HTML(                    '<span id="info_mcpmod_direction" tabindex="0">'),
                     HTML(                        '<i class="fa fa-info-circle"></i>'),
                     HTML(                    '</span>'),
                     HTML(                    '<script>tippy("#info_mcpmod_direction", {content: "Whether an increase or decrease in the response variable is beneficial.",});'),
                     HTML(                    '</script>'),
                     HTML(                    '<div class="form-group">'),
                     HTML(                        '<select id="ui_select_mcpmod_global_direction" class="selectized uiSelectized">'),
                     HTML(                            '<option value="decreasing">decrease</option>'),
                     HTML(                            '<option value="increasing">increase</option>'),
                     HTML(                        '</select>'),
                     HTML(                    '</div>'), #form-group
                     HTML(                '</div>'), #col-sm-6
                     HTML(             '</div>'), #row
                     ##### SIDEBAR BOX 1b BOXBODY ROW 2 -------------------------------------------------------------#
                     HTML(             '<div class="row">'),
                     HTML(                 '<div class="col-sm-12">'),
                     HTML(                    '<label id="ui_label_mcpmod_global_totaln" style="text-align: right; width: 95%;">Total sample size: </label>'),
                     HTML(                 '</div>'), #col-sm-12
                     HTML(             '</div>'), #row
                     ##### SIDEBAR BOX 1b BOXBODY ROW 3 -------------------------------------------------------------#
                     HTML(             '<div class="row">'),
                     HTML(                 '<div class="col-sm-12">'),
                     ##### SIDEBAR BOX 1b BOXBODY ROW 3 BOXITEM 1: Dose level tabel ---------------------------------#
                     HTML(                     '<div id="ui_table_mcpmod_global_doselevel"></div>'),#style="overflow-x: auto;"
                     HTML(                 '</div>'), #col-sm-12
                     HTML(             '</div>'), #row
                     #####-------------------------------------------------------------------------------------------#
                     HTML(        '</div>'), #box-body
                     HTML(    '</div>'), #box
                     HTML('</div>') #col-sm-12
            ),
            ######### SIDEBAR BOX 1b: END #######################################################################

            ######### SIDEBAR BOX 2: START ######################################################################
            ######### SIDEBAR BOX 2a: Subpopulation ##############################################################
            tags$div(class="row",
                     HTML('<div id="ui_siderbar_local" class="col-sm-12">'),
                     HTML(    '<div id="ui_siderbar_box_local" class="box box-solid bg-blue collapsed-box">'),
                     ##### SIDEBAR BOX 2a BOXHEADER: Subpopulation ---------------------------------------------------#
                     HTML(        '<div class="box-header uiBoxHeaderBlue">'),
                     HTML(            '<h3 class="box-title">Subpopulation</h3>'),
                     HTML(            '<div class="box-tools pull-right">'),
                     HTML(                "<button id='ui_siderbar_box_button_local' class='btn btn-box-tool' data-widget='collapse'>"),
                     HTML(                    "<i class='fa fa-plus' role='presentation' aria-label='plus icon'></i>"),
                     HTML(                "</button>"),
                     HTML(            '</div>'), #box-title
                     HTML(        '</div>'), #box-header
                     ##### SIDEBAR BOX 2a BOXBODY: Subpopulation -----------------------------------------------------#
                     HTML(        '<div class="box-body uiBoxBodyBlue">'),
                     ##### SIDEBAR BOX 2a BOXBODY 2 ROW 1 ------------------------------------------------------------#
                     HTML(            '<div class="row">'),
                     ##### SIDEBAR BOX 2a BOXBODY 2 ROW 1 BOXITEM 1: ratio_cn - Randomization ratio ------------------#
                     HTML(                '<div class="col-sm-4">'),
                     HTML(                    "<b>Randomization ratio</b>"),
                     HTML(                    '<span id="info_2_1_1" tabindex="0">'),
                     HTML(                        '<i class="fa fa-info-circle" role="presentation"></i>'),
                     HTML(                    '</span>'),
                     HTML(                    '<script>tippy("#info_2_1_1", {content: "Randomization ratio between experimental and control arms in subpopulation. For example, for 1:1 randomization, randomization ratio = 1; for 1.5:1 randomization, randomization ratio = 1.5; for 0.5:1 randomization, randomization ratio = 0.5; etc.",});'),
                     HTML(                    '</script>'),
                     HTML(                    '<div class="form-group">'),
                     HTML(                        '<input id="ui_number_local_ratio" type="number" class="form-control" min="0" step="any">'),
                     HTML(                    '</div>'),
                     HTML(                '</div>'), #col-sm-5
                     ##### SIDEBAR BOX 2a BOXBODY ROW 1 BOXITEM 2: thd - Proportion of the overall treaatment effect preserved#
                     HTML(                '<div class="col-sm-8">'),
                     HTML(                    '<b>Proportion of the overall treatment effect preserved</b>'),
                     HTML(                    '<div class="form-group">'),
                     HTML(                        '<input id="ui_number_local_thd" type="number" class="form-control" min="0" max="1" step="any">'),
                     HTML(                    '</div>'), #form-group
                     HTML(                '</div>'), #col-sm-7
                     HTML(            '</div>'), #row
                     ##### SIDEBAR BOX 2a BOXBODY ROW 2 --------------------------------------------------------------#
                     HTML(            '<div class="row">'),
                     ##### SIDEBAR BOX 2a BOXBODY ROW 2 BOXITEM 1: china_all_in - Are all included in the global study?
                     HTML(                '<div class="col-sm-12">'),
                     HTML(                    '<b>Are all included in the global study?</b>'),
                     HTML(                    '<div class="form-group">'),
                     HTML(                        '<select id="ui_select_local_include" class="selectized uiSelectized">'),
                     HTML(                            '<option value="yes">completely included</option>'),
                     HTML(                            '<option value="no">completely excluded</option>'),
                     HTML(                            '<option value="part">partially included</option>'),
                     HTML(                        '</select>'),
                     HTML(                    '</div>'), #form-group
                     HTML(                '</div>'), # col-sm-12
                     HTML(            '</div>'), #row
                     ##### SIDEBAR BOX2a BOXBODY ROW 3 ---------------------------------------------------------------#
                     ##### NOT partially included -------------------------------------------------------------------#
                     HTML(            '<fieldset id="ui_fieldset_no_partial" style="display: block;">'),
                     HTML(                '<div class="row">'),
                     ##### SIDEBAR BOX 2a BOXBODY ROW 3 BOXITEM 1: pct_seq - N of Subpopulation /N of Global (%)
                     HTML(                    '<div class="col-sm-8">'),
                     HTML(                        '<b>N of Subpopulation /N of Global (%)</b>'),
                     HTML(                        '<span id="info_2_3_1" tabindex="0">'),
                     HTML(                            '<i class="fa fa-info-circle" role="presentation"></i>'),
                     HTML(                        '</span>'),
                     HTML(                        '<script>tippy("#info_2_3_1", {content: "Number of participants in the subpopulation divided by number of participants in the global study (%).",});'),
                     HTML(                        '</script>'),
                     HTML(                        '<div class="form-group">'),
                     HTML(                            '<input id="ui_text_local_pct_seq" type="text">'),
                     HTML(                        '</div>'), #form-group
                     HTML(                    '</div>'), #col-sm-8
                     ##### SIDEBAR BOX 2a BOXBODY ROW 3 BOXITEM 2: pct_seq_step - Step (%) ---------------------------#
                     HTML(                    '<div class="col-sm-4">'),
                     HTML(                        '<b>Step (%)</b>'),
                     HTML(                        '<div class="form-group">'),
                     HTML(                            '<input id="ui_number_local_pct_seq_step" type="number" class="form-control" min="0" max="100" step="any">'),
                     HTML(                        '</div>'), #form-group
                     HTML(                    '</div>'), #col-sm-4
                     HTML(                '</div>'), #row
                     HTML(            '</fieldset>'),
                     #####-------------------------------------------------------------------------------------------#
                     ##### partially included -----------------------------------------------------------------------#
                     ##### SIDEBAR BOX2a BOXBODY ROW 4 ---------------------------------------------------------------#
                     HTML(            '<fieldset id="ui_fieldset_partial" style="display: none;">'),
                     HTML(                '<div class="row">'),
                     ##### SIDEBAR BOX 2a BOXBODY ROW 4 BOXITEM 1: pct_seq - N of Subpopulation /N of Global (%)
                     HTML(                    '<div class="col-sm-12">'),
                     HTML(                        '<b>N of Subpopulation /N of Global (%)</b>'),
                     HTML(                        '<span id="info_2_4_1" tabindex="0">'),
                     HTML(                            '<i class="fa fa-info-circle" role="presentation"></i>'),
                     HTML(                        '</span>'),
                     HTML(                        '<script>tippy("#info_2_4_1", {content: "Number of participants in the subpopulation (within and outside the global study) divided by number of participants in the global study (%). ",});'),
                     HTML(                        '</script>'),
                     HTML(                        '<div class="form-group">'),
                     HTML(                            '<input id="ui_number_local_pct" type="number" class="form-control" min="0" max="100" step="any">'),
                     HTML(                        '</div>'), #form-group
                     HTML(                    '</div>'), #col-sm-12
                     HTML(                '</div>'), #row
                     HTML(                '<div class="row">'),
                     ##### SIDEBAR BOX 2a BOXBODY ROW 5 BOXITEM 1: pct_seq - N of Subpopulation within Global /N of Subpopulation (%)
                     HTML(                    '<div class="col-sm-8">'),
                     HTML(                        '<b>N of Subpopulation within Global /N of Subpopulation (%)</b>'),
                     HTML(                        '<span id="info_2_5_1" tabindex="0">'),
                     HTML(                            '<i class="fa fa-info-circle" role="presentation"></i>'),
                     HTML(                        '</span>'),
                     HTML(                        '<script>tippy("#info_2_5_1", {content: "Number of participants in the subpopulation included in the global study divided by number of participants in the subpopulation (%).",});'),
                     HTML(                        '</script>'),
                     HTML(                        '<div class="form-group">'),
                     HTML(                            '<input id="ui_text_local_pct_in_seq" type="text">'),
                     HTML(                        '</div>'), #form-group
                     HTML(                    '</div>'), #col-sm-8
                     ##### SIDEBAR BOX 2a BOXBODY ROW 5 BOXITEM 2: pct_in_seq_step - Step (%) ------------------------#
                     HTML(                    '<div class="col-sm-4">'),
                     HTML(                        '<b>Step (%)</b>'),
                     HTML(                        '<div class="form-group">'),
                     HTML(                            '<input id="ui_number_local_pct_in_seq_step" type="number" class="form-control" min="0" max="100" step="any">'),
                     HTML(                        '</div>'), #form-group
                     HTML(                    '</div>'), #col-sm-4
                     HTML(                '</div>'), #row
                     HTML(            '</fieldset>'),
                     #####-------------------------------------------------------------------------------------------#
                     HTML(        '</div>'), #box-body
                     HTML(    '</div>'), #box
                     HTML('</div>') #col-sm-12
            ),
            ######### SIDEBAR BOX 2a: END ########################################################################

            ######### SIDEBAR BOX 2b: Subpopulation ##############################################################
            tags$div(class="row",
                     HTML('<div id="ui_siderbar_mcpmod_local" class="col-sm-12" style="display: none;">'),
                     HTML(    '<div class="box box-solid bg-blue collapsed-box">'),
                     ##### SIDEBAR BOX 2b BOXHEADER: Subpopulation ---------------------------------------------------#
                     HTML(        '<div class="box-header uiBoxHeaderBlue">'),
                     HTML(            '<h3 class="box-title">Subpopulation</h3>'),
                     HTML(            '<div class="box-tools pull-right">'),
                     HTML(                "<button class='btn btn-box-tool' data-widget='collapse'>"),
                     HTML(                    "<i class='fa fa-plus' role='presentation' aria-label='plus icon'></i>"),
                     HTML(                "</button>"),
                     HTML(            '</div>'), #box-title
                     HTML(        '</div>'), #box-header
                     ##### SIDEBAR BOX 2b BOXBODY: Subpopulation -----------------------------------------------------#
                     HTML(        '<div class="box-body uiBoxBodyBlue">'),
                     ##### SIDEBAR BOX 2b BOXBODY 2b ROW 1 ------------------------------------------------------------#
                     HTML(            '<div class="row">'),
                     ##### SIDEBAR BOX 2b BOXBODY 2b ROW 1 BOXITEM 1: thd - Proportion of the overall treaatment effect preserved#
                     HTML(                '<div class="col-sm-8">'),
                     HTML(                    '<b>Proportion of the overall treatment effect preserved</b>'),
                     HTML(                '</div>'), #col-sm-8
                     HTML(                '<div class="col-sm-4">'),
                     HTML(                    '<div class="form-group">'),
                     HTML(                        '<input id="ui_number_mcpmod_local_pi" type="number" class="form-control" min="0" max="1" step="any">'),
                     HTML(                    '</div>'), #form-group
                     HTML(                '</div>'), #col-sm-4
                     HTML(            '</div>'), #row
                     ##### SIDEBAR BOX 2b BOXBODY 2b ROW 2 --------------------------------------------------------------#
                     HTML(            '<div class="row">'),
                     HTML(                '<div class="col-sm-6">'),
                     HTML(                    '<label id="ui_label_mcpmod_global_totaln_clone" style="text-align: right; width: 90%;">Total sample size:</label>'),
                     HTML(                '</div>'), #col-sm-6
                     HTML(                '<div class="col-sm-6">'),
                     HTML(                    '<label id="ui_label_mcpmod_local_totaln" style="text-align: right; width: 95%;">Total sample size:</label>'),
                     HTML(                '</div>'), #col-sm-6
                     HTML(            '</div>'), #row
                     ##### SIDEBAR BOX 2b BOXBODY 2b ROW 3 -------------------------------------------------------------#
                     HTML(            '<div class="row">'),
                     HTML(                '<div class="col-sm-12">'),
                     ##### SIDEBAR BOX 2b BOXBODY 2b ROW 3 BOXITEM 1: Dose level tabel ---------------------------------#
                     HTML(                    '<div id="ui_table_mcpmod_local_doselevel"></div>'),#style="overflow-x: auto;"
                     HTML(                '</div>'), #col-sm-12
                     HTML(            '</div>'), #row
                     ##### SIDEBAR BOX 2b BOXBODY 2b ROW 4 ------------------------------------------------------------#
                     HTML(            '<br>'),
                     HTML(            '<div class="row">'),
                     ##### SIDEBAR BOX 2b BOXBODY 2b ROW 4 BOXITEM 1: pct - N of Subpopulation /N of Global (%)
                     HTML(                '<div class="col-sm-8">'),
                     HTML(                    '<b>N of Subpopulation /N of Global (%)'),
                     HTML(                    '</b>'),
                     HTML(                '</div>'), #col-sm-8
                     HTML(                '<div class="col-sm-4">'),
                     HTML(                    '<div class="form-group">'),
                     HTML(                        '<input id="ui_number_mcpmod_local_pct" type="number" class="form-control" disabled="disabled" style="background-color:#509DC8;color:white">'),
                     HTML(                    '</div>'), #form-group
                     HTML(                '</div>'), #col-sm-4
                     HTML(            '</div>'), #row
                     #####-------------------------------------------------------------------------------------------#
                     HTML(        '</div>'), #box-body
                     HTML(    '</div>'), #box
                     HTML('</div>') #col-sm-12
            ),
            ######## SIDEBAR BOX 2b: END ########################################################################

            ######## SIDEBAR BOX 3: START #######################################################################
            ######## SIDEBAR BOX 3: Simulation options ##########################################################
            tags$div(class="row",
                     HTML('<div class="col-sm-12">'),
                     HTML(    '<div class="box box-solid bg-blue collapsed-box">'),
                     ##### SIDEBAR BOX 3 BOXHEADER: Simulation options ----------------------------------------------#
                     HTML(        '<div class="box-header uiBoxHeaderBlue">'),
                     HTML(            '<h3 class="box-title">Simulation options</h3>'),
                     HTML(            '<div class="box-tools pull-right">'),
                     HTML(                '<button class="btn btn-box-tool" data-widget="collapse">'),
                     HTML(                    '<i class="fa fa-plus" role="presentation" aria-label="plus icon"></i>'),
                     HTML(                '</button>'),
                     HTML(            '</div>'), #box-tools
                     HTML(        '</div>'), #box-header
                     ##### SIDEBAR BOX 3 BOXBODY: Simulation options ------------------------------------------------#
                     HTML(        '<div class="box-body uiBoxBodyBlue">'),
                     HTML(        '<fieldset id="simulation_info">'),
                     ##### SIDEBAR BOX 3 BOXBODY ROW 1 --------------------------------------------------------------#
                     HTML(            '<div class="row">'),
                     ##### SIDEBAR BOX 3 BOXBODY ROW 1 BOXITEM 1: simN - Number of simulations ----------------------#
                     HTML(                '<div class="col-sm-6">'),
                     HTML(                    '<b>Number of simulations</b>'),
                     HTML(                    '<div class="form-group">'),
                     HTML(                        '<input id="ui_number_option_simn" type="number" class="form-control" value="1000" min="0" step="1">'),
                     HTML(                    '</div>'), #form-group
                     HTML(                '</div>'), #col-sm-6
                     ##### SIDEBAR BOX 3 BOXBODY ROW 1 BOXITEM 2: seed - Seed ---------------------------------------#
                     HTML(                '<div class="col-sm-6">'),
                     HTML(                    '<b>Seed</b>'),
                     HTML(                    '<span id="info_3_1_2" tabindex="0">'),
                     HTML(                        "<i class='fa fa-info-circle' role='presentation' aria-label='info-circle icon'></i>"),
                     HTML(                    "</span>"),
                     HTML(                    '<script>tippy("#info_3_1_2", {content: "Using current system time(last five numbers) as default.",});'),
                     HTML(                    '</script>'),
                     HTML(                    '<div class="form-group">'),
                     HTML(                        '<input id="ui_number_option_seed" type="number" class="form-control" value="30439" min="0" step="1">'),
                     HTML(                    '</div>'), #form-group
                     HTML(                '</div>'), #col-sm-6
                     HTML(            '</div>'), #row
                     #####-------------------------------------------------------------------------------------------#
                     HTML(        '</fieldset>'),
                     HTML(        '</div>'), #boxbody
                     HTML(    '</div>'), #box
                     HTML('</div>') #col-sm-12
            ),
            ######## SIDEBAR BOX 3: END #########################################################################
            tags$hr(class='hr_half'),
            ######### SIDEBAR BOX 4: Reset ######################################################################
            tags$div(class="row",
                     HTML('<div class="col-sm-12">'),
                     HTML(    '<div class="box box-solid bg-blue collapsed-box uiBox">'),
                     ##### SIDEBAR BOX 1 BOXHEADER: Reset -----------------------------------------------------------#
                     HTML(        '<div class="box-header uiBoxHeaderBlue">'),
                     HTML(            '<h3 class="box-title">Reset inputs</h3>'),
                     HTML(            '<div class="box-tools pull-right">'),
                     HTML(                '<button class="btn btn-box-tool" data-widget="collapse">'),
                     HTML(                    '<i class="fa fa-plus" role="presentation" aria-label="plus icon"></i>'),
                     HTML(                '</button>'),
                     HTML(            '</div>'), #box-tools
                     HTML(        '</div>'), #box-header
                     ##### SIDEBAR BOX 1 BOXBODY : Reset ------------------------------------------------------------#
                     HTML(        '<div class="box-body uiBoxBodyBlue">'),
                     ##### SIDEBAR BOX 1 BOXBODY ROW 1 --------------------------------------------------------------#
                     HTML(            '<div class="row-sm">'),
                     HTML(                '<div class="col-sm">'),
                     HTML(                    '<div class="form-group">'),
                     HTML(                        '<b>Please upload historical data file</b>'),
                     HTML(                        '<input type="file" class="form-control" accept=".csv">'),
                     HTML(                    '</div>'),
                     HTML(                '</div>'),
                     HTML(            '</div>'),
                     ##### SIDEBAR BOX 1 BOXBODY ROW 2 --------------------------------------------------------------#
                     HTML(            '<div class="row-sm">'),
                     HTML(                '<div class="col-sm-6">'),
                     HTML(                    '<button id="ui_btn_default_reset" class="btn btn-default">Use default value reset</button>'),
                     HTML(                '</div>'),
                     HTML(                '<div class="col-sm-6">'),
                     HTML(                    '<button id="ui_btn_history_reset" class="btn btn-default" style="display: none;">Use historical value reset</button>'),
                     HTML(                '</div>'),
                     HTML(            '</div>'), #row-sm
                     HTML(        '</div>'), #box-body
                     HTML(    '</div>'), #box
                     HTML('</div>') #col-sm-12
            )
            ######## SIDEBAR BOX 4: END #########################################################################
        ),
        ############ DASHBOARD SIDEBAR: END #####################################################################


        ############ DASHBOARD BODY: START ######################################################################
        dashboardBody(
            ######## BOX: START #################################################################################
            tags$div(class="row",
                     HTML('<div class="col-sm-12">'),
                     HTML(    '<div class="box box-solid box-primary">'),
                     ##### BODY BOX BOXHEADER -----------------------------------------------------------------------#
                     #HTML(        '<div class="box-header">'),
                     #HTML(            '<label class="box-title"></label>'),
                     #HTML(        '</div>'), #box-header
                     ##### BODY BOX BOXBODY -------------------------------------------------------------------------#
                     HTML(        '<div class="box-body uiBodyBoxBody">'),
                     ##### BODY BOX BOXBODY ROW 1 -------------------------------------------------------------------#
                     HTML(            '<div class="row">'),
                     HTML(                '<div class="col-sm-12">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE: START #####################################################
                     HTML(                    '<div class="tab-content" id="body-tabs">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 0: START ###################################################
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 0: Welcome #################################################
                     HTML(                        '<div class="tab-pane active" id="shiny-tab-blank" role="tabpanel">'),
                     HTML(                            '<div class="box box-solid box-primary">'),
                     HTML(                                '<div class="box-header">'),
                     HTML(                                    '<h1 class="box-title"><font size="22">Welcome!</font></h1>'),
                     HTML(                                '</div>'), #box-header
                     HTML(                                '<div class="box-body">'),
                     HTML(                                    '<div class="row">'),
                     HTML(                                        '<div class="col-sm-12">'),
                     HTML(                                            '<img src="background2.jpg" style="height: 100%; width: 100%; object-fit: contain; opacity: 0.1;">'),
                     HTML(                                            '<span style="position: absolute; top: 10%; left: 10%; width: 80%; height: 80%;">'),
                     HTML(                                                '<ul>'),
                     HTML(                                                    '<li style="font-size: 140%;"><b style="color:#eb68a3;">RegionSizeR</b> is a Shiny application designed for planning the sample size for regions in a multi-regional clinical trial, utilizing the preservation of treatment effect method outlined in ICH E17.</li>'),
                     HTML(                                                    '<li style="font-size: 140%;">This application supports various types of endpoints, including continuous, binary, and time-to-event, for superiority, non-inferiority, and MCP-Mod designs. It also considers specific enrollment characteristics in the region of interest during the planning process.</li>'),
                     HTML(                                                    '<li style="font-size: 140%;">For more information about the user tips, please click the button'),
                     HTML(                                                        '<img src="userTip4.gif" style="cursor: pointer;" title="RegionSizeR_User_Tips.docx" onclick="openUserTips()">'),
                     HTML(                                                        '. If you have any questions when using RegionSizeR, please feel free to contact us via <a href="mailto:regionsizer@bayer.com">regionsizer@bayer.com</a>.'),
                     HTML(                                                    '</li>'),
                     HTML(                                                    '<li style="font-size: 140%;">Note: Despite our validation efforts, it is not guaranteed that all scenarios have been tested. Therefore, we recommend that users perform their own validation based on specific circumstances before using it for any official purposes.</li>'),
                     HTML(                                                '</ul>'),
                     HTML(                                            '</span>'),
                     HTML(                                        '</div>'),
                     HTML(                                    '</div>'),
                     HTML(                                '</div>'), #box-body
                     HTML(                            '</div>'), #box
                     HTML(                        '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 0: END #####################################################

                     ##### BODY BOX BOXBODY ROW 1 TABPANE 1: START ###################################################
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 1: Superiority > Continuous Endpoint >> T Test #############
                     HTML(                        '<div class="tab-pane" id="shiny-tab-con_end_t_test">'),
                     HTML(                            '<div class="box box-solid box-primary">'),
                     HTML(                                '<div class="box-header">'),
                     HTML(                                    '<h3 class="box-title"><b>Superiority > Continuous Endpoint >> T Test</b></h3>'),
                     HTML(                                '</div>'), #box-header
                     HTML(                                '<div class="box-body">'),
                     HTML(                                    '<div class="tab-pane" id="shiny-tab-con_end_t_test">'),
                     HTML(                                        '<div class="nav-tabs-custom">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 1 NAV: START ----------------------------------------------#
                     HTML(                                            '<ul id="tabset1" class="nav nav-pills" role="tablist">'),
                     HTML(                                                '<li><a href="#tab-input_con_end_t_test-1" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-list"></i> Input'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<li><a href="#tab-input_con_end_t_test-2" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-info-circle"></i> Summary'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<li><a href="#tab-input_con_end_t_test-3" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-poll"></i> Output'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<!--<li class="header pull-right">T Test</li>-->'),
                     HTML(                                            '</ul>'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 1 NAV: END ------------------------------------------------#
                     HTML(                                            '<div class="tab-content">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 1 SUBPANEL 1: Continuous Endpoint - T Test - Input --------#
                     HTML(                                                '<div class="tab-pane fade in active" id="tab-input_con_end_t_test-1">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 1 SUBPANEL 1 ROW 1 ----------------------------------------#
                     HTML(                                                    '<div class="row-sm">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 1 SUBPANEL 1 ROW 1 FIELDSET 1: Experimental Arm -----------#
                     HTML(                                                        '<div class="col-sm-6">'),
                     HTML(                                                            '<div class="form-group">'),
                     HTML(                                                                '<fieldset class="uiFieldsetBlack">'),
                     HTML(                                                                    '<legend class="uiLegendBlack">Experimental Arm</legend>'),
                     HTML(                                                                    '<b>Mean</b>'),
                     HTML(                                                                    '<input id="ui_number_t_test_mean_trt" type="number" class="form-control" min="0" step="any">'),
                     HTML(                                                                    '<b>Standard Deviation</b>'),
                     HTML(                                                                    '<input id="ui_number_t_test_sd_trt" type="number" class="form-control" min="0" step="any">'),
                     HTML(                                                                '</fieldset>'),
                     HTML(                                                            '</div>'), #form-group
                     HTML(                                                        '</div>'), #col-sm-6
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 1 SUBPANEL 1 ROW 1 FIELDSET 2: Control Arm ----------------#
                     HTML(                                                        '<div class="col-sm-6">'),
                     HTML(                                                            '<div class="form-group">'),
                     HTML(                                                                '<fieldset class="uiFieldsetBlack">'),
                     HTML(                                                                    '<legend class="uiLegendBlack">Control Arm</legend>'),
                     HTML(                                                                        '<b>Mean</b>'),
                     HTML(                                                                        '<input id="ui_number_t_test_mean_ctrl" type="number" class="form-control" min="0" step="any">'),
                     HTML(                                                                        '<b>Standard Deviation</b>'),
                     HTML(                                                                        '<input id="ui_number_t_test_sd_ctrl" type="number" class="form-control" min="0" step="any">'),
                     HTML(                                                                '</fieldset>'),
                     HTML(                                                            '</div>'), #form-group
                     HTML(                                                        '</div>'), #col-sm-6
                     HTML(                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 1 SUBPANEL 1 ROW 2: Check Button --------------------------#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div class="col-auto" style="text-align: center">'),
                     HTML(                                                            '<button id="ui_btn_con_end_t_test_check" class="btn btn-primary uiCheckBtn">Check Input</button>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     HTML(                                                '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 1 SUBPANEL 2: Continuous Endpoint - T Test - Summary ------#
                     HTML(                                                '<div class="tab-pane fade" id="tab-input_con_end_t_test-2">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 1 SUBPANEL 2 ROW 1: T Test Summary ------------------------#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div class="col-sm">'),
                     #HTML(                                                            'T Test Summary'),
                     HTML(                                                            '<div id="summary_track_table1"></div>'),
                     HTML(                                                        '</div>'), #col-sm
                     HTML(                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 1 SUBPANEL 2 ROW 2: T Test R code -------------------------#
                     HTML(                                                    '<div class="row-sm uiKeyArea">'),
                     HTML(                                                        '<div class="col-auto">'),
                     HTML(                                                            '<pre>'),
                     HTML(                                                                '<textarea id="ui_textarea_con_end_t_test" class="form-control " rows="7" cols="80" readonly=true>'),
                     HTML(                                                                '</textarea>'),
                     HTML(                                                            '</pre>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 1 SUBPANEL 2 ROW 3: Run Button ----------------------------#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div class="col-auto" style="text-align: center">'),
                     HTML(                                                            '<button id="ui_btn_con_end_t_test_refresh" type="button" class="btn btn-primary">Refresh</button>'),
                     HTML(                                                            '<button id="ui_btn_con_end_t_test_download" type="button" class="btn btn-primary">Download</button>'),
                     HTML(                                                            '<button id="ui_btn_con_end_t_test_run" type="button" class="btn btn-primary action-button  uiCheckBtn">Run</button>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     HTML(                                                '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 1 SUBPANEL 3: Continuous Endpoint - T Test - Output -------#
                     HTML(                                                '<div class="tab-pane fade" id="tab-input_con_end_t_test-3">'),
                     outUI("cont1"),
                     HTML(                                                '</div>'), #tab-pane
                     #####-------------------------------------------------------------------------------------------#
                     HTML(                                            '</div>'), #tab-content
                     HTML(                                        '</div>'), #nav-tabs-custom
                     HTML(                                    '</div>'), #tab-pane
                     HTML(                                '</div>'), #box-body
                     HTML(                            '</div>'), #box
                     HTML(                        '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 1: END #####################################################

                     ##### BODY BOX BOXBODY ROW 1 TABPANE 2: START ###################################################
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 2: Superiority > Binary Endpoint >> Chi-square #############
                     HTML(                        '<div class="tab-pane" id="shiny-tab-bin_end_chi_square">'),
                     HTML(                            '<div class="box box-solid box-primary">'),
                     HTML(                                '<div class="box-header">'),
                     HTML(                                    '<h3 class="box-title"><b>Superiority > Binary Endpoint >> Chi-square</b></h3>'),
                     HTML(                                '</div>'), #box-header
                     HTML(                                '<div class="box-body">'),
                     HTML(                                    '<div class="tab-pane" id="shiny-tab-bin_end_chi_square">'),
                     HTML(                                        '<div class="nav-tabs-custom">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 2 NAV: START ----------------------------------------------#
                     HTML(                                            '<ul id="tabset3" class="nav nav-pills" role="tablist">'),
                     HTML(                                                '<li><a href="#tab-input_bin_end_chi_square-1" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-list"></i> Input'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<li><a href="#tab-input_bin_end_chi_square-2" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-info-circle"></i> Summary'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<li><a href="#tab-input_bin_end_chi_square-3" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-poll"></i> Output'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<!--<li class="header pull-right">Chi-square</li>-->'),
                     HTML(                                            '</ul>'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 2 NAV: END ------------------------------------------------#
                     HTML(                                            '<div class="tab-content">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 2 SUBPANEL 1: Binary Endpoint - Chi-square - Input --------#
                     HTML(                                                '<div class="tab-pane fade in active" id="tab-input_bin_end_chi_square-1">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 2 SUBPANEL 1 ROW 1 ----------------------------------------#
                     HTML(                                                    '<div class="row-sm">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 2 SUBPANEL 1 ROW 1 FIELDSET 1: Experimental Arm -----------#
                     HTML(                                                        '<div class="col-sm-6">'),
                     HTML(                                                            '<div class="form-group">'),
                     HTML(                                                                '<fieldset class="uiFieldsetBlack">'),
                     HTML(                                                                    '<legend class="uiLegendBlack">Experimental Arm</legend>'),
                     HTML(                                                                    '<b>Proportion</b>'),
                     HTML(                                                                    '<input id="ui_number_chi_square_prop_trt" type="number" class="form-control" min="0" max="1" step="0.1">'),
                     HTML(                                                                '</fieldset>'),
                     HTML(                                                            '</div>'), #form-group
                     HTML(                                                        '</div>'), #col-sm-6
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 2 SUBPANEL 1 ROW 1 FIELDSET 2: Control Arm ----------------#
                     HTML(                                                        '<div class="col-sm-6">'),
                     HTML(                                                            '<div class="form-group">'),
                     HTML(                                                                '<fieldset class="uiFieldsetBlack">'),
                     HTML(                                                                    '<legend class="uiLegendBlack">Control Arm</legend>'),
                     HTML(                                                                        '<b>Proportion</b>'),
                     HTML(                                                                        '<input id="ui_number_chi_square_prop_ctrl" type="number" class="form-control" min="0" max="1" step="0.1">'),
                     HTML(                                                                '</fieldset>'),
                     HTML(                                                            '</div>'), #form-group
                     HTML(                                                        '</div>'), #col-sm-6
                     HTML(                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 2 SUBPANEL 1 ROW 2: Check Button---------------------------#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div class="col-auto" style="text-align: center">'),
                     HTML(                                                            '<button id="ui_btn_bin_end_chi_square_check" class="btn btn-primary uiCheckBtn">Check Input</button>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     HTML(                                                '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 2 SUBPANEL 2: Binary Endpoint - Chi-square - Summary ------#
                     HTML(                                                '<div class="tab-pane fade" id="tab-input_bin_end_chi_square-2">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 2 SUBPANEL 2 ROW 1: Chi-square Summary --------------------#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div class="col-sm">'),
                     #HTML(                                                            'Chi-square Summary'),
                     HTML(                                                            '<div id="summary_track_table2"></div>'),
                     HTML(                                                        '</div>'), #col-sm
                     HTML(                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 2 SUBPANEL 2 ROW 2: Chi-square R code -------------------------#
                     HTML(                                                    '<div class="row-sm uiKeyArea">'),
                     HTML(                                                        '<div class="col-auto">'),
                     HTML(                                                            '<pre>'),
                     HTML(                                                                '<textarea id="ui_textarea_bin_end_chi_square" class="form-control " rows="7" cols="80" readonly=true>'),
                     HTML(                                                                '</textarea>'),
                     HTML(                                                            '</pre>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 2 SUBPANEL 2 ROW 3: Run Button ----------------------------#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div class="col-auto" style="text-align: center">'),
                     HTML(                                                            '<button id="ui_btn_bin_end_chi_square_refresh" class="btn btn-primary">Refresh</button>'),
                     HTML(                                                            '<button id="ui_btn_bin_end_chi_square_download" class="btn btn-primary">Download</button>'),
                     HTML(                                                            '<button id="ui_btn_bin_end_chi_square_run" type="button" class="btn btn-primary action-button  uiCheckBtn">Run</button>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     HTML(                                                '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 2 SUBPANEL 3: Binary Endpoint - Chi-square - Output -------#
                     HTML(                                                '<div class="tab-pane fade" id="tab-input_bin_end_chi_square-3">'),
                     outUI("cont2"),
                     HTML(                                                '</div>'), #tab-pane
                     #####-------------------------------------------------------------------------------------------#
                     HTML(                                            '</div>'), #tab-content
                     HTML(                                        '</div>'), #nav-tabs-custom
                     HTML(                                    '</div>'), #tab-pane
                     HTML(                                '</div>'), #box-body
                     HTML(                            '</div>'), #box
                     HTML(                        '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 2: END #####################################################

                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3: START ###################################################
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3: Time-to-event Endpoint - Life test ######################
                     HTML(                        '<div class="tab-pane" id="shiny-tab-t2e_end_life_test">'),
                     HTML(                            '<div class="box box-solid box-primary">'),
                     HTML(                                '<div class="box-header">'),
                     HTML(                                    '<h3 class="box-title"><b>Superiority > Time-to-Event Endpoint >> Logrank Test</b></h3>'),
                     HTML(                                '</div>'), #box-header
                     HTML(                                '<div class="box-body">'),
                     HTML(                                    '<div class="tab-pane" id="shiny-tab-t2e_end_life_test">'),
                     HTML(                                        '<div class="nav-tabs-custom">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 NAV: START ----------------------------------------------#
                     HTML(                                            '<ul id="tabset4" class="nav nav-pills" role="tablist">'),
                     HTML(                                                '<li><a href="#tab-input_t2e_end_life_test-1" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-list"></i> Input'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<li><a href="#tab-input_t2e_end_life_test-2" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-info-circle"></i> Summary'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<li><a href="#tab-input_t2e_end_life_test-3" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-chart-line"></i> Output'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<!--<li class="header pull-right">Life Test</li>-->'),
                     HTML(                                            '</ul>'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 NAV: END ------------------------------------------------#
                     HTML(                                            '<div class="tab-content">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 SUBPANEL 1: Time-to-event Endpoint - Life test - Input --#
                     HTML(                                                '<div class="tab-pane fade in active" id="tab-input_t2e_end_life_test-1">'),
                     HTML(                                                    '<div class="row-sm">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 SUBPANEL 1 ROW 1 FIELDSET 1: Parameters in global study -#
                     HTML(                                                        '<div class="col-sm-6">'),
                     HTML(                                                            '<div class="form-group">'),
                     HTML(                                                                '<fieldset class="uiFieldsetBlack">'),
                     HTML(                                                                    '<legend class="uiLegendBlack">Parameters in global study</legend>'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 SUBPANEL 1 ROW 1 FIELDSET 1 ROW 1 --------------------#
                     HTML(                                                                    '<div class="row-sm">'),
                     HTML(                                                                        '<div class="col-sm-6">'),
                     HTML(                                                                            '<b>Accrual time (mths.)</b>'),
                     HTML(                                                                            '<input id="ui_number_life_test_enroll_len" type="number" class="form-control" min="1" step="1">'),
                     HTML(                                                                        '</div>'), #col-sm-6
                     HTML(                                                                        '<div class="col-sm-6">'),
                     HTML(                                                                            '<b>N of target event</b>'),
                     HTML(                                                                            '<span id="info_tab_3_1_1" tabindex="0">'),
                     HTML(                                                                                "<i class='fa fa-info-circle' role='presentation' aria-label='info-circle icon'></i>"),
                     HTML(                                                                            '</span>'),
                     HTML(                                                                            '<script>tippy("#info_tab_3_1_1", {content: "Total number of events required in global study at primary completion.",});'),
                     HTML(                                                                            '</script>'),
                     HTML(                                                                            '<input id="ui_number_life_test_n_targetevent" type="number" class="form-control" min="1" step="1">'),
                     HTML(                                                                        '</div>'), #col-sm-6
                     HTML(                                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 SUBPANEL 1 ROW 1 FIELDSET 1 ROW 2 --------------------#
                     HTML(                                                                    '<div class="row-sm">'),
                     HTML(                                                                        '<div class="form-group">'),
                     HTML(                                                                            '<fieldset class="uiFieldsetBlack">'),
                     HTML(                                                                                '<legend class="uiSubLegendBlack">Control</legend>'),
                     HTML(                                                                                '<div class="row-sm-12" style="white-space: nowrap;">'),
                     HTML(                                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                                            '<input type="radio" id="ui_radio_life_test_median_type_m" name="ui_radio_group_median_rate_type" value="1">'),
                     HTML(                                                                                            '<b>Median (mths.)</b>'),
                     HTML(                                                                                            '<span id="info_median_rate_type_m" tabindex="0">'),
                     HTML(                                                                                                "<i class='fa fa-info-circle' role='presentation' aria-label='info-circle icon'></i>"),
                     HTML(                                                                                            '</span>'),
                     HTML(                                                                                            '<script>tippy("#info_median_rate_type_m", {content: "Median time in control arm (months).",});'),
                     HTML(                                                                                            '</script>'),
                     HTML(                                                                                    '</div>'), #col-sm-6
                     HTML(                                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                                        '<input id="ui_number_life_test_median_rate_m" class="form-control" type="number" min="1" max="500" step="any">'),
                     HTML(                                                                                    '</div>'), #col-sm-6
                     HTML(                                                                                '</div>'), #row-sm-12
                     HTML(                                                                                '<div class="row-sm-12">'),
                     HTML(                                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                                            '<input type="radio" id="ui_radio_life_test_median_type_y" name="ui_radio_group_median_rate_type" value="0">'),
                     HTML(                                                                                            '<b>Yearly rate</b>'),
                     HTML(                                                                                            '<span id="info_median_rate_type_y" tabindex="0">'),
                     HTML(                                                                                                "<i class='fa fa-info-circle' role='presentation' aria-label='info-circle icon'></i>"),
                     HTML(                                                                                            '</span>'),
                     HTML(                                                                                            '<script>tippy("#info_median_rate_type_y", {content: "1-year cumulative incidence rate with a scalar in [0,1) in control arm.",});'),
                     HTML(                                                                                            '</script>'),
                     HTML(                                                                                    '</div>'), #col-sm-6
                     HTML(                                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                                        '<input id="ui_number_life_test_median_rate_y" class="form-control" type="number" min="0" max="0.99999" step="0.05">'),
                     HTML(                                                                                    '</div>'), #col-sm-6
                     HTML(                                                                                '</div>'), #row-sm-12
                     HTML(                                                                            '</fieldset>'),
                     HTML(                                                                        '</div>'), #form-group
                     HTML(                                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 SUBPANEL 1 ROW 1 FIELDSET 1 ROW 3 --------------------#
                     HTML(                                                                    '<div class="row-sm">'),
                     HTML(                                                                        '<div class="form-group">'),
                     HTML(                                                                            '<fieldset class="uiFieldsetBlack">'),
                     HTML(                                                                                '<legend class="uiSubLegendBlack">Yearly dropout rate</legend>'),
                     HTML(                                                                                '<div class="row-sm-12">'),
                     HTML(                                                                                  '<div class="col-sm-6">'),
                     HTML(                                                                                        '<b>Control</b>'),
                     HTML(                                                                                        '<span id="info_tab_3_1_4" tabindex="0">'),
                     HTML(                                                                                            "<i class='fa fa-info-circle' role='presentation' aria-label='info-circle icon'></i>"),
                     HTML(                                                                                        '</span>'),
                     HTML(                                                                                        '<script>tippy("#info_tab_3_1_4", {content: "A scalar in [0,1) representing the fraction of participants lost to follow-up prior to event per year. For example, yearly dropout rate=0.05 means 5% of drops out prior to documentation of events per year.",});'),
                     HTML(                                                                                        '</script>'),
                     HTML(                                                                                        '<input id="ui_number_life_test_drop_ctrl" type="number" class="form-control" min="0" max="1" step="any">'),
                     HTML(                                                                                    '</div>'), #col-sm-6
                     HTML(                                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                                        '<b>Experimental</b>'),
                     HTML(                                                                                        '<input id="ui_number_life_test_drop_trt" type="number" class="form-control" min="0" max="1" step="any">'),
                     HTML(                                                                                    '</div>'), #col-sm-6
                     HTML(                                                                                '</div>'), #row-sm-12
                     HTML(                                                                            '</fieldset>'),
                     HTML(                                                                        '</div>'), #form-group
                     HTML(                                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 SUBPANEL 1 ROW 1 FIELDSET 1 ROW 4 --------------------#
                     HTML(                                                                    '<div class="row-sm">'),
                     HTML(                                                                        '<div class="col-sm-12">'),
                     HTML(                                                                            '<b>Target HR</b>'),
                     HTML(                                                                            '<span id="info_tab_3_1_5" tabindex="0">'),
                     HTML(                                                                                "<i class='fa fa-info-circle' role='presentation' aria-label='info-circle icon'></i>"),
                     HTML(                                                                            '</span>'),
                     HTML(                                                                            '<script>tippy("#info_tab_3_1_5", {content: "Hazard ratio under the alternative hypothesis, typically less than 1, e.g., 0.65, 0.7, 0.8 etc.",});'),
                     HTML(                                                                            '</script>'),
                     HTML(                                                                            '<input id="ui_number_life_test_hr" type="number" class="form-control" min="0" max="1" step="any">'),
                     HTML(                                                                       '</div>'), #col-sm-12
                     HTML(                                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 SUBPANEL 1 ROW 1 FIELDSET 1: END ------------------------#
                     HTML(                                                                '</fieldset>'),
                     HTML(                                                            '</div>'), #form-group
                     HTML(                                                        '</div>'), #col-sm-6
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 SUBPANEL 1 ROW 1 FIELDSET 2: chinese_all_in -------------#
                     HTML(                                                        '<div class="col-sm-6">'),
                     HTML(                                                            '<div class="form-group">'),
                     HTML(                                                                '<fieldset class="uiFieldsetBlack">'),
                     HTML(                                                                    '<legend name="ui_legend_of_chinese_all_in" class="uiLegendBlack">Value of chinese_all_in</legend>'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 SUBPANEL 1 ROW 1 FIELDSET 2 FIELDSET 1 ------------------#
                     HTML(                                                                    '<div class="row-sm">'),
                     HTML(                                                                        '<div class="form-group">'),
                     HTML(                                                                            '<fieldset id="fieldset_pattern" class="uiFieldsetBlack">'),
                     HTML(                                                                                '<legend class="uiSubLegendBlue">Pattern:</legend>'),
                     HTML(                                                                                '<b>Subpopulation within global study has the same recruitment pattern with global study</b>'),
                     HTML(                                                                                '<div class="btn-group form-inline">'),
                     HTML(                                                                                    '<a id="ui_number_life_test_pattern_y" class="btn btn-primary" data-toggle="ui_number_life_test_pattern" data-title="1">Yes</a>'),
                     HTML(                                                                                    '<a id="ui_number_life_test_pattern_n" class="btn btn-primary" data-toggle="ui_number_life_test_pattern" data-title="0">No</a>'),
                     HTML(                                                                                    '<input type="hidden" id="ui_number_life_test_pattern">'),
                     HTML(                                                                                '</div>'), #btn-group
                     HTML(                                                                             '</fieldset>'),
                     HTML(                                                                         '</div>'), #form-group
                     HTML(                                                                     '<div class="row-sm">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 SUBPANEL 1 ROW 1 FIELDSET 2 FIELDSET 2 ------------------#
                     HTML(                                                                    '<div class="row-sm">'),
                     HTML(                                                                        '<div class="form-group">'),
                     HTML(                                                                            '<fieldset id="fieldset_global_study" class="uiFieldsetBlack">'),
                     HTML(                                                                                '<legend class="uiSubLegendBlue">Global study</legend>'),
                     HTML(                                                                                '<b id="ui_label_life_test_recruit_global" class="uiLabel">Uniform recruitment?</b>'),
                     HTML(                                                                                '<div class="btn-group form-inline">'),
                     HTML(                                                                                    '<a id="ui_number_life_test_unif_global_y" class="btn btn-primary" data-toggle="ui_number_life_test_unif_global" data-title="1">Yes</a>'),
                     HTML(                                                                                    '<a id="ui_number_life_test_unif_global_n" class="btn btn-primary" data-toggle="ui_number_life_test_unif_global" data-title="0">No</a>'),
                     HTML(                                                                                    '<input type="hidden" id="ui_number_life_test_unif_global">'),
                     HTML(                                                                                '</div>'), #btn-group
                     HTML(                                                                             '</fieldset>'),
                     HTML(                                                                         '</div>'), #form-group
                     HTML(                                                                     '<div class="row-sm">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 SUBPANEL 1 ROW 1 FIELDSET 2 FIELDSET 3 ------------------#
                     HTML(                                                                    '<div class="row-sm">'),
                     HTML(                                                                        '<div id="fieldset_local_in_global" class="form-group">'),
                     HTML(                                                                            '<fieldset class="uiFieldsetBlack">'),
                     HTML(                                                                                '<legend class="uiSubLegendBlue">Subpopulation within global study:</legend>'),
                     HTML(                                                                                '<div class="form-group">'),
                     HTML(                                                                                    '<b id="ui_label_life_test_recruit_st_cn" class="uiLabel">Starting point (month)</b>'),
                     HTML(                                                                                    '<span id="info_life_test_enroll_st_cn" tabindex="0">'),
                     HTML(                                                                                        "<i class='fa fa-info-circle' role='presentation' aria-label='info-circle icon'></i>"),
                     HTML(                                                                                    '</span>'),
                     HTML(                                                                                    '<script>tippy("#info_life_test_enroll_st_cn", {content: "Recruitment start time (month) for Subpopulation within global study. For example, starting.point=1 means synchronous recruitment with global study; starting.point=2, 3, 4 etc. means a delay recruitment for Subpopulation compared to global study.",});'),
                     HTML(                                                                                    '</script>'),
                     HTML(                                                                                    '<input id="ui_number_life_test_enroll_st_cn" type="number" class="form-control" min="1" step="1">'),
                     HTML(                                                                                '</div>'),
                     HTML(                                                                                '<div id="input_recru_local_in_global" class="form-group">'),
                     HTML(                                                                                    '<b id="ui_label_life_test_recruit_cn_in" class="uiLabel">Uniform recruitment?</b>'),
                     HTML(                                                                                    '<div class="btn-group form-inline">'),
                     HTML(                                                                                        '<a id="ui_number_life_test_unif_cn_in_y" class="btn btn-primary" data-toggle="ui_number_life_test_unif_cn_in" data-title="1">Yes</a>'),
                     HTML(                                                                                        '<a id="ui_number_life_test_unif_cn_in_n" class="btn btn-primary" data-toggle="ui_number_life_test_unif_cn_in" data-title="0">No</a>'),
                     HTML(                                                                                        '<input type="hidden" id="ui_number_life_test_unif_cn_in">'),
                     HTML(                                                                                    '</div>'), #btn-group
                     HTML(                                                                                 '</div>'), #form-group
                     HTML(                                                                             '</fieldset>'),
                     HTML(                                                                         '</div>'), #form-group
                     HTML(                                                                     '<div class="row-sm">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 SUBPANEL 1 ROW 1 FIELDSET 2 FIELDSET 4 ------------------#
                     HTML(                                                                    '<div class="row-sm">'),
                     HTML(                                                                        '<div class="form-group">'),
                     HTML(                                                                            '<fieldset id="fieldset_local_out_global" class="uiFieldsetBlack">'),
                     HTML(                                                                                '<legend class="uiSubLegendBlue">Global participants excluding subpopulation:</legend>'),
                     HTML(                                                                                '<b id="ui_label_life_test_recruit_noncn" class="uiLabel">Uniform recruitment?</b>'),
                     HTML(                                                                                '<div class="btn-group form-inline">'),
                     HTML(                                                                                    '<a id="ui_number_life_test_unif_noncn_y" class="btn btn-primary" data-toggle="ui_number_life_test_unif_noncn" data-title="1">Yes</a>'),
                     HTML(                                                                                    '<a id="ui_number_life_test_unif_noncn_n" class="btn btn-primary" data-toggle="ui_number_life_test_unif_noncn" data-title="0">No</a>'),
                     HTML(                                                                                    '<input type="hidden" id="ui_number_life_test_unif_noncn">'),
                     HTML(                                                                                '</div>'), #btn-group
                     HTML(                                                                             '</fieldset>'),
                     HTML(                                                                         '</div>'), #form-group
                     HTML(                                                                     '<div class="row-sm">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 SUBPANEL 1 ROW 1 FIELDSET 2 FIELDSET 5 ------------------#
                     HTML(                                                                    '<div class="row-sm">'),
                     HTML(                                                                        '<div class="form-group">'),
                     HTML(                                                                            '<fieldset id="fieldset_ex_global" class="uiFieldsetBlack">'),
                     HTML(                                                                                '<legend class="uiSubLegendBlue">Global participants:</legend>'),
                     HTML(                                                                                '<b id="ui_label_life_test_recruit_noncn_ex" class="uiLabel">Uniform recruitment?</b>'),
                     HTML(                                                                                '<div class="btn-group form-inline">'),
                     HTML(                                                                                    '<a id="ui_number_life_test_unif_noncn_ex_y" class="btn btn-primary" data-toggle="ui_number_life_test_unif_noncn_ex" data-title="1">Yes</a>'),
                     HTML(                                                                                    '<a id="ui_number_life_test_unif_noncn_ex_n" class="btn btn-primary" data-toggle="ui_number_life_test_unif_noncn_ex" data-title="0">No</a>'),
                     HTML(                                                                                    '<input type="hidden" id="ui_number_life_test_unif_noncn_ex">'),
                     HTML(                                                                                '</div>'), #btn-group
                     HTML(                                                                             '</fieldset>'),
                     HTML(                                                                         '</div>'), #form-group
                     HTML(                                                                     '<div class="row-sm">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 SUBPANEL 1 ROW 1 FIELDSET 2 FIELDSET 6 ------------------#
                     HTML(                                                                    '<div class="row-sm">'),
                     HTML(                                                                        '<div class="form-group">'),
                     HTML(                                                                            '<fieldset id="fieldset_ex_local" class="uiFieldsetBlack">'),
                     HTML(                                                                                '<legend class="uiSubLegendBlue">Subpopulation out of global study:</legend>'),
                     HTML(                                                                                '<div class="form-group">'),
                     HTML(                                                                                    '<b >Accrual time (months)</b>'),
                     HTML(                                                                                    '<input id="ui_number_life_test_enroll_len_out" type="number" class="form-control" min="1" step="1">'),
                     HTML(                                                                                '</div>'),
                     HTML(                                                                                '<div class="form-group">'),
                     HTML(                                                                                    '<b>Follow up time (months)</b>'),
                     HTML(                                                                                    '<span id="info_fu_len_out" tabindex="0">'),
                     HTML(                                                                                        "<i class='fa fa-info-circle' role='presentation' aria-label='info-circle icon'></i>"),
                     HTML(                                                                                    '</span>'),
                     HTML(                                                                                    '<script>tippy("#info_fu_len_out", {content: "Follow up time in months, for example, 6 mean following last participants for 6 months.",});'),
                     HTML(                                                                                    '</script>'),
                     HTML(                                                                                    '<input id="ui_number_life_test_fu_len_out" type="number" class="form-control" min="1" step="1">'),
                     HTML(                                                                                '</div>'),
                     HTML(                                                                                '<div class="form-group">'),
                     HTML(                                                                                    '<b id="ui_label_life_test_recruit_cn_out" class="uiLabel">Uniform recruitment?</b>'),
                     HTML(                                                                                    '<div class="btn-group form-inline">'),
                     HTML(                                                                                        '<a id="ui_number_life_test_unif_cn_out_y" class="btn btn-primary" data-toggle="ui_number_life_test_unif_cn_out" data-title="1">Yes</a>'),
                     HTML(                                                                                        '<a id="ui_number_life_test_unif_cn_out_n" class="btn btn-primary" data-toggle="ui_number_life_test_unif_cn_out" data-title="0">No</a>'),
                     HTML(                                                                                        '<input type="hidden" id="ui_number_life_test_unif_cn_out">'),
                     HTML(                                                                                    '</div>'), #btn-group
                     HTML(                                                                                 '</div>'), #form-group
                     HTML(                                                                             '</fieldset>'),
                     HTML(                                                                         '</div>'), #form-group
                     HTML(                                                                     '<div class="row-sm">'),
                     HTML(                                                                '</fieldset>'),
                     HTML(                                                            '</div>'), #form-group
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 SUBPANEL 1 ROW 1 FIELDSET 2: END ------------------------#
                     HTML(                                                        '</div>'), #col-sm-6
                     HTML(                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 SUBPANEL 1 ROW 2: Check Button---------------------------#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div  style="text-align: center">'), #class="col-auto"
                     HTML(                                                            '<button id="ui_btn_t2e_end_life_test_check" class="btn btn-primary uiCheckBtn">Check Input</button>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     HTML(                                                '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 SUBPANEL 2: Time-to-event Endpoint - Life test - Summary #
                     HTML(                                                '<div class="tab-pane fade" id="tab-input_t2e_end_life_test-2">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 SUBPANEL 2 ROW 1: Time-to-event Endpoint - Life test Summary --#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div class="col-sm">'),
                     #HTML(                                                            'Life test Summary'),
                     HTML(                                                            '<div id="summary_track_table3"></div>'),
                     HTML(                                                        '</div>'), #col-sm
                     HTML(                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 SUBPANEL 2 ROW 2: Time-to-event Endpoint - Life test R code ---#
                     HTML(                                                    '<div class="row-sm uiKeyArea">'),
                     HTML(                                                        '<div class="col-auto">'),
                     HTML(                                                            '<pre>'),
                     HTML(                                                                '<textarea id="ui_textarea_t2e_end_life_test" class="form-control " rows="7" cols="80" readonly=true>'),
                     HTML(                                                                '</textarea>'),
                     HTML(                                                            '</pre>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 SUBPANEL 2 ROW 3: Run Button ----------------------------------#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div class="col-auto" style="text-align: center">'),
                     HTML(                                                            '<button id="ui_btn_t2e_end_life_test_refresh" class="btn btn-primary">Refresh</button>'),
                     HTML(                                                            '<button id="ui_btn_t2e_end_life_test_download" class="btn btn-primary">Download</button>'),
                     HTML(                                                            '<button id="ui_btn_t2e_end_life_test_run" type="button" class="btn btn-primary action-button  uiCheckBtn">Run</button>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     HTML(                                                '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3 SUBPANEL 3: Time-to-event Endpoint - Life test - Output -#
                     HTML(                                                '<div class="tab-pane fade" id="tab-input_t2e_end_life_test-3">'),
                     outUI("cont3"),
                     HTML(                                                '</div>'),
                     #####-------------------------------------------------------------------------------------------#
                     HTML(                                            '</div>'), #tab-content
                     HTML(                                        '</div>'), #nav-tabs-custom
                     HTML(                                    '</div>'), #tab-pane
                     HTML(                                '</div>'), #box-body
                     HTML(                            '</div>'), #box
                     HTML(                        '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 3: END #####################################################

                     ##### BODY BOX BOXBODY ROW 1 TABPANE 4: START ###################################################
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 4: Non-inferiority > Continuous Endpoint ###################
                     HTML(                        '<div class="tab-pane" id="shiny-tab-ni_con_end">'),
                     HTML(                            '<div class="box box-solid box-primary">'),
                     HTML(                                '<div class="box-header">'),
                     HTML(                                    '<h3 class="box-title"><b>Non-inferiority > Continuous Endpoint</b></h3>'),
                     HTML(                                '</div>'), #box-header
                     HTML(                                '<div class="box-body">'),
                     HTML(                                    '<div class="tab-pane" id="shiny-tab-ni_con_end">'),
                     HTML(                                        '<div class="nav-tabs-custom">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 4 NAV: START ----------------------------------------------#
                     HTML(                                            '<ul id="tabset5" class="nav nav-pills" role="tablist">'),
                     HTML(                                                '<li><a href="#tab-input_ni_con_end-1" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-list"></i> Input'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<li><a href="#tab-input_ni_con_end-2" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-info-circle"></i> Summary'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<li><a href="#tab-input_ni_con_end-3" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-chart-line"></i> Output'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<!--<li class="header pull-right">Continuous Endpoint</li>-->'),
                     HTML(                                            '</ul>'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 4 NAV: END ------------------------------------------------#
                     HTML(                                            '<div class="tab-content">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 4 SUBPANEL 1: Non-inferiority - Continuous Endpoint - Input --#
                     HTML(                                                '<div class="tab-pane fade in active" id="tab-input_ni_con_end-1">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 4 SUBPANEL 1 ROW 1 ----------------------------------------#
                     HTML(                                                    '<div class="row-sm">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 4 SUBPANEL 1 ROW 1 FIELDSET 1: Experimental Arm -----------#
                     HTML(                                                        '<div class="col-sm-6">'),
                     HTML(                                                            '<div class="form-group">'),
                     HTML(                                                                '<fieldset class="uiFieldsetBlack">'),
                     HTML(                                                                    '<legend class="uiLegendBlack">Experimental Arm</legend>'),
                     HTML(                                                                    '<b>Mean</b>'),
                     HTML(                                                                    '<input id="ui_number_ni_con_end_mean_trt" type="number" class="form-control" min="0" step="any">'),
                     HTML(                                                                    '<b>Standard Deviation</b>'),
                     HTML(                                                                    '<input id="ui_number_ni_con_end_sd_trt" type="number" class="form-control" min="0" step="any">'),
                     HTML(                                                                '</fieldset>'),
                     HTML(                                                            '</div>'), #form-group
                     HTML(                                                        '</div>'), #col-sm-6
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 4 SUBPANEL 1 ROW 1 FIELDSET 2: Control Arm ----------------#
                     HTML(                                                        '<div class="col-sm-6">'),
                     HTML(                                                            '<div class="form-group">'),
                     HTML(                                                                '<fieldset class="uiFieldsetBlack">'),
                     HTML(                                                                    '<legend class="uiLegendBlack">Control Arm</legend>'),
                     HTML(                                                                    '<b>Mean</b>'),
                     HTML(                                                                    '<input id="ui_number_ni_con_end_mean_ctrl" type="number" class="form-control" min="0" step="any">'),
                     HTML(                                                                    '<b>Standard Deviation</b>'),
                     HTML(                                                                    '<input id="ui_number_ni_con_end_sd_ctrl" type="number" class="form-control" min="0" step="any">'),
                     HTML(                                                                '</fieldset>'),
                     HTML(                                                            '</div>'), #form-group
                     HTML(                                                        '</div>'), #col-sm-6
                     HTML(                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 4 SUBPANEL 1 ROW 2: Check Button --------------------------#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div class="col-auto" style="text-align: center">'),
                     HTML(                                                            '<button id="ui_btn_ni_con_end_check" class="btn btn-primary uiCheckBtn">Check Input</button>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     HTML(                                                '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 4 SUBPANEL 2: Non-inferiority - Continuous Endpoint - Summary #
                     HTML(                                                '<div class="tab-pane fade" id="tab-input_ni_con_end-2">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 4 SUBPANEL 2 ROW 1: NI Continuous Endpoint Summary ------------------------#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div class="col-sm">'),
                     #HTML(                                                            'Continuous Endpoint Summary'),
                     HTML(                                                            '<div id="summary_track_table4"></div>'),
                     HTML(                                                        '</div>'), #col-sm
                     HTML(                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 4 SUBPANEL 2 ROW 2: NI Continuous Endpoint R code -------------------------#
                     HTML(                                                    '<div class="row-sm uiKeyArea">'),
                     HTML(                                                        '<div class="col-auto">'),
                     HTML(                                                            '<pre>'),
                     HTML(                                                                '<textarea id="ui_textarea_ni_con_end" class="form-control " rows="7" cols="80" readonly=true>'),
                     HTML(                                                                '</textarea>'),
                     HTML(                                                            '</pre>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 4 SUBPANEL 2 ROW 3: Run Button ----------------------------#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div class="col-auto" style="text-align: center">'),
                     HTML(                                                            '<button id="ui_btn_ni_con_end_refresh" class="btn btn-primary">Refresh</button>'),
                     HTML(                                                            '<button id="ui_btn_ni_con_end_download" class="btn btn-primary">Download</button>'),
                     HTML(                                                            '<button id="ui_btn_ni_con_end_run" type="button" class="btn btn-primary action-button  uiCheckBtn">Run</button>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     HTML(                                                '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 4 SUBPANEL 3: Non-inferiority - Continuous Endpoint - Output -#
                     HTML(                                                '<div class="tab-pane fade" id="tab-input_ni_con_end-3">'),
                     outUI("cont4"),
                     HTML(                                                '</div>'),
                     #####-------------------------------------------------------------------------------------------#
                     HTML(                                            '</div>'), #tab-content
                     HTML(                                        '</div>'), #nav-tabs-custom
                     HTML(                                    '</div>'), #tab-pane
                     HTML(                                '</div>'), #box-body
                     HTML(                            '</div>'), #box
                     HTML(                        '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 4: END #####################################################


                     ##### BODY BOX BOXBODY ROW 1 TABPANE 5: START ###################################################
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 5: Non-inferiority > Binary Endpoint #######################
                     HTML(                        '<div class="tab-pane" id="shiny-tab-ni_bin_end">'),
                     HTML(                            '<div class="box box-solid box-primary">'),
                     HTML(                                '<div class="box-header">'),
                     HTML(                                    '<h3 class="box-title"><b>Non-inferiority > Binary Endpoint</b></h3>'),
                     HTML(                                '</div>'), #box-header
                     HTML(                                '<div class="box-body">'),
                     HTML(                                    '<div class="tab-pane" id="shiny-tab-ni_bin_end">'),
                     HTML(                                        '<div class="nav-tabs-custom">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 5 NAV: START ----------------------------------------------#
                     HTML(                                            '<ul id="tabset6" class="nav nav-pills" role="tablist">'),
                     HTML(                                                '<li><a href="#tab-input_ni_bin_end-1" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-list"></i> Input'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<li><a href="#tab-input_ni_bin_end-2" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-info-circle"></i> Summary'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<li><a href="#tab-input_ni_bin_end-3" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-chart-line"></i> Output'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<!--<li class="header pull-right">Continuous Endpoint</li>-->'),
                     HTML(                                            '</ul>'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 5 NAV: END ------------------------------------------------#
                     HTML(                                            '<div class="tab-content">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 5 SUBPANEL 1: Non-inferiority - Binary Endpoint - Input ---#
                     HTML(                                                '<div class="tab-pane fade in active" id="tab-input_ni_bin_end-1">'),
                     HTML(                                                    '<div class="row-sm">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 5 SUBPANEL 1 ROW 1 FIELDSET 1: Experimental Arm -----------#
                     HTML(                                                        '<div class="col-sm-6">'),
                     HTML(                                                            '<div class="form-group">'),
                     HTML(                                                                '<fieldset class="uiFieldsetBlack">'),
                     HTML(                                                                    '<legend class="uiLegendBlack">Experimental Arm</legend>'),
                     HTML(                                                                    '<b>Proportion</b>'),
                     HTML(                                                                    '<input id="ui_number_ni_bin_end_prop_trt" type="number" class="form-control" min="0" max="1" step="0.1">'),
                     HTML(                                                                '</fieldset>'),
                     HTML(                                                            '</div>'), #form-group
                     HTML(                                                        '</div>'), #col-sm-6
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 5 SUBPANEL 1 ROW 1 FIELDSET 2: Control Arm ----------------#
                     HTML(                                                        '<div class="col-sm-6">'),
                     HTML(                                                            '<div class="form-group">'),
                     HTML(                                                                '<fieldset class="uiFieldsetBlack">'),
                     HTML(                                                                    '<legend class="uiLegendBlack">Control Arm</legend>'),
                     HTML(                                                                        '<b>Proportion</b>'),
                     HTML(                                                                        '<input id="ui_number_ni_bin_end_prop_ctrl" type="number" class="form-control" min="0" max="1" step="0.1">'),
                     HTML(                                                                '</fieldset>'),
                     HTML(                                                            '</div>'), #form-group
                     HTML(                                                        '</div>'), #col-sm-6
                     HTML(                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 5 SUBPANEL 1 ROW 2: Check Button---------------------------#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div class="col-auto" style="text-align: center">'),
                     HTML(                                                            '<button id="ui_btn_ni_bin_end_check" class="btn btn-primary uiCheckBtn">Check Input</button>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     HTML(                                                '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 5 SUBPANEL 2: Non-inferiority - Binary Endpoint - Summary -#
                     HTML(                                                '<div class="tab-pane fade" id="tab-input_ni_bin_end-2">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 5 SUBPANEL 2 ROW 1: Chi-square Summary --------------------#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div class="col-sm">'),
                     #HTML(                                                            'NI Chi-square Summary'),
                     HTML(                                                            '<div id="summary_track_table5"></div>'),
                     HTML(                                                        '</div>'), #col-sm
                     HTML(                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 5 SUBPANEL 2 ROW 2: Chi-square R code -------------------------#
                     HTML(                                                    '<div class="row-sm uiKeyArea">'),
                     HTML(                                                        '<div class="col-auto">'),
                     HTML(                                                            '<pre>'),
                     HTML(                                                                '<textarea id="ui_textarea_ni_bin_end" class="form-control " rows="7" cols="80" readonly=true>'),
                     HTML(                                                                '</textarea>'),
                     HTML(                                                            '</pre>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 5 SUBPANEL 2 ROW 3: Run Button ----------------------------#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div class="col-auto" style="text-align: center">'),
                     HTML(                                                            '<button id="ui_btn_ni_bin_end_refresh" class="btn btn-primary">Refresh</button>'),
                     HTML(                                                            '<button id="ui_btn_ni_bin_end_download" class="btn btn-primary">Download</button>'),
                     HTML(                                                            '<button id="ui_btn_ni_bin_end_run" type="button" class="btn btn-primary action-button  uiCheckBtn">Run</button>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     HTML(                                                '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 5 SUBPANEL 3: Non-inferiority - Binary Endpoint - Output --#
                     HTML(                                                '<div class="tab-pane fade" id="tab-input_ni_bin_end-3">'),
                     outUI("cont5"),
                     HTML(                                                '</div>'),
                     #####-------------------------------------------------------------------------------------------#
                     HTML(                                            '</div>'), #tab-content
                     HTML(                                        '</div>'), #nav-tabs-custom
                     HTML(                                    '</div>'), #tab-pane
                     HTML(                                '</div>'), #box-body
                     HTML(                            '</div>'), #box
                     HTML(                        '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 5: END #####################################################

                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6: START ###################################################
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6: Non-inferiority > Time-to-event Endpoint ################
                     HTML(                        '<div class="tab-pane" id="shiny-tab-ni_t2e_end">'),
                     HTML(                            '<div class="box box-solid box-primary">'),
                     HTML(                                '<div class="box-header">'),
                     HTML(                                    '<h3 class="box-title"><b>Non-inferiority > Time-to-Event Endpoint</b></h3>'),
                     HTML(                                '</div>'), #box-header
                     HTML(                                '<div class="box-body">'),
                     HTML(                                    '<div class="tab-pane" id="shiny-tab-ni_t2e_end">'),
                     HTML(                                        '<div class="nav-tabs-custom">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 NAV: START ----------------------------------------------#
                     HTML(                                            '<ul id="tabset4" class="nav nav-pills" role="tablist">'),
                     HTML(                                                '<li><a href="#tab-input_ni_t2e_end-1" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-list"></i> Input'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<li><a href="#tab-input_ni_t2e_end-2" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-info-circle"></i> Summary'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<li><a href="#tab-input_ni_t2e_end-3" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-chart-line"></i> Output'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<!--<li class="header pull-right">Time-to-event Endpoint</li>-->'),
                     HTML(                                            '</ul>'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 NAV: END ------------------------------------------------#
                     HTML(                                            '<div class="tab-content">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 SUBPANEL 1: Non-inferiority - Time-to-event Endpoint - Input -#
                     HTML(                                                '<div class="tab-pane fade in active" id="tab-input_ni_t2e_end-1">'),
                     HTML(                                                    '<div class="row-sm">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 SUBPANEL 1 ROW 1 FIELDSET 1: Parameters in global study -#
                     HTML(                                                        '<div class="col-sm-6">'),
                     HTML(                                                            '<div class="form-group">'),
                     HTML(                                                                '<fieldset class="uiFieldsetBlack">'),
                     HTML(                                                                    '<legend class="uiLegendBlack">Parameters in global study</legend>'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 SUBPANEL 1 ROW 1 FIELDSET 1 ROW 1 -----------------------#
                     HTML(                                                                    '<div class="row-sm">'),
                     HTML(                                                                        '<div class="col-sm-6">'),
                     HTML(                                                                            '<b>Accrual time (mths.)</b>'),
                     HTML(                                                                            '<input id="ui_number_ni_t2e_end_enroll_len" type="number" class="form-control" min="1" step="1">'),
                     HTML(                                                                        '</div>'), #col-sm-6
                     HTML(                                                                        '<div class="col-sm-6">'),
                     HTML(                                                                            '<b>N of target event</b>'),
                     HTML(                                                                            '<span id="info_tab_6_1_1" tabindex="0">'),
                     HTML(                                                                                "<i class='fa fa-info-circle' role='presentation' aria-label='info-circle icon'></i>"),
                     HTML(                                                                            '</span>'),
                     HTML(                                                                            '<script>tippy("#info_tab_6_1_1", {content: "Total number of events required in global study at primary completion.",});'),
                     HTML(                                                                            '</script>'),
                     HTML(                                                                            '<input id="ui_number_ni_t2e_end_n_targetevent" type="number" class="form-control" min="1" step="1">'),
                     HTML(                                                                        '</div>'), #col-sm-6
                     HTML(                                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 SUBPANEL 1 ROW 1 FIELDSET 1 ROW 2 -----------------------#
                     HTML(                                                                    '<div class="row-sm">'),
                     HTML(                                                                        '<div class="form-group">'),
                     HTML(                                                                            '<fieldset class="uiFieldsetBlack">'),
                     HTML(                                                                                '<legend class="uiSubLegendBlack">Control</legend>'),
                     HTML(                                                                                '<div class="row-sm-12" style="white-space: nowrap;">'),
                     HTML(                                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                                            '<input type="radio" id="ui_radio_ni_t2e_end_median_type_m" name="ui_radio_group_ni_median_rate_type" value="1">'),
                     HTML(                                                                                            '<b>Median (mths.)</b>'),
                     HTML(                                                                                            '<span id="info_ni_median_rate_type_m" tabindex="0">'),
                     HTML(                                                                                                "<i class='fa fa-info-circle' role='presentation' aria-label='info-circle icon'></i>"),
                     HTML(                                                                                            '</span>'),
                     HTML(                                                                                            '<script>tippy("#info_ni_median_rate_type_m", {content: "Median time in control arm (months).",});'),
                     HTML(                                                                                            '</script>'),
                     HTML(                                                                                    '</div>'), #col-sm-6
                     HTML(                                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                                        '<input id="ui_number_ni_t2e_end_median_rate_m" class="form-control" type="number" min="1" max="500" step="any">'),
                     HTML(                                                                                    '</div>'), #col-sm-6
                     HTML(                                                                                '</div>'), #row-sm-12
                     HTML(                                                                                '<div class="row-sm-12">'),
                     HTML(                                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                                            '<input type="radio" id="ui_radio_ni_t2e_end_median_type_y" name="ui_radio_group_ni_median_rate_type" value="0">'),
                     HTML(                                                                                            '<b>Yearly rate</b>'),
                     HTML(                                                                                            '<span id="info_ni_median_rate_type_y" tabindex="0">'),
                     HTML(                                                                                                "<i class='fa fa-info-circle' role='presentation' aria-label='info-circle icon'></i>"),
                     HTML(                                                                                            '</span>'),
                     HTML(                                                                                            '<script>tippy("#info_ni_median_rate_type_y", {content: "1-year cumulative incidence rate with a scalar in [0,1) in control arm.",});'),
                     HTML(                                                                                            '</script>'),
                     HTML(                                                                                    '</div>'), #col-sm-6
                     HTML(                                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                                        '<input id="ui_number_ni_t2e_end_median_rate_y" class="form-control" type="number" min="0" max="0.99999" step="0.05">'),
                     HTML(                                                                                    '</div>'), #col-sm-6
                     HTML(                                                                                '</div>'), #row-sm-12
                     HTML(                                                                            '</fieldset>'),
                     HTML(                                                                        '</div>'), #form-group
                     HTML(                                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 SUBPANEL 1 ROW 1 FIELDSET 1 ROW 3 -----------------------#
                     HTML(                                                                    '<div class="row-sm">'),
                     HTML(                                                                        '<div class="form-group">'),
                     HTML(                                                                            '<fieldset class="uiFieldsetBlack">'),
                     HTML(                                                                                '<legend class="uiSubLegendBlack">Yearly dropout rate</legend>'),
                     HTML(                                                                                '<div class="row-sm-12">'),
                     HTML(                                                                                  '<div class="col-sm-6">'),
                     HTML(                                                                                        '<b>Control</b>'),
                     HTML(                                                                                        '<span id="info_tab_6_1_4" tabindex="0">'),
                     HTML(                                                                                            "<i class='fa fa-info-circle' role='presentation' aria-label='info-circle icon'></i>"),
                     HTML(                                                                                        '</span>'),
                     HTML(                                                                                        '<script>tippy("#info_tab_6_1_4", {content: "A scalar in [0,1) representing the fraction of participants lost to follow-up prior to event per year. For example, yearly dropout rate=0.05 means 5% of drops out prior to documentation of events per year.",});'),
                     HTML(                                                                                        '</script>'),
                     HTML(                                                                                        '<input id="ui_number_ni_t2e_end_drop_ctrl" type="number" class="form-control" min="0" max="1" step="any">'),
                     HTML(                                                                                    '</div>'), #col-sm-6
                     HTML(                                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                                        '<b>Experimental</b>'),
                     HTML(                                                                                        '<input id="ui_number_ni_t2e_end_drop_trt" type="number" class="form-control" min="0" max="1" step="any">'),
                     HTML(                                                                                    '</div>'), #col-sm-6
                     HTML(                                                                                '</div>'), #row-sm-12
                     HTML(                                                                            '</fieldset>'),
                     HTML(                                                                        '</div>'), #form-group
                     HTML(                                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 SUBPANEL 1 ROW 1 FIELDSET 1 ROW 4 -----------------------#
                     HTML(                                                                    '<div class="row-sm">'),
                     HTML(                                                                        '<div class="col-sm-12">'),
                     HTML(                                                                            '<b>Target HR</b>'),
                     HTML(                                                                            '<span id="info_tab_6_1_5" tabindex="0">'),
                     HTML(                                                                                "<i class='fa fa-info-circle' role='presentation' aria-label='info-circle icon'></i>"),
                     HTML(                                                                            '</span>'),
                     HTML(                                                                            '<script>tippy("#info_tab_6_1_5", {content: "Hazard ratio under the alternative hypothesis, typically less than 1, e.g., 0.65, 0.7, 0.8 etc.",});'),
                     HTML(                                                                            '</script>'),
                     HTML(                                                                            '<input id="ui_number_ni_t2e_end_hr" type="number" class="form-control" min="0" max="1" step="any">'),
                     HTML(                                                                        '</div>'), #col-sm-12
                     HTML(                                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 SUBPANEL 1 ROW 1 FIELDSET 1: END ------------------------#
                     HTML(                                                                '</fieldset>'),
                     HTML(                                                            '</div>'), #form-group
                     HTML(                                                        '</div>'), #col-sm-6
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 SUBPANEL 1 ROW 1 FIELDSET 2: chinese_all_in -------------#
                     HTML(                                                        '<div class="col-sm-6">'),
                     HTML(                                                            '<div class="form-group">'),
                     HTML(                                                                '<fieldset class="uiFieldsetBlack">'),
                     HTML(                                                                    '<legend name="ui_legend_of_chinese_all_in" class="uiLegendBlack">Value of chinese_all_in</legend>'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 SUBPANEL 1 ROW 1 FIELDSET 2 FIELDSET 1 ------------------#
                     HTML(                                                                    '<div class="row-sm">'),
                     HTML(                                                                        '<div class="form-group">'),
                     HTML(                                                                            '<fieldset id="ni_fieldset_pattern" class="uiFieldsetBlack">'),
                     HTML(                                                                                '<legend class="uiSubLegendBlue">Pattern:</legend>'),
                     HTML(                                                                                '<b>Subpopulation within global study has the same recruitment pattern with global study</b>'),
                     HTML(                                                                                '<div class="btn-group form-inline">'),
                     HTML(                                                                                    '<a id="ui_number_ni_t2e_end_pattern_y" class="btn btn-primary" data-toggle="ui_number_ni_t2e_end_pattern" data-title="1">Yes</a>'),
                     HTML(                                                                                    '<a id="ui_number_ni_t2e_end_pattern_n" class="btn btn-primary" data-toggle="ui_number_ni_t2e_end_pattern" data-title="0">No</a>'),
                     HTML(                                                                                    '<input type="hidden" id="ui_number_ni_t2e_end_pattern">'),
                     HTML(                                                                                '</div>'), #btn-group
                     HTML(                                                                             '</fieldset>'),
                     HTML(                                                                         '</div>'), #form-group
                     HTML(                                                                     '<div class="row-sm">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 SUBPANEL 1 ROW 1 FIELDSET 2 FIELDSET 2 ------------------#
                     HTML(                                                                    '<div class="row-sm">'),
                     HTML(                                                                        '<div class="form-group">'),
                     HTML(                                                                            '<fieldset id="ni_fieldset_global_study" class="uiFieldsetBlack">'),
                     HTML(                                                                                '<legend class="uiSubLegendBlue">Global study</legend>'),
                     HTML(                                                                                '<b id="ui_label_ni_t2e_end_recruit_global" class="uiLabel">Uniform recruitment?</b>'),
                     HTML(                                                                                '<div class="btn-group form-inline">'),
                     HTML(                                                                                    '<a id="ui_number_ni_t2e_end_unif_global_y" class="btn btn-primary" data-toggle="ui_number_ni_t2e_end_unif_global" data-title="1">Yes</a>'),
                     HTML(                                                                                    '<a id="ui_number_ni_t2e_end_unif_global_n" class="btn btn-primary" data-toggle="ui_number_ni_t2e_end_unif_global" data-title="0">No</a>'),
                     HTML(                                                                                    '<input type="hidden" id="ui_number_ni_t2e_end_unif_global">'),
                     HTML(                                                                                '</div>'), #btn-group
                     HTML(                                                                             '</fieldset>'),
                     HTML(                                                                         '</div>'), #form-group
                     HTML(                                                                     '<div class="row-sm">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 SUBPANEL 1 ROW 1 FIELDSET 2 FIELDSET 3 ------------------#
                     HTML(                                                                     '<div class="row-sm">'),
                     HTML(                                                                         '<div id="ni_fieldset_local_in_global" class="form-group">'),
                     HTML(                                                                             '<fieldset class="uiFieldsetBlack">'),
                     HTML(                                                                                 '<legend class="uiSubLegendBlue">Subpopulation within global study:</legend>'),
                     HTML(                                                                                 '<div class="form-group">'),
                     HTML(                                                                                    '<b id="ui_label_ni_t2e_end_recruit_st_cn" class="uiLabel">Starting point (month)</b>'),
                     HTML(                                                                                    '<span id="info_ni_t2e_end_enroll_st_cn" tabindex="0">'),
                     HTML(                                                                                        "<i class='fa fa-info-circle' role='presentation' aria-label='info-circle icon'></i>"),
                     HTML(                                                                                    '</span>'),
                     HTML(                                                                                    '<script>tippy("#info_ni_t2e_end_enroll_st_cn", {content: "Recruitment start time (month) for Subpopulation within global study. For example, starting.point=1 means synchronous recruitment with global study; starting.point=2, 3, 4 etc. means a delay recruitment for Subpopulation compared to global study.",});'),
                     HTML(                                                                                    '</script>'),
                     HTML(                                                                                    '<input id="ui_number_ni_t2e_end_enroll_st_cn" type="number" class="form-control" min="1" step="1">'),
                     HTML(                                                                                 '</div>'),
                     HTML(                                                                                 '<div id="ni_input_recru_local_in_global" class="form-group">'),
                     HTML(                                                                                    '<b id="ui_label_ni_t2e_end_recruit_cn_in" class="uiLabel">Uniform recruitment?</b>'),
                     HTML(                                                                                    '<div class="btn-group form-inline">'),
                     HTML(                                                                                        '<a id="ui_number_ni_t2e_end_unif_cn_in_y" class="btn btn-primary" data-toggle="ui_number_ni_t2e_end_unif_cn_in" data-title="1">Yes</a>'),
                     HTML(                                                                                        '<a id="ui_number_ni_t2e_end_unif_cn_in_n" class="btn btn-primary" data-toggle="ui_number_ni_t2e_end_unif_cn_in" data-title="0">No</a>'),
                     HTML(                                                                                        '<input type="hidden" id="ui_number_ni_t2e_end_unif_cn_in">'),
                     HTML(                                                                                    '</div>'), #btn-group
                     HTML(                                                                                 '</div>'), #form-group
                     HTML(                                                                             '</fieldset>'),
                     HTML(                                                                         '</div>'), #form-group
                     HTML(                                                                     '<div class="row-sm">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 SUBPANEL 1 ROW 1 FIELDSET 2 FIELDSET 4 ------------------#
                     HTML(                                                                     '<div class="row-sm">'),
                     HTML(                                                                         '<div class="form-group">'),
                     HTML(                                                                             '<fieldset id="ni_fieldset_local_out_global" class="uiFieldsetBlack">'),
                     HTML(                                                                                '<legend class="uiSubLegendBlue">Global participants excluding subpopulation:</legend>'),
                     HTML(                                                                                '<b>Uniform recruitment?</b>'),
                     HTML(                                                                                '<div class="btn-group form-inline">'),
                     HTML(                                                                                    '<a id="ui_number_ni_t2e_end_unif_noncn_y" class="btn btn-primary" data-toggle="ui_number_ni_t2e_end_unif_noncn" data-title="1">Yes</a>'),
                     HTML(                                                                                    '<a id="ui_number_ni_t2e_end_unif_noncn_n" class="btn btn-primary" data-toggle="ui_number_ni_t2e_end_unif_noncn" data-title="0">No</a>'),
                     HTML(                                                                                    '<input type="hidden" id="ui_number_ni_t2e_end_unif_noncn">'),
                     HTML(                                                                                '</div>'), #btn-group
                     HTML(                                                                             '</fieldset>'),
                     HTML(                                                                         '</div>'), #form-group
                     HTML(                                                                     '<div class="row-sm">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 SUBPANEL 1 ROW 1 FIELDSET 2 FIELDSET 5 ------------------#
                     HTML(                                                                     '<div class="row-sm">'),
                     HTML(                                                                        '<div class="form-group">'),
                     HTML(                                                                            '<fieldset id="ni_fieldset_ex_global" class="uiFieldsetBlack">'),
                     HTML(                                                                                '<legend class="uiSubLegendBlue">Global participants:</legend>'),
                     HTML(                                                                                '<b id="ui_label_ni_t2e_end_recruit_noncn_ex" class="uiLabel">Uniform recruitment?</b>'),
                     HTML(                                                                                '<div class="btn-group form-inline">'),
                     HTML(                                                                                    '<a id="ui_number_ni_t2e_end_unif_noncn_ex_y" class="btn btn-primary" data-toggle="ui_number_ni_t2e_end_unif_noncn_ex" data-title="1">Yes</a>'),
                     HTML(                                                                                    '<a id="ui_number_ni_t2e_end_unif_noncn_ex_n" class="btn btn-primary" data-toggle="ui_number_ni_t2e_end_unif_noncn_ex" data-title="0">No</a>'),
                     HTML(                                                                                    '<input type="hidden" id="ui_number_ni_t2e_end_unif_noncn_ex">'),
                     HTML(                                                                                '</div>'), #btn-group
                     HTML(                                                                             '</fieldset>'),
                     HTML(                                                                         '</div>'), #form-group
                     HTML(                                                                     '<div class="row-sm">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 SUBPANEL 1 ROW 1 FIELDSET 2 FIELDSET 6 ------------------#
                     HTML(                                                                    '<div class="row-sm">'),
                     HTML(                                                                        '<div class="form-group">'),
                     HTML(                                                                            '<fieldset id="ni_fieldset_ex_local" class="uiFieldsetBlack">'),
                     HTML(                                                                                '<legend class="uiSubLegendBlue">Subpopulation out of global study:</legend>'),
                     HTML(                                                                                '<div class="form-group">'),
                     HTML(                                                                                    '<b>Accrual time (months)</b>'),
                     HTML(                                                                                    '<input id="ui_number_ni_t2e_end_enroll_len_out" type="number" class="form-control" min="1" step="1">'),
                     HTML(                                                                                '</div>'),
                     HTML(                                                                                '<div class="form-group">'),
                     HTML(                                                                                    '<b>Follow up time (months)</b>'),
                     HTML(                                                                                    '<span id="info_ni_fu_len_out" tabindex="0">'),
                     HTML(                                                                                        "<i class='fa fa-info-circle' role='presentation' aria-label='info-circle icon'></i>"),
                     HTML(                                                                                    '</span>'),
                     HTML(                                                                                    '<script>tippy("#info_ni_fu_len_out", {content: "Follow up time in months, for example, 6 mean following last participants for 6 months.",});'),
                     HTML(                                                                                    '</script>'),
                     HTML(                                                                                    '<input id="ui_number_ni_t2e_end_fu_len_out" type="number" class="form-control" min="1" step="1">'),
                     HTML(                                                                                '</div>'),
                     HTML(                                                                                '<div class="form-group">'),
                     HTML(                                                                                    '<b id="ui_label_ni_t2e_end_recruit_cn_out" class="uiLabel">Uniform recruitment?</b>'),
                     HTML(                                                                                    '<div class="btn-group form-inline">'),
                     HTML(                                                                                        '<a id="ui_number_ni_t2e_end_unif_cn_out_y" class="btn btn-primary" data-toggle="ui_number_ni_t2e_end_unif_cn_out" data-title="1">Yes</a>'),
                     HTML(                                                                                        '<a id="ui_number_ni_t2e_end_unif_cn_out_n" class="btn btn-primary" data-toggle="ui_number_ni_t2e_end_unif_cn_out" data-title="0">No</a>'),
                     HTML(                                                                                        '<input type="hidden" id="ui_number_ni_t2e_end_unif_cn_out">'),
                     HTML(                                                                                    '</div>'), #btn-group
                     HTML(                                                                                 '</div>'), #form-group
                     HTML(                                                                             '</fieldset>'),
                     HTML(                                                                         '</div>'), #form-group
                     HTML(                                                                     '<div class="row-sm">'),
                     HTML(                                                                '</fieldset>'),
                     HTML(                                                            '</div>'), #form-group
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 SUBPANEL 1 ROW 1 FIELDSET 2: END ------------------------#
                     HTML(                                                        '</div>'), #col-sm-6
                     HTML(                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 SUBPANEL 1 ROW 2: Check Button---------------------------#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div  style="text-align: center">'), #class="col-auto"
                     HTML(                                                            '<button id="ui_btn_ni_t2e_end_check" class="btn btn-primary uiCheckBtn">Check Input</button>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     HTML(                                                '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 SUBPANEL 2: Non-inferiority - Time-to-event Endpoint - Summary -#
                     HTML(                                                '<div class="tab-pane fade" id="tab-input_ni_t2e_end-2">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 SUBPANEL 2 ROW 1: Non-inferiority - Time-to-event Endpoint Summary -#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div class="col-sm">'),
                     #HTML(                                                            'Life test Summary'),
                     HTML(                                                            '<div id="summary_track_table6"></div>'),
                     HTML(                                                        '</div>'), #col-sm
                     HTML(                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 SUBPANEL 2 ROW 2: Non-inferiority - Time-to-event Endpoint R code -#
                     HTML(                                                    '<div class="row-sm uiKeyArea">'),
                     HTML(                                                        '<div class="col-auto">'),
                     HTML(                                                            '<pre>'),
                     HTML(                                                                '<textarea id="ui_textarea_ni_t2e_end" class="form-control " rows="7" cols="80" readonly=true>'),
                     HTML(                                                                '</textarea>'),
                     HTML(                                                            '</pre>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 SUBPANEL 2 ROW 3: Run Button ----------------------------#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div class="col-auto" style="text-align: center">'),
                     HTML(                                                            '<button id="ui_btn_ni_t2e_end_refresh" class="btn btn-primary">Refresh</button>'),
                     HTML(                                                            '<button id="ui_btn_ni_t2e_end_download" class="btn btn-primary">Download</button>'),
                     HTML(                                                            '<button id="ui_btn_ni_t2e_end_run" type="button" class="btn btn-primary action-button  uiCheckBtn">Run</button>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     HTML(                                                '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6 SUBPANEL 3: Non-inferiority - Time-to-event Endpoint - Output -#
                     HTML(                                                '<div class="tab-pane fade" id="tab-input_ni_t2e_end-3">'),
                     outUI("cont6"),
                     HTML(                                                '</div>'),
                     #####-------------------------------------------------------------------------------------------#
                     HTML(                                            '</div>'), #tab-content
                     HTML(                                        '</div>'), #nav-tabs-custom
                     HTML(                                    '</div>'), #tab-pane
                     HTML(                                '</div>'), #box-body
                     HTML(                            '</div>'), #box
                     HTML(                        '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 6: END #####################################################

                     ##### BODY BOX BOXBODY ROW 1 TABPANE 7: START ###################################################
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 7: MCP-Mod > Continuous Endpoint ###########################
                     HTML(                        '<div class="tab-pane" id="shiny-tab-mcpmod_con_end">'),
                     HTML(                            '<div class="box box-solid box-primary">'),
                     HTML(                                '<div class="box-header">'),
                     HTML(                                    '<h3 class="box-title"><b>MCP-Mod > Continuous Endpoints</b></h3>'),
                     HTML(                                '</div>'), #box-header
                     HTML(                                '<div class="box-body">'),
                     HTML(                                    '<div class="tab-pane" id="shiny-tab-mcpmod_con_end">'),
                     HTML(                                        '<div class="nav-tabs-custom">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 7 NAV: START ----------------------------------------------#
                     HTML(                                            '<ul id="tabset8" class="nav nav-pills" role="tablist">'),
                     HTML(                                                '<li><a href="#tab-input_mcpmod_con_end-1" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-list"></i> Input'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<li><a href="#tab-input_mcpmod_con_end-2" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-info-circle"></i> Summary'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<li><a href="#tab-input_mcpmod_con_end-3" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-chart-line"></i> Output'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<!--<li class="header pull-right">TBD</li>-->'),
                     HTML(                                            '</ul>'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 7 NAV: END ------------------------------------------------#
                     HTML(                                            '<div class="tab-content">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 7 SUBPANEL 1: MCP-Mod - Continuous Endpoint - Input -------#
                     HTML(                                                '<div class="tab-pane fade in active" id="tab-input_mcpmod_con_end-1">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 7 SUBPANEL 1 FIELDSET 1 -----------------------------------#
                     HTML(                                                    '<div class="row-sm-12">'),
                     HTML(                                                        '<div class="form-group">'),
                     HTML(                                                            '<fieldset class="uiFieldsetBlack">'),
                     HTML(                                                                '<legend class="uiSubLegendBlue">Common parameters</legend>'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 7 SUBPANEL 1 ROW 1 -------------------------------------#
                     HTML(                                                                '<div class="row">'),
                     HTML(                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                        '<b>E<sub>0</b>'),
                     HTML(                                                                    '</div>'),
                     HTML(                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                        '<input id="ui_number_mcpmod_con_end_e0" class="form-control" type="number" min="-10" max="10" step="0.01">'),
                     HTML(                                                                    '</div>'),
                     HTML(                                                                '</div>'), #row
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 7 SUBPANEL 1 ROW 2 -------------------------------------#
                     HTML(                                                                '<div class="row">'),
                     HTML(                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                        '<b>Emax within the dose range</b>'),
                     HTML(                                                                        '<span id="info_tab_7_1_2" tabindex="0">'),
                     HTML(                                                                            "<i class='fa fa-info-circle' role='presentation' aria-label='info-circle icon'></i>"),
                     HTML(                                                                        '</span>'),
                     HTML(                                                                        '<script>tippy("#info_tab_7_1_2", {content: "The maximum effect over placebo.",});'),
                     HTML(                                                                        '</script>'),
                     HTML(                                                                    '</div>'),
                     HTML(                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                        '<input id="ui_number_mcpmod_con_end_emax" class="form-control" type="number" min="-10" max="10" step="0.01">'),
                     HTML(                                                                    '</div>'),
                     HTML(                                                                '</div>'), #row
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 7 SUBPANEL 1 ROW 3 -------------------------------------#
                     HTML(                                                                '<div class="row">'),
                     HTML(                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                        '<b>Standard deviation</b>'),
                     HTML(                                                                    '</div>'),
                     HTML(                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                        '<input id="ui_number_mcpmod_con_end_sigma" class="form-control" type="number" min="-10" max="10" step="0.01">'),
                     HTML(                                                                    '</div>'),
                     HTML(                                                                '</div>'), #row
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 7 SUBPANEL 1 ROW 4 -------------------------------------#
                     HTML(                                                                '<div class="row">'),
                     HTML(                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                        '<b sytle="line-height:1px;">Minimal clinical meaningful treatment effect size (&Delta;>0)(if available)</b>'),
                     HTML(                                                                    '</div>'),
                     HTML(                                                                    '<div class="col-sm-1">'),
                     HTML(                                                                        '<label class="form-switch">'),
                     HTML(                                                                            '<input id="ui_number_mcpmod_con_end_delta_yes" type="checkbox" >'),
                     HTML(                                                                            '<i></i>'),
                     HTML(                                                                        '</label>'),
                     HTML(                                                                     '</div>'),
                     HTML(                                                                     '<div class="col-sm-5">'),
                     HTML(                                                                         '<input id="ui_number_mcpmod_con_end_delta" class="form-control" type="number" min="-10" max="10" step="0.01" readonly="readonly">'),
                     HTML(                                                                     '</div>'),
                     HTML(                                                                '</div>'), #row
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 7 SUBPANEL 1 ROW 5 -------------------------------------#
                     HTML(                                                                '<div class="row">'),
                     HTML(                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                        '<b>Global power</b>'),
                     HTML(                                                                        '<span id="info_tab_7_1_5" tabindex="0">'),
                     HTML(                                                                            "<i class='fa fa-info-circle' role='presentation' aria-label='info-circle icon'></i>"),
                     HTML(                                                                        '</span>'),
                     HTML(                                                                        '<script>tippy("#info_tab_7_1_5", {content: "1: A &quot;proof-of-concept&quot; for the dose-response relationship based on multiple contrast test<br/>2: |max treatment effect| > &Delta;: Clinically relevant treatment effect in at least one active dose", allowHTML: true,});'),
                     HTML(                                                                        '</script>'),
                     HTML(                                                                    '</div>'),
                     HTML(                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                        '<div class="form-group">'),
                     HTML(                                                                            '<select id="ui_select_mcpmod_con_end_type" class="selectized uiSelectized" disabled="disabled">'),
                     HTML(                                                                                '<option value="1">1. PoC only</option>'),
                     HTML(                                                                                '<option value="2">2. PoC and |max treatment effect| > &Delta;</option>'),
                     HTML(                                                                                '<option value="3">3. PoC, |max treatment effect| > &Delta;, and MED in the dose range</option>'),
                     HTML(                                                                            '</select>'),
                     HTML(                                                                        '</div>'),
                     HTML(                                                                    '</div>'),
                     HTML(                                                                '</div>'), #row
                     HTML(                                                            '</fieldset>'),
                     HTML(                                                        '</div>'), #form-group
                     HTML(                                                    '</div>'), #row-sm-12
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 7 SUBPANEL 1 FIELDSET 2 -----------------------------------#
                     HTML(                                                    '<div class="row-sm-12">'),
                     HTML(                                                        '<div class="form-group">'),
                     HTML(                                                            '<fieldset class="uiFieldsetBlack">'),
                     HTML(                                                                 '<legend class="uiSubLegendBlue">Models</legend>'),
                     HTML(                                                                 '<div class="row-sm-12 form-inline">'),
                     HTML(                                                                     '<div id="mcpmod_con_end_candidate_model_table"></div>'),
                     HTML(                                                                 '</div>'),
                     HTML(                                                            '</fieldset>'),
                     HTML(                                                        '</div>'), #form-group
                     HTML(                                                    '</div>'), #row-sm-12
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 7 SUBPANEL 1 ROW 4: Check Button---------------------------#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div style="text-align: center">'), #class="col-auto"
                     HTML(                                                            '<button id="ui_btn_mcpmod_con_end_check" class="btn btn-primary uiCheckBtn">Check Input</button>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     HTML(                                                '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 7 SUBPANEL 2: MCP-Mod - Continuous Endpoint - Summary -----#
                     HTML(                                                '<div class="tab-pane fade" id="tab-input_mcpmod_con_end-2">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 7 SUBPANEL 2 ROW 1: MCP-Mod - Continuous Endpoint Summary -#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div class="col-sm">'),
                     #HTML(                                                            'MCP-Mod Continuous Endpoint Summary'),
                     HTML(                                                            '<div id="summary_track_table7"></div>'),
                     HTML(                                                        '</div>'), #col-sm
                     HTML(                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 7 SUBPANEL 2 ROW 2: MCP-Mod - Continuous Endpoint R code --#
                     HTML(                                                    '<div class="row-sm uiKeyArea">'),
                     HTML(                                                        '<div class="col-auto">'),
                     HTML(                                                            '<pre>'),
                     HTML(                                                                '<textarea id="ui_textarea_mcpmod_con_end" class="form-control " rows="7" cols="80" readonly=true>'),
                     HTML(                                                                '</textarea>'),
                     HTML(                                                            '</pre>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 7 SUBPANEL 2 ROW 3: MCP-Mod - Continuous Endpoint Run Button -#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div class="col-auto" style="text-align: center">'),
                     HTML(                                                            '<button id="ui_btn_mcpmod_con_end_refresh" class="btn btn-primary">Refresh</button>'),
                     HTML(                                                            '<button id="ui_btn_mcpmod_con_end_download" class="btn btn-primary">Download</button>'),
                     HTML(                                                            '<button id="ui_btn_mcpmod_con_end_run" type="button" class="btn btn-primary action-button  uiCheckBtn">Run</button>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     HTML(                                                '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 7 SUBPANEL 3: MCP-Mod - Continuous Endpoint - Output ------#
                     HTML(                                                '<div class="tab-pane fade" id="tab-input_mcpmod_con_end-3">'),
                     outUI("cont7"),
                     HTML(                                                '</div>'),
                     #####-------------------------------------------------------------------------------------------#
                     HTML(                                            '</div>'), #tab-content
                     HTML(                                        '</div>'), #nav-tabs-custom
                     HTML(                                    '</div>'), #tab-pane
                     HTML(                                '</div>'), #box-body
                     HTML(                            '</div>'), #box
                     HTML(                        '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 7: END #####################################################

                     ##### BODY BOX BOXBODY ROW 1 TABPANE 8: START ###################################################
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 8: MCP-Mod > Binary Endpoint ###########################
                     HTML(                        '<div class="tab-pane" id="shiny-tab-mcpmod_bin_end">'),
                     HTML(                            '<div class="box box-solid box-primary">'),
                     HTML(                                '<div class="box-header">'),
                     HTML(                                    '<h3 class="box-title"><b>MCP-Mod > Binary Endpoints</b></h3>'),
                     HTML(                                '</div>'), #box-header
                     HTML(                                '<div class="box-body">'),
                     HTML(                                    '<div class="tab-pane" id="shiny-tab-mcpmod_bin_end">'),
                     HTML(                                        '<div class="nav-tabs-custom">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 8 NAV: START ----------------------------------------------#
                     HTML(                                            '<ul id="tabset8" class="nav nav-pills" role="tablist">'),
                     HTML(                                                '<li><a href="#tab-input_mcpmod_bin_end-1" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-list"></i> Input'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<li><a href="#tab-input_mcpmod_bin_end-2" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-info-circle"></i> Summary'),
                     HTML(                                                '</a></li>'),
                     HTML(                                                '<li><a href="#tab-input_mcpmod_bin_end-3" role="tab" data-toggle="pill">'),
                     HTML(                                                    '<i class="fas fa-chart-line"></i> Output'),
                     HTML(                                                '</a></li>'),
                     HTML(                                            '</ul>'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 8 NAV: END ------------------------------------------------#
                     HTML(                                            '<div class="tab-content">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 8 SUBPANEL 1: MCP-Mod - Continuous Endpoint - Input -------#
                     HTML(                                                '<div class="tab-pane fade in active" id="tab-input_mcpmod_bin_end-1">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 8 SUBPANEL 1 FIELDSET 1 -----------------------------------#
                     HTML(                                                    '<div class="row-sm-12">'),
                     HTML(                                                        '<div class="form-group">'),
                     HTML(                                                            '<fieldset class="uiFieldsetBlack">'),
                     HTML(                                                                '<legend class="uiSubLegendBlue">Common parameters</legend>'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 8 SUBPANEL 1 ROW 1 -------------------------------------#
                     HTML(                                                                '<div class="row">'),
                     HTML(                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                        '<b>Response rate under placebo (R<sub>0</sub>)</b>'),
                     HTML(                                                                    '</div>'),
                     HTML(                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                        '<input id="ui_number_mcpmod_bin_end_e0" class="form-control" type="number" min="0" max="1" step="0.01">'),
                     HTML(                                                                    '</div>'),
                     HTML(                                                                '</div>'), #row
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 8 SUBPANEL 1 ROW 2 -------------------------------------#
                     HTML(                                                                '<div class="row">'),
                     HTML(                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                        '<b>The maximum response rate within dose range without placebo adjustment (R<sub>max</sub>)</b>'),
                     HTML(                                                                    '</div>'),
                     HTML(                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                        '<input id="ui_number_mcpmod_bin_end_emax" class="form-control" type="number" min="0" max="1" step="0.01">'),
                     HTML(                                                                    '</div>'),
                     HTML(                                                                '</div>'), #row
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 8 SUBPANEL 1 ROW 3 -------------------------------------#
                     HTML(                                                                '<div class="row">'),
                     HTML(                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                        '<b sytle="line-height:1px;">Minimal clinical meaningful response rate without placebo adjustment (if available) (R<sub>clin</sub>)</b>'),
                     HTML(                                                                    '</div>'),
                     HTML(                                                                    '<div class="col-sm-1">'),
                     HTML(                                                                        '<label class="form-switch">'),
                     HTML(                                                                            '<input id="ui_number_mcpmod_bin_end_delta_yes" type="checkbox" >'),
                     HTML(                                                                            '<i></i>'),
                     HTML(                                                                        '</label>'),
                     HTML(                                                                     '</div>'),
                     HTML(                                                                     '<div class="col-sm-5">'),
                     HTML(                                                                         '<input id="ui_number_mcpmod_bin_end_delta" class="form-control" type="number" min="0" max="1" step="0.01" readonly="readonly">'),
                     HTML(                                                                     '</div>'),
                     HTML(                                                                '</div>'), #row
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 8 SUBPANEL 1 ROW 4 -------------------------------------#
                     HTML(                                                                '<div class="row">'),
                     HTML(                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                        '<b>Global power</b>'),
                     HTML(                                                                        '<span id="info_tab_8_1_4" tabindex="0">'),
                     HTML(                                                                            "<i class='fa fa-info-circle' role='presentation' aria-label='info-circle icon'></i>"),
                     HTML(                                                                        '</span>'),
                     HTML(                                                                        '<script>tippy("#info_tab_8_1_4", {content: "1: A &quot;proof-of-concept&quot; for the dose-response relationship based on multiple contrast test<br/>2: |max treatment effect| > &Delta;，equals to<br/>|logit(R<sub>max</sub>) - logit(R<sub>0</sub>)| > |logit(R<sub>clin</sub>) - logit(R<sub>0</sub>)|.<br/>i.e. Clinical relevant treatment effect in at least one active dose.", allowHTML: true,});'),
                     HTML(                                                                        '</script>'),
                     HTML(                                                                    '</div>'),
                     HTML(                                                                    '<div class="col-sm-6">'),
                     HTML(                                                                        '<div class="form-group">'),
                     HTML(                                                                            '<select id="ui_select_mcpmod_bin_end_type" class="selectized uiSelectized" disabled="disabled">'),
                     HTML(                                                                                '<option value="1">1. PoC only</option>'),
                     HTML(                                                                                '<option value="2">2. PoC and |max treatment effect| > &Delta;</option>'),
                     HTML(                                                                                '<option value="3">3. PoC, |max treatment effect| > &Delta;, and MED in the dose range</option>'),
                     HTML(                                                                            '</select>'),
                     HTML(                                                                        '</div>'),
                     HTML(                                                                    '</div>'),
                     HTML(                                                                '</div>'), #row
                     HTML(                                                            '</fieldset>'),
                     HTML(                                                        '</div>'), #form-group
                     HTML(                                                    '</div>'), #row-sm-12
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 8 SUBPANEL 1 FIELDSET 2 -----------------------------------#
                     HTML(                                                    '<div class="row-sm-12">'),
                     HTML(                                                        '<div class="form-group">'),
                     HTML(                                                            '<fieldset class="uiFieldsetBlack">'),
                     HTML(                                                                 '<legend class="uiSubLegendBlue">Models</legend>'),
                     HTML(                                                                 '<div class="row-sm-12 form-inline">'),
                     HTML(                                                                     '<div id="mcpmod_bin_end_candidate_model_table"></div>'),
                     HTML(                                                                 '</div>'),
                     HTML(                                                            '</fieldset>'),
                     HTML(                                                        '</div>'), #form-group
                     HTML(                                                    '</div>'), #row-sm-12
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 8 SUBPANEL 1 ROW 4: Check Button---------------------------#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div style="text-align: center">'), #class="col-auto"
                     HTML(                                                            '<button id="ui_btn_mcpmod_bin_end_check" class="btn btn-primary uiCheckBtn">Check Input</button>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     HTML(                                                '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 8 SUBPANEL 2: MCP-Mod - Binary Endpoint - Summary ---------#
                     HTML(                                                '<div class="tab-pane fade" id="tab-input_mcpmod_bin_end-2">'),
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 8 SUBPANEL 2 ROW 1: MCP-Mod - Binary Endpoint Summary -----#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div class="col-sm">'),
                     #HTML(                                                            'MCP-Mod Continuous Endpoint Summary'),
                     HTML(                                                            '<div id="summary_track_table8"></div>'),
                     HTML(                                                        '</div>'), #col-sm
                     HTML(                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 8 SUBPANEL 2 ROW 2: MCP-Mod - Binary Endpoint R code ------#
                     HTML(                                                    '<div class="row-sm uiKeyArea">'),
                     HTML(                                                        '<div class="col-auto">'),
                     HTML(                                                            '<pre>'),
                     HTML(                                                                '<textarea id="ui_textarea_mcpmod_bin_end" class="form-control " rows="7" cols="80" readonly=true>'),
                     HTML(                                                                '</textarea>'),
                     HTML(                                                            '</pre>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 8 SUBPANEL 2 ROW 3: MCP-Mod - Binary Endpoint Run Button --#
                     HTML(                                                    '<div class="row-sm">'),
                     HTML(                                                        '<div class="col-auto" style="text-align: center">'),
                     HTML(                                                            '<button id="ui_btn_mcpmod_bin_end_refresh" class="btn btn-primary">Refresh</button>'),
                     HTML(                                                            '<button id="ui_btn_mcpmod_bin_end_download" class="btn btn-primary">Download</button>'),
                     HTML(                                                            '<button id="ui_btn_mcpmod_bin_end_run" type="button" class="btn btn-primary action-button  uiCheckBtn">Run</button>'),
                     HTML(                                                        '</div>'), #col-auto
                     HTML(                                                    '</div>'), #row-sm
                     HTML(                                                '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 8 SUBPANEL 3: MCP-Mod - Binary Endpoint - Output ----------#
                     HTML(                                                '<div class="tab-pane fade" id="tab-input_mcpmod_bin_end-3">'),
                     outUI("cont8"),
                     HTML(                                                '</div>'),
                     #####-------------------------------------------------------------------------------------------#
                     HTML(                                            '</div>'), #tab-content
                     HTML(                                        '</div>'), #nav-tabs-custom
                     HTML(                                    '</div>'), #tab-pane
                     HTML(                                '</div>'), #box-body
                     HTML(                            '</div>'), #box
                     HTML(                        '</div>'), #tab-pane
                     ##### BODY BOX BOXBODY ROW 1 TABPANE 8: END #####################################################

                     HTML(                    '</div>'), #tab-content
                     ##### BODY BOX BOXBODY ROW 1 TABPANE: END #######################################################
                     HTML(                '</div>'), #col-sm-12
                     HTML(            '</div>'), #row
                     HTML(        '</div>'), #boxbody
                     HTML(    '</div>'), #box
                     HTML('</div>') #col-sm-12
            )
            ######## BODY BOX: END ##############################################################################
        )
        ############ DASHBOARD BODY: END ########################################################################

    ) #dashboardPage END
) #fluidPage END


# Server of plot and data part
out <- function (input, output, session, data1, data2, plot) {
    output$data1 <- renderDataTable(data1)
    output$data2 <- DT::renderDataTable(data2)
    output$plot <- renderPlotly(plot)

    # download
    output$downloaddata <- downloadHandler(filename=function() {
        paste0("data-", Sys.Date(), ".csv")},
        content = function(file) {
            write.csv(isolate(data1), file)
        }
    )
}

server <- function(input, output) {

    # 1 - Superiority > Continuous Endpoint > T-Test
    observeEvent(input$ui_btn_con_end_t_test_run, {
        #waiter_show(html = waiting_screen, color = "black")
        source('app/continuous_ttest.R')
        print("Run: Superiority > Continuous Endpoint >> T Test")
        code <- input$ui_textarea_con_end_t_test
        print(code)
        withProgress(
            message = "Simulation in progress",
            detail = 'This may take while...',{
                incProgress(0.1)
                res <- eval(parse(text = code))
            })
        callModule(out, "cont1", res$df1, res$df2, res$plot)
        #waiter_hide()
    })

    # 2 - Superiority > Binary Endpoint > Chi-square
    observeEvent(input$ui_btn_bin_end_chi_square_run, {
        #waiter_show(html = waiting_screen, color = "black")
        source('app/binary_chiq.R')
        print("Run: Superiority > Binary Endpoint >> Chi-square")
        code <- input$ui_textarea_bin_end_chi_square
        print(code)
        withProgress(
            message = "Simulation in progress",
            detail = 'This may take while...',{
                incProgress(0.1)
                res <- eval(parse(text = code))
            })
        callModule(out, "cont2", res$df1, res$df2, res$plot)
        #waiter_hide()
    })

    # 3 - Superiority > Time-to-Event Endpoint > Life Test
    observeEvent(input$ui_btn_t2e_end_life_test_run, {
        #waiter_show(html = waiting_screen, color = "black")
        source('app/TTE.R')
        print("Run: Superiority > Time-to-Event Endpoint >> Life Test")
        code <- input$ui_textarea_t2e_end_life_test
        print(code)
        withProgress(
            message = "Simulation in progress.",
            detail = "If 10000 simulations of 15000 paticipants for 5 sequences will take about 10 minutes.",{
                incProgress(0.1)
                res <- eval(parse(text = code))
            })
        callModule(out, "cont3", res$df1, res$df2, res$plot)
        #waiter_hide()
    })

    # 4 - Non-inferiority > Continuous Endpoint
    observeEvent(input$ui_btn_ni_con_end_run, {
        #waiter_show(html = waiting_screen, color = "black")
        source('app/continuous_ttest_ni.R')
        print("Run: Non-inferiority > Continuous Endpoint")
        code <- input$ui_textarea_ni_con_end
        print(code)
        withProgress(
            message = "Simulation in progress",
            detail = 'This may take while...',{
                incProgress(0.1)
                res <- eval(parse(text = code))
            })
        callModule(out, "cont4", res$df1, res$df2, res$plot)
        #waiter_hide()
    })

    # 5 - Non-inferiority > Binary Endpoint
    observeEvent(input$ui_btn_ni_bin_end_run, {
        #waiter_show(html = waiting_screen, color = "black")
        source('app/binary_chiq_ni.R')
        print("Run: Non-inferiority > Binary Endpoint")
        code <- input$ui_textarea_ni_bin_end
        print(code)
        withProgress(
            message = "Simulation in progress",
            detail = 'This may take while...',{
                incProgress(0.1)
                res <- eval(parse(text = code))
            })
        callModule(out, "cont5", res$df1, res$df2, res$plot)
        #waiter_hide()
    })

    # 6 - Non-inferiority > Time-to-Event Endpoint
    observeEvent(input$ui_btn_ni_t2e_end_run, {
        #waiter_show(html = waiting_screen, color = "black")
        source('app/TTE_ni.R')
        print("Run: Non-inferiority > Time-to-Event Endpoint")
        code <- input$ui_textarea_ni_t2e_end
        print(code)
        withProgress(
            message = "Simulation in progress.",
            detail = "If 10000 simulations of 15000 paticipants for 5 sequences will take about 10 minutes.",{
                incProgress(0.1)
                res <- eval(parse(text = code))
            })
        callModule(out, "cont6", res$df1, res$df2, res$plot)
        #waiter_hide()
    })

    # 7 - MCP-Mod > Continuous Endpoint
    observeEvent(input$ui_btn_mcpmod_con_end_run, {
        #waiter_show(html = waiting_screen, color = "black")
        source('app/mcp_mod.R')
        print("Run: MCP-Mod > Continuous Endpoint")
        code <- input$ui_textarea_mcpmod_con_end
        print(code)
        withProgress(
            message = "Simulation in progress.",
            detail = "",{
                incProgress(0.1)
                res <- eval(parse(text = code))
            })
        callModule(out, "cont7", res$df1, res$df2)
        #waiter_hide()
    })

    # 8 - MCP-Mod > Binary Endpoint
    observeEvent(input$ui_btn_mcpmod_bin_end_run, {
        #waiter_show(html = waiting_screen, color = "black")
        source('app/mcp_mod_bin.R')
        print("Run: MCP-Mod > Binary Endpoint")
        code <- input$ui_textarea_mcpmod_bin_end
        print(code)
        withProgress(
            message = "Simulation in progress.",
            detail = "",{
                incProgress(0.1)
                res <- eval(parse(text = code))
            })
        callModule(out, "cont8", res$df1, res$df2)
        #waiter_hide()
    })
}

# Run the application
#options(shiny.host="10.31.5.80", shiny.port=10012)
shinyApp(ui = ui, server = server)


