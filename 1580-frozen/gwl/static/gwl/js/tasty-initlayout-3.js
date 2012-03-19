/*
;;
;; Copyright 2002-2011 Genworks International and Genworks BV 
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GDL).
;;
;; This source file contains free software: you can redistribute it
;; and/or modify it under the terms of the GNU Affero General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This source file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public
;; License along with this source file.  If not, see
;; <http://www.gnu.org/licenses/>.
;; 
*/

/*
*       GDL TATU BASIC JAVASCRIPT SETUP SHEET
*   This javascript will setup the layout (based on the jquery layout plugin)
*   for the TATU interface release ta2.0 (gdl1577 and later).
*
*/


// Setting up a side-by-side 3 pane layout

var PageLayout, ApplicationLayout, InnerLayout, $Tabs, $Accordion, $InspectorTable;

function focusCenter() {
        // This function is placed in the menu to focus all attention on the
        // inner-center pane - where normally the viewport is located   
        InnerLayout.close("west");
        InnerLayout.close("east");
        ApplicationLayout.close("west");
        ApplicationLayout.close("east");
        PageLayout.close("north");      // tabs up north
};

function resizeDocIFrame () {
        // Resizes the IFrame in the documentation tab
        var theheight = PageLayout.state.center.innerHeight-25;
        $("#Documentation > iframe.ui-widget-content").height(theheight);
};


function resizeTabLayout () {
        // Resizes the various inner layouts based on which tab is selected
        // tab 0 = application tab
        // tab 1 = documentation tab
        if (!$Tabs) return; // make sure tabs are initialized
        var selected = $Tabs.tabs('option', 'selected');
        // ONLY resize the layout when first tab is 'visible'
        if (selected == 0) { // first tab
                // non-IE browsers are 'hiding' the ApplicationLayout...
                $('#ApplicationLayout').show(); // FIX for non-IE
                // then resize the layout to fit the new container size
                ApplicationLayout.resizeAll(); // triggers cascading resize of inner-layouts
        };
        // FLAG: JB-090814 only works with RC3 of the jQuery Layout code - which is due
        // for release during beta test.
        if (selected == 1) { //second documentation tab
                resizeDocIFrame();
        };
};

function initTabs () {
        // Creates the Tabs
        // best to create the tabs first, because is 'container' for the tab-layout (ApplicationLayout)
        $Tabs = $("#tabs").tabs({
                // resize layout EACH TIME the layout-tab becomes 'visible'
                show:                                   resizeTabLayout 
        });
};

function initAccordion () {
        // Initiates the accordion
        $Accordion = $("div.main-inspector-div").accordion({
                fillSpace:                              true
        });
};

/*
// FLAG JB 100204 no new inspector available in
// production version, this part of the code is commented out until 
// the new inspector package is released

function initInspectorTable () {
        $InspectorTable = $('table.inspector-table').dataTable({
                bPaginate:                              false
        ,       bJQueryUI:                              true
        ,       bSort:                                  true
        ,       bAutoWidth:                     false
        });
};
*/

function initPageLayout () {
        // Initiates the Page Layout (most outer layout including tabs-n-stuff
        // use different outer-layout classNames to simplify/clarify CSS
        PageLayout = $('body').layout({ 
                name:                                   "PageLayout"
        ,       north__paneSelector:    "#TabButtons"
        ,       center__paneSelector:   "#TabPanelsContainer"
        //,     center__onresize:               "ApplicationLayout.resizeAll"
        ,   center__onresize:           "resizeTabLayout"
        ,       spacing_open:                   0
        });
};

function initApplicationLayout () {     
        // Initiated the Application, which is located within the application Tab
        ApplicationLayout = $('#ApplicationLayout').layout({
                name:                                   "ApplicationLayout"
        ,       center__paneSelector:   ".outer-center"
        ,       west__paneSelector:             ".outer-west"
        ,       east__paneSelector:             ".outer-east"
        ,       north__paneSelector:    ".outer-north"
        ,       south__paneSelector:    ".outer-south"
        ,       contentSelector:                ".ui-widget-content"
        ,       center__onresize:               "InnerLayout.resizeAll"
        ,       spacing_open:                   4
        ,       spacing_closed:                 4
        ,       north__spacing_open:    1
        ,       north__togglerLength_open: 0
        ,       north__togglerLength_close: -1
        ,       north__resizable:               false
        ,       north__slidable:                false
        ,       north__fxName:                  "none"
        ,       north__showOverflowOnHover: true
        ,       east__initClosed:               true
        ,       west__size:                     200
        //,   west__showOverflowOnHover: true
        ,       south__size:                    'auto'
        ,       south__togglerLength_open: 0
        ,       south__togglerLength_close: -1
        ,       south__resizable:               false
        ,       south__slidable:                false
        ,       south__spacing_open:    1
        });
        
        InnerLayout = $('#InnerLayout').layout({
                name:                                   "InnerLayout"
                ,       center__onresize: "gdlResize();"
                ,       center__paneSelector:   ".inner-center"
                ,       west__paneSelector:             ".inner-west"
                ,       east__paneSelector:             ".inner-east"
                ,       north__paneSelector:    ".inner-north"
                ,       contentSelector:                ".ui-widget-content"
                ,       east__initClosed:               true
                ,       north__initClose:               true
                ,       north__initHidden:              true
                ,       spacing_open:                   4
                ,       spacing_closed:                 4
                ,       west__size:                     361
                ,       east__size:                     250
                ,       east__fxSpeed:                  "slow"
                ,       north__minSize:                 15
                ,       north__spacing_open:    2
                ,       north__togglerLength_open: 50
                ,       north__togglerLength_close: -1
            });
};

function initMenus () {
        // Initiated the Menus
        $('div.outer-north ul#menu').superfish({
                delay:                                  1
        ,       speed:                                  "fast"
        });     
        $('div.outer-north ul#toolbar').superfish();
};      

$(document).ready(function () { 
        // Initiate all the elements on the page
        initTabs();
        // Initiate Layouts
        initPageLayout();
        initApplicationLayout();

        // Initiate Functional elements
        initMenus();
        initAccordion();
        // FLAG JB 100204 no new inspector available in
        // production version, this part of the code is commented out until 
        // the new inspector package is released
        // initInspectorTable(); 
        
        PageLayout.resizeAll();
        // Hide the loader when the page is correctly initiated.
        $("#loader-message").hide();
});
