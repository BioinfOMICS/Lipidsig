library(shiny)
library(magrittr)

#### Consolidated CSS (single source of truth) ####
body.css <- "
/* ── Layout ─────────────────────────────────────────── */
#body_div {
  width: 1200px;
  margin: 15px auto;
}
#body_div {
  width: 1200px;
  margin: 15px auto;
}

/* ── Typography ──────────────────────────────────────── */
h1 {
  font-weight: 250;
  line-height: 1.1;
  color: midnightblue;
}
h6 {
  font-size: 18px;
  line-height: 1.5;
  color: black;
}

/* ── Profiling tab panel height ──────────────────────── */
#PRO_tabPanel_div {
  height: 1000px;
}

/* ── Navbar ──────────────────────────────────────────── */
.navbar-default {
text-align: center;
}
.navbar-brand {
  float: left;
  padding: 0;
  font-size: 19px;
  line-height: 21px;
}
/* 移除 Bootstrap/shinythemes 預設的 navbar <a> 上下 padding */
.navbar-default .navbar-nav > li > a {
  padding-top: 0px !important;
  padding-bottom: 0px !important;
  color: #f5f5f5 !important;
  text-decoration: none !important;
}
/* 縮小水平間距，防止 12 個項目換行 */
.navbar-default .navbar-nav > li > a,
.navbar-nav > li > a {
  padding-left: 6px !important;
  padding-right: 6px !important;
}
/* Navbar <h4>：固定高度 + flexbox 垂直置中 + 縮小字體 */
.navbar-default .navbar-nav > li > h4 {
  font-size: 14px !important;
  margin: 0 !important;
  height: 50px !important;
  display: flex !important;
  align-items: center !important;
  justify-content: center !important;
  padding-top: 0 !important;
  padding-bottom: 0 !important;
  text-align: center;
  line-height: 1.3;
}
.navbar-default .navbar-nav > li > h4 > a {
  color: #f5f5f5 !important;
  text-decoration: none !important;
  text-align: center;
  line-height: 1.3;
}
.navbar-default .navbar-nav > li > h4 > a:hover {
  color: #18bc9c !important;
}
.navbar-default .navbar-nav > .open > a,
.navbar-default .navbar-nav > .open > a:focus,
.navbar-default .navbar-nav > .open > a:hover {
  color: #555;
  background-color: #e7e7e7;
}
/* 內容區的 sub-tab / pill 不受影響 */
.nav > li > a {
  position: relative;
  display: block;
}
.nav-tabs > li > a {
  padding: 10px 13px !important;
}
.nav-pills > li > a {
  padding: 10px 10px !important;
}

/* ── Dropdown ────────────────────────────────────────── */
.dropdown-menu {
  position: absolute;
  top: 100%;
  left: 0;
  z-index: 1000;
  display: none;
  float: left;
  min-width: 160px;
  padding: 5px 0;
  margin: 2px 0 0;
  font-size: 14px;
  text-align: left;
  list-style: none;
  background-color: #fff;
  background-clip: padding-box;
  border: 1px solid rgba(0,0,0,.15);
  border-radius: 4px;
  -webkit-box-shadow: 0 6px 12px rgb(0 0 0 / 18%);
  box-shadow: 0 6px 12px rgb(0 0 0 / 18%);
}
.open > .dropdown-menu {
  display: block;
}
.navbar-nav > li > .dropdown-menu {
  margin-top: 0;
  border-top-left-radius: 0;
  border-top-right-radius: 0;
}
.dropdown-menu > li > a {
  display: block;
  padding: 3px 20px;
  clear: both;
  font-weight: normal;
  line-height: 1.42857143;
  color: #2c3e50;
  white-space: nowrap;
}
.caret {
  display: inline-block;
  width: 0;
  height: 0;
  margin-left: 2px;
  vertical-align: middle;
  border-top: 4px dashed;
  border-right: 4px solid transparent;
  border-left: 4px solid transparent;
}

/* ── Forms ───────────────────────────────────────────── */
.radio, .checkbox {
  margin-top: 0px;
}

/* ── SweetAlert ──────────────────────────────────────── */
.swal-title {
  color: black;
}
h2#swal2-title {
  font-size: 30px;
  color: black;
  padding: 0px;
}
button.swal2-confirm.swal2-styled {
  width: 70px;
  font-size: 15px;
}
.swal-text {
  font-size: 18px;
  position: relative;
  float: none;
  line-height: normal;
  vertical-align: top;
  text-align: left;
  display: inline-block;
  margin: 0;
  padding: 0 10px;
  font-weight: 400;
  color: black;
  max-width: calc(100% - 20px);
  overflow-wrap: break-word;
  box-sizing: border-box;
}

/* ── Home page blocks ────────────────────────────────── */
.home_block {
  position: relative;
  display: table;
  width: 100%;
  height: 56px;
  margin-bottom: 10px;
  font-weight: 700;
  border-radius: 7px;
  cursor: pointer;
  overflow: hidden;
  transition: box-shadow .4s;
  background: linear-gradient(#8adff9, #27537e);
}
.home_block span.name {
  left: 0;
  font-size: 22px;
  font-weight: 550;
  font-family: georgia;
  line-height: 1.2em;
  display: block;
  padding-right: 5px;
  padding-top: 10px;
  padding-left: 5px;
  word-break: normal;
  text-align: center;
  color: #fff;
  transition: font-weight .4s;
}
"

##### Shiny User Interface #####
shiny::shinyUI(shiny::fluidPage(

  ## ── Global styles & scripts (loaded once) ──────────────
  htmltools::tags$style(body.css),
  theme = shinythemes::shinytheme("flatly"),
  shinyjs::useShinyjs(),

  ## sweetalert (used by all tabs via show())
  htmltools::HTML('<script src="sweetalert.min.js"></script>'),
  ## disable.js (used by Network / Enrichment)
  htmltools::HTML('<script src="disable.js"></script>'),
  ## scrolldown helper
  htmltools::HTML('<script src="scrolldown.js"></script>'),
  ## show() loading overlay — defined once globally
  htmltools::HTML('<script type="text/javascript">
    function show() {
      swal({
        title: "",
        text: "Loading...",
        icon: "www/Description/loading.gif",
        imageHeight: 80,
        imageWidth: 80,
        buttons: false,
        closeOnClickOutside: false,
        timer: 100000
      });
    }
    /* 切換 navbarPage 內部頁籤（不跳出 app） */
    function switchTab(tabValue) {
      var $tab = $("ul.navbar-nav a[data-value=\'" + tabValue + "\']");
      if ($tab.length) {
        $tab.tab("show");
      }
    }
  </script>'),

  ## ── 動態注入 navbar CSS（保證在 shinythemes 之後載入，一定蓋得過）──
  htmltools::HTML('<script type="text/javascript">
    $(document).ready(function() {
      var navbarCSS = [
        /* 移除 Bootstrap 預設 navbar <a> 的上下 padding */
        ".navbar-default .navbar-nav>li>a { padding-top:0!important; padding-bottom:0!important; padding-left:6px!important; padding-right:6px!important; color:#f5f5f5!important; text-decoration:none!important; }",
        /* <h4> 固定高度 + flexbox 垂直置中 + 字體縮小 */
        ".navbar-default .navbar-nav>li>h4 { font-size:14px!important; margin:0!important; height:50px!important; display:flex!important; align-items:center!important; justify-content:center!important; padding:0!important; line-height:1.3; text-align:center; }",
        /* <h4> 內的 <a> 繼承顏色 */
        ".navbar-default .navbar-nav>li>h4>a { color:#f5f5f5!important; text-decoration:none!important; line-height:1.3; text-align:center; }",
        ".navbar-default .navbar-nav>li>h4>a:hover { color:#18bc9c!important; }",
        /* active tab 樣式 */
        ".navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:hover { color:#18bc9c!important; background-color:transparent!important; }",
        /* 避免影響頁面內的 nav-tabs 和 nav-pills */
        ".nav-tabs>li>a { padding:10px 13px!important; }",
        ".nav-pills>li>a { padding:10px 10px!important; }"
      ].join(" ");
      $("<style>").text(navbarCSS).appendTo("head");
    });
  </script>'),

  ## ── Page body ───────────────────────────────────────────
  htmltools::div(id = 'body_div',

    shiny::titlePanel("LipidSig 2.0: Integrating Lipid Characteristic Insights into Advanced Lipidomics Data Analysis"),

    shiny::navbarPage(
      title    = htmltools::h4(''),
      id       = 'narbarpage',
      selected = 'Home',

      #### Home ####
      source("ui_home.R", local = TRUE)$value,

      #### Data Check ####
      source("ui_DataCheck.R", local = TRUE)$value,

      #### Profiling ####
      source("ui_Profiling.R", local = TRUE)$value,

      #### Differential Expression ####
      source("ui_DE.R", local = TRUE)$value,

      #### Enrichment ####
      source("ui_Enrichment.R", local = TRUE)$value,

      #### Machine Learning ####
      source("ui_ML.R", local = TRUE)$value,

      #### Correlation ####
      source("ui_Correlation.R", local = TRUE)$value,

      #### Network ####
      source("ui_Network.R", local = TRUE)$value,

      #### ID Conversion ####
      source("ui_IDconversion.R", local = TRUE)$value

    ) ## navbarPage
  ) ## div #body_div
)) ## shinyUI
