// JavaScript File for settings.html 
// @author: C. Musselle 

// Debugging
window.onerror = function (msg, url, line) {
   alert("Message : " + msg + "\n\nurl : " + url + "\n\nLine No: " + line);
}

//window.onload = function() { fetch_settings('current') }
// For some reason the page is loaded blank until the window is refocused onto.
// Because of this the event is added to a SCRIPT tag at the end of the HTML
// file which will avoid the blank window. Migrating to jQuery and using the
// .ready() would also avoid the error and clean up the code.


// Define Globals
var settings;

// Parse Wikihouse Settings for the input JSON string  
function recieve_wikihouse_settings(args) {
	
	  /*
	  @@wikihouse_settings = {
	  "sheet_height"        => wikihouse_sheet_height,
	  "sheet_inner_height"  => wikihouse_sheet_inner_height,
	  "sheet_width"         => wikihouse_sheet_width, 
	  "sheet_inner_width"   => wikihouse_sheet_inner_width, 
	  "padding"             => wikihouse_panel_padding,
	  "margin"              => wikihouse_sheet_margin,
	  "font_height"         => wikihouse_font_height,
	  "drill_width"         => wikihouse_drill_width,
	  "scale"               => wikihouse_scale,
	  }
	  */

	  settings = JSON.parse(args)
	  
	  document.getElementById("sheet_height").value = Math.round(settings.sheet_height*10)/10;
	  document.getElementById("sheet_width").value = Math.round(settings.sheet_width*10)/10;
	  document.getElementById("sheet_depth").value = Math.round(settings.sheet_depth*10)/10;
	  document.getElementById("margin").value = Math.round(settings.margin*10)/10;
	  document.getElementById("padding").value = Math.round(settings.padding*10)/10;
	  document.getElementById("font_height").value = Math.round(settings.font_height*10)/10;
	  document.getElementById("drill_width").value = Math.round(settings.drill_width*10)/10;
	  document.getElementById("scale").value = Math.round(settings.scale*10000)/10000;
	}

// Update Settings 
function send_wikihouse_settings(mode) {
  
	
  var fields = new Array("sheet_height", "sheet_width", "sheet_depth", 
		  "margin", "padding", "font_height" , "drill_width" , "scale");
  
  var idx, value, args;  

  for (idx in fields) {
	  
	  value = eval(document.getElementById(fields[idx]).value)
	  
	  if (typeof value == "number") {
		  // Only update those that are genuine numbers
		  settings[fields[idx]] = value
	  }
  }


  //Convert to String  
  args = JSON.stringify(settings)
  
  // Possibly add close flag for dialogue 
  if (mode == 1) {
    args = args + "--close";
  }
  
  //Send argument to SketchUp script for processing
  window.location.href = "skp:update_settings@" + args;
}

function cancel() {
  window.location.href = 'skp:cancel_settings@';
}

function display_status(msg) {
  document.getElementById("status_out").innerHTML = msg;
}

function fetch_settings(arg) {
  window.location.href = 'skp:fetch_settings@' + arg;
}

// For debugging
function do_stuff() {
	alert(typeof settings.padding)
//	alert(settings["sheet_height"] = eval('12'))
//	alert(settings["sheet_height"] = eval(234566))
}






