HTMLWidgets.widget({

  name: "pivottabler",

  type: "output",

  factory: function(el, width, height) {

    var pivotElement = el;
    var initialised = false;

    return {
      renderValue: function(widgetData) {

        if(!initialised) {
          docHead = document.head || document.getElementsByTagName("head")[0];
          var styles = document.createElement("style");
          styles.innerHTML = widgetData.tableCss;
          docHead.appendChild(styles);
          initialised = true;
        }
        pivotElement.innerHTML = widgetData.tableHtml;
      },

      resize: function(width, height) {
      },

    };
  }
});
