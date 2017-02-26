HTMLWidgets.widget({

  name: "pivottabler",

  type: "output",

  factory: function(el, width, height) {

    var pivotElement = el;

    return {
      renderValue: function(widgetData) {
        // parse data
        var parser = new DOMParser();
        pivotElement.innerHTML = widgetData.tableHtml;
      },

      resize: function(width, height) {
      },

    };
  }
});
