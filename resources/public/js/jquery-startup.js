$(function() {
  $("a[data-method]").click(function(e) {
    // Find the anchor element
    var elem = e.target;
    while (elem != null && elem.tagName != "A") {
      elem = elem.parentElement;
    }
    if (elem == null) {
      console.log("Unable to find the anchor element");
      return false;
    }

    // Post to the url
    var a = $(elem);
    var url = a.attr("href");
    try {
      a.after("<form id=\"dynamic-form\" action=\"" +
          url + "\" method=\"" +
          a.data('method') + "\"></form");
      $("#dynamic-form").submit();
    } catch (e) {
      console.log("error");
      console.log(e);
    }

    return false;
  });
});
