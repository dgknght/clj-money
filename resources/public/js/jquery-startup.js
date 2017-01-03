$(function() {

  function findAncestor(elem, tagName) {
    while (elem != null) {
      if (elem.tagName == tagName) {
        return elem
      }
      elem = elem.parentElement;
    }
    return null;
  }

  function postLink(a) {
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
  }

  // Links that post
  $("a[data-method]").click(function(e) {
    e.stopPropagation();
    // Find the anchor element
    var elem = findAncestor(e.target, "A");
    if (elem == null) {
      console.log("Unable to find the anchor element");
      return false;
    }

    a = $(elem);

    // Post to the url
    if (a.data("confirm") == null) {
      postLink(a);
    }
    return false;
  });

  // Links with confirmation
  $("a[data-confirm]").click(function(e) {
    e.stopPropagation();
    // Find the anchor element
    var elem = findAncestor(e.target, "A");
    if (elem == null) {
      console.log("Unable to find the anchor element");
      return false;
    }

    var a = $(elem);
    if (confirm(a.data("confirm"))) {
      if (a.data("method") == null) {
        return true;
      } else {
        postLink(a);
      }
    }
    return false;
  });

  // Date Pickers
  $(".date-field").datepicker();

  // Budget Monitors
  $(".paced-progress-bar").pacedProgressBar({borderColor: "#CCC"});
});
