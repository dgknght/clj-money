$(function() {
  $("a[data-method]").click(function(e) {
    var a = $(e.target);
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
