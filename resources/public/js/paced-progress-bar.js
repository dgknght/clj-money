(function( $ ) {
  jQuery.fn.pacedProgressBar = function(options) {
    return this.each(function() {
      var canvas = document.createElement("canvas");
      $(canvas).addClass("budget-monitor-canvas");
      $(this).append(canvas);

      var data = $(this).data();

      var context = canvas.getContext("2d");

      var height = options.height || this.offsetHeight;
      var width = options.width || this.offsetWidth;

      canvas.height = height;
      canvas.width = width;

      // Current value
      var valuePercent = null;
      if (data.value <= data.pacer) {
        context.fillStyle = options.goodCurrentValueColor || "#1aa33c";
        valuePercent = data.value / data.max;
      } else {
        context.fillStyle = options.badCurrentValueColor || "#d1311f";
        valuePercent = 1;
      }
      var valueXOffset = 1;
      var valueYOffset = 1;
      var valueHeight = height - (valueYOffset * 2);
      var valueWidth = (width - (valueXOffset * 2)) * valuePercent;
      context.fillRect(valueXOffset, valueYOffset, valueWidth, valueHeight);

      // Border
      context.strokeStyle = options.borderColor || "black";
      context.lineWidth = 2;
      context.strokeRect(1, 2, width-2, height-2);

      // Pacer
      var pacerX = width * (data.pacer / data.max);
      context.beginPath();
      context.lineWidth = 4;
      if (data.value <= data.pacer) {
        context.strokeStyle = options.goodPacerColor || "#a5281a";
      } else {
        context.strokeStyle = options.badPacerColor || "#d0d622";
      }
      context.moveTo(pacerX, 1);
      context.lineTo(pacerX, height - 1);
      context.stroke();
    });
  };
})(jQuery);
