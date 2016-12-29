(function( $ ) {
  jQuery.fn.pacedProgressBar = function(options) {
    return this.each(function() {
      var canvas = document.createElement("canvas");
      $(canvas).addClass("budget-monitor-canvas");
      $(this).append(canvas);

      var data = $(this).data();

      var context = canvas.getContext("2d");

      var height = options.height || 96;
      var width = options.width || this.offsetWidth;

      // Border
      context.strokeStyle = options.borderColor || "black";
      context.lineWidth = 2;
      context.strokeRect(1, 2, width-2, height-2);

      // Current value
      if (data.value <= data.pacer) {
        context.fillStyle = options.goodCurrentValueColor || "#1aa33c";
      } else {
        context.fillStyle = options.badCurrentValueColor || "#d1311f";
      }
      var valueXOffset = 2;
      var valueYOffset = 4;
      var valueHeight = height - (valueYOffset * 2);
      var valueWidth = (width - (valueXOffset * 2)) * (data.value / data.max);
      var availableValueWidth = width - (valueXOffset * 2);
      if (valueWidth > availableValueWidth) {
        valueWidth = availableValueWidth;
      }
      context.fillRect(valueXOffset, valueYOffset, valueWidth, valueHeight);

      // Pacer
      var pacerX = width * (data.pacer / data.max);
      context.beginPath();
      context.lineWidth = 4;
      if (data.value <= data.pacer) {
        context.strokeStyle = options.goodPacerColor || "#a5281a";
      } else {
        context.strokeStyle = options.badPacerColor || "#d0d622";
      }
      context.moveTo(pacerX, 0);
      context.lineTo(pacerX, height);
      context.stroke();
    });
  };
})(jQuery);
