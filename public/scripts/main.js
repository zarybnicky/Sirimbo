var masonry;
$(function() {
  $('.ui.video').video();
  $(".sticky").sticky({topSpacing:40});

  objectFit.polyfill({
    selector: '.highlights-image img',
    fittype: 'cover'
  });
  Stickyfill.add($('.sticky-top'));

  function maxHeight(element) {
    var height = 0;
    $(element).each(function() {
      if($(this).height() > height) {
        height = $(this).height();
      }
    });
    return height;
  }
  function minHeight(element) {
    var height = -1;
    $(element).each(function() {
      if($(this).height() < height || height == -1) {
        height = $(this).height();
      }
    });
    return height;
  }
  $(".highlights-info").height(maxHeight($(".highlights-info")));

  $(".highlights li").filter(function(index, element) {
    return $(this).find(".highlights-info .desc").is(":not(:empty)");
  }).hover(function() {
    var height = minHeight($(this).siblings().find(".highlights-info")) + 59;
    $(this).find(".highlights-info").animate({
      "height": height
    }, {
      "start": function() {
        $(this).find(".desc")
          .css("opacity", "0")
          .css("display", "block")
          .animate({ "opacity" : "1" });
      },
      "duration": 250
    });
  }, function() {
    var height = minHeight($(this).siblings() .find(".highlights-info"));
    $(this).find(".highlights-info").animate({
      "height": height
    }, {
      "start": function() {
        $(this).find(".desc").fadeOut(100);
      },
      "duration": 250
    });
  });

  var map;
  if (document.getElementById('m')) {
    map = L.map('m').setView([49.58727525, 17.25661055], 13);
    L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
      attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
    }).addTo(map);

    L.marker([49.57963, 17.2495939]).addTo(map);
    L.marker([49.59490, 17.26340]).addTo(map);
  }
  if (document.getElementById('holeckova')) {
    map = L.map('holeckova').setView([49.57963, 17.2495939], 16);
    L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
      attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
    }).addTo(map);
    L.marker([49.57963, 17.2495939]).addTo(map);
  }
  if (document.getElementById('slovan')) {
    map = L.map('slovan').setView([49.59490, 17.26340], 16);
    L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
      attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
    }).addTo(map);
    L.marker([49.59490, 17.26340]).addTo(map);
  }
});
