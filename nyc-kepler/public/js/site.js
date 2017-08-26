function draw_planet(mass_earth, radius_earth) {
  var base_radius = 50;
  var tw = 950;
  var th = 300;

  var radius_el = document.getElementById('radius_paper');

  if (radius_el) {
    var radius_paper = new Raphael(radius_el, tw, th);

    draw_pair(
        radius_paper,
        base_radius,
        radius_earth,
        4);
  }

  var mass_el = document.getElementById('mass_paper');

  if (mass_el) {
    var mass_paper = new Raphael(mass_el, tw, th);

    draw_pair(
        mass_paper,
        base_radius,
        mass_earth,
        4);
  }
}

function draw_pair(paper, earth_width, exo_multiple, stroke_width) {
  var exo_width = earth_width * exo_multiple;
  var exo_offset = 0;

  if (
      (exo_width > paper.width - (earth_width * 2)) ||
      (exo_width < paper.width - earth_width)){
    exo_offset = ((earth_width + stroke_width) * 2) + exo_width + stroke_width;
  }

  console.debug(exo_width + ' | ' + exo_offset);

  var earth = paper.circle(earth_width + stroke_width, earth_width + stroke_width, earth_width);
  var exo = paper.circle(exo_offset, 0, exo_width);

  earth.attr({
    'stroke': '#729fcf',
    'fill': '#3465a4',
    'stroke-width': stroke_width});

  exo.attr({
    'stroke': '#fce94f',
    'fill': '#edd400',
    'stroke-width': stroke_width});
}
