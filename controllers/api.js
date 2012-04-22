var request = require('request');
var cache = require('memory-cache');

/**
 * Prints to 'resp' a big list of exoplanets from exoplanets.org.
 *
 * @param req
 * @param resp
 *
 * @return
 *   A big JSON object.
 */
exports.exoplanets = function (req, res) {
  var cache_key = 'masons-holiday-destinations';
  var csv = 'http://exoplanets.org/exoplanets.csv';
  var stored = cache.get(cache_key);

  if (stored) {
    res.send(stored);
    return;
  }
  request(csv, function (error, response, body) {
    console.log('Fetching ' + csv);

    var planets = [];
    var planet = null;
    var column_names = null;

    body = body.replace('\r', '\n');

    var lines = body.split('\n');

    for (var ii in lines) {
      var line = lines[ii];

      if (line.length < 1) {
        continue;
      }

      if (ii == 0) {
        column_names = csv_columns(line);
        continue;
      }

      planet = {};
      var cols = csv_columns(line);
      for (var jj in cols) {
        var col = cols[jj];
        planet[column_names[jj].toLowerCase()] = col;
      }

      planets.push(planet);
    }

    cache.put(cache_key, planets, 24 * 60 * 60);

    res.json(planets);
  });
}

/**
 * I guess node doesn't have this. Seems kind of unlikely, but...
 *
 * @param string line
 *   A string with some commas in it.
 *
 * @return
 *   A list of some strings.
 */
function csv_columns(line) {
  var cols = [];
  var prev_c = null;
  var col_buffer = '';
  var in_string = false;

  var chars = line.split('');
  for (var ii in chars) {
    var c = chars[ii];

    if (c == ',' && !in_string) {
      cols.push(col_buffer);
      col_buffer = '';
      continue;
    }

    if (c == '"' && prev_c != '\\') {
      in_string = !in_string;
    }

    if (!in_string || (in_string && c != '\\')) {
      col_buffer += c;
    }

    prev_c = c;
  }

  cols.push(col_buffer);

  return cols;
}
