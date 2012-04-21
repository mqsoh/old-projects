configs = require '../configs'
api = require '../controllers/api'

routes = (app) ->
  app.get '/', (req, res) ->
    res.render 'index',
      title: 'NYC Kepler'

  app.get '/api/kepler/confirmed-planets', api.kepler.confirmed_planets
  app.get '/fake-nasa-page', api.kepler.nasa_web_page

module.exports = routes
