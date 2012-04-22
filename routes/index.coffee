configs = require '../configs'
api = require '../controllers/api'

routes = (app) ->
  app.get '/', (req, res) ->
    res.render 'index',
      title: 'NYC Kepler'

  app.get '/api/exoplanets', api.exoplanets

module.exports = routes
