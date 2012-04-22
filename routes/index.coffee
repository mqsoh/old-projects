configs = require '../configs'
api = require '../controllers/api'
site = require '../controllers/site'

routes = (app) ->
  app.get '/', site.home
  app.get '/exoplanet-visualization', site.visualization
  app.get '/exoplanet-list', site.list

  app.get '/api/exoplanets', api.exoplanets

module.exports = routes
