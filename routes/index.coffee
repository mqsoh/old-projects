configs = require '../configs'

routes = (app) ->
  app.get '/', (req, res) ->
    res.render 'index',
      title: 'NYC Kepler'

module.exports = routes
