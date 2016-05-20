var path = require('path');
var webpack = require('webpack');

module.exports = {
  devtool: 'eval',
  entry: [
    'webpack-dev-server/client?http://localhost:3000',
    'webpack/hot/only-dev-server',
    './src/index'
    // './src/ts/orb'
  ],
  output: {
    path: path.join(__dirname, 'dist'),
    filename: 'bundle.js',
    publicPath: '/static/'
  },
  plugins: [
    new webpack.DefinePlugin({
      'process.env': {
        'NODE_ENV': JSON.stringify('production')
      }
    }),
    new webpack.HotModuleReplacementPlugin()
  ],
  resolve: {

  /*
   * An array of extensions that should be used to resolve modules.
   *
   * See: http://webpack.github.io/docs/configuration.html#resolve-extensions
   */
  extensions: ['', '.ts', '.js', '.tsx']
  },
  module: {
    loaders: [
      // {test: /\.js$/,
      //   loader:'raw-loader'}
      // {
      //   test: /\.jsx$/,
      //   loaders: ['react-hot', 'babel'],
      //   include: path.join(__dirname, 'src')
      // },
      {
        test: /\.tsx$/,
        loaders: ['react-hot', 'babel', 'awesome-typescript-loader'],
        include: path.join(__dirname, 'src')
      },
      {test: /\.ts$/,
        loaders: ['awesome-typescript-loader']
      },
      {test: /\.css$/,
        loaders:['style','css']
      }
      // // Font loader
      // { test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/, loader: 'url-loader'+'?limit=10000&mimetype=application/font-woff' },
      // { test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/, loader: 'file-loader' }

    ]
  }
};
